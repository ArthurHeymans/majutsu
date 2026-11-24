;;; majutsu-org-test.el --- Tests for Majutsu Org links -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the `majutsu' and `majutsu-rev' link types.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eieio)
(require 'majutsu-org)

(defmacro majutsu-org-test--with-buffer (root revision &rest body)
  "Run BODY in a Majutsu buffer with ROOT and REVISION at point.
`org-link-store-props' records the stored plist in
`org-store-link-plist' and returns it, like the real implementation."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (delay-mode-hooks (majutsu-mode))
     (cl-letf (((symbol-function 'majutsu--buffer-root)
                (lambda (&optional _buffer) ,root))
               ((symbol-function 'majutsu-revision-at-point)
                (lambda () ,revision))
               ((symbol-function 'org-link-store-props)
                (lambda (&rest plist)
                  (setq org-store-link-plist plist)
                  plist)))
       ,@body)))

(ert-deftest majutsu-org-setup/registers-link-types-and-remap ()
  (should (org-link-get-parameter "majutsu" :follow))
  (should (org-link-get-parameter "majutsu" :complete))
  (should (org-link-get-parameter "majutsu-rev" :store))
  (should (eq (lookup-key majutsu-mode-map [remap org-store-link])
              #'majutsu-org-store-link)))

(ert-deftest majutsu-org-store-repository/without-revision ()
  (majutsu-org-test--with-buffer "/repo/" nil
    (should (majutsu-org-store-repository))
    (should (equal (plist-get org-store-link-plist :type) "majutsu"))
    (should (equal (plist-get org-store-link-plist :link) "majutsu:/repo/"))))

(ert-deftest majutsu-org-store-repository/available-beside-revision ()
  (majutsu-org-test--with-buffer "/repo/" "abc123"
    (should (majutsu-org-store-repository))
    (should (equal (plist-get org-store-link-plist :link) "majutsu:/repo/"))))

(ert-deftest majutsu-org-store-revision/stores-revision-at-point ()
  (majutsu-org-test--with-buffer "/repo/" "abc123"
    (should (majutsu-org-store-revision))
    (should (equal (plist-get org-store-link-plist :type) "majutsu-rev"))
    (should (equal (plist-get org-store-link-plist :link)
                   "majutsu-rev:/repo/::abc123"))))

(ert-deftest majutsu-org-store-link/prefers-revision-over-repository ()
  "When both links are available, Org offers the revision first."
  (majutsu-org-test--with-buffer "/repo/" "abc123"
    (let (choices default)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest args)
                   (setq choices collection
                         default (nth 4 args))
                   (symbol-name (car collection)))))
        (org-link--try-link-store-functions t))
      (should (equal choices '(majutsu-org-store-revision
                               majutsu-org-store-repository)))
      (should (equal default "majutsu-org-store-revision")))))

(ert-deftest majutsu-org-store-link/stores-one-link-per-selected-section ()
  (majutsu-org-test--with-buffer "/repo/" nil
    (let ((inhibit-read-only t)
          (org-stored-links nil)
          (sections (list (magit-section) (magit-section))))
      (insert "a\nb\nc\n")
      (oset (car sections) start 1)
      (oset (cadr sections) start 2)
      (cl-letf (((symbol-function 'magit-region-sections)
                 (lambda (&rest _args) sections))
                ((symbol-function 'majutsu-revision-at-point)
                 (lambda () (format "rev%d" (point))))
                ((symbol-function 'deactivate-mark) (lambda (&optional _) nil)))
        (majutsu-org-store-link))
      (should (equal (mapcar #'car org-stored-links)
                     '("majutsu-rev:/repo/::rev2"
                       "majutsu-rev:/repo/::rev1"))))))

(ert-deftest majutsu-org-open-repository/opens-log-for-directory ()
  (let (directory)
    (cl-letf (((symbol-function 'majutsu-log)
               (lambda (&optional dir) (setq directory dir))))
      (majutsu-org-open-repository "/repo/"))
    (should (equal directory "/repo/"))))

(ert-deftest majutsu-org-open-revision/diffs-revision-in-repository ()
  (let (revision directory)
    (cl-letf (((symbol-function 'majutsu-diff-revset)
               (lambda (rev &rest _args)
                 (setq revision rev
                       directory default-directory))))
      (majutsu-org-open-revision "/repo/::abc123"))
    (should (equal revision "abc123"))
    (should (equal directory "/repo/"))))

(ert-deftest majutsu-org-open-revision/preserves-range-revsets-and-decodes ()
  (let (revision directory)
    (cl-letf (((symbol-function 'majutsu-diff-revset)
               (lambda (rev &rest _args)
                 (setq revision rev
                       directory default-directory))))
      (majutsu-org-open-revision "/my%20repo/::main::@")
      (should (equal directory "/my repo/"))
      (should (equal revision "main::@"))
      (majutsu-org-open-revision "/my%20repo/::main%3A%3A%40")
      (should (equal directory "/my repo/"))
      (should (equal revision "main::@")))))

(ert-deftest majutsu-org-store-revision/encodes-components ()
  (majutsu-org-test--with-buffer "/my repo/" "main::@"
    (let ((majutsu-org-prompt-for-symbolic-revisions nil)
          (majutsu-org-revision-storage 'symbolic))
      (should (majutsu-org-store-revision))
      (should (equal (plist-get org-store-link-plist :link)
                     "majutsu-rev:/my%20repo/::main%3A%3A%40")))))

(ert-deftest majutsu-org-store-revision/canonical-and-prefix-overrides ()
  (majutsu-org-test--with-buffer "/repo/" "main"
    (let ((majutsu-org-prompt-for-symbolic-revisions nil))
      (cl-letf (((symbol-function 'majutsu-jj-lines)
                 (lambda (&rest args)
                   (if (string-match-p "commit_id" (car (last args)))
                       '("0123456789abcdef0123456789abcdef01234567")
                     '("zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")))))
        (let ((current-prefix-arg nil))
          (majutsu-org-store-revision)
          (should (equal (plist-get org-store-link-plist :link)
                         "majutsu-rev:/repo/::zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")))
        (let ((current-prefix-arg '(4)))
          (majutsu-org-store-revision)
          (should (equal (plist-get org-store-link-plist :link)
                         "majutsu-rev:/repo/::0123456789abcdef0123456789abcdef01234567")))
        (let ((current-prefix-arg '(16)))
          (majutsu-org-store-revision)
          (should (equal (plist-get org-store-link-plist :link)
                         "majutsu-rev:/repo/::main")))))))

(ert-deftest majutsu-org-store-revision/prompts-for-bookmark-or-tag ()
  (dolist (case `(("bookmark-only"
                   ,(concat "zzzzzzzzzzzzzzzz" "\x1e" "0123456789abcdef" "\x1e" "main" "\x1e")
                   "Bookmark: main"
                   "main")
                  ("tag-only"
                   ,(concat "zzzzzzzzzzzzzzzz" "\x1e" "0123456789abcdef" "\x1e" "\x1e" "v1.0")
                   "Tag: v1.0"
                   "v1.0")))
    (pcase-let ((`(,_label ,line ,choice ,expected) case))
      (majutsu-org-test--with-buffer "/repo/" "rev"
        (let ((majutsu-org-prompt-for-symbolic-revisions t)
              (current-prefix-arg nil)
              prompted-choices)
          (cl-letf (((symbol-function 'majutsu-jj-lines)
                     (lambda (&rest _args) (list line)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _args)
                       (setq prompted-choices (mapcar #'car choices))
                       choice)))
            (majutsu-org-store-revision)
            (should (member choice prompted-choices))
            (should (equal (plist-get org-store-link-plist :link)
                           (format "majutsu-rev:/repo/::%s" expected)))))))))

(ert-deftest majutsu-org-log/stores-and-opens-filtered-log ()
  (require 'majutsu-log)
  (with-temp-buffer
    (delay-mode-hooks (majutsu-log-mode))
    (setq-local majutsu-buffer-log-args '("--revisions=main::@"))
    (cl-letf (((symbol-function 'majutsu--buffer-root) (lambda (&optional _) "/repo/"))
              ((symbol-function 'org-link-store-props)
               (lambda (&rest plist)
                 (setq org-store-link-plist plist)
                 plist)))
      (should (majutsu-org-store-log))
      (should (equal (plist-get org-store-link-plist :type) "majutsu-log"))
      (should (equal (plist-get org-store-link-plist :link)
                     "majutsu-log:/repo/::main%3A%3A%40"))))
  (let (captured-mode captured-locked captured-bindings captured-dir)
    (cl-letf (((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings)
                 (setq captured-mode mode
                       captured-locked locked
                       captured-bindings bindings
                       captured-dir default-directory))))
      (majutsu-org-open-log "/repo/::main%3A%3A%40")
      (should (eq captured-mode #'majutsu-log-mode))
      (should (eq captured-locked t))
      (should (equal captured-dir "/repo/"))
      (should (equal (cadr (assq 'majutsu-buffer-log-args captured-bindings))
                     '("--revisions=main::@"))))))

(ert-deftest majutsu-org-file/stores-and-opens-blob-at-line ()
  (require 'majutsu-file)
  (should (eq (lookup-key majutsu-blob-mode-map [remap org-store-link])
              #'majutsu-org-store-link))
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((majutsu-blob-mode t)
          (majutsu-buffer-blob-path "lisp/example.el")
          (majutsu-org-revision-storage 'change-id))
      (cl-letf (((symbol-function 'majutsu--buffer-root) (lambda (&optional _) "/repo/"))
                ((symbol-function 'majutsu-revision-at-point) (lambda () "abc123"))
                ((symbol-function 'majutsu-jj-lines)
                 (lambda (&rest _args) '("zzzzzzzzzzzzzzzz")))
                ((symbol-function 'org-link-store-props)
                 (lambda (&rest plist)
                   (setq org-store-link-plist plist)
                   plist)))
        (should (majutsu-org-store-file))
        (should (equal (plist-get org-store-link-plist :type) "majutsu-file"))
        (should (equal (plist-get org-store-link-plist :link)
                       "majutsu-file:/repo/::zzzzzzzzzzzzzzzz::lisp/example.el::2")))))
  (with-temp-buffer
    (insert "first\nsecond\nthird\n")
    (let ((blob-buffer (current-buffer))
          opened-dir opened-rev opened-file)
      (cl-letf (((symbol-function 'majutsu-find-file)
                 (lambda (rev file)
                   (setq opened-dir default-directory
                         opened-rev rev
                         opened-file file)
                   blob-buffer)))
        (majutsu-org-open-file "/repo/::zzzzzzzzzzzzzzzz::lisp/example.el::3")
        (should (equal opened-dir "/repo/"))
        (should (equal opened-rev "zzzzzzzzzzzzzzzz"))
        (should (equal opened-file "lisp/example.el"))
        (should (= (line-number-at-pos) 3))))))

(provide 'majutsu-org-test)
;;; majutsu-org-test.el ends here
