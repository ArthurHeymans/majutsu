;;; majutsu-org-test.el --- Tests for Majutsu Org links  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Org links to Majutsu buffers.

;;; Code:

(require 'ert)
(require 'majutsu-org)

(ert-deftest majutsu-org-repository-store/stores-log-link ()
  (with-temp-buffer
    (majutsu-log-mode)
    (setq-local majutsu--default-directory "/tmp/repo/")
    (let (properties)
      (cl-letf (((symbol-function 'majutsu-revision-at-point) #'ignore)
                ((symbol-function 'org-link-store-props)
                 (lambda (&rest props) (setq properties props))))
        (majutsu-org-repository-store))
      (should (equal properties
                     '(:type "majutsu"
                       :link "majutsu:/tmp/repo/"
                       :description "/tmp/repo/ (majutsu)"))))))

(ert-deftest majutsu-org-repository-store/defers-to-revision-link ()
  (with-temp-buffer
    (majutsu-log-mode)
    (setq-local majutsu--default-directory "/tmp/repo/")
    (cl-letf (((symbol-function 'majutsu-revision-at-point)
               (lambda () "change-id"))
              ((symbol-function 'org-link-store-props)
               (lambda (&rest _) (ert-fail "Unexpected repository link"))))
      (should-not (majutsu-org-repository-store)))))

(ert-deftest majutsu-org-revision-store/stores-revision-at-point ()
  (with-temp-buffer
    (majutsu-log-mode)
    (setq-local majutsu--default-directory "/tmp/repo/")
    (let ((majutsu-org-revision-storage 'symbolic)
          properties)
      (cl-letf (((symbol-function 'majutsu-revision-at-point)
                 (lambda () "change-id"))
                ((symbol-function 'org-link-store-props)
                 (lambda (&rest props) (setq properties props))))
        (majutsu-org-revision-store))
      (should (equal properties
                     '(:type "majutsu-rev"
                       :link "majutsu-rev:/tmp/repo/::change-id"
                       :description "/tmp/repo/ (majutsu-rev change-id)"))))))

(ert-deftest majutsu-org-log-store/stores-filtered-log ()
  (with-temp-buffer
    (majutsu-log-mode)
    (setq-local majutsu--default-directory "/tmp/repo/")
    (setq-local majutsu-buffer-log-args '("--no-graph" "--revisions=main::@"))
    (let (properties)
      (cl-letf (((symbol-function 'majutsu-revision-at-point) #'ignore)
                ((symbol-function 'org-link-store-props)
                 (lambda (&rest props) (setq properties props))))
        (majutsu-org-log-store))
      (should (equal (plist-get properties :link)
                     "majutsu-log:/tmp/repo/::main%3A%3A%40")))))

(ert-deftest majutsu-org-log-follow/restores-revision-filter ()
  (let (captured-bindings)
    (cl-letf (((symbol-function 'majutsu-toplevel)
               (lambda (&optional dir) dir))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (_mode locked bindings &rest _)
                 (should locked)
                 (setq captured-bindings bindings))))
      (majutsu-org-log-follow "/tmp/repo/::main%3A%3A%40" nil))
    (should (equal (cadr (assq 'majutsu-buffer-log-args captured-bindings))
                   '("--revisions=main::@")))))

(ert-deftest majutsu-org-canonical-revision/stores-change-id-by-default ()
  (let ((majutsu-org-revision-storage 'change-id))
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (should (equal (car (last args 2)) "-T"))
                 '("full-change-id"))))
      (should (equal (majutsu-org--canonical-revision
                      "main" (majutsu-org--revision-storage-kind))
                     "full-change-id")))))

(ert-deftest majutsu-org-canonical-revision/prefix-selects-storage-kind ()
  (let ((majutsu-org-revision-storage 'change-id))
    (let ((current-prefix-arg '(4)))
      (should (eq (majutsu-org--revision-storage-kind) 'commit-id)))
    (let ((current-prefix-arg '(16)))
      (should (eq (majutsu-org--revision-storage-kind) 'symbolic)))))

(ert-deftest majutsu-org-canonical-revision/preserves-multi-revision-revset ()
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _) '("one" "two"))))
    (should (equal (majutsu-org--canonical-revision "all()" 'change-id)
                   "all()"))))

(ert-deftest majutsu-org-revision-store/stores-selected-revisions ()
  (with-temp-buffer
    (majutsu-log-mode)
    (setq-local majutsu--default-directory "/tmp/repo/")
    (let ((majutsu-org-revision-storage 'symbolic)
          links)
      (cl-letf (((symbol-function 'magit-region-values)
                 (lambda (&rest _) '("first" "second")))
                ((symbol-function 'org-link-store-props)
                 (lambda (&rest props)
                   (push (plist-get props :link) links))))
        (should (majutsu-org-revision-store)))
      (should (equal (nreverse links)
                     '("majutsu-rev:/tmp/repo/::first"
                       "majutsu-rev:/tmp/repo/::second"))))))

(ert-deftest majutsu-org-repository-follow/opens-log-in-repository ()
  (let (directory)
    (cl-letf (((symbol-function 'majutsu-toplevel)
               (lambda (&optional dir) dir))
              ((symbol-function 'majutsu-log)
               (lambda (dir) (setq directory dir))))
      (majutsu-org-repository-follow "/tmp/repo" nil))
    (should (equal directory "/tmp/repo/"))))

(ert-deftest majutsu-org-repository-follow/rejects-non-repository ()
  (cl-letf (((symbol-function 'majutsu-toplevel) #'ignore)
            ((symbol-function 'majutsu-log)
             (lambda (&rest _) (ert-fail "Unexpected log buffer"))))
    (should-error (majutsu-org-repository-follow "/tmp/not-a-repo" nil)
                  :type 'user-error)))

(ert-deftest majutsu-org-components/round-trip-special-characters ()
  (let* ((value "/tmp/repo:one/with spaces/%done")
         (encoded (majutsu-org--encode-component value)))
    (should-not (string-match-p "[ :]" encoded))
    (should (string-prefix-p "/tmp/" encoded))
    (should (equal (majutsu-org--decode-component encoded) value))))

(ert-deftest majutsu-org-split-revision-path/decodes-components ()
  (should (equal (majutsu-org--split-revision-path
                  "/tmp/repo%3Aone/::main%3A%3A%40")
                 '("/tmp/repo:one/" . "main::@"))))

(ert-deftest majutsu-org-split-revision-path/preserves-revset-operators ()
  (should (equal (majutsu-org--split-revision-path
                  "/tmp/repo/::main::@ & ~tags()")
                 '("/tmp/repo/" . "main::@ & ~tags()"))))

(ert-deftest majutsu-org-split-revision-path/rejects-missing-components ()
  (dolist (path '("/tmp/repo/" "::revision" "/tmp/repo/::"))
    (should-error (majutsu-org--split-revision-path path)
                  :type 'user-error)))

(ert-deftest majutsu-org-revision-follow/opens-revision-diff ()
  (let (directory revision)
    (cl-letf (((symbol-function 'majutsu-toplevel)
               (lambda (&optional dir) dir))
              ((symbol-function 'majutsu-diff-revset)
               (lambda (rev)
                 (setq directory default-directory
                       revision rev))))
      (majutsu-org-revision-follow "/tmp/repo/::change-id" nil))
    (should (equal directory "/tmp/repo/"))
    (should (equal revision "change-id"))))

(ert-deftest majutsu-org-revision-follow/preserves-revset-operators ()
  (let (revision)
    (cl-letf (((symbol-function 'majutsu-toplevel)
               (lambda (&optional dir) dir))
              ((symbol-function 'majutsu-diff-revset)
               (lambda (rev) (setq revision rev))))
      (majutsu-org-revision-follow "/tmp/repo/::main::@" nil))
    (should (equal revision "main::@"))))

(ert-deftest majutsu-org-revision-complete/reads-repository-and-revision ()
  (cl-letf (((symbol-function 'majutsu-org--read-repository)
             (lambda (&optional _) "/tmp/repo/"))
            ((symbol-function 'majutsu-read-revset)
             (lambda (&rest _) "change-id")))
    (should (equal (majutsu-org-revision-complete)
                   "majutsu-rev:/tmp/repo/::change-id"))))

(ert-deftest majutsu-org/registers-links-and-store-remapping ()
  (should (eq (org-link-get-parameter "majutsu" :store)
              #'majutsu-org-repository-store))
  (should (eq (org-link-get-parameter "majutsu-rev" :follow)
              #'majutsu-org-revision-follow))
  (should (eq (org-link-get-parameter "majutsu-log" :store)
              #'majutsu-org-log-store))
  (should (eq (command-remapping #'org-store-link nil majutsu-mode-map)
              #'majutsu-org-store-link)))

(provide 'majutsu-org-test)
;;; majutsu-org-test.el ends here
