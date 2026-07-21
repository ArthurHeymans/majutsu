;;; majutsu-org.el --- Org links to Majutsu buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: hypermedia, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library defines Org links for Majutsu repositories, revisions,
;; revision-filtered logs, and historical file locations.

;;; Code:

(require 'cl-lib)
(require 'majutsu-diff)
(require 'majutsu-file)
(require 'majutsu-log)
(require 'org)
(require 'subr-x)
(require 'url-util)

(defgroup majutsu-org nil
  "Org links to Majutsu buffers."
  :group 'majutsu
  :group 'org-link)

(defcustom majutsu-org-prompt-for-symbolic-revisions t
  "Whether to offer bookmarks and tags when storing revision links.

When t, prompt only when the commit has a bookmark or tag.  When
`always', prompt for every single commit.  When nil, always use
`majutsu-org-revision-storage'.  Prefix arguments bypass the prompt."
  :group 'majutsu-org
  :type '(choice (const :tag "Never prompt" nil)
                 (const :tag "Prompt when symbols exist" t)
                 (const :tag "Always prompt" always)))

(defcustom majutsu-org-revision-storage 'change-id
  "Identity stored in `majutsu-rev' links.

`change-id' follows a logical JJ change across rewrites.  `commit-id'
stores an immutable commit, while `symbolic' preserves the bookmark,
tag, revset, or other revision expression found at point.  A single
universal prefix argument stores a commit id, and two preserve the
symbolic revision regardless of this option."
  :group 'majutsu-org
  :type '(choice (const :tag "Change ID" change-id)
                 (const :tag "Commit ID" commit-id)
                 (const :tag "Symbolic revision" symbolic)))

;;;###autoload
(with-eval-after-load 'majutsu-mode
  (keymap-set majutsu-mode-map "<remap> <org-store-link>"
              #'majutsu-org-store-link))

;;;###autoload
(with-eval-after-load 'majutsu-file
  (keymap-set majutsu-blob-mode-map "<remap> <org-store-link>"
              #'majutsu-org-store-link))

;;;###autoload
(defun majutsu-org-store-link ()
  "Store an Org link for the current Majutsu buffer."
  (interactive)
  (call-interactively #'org-store-link))

(defun majutsu-org--repository ()
  "Return the repository belonging to the current Majutsu buffer."
  (when-let* ((root (or (bound-and-true-p majutsu--default-directory)
                        (majutsu-toplevel))))
    (abbreviate-file-name (file-name-as-directory root))))

(defun majutsu-org--read-repository (&optional initial)
  "Read and return a JJ repository directory, starting at INITIAL."
  (let* ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Repository: " initial nil t))))
         (root (majutsu-toplevel directory)))
    (unless root
      (user-error "%s is not inside a JJ repository" directory))
    (file-name-as-directory root)))

(defun majutsu-org--encode-component (value)
  "Encode VALUE for use as one component of a Majutsu Org link."
  (replace-regexp-in-string "%2F" "/" (url-hexify-string value) t t))

(defun majutsu-org--decode-component (value)
  "Decode VALUE from a Majutsu Org link component."
  (url-unhex-string value))

(defun majutsu-org--split-revision-path (path)
  "Split a majutsu revision link PATH into repository and revision.

Split at the first `::' so JJ revsets containing that operator are
preserved intact."
  (if-let* ((separator (string-search "::" path))
            (repository (substring path 0 separator))
            (revision (substring path (+ separator 2)))
            ((not (string-empty-p repository)))
            ((not (string-empty-p revision))))
      (cons (majutsu-org--decode-component repository)
            (majutsu-org--decode-component revision))
    (user-error "Invalid majutsu-rev link: %s" path)))

;;;###autoload
(with-eval-after-load 'org
  (org-link-set-parameters "majutsu"
                           :store #'majutsu-org-repository-store
                           :follow #'majutsu-org-repository-follow
                           :complete #'majutsu-org-repository-complete)
  (org-link-set-parameters "majutsu-rev"
                           :store #'majutsu-org-revision-store
                           :follow #'majutsu-org-revision-follow
                           :complete #'majutsu-org-revision-complete)
  (org-link-set-parameters "majutsu-log"
                           :store #'majutsu-org-log-store
                           :follow #'majutsu-org-log-follow
                           :complete #'majutsu-org-log-complete)
  (org-link-set-parameters "majutsu-file"
                           :store #'majutsu-org-file-store
                           :follow #'majutsu-org-file-follow
                           :complete #'majutsu-org-file-complete))

(defun majutsu-org-repository-store ()
  "Store a link to the current Majutsu repository log."
  (when (and (derived-mode-p 'majutsu-mode)
             (not (majutsu-revision-at-point))
             (not (and (derived-mode-p 'majutsu-log-mode)
                       (majutsu-org--log-revset))))
    (when-let* ((repository (majutsu-org--repository)))
      (org-link-store-props
       :type "majutsu"
       :link (concat "majutsu:" (majutsu-org--encode-component repository))
       :description (format "%s (majutsu)" repository)))))

(defun majutsu-org-repository-follow (repository _arg)
  "Open the Majutsu log for REPOSITORY."
  (let* ((repository (majutsu-org--decode-component repository))
         (directory (file-name-as-directory (expand-file-name repository)))
         (root (majutsu-toplevel directory)))
    (unless root
      (user-error "%s is not inside a JJ repository" directory))
    (majutsu-log root)))

(defun majutsu-org-repository-complete (&optional _arg)
  "Complete a link to a Majutsu repository log."
  (concat "majutsu:"
          (majutsu-org--encode-component
           (abbreviate-file-name (majutsu-org--read-repository)))))

(defun majutsu-org--log-revset ()
  "Return the revision filter represented by the current log buffer."
  (when (derived-mode-p 'majutsu-log-mode)
    (when-let* ((argument
                 (seq-find (lambda (item)
                             (string-prefix-p "--revisions=" item))
                           majutsu-buffer-log-args)))
      (substring argument (length "--revisions=")))))

(defun majutsu-org-log-store ()
  "Store the revision-filtered view of the current Majutsu log."
  (when (and (derived-mode-p 'majutsu-log-mode)
             (not (majutsu-revision-at-point)))
    (when-let* ((revision (majutsu-org--log-revset))
                (repository (majutsu-org--repository)))
      (org-link-store-props
       :type "majutsu-log"
       :link (format "majutsu-log:%s::%s"
                     (majutsu-org--encode-component repository)
                     (majutsu-org--encode-component revision))
       :description (format "%s (majutsu-log %s)" repository revision)))))

(defun majutsu-org-log-follow (path _arg)
  "Open the revision-filtered Majutsu log identified by PATH."
  (pcase-let* ((`(,repository . ,revision)
                 (majutsu-org--split-revision-path path))
                (default-directory
                 (file-name-as-directory (expand-file-name repository))))
    (unless (majutsu-toplevel default-directory)
      (user-error "%s is not inside a JJ repository" default-directory))
    (majutsu-setup-buffer #'majutsu-log-mode t
      (majutsu-buffer-log-args (list (concat "--revisions=" revision)))
      (majutsu-buffer-log-filesets nil))))

(defun majutsu-org-log-complete (&optional _arg)
  "Complete a link to a revision-filtered Majutsu log."
  (let* ((repository (majutsu-org--read-repository))
         (default-directory repository)
         (revision (majutsu-read-revset "Log revset")))
    (format "majutsu-log:%s::%s"
            (majutsu-org--encode-component
             (abbreviate-file-name repository))
            (majutsu-org--encode-component revision))))

(defun majutsu-org--parse-file-path (path)
  "Parse majutsu-file link PATH into repository, revision, file and line."
  (pcase (split-string path "::")
    (`(,repository ,revision ,file ,line)
     (let ((line (string-to-number line)))
       (unless (> line 0)
         (user-error "Invalid line in majutsu-file link: %s" path))
       (list (majutsu-org--decode-component repository)
             (majutsu-org--decode-component revision)
             (majutsu-org--decode-component file)
             line)))
    (_ (user-error "Invalid majutsu-file link: %s" path))))

(defun majutsu-org-file-store ()
  "Store a link to the current Majutsu blob and line."
  (when (and (bound-and-true-p majutsu-blob-mode)
             (bound-and-true-p majutsu-buffer-blob-path))
    (when-let* ((repository (majutsu-org--repository))
                (source-revision (majutsu-revision-at-point))
                (revision (majutsu-org--canonical-revision
                           source-revision
                           (majutsu-org--revision-storage-kind)))
                (file majutsu-buffer-blob-path)
                (line (line-number-at-pos)))
      (org-link-store-props
       :type "majutsu-file"
       :link (format "majutsu-file:%s::%s::%s::%d"
                     (majutsu-org--encode-component repository)
                     (majutsu-org--encode-component revision)
                     (majutsu-org--encode-component file)
                     line)
       :description (format "%s:%d (%s)" file line revision)))))

(defun majutsu-org-file-follow (path _arg)
  "Open the Majutsu file and line identified by PATH."
  (pcase-let* ((`(,repository ,revision ,file ,line)
                 (majutsu-org--parse-file-path path))
                (default-directory
                 (file-name-as-directory (expand-file-name repository))))
    (unless (majutsu-toplevel default-directory)
      (user-error "%s is not inside a JJ repository" default-directory))
    (let ((buffer (majutsu-find-file revision file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun majutsu-org-file-complete (&optional _arg)
  "Complete a link to a file at a JJ revision."
  (let* ((repository (majutsu-org--read-repository))
         (default-directory repository)
         (revision (majutsu-read-revset "Revision"))
         (file (majutsu-file--read-path revision repository))
         (line (read-number "Line: " 1)))
    (format "majutsu-file:%s::%s::%s::%d"
            (majutsu-org--encode-component
             (abbreviate-file-name repository))
            (majutsu-org--encode-component revision)
            (majutsu-org--encode-component file)
            line)))

(defun majutsu-org--revision-storage-kind ()
  "Return the requested revision storage kind for the current command."
  (pcase current-prefix-arg
    ('(4) 'commit-id)
    ('(16) 'symbolic)
    (_ majutsu-org-revision-storage)))

(defun majutsu-org--revision-metadata (revision)
  "Return identity metadata when REVISION selects exactly one commit."
  (let* ((field-separator "\x1e")
         (list-separator "\x1f")
         (template
          (concat "change_id ++ \"\\x1e\" ++ commit_id ++ \"\\x1e\" ++ "
                  "bookmarks.map(|x| x.name()).join(\"\\x1f\") ++ "
                  "\"\\x1e\" ++ tags.map(|x| x.name()).join(\"\\x1f\") ++ "
                  "\"\\n\""))
         (lines (majutsu-jj-lines "log" "--no-graph" "-r" revision
                                  "-T" template)))
    (when (= (length lines) 1)
      (pcase (split-string (car lines) field-separator nil)
        (`(,change-id ,commit-id ,bookmarks ,tags)
         (list :change-id change-id
               :commit-id commit-id
               :bookmarks (split-string bookmarks list-separator t)
               :tags (split-string tags list-separator t)))))))

(defun majutsu-org--short-revision (revision)
  "Return a compact display form of REVISION."
  (substring revision 0 (min 12 (length revision))))

(defun majutsu-org--select-stored-revision (source-revision)
  "Select the identity to store for SOURCE-REVISION."
  (let ((kind (majutsu-org--revision-storage-kind)))
    (if (or current-prefix-arg
            (null majutsu-org-prompt-for-symbolic-revisions))
        (majutsu-org--canonical-revision source-revision kind)
      (if-let* ((metadata (majutsu-org--revision-metadata source-revision))
                (bookmarks (plist-get metadata :bookmarks))
                (tags (plist-get metadata :tags))
                ((or (eq majutsu-org-prompt-for-symbolic-revisions 'always)
                     bookmarks tags)))
          (let* ((change-id (plist-get metadata :change-id))
                 (commit-id (plist-get metadata :commit-id))
                 (choices
                  (append
                   `((,(format "Change ID: %s"
                               (majutsu-org--short-revision change-id))
                      . ,change-id)
                     (,(format "Commit ID: %s"
                               (majutsu-org--short-revision commit-id))
                      . ,commit-id))
                   (mapcar (lambda (bookmark)
                             (cons (format "Bookmark: %s" bookmark) bookmark))
                           bookmarks)
                   (mapcar (lambda (tag)
                             (cons (format "Tag: %s" tag) tag))
                           tags)))
                 (default-value
                  (pcase kind
                    ('commit-id commit-id)
                    ('symbolic source-revision)
                    (_ change-id)))
                 (default-label
                  (car (rassoc default-value choices)))
                 (selection
                  (completing-read "Store revision as: " choices nil t nil nil
                                   default-label)))
            (alist-get selection choices nil nil #'equal))
        (majutsu-org--canonical-revision source-revision kind)))))

(defun majutsu-org--canonical-revision (revision kind)
  "Return REVISION converted to the identity specified by KIND.

If REVISION selects other than one commit, preserve it symbolically."
  (if (eq kind 'symbolic)
      revision
    (let* ((template (pcase kind
                       ('change-id "change_id ++ \"\\n\"")
                       ('commit-id "commit_id ++ \"\\n\"")))
           (values (majutsu-jj-lines "log" "--no-graph" "-r" revision
                                     "-T" template)))
      (if (= (length values) 1)
          (car values)
        revision))))

(defun majutsu-org--revision-store-1 (source-revision repository)
  "Store SOURCE-REVISION from REPOSITORY as one `majutsu-rev' link."
  (let ((revision (majutsu-org--select-stored-revision source-revision)))
    (org-link-store-props
     :type "majutsu-rev"
     :link (format "majutsu-rev:%s::%s"
                   (majutsu-org--encode-component repository)
                   (majutsu-org--encode-component revision))
     :description (format "%s (majutsu-rev %s)" repository revision))))

(defun majutsu-org-revision-store ()
  "Store links to revisions at point or selected in a Majutsu buffer."
  (when (and (derived-mode-p 'majutsu-mode)
             (not (bound-and-true-p majutsu-buffer-blob-path)))
    (when-let* ((repository (majutsu-org--repository))
                (revisions (or (magit-region-values 'jj-commit t)
                               (when-let* ((revision
                                            (majutsu-revision-at-point)))
                                 (list revision)))))
      (mapc (lambda (revision)
              (majutsu-org--revision-store-1 revision repository))
            revisions)
      t)))

(defun majutsu-org-revision-follow (path _arg)
  "Open the revision diff identified by majutsu-rev link PATH."
  (pcase-let* ((`(,repository . ,revision)
                 (majutsu-org--split-revision-path path))
                (default-directory
                 (file-name-as-directory (expand-file-name repository))))
    (unless (majutsu-toplevel default-directory)
      (user-error "%s is not inside a JJ repository" default-directory))
    (majutsu-diff-revset revision)))

(defun majutsu-org-revision-complete (&optional _arg)
  "Complete a link to a Majutsu revision diff."
  (let* ((repository (majutsu-org--read-repository))
         (default-directory repository)
         (revision (majutsu-read-revset "Revision")))
    (format "majutsu-rev:%s::%s"
            (majutsu-org--encode-component
             (abbreviate-file-name repository))
            (majutsu-org--encode-component revision))))

(provide 'majutsu-org)
;;; majutsu-org.el ends here
