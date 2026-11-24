;;; majutsu-org.el --- Org links to Majutsu buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc, hypermedia
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library defines the Org link types `majutsu' and `majutsu-rev',
;; in the spirit of `orgit' for Magit:
;;
;;   majutsu:/path/to/repo/            opens the repository's log
;;   majutsu-rev:/path/to/repo/::REV   opens the revision
;;
;; Use `org-store-link' inside a Majutsu buffer to store a link and
;; `org-insert-link' to insert it.  When the region selects several
;; revisions, one `majutsu-rev' link is stored for each.  Loading
;; `majutsu' registers this integration once Org becomes available.

;;; Code:

(require 'majutsu-mode)
(require 'org)
(require 'url-util)

(declare-function majutsu-log "majutsu-log" (&optional directory))
(declare-function majutsu-diff-revset "majutsu-diff"
                  (revset &optional args range filesets))
(declare-function majutsu-jj-lines "majutsu-jj" (&rest args))
(declare-function majutsu-read-revset "majutsu-jj" (prompt &rest args))

(defgroup majutsu-org nil
  "Org link integration for Majutsu."
  :group 'majutsu)

(defcustom majutsu-org-revision-storage 'change-id
  "How `majutsu-org' stores single-commit revisions in links.

`change-id' stores the full JJ change ID so links follow logical changes
across rewrites.  `commit-id' stores the immutable commit ID.  `symbolic'
preserves the revision string verbatim.  Multi-revision revsets are always
stored symbolically.

Prefix arguments to `org-store-link' override this setting: `C-u' stores an
immutable commit ID and `C-u C-u' stores the symbolic revision."
  :group 'majutsu-org
  :type '(choice (const :tag "Change ID" change-id)
                 (const :tag "Commit ID" commit-id)
                 (const :tag "Symbolic revision" symbolic)))

(defcustom majutsu-org-prompt-for-symbolic-revisions t
  "Whether to prompt for revision identity when storing an Org link.

When t, prompt for change ID, commit ID, or bookmark/tag name whenever a
single commit has bookmarks or tags.  When `always', always prompt for
single-commit links.  When nil, never prompt and follow
`majutsu-org-revision-storage'.  Prefix arguments bypass prompting."
  :group 'majutsu-org
  :type '(choice (const :tag "When commit has bookmarks or tags" t)
                 (const :tag "Always" always)
                 (const :tag "Never" nil)))

(defvar majutsu-org--store-revision-only nil
  "When non-nil, offer only the `majutsu-rev' store function.
Bound while storing one link per revision in an active region.")

(defun majutsu-org--root ()
  "Return the current repository root as an absolute directory name."
  (when-let* ((root (or (majutsu--buffer-root)
                        (majutsu-toplevel))))
    (file-name-as-directory (expand-file-name root))))

(defun majutsu-org--encode-component (value)
  "Percent-encode VALUE for one component of a Majutsu Org link.
Keep directory separators readable."
  (replace-regexp-in-string "%2F" "/" (url-hexify-string value) t t))

(defun majutsu-org--decode-component (value)
  "Decode percent-encoded VALUE from a Majutsu Org link component."
  (url-unhex-string value))

(defun majutsu-org--split-revision-path (path &optional link-type)
  "Split PATH into (REPOSITORY . REVISION) at the first `::'.
Preserves JJ revsets containing `::' and decodes both components.
LINK-TYPE is used in error messages."
  (if-let* ((separator (string-search "::" path))
            (repository (substring path 0 separator))
            (revision (substring path (+ separator 2)))
            ((not (string-empty-p repository)))
            ((not (string-empty-p revision))))
      (cons (majutsu-org--decode-component repository)
            (majutsu-org--decode-component revision))
    (user-error "Invalid %s link: %s" (or link-type "majutsu-rev") path)))

(defun majutsu-org--revision-storage-kind ()
  "Return the requested revision storage kind for the current command."
  (pcase current-prefix-arg
    ('(4) 'commit-id)
    ('(16) 'symbolic)
    (_ majutsu-org-revision-storage)))

(defun majutsu-org--revision-metadata (root revision)
  "Return identity metadata when REVISION in ROOT selects one commit."
  (let* ((default-directory root)
         (field-separator "\x1e")
         (list-separator "\x1f")
         (template
          (concat "change_id ++ \"\\x1e\" ++ commit_id ++ \"\\x1e\" ++ "
                  "bookmarks.map(|x| x.name()).join(\"\\x1f\") ++ "
                  "\"\\x1e\" ++ tags.map(|x| x.name()).join(\"\\x1f\") ++ "
                  "\"\\n\""))
         (lines (ignore-errors
                  (majutsu-jj-lines "log" "--no-graph" "-r" revision
                                    "-T" template))))
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

(defun majutsu-org--canonical-revision (root revision kind)
  "Return REVISION in ROOT converted to the identity specified by KIND.
If REVISION selects other than one commit, preserve it symbolically."
  (if (eq kind 'symbolic)
      revision
    (let* ((default-directory root)
           (template (pcase kind
                       ('change-id "change_id ++ \"\\n\"")
                       ('commit-id "commit_id ++ \"\\n\"")))
           (values (ignore-errors
                     (majutsu-jj-lines "log" "--no-graph" "-r" revision
                                       "-T" template))))
      (if (= (length values) 1)
          (car values)
        revision))))

(defun majutsu-org--select-stored-revision (root source-revision)
  "Select the identity to store for SOURCE-REVISION in ROOT."
  (let ((kind (majutsu-org--revision-storage-kind)))
    (if (or current-prefix-arg
            (null majutsu-org-prompt-for-symbolic-revisions))
        (majutsu-org--canonical-revision root source-revision kind)
      (if-let* ((metadata (majutsu-org--revision-metadata root source-revision)))
          (let ((bookmarks (plist-get metadata :bookmarks))
                (tags (plist-get metadata :tags)))
            (if (or (eq majutsu-org-prompt-for-symbolic-revisions 'always)
                    bookmarks tags)
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
              (majutsu-org--canonical-revision root source-revision kind)))
        (majutsu-org--canonical-revision root source-revision kind)))))

(defun majutsu-org--store-revision (root revision)
  "Store a `majutsu-rev' link to REVISION in ROOT."
  (let* ((source (substring-no-properties revision))
         (stored (majutsu-org--select-stored-revision root source)))
    (org-link-store-props
     :type "majutsu-rev"
     :link (format "majutsu-rev:%s::%s"
                   (majutsu-org--encode-component root)
                   (majutsu-org--encode-component stored))
     :description (format "%s::%s" (abbreviate-file-name root) stored))))

(defun majutsu-org-store-revision ()
  "Store a `majutsu-rev' link to the revision at point."
  (when-let* ((revision (and (derived-mode-p 'majutsu-mode)
                             (majutsu-revision-at-point)))
              (root (majutsu-org--root)))
    (majutsu-org--store-revision root revision)))

(defun majutsu-org-store-repository ()
  "Store a `majutsu' link to the current repository."
  (when (and (derived-mode-p 'majutsu-mode)
             (not majutsu-org--store-revision-only))
    (when-let* ((root (majutsu-org--root)))
      (org-link-store-props
       :type "majutsu"
       :link (format "majutsu:%s" (majutsu-org--encode-component root))
       :description (format "%s (majutsu)" (abbreviate-file-name root))))))

(defun majutsu-org-store-link (&optional _arg)
  "Store Org links to the selected or current Majutsu revisions.

With several revisions selected in the region, store one
`majutsu-rev' link per revision.  Otherwise behave like
`org-store-link'; when both the repository and a revision are
available, Org asks which link to store, preferring the revision."
  (interactive "P")
  (if-let* ((sections (magit-region-sections 'jj-commit)))
      (save-excursion
        (let ((majutsu-org--store-revision-only t))
          (dolist (section sections)
            (goto-char (oref section start))
            (set-mark (point))
            (activate-mark)
            (call-interactively #'org-store-link)))
        (deactivate-mark))
    (call-interactively #'org-store-link)))

(defun majutsu-org-open-repository (path)
  "Open the Majutsu log for the repository at PATH."
  (majutsu-log (file-name-as-directory
                (expand-file-name (majutsu-org--decode-component path)))))

(defun majutsu-org-open-revision (path)
  "Open the revision encoded as REPO::REV in PATH."
  (pcase-let* ((`(,repository . ,revision)
                (majutsu-org--split-revision-path path "majutsu-rev"))
               (default-directory (file-name-as-directory
                                   (expand-file-name repository))))
    (majutsu-diff-revset revision)))

(defun majutsu-org-complete-repository (&optional _arg)
  "Complete a `majutsu' link by reading a repository directory."
  (concat "majutsu:"
          (majutsu-org--encode-component
           (file-name-as-directory
            (expand-file-name
             (read-directory-name "Repository: " (majutsu-org--root)))))))

(defun majutsu-org-complete-revision (&optional _arg)
  "Complete a `majutsu-rev' link by reading a repository and revision."
  (let* ((root (or (majutsu-org--root)
                   (file-name-as-directory
                    (expand-file-name (read-directory-name "Repository: ")))))
         (default-directory root))
    (format "majutsu-rev:%s::%s"
            (majutsu-org--encode-component root)
            (majutsu-org--encode-component (majutsu-read-revset "Revision")))))

(defun majutsu-org-setup ()
  "Register the Majutsu Org link types and the `org-store-link' remap."
  (org-link-set-parameters "majutsu-rev"
                           :store #'majutsu-org-store-revision
                           :follow #'majutsu-org-open-revision
                           :complete #'majutsu-org-complete-revision)
  (org-link-set-parameters "majutsu"
                           :store #'majutsu-org-store-repository
                           :follow #'majutsu-org-open-repository
                           :complete #'majutsu-org-complete-repository)
  (keymap-set majutsu-mode-map "<remap> <org-store-link>" #'majutsu-org-store-link))

(majutsu-org-setup)

(provide 'majutsu-org)
;;; majutsu-org.el ends here
