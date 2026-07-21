;;; majutsu-org.el --- Org links to Majutsu buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: hypermedia, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library defines the Org link types `majutsu' and `majutsu-rev'.
;; The former opens a repository log and the latter opens a revision diff.

;;; Code:

(require 'cl-lib)
(require 'majutsu-diff)
(require 'majutsu-log)
(require 'org)
(require 'subr-x)
(require 'url-util)

(defgroup majutsu-org nil
  "Org links to Majutsu buffers."
  :group 'majutsu
  :group 'org-link)

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
                           :complete #'majutsu-org-revision-complete))

(defun majutsu-org-repository-store ()
  "Store a link to the current Majutsu repository log."
  (when (and (derived-mode-p 'majutsu-mode)
             (not (majutsu-revision-at-point)))
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

(defun majutsu-org--revision-storage-kind ()
  "Return the requested revision storage kind for the current command."
  (pcase current-prefix-arg
    ('(4) 'commit-id)
    ('(16) 'symbolic)
    (_ majutsu-org-revision-storage)))

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

(defun majutsu-org-revision-store ()
  "Store a link to the JJ revision at point in a Majutsu buffer."
  (when (derived-mode-p 'majutsu-mode)
    (when-let* ((source-revision (majutsu-revision-at-point))
                (repository (majutsu-org--repository))
                (revision (majutsu-org--canonical-revision
                           source-revision
                           (majutsu-org--revision-storage-kind))))
      (org-link-store-props
       :type "majutsu-rev"
       :link (format "majutsu-rev:%s::%s"
                     (majutsu-org--encode-component repository)
                     (majutsu-org--encode-component revision))
       :description (format "%s (majutsu-rev %s)" repository revision)))))

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
