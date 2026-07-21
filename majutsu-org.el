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

(require 'majutsu-diff)
(require 'majutsu-log)
(require 'org)
(require 'subr-x)

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

(defun majutsu-org--split-revision-path (path)
  "Split a majutsu revision link PATH into repository and revision.

Split at the first `::' so JJ revsets containing that operator are
preserved intact."
  (if-let* ((separator (string-search "::" path))
            (repository (substring path 0 separator))
            (revision (substring path (+ separator 2)))
            ((not (string-empty-p repository)))
            ((not (string-empty-p revision))))
      (cons repository revision)
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
       :link (concat "majutsu:" repository)
       :description (format "%s (majutsu)" repository)))))

(defun majutsu-org-repository-follow (repository _arg)
  "Open the Majutsu log for REPOSITORY."
  (let* ((directory (file-name-as-directory (expand-file-name repository)))
         (root (majutsu-toplevel directory)))
    (unless root
      (user-error "%s is not inside a JJ repository" directory))
    (majutsu-log root)))

(defun majutsu-org-repository-complete (&optional _arg)
  "Complete a link to a Majutsu repository log."
  (concat "majutsu:" (abbreviate-file-name (majutsu-org--read-repository))))

(defun majutsu-org-revision-store ()
  "Store a link to the JJ revision at point in a Majutsu buffer."
  (when (derived-mode-p 'majutsu-mode)
    (when-let* ((revision (majutsu-revision-at-point))
                (repository (majutsu-org--repository)))
      (org-link-store-props
       :type "majutsu-rev"
       :link (format "majutsu-rev:%s::%s" repository revision)
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
            (abbreviate-file-name repository)
            revision)))

(provide 'majutsu-org)
;;; majutsu-org.el ends here
