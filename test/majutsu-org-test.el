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
    (let (properties)
      (cl-letf (((symbol-function 'majutsu-revision-at-point)
                 (lambda () "change-id"))
                ((symbol-function 'org-link-store-props)
                 (lambda (&rest props) (setq properties props))))
        (majutsu-org-revision-store))
      (should (equal properties
                     '(:type "majutsu-rev"
                       :link "majutsu-rev:/tmp/repo/::change-id"
                       :description "/tmp/repo/ (majutsu-rev change-id)"))))))

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
  (should (eq (command-remapping #'org-store-link nil majutsu-mode-map)
              #'majutsu-org-store-link)))

(provide 'majutsu-org-test)
;;; majutsu-org-test.el ends here
