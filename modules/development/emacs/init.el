;; -*- lexical-binding: t -*-
;;; Code:
;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(use-package org
  :init
  (auto-fill-mode -1)
  :config
  (setq org-hide-leading-stars             t
        org-hide-macro-markers             t
        org-ellipsis                       " ⤵"
        org-image-actual-width             600
        org-redisplay-inline-images        t
        org-display-inline-images          t
        org-startup-with-inline-images     "inlineimages"
        org-pretty-entities                t
        org-fontify-whole-heading-line     t
        org-fontify-done-headline          t
        org-fontify-quote-and-verse-blocks t
        org-startup-indented               t
        org-startup-align-all-tables       t
        org-use-property-inheritance       t
        org-list-allow-alphabetical        t
        org-M-RET-may-split-line           nil
        org-src-window-setup               'split-window-below
        org-src-fontify-natively           t
        org-src-tab-acts-natively          t
        org-src-preserve-indentation       t
        org-log-done                       'time
        org-directory "~/org"
	diary-file "~/org/diary"
        org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 10))
        org-agenda-files (list
                          "~/org/todo.org"
                          "~/org/work/todo.org"
                          "~/org/programming/todo.org"
                          "~/org/rpgs/todo.org"
                          "~/org/contacts.org.gpg")
        calendar-date-style 'european
        mark-diary-entries-in-calendar t
        org-confirm-babel-evaluate nil)
  )
(let* ((dotfile-dir (file-name-directory (or (buffer-file-name)
					     load-file-name)))
       (config-org  (expand-file-name "config.org" dotfile-dir))
       (config-el   (expand-file-name "config.el"  dotfile-dir)))
  (require 'ob-tangle)
  ;;tangle and load if newer than compiled
  (if (or (not (file-exists-p config-el))
          (file-newer-than-file-p config-org config-el))
      (org-babel-load-file config-org t)
    (load-file config-el)))
