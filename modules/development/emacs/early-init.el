;; -*- lexical-binding: t -*-
;;; Code:
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box nil ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t
      native-comp-async-report-warnings-errors 'silent
      native-comp-deferred-compilation t)


(setq initial-frame-alist `((horizontal-scroll-bars . nil)
                            (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                            (tool-bar-lines . 0) ; alternative to disabling `tool-bar-mode'
                            (vertical-scroll-bars . nil)
                            (scroll-bar-width . 6)
                            (width . (text-pixels . 800))
                            (height . (text-pixels . 900))
                            (undecorated . t)
                            (border-width . 5)
                            ,@(list '(fullscreen . maximized))))

;; Do it again after init so that any intermediate changes are not
;; retained.  Note that we cannot rely on setting this to
;; `initial-frame-alist' as that may change in the meantime.  We
;; explicitly set the value to be certain of the outcome.  This does
;; not inhibit other programs from modifying the list, though I would
;; consider it undesirable if they were touching these specific
;; settings.
(add-hook 'after-init-hook (lambda ()
                             (setq default-frame-alist `((horizontal-scroll-bars . nil)
                                                         (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                                                         (tool-bar-lines . 0) ; alternative to disabling `tool-bar-mode'
                                                         (vertical-scroll-bars . nil)
                                                         (scroll-bar-width . 6)
                                                         (width . (text-pixels . 800))
                                                         (height . (text-pixels . 900))
                                                         (undecorated . t)
                                                         (border-width . 5)
                                                         ,@(list '(fullscreen . maximized))))))
