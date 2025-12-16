;; -*- lexical-binding: t -*-
;;; Code:
;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
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
;;; init.el ends here
