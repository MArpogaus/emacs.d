;;; Package --- Summary

;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration file
;; or tangling and loading a literate org configuration file.

;;; Code:

;; Don't attempt to find/apply special file handlers to files loaded during startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "emacs.elc" user-emacs-directory))
      (load-file (expand-file-name "emacs.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e2337309361eef29e91656c5e511a6cb8c54ce26231e11424a8873ea88d9093e" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
