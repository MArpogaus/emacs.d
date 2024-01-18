;;; my-vc.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-01-18
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/emacsorphanage/git-gutter.git][git-gutter]]
;; Emacs port of Sublime Text Plugin GitGutter.

(use-package git-gutter
  :custom
  (git-gutter:ask-p . nil)
  (git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  ;;update interval for diff information
  (git-gutter:update-interval 0.5)

  ;; PERF: Only enable the backends that are available, so it doesn't have to
  ;;   check when opening each buffer.
  (git-gutter:handled-backends
   (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                :key #'symbol-name)))
  :config
  ;; (add-hook 'after-focus-change-function #'git-gutter:update-all-windows)
  (use-package git-gutter-fringe
    :demand t
    :config
    (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
  :bind
  (:repeat-map my/git-gutter-repeat-map
               ("n" . git-gutter:next-hunk)
               ("p" . git-gutter:previous-hunk)
               ("s" . git-gutter:stage-hunk)
               ("d" . git-gutter:popup-hunk)
               ("r" . git-gutter:revert-hunk)
               :exit
               ("c" . magit-commit-create))
  :hook
  (after-init . global-git-gutter-mode))

;; [[https://codeberg.org/pidu/git-timemachine.git][git-timemachine]]

(use-package git-timemachine
  :bind
  (:map my/version-control-map
        ("t" . git-timemachine)))

;; [[https://github.com/magit/magit.git][magit]]
;; The magical git client. Let's load magit only when one of the several entry pont
;; functions we invoke regularly outside of magit is called.


(use-package magit
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all)
  :bind
  (:map my/version-control-map
        ("F"  . magit-fetch-all)
        ("P"  . magit-push-current)
        ("b"  . magit-branch)
        ("b"  . magit-branch-or-checkout)
        ("c"  . magit-commit)
        ("d"  . magit-diff-unstaged)
        ("f"  . magit-fetch)
        ("la" . magit-log-all)
        ("lc" . magit-log-current)
        ("lf" . magit-log-buffer-file)
        ("p"  . magit-pull-branch)
        ("v"  . magit-status)
        ("r"  . magit-rebase)))

;; Library Footer

(provide 'my-vc)
;;; my-vc.el ends here
