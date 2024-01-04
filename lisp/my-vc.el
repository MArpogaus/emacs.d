;;; my-vc.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; This file has been generated from emacs.org file. DO NOT EDIT.

;; Copyright (C) 2010-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: internal
;; URL: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

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

(use-package git-timemachine
  :bind
  (:map my/version-control-map
        ("t" . git-timemachine)))

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

(provide 'my-vc)
;;; my-vc.el ends here
