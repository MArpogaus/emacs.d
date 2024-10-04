;;; my-vc.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-10-04
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/dgutov/diff-hl.git][diff-hl]]
;; Emacs package for highlighting uncommitted changes.

(use-package diff-hl
  :custom
  ;; DOOM: https://github.com/doomemacs/doomemacs/blob/98d753e1036f76551ccaa61f5c810782cda3b48a/modules/ui/vc-gutter/config.el#L167C1-L173C41
  ;; PERF: A slightly faster algorithm for diffing.
  (vc-git-diff-switches '("--histogram"))
  ;; PERF: Slightly more conservative delay before updating the diff
  (diff-hl-flydiff-delay 0.5)  ; default: 0.3
  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (diff-hl-show-staged-changes nil)
  :preface
  (defun my/diff-hl-inline-popup-show-adv (orig-func &rest args)
    (setcar (nthcdr 2 args) "")
    (apply orig-func args))
  (defun my/diff-hl-fix-face-colors (&rest _)
    "Set foreground to background color for diff-hl faces"
    (seq-do (lambda (face)
              (if-let ((color (face-background face)))
                  (progn (set-face-foreground face color)
                         (set-face-background face nil))))
            '(diff-hl-insert
              diff-hl-delete
              diff-hl-change)))
  :config
  (advice-add #'diff-hl-inline-popup-show :around #'my/diff-hl-inline-popup-show-adv)
  ;; UI: minimal fringe indicators
  ;; https://github.com/dgutov/diff-hl/issues/116#issuecomment-1573253134
  (let* ((width 2)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap 'my/diff-hl-bitmap bitmap 1 width '(top t)))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my/diff-hl-bitmap))
  (my/diff-hl-fix-face-colors)
  (advice-add #'enable-theme :after #'my/diff-hl-fix-face-colors)
  (when (not (display-graphic-p))
    (diff-hl-margin-mode))
  :bind
  (:map my/version-control-map
        ("g" . diff-hl-show-hunk)
        :repeat-map diff-hl-show-hunk-map
        ("n" . diff-hl-show-hunk-next)
        ("p" . diff-hl-show-hunk-previous)
        ("r" . diff-hl-revert-hunk)
        ("S" . diff-hl-stage-current-hunk)
        :exit
        ("C" . magit-commit-create))
  :hook
  ((find-file    . diff-hl-mode)
   (vc-dir-mode  . diff-hl-dir-mode)
   (dired-mode   . diff-hl-dired-mode)
   (diff-hl-mode . diff-hl-flydiff-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

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

;; [[https://github.com/alphapapa/magit-todos.git][magit-todos]]
;; Show source files' TODOs (and FIXMEs, etc) in Magit status buffer.

(use-package magit-todos
  :after magit
  :init (magit-todos-mode))

;; Library Footer

(provide 'my-vc)
;;; my-vc.el ends here
