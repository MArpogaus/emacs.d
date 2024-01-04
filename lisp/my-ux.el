;;; my-ux.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; This file has been generated from emacs.org file. DO NOT EDIT.

;; Copyright (C) 2010-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: internal
;; URL: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

(use-package autorevert
  :custom
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  :hook
  (after-init . global-auto-revert-mode))

(use-package bookmark
  :straight nil
  :custom
  (bookmark-save-flag 1))

(use-package comint-mime
  :hook
  ((shell-mode . comint-mime-setup)
   (inferior-python-mode . comint-mime-setup)))

(use-package delsel
  :hook
  ((prog-mode conf-mode text-mode) . delete-selection-mode))

(use-package dirvish
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-use-header-line nil)
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (with-eval-after-load 'doom-modeline
    (setq dirvish-mode-line-height doom-modeline-height)
    (setq dirvish-header-line-height
          doom-modeline-height))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map my/open-map
   ("D" . dirvish)
   :map my/toggle-map
   ("d" . dirvish-side)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ([mouse-1] . dirvish-subtree-toggle-or-open)
   ("F" . dirvish-toggle-fullscreen)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)
   ("M-f" . dirvish-history-go-forward)
   ("M-j" . dirvish-fd-jump)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-s" . dirvish-setup-menu)
   ("M-t" . dirvish-layout-toggle)
   ("N"   . dirvish-narrow)
   ("TAB" . dirvish-subtree-toggle)
   ("^"   . dirvish-history-last)
   ("a"   . dirvish-quick-access)
   ("b"   . dirvish-goto-bookmark)
   ("f"   . dirvish-file-info-menu)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("y"   . dirvish-yank-menu)
   ("z" . dirvish-show-history))
  :hook
  ((after-init . dirvish-override-dired-mode)
   (dired-mode . (lambda nil (setq-local mouse-1-click-follows-link nil)))))

(use-package elec-pair
  :hook
  ((prog-mode conf-mode) . electric-pair-mode))

(use-package golden-ratio
  :custom
  (golden-ratio-exclude-modes '(speedbar-mode vundo-mode dired-mode symbols-outline-mode))
  (golden-ratio-exclude-buffer-regexp '(" ?\\*MINIMAP\\*" " ?\\*Outline\\*"))
  ;; (golden-ratio-auto-scale t)
  :config
  (add-to-list 'golden-ratio-inhibit-functions
               (lambda ()
                 (and which-key--buffer
                      (window-live-p (get-buffer-window which-key--buffer)))))
  :bind
  (:map my/toggle-map
        ("g" . golden-ratio-mode))
  :hook
  (after-init . golden-ratio-mode))

(use-package minimap
  :custom
  (minimap-window-location 'right)
  (minimap-hide-fringes t)
  (minimap-minimum-width 25)
  (minimap-major-modes '(prog-mode conf-mode))
  :config
  (with-eval-after-load 'golden-ratio
    (add-to-list 'golden-ratio-inhibit-functions
                 (lambda ()
                   (and minimap-buffer-name
                        (window-live-p (get-buffer-window minimap-buffer-name)))))
    (add-to-list 'golden-ratio-exclude-buffer-names `(,minimap-buffer-name)))
  :bind
  (:map my/toggle-map
        ("m" . minimap-mode)))

(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren nil)
  :hook
  (prog-mode . show-paren-mode))

(use-package recentf
  :custom
  (recentf-keep '(file-remote-p file-readable-p))
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  :bind
  (:map my/open-map
        ("r" . recentf-open))
  :hook
  (after-init . recentf-mode))

(use-package repeat
  :hook
  (after-init . repeat-mode))

(use-package rotate
  :bind
  (:repeat-map my/window-map
               ("R" . rotate-layout)
               ("W" . rotate-window)))

(use-package savehist
  :custom
  (kill-ring-max 500)
  (history-length 500)
  (savehist-additional-variables
   '(kill-ring
     command-history
     set-variable-value-history
     custom-variable-history
     query-replace-history
     read-expression-history
     minibuffer-history
     read-char-history
     face-name-history
     bookmark-history
     file-name-history))
  ;; No duplicates in history
  (history-delete-duplicates t)
  :config
  (put 'minibuffer-history         'history-length 500)
  (put 'file-name-history          'history-length 500)
  (put 'set-variable-value-history 'history-length 250)
  (put 'custom-variable-history    'history-length 250)
  (put 'query-replace-history      'history-length 250)
  (put 'read-expression-history    'history-length 250)
  (put 'read-char-history          'history-length 250)
  (put 'face-name-history          'history-length 250)
  (put 'bookmark-history           'history-length 250)
  :hook
  ;;Start history mode.
  (after-init . savehist-mode))

(use-package saveplace
  :hook
  (after-init . save-place-mode))

(use-package time-stamp
  :custom
  (time-stamp-active t)
  (time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S (%U)")
  :hook
  (before-save . time-stamp))

(use-package winner
  :hook
  (after-init . winner-mode))

(use-package writeroom-mode
  :bind (:map my/toggle-map ("z" . writeroom-mode)))

(provide 'my-ux)
;;; my-ux.el ends here
