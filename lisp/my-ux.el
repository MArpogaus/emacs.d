;;; my-ux.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-04-10
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; autorevert :build_in:
;; Revert buffers when the underlying file has changed

(use-package autorevert
  :straight nil
  :custom
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  :hook
  (after-init . global-auto-revert-mode))

;; bookmark :build_in:

(use-package bookmark
  :straight nil
  :custom
  (bookmark-save-flag 1))

;; [[https://github.com/emacs-straight/comint-mime.git][comint-mime]]
;; Mirror of the comint-mime package from GNU ELPA, current as of 2024-01-18.
;; Provides a mechanism for REPLs (or comint buffers, in Emacs parlance) to display graphics and other types of special content.

(use-package comint-mime
  :hook
  ((shell-mode . comint-mime-setup)
   (inferior-python-mode . comint-mime-setup)))

;; delsel :build_in:
;; Replace selected text when typing

(use-package delsel
  :straight nil
  :hook
  ((prog-mode conf-mode text-mode) . delete-selection-mode))

;; elec-pair :build_in:
;; Automatically add closing parentheses, quotes, etc.

(use-package elec-pair
  :straight nil
  :hook
  ((prog-mode conf-mode) . electric-pair-mode))

;; [[https://github.com/roman/golden-ratio.el.git][golden-ratio]]
;; When working with many windows at the same time, each window has a size that is not convenient for editing.

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
        ("g" . golden-ratio-mode)))

;; [[https://github.com/dengste/minimap.git][minimap]]
;; Sidebar showing a "mini-map" of a buffer.

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

;; paren :build_in:
;; Paren mode for highlighting matcing paranthesis


(use-package paren
  :straight nil
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren nil)
  :hook
  (prog-mode . show-paren-mode))

;; recentf :build_in:

;; 50 Recents files with some exclusion (regex patterns).


(use-package recentf
  :straight nil
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

;; repeat :build_in:
;; Enable repeat maps

(use-package repeat
  :straight nil
  :preface
  ;; https://karthinks.com/software/it-bears-repeating/#adding-repeat-mode-support-to-keymaps
  (defun my/repeatize-keymap (keymap)
    "Add `repeat-mode' support to a KEYMAP."
    (map-keymap
     (lambda (_key cmd)
       (when (symbolp cmd)
         (put cmd 'repeat-map keymap)))
     (symbol-value keymap)))
  :config
  (with-eval-after-load 'smerge
    (my/repeatize-keymap 'smerge-basic-map))
  :hook
  (after-init . repeat-mode))

;; [[https://github.com/daichirata/emacs-rotate.git][rotate]]
;; Rotate the layout of emacs.

(use-package rotate
  :bind
  (:repeat-map my/window-map
               ("R" . rotate-layout)
               ("W" . rotate-window)))

;; savehist :build_in:

(use-package savehist
  :straight nil
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

;; saveplace :build_in:
;; Record cursor position from one session to the other

(use-package saveplace
  :straight nil
  :hook
  (after-init . save-place-mode))

;; time-stamp :build_in:
;; Automatically update file timestamps when file is saved

(use-package time-stamp
  :straight nil
  :custom
  (time-stamp-active t)
  (time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S (%U)")
  :hook
  (before-save . time-stamp))

;; [[https://github.com/joostkremers/writeroom-mode.git][writeroom-mode]]
;; Distraction-free writing for Emacs.

(use-package writeroom-mode
  :bind (:map my/toggle-map ("z" . writeroom-mode)))

;; Library Footer

(provide 'my-ux)
;;; my-ux.el ends here
