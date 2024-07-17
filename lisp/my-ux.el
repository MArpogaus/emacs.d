;;; my-ux.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-07-17
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

;; face-remap :build_in:
;; Keybindings and optimizations for text-scale-mode.
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4278C1-L4303C64
;; https://karthinks.com/software/scaling-latex-previews-in-emacs/

(use-package face-remap
  :straight nil
  :preface
  (defvar my/buffer-scale-map (make-sparse-keymap) "key-map for buffer text scale commands")

  (defun my/text-scale-adjust-latex-previews ()
    "Adjust the size of latex preview fragments when changing the
buffer's text scale."
    (pcase major-mode
      ((or 'latex-mode (guard 'org-auctex-mode))
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'category)
                 'preview-overlay)
             (my/zoom-latex-preview ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'org-overlay-type)
                 'org-latex-overlay)
             (my/zoom-latex-preview ov))))))

  (defun my/zoom-latex-preview (ov)
    (overlay-put
     ov 'display
     (cons 'image 
           (plist-put
            (cdr (overlay-get ov 'display))
            :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))
  :init
  (define-key my/buffer-map (kbd "z") (cons "scale" my/buffer-scale-map))
  :bind
  (:repeat-map my/buffer-scale-map
               ("+" . text-scale-increase)
               ("-" . text-scale-decrease)
               ("=" . text-scale-adjust))
  :hook
  (text-scale-mode . my/text-scale-adjust-latex-previews))

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
  (minimap-width-fraction 0)
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

;; outline :build_in:
;; Outline-mode helps to fold and transform headers. Org-mode itself uses outline-mode for its headlines.

(use-package outline
  :straight nil
  :preface
  (defvar my/outline-repeat-map (make-sparse-keymap) "key-map for outline-mode commands")
  :init
  (define-key my/leader-map (kbd "TAB") (cons "outline" my/outline-repeat-map))
  :config
  (define-key my/outline-repeat-map (kbd "e") (cons "edit" outline-editing-repeat-map))
  (define-key my/outline-repeat-map (kbd "n") (cons "navigate" outline-navigation-repeat-map))
  :bind
  (:map outline-minor-mode-map
        ("M-S-<down>"  . outline-move-subtree-down)
        ("M-S-<left>"  . outline-demote)
        ("M-S-<right>" . outline-promote)
        ("M-S-<up>"    . outline-move-subtree-up)
        ("M-<return>"  . outline-insert-heading)
        ("C-S-<tab>"   . outline-cycle-buffer)
        ("C-<backtab>" . outline-cycle-buffer)
        :repeat-map my/outline-repeat-map
        ("SPC"         . outline-mark-subtree)
        ("TAB"         . outline-cycle)
        ("S-<tab>"     . outline-cycle-buffer)
        ("<backtab>"   . outline-cycle-buffer)
        ("a"           . outline-show-all))
  :hook
  (((text-mode prog-mode conf-mode) . outline-minor-mode)
   (outline-mode . reveal-mode)))

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

;; [[https://github.com/karthink/popper.git][popper]]

(use-package popper
  :bind
  (:map my/toggle-map
        ("p"   . popper-toggle)
        ("P" . popper-toggle-type))
  :custom
  ;; Define popup buffers
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*Process List\\*"
     help-mode
     helpful-mode
     compilation-mode
     "^\\*.*eshell.*\\*$" eshell-mode ;eshell as a popup
     "^\\*.*shell.*\\*$"  shell-mode  ;shell as a popup
     "^\\*.*term.*\\*$"   term-mode   ;term as a popup
     "^\\*.*vterm.*\\*$"  vterm-mode  ;vterm as a popup
     "^\\*Flymake diagnostics for .*\\*" flymake-diagnostics-buffer-mode
     ))
  ;; grouping popups by project
  (popper-mode-line nil)
  :config
  (with-eval-after-load 'project
    (setq popper-group-function #'popper-group-by-project))
  :hook
  ((after-init . popper-mode)
   (after-init . popper-echo-mode)))

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
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
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
  (with-eval-after-load 'smerge-mode
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
