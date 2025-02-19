;;; my-ux.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-02-19
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/abo-abo/ace-window.git][ace-window]]
;; Quickly switch windows in Emacs.

(use-package ace-window
  :preface
  ;; https://karthinks.com/software/emacs-window-management-almanac/#a-window-prefix-command-for-ace-window
  (defun my/ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  :custom
  (aw-dispatch-always t)
  (aw-minibuffer-flag nil)
  ;; Make Emacs ask where to place a new buffer
  (display-buffer-base-action '((display-buffer-reuse-window
                                 display-buffer-in-previous-window
                                 ace-display-buffer)))
  :autoload ace-display-buffer
  :config
  ;; Ignore the inibuffer
  (add-to-list 'aw-ignored-buffers 'minibuffer-mode)
  :bind
  (("M-O" . ace-window)
   ("M-o" . my/ace-window-prefix)))

;; [[https://github.com/emacscollective/auto-compile.git][auto-compile]]
;; Automatically compile outdated Emacs Lisp libraries.

(use-package auto-compile
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  :init
  (auto-compile-on-load-mode)
  :hook
  (emacs-lisp-mode . auto-compile-on-save-mode))

;; autorevert :build_in:
;; Revert buffers when the underlying file has changed

(use-package autorevert
  :ensure nil
  :custom
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  ;; Avoid polling for changes and rathe get notified by the system
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  :hook
  (elpaca-after-init . global-auto-revert-mode))

;; bookmark :build_in:

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1))

;; [[https://github.com/emacs-straight/comint-mime.git][comint-mime]]
;; Mirror of the comint-mime package from GNU ELPA, current as of 2024-01-18.
;; Provides a mechanism for REPLs (or comint buffers, in Emacs parlance) to display graphics and other types of special content.

(use-package comint-mime
  :hook
  (inferior-python-mode . comint-mime-setup))

;; delsel :build_in:
;; Replace selected text when typing

(use-package delsel
  :ensure nil
  :hook
  ((prog-mode conf-mode text-mode) . delete-selection-mode))

;; elec-pair :build_in:
;; Automatically add closing parentheses, quotes, etc.

(use-package elec-pair
  :ensure nil
  :hook
  ((prog-mode conf-mode) . electric-pair-mode))

;; face-remap :build_in:
;; Keybindings and optimizations for text-scale-mode.
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4278C1-L4303C64
;; https://karthinks.com/software/scaling-latex-previews-in-emacs/

(use-package face-remap
  :ensure nil
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

;; [[https://github.com/hkjels/mini-ontop.el.git][mini-ontop]]
;; Prevent windows from jumping on minibuffer activation.

(use-package mini-ontop
  :ensure (:host github :repo "hkjels/mini-ontop.el")
  :custom
  (mini-ontop-lines 22)
  :config
  (with-eval-after-load 'embark
    (add-to-list 'mini-ontop-ignore-predicates (lambda nil (eq this-command #'embark-act))))
  :hook
  (elpaca-after-init . mini-ontop-mode))

;; [[https://github.com/magnars/multiple-cursors.el.git][multiple-cursors]]

(use-package multiple-cursors
  :preface
  (defvar my/mc-map (make-sparse-keymap) "key-map for multiple cursor commands")
  :init
  (define-key my/leader-map (kbd "m") (cons "mc" my/mc-map))
  :bind
  (("C-S-<mouse-1>" . mc/add-cursor-on-click)
   :map mc/keymap
   ("<escape>" . mc/keyboard-quit)
   :repeat-map my/mc-map
   ("n" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-like-this)
   :exit
   ("a" . mc/mark-all-like-this)
   ("m" . mc/edit-lines)))

;; outline :build_in:
;; Outline-mode helps to fold and transform headers. Org-mode itself uses outline-mode for its headlines.

(use-package outline
  :ensure nil
  :preface
  (defvar my/outline-repeat-map (make-sparse-keymap) "key-map for outline-mode commands")
  (defun my/outline-mode-hook nil
    (when (not (eq major-mode 'org-mode))
      (reveal-mode 1)))
  :init
  (define-key my/leader-map (kbd "TAB") (cons "outline" my/outline-repeat-map))
  :config
  (define-key my/outline-repeat-map (kbd "e") (cons "edit" outline-editing-repeat-map))
  (define-key my/outline-repeat-map (kbd "n") (cons "navigate" outline-navigation-repeat-map))
  :bind
  (:repeat-map my/outline-repeat-map
               ("SPC"         . outline-mark-subtree)
               ("TAB"         . outline-cycle)
               ("S-<tab>"     . outline-cycle-buffer)
               ("<backtab>"   . outline-cycle-buffer)
               ("a"           . outline-show-all))
  :hook
  (((text-mode prog-mode conf-mode) . outline-minor-mode)
   (outline-minor-mode . my/outline-mode-hook)))

;; paren :build_in:
;; Paren mode for highlighting matcing paranthesis


(use-package paren
  :ensure nil
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren nil)
  :hook
  (prog-mode . show-paren-mode))

;; [[https://github.com/karthink/popper.git][popper]]

(use-package popper
  :preface
  (defvar my/popper-top-modes-list
    '(flymake-diagnostics-buffer-mode
      locate-mode
      occur-mode
      xref--xref-buffer-mode)
    "List of major-modes displayed above.")

  (defvar my/popper-top-names-list
    '("^\\*TeX Help\\*$"
      "^\\*TeX errors\\*$"
      "^COMMIT_EDITMSG$")
    "List of buffer names displayed above.")

  (defvar my/popper-bottom-modes-list
    '(eshell-mode
      shell-mode
      term-mode
      vterm-mode
      comint-mode)
    "List of major-modes displayed below.")

  (defvar my/popper-bottom-names-list
    '("^\\*.*eshell.*\\*$"
      "^\\*.*shell.*\\*$"
      "^\\*.*term.*\\*$"
      "^\\*.*vterm.*\\*$"
      "^\\*diff-hl\\*$"
      "^\\*Process List\\*$")
    "List of buffer names displayed below.")

  (defvar my/popper-right-modes-list
    '(Info-mode
      TeX-output-mode
      pdf-view-mode
      eldoc-mode
      grep-mode
      help-mode
      helpful-mode
      magit-status-mode)
    "List of major-modes displayed right.")

  (defvar my/popper-right-names-list
    '("^\\*eldoc.*\\*$"
      "^\\*info\\*$"
      "^magit-diff:.*$")
    "List of buffer names displayed right.")

  (defun my/get-buffer-match-condtion (modes-list &optional names-list)
    (let ((modes-cond `(or ,@(mapcar (lambda (mode) `(derived-mode . ,mode)) modes-list))))
      (if names-list `(or ,modes-cond (or ,@names-list))
        modes-cond)))
  (setq magit-display-buffer-function #'display-buffer
        magit-commit-diff-inhibit-same-window t)
  (setq display-buffer-alist
        `(
          ;; Windows on top
          (,(my/get-buffer-match-condtion my/popper-top-modes-list my/popper-top-names-list)
           (display-buffer-in-side-window)
           (direction . above)
           (side . top)
           (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
           (dedicated . t)
           (window-parameters . ((tab-line-format . none)
                                 (mode-line-format . none))))
          ;; Windows on the right side
          (,(my/get-buffer-match-condtion my/popper-right-modes-list my/popper-right-names-list)
           (display-buffer-in-side-window)
           (side . right)
           (window-width . 80)
           (dedicated . t)
           (inhibit-same-window . t)
           (window-parameters . ((tab-line-format . none)
                                 (mode-line-format . none))))
          ;; Windows at the bottom
          (,(my/get-buffer-match-condtion my/popper-bottom-modes-list my/popper-bottom-names-list)
           (display-buffer-in-side-window)
           (side . bottom)
           (preserve-size . (nil . t))
           (window-height . 20)
           (dedicated . t)
           (window-parameters . ((tab-line-format . none)
                                 (mode-line-format . none))))))

  ;; (setq window-combination-resize t
  ;;       even-window-sizes 'height-only
  ;;       window-sides-vertical t
  ;;       fit-window-to-buffer-horizontally t)

  :custom
  ;; Define popup buffers
  (popper-reference-buffers
   (append my/popper-top-modes-list
           my/popper-top-names-list
           my/popper-right-modes-list
           my/popper-right-names-list
           my/popper-bottom-modes-list
           my/popper-bottom-names-list
           '(("^\\*Warnings\\*$" . hide)
             ("^\\*Compile-Log\\*$" . hide)
             ("^\\*[Oo]utput\\*" . hide)
             ("^\\*Async Shell Command\\*$" . hide)
             ("^\\*Detached Shell Command\\*$" . hide))))
  ;; Respect the rules in display-buffer-alist
  (popper-display-control 'user)
  :config
  ;; grouping popups by project
  (with-eval-after-load 'project
    (setq popper-group-function #'popper-group-by-project))
  :bind
  (:map my/toggle-map
        ("p" . popper-toggle)
        ("P" . popper-toggle-type))
  :hook
  ((elpaca-after-init . popper-mode)
   (elpaca-after-init . popper-echo-mode)))

;; recentf :build_in:

;; 50 Recents files with some exclusion (regex patterns).


(use-package recentf
  :ensure nil
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
  (elpaca-after-init . recentf-mode))

;; repeat :build_in:
;; Enable repeat maps

(use-package repeat
  :ensure nil
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
  (elpaca-after-init . repeat-mode))

;; [[https://github.com/daichirata/emacs-rotate.git][rotate]]
;; Rotate the layout of emacs.

(use-package rotate
  :bind
  (:repeat-map my/window-map
               ("R" . rotate-layout)
               ("W" . rotate-window)))

;; savehist :build_in:

(use-package savehist
  :ensure nil
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
  (elpaca-after-init . savehist-mode))

;; saveplace :build_in:
;; Record cursor position from one session to the other

(use-package saveplace
  :ensure nil
  :hook
  (elpaca-after-init . save-place-mode))

;; time-stamp :build_in:
;; Automatically update file timestamps when file is saved

(use-package time-stamp
  :ensure nil
  :custom
  (time-stamp-active t)
  (time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S (%U)")
  :hook
  (before-save . time-stamp))

;; [[https://github.com/mhayashi1120/Emacs-wgrep.git][wgrep]]
;; Writable grep buffer and apply the changes to files.

(use-package wgrep
  :after grep
  :demand t
  :custom
  (wgrep-auto-save-buffer t))

;; [[https://github.com/joostkremers/writeroom-mode.git][writeroom-mode]]
;; Distraction-free writing for Emacs.

(use-package writeroom-mode
  :bind (:map my/toggle-map ("z" . writeroom-mode)))

;; Library Footer

(provide 'my-ux)
;;; my-ux.el ends here
