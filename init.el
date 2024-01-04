;;; init.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; This file has been generated from emacs.org file. DO NOT EDIT.

;; Copyright (C) 2010-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: internal
;; URL: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

(when (< emacs-major-version 29)
  (straight-use-package 'use-package)
  (use-package bind-key))

(setq straight-use-package-by-default t
      use-package-always-defer t)
;; make use-package more verbose when ´‘--debug-init´ is passed
;; https://www.gnu.org/software/emacs/manual/html_node/use-package/Troubleshooting.html
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))

(setq user-full-name "Marcel Arpogaus"
      user-mail-address "znepry.necbtnhf@tznvy.pbz")

(use-package no-littering
  :demand t
  :init
  (setq org-directory (expand-file-name "Notes/org/" (getenv "HOME"))
        org-cite-global-bibliography (file-expand-wildcards (expand-file-name "bib/*.bib" org-directory))
        org-brain-path (expand-file-name "brain/" org-directory)
        my/templates-path (expand-file-name "templates.eld" user-emacs-directory)
        ;; Since init.el will be generated from this file, we save customization in a dedicated file.
        custom-file (expand-file-name "custom.el" user-emacs-directory))

  ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
  (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
        url-history-file (expand-file-name "url/history" user-emacs-directory)))

(use-package emacs
  :custom
  ;; Startup
  ;; Emacs does a lot of things at startup and here, we disable pretty much everything.
  (inhibit-startup-screen t)			 ; Disable start-up screen
  (inhibit-startup-message t)			 ; Disable startup message
  (inhibit-startup-echo-area-message t)	 ; Disable initial echo message
  (initial-scratch-message "")		 ; Empty the initial *scratch* buffer

  ;; Encoding
  ;; We tell emacs to use UTF-8 encoding as much as possible.
  (set-default-coding-systems 'utf-8)		 ; Default to utf-8 encoding
  (prefer-coding-system       'utf-8)		 ; Add utf-8 at the front for automatic detection.
  (set-terminal-coding-system 'utf-8)		 ; Set coding system of terminal output
  (set-keyboard-coding-system 'utf-8)		 ; Set coding system for keyboard input on TERMINAL
  (set-language-environment "English")	 ; Set up multilingual environment

  ;; Recovery
  ;; If Emacs or the computer crashes, you can recover the files you were editing at the time of the crash from their auto-save files. To do this, start Emacs again and type the command ~M-x recover-session~. Here, we parameterize how files are saved in the background.
  (auto-save-default t)			 ; Auto-save every buffer that visits a file
  (auto-save-timeout 20)			 ; Number of seconds between auto-save
  (auto-save-interval 200)			 ; Number of keystrokes between auto-saves

  ;; Text
  ;; Pretty self-explanatory
  (use-short-answers t)			 ; Replace yes/no prompts with y/n
  (confirm-nonexistent-file-or-buffer nil)	 ; Ok to visit non existent files

  ;; Mouse
  ;; Mouse behavior can be finely controlled using mouse-avoidance-mode.
  (mouse-yank-at-point t)			 ; Yank at point rather than pointer
  (mouse-avoidance-mode 'exile)		 ; Avoid collision of mouse with point
  ;; Mouse active in tty mode.
  (xterm-mouse-mode (display-graphic-p))

  ;; Scroll
  ;; Smoother scrolling.
  (scroll-conservatively 101)                  ; Avoid recentering when scrolling far
  (scroll-margin 2)                            ; Add a margin when scrolling vertically
  (recenter-positions '(5 bottom))             ; Set re-centering positions

  ;; Cursor
  ;; We set the appearance of the cursor: horizontal line, 2 pixels thick, no blinking
  (cursor-in-non-selected-windows nil)	 ; Hide the cursor in inactive windows
  (cursor-type '(hbar . 2))			 ; Underline-shaped cursor
  (cursor-intangible-mode t)			 ; Enforce cursor intangibility
  (x-stretch-cursor nil)			 ; Don't stretch cursor to the glyph width
  (blink-cursor-mode nil)			 ; Still cursor

  ;; Typography
  (fill-column 80)				 ; Default line width
  (sentence-end-double-space nil)		 ; Use a single space after dots
  (bidi-paragraph-direction 'left-to-right)	 ; Faster
  (truncate-string-ellipsis "…")		 ; Nicer ellipsis

  ;; Default mode
  ;; Default & initial mode is text.
  (initial-major-mode 'text-mode)		 ; Initial mode is text
  (default-major-mode 'text-mode)		 ; Default mode is text

  ;; Tabulations
  ;; No tabulation, ever.
  (indent-tabs-mode nil)			 ; Stop using tabs to indent
  (tab-width 4)				 ; Smaller width for tab characters

  ;; Don't pop up UI dialogs when prompting
  (use-dialog-box nil)
  (use-file-dialog nil)

  ;; disable native compiler warnings
  (native-comp-async-report-warnings-errors nil)

  ;; DOOM: add some space between fringe it and buffer.
  (fringes-outside-margins t)

  ;; Resize frame and windows pixelwise
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  :preface
  ;; History
  ;; Remove text properties for kill ring entries (see https://emacs.stackexchange.com/questions/4187). This saves a lot of time when loading it.
  (defun unpropertize-kill-ring ()
    (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

  :config
  ;; Load customization File
  (load custom-file 'noerror 'nomessage)

  :bind
  ;;ESC Cancels All
  (("<escape>" . keyboard-escape-quit))

  :hook
  ;; Enable word wrapping
  (((prog-mode conf-mode text-mode) . visual-line-mode)
   ;; display column number in modeline
   ((prog-mode conf-mode) . column-number-mode)
   (kill-emacs . unpropertize-kill-ring)))

;; setup keymaps
(use-package emacs
  :preface
  (defvar my/leader-map (make-sparse-keymap) "key-map for leader key")
  (defvar my/version-control-map (make-sparse-keymap) "key-map for version control commands")
  (defvar my/git-gutter-repeat-map (make-sparse-keymap) "key-map for GitGutter commands")
  (defvar my/completion-map (make-sparse-keymap) "key-map for completion commands")
  (defvar my/buffer-map (make-sparse-keymap) "key-map for buffer commands")
  (defvar my/buffer-scale-map (make-sparse-keymap) "key-map for buffer text scale commands")
  (defvar my/window-map (make-sparse-keymap) "key-map for window commands")
  (defvar my/file-map (make-sparse-keymap) "key-map for file commands")
  (defvar my/workspace-map (make-sparse-keymap) "key-map for workspace commands")
  (defvar my/toggle-map (make-sparse-keymap) "key-map for toggle commands")
  (defvar my/open-map (make-sparse-keymap) "key-map for open commands")
  (defvar my/lsp-map (make-sparse-keymap) "key-map for lsp commands")

  :config
  ;; remove keybind for suspend-frame
  (global-unset-key (kbd "C-z"))

  ;; version control and lsp commands
  (define-key my/leader-map "v" (cons "version-control" my/version-control-map))
  (define-key my/version-control-map "g" (cons "gutter" my/git-gutter-repeat-map))
  (define-key my/leader-map "l" (cons "lsp" my/lsp-map))

  ;; completion commands
  (define-key my/leader-map "." (cons "completion" my/completion-map))

  ;; file, buffer, window and workspace commands
  (define-key my/leader-map "b" (cons "buffer" my/buffer-map))
  (define-key my/buffer-map "z" (cons "scale" my/buffer-scale-map))
  (define-key my/leader-map "w" (cons "window" my/window-map))
  (define-key my/leader-map "f" (cons "file" my/file-map))
  (define-key project-prefix-map "w" (cons "workspace" my/workspace-map))

  ;; toggle commands
  (define-key my/leader-map "t" (cons "toggle" my/toggle-map))

  ;; opening recent files ne buffer frame etc
  (define-key my/leader-map "o" (cons "open" my/open-map))

  ;; add predefined maps to leader map
  (define-key my/leader-map "g" (cons "goto" goto-map))
  (define-key my/leader-map "h" (cons "help" help-map))
  (define-key my/leader-map "p" (cons "project" project-prefix-map))
  (define-key my/leader-map "s" (cons "search" search-map))
  ;;    (define-key my/leader-map "x" (cons "C-x" ctl-x-map))

  :bind
  (:map my/buffer-map
        ("e" . eval-buffer)
        ("k" . kill-this-buffer)
        ("K" . kill-buffer)
        ("c" . clone-buffer)
        ("r" . revert-buffer)
        ("e" . eval-buffer)
        ("s" . save-buffer)
        :map my/file-map
        ("f" . find-file)
        ("F" . find-file-other-window)
        ("d" . find-dired)
        ("c" . copy-file)
        ("f" . find-file)
        ("d" . delete-file)
        ("r" . rename-file)
        ("w" . write-file)
        :map my/open-map
        ("F" . make-frame)
        ("i" . ielm)
        ("e" . eshell)
        ("t" . term)
        ("s" . scratch-buffer)
        :repeat-map my/buffer-scale-map
        ("+" . text-scale-increase)
        ("-" . text-scale-decrease)
        ("=" . text-scale-adjust)
        :repeat-map my/window-map
        ("n" . next-window-any-frame)
        ("p" . previous-window-any-frame)
        ("k" . delete-window)
        ("K" . kill-buffer-and-window)
        ("+" . enlarge-window)
        ("-" . shrink-window)
        ("*" . enlarge-window-horizontally)
        ("’" . shrink-window-horizontally)
        ("r" . split-window-right)
        ("b" . split-window-below)
        ("v" . split-window-vertically)
        ("h" . split-window-horizontally)
        ("m" . delete-other-windows)
        ("m" . delete-other-windows)
        ("M" . delete-other-windows-vertically)
        :exit
        ("=" . balance-windows)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'my-ui)

(require 'my-ux)

(require 'my-org)

(require 'my-tools)

(require 'my-completion)

(require 'my-vc)

(require 'my-project)

(require 'my-programming)

(require 'my-keybindings)

(provide 'init)
;;; init.el ends here
