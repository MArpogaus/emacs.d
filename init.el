;;; init.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-03-03
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; Configure use-package

;; Enable lazy loading per default
(setq use-package-always-defer t
      use-package-always-ensure t)

;; make use-package more verbose when ´‘--debug-init´ is passed
;; https://www.gnu.org/software/emacs/manual/html_node/use-package/Troubleshooting.html
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        jka-compr-verbose t
        warning-minimum-level :warning
        byte-compile-warnings t
        byte-compile-verbose t
        native-comp-warning-on-missing-source t
        debug-on-error t))

;; Configure Elpaca

;; Use elpaca lockfile
;;(setq elpaca-lock-file (expand-file-name "elpaca.lock" user-emacs-directory))

;; Install never version of transient and jsonrpc
(setq elpaca-ignored-dependencies
      (seq-remove (lambda (x) (memq x '(transient jsonrpc)))
                  elpaca-ignored-dependencies))

;; Enable use-package integration
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; no-littering
;; Use no-littering to automatically set common paths to the new user-emacs-directory =~/.cache/emacs=..

(use-package no-littering
  :demand t
  :init
  (setq emacs-config-directory user-emacs-directory
        ;; Since init.el will be generated from this file, we save customization in a dedicated file.
        custom-file (expand-file-name "custom.el" user-emacs-directory)
        ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
        user-emacs-directory (expand-file-name "~/.cache/emacs/"))
  :config
  ;; store backup and auto-save files in =no-littering-var-directory=
  (no-littering-theme-backups)
  ;; Load customization File
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror 'nomessage)) -99))

;; Better Defaults

(use-package emacs
  :ensure nil
  :custom
  ;; Startup
  ;; Emacs does a lot of things at startup and here, we disable pretty much everything.
  (inhibit-splash-screen t)                            ; disable startup screens and messages
  (inhibit-startup-buffer-menu t)                      ; Disable display of buffer list when more than 2 files are loaded
  (inhibit-startup-echo-area-message t)                ; Disable initial echo message
  (inhibit-startup-message t)                          ; Disable startup message
  (inhibit-startup-screen t)                           ; Disable start-up screen
  (initial-scratch-message "")                         ; Empty the initial *scratch* buffer

  ;; Recovery
  ;; If Emacs or the computer crashes, you can recover the files you were editing at the time of the crash from their auto-save files. To do this, start Emacs again and type the command ~M-x recover-session~. Here, we parameterize how files are saved in the background.
  (auto-save-default t)                                ; Auto-save every buffer that visits a file
  (auto-save-timeout 10)                               ; Number of seconds between auto-save
  (auto-save-interval 200)                             ; Number of keystrokes between auto-saves

  ;; Backups
  (backup-by-copying t)                                ; Backs up by moving the actual file

  ;; Dialogs
  ;; use simple text prompts
  (use-dialog-box nil)                                 ; Don't pop up UI dialogs when prompting
  (use-file-dialog nil)                                ; Don't use UI dialogs for file search
  (use-short-answers t)                                ; Replace yes/no prompts with y/n
  (confirm-nonexistent-file-or-buffer nil)             ; Ok to visit non existent files

  ;; Mouse
  ;; Mouse behavior can be finely controlled using mouse-avoidance-mode.
  (context-menu-mode (display-graphic-p))              ; Enable context menu on right click
  (mouse-yank-at-point t)                              ; Yank at point rather than pointer
  (xterm-mouse-mode (not (display-graphic-p)))         ; Mouse active in tty mode.

  ;; Smoother scrolling
  (scroll-margin 0)                                    ; Reduce margin triggering automatic scrolling
  (scroll-conservatively 101)                          ; Avoid recentering when scrolling far
  (scroll-preserve-screen-position t)                  ; Don't move point when scrolling
  (fast-but-imprecise-scrolling t)                     ; More performant rapid scrolling over unfontified regions
  (pixel-scroll-precision-interpolate-mice nil)        ; Disable interpolation (causes wired jumps)
  (pixel-scroll-precision-mode (display-graphic-p))    ; Enable pixel-wise scrolling
  (pixel-scroll-precision-use-momentum t)              ; Enable momentum for scrolling lagre buffers

  ;; Cursor
  ;; We set the appearance of the cursor: horizontal line, 2 pixels thick, no blinking
  (cursor-type '(hbar . 2))                            ; Underline-shaped cursor
  (cursor-intangible-mode t)                           ; Enforce cursor intangibility
  (x-stretch-cursor nil)                               ; Don't stretch cursor to the glyph width
  (blink-cursor-mode nil)                              ; Still cursor

  ;; Typography
  (fill-column 80)                                     ; Default line width
  (sentence-end-double-space nil)                      ; Use a single space after dots
  (truncate-string-ellipsis "…")                       ; Nicer ellipsis

  ;; Default mode
  ;; Default & initial mode is text.
  (initial-major-mode 'fundamental-mode)               ; Initial mode is text
  (default-major-mode 'fundamental-mode)               ; Default mode is text

  ;; Tabulations
  ;; No tabulation, ever.
  (indent-tabs-mode nil)                               ; Stop using tabs to indent

  ;; Performance
  ;; https://github.com/alexluigit/dirvish/blob/main/docs/.emacs.d.example/early-init.el
  (read-process-output-max (* 1024 1024))              ; Increase how much is read from processes in a single chunk.
  (select-active-regions 'only)                        ; Emacs hangs when large selections contain mixed line endings.
  (vc-handled-backends '(Git SVN))                     ; Remove unused VC backend


  ;; Miscellaneous
  (native-comp-async-report-warnings-errors 'silent)   ; disable native compiler warnings
  (fringes-outside-margins t)                          ; DOOM: add some space between fringe it and buffer.
  (windmove-mode nil)                                  ; Diasble windmove mode
  (comment-auto-fill-only-comments t)                  ; Use auto fill mode only in comments
  (custom-buffer-done-kill t)                          ; Kill custom buffer when done

  ;; Enable window dividers
  (window-divider-default-bottom-width 2)
  (window-divider-default-right-width 2)
  (window-divider-default-places t)
  (window-divider-mode t)
  :preface
  ;; History
  ;; Remove text properties for kill ring entries (see https://emacs.stackexchange.com/questions/4187). This saves a lot of time when loading it.
  (defun unpropertize-kill-ring ()
    (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
  :init
  (modify-all-frames-parameters '((width . 200)
                                  (height . 50)))
  :config
  ;; We tell emacs to use UTF-8 encoding as much as possible.
  (set-default-coding-systems 'utf-8)                  ; Default to utf-8 encoding
  (prefer-coding-system       'utf-8)                  ; Add utf-8 at the front for automatic detection.
  (set-terminal-coding-system 'utf-8)                  ; Set coding system of terminal output
  (set-keyboard-coding-system 'utf-8)                  ; Set coding system for keyboard input on TERMINAL
  (set-language-environment "English")                 ; Set up multilingual environment
  :hook
  ;; Enable word wrapping
  (((prog-mode conf-mode text-mode) . visual-line-mode)
   ;; Enable automatic linebreaks before `fill-column' is eceeded
   ((prog-mode conf-mode text-mode) . auto-fill-mode)
   ;; Compress kill ring when exiting emacs
   (kill-emacs . unpropertize-kill-ring)))

;; Custom Lisp Functions
;; In this section, I define some custom Lisp functions.

(use-package emacs
  :ensure nil
  :preface
  (defun my/backward-kill-thing ()
    "Delete sexp, symbol, word or whitespace backward depending on the context at point."
    (interactive)
    (let ((bounds (seq-some #'bounds-of-thing-at-point '(sexp symbol word))))
      (cond
       ;; If there are bounds and point is within them, kill the region
       ((and bounds (< (car bounds) (point)))
        (kill-region (car bounds) (point)))

       ;; If there's whitespace before point, delete it
       ((thing-at-point-looking-at "\\([ \n]+\\)")
        (if (< (match-beginning 1) (point))
            (kill-region (match-beginning 1) (point))
          (kill-backward-chars 1)))

       ;; If none of the above, delete one character backward
       (t
        (kill-backward-chars 1)))))

  (defun simulate-key-press (key)
    "Pretend that KEY was pressed.
KEY must be given in `kbd' notation.
Refference: https://emacs.stackexchange.com/a/13432"
    `(lambda () (interactive)
       (setq prefix-arg current-prefix-arg)
       (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

  (defun my/extract-username-repo ()
    "Extract the username and repository name from a GitHub repository link at point."
    (interactive)
    (save-excursion
      (org-back-to-heading)
      (let* ((element (org-element-at-point))
             (headline (org-element-property :raw-value element))
             (url (save-match-data
                    (string-match org-bracket-link-regexp headline)
                    (match-string 1 headline))))
        (if (and url (string-match "github.com/\\([[:alnum:]\.\-]+\\)/\\([[:alnum:]\.\-]+\\)\\(\.git\\)" url))
            (list (match-string 1 url) (match-string 2 url))
          (error "No GitHub link found at point.")))))

  (defun my/insert-github-repo-description ()
    "Retrieve and insert the short description of a GitHub repository at point."
    (interactive)
    (let* ((repo-info (my/extract-username-repo))
           (username (car repo-info))
           (repo (cadr repo-info)))
      (message (format "Inserting description for GitHub Repository. User: %s, Repo: %s" username repo))
      (let* ((url (format "https://api.github.com/repos/%s/%s" username repo))
             (response (with-current-buffer (url-retrieve-synchronously url)
                         (prog1 (buffer-substring-no-properties (point-min) (point-max))
                           (kill-buffer)))))
        (string-match "\r?\n\r?\n" response)
        (setq response (substring response (match-end 0)))
        (let* ((json (json-read-from-string response))
               (description (cdr (assoc 'description json))))
          (if description
              (progn
                (setq description (string-trim description))
                (setq description (concat (capitalize (substring description 0 1))
                                          (substring description 1)))
                (unless (string-suffix-p "." description)
                  (setq description (concat description ".")))
                (insert description))
            (error "No description, website, or topics provided."))))))

  ;; Thanks doom-modeline: https://github.com/seagle0128/doom-modeline/blob/ec6bc00ac035e75ad10b52e516ea5d95cc9e0bd9/doom-modeline-core.el#L1454C8-L1454C39
  (defun my/get-bar-image (height width color)
    "Get a rectangular bar image with specified height, width and color."
    (if (and (image-type-available-p 'pbm) (display-graphic-p))
        (propertize
         " " 'display
         (create-image
          (concat (format "P1\n%i %i\n" width height) (make-string (* width height) ?1) "\n")
          'pbm t :foreground color :ascent 'center))
      (propertize "|" 'face (list :foreground color
                                  :background color))))
  (define-minor-mode my/minimal-ui-mode
    "Toggle automatic tab group management based on command execution."
    :global t
    :group 'frames
    (if my/minimal-ui-mode
        (progn
          (setq tab-bar-show nil)
          (tab-bar-mode -1)
          (global-tab-line-mode -1)
          (global-hide-mode-line-mode 1)
          (spacious-padding-mode -1))
      (progn
        (setq tab-bar-show t)
        (tab-bar-mode 1)
        (global-tab-line-mode 1)
        (global-hide-mode-line-mode -1)
        (spacious-padding-mode 1)))))

;; Keymaps

;; We define some keymaps here used by other package declarations and fill the leader keymap with the most important bindings for basic commands.
;; Package specific keymap definitions are kept in preface of the respective package declaration.


;; setup keymaps
(use-package emacs
  :ensure nil
  :preface
  (defvar my/leader-map (make-sparse-keymap) "key-map for leader key")
  (defvar my/buffer-map (make-sparse-keymap) "key-map for buffer commands")
  (defvar my/file-map (make-sparse-keymap) "key-map for file commands")
  (defvar my/toggle-map (make-sparse-keymap) "key-map for toggle commands")
  (defvar my/open-map (make-sparse-keymap) "key-map for open commands")
  (defvar my/version-control-map (make-sparse-keymap) "key-map for version control commands")
  :config
  ;; leader keymap
  (define-key my/leader-map (kbd "b") (cons "buffer" my/buffer-map))
  (define-key my/leader-map (kbd "f") (cons "file" my/file-map))
  (define-key my/leader-map (kbd "o") (cons "open" my/open-map))
  (define-key my/leader-map (kbd "t") (cons "toggle" my/toggle-map))
  (define-key my/leader-map (kbd "v") (cons "version-control" my/version-control-map))

  (define-key my/leader-map (kbd "g") (cons "goto" goto-map))
  (define-key my/leader-map (kbd "h") (cons "help" help-map))
  (define-key my/leader-map (kbd "s") (cons "search" search-map))
  (define-key my/leader-map (kbd "c") (cons "C-c" (simulate-key-press "C-c")))
  (define-key my/leader-map (kbd "x") (cons "C-x" (simulate-key-press "C-x")))
  
  ;; Remove binding to view-echo-area-messages when clicking on inactive minibuffer
  (define-key minibuffer-inactive-mode-map (kbd "<mouse-1>") nil)

  ;; remove keybind for suspend-frame
  (global-unset-key (kbd "C-z"))

  ;; Don't kill windows when clicking on the mode line
  (global-unset-key [mode-line mouse-2])
  (global-unset-key [mode-line mouse-3])
  :bind
  ;;ESC Cancels All
  (("<escape>" . keyboard-escape-quit)
   ("C-<backspace>" . my/backward-kill-thing)
   :map my/buffer-map
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
   :map my/toggle-map
   ("M" . my/minimal-ui-mode)))

;; Configure Packages
;; We save the following package declaration into separate files in the =modules= directory.
;; To load the we have to add this directory to the =load-path=.

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Org
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-org.el
;; :END:

(require 'my-org)

;; UI
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-ui.el
;; :END:

(require 'my-ui)

;; UX
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-ux.el
;; :END:

(require 'my-ux)

;; Denote
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-denote.el
;; :END:

(require 'my-denote)

;; Tools
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-tools.el
;; :END:

(require 'my-tools)

;; Completion
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-completion.el
;; :END:

(require 'my-completion)

;; Version Control
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-vc.el
;; :END:

(require 'my-vc)

;; Project Management
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-project.el
;; :END:

(require 'my-project)

;; Programming
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-programming.el
;; :END:

(require 'my-programming)

;; Keybindings
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-keybindings.el
;; :END:

(require 'my-keybindings)

;; Conventional Library Footer for =init.el=

(provide 'init)
;;; init.el ends here
