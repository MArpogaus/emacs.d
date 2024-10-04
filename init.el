;;; init.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-10-04
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; Package Management
;; Lets install and configure =use-package= and use =straight= as the underlying package manager.
;; We also load =bind-key= here which is used by =use-package= for keybindings.


(when (< emacs-major-version 29)
  (straight-use-package 'use-package)
  (use-package bind-key))

;; make use-package more verbose when ´‘--debug-init´ is passed
;; https://www.gnu.org/software/emacs/manual/html_node/use-package/Troubleshooting.html
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        jka-compr-verbose t
        byte-compile-warnings t
        byte-compile-verbose t
        native-comp-warning-on-missing-source t
        debug-on-error t))

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
  (no-littering-theme-backups))

;; Better Defaults

(use-package emacs
  :straight nil
  :custom
  ;; Startup
  ;; Emacs does a lot of things at startup and here, we disable pretty much everything.
  (inhibit-splash-screen t)                            ; disable startup screens and messages
  (inhibit-startup-buffer-menu t)                      ; Disable display of buffer list when more than 2 files are loaded
  (inhibit-startup-echo-area-message t)                ; Disable initial echo message
  (inhibit-startup-message t)                          ; Disable startup message
  (inhibit-startup-screen t)                           ; Disable start-up screen
  (initial-scratch-message "")                         ; Empty the initial *scratch* buffer

  ;; Encoding
  ;; We tell emacs to use UTF-8 encoding as much as possible.
  (set-default-coding-systems 'utf-8)                  ; Default to utf-8 encoding
  (prefer-coding-system       'utf-8)                  ; Add utf-8 at the front for automatic detection.
  (set-terminal-coding-system 'utf-8)                  ; Set coding system of terminal output
  (set-keyboard-coding-system 'utf-8)                  ; Set coding system for keyboard input on TERMINAL
  (set-language-environment "English")                 ; Set up multilingual environment

  ;; Recovery
  ;; If Emacs or the computer crashes, you can recover the files you were editing at the time of the crash from their auto-save files. To do this, start Emacs again and type the command ~M-x recover-session~. Here, we parameterize how files are saved in the background.
  (auto-save-default t)                                ; Auto-save every buffer that visits a file
  (auto-save-timeout 10)                               ; Number of seconds between auto-save
  (auto-save-interval 200)                             ; Number of keystrokes between auto-saves

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

  ;; Scroll
  ;; Smoother scrolling.
  (auto-window-vscroll nil)                            ; Disable automatic adjusting of =window-vscroll=
  (fast-but-imprecise-scrolling t)                     ; More performant rapid scrolling over unfontified region
  (hscroll-margin 1)                                   ; Reduce margin triggering automatic horizontal scrolling
  (hscroll-step 1)                                     ; Slower horizontal scrolling
  (mouse-wheel-scroll-amount '(1 ((shift) . hscroll))) ; Reduce vertical scroll speed
  (mouse-wheel-scroll-amount-horizontal 2)             ; Reduce horizontal scroll speed
  (pixel-scroll-precision-interpolate-mice nil)        ; Disable interpolation (causes wired jumps)
  (pixel-scroll-precision-mode (display-graphic-p))    ; Enable pixel-wise scrolling
  (pixel-scroll-precision-use-momentum t)              ; Enable momentum for scrolling lagre buffers
  (scroll-conservatively 101)                          ; Avoid recentering when scrolling far
  (scroll-preserve-screen-position t)                  ; Don't move point when scrolling

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
  (fast-but-imprecise-scrolling t)                     ; More performant rapid scrolling over unfontified regions
  (frame-inhibit-implied-resize t)                     ; Inhibit frame resizing for performance
  (read-process-output-max (* 1024 1024))              ; Increase how much is read from processes in a single chunk.
  (select-active-regions 'only)                        ; Emacs hangs when large selections contain mixed line endings.

  ;; Miscellaneous
  (native-comp-async-report-warnings-errors 'silent)   ; disable native compiler warnings
  (fringes-outside-margins t)                          ; DOOM: add some space between fringe it and buffer.
  (window-resize-pixelwise t)                          ; Resize windows pixelwise
  (frame-resize-pixelwise t)                           ; Resize frame pixelwise
  (windmove-mode nil)                                  ; Diasble windmove mode
  :preface
  ;; History
  ;; Remove text properties for kill ring entries (see https://emacs.stackexchange.com/questions/4187). This saves a lot of time when loading it.
  (defun unpropertize-kill-ring ()
    (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
  :init
  (modify-all-frames-parameters '((width . 200)
                                  (height . 50)))
  :config
  ;; Load customization File
  (load custom-file 'noerror 'nomessage)
  :hook
  ;; Enable word wrapping
  (((prog-mode conf-mode text-mode) . visual-line-mode)
   (kill-emacs . unpropertize-kill-ring)))

;; Keymaps

;; We define some keymaps here used by other package declarations and fill the leader keymap with the most important bindings for basic commands.
;; Package specific keymap definitions are kept in preface of the respective package declaration.


;; setup keymaps
(use-package emacs
  :straight nil
  :preface
  (defvar my/leader-map (make-sparse-keymap) "key-map for leader key")
  (defvar my/buffer-map (make-sparse-keymap) "key-map for buffer commands")
  (defvar my/window-map (make-sparse-keymap) "key-map for window commands")
  (defvar my/file-map (make-sparse-keymap) "key-map for file commands")
  (defvar my/toggle-map (make-sparse-keymap) "key-map for toggle commands")
  (defvar my/open-map (make-sparse-keymap) "key-map for open commands")
  (defvar my/version-control-map (make-sparse-keymap) "key-map for version control commands")

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

  :config
  ;; leader keymap
  (define-key my/leader-map (kbd "b") (cons "buffer" my/buffer-map))
  (define-key my/leader-map (kbd "f") (cons "file" my/file-map))
  (define-key my/leader-map (kbd "o") (cons "open" my/open-map))
  (define-key my/leader-map (kbd "t") (cons "toggle" my/toggle-map))
  (define-key my/leader-map (kbd "v") (cons "version-control" my/version-control-map))
  (define-key my/leader-map (kbd "w") (cons "window" my/window-map))

  (define-key my/leader-map (kbd "g") (cons "goto" goto-map))
  (define-key my/leader-map (kbd "h") (cons "help" help-map))
  (define-key my/leader-map (kbd "s") (cons "search" search-map))

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

;; Custom Lisp Functions

;; In this section, I define some custom Lisp functions.

(use-package emacs
  :preface
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
  ;; (cl-defun create-org-entry-for-package (recipe)
  ;;   (interactive (list (straight-get-recipe nil nil)))
  ;;   (straight--with-plist recipe
  ;;       (package local-repo type)
  ;;     (message-box type)
  ;;     (if (eq type 'git)
  ;;         (straight-vc-git--destructure recipe
  ;;             (package local-repo branch nonrecursive depth
  ;;                      remote upstream-remote
  ;;                      host upstream-host
  ;;                      protocol upstream-protocol
  ;;                      repo upstream-repo fork-repo)
  ;;           (message upstream-remote)
  ;;           (let ((parent-headline-level (org-outline-level)))
  ;;             (save-excursion
  ;;               (org-insert-heading (1+ parent-headline-level))
  ;;               (insert (format "*** [[%s][%s]]\n" upstream-remote package))
  ;;               ;; (insert (format "%s\n" description))
  ;;               (insert (format "#+begin_src emacs-lisp\n(use-package %s\n  :demand t\n  :after (eglot consult))\n#+end_src\n" package))
  ;;               (org-edit-src-code)))
  ;;           )
  ;;       )))
  )

;; Configure Packages
;; We save the following package declaration into separate files in the =modules= directory.
;; To load the we have to add this directory to the =load-path=.

(add-to-list 'load-path "~/.emacs.d/lisp/")

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

;; Org
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-org.el
;; :END:

(require 'my-org)

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
