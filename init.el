;;; init.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file.  DO NOT EDIT.

;;; Code:

;; Better Defaults

(use-package emacs
  :ensure nil
  :custom
  ;; Startup
  ;; Emacs does a lot of things at startup and here, we disable pretty much everything.
  (inhibit-startup-buffer-menu t)                    ; Disable display of buffer list when more than 2 files are loaded
  (inhibit-startup-screen t)                         ; Disable start-up screen
  (initial-scratch-message "")                       ; Empty the initial *scratch* buffer

  ;; Recover files when emacs crashes
  (auto-save-timeout 10)                             ; Number of seconds between auto-save
  (auto-save-interval 200)                           ; Number of keystrokes between auto-saves
  (kill-buffer-delete-auto-save-files t)             ; Drop the auto-save file when its buffer is killed

  ;; Auto save file buffers
  (auto-save-visited-mode t)                         ; enable auto save for all file buffers
  (auto-save-visited-interval 1)                     ; save every second

  ;; Backups
  (backup-by-copying t)                              ; Back up by copying, not renaming

  ;; Dialogs
  (use-dialog-box nil)                               ; Don't pop up UI dialogs when prompting
  (use-file-dialog nil)                              ; Don't use UI dialogs for file search
  (use-short-answers t)                              ; Replace yes/no prompts with y/n
  (confirm-nonexistent-file-or-buffer nil)           ; Ok to visit non existent files

  ;; Regions
  (delete-active-region 'kill)                       ; Make single-char deletion commands kill an active region
  (use-empty-active-region t)                        ; Region-aware commands act on empty regions

  ;; Mouse
  (context-menu-mode t)                              ; Enable context menu on right click
  (mouse-yank-at-point t)                            ; Yank at point rather than pointer
  (save-interprogram-paste-before-kill t)            ; save existing clipboard text into kill ring before replacing it
  (kill-do-not-save-duplicates t)                    ; No duplicates in the kill ring
  (browse-url-browser-function #'browse-url-xdg-open) ; pgtk's GTK launcher opens nothing in a distrobox
  (xterm-mouse-mode (not (display-graphic-p)))       ; Mouse active in tty mode

  ;; Smoother scrolling
  (scroll-conservatively 101)                        ; Avoid recentering when scrolling far
  (scroll-preserve-screen-position t)                ; Don't move point when scrolling
  (fast-but-imprecise-scrolling t)                   ; More performant rapid scrolling over unfontified regions
  (mouse-wheel-tilt-scroll t)                        ; Enable horizontal scrolling with touchpad
  (scroll-error-top-bottom t)                        ; Page to the buffer edge instead of signalling an error

  ;; Cursor
  (cursor-type '(hbar . 2))                          ; Underline-shaped cursor
  (blink-cursor-mode nil)                            ; Still cursor

  ;; Typography
  (fill-column 80)                                   ; Default line width
  (truncate-lines t)                                 ; Give each line of text just one screen line
  (sentence-end-double-space nil)                    ; Use a single space after dots
  (truncate-string-ellipsis "…")                     ; Nicer ellipsis

  ;; Default mode
  (initial-major-mode 'fundamental-mode)             ; Initial mode is fundamental

  ;; Tabulations
  (indent-tabs-mode nil)                             ; Stop using tabs to indent

  ;; Search
  (isearch-repeat-on-direction-change t)             ; Better isearch movement
  (isearch-lazy-count t)                             ; Enable search match counting
  (lazy-count-prefix-format nil)                     ; Display Count at the end
  (lazy-count-suffix-format " [%s/%s]")              ; of the search prompt

  ;; Performance
  ;; https://github.com/alexluigit/dirvish/blob/main/docs/.emacs.d.example/early-init                                 . el
  (read-process-output-max (* 4 1024 1024))          ; Read more process output per chunk
  (select-active-regions 'only)                      ; Emacs hangs when large selections contain mixed line endings
  (vc-handled-backends '(Git SVN))                   ; Remove unused VC backend
  (vc-git-diff-switches '("--histogram"))            ; DOOM: a slightly faster diff algorithm

  ;; Minibuffer and history
  (enable-recursive-minibuffers t)                   ; Minibuffer commands inside the minibuffer
  (history-delete-duplicates t)                      ; No duplicates in history
  (history-length 500)
  (kill-ring-max 500)
  (minibuffer-prompt-properties                      ; No cursor in the prompt
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (read-extended-command-predicate                   ; M-x hides commands of other modes
   #'command-completion-default-include-p)

  ;; Miscellaneous
  (comment-auto-fill-only-comments t)                ; Use auto fill mode only in comments
  (compilation-scroll-output 'first-error)           ; Stop scrolling at the first error
  (custom-buffer-done-kill t)                        ; Kill custom buffer when done
  (delete-by-moving-to-trash t)                      ; Deleted files go to the trash
  (revert-without-query (list "."))                  ; Revert without asking
  (tab-always-indent 'complete)                      ; TAB indents, then completes
  (backward-delete-char-untabify-method 'all)        ; Hungry deletion
  (fringes-outside-margins t)                        ; DOOM: add some space between fringe and buffer
  (help-window-select t)                             ; Select help windows when they open
  (native-comp-async-report-warnings-errors 'silent) ; disable native compiler warnings
  (shell-command-prompt-show-cwd t)                  ; Show current path when executing shell commands
  (set-mark-command-repeat-pop t)                    ; Enable repeat for mark commands

  ;; Enable window dividers
  (window-divider-default-bottom-width 2)
  (window-divider-default-places t)
  (window-divider-default-right-width 2)
  (window-divider-mode t)
  :config
  ;; We tell emacs to use UTF-8 encoding as much as possible.
  ;; UTF-8 everywhere; a later "English" would reset the file defaults.
  (set-language-environment "UTF-8")
  :preface
  (defun my/whitespace-cleanup-unless-idle ()
    "Run `whitespace-cleanup' unless Emacs is idle.
An idle save comes from `auto-save-visited-mode', not from a command."
    (unless (current-idle-time)
      (whitespace-cleanup)))
  :hook
  ;; Enable word wrapping in prose modes
  (((org-mode markdown-mode text-mode message-mode) . visual-line-mode)
   ;; Enable automatic line breaks before `fill-column' is exceeded, in
   ;; comments only: prose above is soft-wrapped instead
   ((prog-mode conf-mode) . auto-fill-mode)
   (before-save . my/whitespace-cleanup-unless-idle)))

;; Configure Elpaca

;; Use elpaca lockfile
(setq elpaca-lock-file (expand-file-name "elpaca.lock" user-emacs-directory))

;; Allow automatic upgrade of built-in transient
;; https://github.com/progfolio/elpaca/issues/272#issuecomment-2298727726
(setq elpaca-ignored-dependencies
      (delq 'transient elpaca-ignored-dependencies))

;; Enable use-package integration
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Configure use-package
;; Every package loads lazily. A =use-package= block follows these rules:

;; 1. A global mode starts with =:hook elpaca-after-init=, or =(elpaca-after-init . MODE)= when MODE is not named after the package. A hook with a depth uses =add-hook= in =:init=. The built-in global modes of Better Defaults are turned on in its =:custom=.
;; 2. A mode for some buffers starts from the hook of their mode.
;; 3. An add-on that must be active with package X: =:after X :demand t=, its mode in =:config=.
;; 4. =:after= only with =:demand t=: alone, it also holds back the keys and hooks of the block.
;; 5. A package with only commands needs no =:after=: its autoloads load it.
;; 6. A block with =:config= needs a trigger: =:hook=, =:bind=, =:mode=, =:commands=, =:demand=, or a load by Emacs or another package.
;; 7. A setting and a key go into the block of the package that owns the variable or the keymap. Window rules all stay in the auto-side-windows block.
;; 8. Advice goes into =:config=, or into =:init= when it must work before the package loads.
;; 9. A block without keywords only installs the package.
;; 10. No =require= and no =with-eval-after-load= in a block.


;; Enable lazy loading per default
(setq use-package-always-defer t
      use-package-always-ensure t)

;; Make use-package more verbose when `--debug-init' is passed
;; https://www.gnu.org/software/emacs/manual/html_node/use-package/Troubleshooting.html
(when init-file-debug
  (setq use-package-verbose t
        use-package-compute-statistics t
        byte-compile-warnings t
        byte-compile-verbose t
        debug-on-error t))

;; Keymaps

;; We define some keymaps here used by other package declarations and fill the leader keymap with the most important bindings for basic commands.
;; Package specific keymap definitions are kept in preface of the respective package declaration.


;; setup keymaps
(use-package emacs
  :ensure nil
  :preface
  (defvar my/leader-map (make-sparse-keymap) "key-map for leader key")
  (defvar my/ai-map (make-sparse-keymap) "key-map for ai key")
  (defvar my/buffer-map (make-sparse-keymap) "key-map for buffer commands")
  (defvar my/denote-map (make-sparse-keymap) "key-map for denote commands")
  (defvar my/file-map (make-sparse-keymap) "key-map for file commands")
  (defvar my/open-map (make-sparse-keymap) "key-map for open commands")
  (defvar my/toggle-map (make-sparse-keymap) "key-map for toggle commands")
  (defvar my/version-control-map (make-sparse-keymap) "key-map for version control commands")
  ;; Bound from other modules, so they exist before any module loads.
  (defvar my/window-map (make-sparse-keymap) "key-map for window commands")
  (defvar my/navigation-map (make-sparse-keymap) "key-map for structural navigation")
  (defvar my/workspace-map (make-sparse-keymap) "key-map for workspace commands")
  ;;ref: https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))
  :config
  ;; leader keymap
  (define-key my/leader-map (kbd "a") (cons "ai" my/ai-map))
  (define-key my/leader-map (kbd "b") (cons "buffer" my/buffer-map))
  (define-key my/leader-map (kbd "e") (cons "expression" my/navigation-map))
  (define-key my/leader-map (kbd "f") (cons "file" my/file-map))
  (define-key my/leader-map (kbd "g") (cons "goto" goto-map))
  (define-key my/leader-map (kbd "h") (cons "help" help-map))
  (define-key my/leader-map (kbd "n") (cons "denote" my/denote-map))
  (define-key my/leader-map (kbd "o") (cons "open" my/open-map))
  (define-key my/leader-map (kbd "s") (cons "search" search-map))
  (define-key my/leader-map (kbd "t") (cons "toggle" my/toggle-map))
  (define-key my/leader-map (kbd "v") (cons "version-control" my/version-control-map))
  (define-key my/leader-map (kbd "w") (cons "window" my/window-map))
  (define-key my/leader-map (kbd "W") (cons "workspace" my/workspace-map))

  ;; Remove binding to view-echo-area-messages when clicking on inactive minibuffer
  (define-key minibuffer-inactive-mode-map (kbd "<mouse-1>") nil)

  ;; remove keybind for suspend-frame
  (global-unset-key (kbd "C-z"))

  ;; Don't kill windows when clicking on the mode line
  (global-unset-key [mode-line mouse-2])
  (global-unset-key [mode-line mouse-3])
  :bind
  ;;ESC Cancels All
  (("<escape>"            . keyboard-escape-quit)
   ([remap keyboard-quit] . prot/keyboard-quit-dwim)
   ("S-<down-mouse-1>"    . mouse-save-then-kill)
   :map context-menu-mode-map
   ("<apps>"              . context-menu-open)
   :map isearch-mode-map
   ("<up>"                . isearch-repeat-backward)
   ("<down>"              . isearch-repeat-forward)
   ([remap yank]          . isearch-yank-kill)
   ;; RET answers yes in y-or-n prompts
   :map y-or-n-p-map
   ("<return>"            . y-or-n-p-insert-y)
   :map my/leader-map
   ("q"                   . save-buffers-kill-terminal)
   :map my/buffer-map
   ("K"                   . kill-buffer)
   ("b"                   . switch-to-buffer)
   ("c"                   . clone-buffer)
   ("e"                   . eval-buffer)
   ("f"                   . switch-to-buffer-other-frame)
   ("k"                   . kill-current-buffer)
   ("r"                   . revert-buffer)
   ("s"                   . save-buffer)
   ("t"                   . switch-to-buffer-other-tab)
   ("w"                   . switch-to-buffer-other-window)
   :map my/file-map
   ("D"                   . delete-file)
   ("F"                   . find-file-other-window)
   ("c"                   . copy-file)
   ("d"                   . find-dired)
   ("f"                   . find-file)
   ("r"                   . rename-file)
   ("w"                   . write-file)
   :map goto-map
   ("g"                   . goto-line)
   :map my/open-map
   ("F"                   . make-frame)
   ("e"                   . eshell)
   ("i"                   . ielm)
   ("s"                   . scratch-buffer)
   ("t"                   . term)
   :map my/toggle-map
   ("M"                   . my/minimal-ui-mode)
   ("v"                   . visual-line-mode)))

;; no-littering
;; Use no-littering to automatically set common paths to the new user-emacs-directory =~/.cache/emacs=.

(use-package no-littering
  :demand t
  :init
  ;; `emacs-config-directory' is captured in early-init.el.
  (setq
   ;; Since init.el will be generated from this file, we save customization in a dedicated file.
   custom-file (expand-file-name "custom.el" user-emacs-directory)
   ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
   user-emacs-directory (expand-file-name "~/.cache/emacs/"))
  :config
  ;; store backup and auto-save files in =no-littering-var-directory=
  (no-littering-theme-backups)
  ;; Load customization File
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror 'nomessage)) -99))

;; Shared Lisp Functions and Variables
;; In this section, I define some custom Lisp functions.

(use-package emacs
  :ensure nil
  :preface
  (defvar my/modeline-height 30 "Height of the mode line in pixels.")
  (defvar my/tabline-height 25 "Height of the tab line in pixels.")
  (defvar citar-bibliography)
  (defun my/bib-note-template ()
    "Return the body of a bibliography note, for citar and citar-denote."
    (concat "#+cite_export: biblatex ieee\n"
            "#+bibliography: " (car citar-bibliography) "\n\n"
            "* Notes\n"
            ":PROPERTIES:\n"
            ":NOTER_DOCUMENT: ${file} \n"
            ":END:\n\n"
            "* Summary :childless:showchildren:export:\n"
            "This is a summary of [cite/t:@${=key=}].\n"
            "** Bibliography\n"))
  (defun my/extract-username-repo ()
    "Extract the username and repository name from a GitHub repository link at point."
    (interactive)
    (save-excursion
      (org-back-to-heading)
      (let* ((element (org-element-at-point))
             (headline (org-element-property :raw-value element))
             (url (save-match-data
                    (and (string-match org-link-bracket-re headline)
                         (match-string 1 headline)))))
        ;; With or without the `.git': both spellings are in this file.
        (if (and url (string-match "github\\.com/\\([[:alnum:]._-]+\\)/\\([[:alnum:]._-]+?\\)\\(?:\\.git\\)?/?\\'" url))
            (list (match-string 1 url) (match-string 2 url))
          (error "No GitHub link found at point")))))
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
            (error "No description, website, or topics provided"))))))

  ;; Thanks doom-modeline: https://github.com/seagle0128/doom-modeline/blob/ec6bc00ac035e75ad10b52e516ea5d95cc9e0bd9/doom-modeline-core.el#L1454C8-L1454C39
  (defun my/get-bar-image (height width color)
    "Get a rectangular bar image with specified height, width and color."
    (if (and (image-type-available-p 'pbm) (display-graphic-p))
        (propertize
         " " 'display
         (create-image
          (concat (format "P1\n%i %i\n" width height) (make-string (* width height) ?1) "\n")
          'pbm t :foreground color :ascent 'center :scale 1))
      ;; Without a color, a terminal draws the bar in the default face.
      (propertize "|" 'face (and color (list :foreground color :background color)))))
  (define-minor-mode my/minimal-ui-mode
    "Hide the tab bar, the tab line, the mode line, the padding and the fringes."
    :global t
    :group 'frames
    (if my/minimal-ui-mode
        (progn
          (setq tab-bar-show nil)
          (tab-bar-mode -1)
          (global-tab-line-mode -1)
          (my/global-mode-line-invisible-mode 1)
          ;; Order matters: disabling spacious-padding resets the
          ;; fringes it widened, so zero them afterwards.
          (spacious-padding-mode -1)
          (set-fringe-style 0))
      (progn
        (setq tab-bar-show t)
        (tab-bar-mode 1)
        (global-tab-line-mode 1)
        (my/global-mode-line-invisible-mode -1)
        (spacious-padding-mode 1)
        (set-fringe-style nil)))))

;; Configure Packages
;; The package declarations below are tangled into separate files in the =lisp/= directory.
;; This directory is added to the =load-path=.

(add-to-list 'load-path (expand-file-name "lisp/" emacs-config-directory))

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

;; AI
;; :PROPERTIES:
;; :header-args+: :tangle lisp/my-ai.el
;; :END:

(require 'my-ai)

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
