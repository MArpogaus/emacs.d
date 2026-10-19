;;; early-init.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-09
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file.  DO NOT EDIT.

;;; Code:

;; Configuration Directory
;; =no-littering= points =user-emacs-directory= at =~/.cache/emacs/=, so this keeps the directory the configuration lives in.
;; It must be set here: the =:init= block of =no-littering= runs after =init.el= has loaded, and =init.el= needs this value to find the =lisp/= modules.

(defvar emacs-config-directory user-emacs-directory
  "Directory this configuration lives in, unaffected by `no-littering'.")

;; Configure Byte Compile

;; Load the newer of the .el and .elc files.
(setq load-prefer-newer t)

;; Disable certain byte compiler warnings to cut down on the noise.
(setq byte-compile-warnings '(not unresolved noruntime lexical make-local))

;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)

;; DOOM runtime optimizations
;; These optimizations come from [[https://github.com/doomemacs/doomemacs/blob/da3d0687c5008edbbe5575ac1077798553549a6a/lisp/doom-start.el#L30][here]].

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: no bidirectional text scanning.
(setq-default bidi-paragraph-direction 'left-to-right)

;; PERF: faster redisplay, bidi text in brackets may reorder wrongly.
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting is expensive for icon fonts; this costs memory.
(setq inhibit-compacting-font-caches t)

;; PGTK: a lower timeout makes child frames snappier (lsp-ui#613).
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Skip fontification while input is pending, for faster scrolling.
(setq redisplay-skip-fontification-on-input t)

;; Hide initial frame until all packages are loaded

(add-to-list 'initial-frame-alist (cons 'visibility init-file-debug))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'elpaca-after-init-hook #'make-frame-visible 98)
;; Show the frame after a while, even if init failed.
(add-hook 'after-init-hook
          (lambda ()
            (run-at-time 15 nil (lambda ()
                                  (when (display-graphic-p)
                                    (make-frame-visible))))))

;; Install Elpaca
;; This clones =Elpaca= into the =elpaca= subdirectory of =user-emacs-directory=, then builds and activates it.

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Minimal UI
;; Disable menu, tool and scroll bars.

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Optimize Startup Time

;; The following optimizations have been inspired by:

;; - https://gist.github.com/axyz/76871b404df376271b521212fba8a621
;; - https://github.com/alexluigit/dirvish/blob/main/docs/.emacs.d.example/early-init.el
;; - https://github.com/jamescherti/minimal-emacs.d/blob/main/early-init.el
;; - https://github.com/mnewt/dotemacs/blob/master/early-init.el
;; - https://github.com/nilcons/emacs-use-package-fast#a-trick-less-gc-during-startup


;; No garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore normal GC thresholds after startup.
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)) 97)

;; Profile emacs startup
(defun my/count-modes-and-commands ()
  "Count major modes, minor modes, and commands available in the current Emacs session."
  (let ((major-modes 0)
        (minor-modes 0)
        (commands 0))
    (mapatoms
     (lambda (s)
       (when (commandp s)
         (setq commands (1+ commands))
         (when (string-match-p ".+-mode$" (symbol-name s))
           (if (boundp s)
               (setq minor-modes (1+ minor-modes))
             (setq major-modes (1+ major-modes)))))))
    (message "📊 Emacs loaded with %d major modes, %d minor modes, %d commands and %d features."
             major-modes minor-modes commands (length features))))
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (when init-file-debug
              (my/count-modes-and-commands))
            (message "🚀 Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract elpaca-after-init-time before-init-time))
                     gcs-done)) 99)

;; Halves startup time with fonts larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Remove the "For information about GNU Emacs..." startup message.
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Never show the hello file.
(defalias #'view-hello-file #'ignore)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '((defvaralias)))

;; No file handler is needed during startup.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Keep the handlers registered during startup (tramp).
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist
                                       file-name-handler-alist-old)))))

;; Set default frame size and font
;; :PROPERTIES:
;; :ID:       5eb92f35-9f4c-436f-864f-c270dc2f1f29
;; :END:
;; The font is set on =window-setup-hook=, after the display can tell whether the font exists: a missing family in =default-frame-alist= makes frame creation fail and kills Emacs at startup.
;; A terminal frame has no font to set; =server-after-make-frame-hook= sets the font for the first graphic frame of the daemon.

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 25))

(defun my/set-my-frame-font ()
  "Use FiraCode Nerd Font in this frame and in new frames.
Do nothing in a terminal.  Warn if a graphic display lacks the font."
  (when (and (display-graphic-p)
             ;; Asked once per session.
             (not (assq 'font default-frame-alist)))
    (let ((family "FiraCode Nerd Font"))
      (if (find-font (font-spec :family family))
          (progn
            (add-to-list 'default-frame-alist (cons 'font family))
            (set-frame-font family nil t)
            ;; Material Design icons also in fonts without them.
            (set-fontset-font t '(#xf0001 . #xf1af0) "Symbols Nerd Font" nil 'append))
        (warn "Font `%s' is not installed, frames keep the default font"
              family)))))

(add-hook 'window-setup-hook #'my/set-my-frame-font)
(add-hook 'server-after-make-frame-hook #'my/set-my-frame-font)



;; =my/glyph= picks the first icon a display can draw.
;; A graphic frame is asked per character; a terminal cannot be asked, so =my/terminal-glyphs= tells it.

(defvar my/terminal-glyphs t
  "Whether this terminal draws the glyphs a graphic frame draws.
Emacs cannot ask a terminal font what it holds, and
`char-displayable-p' checks only the coding system.")

(defun my/glyph (&rest candidates)
  "Return the first of CANDIDATES this display can draw.
A graphic frame checks every character of a candidate.  A terminal
checks only if `my/terminal-glyphs' is non-nil; else the last
candidate wins, so make it plain text."
  (or (and (or (display-graphic-p) my/terminal-glyphs)
           (seq-find (lambda (candidate)
                       (seq-every-p (if (display-graphic-p)
                                        (lambda (ch)
                                          (internal-char-font nil ch))
                                      #'char-displayable-p)
                                    candidate))
                     candidates))
      (car (last candidates))))

;; ZZ Conventional Library Footer

(provide 'early-init)
;;; early-init.el ends here
