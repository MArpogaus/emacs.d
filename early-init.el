;;; early-init.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-01-18
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; Configure Byte Compile

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)

;; Disable certain byte compiler warnings to cut down on the noise.
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Optimize Startup Time
;; We're going to increase the gc-cons-threshold to a very high number to decrease the load time and add a hook to measure Emacs startup time.

;; The following optimisatzion have been inspired by:

;; - https://github.com/nilcons/emacs-use-package-fast#a-trick-less-gc-during-startup
;; - https://github.com/mnewt/dotemacs/blob/master/early-init.el
;; - https://github.com/alexluigit/dirvish/blob/main/docs/.emacs.d.example/early-init.el


(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; Let's lower our GC thresholds back down to a sane level.
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold (* 2 1000 1000)
                                   gc-cons-percentage 0.1)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; Default frame settings. This is actually maximized, not full screen.
(push '(fullscreen . maximized) initial-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))

;; further preformance tweaks
(setq
 auto-mode-case-fold nil ; Use case-sensitive `auto-mode-alist' for performance
 fast-but-imprecise-scrolling t ; More performant rapid scrolling over unfontified regions
 ffap-machine-p-known 'reject ; Don't ping things that look like domain names
 idle-update-delay 1.0  ; slow down UI updates down
 inhibit-compacting-font-caches t ; Inhibit frame resizing for performance
 read-process-output-max (* 1024 1024) ; Increase how much is read from processes in a single chunk.
 redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
 command-line-x-option-alist nil ; Remove irreleant command line options for faster startup
 select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings.
 vc-follow-symlinks t) ; Do not ask about symlink following

;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Configure Straight
;; This section provides the bootstrap code for =straight.el=, a package manager for Emacs.
;; The code includes optimization for startup time, disables file modification checking for performance, and loads the =straight.el= bootstrap file, which contains essential functionality.


;; prevent package.el loading packages prior to their init-file loading.
(setq package-quickstart nil
      package-enable-at-startup nil)

;; straight.el bootstrap code
;;disable checking (for speedup).
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Conventional Library Footer

(provide 'early-init)
;;; early-init.el ends here
