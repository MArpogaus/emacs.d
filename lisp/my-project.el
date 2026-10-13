;;; my-project.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-09
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/MArpogaus/auto-tab-groups.git][auto-tab-groups]]
;; Tab group based workflow isolation.

(use-package auto-tab-groups
  :ensure (:host github :repo "MArpogaus/auto-tab-groups")
  :custom
  ;; These commands open their buffer in a tab of the named group.
  (auto-tab-groups-create-commands
   '(((denote--prepare-note denote-open-or-create denote-silo-open-or-create
       denote-silo-dired denote-dired denote-sequence-find denote-sequence-dired
       denote-menu-list-notes consult-denote-find consult-denote-grep)
      . "denote")
     ((custom-buffer-create custom-buffer-create-other-window) . "customize")
     ((my/gtd) . "GTD")
     ((dirvish dirvish-fd) . "dirvish")))
  ;; `:always-close': close whatever the command returns.
  (auto-tab-groups-close-commands
   '((dirvish-quit "dirvish" :always-close t)
     (my/kill-denote-buffers "denote" :always-close t)
     (Custom-buffer-done "customize" :always-close t)))
  :bind
  (:map my/workspace-map
        ("w" . auto-tab-groups-new-group))
  :hook
  ((elpaca-after-init . auto-tab-groups-mode)
   (elpaca-after-init . auto-tab-groups-project-mode)))

;; project :build_in:

(use-package project
  :ensure nil
  :bind
  (:map my/leader-map
        ("SPC" . project-list-buffers))
  :custom
  (project-vc-extra-root-markers '(".project"))
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")))
  :init
  (define-key my/leader-map (kbd "p") (cons "project" project-prefix-map)))

;; [[https://github.com/karthink/project-x.git][project-x]]
;; Enhancements to the built-in project library.

(use-package project-x
  :ensure (:host github :repo "karthink/project-x")
  :after project
  :bind (:map project-prefix-map
              ("S" . project-x-window-state-save)
              ("l" . project-x-window-state-load))
  :commands project-x-try-local project-x--window-state-write
  :init
  (add-to-list 'project-switch-commands
               '(project-x-window-state-load "Restore windows" ?j) t)
  (add-hook 'project-find-functions 'project-x-try-local 90)
  :config
  ;; Not in `:init': there the hook would load the package at quit.
  (add-hook 'kill-emacs-hook 'project-x--window-state-write))

;; speedbar :build_in:
;; A tree of the files and the symbols around the current one, in a window of
;; this frame rather than a frame of its own.

(use-package speedbar
  :ensure nil
  :custom
  ;; Increase the indentation for better usability.
  (speedbar-indentation-width 3)
  ;; Update automatically, and use text instead of images.
  (speedbar-update-flag t)
  (speedbar-use-images nil)
  ;; Every file, not only those it can tag.
  (speedbar-show-unknown-files t)

  :bind
  (:map my/toggle-map
        ("s" . speedbar-window-mode))

  :hook
  ((speedbar-mode . (lambda()
                      ;; No word wrapping in speedbar.
                      (visual-line-mode 0)
                      ;; Smaller text in speedbar.
                      (text-scale-adjust -1)))))

;; ZZ Library Footer

(provide 'my-project)
;;; my-project.el ends here
