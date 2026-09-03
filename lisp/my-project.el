;;; my-project.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-03
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
  :after tab-bar mood-line project nerd-icons
  :custom
  ;; the following functions trigger the creation of a new tab assigned to group with the name of the given string, or returned by a provided function
  (auto-tab-groups-create-commands
   '(((denote--prepare-note denote-menu-list-notes consult-denote-find consult-denote-grep) . "denote")
     ((custom-buffer-create custom-buffer-create-other-window) . "customize")
     ((dirvish dirvish-fd) . "dirvish")))
  (auto-tab-groups-close-commands
   '((dirvish-quit "dirvish" :ignore-result t)
     (my/kill-denote-buffers "denote" :ignore-result t)
     (Custom-buffer-done "customize" :ignore-result t)))
  :bind
  (:map my/workspace-map
        ("w" . auto-tab-groups-new-group))
  :init
  ;; automatically assign projects to groups
  (auto-tab-groups-project-mode)
  ;; Enable automatic tab group management based on the rules defined above
  (auto-tab-groups-mode))

;; project :build_in:

(use-package project
  :ensure nil
  :autoload project-prefix-map
  :bind
  (:map my/leader-map
        ("SPC" . project-list-buffers))
  :custom
  (project-vc-extra-root-markers '(".project"))
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             ;; (project-vc-dir "VC-Dir")
                             ;; (project-eshell "Eshell")
                             ))
  :init
  (define-key my/leader-map (kbd "p") (cons "project" project-prefix-map)))

;; [[https://github.com/karthink/project-x.git][project-x]]
;; Enhancements to Emacs' built in project library.

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
  (add-hook 'kill-emacs-hook 'project-x--window-state-write))

;; speedbar :build_in:
;; A tree of the files and the symbols around the current one, in a window of
;; this frame rather than a frame of its own.

(use-package speedbar
  :ensure nil
  :custom
  (speedbar-frame-parameters
   '((name . "speedbar")
     (title . "speedbar")
     (minibuffer . nil)
     (border-width . 2)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 10)))
  ;; Increase the indentation for better useability.
  (speedbar-indentation-width 3)
  ;; make speedbar update automaticaly, and dont use ugly icons(images)
  (speedbar-update-flag t)
  (speedbar-use-images nil)
  ;; A window of this frame, not a frame of its own.
  (speedbar-prefer-window t)

  :bind
  (:map my/toggle-map
        ("s" . speedbar-window-mode))

  :config
  ;; list of supported file-extensions
  ;; feel free to add to this list
  (speedbar-add-supported-extension
   (list
    ;; lua and fennel(lisp that transpiles to lua)
    ".lua"
    ".fnl"
    ".fennel"
    ;; shellscript
    ".sh"
    ".bash";;is this ever used?
    ;; web languages
    ;; Hyper-Text-markup-language(html) and php
    ".php"
    ".html"
    ".htm"
    ;; ecma(java/type)-script
    ".js"
    ".json"
    ".ts"
    ;; stylasheets
    ".css"
    ".less"
    ".scss"
    ".sass"
    ;; c/c++ and makefiles
    ".c"
    ".cpp"
    ".h"
    "makefile"
    "MAKEFILE"
    "Makefile"
    ;; runs on JVM, java,kotlin etc
    ".java"
    ".kt";;this is for kotlin
    ".mvn"
    ".gradle" ".properties";; this is for gradle-projects
    ".clj";;lisp on the JVM
    ;; lisps
    ".cl"
    ".el"
    ".scm"
    ".lisp"
    ;; configuration
    ".yaml"
    ".toml"
    ;; json is already in this list
    ;; notes,markup and orgmode
    ".md"
    ".markdown"
    ".org"
    ".txt"
    "README"
    ;; Jupyter Notebooks
    ".ipynb"))

  :hook
  ((speedbar-mode . (lambda()
                      ;; Disable word wrapping in speedbar if you always enable it globally.
                      (visual-line-mode 0)
                      ;; Change speedbar's text size.  May need to alter the icon size if you change size.
                      (text-scale-adjust -1)))))

;; ZZ Library Footer

(provide 'my-project)
;;; my-project.el ends here
