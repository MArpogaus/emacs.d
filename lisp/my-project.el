;;; my-project.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-04-04
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; project :build_in:

(use-package project
  :straight nil
  :custom
  (project-vc-extra-root-markers '(".project")))

;; [[https://github.com/karthink/project-x.git][project-x]]
;; Enhancements to Emacs' built in project library.

(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :after project
  :bind (:map project-prefix-map
              ("w" . project-x-window-state-save)
              ("j" . project-x-window-state-load))
  :commands project-x-try-local project-x--window-state-write
  :init
  (add-to-list 'project-switch-commands
               '(?j "Restore windows" project-x-windows) t)
  (add-hook 'project-find-functions 'project-x-try-local 90)
  (add-hook 'kill-emacs-hook 'project-x--window-state-write))

;; [[https://github.com/fritzgrabo/project-tab-groups.git][project-tab-groups]]
;; Support a "one tab group per project" workflow.

(use-package project-tab-groups
  :after tab-bar project
  :config
  (with-eval-after-load 'tab-bar-echo-area
    (push #'project-switch-project tab-bar-echo-area-trigger-display-functions)
    (tab-bar-echo-area-apply-display-tab-names-advice))
  :init
  (project-tab-groups-mode))

;; speedbar :build_in:

(use-package speedbar
  :straight nil
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

;; [[https://github.com/emacsorphanage/sr-speedbar.git][sr-speedbar]]
;; Same frame speedbar.

(use-package sr-speedbar
  :custom
  (sr-speedbar-right-side nil)
  :bind
  (:map my/toggle-map
        ("s" . sr-speedbar-toggle)))

;; Library Footer

(provide 'my-project)
;;; my-project.el ends here
