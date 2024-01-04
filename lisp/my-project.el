;;; my-project.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; This file has been generated from emacs.org file. DO NOT EDIT.

;; Copyright (C) 2010-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: internal
;; URL: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

(use-package project
  :custom
  (project-vc-extra-root-markers '(".project")))

(use-package project-tab-groups
  :config
  (with-eval-after-load 'tab-bar-echo-area
    (push #'project-switch-project tab-bar-echo-area-trigger-display-functions)
    (tab-bar-echo-area-apply-display-tab-names-advice))
  :hook (after-init . project-tab-groups-mode))

(use-package speedbar
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

(use-package sr-speedbar
  :custom
  (sr-speedbar-right-side nil)
  :bind
  (:map my/toggle-map
        ("s" . sr-speedbar-toggle)))

(provide 'my-project)
;;; my-project.el ends here
