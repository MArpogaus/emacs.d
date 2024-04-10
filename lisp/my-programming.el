;;; my-programming.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-04-10
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/emacs-straight/auctex.git][auctex]]
;; Integrated environment for *TeX*

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :custom
  ;; Use PDF Tools for pdf output
  (TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))

  ;; Enable auto-saving of TeX files
  (TeX-auto-save t)
  ;; Enable parsing of the current TeX file
  (TeX-parse-self t)
  ;; Disable query prompts when saving TeX files
  (TeX-save-query nil)
  ;; Ask for master document
  (TeX-master nil)
  ;; Enable PDF mode for TeX files
  (TeX-PDF-mode t)
  :hook
  ;; Set up preview, math mode, source correlate, and reftex in LaTeX mode
  ((LaTeX-mode . LaTeX-preview-setup)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . turn-on-reftex)))

;; [[https://github.com/astoff/code-cells.el.git][code-cells]]
;; Emacs utilities for code split into cells, including Jupyter notebooks.

(use-package code-cells
  :preface
  (defun my/code-cells-eval (start end)
    (interactive (code-cells--bounds (prefix-numeric-value current-prefix-arg)
                                     'use-region
                                     'no-header))
    (code-cells-eval start end)
    (code-cells-forward-cell 1))
  :config
  (add-to-list 'code-cells-eval-region-commands '(python-base-mode . python-shell-send-region))
  :bind
  (:map code-cells-mode-map
        ("C-S-<return>" . my/code-cells-eval))
  :hook
  (python-base-mode . code-cells-mode-maybe))

;; [[https://github.com/mickeynp/combobulate.git][combobulate]]
;; Structured Editing and Navigation in Emacs.

(use-package combobulate
  :after treesit
  :custom
  ;; ;; You can customize Combobulate's key prefix here.
  ;; ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :config
  (define-key my/open-map "c" (cons "combobulate" combobulate-key-map))
  :bind
  (:map combobulate-key-map
        ("S-<down>"  . combobulate-navigate-down-list-maybe)
        ("S-<left>"  . combobulate-navigate-previous)
        ("S-<right>" . combobulate-navigate-next)
        ("M-<left>"  . combobulate-navigate-logical-previous)
        ("M-<right>" . combobulate-navigate-logical-next)
        ("S-<up>"    . combobulate-navigate-up-list-maybe)
        ("M-<down>"  . combobulate-drag-down)
        ("M-<up>"    . combobulate-drag-up))
  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

;; [[https://github.com/necaris/conda.el.git][conda]]
;; Emacs helper library (and minor mode) to work with conda environments.

(use-package conda
  :after tab-bar project python
  :custom
  ;; support for mambaforge envs
  (conda-anaconda-home "~/mambaforge/")
  (conda-env-home-directory "~/mambaforge/")
  :commands conda-env-candidates
  :preface
  (defun my/find-python-interpreter-advice (&rest _)
    "Find the Python interpreter and set `python-shell-interpreter' and `python-shell-interpreter-args' accordingly."
    (cond
     ((executable-find "ipython3")
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "--simple-prompt --classic"))
     ((executable-find "python3")
      (setq python-shell-interpreter "python3"
            python-shell-interpreter-args "-i"))
     (t
      (setq python-shell-interpreter "python"
            python-shell-interpreter-args "-i"))))
  (defun my/conda-env-auto-activate-advice (&rest _)
    "Activate conda environment for project."
    (let* ((project-current (project-current))
           ;; If buffer belongs to a project use its name.
           ;; If not fall back to tab group
           (current-project-name 
            (or (and project-current (project-name project-current))
                (alist-get 'group (tab-bar--current-tab))))
           ;; Skip env activation if project is located at a remote location
           (project-remote-p
            (and project-current (file-remote-p (project-root project-current)))))
      (progn
        (message (format "%s %s" current-project-name project-remote-p))
        (cond
         ((and (bound-and-true-p conda-project-env-path)
               (not project-remote-p))
          (message (format "activating env for project: %s" conda-project-env-path))
          (conda-env-activate-for-buffer))
         ((and current-project-name
               (not project-remote-p)
               (member current-project-name (conda-env-candidates)))
          (message (format "found conda env with project name: %s" current-project-name))
          (conda-env-activate current-project-name))
         (t (conda-env-deactivate))))))
  (define-minor-mode my/global-conda-env-autoactivate-mode
    "Toggle conda-env-autoactivate mode.

This mode automatically tries to activate a conda environment for the current
buffer."
    ;; The initial value.
    nil
    ;; The indicator for the mode line.
    nil
    ;; The minor mode bindings.
    nil
    ;; Kwargs
    :group 'conda
    :global t
    ;; Forms
    (if my/global-conda-env-autoactivate-mode ;; already on, now switching off
        (progn
          (advice-add #'tab-bar-select-tab :after #'my/conda-env-auto-activate-advice)
          (advice-add #'project-switch-project :after #'my/conda-env-auto-activate-advice)
          ;; Try to activat env imediatly after switching to a project
          (my/conda-env-auto-activate-advice))
      (progn
        (advice-remove #'tab-bar-select-tab #'my/conda-env-auto-activate-advice)
        (advice-remove #'project-switch-project #'my/conda-env-auto-activate-advice)
        (conda-env-deactivate))))
  :init
  ;; enable automatic activation on project switch
  (my/global-conda-env-autoactivate-mode)
  :config
  ;; interactive shell support
  (conda-env-initialize-interactive-shells)
  ;; eshell support
  (conda-env-initialize-eshell)
  ;; Add current conda env to mode line
  (add-to-list 'global-mode-string
               '(conda-env-current-name (" 󰌠 conda: " conda-env-current-name " "))
               'append)
  ;; Add custom hook to set correct python interpreter
  (advice-add #'conda-env-activate :after #'my/find-python-interpreter-advice)
  (advice-add #'conda-env-deactivate :after #'my/find-python-interpreter-advice))

;; [[https://github.com/svaante/dape.git][dape]]
;; Debug Adapter Protocol for Emacs.

(use-package dape
  ;; To use window configuration like gud (gdb-mi)
  ;; :init
  ;; (setq dape-buffer-window-arrangement 'gud)
  :bind
  (("<left-fringe> <mouse-1>" . dape-mouse-breakpoint-toggle)
   :repeat-map my/debug-map
   ("d" . dape)
   ("p" . dape-pause)
   ("c" . dape-continue)
   ("n" . dape-next)
   ("s" . dape-step-in)
   ("o" . dape-step-out)
   ("r" . dape-restart)
   ("i" . dape-info)
   ("R" . dape-repl)
   ("m" . dape-read-memory)
   ("l" . dape-breakpoint-log)
   ("e" . dape-breakpoint-expression)
   ("b" . dape-breakpoint-toggle)
   ("B" . dape-breakpoint-remove-all)
   ("t" . dape-select-thread)
   ("S" . dape-select-stack)
   ("x" . dape-evaluate-expression)
   ("w" . dape-watch-dwim)
   ("D" . dape-disconnect-quit)
   :exit
   ("q" . dape-quit))
  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)

  :hook
  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ((dape-on-start-hooks . (lambda () (save-some-buffers t t)))
   (prog-mode . dape-breakpoint-global-mode)))

;; [[https://github.com/spotify/dockerfile-mode.git][docker]]
;; An emacs mode for handling Dockerfiles.

(use-package docker
  :commands docker)
(use-package dockerfile-mode
  :mode "/Dockerfile\\'"
  :mode "/Containerfile\\'"
  :mode "\\.dockerfile\\'"
  :mode "\\.containerfile\\'")
(use-package tramp-container
  :straight nil
  :after docker)

;; [[https://github.com/emacs-straight/eglot.git][eglot]]
;; A client for Language Server Protocol servers.

(use-package eglot
  :custom
  ;; Filter list of all possible completions with Orderless
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (completion-category-defaults nil)
  :preface
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (cons (cape-capf-super
                       #'cape-file
                       #'eglot-completion-at-point
                       #'tempel-complete)
                      completion-at-point-functions)))
  :bind
  (:map my/lsp-map
        ("l" . eglot)
        ("=" . eglot-format-buffer)
        ("R" . eglot-reconnect)
        ("f" . eglot-find-declaration)
        ("i" . eglot-find-implementation)
        ("k" . eglot-shutdown)
        ("o" . eglot-code-action-organize-imports)
        ("q" . eglot-code-action-quickfix)
        ("r". eglot-rename))
  :config
  ;; Continuously update the candidates using cape cache buster
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  ((python-base-mode . eglot-ensure)
   (eglot-managed-mode . my/eglot-capf)))

;; [[https://github.com/jdtsmith/eglot-booster.git][eglot-booster]]
;; Boost eglot using [[https://github.com/blahgeek/emacs-lsp-booster][lsp-booster]].

(use-package eglot-booster
  :after eglot
  :if (executable-find "emacs-lsp-booster")
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :config	(eglot-booster-mode))

;; [[https://github.com/emacs-straight/eldoc.git][eldoc]]
;; Configure emacs documentation support.

(use-package eldoc
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-"))

;; [[https://github.com/emacs-ess/ESS.git][ESS]]
;; Emacs Speaks Statistics: ESS.

(use-package ess
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.[rR]nw\\'" . Rnw-mode)
         ("\\.jl\\'" . julia-mode))
  :config
  (require 'ess-site))

;; [[https://github.com/kaz-yos/eval-in-repl.git][eval-in-repl]]
;; Consistent ESS-like eval interface for various REPLs.

(use-package eval-in-repl
  :custom
  ;; Uncomment if no need to jump after evaluating current line
  ;; (eir-jump-after-eval nil)

  ;; Uncomment if you want to always split the script window into two.
  ;; This will just split the current script window into two without
  ;; disturbing other windows.
  ;; (eir-always-split-script-window t)

  ;; Uncomment if you always prefer the two-window layout.
  ;; (eir-delete-other-windows t)

  ;; Place REPL on the left of the script window when splitting.
  (eir-repl-placement 'left)
  :preface
  (defun my/setup-eir-python nil
    (require 'eval-in-repl-python)
    (local-set-key (kbd "<C-return>") 'eir-eval-in-python))
  (defun my/setup-eir-lisp nil
    (require 'eval-in-repl-ielm)
    ;; Evaluate expression in the current buffer.
    (setq-local eir-ielm-eval-in-current-buffer t)
    (local-set-key (kbd "<C-return>") 'eir-eval-in-ielm))
  :hook
  (((python-mode python-ts-mode) . my/setup-eir-python)
   ((emacs-lisp-mode lisp-interaction-mode Info-mode) . my/setup-eir-lisp)))

;; [[https://github.com/emacs-straight/flymake.git][flymake]]
;; Universal on-the-fly syntax checker for Emacs.

(use-package flymake
  :custom
  ;; Let git gutter have left fringe, flymake can have right fringe
  (flymake-fringe-indicator-position 'right-fringe)
  :hook
  ((prog-mode conf-mode) . flymake-mode))

;; [[https://github.com/lassik/emacs-format-all-the-code.git][format-all]]
;; Auto-format source code in many languages with one command.

(use-package format-all
  ;;:hook (prog-mode . format-all-mode)
  :bind
  (:map my/toggle-map
        ("f" . format-all-buffer)))

;; [[https://github.com/immerrr/lua-mode.git][lua]]
;; Emacs major mode for editing Lua.

(use-package lua-mode
  :mode "\\.lua\\'")

;; [[https://github.com/jrblevin/markdown-mode.git][markdown]]
;; Emacs Markdown Mode.

(use-package markdown-mode
  :mode "\\.md\\'")

;; [[https://github.com/douglasdavis/numpydoc.el.git][numpydoc]]
;; Insert NumPy style docstrings in Python functions.

(use-package numpydoc
  :after python)

;; [[https://github.com/paetzke/py-isort.el.git][py-isort]]
;; Py-isort.el integrates isort into Emacs.

(use-package py-isort
  :after conda)

;; [[https://github.com/Wilfred/pyimport.git][pyimport]]
;; Manage Python imports from Emacs!.

(use-package pyimport
  :after conda)

;; python :build_in:

(use-package python
  :straight nil
  :custom
  ;; Let Emacs guess Python indent silently
  (python-indent-guess-indent-offset t)
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-dedicated 'project))

;; [[https://github.com/eanopolsky/sphinx-doc.el.git][sphinx-doc]]
;; Generate Sphinx friendly docstrings for Python functions in Emacs.

(use-package sphinx-doc
  :straight (:host github :repo "eanopolsky/sphinx-doc.el" :branch "square-brackets-in-return-types")
  :hook
  (python-mode . sphinx-doc-mode))

;; [[https://github.com/liushihao456/symbols-outline.el.git][symbols-outline]]
;; Display symbols (functions, variables, etc) in a side window.

(use-package symbols-outline
  :bind
  (:map my/toggle-map
        ("o" . symbols-outline-show))
  :custom
  (symbols-outline-window-position 'left)
  :config
  ;; By default the ctags backend is selected
  (unless (executable-find "ctags")
    ;; Use lsp-mode or eglot as backend
    (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))
  (symbols-outline-follow-mode))

;; [[https://github.com/renzmann/treesit-auto.git][treesit-auto]]
;; built-in tree-sitter integration for Emacs

(use-package treesit-auto
  :if (>= emacs-major-version 29)
  :custom
  (treesit-auto-install 'prompt)
  :hook
  (after-init . global-treesit-auto-mode))

;; [[https://github.com/garyo/ts-fold.git][ts-fold]]
;; Code-folding using tree-sitter.
;; Using the forked version with treesit support here

(use-package ts-fold
  :straight (:host github :repo "garyo/ts-fold" :branch "andrew-sw/treesit-el-support")
  :preface
  (defun my/ts-fold-mode-hook ()
    (keymap-local-set "<backtab>" 'ts-fold-toggle))
  :hook
  (((yaml-ts-mode python-ts-mode) . ts-fold-mode)
   (ts-fold-mode . my/ts-fold-mode-hook)))

;; [[https://github.com/yoshiki/yaml-mode.git][yaml]]
;; The emacs major mode for editing files in the YAML data serialization format.

(use-package yaml-mode
  :bind
  (:map yaml-mode-map ("\C-m" . newline-and-indent))
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

;; Library Footer

(provide 'my-programming)
;;; my-programming.el ends here
