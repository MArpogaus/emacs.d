;;; my-programming.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-04-18
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/emacs-straight/auctex.git][auctex]]
;; Integrated environment for *TeX*

(use-package auctex
  :preface
  ;; Custom auto-compile minor mode
  (define-minor-mode my/auto-compile-mode
    "Automatically compile LaTeX files after saving."
    :lighter " LaTeX Auto Compile"
    ;; Add/remove after-save hook based on mode state
    (if my/auto-compile-mode
        (add-hook 'after-save-hook #'my/compile-latex-on-save nil t)
      (remove-hook 'after-save-hook #'my/compile-latex-on-save t)))

  ;; Function to compile LaTeX document after saving
  (defun my/compile-latex-on-save ()
    (when (eq major-mode 'LaTeX-mode)
      (TeX-command-run-all nil)))
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
  ;; Enable PDF mode for TeX files
  (TeX-PDF-mode t)
  ;; Don't start server for inverse search (is already running)
  (TeX-source-correlate-start-server nil)
  :hook
  ;; Set up preview, math mode, inverse search, and reftex in LaTeX mode
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
  ;; Setup speed keys
  (let ((map code-cells-mode-map))
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB") (code-cells-speed-key 'outline-cycle)))
  :bind
  (:map code-cells-mode-map
        ("M-S-<down>"   . outline-move-subtree-down)
        ("M-S-<right>"  . outline-demote)
        ("M-S-<left>"   . outline-promote)
        ("M-S-<up>"     . outline-move-subtree-up)
        ("M-<return>"   . outline-insert-heading)
        ("C-S-<tab>"    . outline-cycle-buffer)
        ("C-<backtab>"  . outline-cycle-buffer)
        ("C-S-<return>" . my/code-cells-eval))
  :hook
  (python-base-mode . code-cells-mode-maybe))

;; [[https://github.com/mickeynp/combobulate.git][combobulate]]
;; Structured Editing and Navigation in Emacs.

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate" :nonrecursive t)
  :custom
  ;; Disable combobulate key prefix 
  (combobulate-key-prefix nil)
  :config
  (define-key my/open-map (kbd "c") (cons "combobulate" combobulate-options-key-map))
  :bind
  (:map combobulate-key-map
        ("S-<left>"  . combobulate-navigate-previous)
        ("S-<right>" . combobulate-navigate-next)
        ("S-<down>"  . combobulate-navigate-down)
        ("S-<up>"    . combobulate-navigate-up)
        ("M-<left>"  . combobulate-navigate-logical-previous)
        ("M-<right>" . combobulate-navigate-logical-next)
        ("M-<down>"  . combobulate-drag-down)
        ("M-<up>"    . combobulate-drag-up))
  :hook
  ((prog-mode yaml-ts-mode) . combobulate-mode))

;; [[https://github.com/svaante/dape.git][dape]]
;; Debug Adapter Protocol for Emacs.

(use-package dape
  :preface
  (defvar my/debug-map (make-sparse-keymap) "key-map for debug commands")
  :init
  (define-key my/leader-map (kbd "d") (cons "debug" my/debug-map))
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  :bind
  (("<left-fringe> C-<mouse-1>" . dape-mouse-breakpoint-toggle)
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
  ;; Set breakpints via fringe or margin mouse clicks
  ((prog-mode . dape-breakpoint-global-mode)
   ;; Save buffers on startup, useful for interpreted languages
   (dape-on-start-hooks . (lambda () (save-some-buffers t t)))))

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
  :ensure nil
  :after docker)

;; [[https://github.com/emacs-straight/eglot.git][eglot]] :build_in:
;; A client for Language Server Protocol servers.

(use-package eglot
  :ensure nil
  :after project
  :preface
  (defvar my/lsp-map (make-sparse-keymap) "key-map for lsp commands")
  :init
  (define-key my/leader-map (kbd "l") (cons "lsp" my/lsp-map))
  :custom
  ;; Filter list of all possible completions with Orderless
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (completion-category-defaults nil)
  (eglot-send-changes-idle-time 0.1)
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
        ("r" . eglot-rename))
  :config
  ;; Continuously update the candidates using cape cache buster
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  :hook
  ((python-base-mode . eglot-ensure)
   (eglot-managed-mode . my/eglot-capf)))

;; [[https://github.com/jdtsmith/eglot-booster.git][eglot-booster]]
;; Boost eglot using [[https://github.com/blahgeek/emacs-lsp-booster][lsp-booster]].

(use-package eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :init (eglot-booster-mode))

;; [[https://github.com/emacs-straight/eldoc.git][eldoc]] :build_in:
;; Configure emacs documentation support.

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :bind
  (:map my/lsp-map
        ("d" . eldoc-doc-buffer))
  :config
  (eldoc-add-command-completions "paredit-")
  (with-eval-after-load 'combobulate
    (eldoc-add-command-completions "combobulate-")))

;; [[https://github.com/casouri/eldoc-box.git][eldoc-box]]
;; Childframe doc for eglot and anything that uses eldoc.

(use-package eldoc-box
  :after eglot
  :bind
  (:map my/lsp-map
        ("D" . eldoc-box-hover-at-point-mode))
  :config
  (with-eval-after-load 'pixel-scroll
    (add-to-list 'eldoc-box-self-insert-command-list #'pixel-scroll-precision)
    (add-to-list 'eldoc-box-self-insert-command-list #'pixel-scroll-start-momentum)))

;; [[https://github.com/purcell/envrc.git][envrc]]
;; Emacs support for direnv which operates buffer-locally.

(use-package envrc
  :if (executable-find "direnv")
  :preface
  (defun my/find-python-interpreter nil
    "Find the Python interpreter and set `python-shell-interpreter' and `python-shell-interpreter-args' accordingly."
    (cond
     ((executable-find "ipython3")
      (setq-local python-shell-interpreter "ipython3"
                  python-shell-interpreter-args "--simple-prompt"))
     ((executable-find "python3")
      (setq-local python-shell-interpreter "python3")
      (kill-local-variable 'python-shell-interpreter-args))
     (t (kill-local-variable 'python-shell-interpreter)
        (kill-local-variable 'python-shell-interpreter-args)))
    nil)
  :config
  ;; Fix problem with python promt detection
  ;; https://github.com/purcell/envrc#troubleshooting
  ;; (with-eval-after-load 'python
  ;;   (advice-add 'python-shell-make-comint :around #'envrc-propagate-environment))
  :init
  ;; The global mode should be enabled late in the startup sequence,
  ;; to prevent inference with other other global minor modes.
  ;; We have to use add-hook here manually until [[https://github.com/jwiegley/use-package/issues/965][#965]] is solved.
  (add-hook 'elpaca-after-init-hook #'envrc-global-mode 97)
  :hook
  (envrc-mode . my/find-python-interpreter))

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
    (local-set-key (kbd "C-<return>") 'eir-eval-in-python))
  (defun my/setup-eir-lisp nil
    (require 'eval-in-repl-ielm)
    ;; Evaluate expression in the current buffer.
    (setq-local eir-ielm-eval-in-current-buffer t)
    (local-set-key (kbd "C-<return>") 'eir-eval-in-ielm))
  :hook
  ((python-base-mode . my/setup-eir-python)
   ((emacs-lisp-mode lisp-interaction-mode Info-mode) . my/setup-eir-lisp)))

;; [[https://github.com/emacs-straight/flymake.git][flymake]] :build_in:
;; Universal on-the-fly syntax checker for Emacs.

(use-package flymake
  :ensure nil
  :after project
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

;; [[https://github.com/Wilfred/pyimport.git][pyimport]]
;; Manage Python imports from Emacs!.

(use-package pyimport
  :after conda)

;; [[https://github.com/paetzke/py-isort.el.git][py-isort]]
;; Py-isort.el integrates isort into Emacs.

(use-package py-isort
  :after conda)

;; python :build_in:

(use-package python
  :ensure nil
  :custom
  ;; Let Emacs guess Python indent silently
  (python-indent-guess-indent-offset t)
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-dedicated 'project))

;; [[https://github.com/eanopolsky/sphinx-doc.el.git][sphinx-doc]]
;; Generate Sphinx friendly docstrings for Python functions in Emacs.

(use-package sphinx-doc
  :ensure (:host github :repo "eanopolsky/sphinx-doc.el" :branch "square-brackets-in-return-types")
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
  (elpaca-after-init . global-treesit-auto-mode))

;; [[https://github.com/yoshiki/yaml-mode.git][yaml]]
;; The emacs major mode for editing files in the YAML data serialization format.

(use-package yaml-mode
  :bind
  (:map yaml-mode-map ("\C-m" . newline-and-indent))
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

;; Library Footer

(provide 'my-programming)
;;; my-programming.el ends here
