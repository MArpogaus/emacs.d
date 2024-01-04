;;; my-programming.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; This file has been generated from emacs.org file. DO NOT EDIT.

;; Copyright (C) 2010-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: internal
;; URL: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

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

(use-package code-cells
  :hook
  (python-base-mode . code-cells-mode-maybe))

(use-package combobulate
  :after treesit
  :custom
  ;; ;; You can customize Combobulate's key prefix here.
  ;; ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :config
  (define-key my/open-map "c" (cons "combobulate" combobulate-key-map))

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

(use-package dape
  ;; Currently only on github
  :straight (dape :type git :host github :repo "svaante/dape")
  :config
  ;; Add inline variable hints, this feature is highly experimental
  ;; (setq dape-inline-variables t)

  ;; To remove info buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)

  ;; To remove repl buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)

  ;; Add Debug Adapters
  ;; https://github.com/svaante/dape#supported-debug-adapters
  (add-to-list 'dape-configs
               `(debugpy
                 modes (python-ts-mode python-mode)
                 command "python3"
                 command-args ("-m" "debugpy.adapter")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-find-file-buffer-default))
  )

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

(use-package eglot
  :custom
  ;; Filter list of all possible completions with Orderless
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (completion-category-defaults nil)
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
  (with-eval-after-load 'cape
    ;; Continuously update the candidates using cape cache buster
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
  :hook ((python-mode python-ts-mode) . eglot-ensure))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-"))

(use-package ess
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.[rR]nw\\'" . Rnw-mode)
         ("\\.jl\\'" . julia-mode))
  :config
  (require 'ess-site))

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

(use-package flycheck
  :custom
  ;; Let git gutter have left fringe, flycheck can have right fringe
  (flycheck-indication-mode 'right-fringe)

  ;; Doom: https://github.com/doomemacs/doomemacs/blob/dbb48712eea6dfe16815a3e5e5746b31dab6bb2f/modules/checkers/syntax/config.el#L15
  ;; Don't recheck on idle as often
  (flycheck-idle-change-delay 1.0)
  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (flycheck-display-errors-delay 0.25)
  :preface
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (defun my/flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s: %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err))
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun my/flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'my/flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))

  :config
  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  :hook
  ((after-init . global-flycheck-mode)
   (flycheck-mode . my/flycheck-prefer-eldoc)))

(use-package flymake
  :custom
  ;; Let git gutter have left fringe, flymake can have right fringe
  (flymake-fringe-indicator-position 'right-fringe))

(use-package format-all
  ;;:hook (prog-mode . format-all-mode)
  :bind
  (:map my/toggle-map
        ("f" . format-all-buffer)))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package python
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :custom
  ;; Let Emacs guess Python indent silently
  (python-indent-guess-indent-offset t)
  (python-indent-guess-indent-offset-verbose nil))

(use-package conda
  :after python
  :custom
  ;; support for mambaforge envs
  (conda-anaconda-home "~/mambaforge/")
  (conda-env-home-directory "~/mambaforge/")
  (conda-activate-base-by-default t)
  :preface
  (defun my/find-python-interpreter nil
    (cond
     ((executable-find "ipython3")
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "--simple-prompt --classic"))
     ((executable-find "python3")
      (setq python-shell-interpreter "python3"
            python-shell-interpreter-args "-i"))
     (t (setq python-shell-interpreter "python"
              python-shell-interpreter-args "-i"))))
  :config
  ;; interactive shell support
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; enable auto-activation
  (conda-env-autoactivate-mode t)
  ;; if you want to automatically activate a conda environment on the opening of a file:
  ;; (add-to-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
  ;;                       (conda-env-activate-for-buffer))))
  (advice-add #'conda-env-activate :after #'my/find-python-interpreter))

(use-package numpydoc
  :after python)

(use-package sphinx-doc
  :straight '(:type git :host github :repo "eanopolsky/sphinx-doc.el"
                    :branch "square-brackets-in-return-types")
  :hook
  (python-mode . sphinx-doc-mode))

(use-package pyimport
  :after conda)

(use-package py-isort
  :after conda)

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

(use-package treesit-auto
  :if (>= emacs-major-version 29)
  :custom
  (treesit-auto-install 'prompt)
  :hook
  (after-init . global-treesit-auto-mode))

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "garyo/ts-fold" :branch "andrew-sw/treesit-el-support")
  :bind
  (:map my/toggle-map
        ([tab] . ts-fold-toggle))
  :hook
  ((after-init . global-ts-fold-mode)
   (after-init . global-ts-fold-indicators-mode)))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(provide 'my-programming)
;;; my-programming.el ends here
