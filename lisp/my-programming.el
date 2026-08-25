;;; my-programming.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-08-25
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
  (TeX-bar-LaTeX-buttons
   '(open-file kill-buffer save-buffer separator undo
               separator cut copy paste separator search-forward
               separator latex next-error view bibtex spell))
  :mode
  ;; Ensure auctex is preferred over the build in `latex-mode`
  ("\\.tex\\'" . LaTeX-mode)
  :hook
  ;; Set up preview, math mode, inverse search, and reftex in LaTeX mode
  ((LaTeX-mode . LaTeX-preview-setup)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . LaTeX-install-toolbar)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . turn-on-reftex)))

;; [[https://github.com/astoff/code-cells.el.git][code-cells]]
;; Emacs utilities for code split into cells, including Jupyter notebooks.
;; Two binding layers, mimicking JupyterLab: modified keys for /edit mode/
;; (the =<return>= family, =C-<up>/<down>= navigation, =M-S-<up>/<down>= to
;; move cells), and =my/code-cells-repeat-map= as /command mode/ — entered
;; through any of its commands or via =SPC j=, single keys (=n p j k e r a b
;; c i o O 0 R=) then chain until a foreign key exits.  =C-c C-c= interrupts
;; the running cell, as in the REPL (with =standard-keys-mode= that is
;; =C-p C-c=; note it shadows =python-shell-send-buffer= in cell buffers).
;; No other prefix bindings, and no plain speed keys, which meow's normal
;; state would shadow.  Rarely used commands (split, merge, markdown cells)
;; are =M-x= only.  Cell results are rendered inline by the =pycell= package below.

(use-package code-cells
  :preface
  (defvar my/code-cells-repeat-map (make-sparse-keymap)
    "key-map for code cell commands")
  (defun my/code-cells-insert-below ()
    "Insert a new cell below the current one (JupyterLab: b)."
    (interactive)
    (pcase-let ((`(,_ ,end) (code-cells--bounds)))
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert "# %%\n\n")
      (forward-line -1)))
  (defun my/code-cells-insert-above ()
    "Insert a new cell above the current one (JupyterLab: a)."
    (interactive)
    (pcase-let ((`(,start ,_) (code-cells--bounds)))
      (goto-char start)
      (insert "# %%\n\n")
      (forward-line -1)))
  (defun my/code-cells-split ()
    "Split the current cell at point (JupyterLab: C-S--)."
    (interactive)
    (unless (bolp) (insert "\n"))
    (insert "# %%\n"))
  (defun my/code-cells-merge-above ()
    "Merge the current cell with the one above (JupyterLab: S-m)."
    (interactive)
    (pcase-let ((`(,start ,_) (code-cells--bounds)))
      (goto-char start)
      (when (and (looking-at-p code-cells-boundary-regexp)
                 (/= start (point-min)))
        (delete-region start (progn (forward-line) (point))))))
  (defun my/code-cells-merge-below ()
    "Merge the current cell with the one below."
    (interactive)
    (pcase-let ((`(,_ ,end) (code-cells--bounds)))
      (save-excursion
        (goto-char end)
        (when (looking-at-p code-cells-boundary-regexp)
          (delete-region end (progn (forward-line) (point)))))))
  (defun my/code-cells-eval-and-insert-below (arg)
    "Evaluate the current cell and insert a new one below (JupyterLab: M-RET)."
    (interactive "p")
    (pcase-let ((`(,start ,end) (code-cells--bounds arg nil t)))
      (code-cells-eval start end))
    (my/code-cells-insert-below))
  (defun my/code-cells-insert-markdown-below ()
    "Insert a jupytext markdown cell below the current one (JupyterLab: m)."
    (interactive)
    (my/code-cells-insert-below)
    (save-excursion
      (forward-line -1)
      (end-of-line)
      (insert " [markdown]"))
    (insert "# "))
  :init
  (define-key my/leader-map (kbd "j") (cons "cells" my/code-cells-repeat-map))
  :config
  ;; code-cells' own C-c % prefix map declares `:repeat t', claiming the
  ;; repeat-map property of the very commands bound in our repeat map when
  ;; the package loads.  Drop that prefix and re-assert our map, so the
  ;; repeat chain and `SPC j' stay one and the same.
  (keymap-unset code-cells-mode-map "C-c %" t)
  (my/repeatize-keymap 'code-cells--prefix-map t)
  (my/repeatize-keymap 'my/code-cells-repeat-map)
  :bind
  (:map code-cells-mode-map
        ;; evaluation
        ("C-<return>"   . code-cells-eval)
        ("S-<return>"   . code-cells-eval-and-step)
        ("C-S-<return>" . code-cells-eval-and-step)
        ("M-<return>"   . my/code-cells-eval-and-insert-below)
        ("C-M-<return>" . pycell-restart-and-run-all)
        ;; navigation
        ("C-<up>"       . code-cells-backward-cell)
        ("C-<down>"     . code-cells-forward-cell)
        ;; cell structure
        ("M-S-<up>"     . code-cells-move-cell-up)
        ("M-S-<down>"   . code-cells-move-cell-down)
        ;; outline
        ("M-S-<right>"  . outline-demote)
        ("M-S-<left>"   . outline-promote)
        ("C-S-<tab>"    . outline-cycle-buffer)
        ("C-<backtab>"  . outline-cycle-buffer)
        ;; interrupt, under the C-c prefix like in the REPL; reachable as
        ;; C-p C-c through standard-keys' dynamic prefix, and as plain
        ;; C-c C-c without it.  Shadows python-shell-send-buffer here.
        ("C-c C-c"      . pycell-interrupt)
        ;; "command mode" à la JupyterLab: any entry key activates the
        ;; repeat map, single keys then chain until a foreign key exits.
        ;; The same map hangs off the leader as `SPC j'.
        :map my/code-cells-repeat-map
        ("n" . code-cells-forward-cell)
        ("p" . code-cells-backward-cell)
        ("j" . code-cells-forward-cell)
        ("k" . code-cells-backward-cell)
        ("e" . code-cells-eval)
        ("r" . code-cells-eval-and-step)
        ("a" . my/code-cells-insert-above)
        ("b" . my/code-cells-insert-below)
        ("c" . pycell-copy-output)
        ("i" . pycell-interrupt)
        ("o" . pycell-toggle-output)
        ("O" . pycell-remove-overlays)
        ("0" . pycell-restart)
        ("R" . pycell-restart-and-run-all))
  :hook
  (python-base-mode . code-cells-mode-maybe))

;; [[https://github.com/MArpogaus/pycell][pycell]] :own:
;; Notebook style results for Python code cells: the output of an evaluated
;; cell shows up below it, and markdown cells render in place.  This used to
;; be a long block right here; it is a package now, so only the parts that
;; are specific to this configuration remain.

(use-package pycell
  :ensure (:host github :repo "MArpogaus/pycell")
  ;; The package installs no hook itself; this is where it turns on.
  :hook (code-cells-mode . pycell-mode-maybe)
  :config
  (with-eval-after-load 'auto-side-windows
    (dolist (name '("^\\*pycell: .*\\*$" "^\\*pycell md: .*\\*$"))
      (add-to-list 'auto-side-windows-right-buffer-names name))))

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

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))
  :hook
  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  ;; Set breakpints via fringe or margin mouse clicks
  (prog-mode . dape-breakpoint-global-mode))

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
        ("a" . eglot-code-actions)
        ("r" . eglot-rename))
  :config
  ;; Filter list of all possible completions with Orderless
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (add-to-list 'completion-category-overrides '(eglot (styles orderless)))
  (add-to-list 'completion-category-overrides '(eglot-capf (styles orderless)))
  ;; https://github.com/doomemacs/doomemacs/blob/a90c06dc6b104afbe0c93f0107df5c42b8137b5e/modules/tools/lsp/%2Beglot.el#L36
  (plist-put eglot-events-buffer-config :size 0)

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
  (eldoc-echo-area-use-multiline-p nil)
  :bind
  (:map my/lsp-map
        ("d" . eldoc-doc-buffer))
  :config
  (with-eval-after-load 'combobulate
    (eldoc-add-command-completions "combobulate-")))

;; [[https://github.com/casouri/eldoc-box.git][eldoc-box]]
;; Childframe doc for eglot and anything that uses eldoc.

(use-package eldoc-box
  :after eglot
  :bind
  (:map my/lsp-map
        ("M" . eldoc-box-mouse-mode)
        ("D" . eldoc-box-hover-at-point-mode))
  :config
  (with-eval-after-load 'pixel-scroll
    (add-to-list 'eldoc-box-self-insert-command-list #'pixel-scroll-precision)
    (add-to-list 'eldoc-box-self-insert-command-list #'pixel-scroll-start-momentum)))

;; elisp-mode :build_in:
;; Font lock for Emacs Lisp from a code analysis rather than from the symbol
;; alone: a let-bound variable no longer reads as a function call.

(use-package elisp-mode
  :ensure nil
  :custom
  (elisp-fontify-semantically t))

;; [[https://codeberg.org/pastor/ben.el][ben]]
;; Asynchronous fork of envrc: https://github.com/purcell/envrc

(use-package ben
  :ensure (:host codeberg :repo "pastor/ben.el")
  :bind
  (:map my/leader-map
        ("E" . ben-command-map))
  :custom
  (ben-indicator '((:eval (ben--status))))
  (ben-on-indicator '(:propertize "󰯸" face ben-mode-line-on-face))
  (ben-denied-indicator '(:propertize "󰯹" face ben-mode-line-denied-face))
  (ben-error-indicator '(:propertize "󰯹" face ben-mode-line-error-face))
  (ben-none-indicator nil); '(:propertize "󰯹" face mode-line))
  (ben-status-frames '("" "" "" "" "" ""))
  :init
  (add-hook 'elpaca-after-init-hook #'ben-global-mode 97))

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
  ;; Lay the message out below its line, pointing at the spot it is about.
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  :hook
  ((prog-mode conf-mode) . flymake-mode))

;; [[https://github.com/lassik/emacs-format-all-the-code.git][format-all]]
;; Auto-format source code in many languages with one command.

(use-package format-all
  ;;:hook (prog-mode . format-all-mode)
  :bind
  (:map my/toggle-map
        ("f" . format-all-buffer)))

;; hideshow :build_in:
;; Code folding.  Hideshow reads tree-sitter's =list= thing where the mode
;; defines one, so a parsed buffer folds the same way as an unparsed one and
;; folding needs no package of its own.

(use-package hideshow
  :ensure nil
  :custom
  ;; Say how much a fold swallowed.  The indicators stay off: they draw
  ;; in the fringe, where diff-hl already draws.
  (hs-display-lines-hidden t)
  :bind
  (:map hs-minor-mode-map
        ("<backtab>" . hs-cycle))
  ;; YAML folds by indentation with `outline-indent', which is the one
  ;; folding UI that buffer needs.
  :hook
  ((prog-mode conf-mode) . hs-minor-mode))

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

;; python :build_in:
;; Prefer IPython as the REPL whenever one is on the =PATH= of the buffer's
;; environment.  =executable-find= is evaluated per buffer, so it picks up a
;; project-local IPython installed by =uv= or exported by =ben=, and the result is
;; stored as an absolute path so the REPL keeps using that interpreter.

(use-package python
  :ensure nil
  :custom
  ;; Let Emacs guess Python indent silently
  (python-indent-guess-indent-offset t)
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-dedicated 'project)
  :preface
  (defun my/python-set-interpreter ()
    "Use IPython as `python-shell-interpreter' when one is on `exec-path'."
    (when (derived-mode-p 'python-base-mode)
      (if-let* ((ipython (or (executable-find "ipython")
                             (executable-find "ipython3"))))
          (setq-local python-shell-interpreter ipython
                      python-shell-interpreter-args "-i --simple-prompt --no-color-info")
        (kill-local-variable 'python-shell-interpreter)
        (kill-local-variable 'python-shell-interpreter-args))))
  :config
  ;; Run on mode start *and* after `ben' applies a direnv, because `exec-path'
  ;; only becomes project-local at the latter point.  `ben' skips its hook in
  ;; buffers without an `.envrc', hence both.
  (add-hook 'python-base-mode-hook #'my/python-set-interpreter)
  (add-hook 'ben-after-apply-hook #'my/python-set-interpreter))

;; [[https://github.com/z80dev/uv-mode.git][uv-mode]]
;; Emacs integration for uv virtual environments.

(use-package uv-mode)

;; [[https://github.com/eanopolsky/sphinx-doc.el.git][sphinx-doc]]
;; Generate Sphinx friendly docstrings for Python functions in Emacs.

(use-package sphinx-doc
  :ensure (:host github :repo "eanopolsky/sphinx-doc.el" :branch "square-brackets-in-return-types")
  :hook
  (python-base-mode . sphinx-doc-mode))

;; [[https://github.com/liushihao456/symbols-outline.el.git][symbols-outline]]
;; Display symbols (functions, variables, etc) in a side window.

(use-package symbols-outline
  :preface
  (defun my/symbols-outline-toggle ()
    "Show the symbols outline, or close its window when it is up.
`symbols-outline-show' only ever shows: called again it selects the
window it already made, so a key bound to it cannot put the panel away."
    (interactive)
    (if-let* ((window (and
                       (bound-and-true-p symbols-outline-buffer-name)
                       (get-buffer-window symbols-outline-buffer-name))))
        (delete-window window)
      (symbols-outline-show)))
  :bind
  (:map my/toggle-map
        ("o" . my/symbols-outline-toggle))
  ;; `symbols-outline-show' calls `display-buffer-in-side-window' itself
  ;; rather than `display-buffer', so `display-buffer-alist' never sees
  ;; it: auto-side-windows does not dress this panel and
  ;; `auto-side-windows-after-display-hook' does not run for it.  The
  ;; mode is the buffer's, so the buffer asks for it here, and
  ;; `window-box-window-predicate' still decides the place.
  :hook (symbols-outline-mode . window-box-mode)
  :custom
  (symbols-outline-window-position 'left)
  :config
  ;; By default the ctags backend is selected
  (unless (executable-find "ctags")
    ;; Use lsp-mode or eglot as backend
    (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))
  (symbols-outline-follow-mode))

;; treesit :build_in:
;; The tree-sitter modes in place of their classic counterparts, with the
;; grammars fetched as they are wanted: =treesit-enabled-modes= does the
;; remapping and =treesit-auto-install-grammar= the fetching.

(use-package treesit
  :ensure nil
  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'ask))

;; yaml-ts-mode :build_in:
;; The emacs major mode for editing files in the YAML data serialization format.

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")

;; ZZ Library Footer

(provide 'my-programming)
;;; my-programming.el ends here
