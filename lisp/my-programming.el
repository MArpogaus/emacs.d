;;; my-programming.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-09
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
;; c i o O 0 A R=) then chain until a foreign key exits.  =C-c C-c= interrupts
;; the running cell, as in the REPL (with =standard-keys-mode= that is
;; =C-p C-c=; note it shadows =python-shell-send-buffer= in cell buffers).
;; No other prefix bindings, and no plain speed keys, which meow's normal
;; state would shadow.  Rarely used commands (split, merge, markdown cells)
;; are =M-x= only.  Cell results are rendered inline by the =overblock-pycell= package below.

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
  ;; code-cells' C-c % map claims the repeat-map of our commands.
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
        ("C-M-<return>" . overblock-pycell-restart-and-run-all)
        ;; navigation
        ("C-<up>"       . code-cells-backward-cell)
        ("C-<down>"     . code-cells-forward-cell)
        ;; cell structure; code-cells' move leaves results behind
        ("M-S-<up>"     . overblock-pycell-move-cell-up)
        ("M-S-<down>"   . overblock-pycell-move-cell-down)
        ;; outline
        ("M-S-<right>"  . outline-demote)
        ("M-S-<left>"   . outline-promote)
        ("C-S-<tab>"    . outline-cycle-buffer)
        ("C-<backtab>"  . outline-cycle-buffer)
        ;; interrupt; shadows `python-shell-send-buffer'
        ("C-c C-c"      . overblock-run-interrupt)
        ;; JupyterLab-like command mode, also on `SPC j'
        :map my/code-cells-repeat-map
        ("n" . code-cells-forward-cell)
        ("p" . code-cells-backward-cell)
        ("j" . code-cells-forward-cell)
        ("k" . code-cells-backward-cell)
        ("e" . code-cells-eval)
        ("r" . code-cells-eval-and-step)
        ("a" . my/code-cells-insert-above)
        ("b" . my/code-cells-insert-below)
        ("c" . overblock-run-copy-output)
        ("i" . overblock-run-interrupt)
        ("o" . overblock-run-toggle-output)
        ("O" . overblock-run-discard-output)
        ("0" . overblock-pycell-restart)
        ("A" . overblock-run-above)
        ("R" . overblock-pycell-restart-and-run-all))
  :hook
  (python-base-mode . code-cells-mode-maybe))

;; [[https://github.com/MArpogaus/overblock][overblock]] :own:
;; Notebook style results for Python code cells: the output of an evaluated
;; cell shows up below it, and markdown cells render in place.  This used to
;; be a long block right here; it is a package now, so only the parts that
;; are specific to this configuration remain — among them that the terminal
;; here draws the icon font, which is the one thing Emacs cannot find out
;; for itself.

;; The layer the three below are built on: text blocks over a region of
;; a buffer.  It draws nothing by itself.
(use-package overblock
  :ensure (overblock :host github :repo "MArpogaus/overblock"
                     :branch "dev"
                     :files ("overblock.el" "overblock-repl.el"
                             "overblock-run.el"))
  :custom
  ;; The same word the panel header goes by: without it every bar of
  ;; every package here falls back to its plain candidates in a
  ;; terminal.
  (overblock-terminal-glyphs my/terminal-glyphs)
  :config
  ;; Every buffer the family opens beside a file goes to the right:
  ;; a result popped out (*overblock-pycell: f:12*, *overblock-rmd: …*),
  ;; a markdown cell being edited (*overblock-pycell md: …*), a doc
  ;; string being edited (*overblock-pydoc: …*).  The shells go to
  ;; the bottom with every other comint buffer.
  (with-eval-after-load 'auto-side-windows
    (add-to-list 'auto-side-windows-right-buffer-names
                 "^\\*overblock-[a-z]+\\(?: md\\)?: .*\\*$")))

;; The runner is a file of the layer, and the map of a result popped
;; out into its own buffer lives there: bound once that file loads,
;; from any notebook.  A pop-out remembers the shell it came from, so
;; the interrupt works where a long run is watched.
(use-package overblock-run
  :ensure nil
  :after overblock
  :bind
  (:map overblock-run-pop-map
        ("C-c C-c" . overblock-run-interrupt)))

;; What draws a LaTeX fragment in a rendering: `latex' and `dvisvgm'
;; in a process of its own, one compile per equation, and an SVG that
;; carries neither the colour nor the size — both are given to it when
;; it is drawn, so a theme change and a `text-scale-adjust' cost no
;; LaTeX.  `overblock-md' requires it.
;;
;; Declared with its URL, and before what needs it: the package is on
;; MELPA, and elpaca can only resolve an id from a menu it has
;; fetched — without this it answered "Unable to determine recipe
;; URL", and took every package that requires it down with it.
(use-package latex-to-svg-backend
  :ensure (latex-to-svg-backend
           :host github :repo "alberti42/latex-to-svg-backend"))

;; Markup rendered to text: a converter and shr, or a mode's font lock.
;; `overblock-md-preview-mode' renders a markdown buffer block by block
;; and gives the source back where you click.  On for every markdown
;; file but an Rmd, whose prose the notebook below renders itself; the
;; hook is what loads the package, so nothing here costs a start-up.
(use-package overblock-md
  :ensure (overblock-md :host github :repo "MArpogaus/overblock"
                        :branch "dev"
                        :files ("overblock-md.el" "overblock-md-preview.el"))
  :preface
  (defun my/overblock-md-preview-maybe ()
    "Turn on `overblock-md-preview-mode', unless this is an Rmd file."
    (unless (and buffer-file-name
                 (string-match-p "\\.[rR]md\\'" buffer-file-name))
      (overblock-md-preview-mode)))
  :hook (markdown-mode . my/overblock-md-preview-maybe))

;; Python doc strings rendered where they are written, with a bar of
;; their own.  On in every Python buffer; the doc string point is in
;; shows its source, so writing one is writing plain text.
(use-package overblock-pydoc
  :ensure (overblock-pydoc :host github :repo "MArpogaus/overblock"
                           :branch "dev"
                           :files ("overblock-pydoc.el"))
  :hook (python-base-mode . overblock-pydoc-mode))

(use-package overblock-pycell
  ;; Five packages live in that repository — the layer, the markup
  ;; renderer, the doc string mode, this notebook and the one for R
  ;; Markdown — so the recipe names the files of this one.  The layer and the renderer arrive as
  ;; its dependencies, each with a recipe of its own below.
  :ensure (overblock-pycell :host github :repo "MArpogaus/overblock"
                            :branch "dev"
                            :files ("overblock-pycell.el"))
  ;; The package installs no hook itself; this is where it turns on.
  :hook (code-cells-mode . overblock-pycell-mode-maybe))

;; The same notebook for an =.Rmd= file: a chunk runs in the R that ESS
;; started and its output shows under it, and the prose between chunks
;; renders as markdown.  The keys are the notebook's, so a hand that
;; knows one knows the other.
(use-package overblock-rmd
  :ensure (overblock-rmd :host github :repo "MArpogaus/overblock"
                         :branch "dev"
                         :files ("overblock-rmd.el"))
  :after ess
  :hook (markdown-mode . overblock-rmd-mode-maybe)
  :preface
  ;; The same "command mode" as the Python notebook's: the same letters
  ;; do the same things, entered through any of them and chained until
  ;; a foreign key exits.  No insert keys (a b), because a chunk is
  ;; written by hand between its fences.
  (defvar my/overblock-rmd-repeat-map (make-sparse-keymap)
    "Repeat map for chunk commands; mirrors `my/code-cells-repeat-map'.")
  :config
  (my/repeatize-keymap 'my/overblock-rmd-repeat-map)
  :bind
  ;; The keys of the Python notebook, one for one.
  (:map overblock-rmd-mode-map
        ("C-<return>"   . overblock-rmd-run-chunk)
        ("S-<return>"   . overblock-rmd-run-chunk-and-step)
        ("C-S-<return>" . overblock-rmd-run-chunk-and-step)
        ("C-M-<return>" . overblock-rmd-restart-and-run-all)
        ("C-<up>"       . overblock-rmd-backward-chunk)
        ("C-<down>"     . overblock-rmd-forward-chunk)
        ("C-c C-c"      . overblock-run-interrupt)
        :map my/overblock-rmd-repeat-map
        ("n" . overblock-rmd-forward-chunk)
        ("p" . overblock-rmd-backward-chunk)
        ("j" . overblock-rmd-forward-chunk)
        ("k" . overblock-rmd-backward-chunk)
        ("e" . overblock-rmd-run-chunk)
        ("r" . overblock-rmd-run-chunk-and-step)
        ("c" . overblock-run-copy-output)
        ("i" . overblock-run-interrupt)
        ("o" . overblock-run-toggle-output)
        ("O" . overblock-run-discard-output)
        ("0" . overblock-rmd-restart)
        ("A" . overblock-run-above)
        ("R" . overblock-rmd-restart-and-run-all)))

;; [[https://github.com/mickeynp/combobulate.git][combobulate]]
;; Structured Editing and Navigation in Emacs.

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate" :nonrecursive t)
  :custom
  ;; Disable combobulate key prefix
  (combobulate-key-prefix nil)
  :preface
  (defun my/combobulate-mode-maybe ()
    "Turn `combobulate-mode' on in a buffer that tree-sitter has parsed.
The mode errors where the language has no grammar, and `set-auto-mode'
reports that error rather than raising it: the rest of the major mode
setup is dropped, `after-change-major-mode-hook' included, and every
globalized minor mode stays off in the buffer."
    (when (treesit-parser-list) (combobulate-mode 1)))
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
  ((prog-mode yaml-ts-mode) . my/combobulate-mode-maybe))

;; [[https://github.com/svaante/dape.git][dape]]
;; Debug Adapter Protocol for Emacs.

(use-package dape
  :preface
  (defvar my/debug-map (make-sparse-keymap) "key-map for debug commands")
  :init
  (define-key my/leader-map (kbd "d") (cons "debug" my/debug-map))
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

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  :hook
  ;; Set breakpints via fringe or margin mouse clicks
  (prog-mode . dape-breakpoint-global-mode))

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
  (defvar-local my/eglot-super-capf nil
    "The capf `my/eglot-capf' added in this buffer.")
  (defun my/eglot-capf ()
    "Put files, eglot and tempel into one capf while eglot manages the buffer."
    (setq-local completion-at-point-functions
                (remq my/eglot-super-capf completion-at-point-functions))
    (when (eglot-managed-p)
      (setq my/eglot-super-capf
            (cape-capf-super #'cape-file #'eglot-completion-at-point #'tempel-complete))
      (push my/eglot-super-capf completion-at-point-functions)))
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
  :mode ("\\.[rR]\\'" . R-mode)
  :custom
  ;; Start R in the file's directory without asking.
  (ess-ask-for-ess-directory nil)
  ;; Do not wait for the echo: a long evaluation would block Emacs.
  (ess-eval-visibly 'nowait)
  ;; No workspace written on exit or read on start.
  (inferior-R-args "--no-save --no-restore-data")
  :config
  (require 'ess-site))

;; [[https://github.com/polymode/poly-R][poly-R]]
;; One buffer, two major modes: markdown for the prose of an =.Rmd= file and
;; =ess-r-mode= inside every =```{r}= chunk, so the chunks are fontified and
;; indented as R.  polymode does that with an indirect buffer per chunk,
;; and would switch to it when the point enters one; =overblock-rmd= below
;; keeps the buffer in its host mode instead, since the switch carried its
;; bars and results along and drew them twice.  Evaluation is the
;; notebook's, on the keys of =overblock-rmd-mode-map=.

;; =poly-markdown+r-mode= is what an =.Rmd= file wants; the =gfm= variant
;; reads GitHub's flavour of markdown, and the noweb one is for =.Rnw=.
;; The results of a chunk are shown inline
;; by [[https://github.com/MArpogaus/overblock][overblock-rmd]], further down.

(use-package poly-R
  :ensure (poly-R :host github :repo "polymode/poly-R")
  :after (ess markdown-mode)
  :mode ("\\.[rR]md\\'" . poly-markdown+r-mode)
  :custom
  ;; Show a chunk's process buffer on error, not the woven file.
  (polymode-display-output-file nil)
  (polymode-display-process-buffers t)
  ;; Do not re-export unmodified chunks.
  (polymode-skip-processing-when-unmodified t))

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
  ;; Show the size of a fold; no fringe indicators, diff-hl is there.
  (hs-display-lines-hidden t)
  ;; TAB at the start of a block line cycles it; <backtab> stays the mode's.
  (hs-cycle-filter #'bolp)
  ;; YAML folds with `outline-indent'.
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

;; [[https://github.com/purcell/inheritenv][inheritenv]]
;; Carry a buffer's environment into the temp buffers that commands make: a
;; buffer-local =process-environment= is invisible to =shell-command= and its
;; like without help.  =ben= brings the package along for the buffers direnv
;; owns; =my/python-env-mode= below uses it for the buffers it owns itself.

(use-package inheritenv
  :demand t)

;; python :build_in:
;; Prefer IPython as the REPL whenever one is on the =PATH= of the buffer's
;; environment.  =executable-find= is evaluated per buffer, so it picks up a
;; project-local IPython installed by =uv= or exported by =ben=, and the result is
;; stored as an absolute path so the REPL keeps using that interpreter.

;; A project without an =.envrc= gets its environment from =C-c v= (=C-p v= with
;; =standard-keys-mode=); =C-c V= drops it again.  An environment is any
;; directory with a =bin/python= inside, so a venv and a conda prefix are the
;; same thing here, and the presence of =conda-meta= says which variable to
;; export.  The command looks in the project root first: it takes the only
;; environment it finds there, asks which one where there are several, and
;; falls back to the environments of =my/python-env-parents= (conda, mamba,
;; =virtualenvwrapper=) where the project holds none.

;; =my/python-env-mode= carries the choice, one buffer at a time, so two
;; projects can use two environments in one session.  It is a globalized mode
;; after the manner of =ben-global-mode=: the mode goes on in every buffer and
;; does nothing until the buffer's project has a choice to apply, which is
;; also what lets a buffer opened later pick the choice up.  A per-major-mode
;; hook cannot do this job, because =python.el= loads inside the first Python
;; buffer's own mode setup, too late for that buffer.

;; To activate an environment without being asked at all, put
;; =my/python-env-path= in a =.dir-locals.el=: the variable is read per buffer
;; and takes precedence over the choice made with the command.

(use-package python
  :ensure nil
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-dedicated 'project)
  :preface
  (defvar my/python-env-parents '("~/.conda/envs" "~/miniconda3/envs"
                                  "~/micromamba/envs" "~/.virtualenvs")
    "Directories that hold one Python environment per subdirectory.")

  (defvar my/python-env-alist nil
    "Alist of (DIRECTORY . ENVIRONMENT) set by `my/python-env-activate'.")

  (defvar-local my/python-env-path nil
    "Python environment of this buffer, taken without asking.
Set it in a `.dir-locals.el' to activate one automatically.  It takes
precedence over anything `my/python-env-activate' chose.")
  (put 'my/python-env-path 'safe-local-variable #'stringp)

  (defvar-local my/python-env--applied nil
    "Environment `my/python-env-mode' put into this buffer.
It also says the environment here is ours, so clearing one never takes
away what `ben' exported.")

  (defun my/python-env-dir ()
    "Return the directory an environment is keyed on: the project root, or this one."
    (if-let* ((project (project-current))) (project-root project) default-directory))

  (defun my/python-env--in (parent)
    "Return every directory in PARENT that holds a Python environment."
    (and (file-directory-p parent)
         (seq-filter (lambda (dir)
                       (file-executable-p (expand-file-name "bin/python" dir)))
                     (directory-files parent t directory-files-no-dot-files-regexp))))

  (defun my/python-env-read ()
    "Return the environment of this project, asking where the answer is open.
The single environment of the project root needs no question; several of
them, or none there at all, do."
    (let ((local (my/python-env--in (my/python-env-dir))))
      (if (and local (not (cdr local)))
          (car local)
        (completing-read "Environment: "
                         (or local (mapcan #'my/python-env--in my/python-env-parents))
                         nil t))))

  (defun my/python-env--clear ()
    "Take back what `my/python-env--set' gave this buffer."
    (when my/python-env--applied
      (kill-local-variable 'exec-path)
      (kill-local-variable 'process-environment)
      (kill-local-variable 'my/python-env--applied)))

  (defun my/python-env--set ()
    "Put the environment chosen for this buffer into this buffer.
Whatever an earlier call gave it goes first, so a new choice replaces the
old one instead of stacking on it.  Buffers under an `.envrc' get nothing
from us: `ben' exports an environment there already, and direnv is the
one that says what it holds."
    (my/python-env--clear)
    (when-let* (((not (eq (bound-and-true-p ben--status) 'on)))
                (env (or my/python-env-path
                         (and my/python-env-alist
                              (cdr (assoc (my/python-env-dir) my/python-env-alist)))))
                (bin (expand-file-name "bin" env)))
      (setq-local my/python-env--applied env
                  exec-path (cons bin exec-path)
                  process-environment
                  (append (list (concat "PATH=" bin path-separator (getenv "PATH"))
                                (concat (if (file-directory-p
                                             (expand-file-name "conda-meta" env))
                                            "CONDA_PREFIX"
                                          "VIRTUAL_ENV")
                                        "=" env))
                          process-environment)))
    ;; Also without an environment, so the old interpreter goes.
    (my/python-set-interpreter))

  (define-minor-mode my/python-env-mode
    "Give this buffer the Python environment chosen for its project.
The environment is the `exec-path' and the `process-environment' of this
buffer alone."
    :init-value nil :lighter nil
    (if my/python-env-mode (my/python-env--set) (my/python-env--clear)))

  (define-globalized-minor-mode my/python-env-global-mode
    my/python-env-mode
    (lambda () (unless (minibufferp) (my/python-env-mode 1)))
    :predicate t)

  (defun my/python-env--refresh (dir)
    "Give every buffer that DIR keys its environment again."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and my/python-env-mode (equal dir (my/python-env-dir)))
          (my/python-env--set)))))

  (defun my/python-env-activate (env)
    "Use ENV in every buffer of this buffer's project, and in the ones to come.
ENV nil drops the choice again, which is `my/python-env-deactivate'."
    (interactive (list (my/python-env-read)))
    (let ((dir (my/python-env-dir)))
      (setf (alist-get dir my/python-env-alist nil (null env) #'equal) env)
      (my/python-env--refresh dir)))

  (defun my/python-env-deactivate ()
    "Drop the environment chosen for this buffer's project."
    (interactive)
    (my/python-env-activate nil))

  (defun my/python-set-interpreter ()
    "Use IPython as `python-shell-interpreter' when one is on `exec-path'."
    (when (derived-mode-p 'python-base-mode)
      (if-let* ((ipython (or (executable-find "ipython")
                             (executable-find "ipython3"))))
          (setq-local python-shell-interpreter ipython
                      python-shell-interpreter-args "-i --simple-prompt --no-color-info")
        (kill-local-variable 'python-shell-interpreter)
        (kill-local-variable 'python-shell-interpreter-args))))
  :bind
  (:map python-base-mode-map
        ("C-c v" . my/python-env-activate)
        ("C-c V" . my/python-env-deactivate))
  :init
  (add-hook 'elpaca-after-init-hook #'my/python-env-global-mode)
  ;; Directory-local variables are read after the major mode is set.
  (add-hook 'hack-local-variables-hook #'my/python-env-mode)
  ;; These start their processes in a temp buffer, which inherits none of the
  ;; environment of the buffer the command was called from.
  (dolist (command '(shell-command shell-command-to-string async-shell-command
                                   compile org-babel-eval))
    (advice-add command :around #'inheritenv-apply))
  :config
  ;; Also after `ben' applies a direnv: only then is `exec-path' local.
  (add-hook 'python-base-mode-hook #'my/python-set-interpreter)
  (add-hook 'ben-after-apply-hook #'my/python-set-interpreter))

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
  (treesit-enabled-modes t))

;; yaml-ts-mode :build_in:
;; The emacs major mode for editing files in the YAML data serialization format.

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")

;; ZZ Library Footer

(provide 'my-programming)
;;; my-programming.el ends here
