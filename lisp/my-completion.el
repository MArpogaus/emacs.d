;;; my-completion.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-09
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/minad/cape.git][cape]]
;; Cape provides completion-at-point extensions (Capfs) for Corfu or the default completion UI.

(use-package cape
  :init
  (define-key my/leader-map (kbd ".") (cons "completion" 'cape-prefix-map))
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; [[https://github.com/emacs-citar/citar.git][citar]]
;; Citar provides a highly-configurable completing-read front-end to browse and act on BibTeX, BibLaTeX, and CSL JSON bibliographic data, and LaTeX, markdown, and org-cite editing support.

(use-package citar
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-biblatex-options "hyperref=true,url=true,backend=biber,natbib=true")
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function 'embark-act)
  (citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
                     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")))
  (citar-symbol-separator "  ")
  :config
  (setq citar-indicators
        (list
         (citar-indicator-create
          :symbol (nerd-icons-faicon
                   "nf-fa-file_o"
                   :face 'nerd-icons-green
                   :v-adjust -0.1)
          :function #'citar-has-files
          :padding "  " ; need this because the default padding is too low for these icons
          :tag "has:files")
         (citar-indicator-create
          :symbol (nerd-icons-octicon
                   "nf-oct-link"
                   :face 'nerd-icons-orange
                   :v-adjust 0.01)
          :function #'citar-has-links
          :padding "  "
          :tag "has:links")
         (citar-indicator-create
          :symbol (nerd-icons-mdicon
                   "nf-md-pencil"
                   :face 'nerd-icons-blue
                   :v-adjust 0.01)
          :function #'citar-has-notes
          :padding "  "
          :tag "has:notes")
         (citar-indicator-create
          :symbol (nerd-icons-faicon
                   "nf-fa-circle_o"
                   :face 'nerd-icons-green)
          :function #'citar-is-cited
          :padding "  "
          :tag "is:cited")))
  :hook
  ((LaTeX-mode . citar-capf-setup)
   (org-mode . citar-capf-setup)))

(use-package citar-embark
  :hook
  ((LaTeX-mode . citar-embark-mode)
   (org-mode . citar-embark-mode)))

;; [[https://github.com/minad/consult.git][consult]]
;; Additional featureful completion commands.

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; Remap default commands with consult equivalents
         ([remap Info-search]                   . consult-info)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap load-theme]                    . consult-theme)
         ([remap locate]                        . consult-locate)
         ([remap man]                           . consult-man)
         ([remap project-list-buffers]          . consult-project-buffer)
         ([remap project-switch-to-buffer]      . consult-project-buffer)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap recentf-open]                  . consult-recent-file)
         ([remap repeat-complex-command]        . consult-complex-command)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-tab]    . consult-buffer-other-tab)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap yank-pop]                      . consult-yank-pop)
         :map goto-map
         ;; M-g bindings in `goto-map'
         ("e"                                   . consult-compile-error)
         ("f"                                   . consult-flymake)             ;; Alternative: consult-flycheck
         ("o"                                   . consult-outline)             ;; Alternative: consult-org-heading
         ("m"                                   . consult-mark)
         ("k"                                   . consult-global-mark)
         ("i"                                   . consult-imenu)
         ("I"                                   . consult-imenu-multi)
         :map search-map
         ("d"                                   . consult-find)
         ("D"                                   . consult-locate)
         ("g"                                   . consult-grep)
         ("G"                                   . consult-git-grep)
         ("r"                                   . consult-ripgrep)
         ("l"                                   . consult-line)
         ("L"                                   . consult-line-multi)
         ("k"                                   . consult-keep-lines)
         ("u"                                   . consult-focus-lines)
         ;; Isearch integration
         ("e"                                   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"                                 . consult-isearch-history)     ;; orig. isearch-edit-string
         ("M-s e"                               . consult-isearch-history)     ;; orig. isearch-edit-string
         ("M-s l"                               . consult-line)                ;; needed by consult-line to detect isearch
         ("M-s L"                               . consult-line-multi)          ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s"                                 . consult-history)             ;; orig. next-matching-history-element
         ("M-r"                                 . consult-history))            ;; orig. previous-matching-history-element

  :custom
  ;; Nicer register previews.
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key "<") ;; "C-+"
  :config
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; [[https://github.com/emacs-straight/corfu.git][corfu]]
;; Corfu is the minimalistic in-buffer completion counterpart of the Vertico minibuffer UI.

(use-package corfu
  :custom
  ;; In every minibuffer but Vertico's and a password prompt, without
  ;; automatic echo and popup.
  (global-corfu-minibuffer
   (lambda ()
     (unless (or (bound-and-true-p vertico--input)
                 (eq (current-local-map) read-passwd-map))
       (setq-local corfu-echo-delay nil
                   corfu-popupinfo-delay nil)
       t)))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Additional Customisations
  (corfu-cycle t)                     ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                      ;; Enable auto completion
  (corfu-auto-trigger ".([{+-*/=,$")  ;; Custom trigger characters
  (corfu-auto-prefix 2)               ;; Complete with less prefix keys
  (corfu-preview-current nil)         ;; Disable current candidate preview
  (corfu-preselect 'directory)        ;; Preselect the first candidate; for directories the prompt
  (corfu-on-exact-match 'show)        ;; Don't insert candidate automatically, just show them
  (corfu-popupinfo-delay '(1.0 . 0.0));; Initial and subsequent delay for info popup in seconds
  :preface
  ;; https://github.com/minad/corfu/wiki#same-key-used-for-both-the-separator-and-the-insertion
  (defun my/corfu-insert-separator-advice (orig-func &rest args)
    "Insert a separator if there are candidates, else quit and insert a space."
    (interactive)
    (if (and (cdr corfu--candidates)
             ;; next char is not letter or number
             (not (string-match-p "[[:alnum:]]" (string (or (char-after) ?\n)))))
        (progn
          (apply orig-func args))
      (progn
        (corfu-quit)
        (insert " "))))
  :config
  ;; Free the RET key for less intrusive behavior.
  (keymap-unset corfu-map "RET")
  (eldoc-add-command #'corfu-insert)
  (advice-add 'corfu-insert-separator :around #'my/corfu-insert-separator-advice)
  :bind
  (("C-SPC"     . completion-at-point)
   :map corfu-map
   ("C-SPC"     . corfu-insert)
   ("<tab>"     . corfu-next)
   ("TAB"       . corfu-next)
   ("<backtab>" . corfu-previous)
   ("SPC"       . corfu-insert-separator)
   ("<escape>"  . corfu-quit))
  :hook
  ;; Corfu everywhere; see also `corfu-exclude-modes'.
  ((elpaca-after-init . global-corfu-mode)
   (elpaca-after-init . corfu-popupinfo-mode)
   (elpaca-after-init . corfu-echo-mode)
   (elpaca-after-init . corfu-history-mode)
   ;; No auto completion in eshell, as in other shells.
   (eshell-mode . (lambda ()
                    (setq-local corfu-auto nil)
                    (corfu-mode)))))

;; dabbrev :build_in:

(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; [[https://github.com/fejfighter/eglot-tempel.git][eglot-tempel]]

(use-package eglot-tempel
  :demand t
  :after eglot
  :config (eglot-tempel-mode))

;; [[https://github.com/oantolin/embark.git][embark]]
;; Embark makes it easy to choose a command to run based on what is near point, both during a minibuffer completion session (in a way familiar to Helm or Counsel users) and in normal buffers.

(use-package embark
  :bind
  (("C-." . my/embark-act-maybe-all)
   ("C-:" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings'
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :preface
  ;; which-key as action menu:
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun my/embark-which-key-indicator ()
    "Return an embark indicator that displays keymaps using which-key.
The which-key help message shows the type and value of the current
target, followed by an ellipsis if there are further targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (defun my/embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'my/embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (defun my/embark-act-all-noconfirm ()
    "Call `embark-act-all' without asking for confirmation."
    (interactive)
    (let (embark-confirm-act-all)
      (embark-act-all)))
  (defun my/embark-act-maybe-all ()
    "Call `my/embark-act-all-noconfirm' if candidates are selected, else fallback to `embark-act'."
    (interactive)
    (if (bound-and-true-p embark--selection)
        (call-interactively #'my/embark-act-all-noconfirm)
      (call-interactively #'embark-act)))
  (defun my/embark-act-or-vertico-enter ()
    "Call `my/embark-act-all-noconfirm' if candidates are selected, else fallback to `vertico-directory-enter'."
    (interactive)
    (if (bound-and-true-p embark--selection)
        (call-interactively #'my/embark-act-all-noconfirm)
      (call-interactively #'vertico-directory-enter)))
  (defun my/embark-select-vertico ()
    "Call `embark-select' on current candidate in vertico buffer, then move on to the next candidate."
    (interactive)
    (call-interactively #'embark-select)
    (vertico-next 1))
  :config
  (setopt embark-indicators
          '(my/embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

  (advice-add #'embark-completing-read-prompter
              :around #'my/embark-hide-which-key-indicator))

;; [[https://github.com/oantolin/embark/blob/master/embark-consult.el][embark-consult]]
;; Embark actions and export for Consult commands.

(use-package embark-consult
  :after embark)

;; [[https://github.com/minad/marginalia.git][marginalia]]
;; Marginalia in the minibuffer.

(use-package marginalia
  :after vertico
  :hook
  (vertico-mode . marginalia-mode))

;; [[https://github.com/rainstormstudio/nerd-icons-completion.git][nerd-icons-completion]]

(use-package nerd-icons-completion
  :hook
  ((elpaca-after-init . nerd-icons-completion-mode)
   (marginalia-mode . nerd-icons-completion-marginalia-setup)))

;; [[https://github.com/LuigiPiucco/nerd-icons-corfu.git][nerd-icons-corfu]]
;; Icons for corfu via nerd-icons.

(use-package nerd-icons-corfu
  :init
  ;; The formatter is autoloaded; nothing else loads this package.
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;; [[https://github.com/oantolin/orderless.git][orderless]]
;; Emacs completion style that matches multiple regexps in any order.

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; [[https://github.com/minad/tempel.git][tempel]]
;; Tempel is a tiny template package with the syntax of the Emacs Tempo library and inline expansion through completion-at-point-functions.

(use-package tempel
  :custom
  (tempel-path (expand-file-name "templates.eld" emacs-config-directory))

  :bind (("M-+" . tempel-expand) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("C-SPC" . tempel-next)
         ("S-C-SPC" . tempel-previous))

  :preface
  ;; Setup completion at point
  (defun my/tempel-setup-capf ()
    ;; `tempel-complete' before the mode's capf, so it is tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  :hook
  ((conf-mode . my/tempel-setup-capf)
   (prog-mode . my/tempel-setup-capf)
   (text-mode . my/tempel-setup-capf)))

;; [[https://github.com/Crandel/tempel-collection.git][tempel-collection]]
;; Collection of tempel templates.

(use-package tempel-collection
  :demand t
  :after tempel)

;; [[https://github.com/emacs-straight/vertico.git][vertico]]
;; Vertico provides a performant and minimalistic vertical completion UI based on the default completion system.


(use-package vertico
  :custom
  ;; Show more candidates
  (vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate
   #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)

  :bind
  (:map vertico-map
        ("C-SPC" . my/embark-select-vertico)
        ("<return>" . my/embark-act-or-vertico-enter))
  :hook
  ((minibuffer-setup . cursor-intangible-mode)
   (elpaca-after-init . vertico-mode)
   (elpaca-after-init . vertico-mouse-mode)))

;; ZZ Library Footer

(provide 'my-completion)
;;; my-completion.el ends here
