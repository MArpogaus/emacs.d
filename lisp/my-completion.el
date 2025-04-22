;;; my-completion.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-04-22
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/minad/cape.git][cape]]
;; Cape provides Completion At Point Extensions which can be used in combination with Corfu, Company or the default completion UI. The completion backends used by completion-at-point are so called completion-at-point-functions (Capfs).

(use-package cape
  :init
  (define-key my/leader-map (kbd ".") (cons "completion" 'cape-prefix-map))
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

  ;; The advices are only needed on Emacs 28 and older.
  (when (< emacs-major-version 29)
    ;; Silence the pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

    ;; Ensure that pcomplete does not write to the buffer
    ;; and behaves as a pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

;; [[https://github.com/emacs-citar/citar.git][citar]]
;; Citar provides a highly-configurable completing-read front-end to browse and act on BibTeX, BibLaTeX, and CSL JSON bibliographic data, and LaTeX, markdown, and org-cite editing support.

(use-package citar
  :after nerd-icons
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function 'embark-act)
  (citar-notes-paths (list (concat denote-directory "bib_notes/")))
  (citar-templates `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
                     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                     (note . ,(concat "${title}\n"
                                      "#+AUTHOR: ${author editor}\n"
                                      "#+DATE: ${date}\n"
                                      "#+SOURCE: ${doi url}\n"
                                      "#+CUSTOM_ID: ${=key= id}\n"
                                      "#+cite_export: biblatex ieee\n"
                                      (concat "#+bibliography: " (car citar-bibliography) "\n\n")
                                      "* Notes :ignore:\n"
                                      ":PROPERTIES:\n"
                                      ":NOTER_DOCUMENT: ${file} \n"
                                      ":END:\n\n"
                                      "* Summary :childless:showchildren:export:\n"
                                      "This is a summary of [cite/t:@${=key=}].\n"
                                      "** Bibliography :ignore:\n"
                                      ))))
  (citar-symbol-separator "  ")
  :config
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-octicon
              "nf-oct-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-pencil"
              :face 'nerd-icons-blue
              :v-adjust 0.01)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icons-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons
              citar-indicator-cited-icons))
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  ;;:bind
  ;;(:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :hook
  ((LaTeX-mode . citar-capf-setup)
   (org-mode . citar-capf-setup)))

(use-package citar-embark
  :hook
  ((LaTeX-mode . citar-embark-mode)
   (org-mode . citar-embark-mode)))

;; [[https://github.com/minad/consult.git][consult]]
;; Additional featureful completion commands.

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (([remap Info-search] . consult-info)
         ([remap recentf-open] . consult-recent-file)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap project-list-buffers]          . consult-project-buffer)
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         :map my/buffer-map
         ("b" . consult-buffer)                ;; orig. switch-to-buffer
         ("w" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("f" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         :map goto-map
         ;; M-g bindings in `goto-map'
         ("e" . consult-compile-error)
         ("f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("g" . consult-goto-line)             ;; orig. goto-line
         ("o" . consult-outline)               ;; Alternative: consult-org-heading
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map search-map
         ("d" . consult-find)
         ("D" . consult-locate)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ;; Isearch integration
         ("e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :custom
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
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

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Configure a different project root function.
  (with-eval-after-load 'projectile
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-function (lambda (_) (projectile-project-root)))))

;; [[https://github.com/liuyinz/consult-todo.git][consult-todo]]
;; Searching and jumping to TODO keywords using consult.

(use-package consult-todo
  :after consult hl-todo)

;; [[https://github.com/emacs-straight/corfu.git][corfu]]
;; Corfu is the minimalistic in-buffer completion counterpart of the Vertico minibuffer UI.

(use-package corfu
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold nil)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (read-extended-command-predicate
   #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Additional Customisations
  (corfu-cycle t)                     ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                      ;; Enable auto completion
  (corfu-auto-prefix 2)               ;; Complete with less prefix keys
  (corfu-quit-no-match 'separator)    ;; Quit auto complete if there is no match
  (corfu-quit-at-boundary 'separator) ;; Never quit at completion boundary
  (corfu-preview-current nil)         ;; Disable current candidate preview
  (corfu-preselect 'directory)        ;; Preselect the fisrt canidate exept for directories select the prompt
  (corfu-on-exact-match 'show)        ;; Dont insert candidate automatically, just show them
  :preface
  ;; Completing in the minibuffer
  (defun my/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  ;; https://github.com/minad/corfu/wiki#same-key-used-for-both-the-separator-and-the-insertion
  (defun my/corfu-insert-separator-advice (orig-func &rest args)
    "Quit completion if there exist no candiates"
    (interactive)
    (if (and (cdr corfu--candidates)
             ;; next char is not letter or number
             (not (string-match-p "[[:alnum:]]" (string (char-after)))))
        (progn
          (apply orig-func args))
      (progn
        (corfu-quit)
        (insert " "))))
  :config
  ;; Free the RET key for less intrusive behavior.
  (keymap-unset corfu-map "RET")
  (require 'corfu-echo)
  (require 'corfu-history)
  (require 'corfu-popupinfo)
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
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  ((elpaca-after-init . global-corfu-mode)
   (elpaca-after-init . corfu-popupinfo-mode)
   (elpaca-after-init . corfu-echo-mode)
   (elpaca-after-init . corfu-history-mode)
   ;; disable auto completion for eshell, such that the completion behavior is similar to widely used shells like Bash, Zsh or Fish.
   (eshell-mode-hook . (lambda ()
                         (setq-local corfu-auto nil)
                         (corfu-mode)))
   ;; Enable minibuffer completion
   (minibuffer-setup . my/corfu-enable-always-in-minibuffer)))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :hook
  (global-corfu-mode . corfu-terminal-mode))

;; dabbrev :build_in:

(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; [[https://github.com/oantolin/embark.git][embark]]
;; Embark makes it easy to choose a command to run based on what is near point, both during a minibuffer completion session (in a way familiar to Helm or Counsel users) and in normal buffers.

(use-package embark
  :after which-key
  :bind
  (("C-." . embark-act)         ;; pick some com fortable binding
   ("C-:" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   :map vertico-map
   ("C-SPC" . embark-select))   ;; good alternative: M-.

  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)

  :preface
  ;; The built-in embark-verbose-indicator displays actions in a buffer along with their keybindings and the first line of their docstrings.
  ;; Users desiring a more compact display can use which-key instead with the following configuration:
  ;; ref.: https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
  The which-key help message will show the type and value of the
  current target followed by an ellipsis if there are further
  targets."
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

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using
the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  :config
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)

  (setopt embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

;; [[https://github.com/oantolin/embark/blob/master/embark-consult.el][embark-consult]]
;; Consult users will also want the embark-consult package.

(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; [[https://github.com/svaante/lsp-snippet.git][lsp-snippet]]

(use-package lsp-snippet
  :ensure (:host github :repo "svaante/lsp-snippet")
  :demand t
  :after tempel eglot
  :config
  ;; Initialize lsp-snippet -> tempel in eglot
  (lsp-snippet-tempel-eglot-init))

;; [[https://github.com/minad/marginalia.git][marginalia]]
;; Marginalia in the minibuffer.

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :hook
  (vertico-mode . marginalia-mode))

;; [[https://github.com/rainstormstudio/nerd-icons-completion.git][nerd-icons-completion]]

(use-package nerd-icons-completion
  :after marginalia vertico
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; [[https://github.com/LuigiPiucco/nerd-icons-corfu.git][nerd-icons-corfu]]
;; Icons for corfu via nerd-icons.

(use-package nerd-icons-corfu
  :after nerd-icons
  :preface
  (defun my/add-nerd-icons-formatter nil
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  :hook
  (corfu-mode . my/add-nerd-icons-formatter))

;; [[https://github.com/oantolin/orderless.git][orderless]]
;; Emacs completion style that matches multiple regexps in any order

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; [[https://github.com/minad/tempel.git][tempel]]
;; Tempel is a tiny template package for Emacs, which uses the syntax of the Emacs Tempo library. Tempo is an ancient temple of the church of Emacs. It is 27 years old, but still in good shape since it successfully resisted change over the decades. However it may look a bit dusty here and there. Therefore we present Tempel, a new implementation of Tempo with inline expansion and integration with recent Emacs facilities. Tempel takes advantage of the standard completion-at-point-functions mechanism which is used by Emacs for in-buffer completion.

;; Configure Tempel
(use-package tempel
  :custom
  ;; Require trigger prefix before template name when completing.
  ;; (tempel-trigger-prefix ">")
  (tempel-path (expand-file-name "templates.eld" emacs-config-directory))

  :bind (("M-+" . tempel-expand) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("C-SPC" . tempel-next)
         ("S-C-SPC" . tempel-previous))

  :preface
  ;; Setup completion at point
  (defun my/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  :hook
  ((conf-mode . my/tempel-setup-capf)
   (prog-mode . my/tempel-setup-capf)
   (text-mode . my/tempel-setup-capf))
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; [[https://github.com/Crandel/tempel-collection.git][tempel-collection]]
;; Collection of tempel templates.
;; The package is young and doesn't have comprehensive coverage.

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

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate
   #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)

  ;; Enable Mouse support
  (vertico-mouse-mode t)
  :preface
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (require 'vertico-directory)
  :bind
  ;; Improve directory navigation
  (:map vertico-map
        ("<return>" . vertico-directory-enter))
  :hook
  ((minibuffer-setup . cursor-intangible-mode)
   (elpaca-after-init . vertico-mode)))

;; Library Footer

(provide 'my-completion)
;;; my-completion.el ends here
