;;; my-ui.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-03
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/LionyxML/auto-dark-emacs.git][auto-dark]]
;; Auto-Dark-Emacs is an auto changer between 2 themes, dark/light, following MacOS, Linux or Windows Dark Mode settings.

(use-package auto-dark
  :custom
  (auto-dark-themes '((doom-one) (doom-one-light)))
  ;; (auto-dark-themes '((doom-ayu-mirage) (doom-ayu-light)))
  ;; (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  :hook elpaca-after-init)

;; [[https://github.com/joaotavora/breadcrumb][breadcrumb]]
;; Emacs headerline indication of where you are in a large project.

(use-package breadcrumb
  :demand t
  :custom
  (breadcrumb-imenu-crumb-separator "  "))

;; display-line-numbers :build_in:
;; Enable line numbers for some modes

(use-package display-line-numbers
  :ensure nil
  :custom
  ;; Count total number of line on startup for correct width
  (display-line-numbers-width-start t)
  :hook
  (((prog-mode conf-mode text-mode) . display-line-numbers-mode)
   ;; disable for org mode
   (org-mode . (lambda () (display-line-numbers-mode -1)))))

;; [[https://github.com/doomemacs/themes][doom-themes]]

(use-package doom-themes
  :custom
  (doom-ayu-light-brighter-comments t)
  :preface
  (defun my/patch-doom-themes (&rest args)
    (ignore args)
    (set-face-foreground 'tab-bar (face-foreground 'tab-bar-tab)))
  :init
  (advice-add 'load-theme :after #'my/patch-doom-themes))
(use-package ember-theme
  :ensure (:host github :repo "ember-theme/emacs")
  :after doom-themes)

;; mode-line-invisible :build_in:
;; Hide the mode line of a buffer.  Emacs has the buffer-local mode, and the
;; whole-session switch is a globalized mode of its own.

(define-globalized-minor-mode my/global-mode-line-invisible-mode
  mode-line-invisible-mode
  (lambda () (mode-line-invisible-mode 1)))

(use-package emacs
  :ensure nil
  :hook
  (symbols-outline-mode . mode-line-invisible-mode))

;; hl-line :build_in:

;; Highlighting of the current line (native mode)


(use-package hl-line
  :ensure nil
  :hook
  ((prog-mode org-mode text-mode tabulated-list-mode) . hl-line-mode))

;; [[https://github.com/tarsius/hl-todo.git][hl-todo]]
;; Highlight TODO keywords.

(use-package hl-todo
  :preface
  (defun my/hl-todo-register-flymake-report-fn ()
    (add-hook #'flymake-diagnostic-functions #'hl-todo-flymake))
  :hook
  (((prog-mode conf-mode LaTeX-mode yaml-ts-mode) . hl-todo-mode)
   (flymake-mode . my/hl-todo-register-flymake-report-fn)))

;; [[https://github.com/jdtsmith/indent-bars.git][indent-bars]]
;; Fast, configurable indentation guide-bars for Emacs.

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python
                               argument_list parameters list list_comprehension dictionary
                               dictionary_comprehension parenthesized_expression subscript)))
  (indent-bars-treesit-scope '((python
                                function_definition class_definition for_statement
                                if_statement with_statement while_statement)))
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-pad-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  :hook
  ((python-base-mode yaml-ts-mode emacs-lisp-mode) . indent-bars-mode))

;; [[https://github.com/mickeynp/ligature.el.git][ligature]]
;; Display typographical ligatures in Emacs. -> -+
;; =Display typographical ligatures in Emacs.-> -+=
;; www Fl  Tl ff fi  fj  fl  ft ffi ffj ffl

;; *Note:* Fira Code is loaded [[id:5eb92f35-9f4c-436f-864f-c270dc2f1f29][here.]]

(use-package ligature
  :if (display-graphic-p)
  :preface
  (defun my/setup-ligatures ()
    ;; Enable traditional ligatures in every possible major mode
    (ligature-set-ligatures 't '("www" "Fl"  "Tl" "ff" "fi"  "fj"  "fl"  "ft" "ffi" "ffj" "ffl"))
    ;; Enable all Cascadia and Fira Code ligatures in programming modes
    (ligature-set-ligatures
     '(prog-mode org-mode)
     '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
       ;; =:= =!=
       ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
       ;; ;; ;;;
       (";" (rx (+ ";")))
       ;; && &&&
       ("&" (rx (+ "&")))
       ;; !! !!! !. !: !!. != !== !~
       ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
       ;; ?? ??? ?:  ?=  ?.
       ("?" (rx (or ":" "=" "\." (+ "?"))))
       ;; %% %%%
       ("%" (rx (+ "%")))
       ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
       ;; |->>-||-<<-| |- |== ||=||
       ;; |==>>==<<==<=>==//==/=!==:===>
       ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                       "-" "=" ))))
       ;; \\ \\\ \/
       ("\\" (rx (or "/" (+ "\\"))))
       ;; ++ +++ ++++ +>
       ("+" (rx (or ">" (+ "+"))))
       ;; :: ::: :::: :> :< := :// ::=
       (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
       ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
       ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                       "="))))
       ;; .. ... .... .= .- .? ..= ..<
       ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
       ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
       ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
       ;; www wwww
       ("w" (rx (+ "w")))
       ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
       ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
       ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
       ;; << <<< <<<<
       ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                       "-"  "/" "|" "="))))
       ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
       ;; >> >>> >>>>
       (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
       ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
       ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                    (+ "#"))))
       ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
       ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
       ;; __ ___ ____ _|_ __|____|_
       ("_" (rx (+ (or "_" "|"))))
       ;; Fira code: 0xFF 0x12
       ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
       ;; The few not covered by the regexps.
       "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
    ;; Enable star ligatures only for programming modes to fix incompatibility
    ;; with `org-modern'
    (ligature-set-ligatures
     '(prog-mode)
     ;; *> */ *)  ** *** ****
     '(("*" (rx (or ">" "/" ")" (+ "*"))))))
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode))
  :hook
  (elpaca-after-init . my/setup-ligatures))

;; [[https://gitlab.com/jabranham/mixed-pitch.git][mixed-pitch]]

(use-package mixed-pitch
  :custom
  (mixed-pitch-variable-pitch-cursor nil)
  :config
  (setq mixed-pitch-fixed-pitch-faces (append mixed-pitch-fixed-pitch-faces
                                              '(corfu-default
                                                corfu-current
                                                org-hide
                                                org-inline-src-block
                                                org-modern-label)))
  :custom-face
  (variable-pitch ((t (:family "Adwaita Sans"))))
  ;; (variable-pitch ((t (:family "Iwona"))))
  ;; (variable-pitch ((t (:family "Bookman Old Style"))))
  ;; (variable-pitch ((t (:family "ETBookOT" :weight thin))))
  :hook
  ((org-mode markdown-mode help-mode helpful-mode messages-buffer-mode Custom-mode) . mixed-pitch-mode))

;; [[https://gitlab.com/jessieh/mood-line.git][mood-line]]

(use-package mood-line
  :custom
  ;; Use pretty Fira Code-compatible glyphs
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  (mood-line-format
   (mood-line-defformat
    :padding ""
    :left
    (((my/get-bar-image my/modeline-height 3 nil)                               . " ")
     ((mood-line-segment-modal)                                                 . " ")
     ((mood-line-segment-multiple-cursors)                                      . " ")
     " "
     ((when (featurep 'breadcrumb) (breadcrumb-imenu-crumbs))                   . " "))
    :right
    (((mood-line-segment-process)                                               . " ")
     ((mood-line-segment-buffer-status)                                         . " ")
     ;; ((mood-line-segment-misc-info)                                             . " ")
     ((format-mode-line mode-line-misc-info)                                    . " ")
     ((mood-line-segment-major-mode)                                            . " ")
     ((mood-line-segment-vc)                                                    . " ")
     ((mood-line-segment-checker)                                               . " "))))
  (mood-line-segment-modal-meow-state-alist
   `((normal ,(nerd-icons-mdicon "nf-md-alpha_n_circle") . font-lock-variable-name-face)
     (insert ,(nerd-icons-mdicon "nf-md-alpha_i_circle") . font-lock-string-face)
     (keypad ,(nerd-icons-mdicon "nf-md-alpha_k_circle") . font-lock-keyword-face)
     (beacon ,(nerd-icons-mdicon "nf-md-alpha_b_circle") . font-lock-type-face)
     (motion ,(nerd-icons-mdicon "nf-md-alpha_m_circle") . font-lock-constant-face)))
  :hook elpaca-after-init)

;; [[https://github.com/rainstormstudio/nerd-icons.el.git][nerd-icons]]
;; A Library for Nerd Font icons. Required for modline icons.

(use-package nerd-icons)

;; [[https://github.com/haji-ali/procress.git][procress]]
;; display LaTeX compilation information in the mode line

(use-package procress
  :ensure (:host github :repo "haji-ali/procress")
  :commands procress-auctex-mode
  :hook
  (LaTeX-mode . procress-auctex-mode)
  :config
  (procress-load-default-svg-images))

;; [[https://github.com/emacs-straight/spacious-padding.git][spacious-padding]]
;; Increase the padding/spacing of GNU Emacs frames and windows.

(use-package spacious-padding
  :custom
  (spacious-padding-widths '(
                             :internal-border-width 10
                             :header-line-width 0
                             :mode-line-width 4
                             :tab-bar-width 4
                             :tab-line-width 2
                             :tab-width 2
                             :right-divider-width 10
                             ;; :scroll-bar-width 2
                             :fringe-width 8
                             ))
  (spacious-padding-subtle-mode-line t)
  :hook elpaca-after-init)

;; tab-bar :build_in:

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-history-limit 100)
  (tab-bar-close-button-show nil)
  :config
  ;; Prevent accidental tab switches when scrolling the buffer
  (define-key tab-bar-map (kbd "<wheel-down>") nil t)
  (define-key tab-bar-map (kbd "<wheel-up>") nil t)
  (define-key my/leader-map (kbd "W") (cons "workspace" my/workspace-map))
  :bind
  (([remap winner-undo] . tab-bar-history-back)
   ([remap winner-redo] . tab-bar-history-forward)
   :map my/toggle-map
   ("t"                 . tab-bar-mode)
   :map my/leader-map
   ("<backtab>"         . tab-switcher)
   :repeat-map my/window-map
   ("u"                 . tab-bar-history-back)
   ("i"                 . tab-bar-history-forward)
   :repeat-map my/workspace-map
   ("N"                 . tab-bar-move-tab)
   ("P"                 . tab-bar-move-tab-backward)
   ("n"                 . tab-next)
   ("p"                 . tab-previous)
   :exit
   ("TAB"               . tab-switcher)
   ("k"                 . tab-close-group))
  :hook
  ((elpaca-after-init . tab-bar-history-mode)
   (elpaca-after-init . tab-bar-mode)))

;; [[https://github.com/MArpogaus/modern-tab-bars][modern-tab-bars]]
;; The look of both rows of tabs: the tab bar, one tab per tab group, and
;; the tab line, one tab per buffer of a window.  This used to be
;; =auto-tab-groups-eyecandy= and the =tab-line= block below it.

;; One declaration, because it is one package: elpaca queues an id once,
;; and a second =:ensure= of the same id is a duplicate it warns about.

(use-package modern-tab
  ;; The id is the main file elpaca looks for — `modern-tab.el' — and
  ;; the repository is named after what it holds.
  ;; `main' holds the start of the repository; the work is on `dev'.
  :ensure (modern-tab :host github :repo "MArpogaus/modern-tab-bars"
                      :branch "dev")
  :custom
  ;; The bar beside a tab group, as high as the mode line.
  (modern-tab-bar-indicator-height my/modeline-height)
  ;; Assign Icons to tab groups
  (modern-tab-bar-icons
   '(("HOME"       . (:style "suc" :icon "custom-emacs"))
     ("dirvish"    . (:style "suc" :icon "custom-folder_oct"))
     ("denote"     . (:style "md"  :icon "notebook_edit"))
     ("customize"  . (:style "cod" :icon "settings"))
     ("^\\[P\\] *" . (:style "oct" :icon "repo"))
     ("^\\[T\\] *" . (:style "cod" :icon "remote"))))
  ;; Remove prefix from project groups
  (modern-tab-bar-group-name-function
   (lambda (group-name)
     (if (string-match "^\\[.\\] *" group-name)
         (substring group-name (match-end 0))
       group-name)))
  ;; The new button makes a group, not a tab.
  (modern-tab-bar-new-command #'auto-tab-groups-new-group)
  ;; The tab line looked like this before the package existed: no bar
  ;; beside a tab.  A width turns one on for the selected tab.
  (modern-tab-line-active-indicator-width 0)
  (modern-tab-line-indicator-height my/tabline-height)
  :hook
  ((tab-line-mode . modern-tab-line-mode)
   (tab-bar-mode . modern-tab-bar-mode)))

;; tab-line :build_in:
;; Configure the build in =tab-line-mode= to display and switch between windows buffers via tabs.

;; The look and the behaviour of the tabs are [[https://github.com/MArpogaus/modern-tab-bars][modern-tab-bars]], configured
;; above: the icon per buffer, the close button that buries or kills, and
;; the row that hides itself where a window shows one buffer.  What stays
;; here is which buffers get a tab line at all.


(use-package tab-line
  :ensure nil
  :custom
  (tab-line-new-tab-choice nil)
  (tab-line-exclude-modes '(completion-list-mode
                            ediff-meta-mode ediff-mode symbols-outline-mode flymake-diagnostics-buffer-mode
                            dirvish-directory-view-mode dirvish-special-preview-mode
                            dape-info-scope-mode dape-info-stack-mode dape-info-watch-mode dape-info-parent-mode
                            dape-info-modules-mode dape-info-sources-mode dape-info-threads-mode dape-info-breakpoints-mode))
  :hook
  (elpaca-after-init . global-tab-line-mode))

;; time :build_in:

(use-package time
  :ensure nil
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  :preface
  (defun my/toggle-display-time-mode (&rest args)
    (ignore args)
    (display-time-mode 'toggle))
  :init
  (advice-add 'toggle-frame-fullscreen
              :after #'my/toggle-display-time-mode))

;; ZZ Library Footer

(provide 'my-ui)
;;; my-ui.el ends here
