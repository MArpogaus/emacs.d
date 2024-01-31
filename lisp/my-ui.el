;;; my-ui.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-01-31
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/emacs-straight/ascii-art-to-unicode.git][ascii-art-to-unicode]]
;; Make org-brain-visualize-mode look a bit nicer.

(use-package ascii-art-to-unicode
  :after org-brain
  :preface
  (defface aa2u-face '((t . nil))
    "Face for aa2u box drawing characters")
  (defun aa2u-org-brain-buffer ()
    (let ((inhibit-read-only t))
      (make-local-variable 'face-remapping-alist)
      (add-to-list 'face-remapping-alist
                   '(aa2u-face . org-brain-wires))
      (ignore-errors (aa2u (point-min) (point-max)))))
  :config
  (advice-add #'aa2u-1c :filter-return
              (lambda (str) (propertize str 'face 'aa2u-face)))
  :hook
  (org-brain-after-visualize . aa2u-org-brain-buffer))

;; [[https://github.com/LionyxML/auto-dark-emacs.git][auto-dark]]
;; Auto-Dark-Emacs is an auto changer between 2 themes, dark/light, following MacOS, Linux or Windows Dark Mode settings.

(use-package auto-dark
  :custom
  (auto-dark-dark-theme 'modus-vivendi)
  (auto-dark-light-theme 'modus-operandi)
  :hook (after-init . auto-dark-mode))

;; [[https://github.com/emacs-dashboard/emacs-dashboard.git][dashboard]]
;; An extensible emacs dashboard.

(use-package dashboard
  :custom
  ;; Content is not centered by default. To center, set
  (dashboard-center-content t)

  ;; display an alternative emacs logo
  (dashboard-startup-banner 'logo)

  ;; To disable shortcut "jump" indicators for each section, set
  (dashboard-show-shortcuts nil)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((bookmarks . 20)
                     (recents  . 10)
                     (projects . 10)))
  (dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  (dashboard-display-icons-p (display-graphic-p))
  (dashboard-set-heading-icons (display-graphic-p))
  (dashboard-set-file-icons (display-graphic-p))
  (dashboard-set-navigator t) ;; show navigator below the banner:
  (dashboard-set-footer nil) ;; disable footer
  :hook
  (after-init . dashboard-setup-startup-hook))

;; display-line-numbers :build_in:
;; Enable line numbers for some modes

(use-package display-line-numbers
  :straight nil
  :hook
  (((prog-mode conf-mode text-mode) . display-line-numbers-mode)
   ;; disable for org mode
   (org-mode . (lambda () (display-line-numbers-mode 0)))))

;; [[https://github.com/seagle0128/doom-modeline.git][doom-modeline]]
;; A fancy and fast mode-line inspired by minimalism design.

(use-package doom-modeline
  :custom
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (doom-modeline-height 20)

  ;; display the real names, please put this into your init file.
  (find-file-visit-truename t)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (doom-modeline-hud t)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (doom-modeline-buffer-file-name-style 'relative-to-project)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (doom-modeline-icon (display-graphic-p))

  ;; If non-nil, only display one number for checker information if applicable.
  (doom-modeline-checker-simple-format t)

  :hook
  (after-init . doom-modeline-mode))

;; hl-line :build_in:

;; Highlighting of the current line (native mode)


(use-package hl-line
  :straight nil
  :hook
  ((prog-mode org-mode) . global-hl-line-mode))

;; [[https://github.com/jdtsmith/indent-bars.git][indent-bars]]
;; Fast, configurable indentation guide-bars for Emacs.

(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))

  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-display-on-blank-lines nil)
  :hook
  ((python-base-mode yaml-ts-mode emacs-lisp-mode) . indent-bars-mode))

;; [[https://github.com/mickeynp/ligature.el.git][ligature]]
;; Display typographical ligatures in Emacs.

(use-package ligature
  :config
  ;; set Fira as default font
  (set-frame-font "FiraCode Nerd Font-10" nil t)
  :preface
  (defun my/setup-ligatures ()
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures '(eww-mode org-mode) '("ff" "fi" "ffi"))
    ;; Enable all Cascadia and Fira Code ligatures in programming modes
    (ligature-set-ligatures '(prog-mode org-mode)
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
                              ;; *> */ *)  ** *** ****
                              ("*" (rx (or ">" "/" ")" (+ "*"))))
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
                              ;; Fira code:
                              "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                              ;; The few not covered by the regexps.
                              "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode))
  :hook
  (after-init . my/setup-ligatures))

;; [[https://git.sr.ht/~protesilaos/modus-themes][modus-themes]]
;; Accessible themes for GNU Emacs, conforming with the highest standard for colour contrast between background and foreground values (WCAG AAA)
;; https://protesilaos.com/emacs/modus-themes


(use-package modus-themes
  :bind
  (:map my/toggle-map
        ("t" . modus-themes-toggle))
  :custom
  ;; Add all your customizations prior to loading the themes
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil))

;; [[https://github.com/rainstormstudio/nerd-icons.el.git][nerd-icons]]
;; A Library for Nerd Font icons. Required for modline icons.

(use-package nerd-icons)

;; [[https://github.com/haji-ali/procress.git][procress]]
;; display LaTeX compilation information in the mode line

(use-package procress
  :straight (:host github :repo "haji-ali/procress")
  :after doom-modeline
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
                             :header-line-width 1
                             :mode-line-width 4
                             :tab-width 4
                             :right-divider-width 10
                             :scroll-bar-width 4))
  :hook
  (after-init . spacious-padding-mode))

;; tab-bar :build_in:

(use-package tab-bar
  :straight nil
  :bind
  (:repeat-map my/workspace-map
               ("p" . tab-previous)
               ("n" . tab-next)
               ("P" . tab-bar-move-tab-backward)
               ("N". tab-bar-move-tab)
               :exit
               ("k" . tab-close-group))
  :custom
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs-groups
                    my/tab-bar-format-new
                    tab-bar-format-align-right
                    tab-bar-format-global
                    tab-bar-format-menu-bar))
  (tab-bar-separator "")
  (tab-bar-auto-width nil)
  (tab-bar-close-button-show t)
  (tab-bar-new-tab-choice "*dashboard*")
  :preface
  (defun my/tab-bar-format-new ()
    "Button to add a new tab."
    `((add-tab menu-item ,tab-bar-new-button project-switch-project
               :help "New")))

  (defun my/tab-bar-tab-group-format-function (tab i &optional current-p)
    (let*((tab-group-name (funcall tab-bar-tab-group-function tab))
          (tab-group-face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive))
          (color (face-attribute (if current-p
                                     'mode-line-emphasis
                                   'tab-bar-tab-group-inactive) :foreground))
          (group-sep (propertize " " 'face (list :height (if current-p 0.4 0.2)
                                                 :foreground color
                                                 :background color)))
          (group-icon (cond
                       ((equal tab-group-name "HOME") "")
                       (t ""))))
      (concat
       group-sep
       (propertize
        (concat
         " "
         group-icon
         " "
         (funcall tab-bar-tab-group-function tab)
         " ")
        'face tab-group-face))))

  (defun my/tab-bar-tab-name-format-function (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if current-p "⏵ " " ")
               (if tab-bar-tab-hints (format "%d " i) "")
               (alist-get 'name tab)
               (if (and tab-bar-close-button-show current-p)
                   tab-bar-close-button " "))
       'face (list :inherit 'tab-bar-tab :weight (if current-p 'bold 'normal)))))

  (defun my/create-home-tab-group (&optional frame)
    (let ((tab-group-name (funcall tab-bar-tab-group-function (tab-bar--current-tab))))
      (when frame (select-frame frame))
      (tab-group (if tab-group-name tab-group-name "HOME"))))

  :config
  (require 'icons)
  (define-icon tab-bar-new nil
    '(
      ;; (emoji "➕")
      (symbol "  " :face tab-bar-tab-inactive)
      (text " + "))
    "Icon for creating a new tab."
    :version "29.1"
    :help-echo "New tab")
  (define-icon tab-bar-close nil
    '(
      ;; (emoji " ❌")
      (symbol " ✕ ") ;; "ⓧ"
      (text " x "))
    "Icon for closing the clicked tab."
    :version "29.1"
    :help-echo "Click to close tab")
  (define-icon tab-bar-menu-bar nil
    '(;; (emoji "🍔")
      (symbol " 󰍜 " :face tab-bar-tab-inactive)
      (text "Menu" :face tab-bar-tab-inactive))
    "Icon for the menu bar."
    :version "29.1"
    :help-echo "Menu bar")

  (setq tab-bar-tab-group-format-function #'my/tab-bar-tab-group-format-function
        tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format-function)

  (add-hook 'after-make-frame-functions 'my/create-home-tab-group)
  (my/create-home-tab-group)
  :hook
  (after-init . tab-bar-mode))

;; tab-line :build_in:

(use-package tab-line
  :straight nil
  :custom
  (tab-line-new-tab-choice . nil)
  (tab-line-new-button-show . nil)
  (tab-line-close-button-show . nil)
  :bind
  (:map my/toggle-map
        ("T" . global-tab-line-mode))
  :preface
  (defun my/tab-line-tab-name-function (buffer &optional _buffers)
    (let ((name (buffer-name buffer)))
      (concat " "
              (nerd-icons-icon-for-file name)
              (format " %s " name))))
  (defun my/switch-to-prev-buffer-skip (window buffer &REST)
    (not (seq-contains-p
          (tab-line-tabs-window-buffers) buffer)))
  :config
  (setq tab-line-tab-name-function #'my/tab-line-tab-name-function
        tab-line-separator "")
  (customize-set-value 'switch-to-prev-buffer-skip #'my/switch-to-prev-buffer-skip)
  :hook
  (after-init . global-tab-line-mode))

;; [[https://codeberg.org/joostkremers/visual-fill-column.git][visual-fill-column]]


(use-package visual-fill-column
  :bind (:map my/toggle-map ("w" . visual-fill-column-mode)))

;; Library Footer

(provide 'my-ui)
;;; my-ui.el ends here
