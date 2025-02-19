;;; my-ui.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-02-19
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
  :hook
  (elpaca-after-init . auto-dark-mode))

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
  :preface
  (defun my/patch-doom-themes (&rest args)
    (ignore args)
    (set-face-foreground 'tab-bar (face-foreground 'tab-bar-tab)))
  :init
  (advice-add 'load-theme :after #'my/patch-doom-themes))

;; [[https://github.com/hlissner/emacs-hide-mode-line.git][hide-mode-line]]
;; An Emacs plugin that hides (or masks) the current buffer's mode-line.

(use-package hide-mode-line
  :hook
  (symbols-outline-mode . hide-mode-line-mode))

;; hl-line :build_in:

;; Highlighting of the current line (native mode)


(use-package hl-line
  :ensure nil
  :hook
  ((prog-mode org-mode) . global-hl-line-mode))

;; [[https://github.com/tarsius/hl-todo.git][hl-todo]]
;; Highlight TODO keywords.

(use-package hl-todo
  :preface
  (defun my/hl-todo-register-flymake-report-fn ()
    (add-hook #'flymake-diagnostic-functions #'hl-todo-flymake))
  :hook
  (((prog-mode conf-mode LaTeX-mode) . hl-todo-mode)
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
;; Display typographical ligatures in Emacs.

(use-package ligature
  :if (display-graphic-p)
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
  (elpaca-after-init . my/setup-ligatures))

;; [[https://gitlab.com/jessieh/mood-line.git][mood-line]]

(use-package mood-line
  :config
  (setq my/modeline-height 30)
  :custom
  ;; Use pretty Fira Code-compatible glyphs
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  (mood-line-format
   (mood-line-defformat
    :left
    (((my/get-bar-image my/modeline-height 2 nil) . " ")
     ((mood-line-segment-modal)                   . " ")
     ((mood-line-segment-anzu)                    . " ")
     ((mood-line-segment-multiple-cursors)        . " ")
     )
    :right
    (((mood-line-segment-process)                 . " ")
     ((mood-line-segment-buffer-status)           . " ")
     ;; ((mood-line-segment-misc-info)               . " ")))
     ((mood-line-segment-major-mode)              . " ")
     ((mood-line-segment-vc)                      . " ")
     ((mood-line-segment-checker)                 . " "))))
  (mood-line-segment-modal-meow-state-alist
   `((normal ,(nerd-icons-mdicon "nf-md-alpha_m_circle") . font-lock-variable-name-face)
     (insert ,(nerd-icons-mdicon "nf-md-alpha_i_circle") . font-lock-string-face)
     (keypad ,(nerd-icons-mdicon "nf-md-alpha_k_circle") . font-lock-keyword-face)
     (beacon ,(nerd-icons-mdicon "nf-md-alpha_b_circle") . font-lock-type-face)
     (motion ,(nerd-icons-mdicon "nf-md-alpha_m_circle") . font-lock-constant-face)))
  :hook
  (elpaca-after-init . mood-line-mode))

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
  :hook
  (elpaca-after-init . spacious-padding-mode))

;; tab-bar :build_in:

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-history-limit 100)
  :preface
  (defvar my/workspace-map (make-sparse-keymap) "key-map for workspace commands")
  :config
  ;; Prevent accidental tab switches when scrolling the buffer
  (define-key tab-bar-map (kbd "<wheel-down>") nil t)
  (define-key tab-bar-map (kbd "<wheel-up>") nil t)
  :config
  (define-key my/leader-map (kbd "W") (cons "workspace" my/workspace-map))
  :bind
  (([remap winner-undo] . tab-bar-history-back)
   ([remap winner-undo] . tab-bar-history-forward)
   :map my/toggle-map
   ("t" . tab-bar-mode)
   :repeat-map my/window-map
   ("u" . tab-bar-history-back)
   ("i" . tab-bar-history-forward)
   :repeat-map my/workspace-map
   ("p" . tab-previous)
   ("n" . tab-next)
   ("P" . tab-bar-move-tab-backward)
   ("N". tab-bar-move-tab)
   :exit
   ("k" . tab-close-group))
  :hook
  ((elpaca-after-init . tab-bar-history-mode)
   (elpaca-after-init . tab-bar-mode)))

;; tab-line :build_in:
;; Configure the build in =tab-line-mode= to display and switch between windows buffers via tabs.

;; Some customizations are made to prettify the look of tabs using =nerd-icons= and make the close button behave as known from other editors.

;; References:
;; - https://github.com/benleis1/emacs-init/blob/main/tab-config.md#tab2-close-tab
;; - https://andreyor.st/posts/2020-05-07-making-emacs-tabs-work-like-in-atom/


(use-package tab-line
  :ensure nil
  :custom
  (tab-line-new-tab-choice nil)
  (tab-line-new-button-show nil)
  (tab-line-tab-name-function #'my/tab-line-tab-name-function)
  (tab-line-close-tab-function #'my/tab-line-close-tab-function)
  (tab-line-exclude-modes '(completion-list-mode
                            imenu-list-major-mode ediff-meta-mode ediff-mode symbols-outline-mode flymake-diagnostics-buffer-mode
                            dired-mode dirvish-directory-view-mode
                            dape-info-scope-mode dape-info-stack-mode dape-info-watch-mode dape-info-parent-mode
                            dape-info-modules-mode dape-info-sources-mode dape-info-threads-mode dape-info-breakpoints-mode))
  (tab-line-close-button-show 'selected)
  :bind
  (:map my/toggle-map
        ("T" . global-tab-line-mode))
  :preface
  (defun my/tab-line-tab-name-function (buffer &optional _buffers)
    (let ((name (buffer-name buffer)))
      (concat ;;(my/get-bar-image 20 2 nil)
       " "
       (nerd-icons-icon-for-file name)
       (format " %s " name))))

  (defun my/tab-line-get-buffer (tab)
    "Return the buffer represented by TAB."
    (if (bufferp tab) tab (cdr (assq 'buffer tab))))

  (defun my/tab-line-windows-with-buffer (buffer)
    "Return a list of windows displaying BUFFER across all frames."
    (seq-filter (lambda (window)
                  (eq buffer (window-buffer window)))
                (window-list-1 nil nil t)))

  (defun my/tab-line-close-or-bury-buffer (buffer)
    "Close or bury BUFFER based on its presence in other windows."
    (let ((other-windows (my/tab-line-windows-with-buffer buffer)))
      (if (> (length other-windows) 1)
          (progn
            (message "Burying buffer %s" buffer)
            (bury-buffer))
        (progn
          (message "Closing buffer %s" buffer)
          (kill-buffer buffer)))))

  (defun my/multi-buffer-window-p ()
    "Evaluates to `t' if windows has mutible tab-line buffers, else `nil'."
    (> (length (tab-line-tabs-window-buffers)) 1))

  (defun my/tab-line-close-tab-function (tab)
    "Close the selected tab.
      If the tab is presented in another window, close the tab by using the `bury-buffer` function.
      If the tab is unique to all existing windows, kill the buffer with the `kill-buffer` function.
      Lastly, if no tabs are left in the window, it is deleted with the `delete-window` function."
    (interactive (list (current-buffer)))
    (let ((window (selected-window))
          (kill-window-p (not (my/multi-buffer-window-p)))
          (buffer (my/tab-line-get-buffer tab)))
      (my/tab-line-close-or-bury-buffer buffer)
      (when kill-window-p 
        (message "Closing window")
        (ignore-errors (delete-window window)))))

  ;; (defun my/enable-tab-line-if-multiple-buffers ()
  ;;   "Enable tab line mode if there are multiple buffers in the current window."
  ;;   (if (my/multi-buffer-window-p)
  ;;       (tab-line-mode 1)
  ;;     (tab-line-mode -1)))
  :config
  (setq tab-line-close-button
        (propertize "✕ "
                    'keymap tab-line-tab-close-map
                    'mouse-face 'tab-line-close-highlight
                    'help-echo "Click to close tab")
        tab-line-separator "")
  :hook
  ;; (window-configuration-change . my/enable-tab-line-if-multiple-buffers)
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

;; Library Footer

(provide 'my-ui)
;;; my-ui.el ends here
