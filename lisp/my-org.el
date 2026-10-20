;;; my-org.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file.  DO NOT EDIT.

;;; Code:

;; ob :build_in:

(use-package ob
  :ensure nil
  :after org
  :demand t
  :custom
  (org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (plantuml . t)
     (python . t)
     (shell . t)))
  :preface
  (defun my/org-babel-execute-and-goto-next-src nil
    "Execute the current block or babel call, then go to the next src block."
    (interactive)
    (let ((org-confirm-babel-evaluate nil))
      (when (org-babel-execute-maybe)
        (org-babel-next-src-block))))

  (defun my/org-metareturn-src-block ()
    "In a src block, execute it and go to the next one."
    (when (and (not current-prefix-arg) (org-in-src-block-p))
      (my/org-babel-execute-and-goto-next-src)
      t))
  :bind
  (:map org-mode-map
        ("M-_" . org-babel-demarcate-block))
  :hook
  (org-metareturn . my/org-metareturn-src-block))

;; [[https://git.savannah.gnu.org/git/emacs/org-mode.git][org]]
;; Agenda view and task management follow https://github.com/rougier/emacs-gtd


(use-package org
  :preface
  (defface my/org-todo-onhold-face '((t :inherit (bold warning org-todo)))
    "Face for todo keywords of paused or blocked tasks.")
  (defface my/org-todo-project-face '((t :inherit (bold font-lock-doc-face org-todo)))
    "Face for the PROJ todo keyword.")
  (defun my/org-capture-inbox ()
    "Capture a task into the inbox."
    (interactive)
    (org-capture nil "i"))
  (defun my/org-capture-note ()
    "Capture a new Denote note."
    (interactive)
    (org-capture nil "n"))
  :init
  (setopt org-directory (expand-file-name "Notes/" (getenv "HOME"))
          org-cite-global-bibliography (list (expand-file-name "denote/bib/references.bib" org-directory)))
  :custom
  (org-ellipsis "…")
  (org-fontify-quote-and-verse-blocks t)
  ;; Return or left-click with mouse follows link
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)

  ;; Todo
  (org-todo-keywords
   '((sequence
      "PROJ(p)"   ; A project, which contains other tasks
      "TODO(t)"   ; A task that needs doing
      "NEXT(n!)"  ; The next task to work on; the switch is logged
      "WAIT(w)"   ; Something holds the task up
      "|"
      "DONE(d)"
      "KILL(k)"))) ; Cancelled or no longer applicable
  (org-todo-keyword-faces
   '(("WAIT" . my/org-todo-onhold-face)
     ("PROJ" . my/org-todo-project-face)))

  ;; Add timestamp to items when done
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; org capture
  (org-capture-templates
   '(("i" "Inbox" entry (file "agenda/inbox.org")
      "* TODO %?\n")
     ("n" "New note (with Denote)" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)))

  ;; org-agenda
  ;; A directory, so Org also finds files created after startup
  (org-agenda-files (list (expand-file-name "agenda/" org-directory)))
  ;; auto-side-windows places the agenda; quit only kills the buffer and
  ;; deletes no window, which fails beside side windows.
  (org-agenda-window-setup 'current-window)

  ;; Refile and Archive
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  ;; The Tasks heading of each project, and the Work and Private headings
  ;; of Miscellaneous and of the reading list
  (org-refile-targets
   '((org-agenda-files :tag . "tasks")
     (org-agenda-files :regexp . "^\\*+ \\(?:Work\\|Private\\)\\(?: \\[[0-9/%]*\\]\\)?\\(?:[ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$")))
  ;; A project without a NEXT task is stuck.
  (org-stuck-projects '("/PROJ" ("NEXT") nil ""))
  (org-agenda-custom-commands
   '(("g" "Get Things Done (GTD)"
      ((agenda "" ((org-agenda-overriding-header "󱨰   Heute")
                   (org-agenda-entry-types '(:timestamp :sexp))
                   (org-agenda-sorting-strategy '(time-up priority-down))
                   (org-agenda-prefix-format "  %?-12t% s")))
       (agenda "" ((org-agenda-overriding-header "\n󰗎   Deadlines")
                   (org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 14)
                   (org-agenda-time-grid nil)
                   (org-agenda-format-date "")))
       (agenda "" ((org-agenda-overriding-header "\n󱛡   Geplant")
                   (org-agenda-entry-types '(:scheduled))
                   (org-agenda-time-grid nil)
                   (org-agenda-format-date "")))
       (tags-todo "inbox" ((org-agenda-overriding-header "\n󱉰   Inbox")))
       (todo "NEXT" ((org-agenda-overriding-header "\n󰳠   Next")
                     (org-agenda-todo-ignore-deadlines 'near)
                     (org-agenda-todo-ignore-scheduled 'all)))
       (todo "WAIT" ((org-agenda-overriding-header "\n󰚭   Waiting"))))
      ((org-agenda-span 'day)
       (org-agenda-tag-filter-preset '("-literature"))
       (org-agenda-sorting-strategy '(priority-down category-keep effort-up))
       (org-agenda-prefix-format "  %i %s")))
     ("l" "Literature" tags-todo "literature"
      ((org-agenda-sorting-strategy '(priority-down category-keep effort-up))
       (org-agenda-prefix-format "  %i %-12:c ")))))

  ;; Use SVGs for latex previews -> No blur when scaling
  (org-preview-latex-default-process 'dvisvgm)
  :hook
  (org-mode . reveal-mode)
  :bind
  (:map my/leader-map
        ("i" . my/org-capture-inbox)
        ("I" . my/org-capture-note)
        :map my/open-map
        ("a" . org-agenda)
        :map org-mode-map
        ("C-c P" . org-present)))

;; org-agenda :build_in:
;; Keys for the GTD work in the agenda; =g= reloads, =t= sets the keyword, =q= in the GTD tab group also ends the agent and closes the group.

(use-package org-agenda
  :ensure nil
  :preface
  (defun my/org-agenda-forget-killed-file ()
    "Rebuild the agenda in view once a buffer of one of its files is killed.
Its entries point into that buffer: a click on one would fail."
    (when-let* (((and (derived-mode-p 'org-mode) (org-agenda-file-p)))
                ((boundp 'org-agenda-buffer-name))
                (window (get-buffer-window org-agenda-buffer-name t)))
      (run-at-time 0 nil (lambda ()
                           (when (window-live-p window)
                             (with-selected-window window (org-agenda-redo-all)))))))
  :hook
  (kill-buffer . my/org-agenda-forget-killed-file)
  :bind
  (:map org-agenda-mode-map
        ("r" . org-agenda-refile)
        ("s" . org-agenda-schedule)
        ("d" . org-agenda-deadline)
        ("c" . my/org-capture-inbox)
        ("q" . my/gtd-quit))
  :config
  ;; Block headers large and in variable pitch.  An override spec, because
  ;; the theme sets this face and would win over a defface spec.
  (face-spec-set 'org-agenda-structure
                 '((t :inherit variable-pitch :height 1.2 :weight bold))
                 'face-override-spec))

;; [[https://github.com/awth13/org-appear.git][org-appear]]
;; Toggle visibility of hidden Org mode element parts upon entering and leaving an element.


(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; [[https://github.com/yilkalargaw/org-auto-tangle.git][org-auto-tangle]]
;; Tangle Org files on save.

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; [[https://github.com/rexim/org-cliplink.git][org-cliplink]]
;; Insert an Org link to the URL in the clipboard, with the page title as description.

(use-package org-cliplink)

;; [[https://github.com/tarsius/orglink.git][orglink]]
;; Use Org Mode links in other modes.

(use-package orglink
  :hook
  (prog-mode . orglink-mode))

;; [[https://github.com/minad/org-modern.git][org-modern]]
;; Give Org headlines, keywords, tables and source blocks a modern style.

(use-package org-modern
  :custom-face
  ;; set basic title font
  (org-level-8 ((t (:weight bold :inherit variable-pitch))))
  ;; Low levels are unimportant => no scaling
  (org-level-7 ((t (:inherit org-level-8))))
  (org-level-6 ((t (:inherit org-level-8))))
  (org-level-5 ((t (:inherit org-level-8))))
  (org-level-4 ((t (:inherit org-level-8))))
  ;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
  (org-level-3 ((t (:inherit org-level-8 :height 1.2)))) ;\large))
  (org-level-2 ((t (:inherit org-level-8 :height 1.44)))) ;\Large))
  (org-level-1 ((t (:inherit org-level-8 :height 1.728)))) ;\LARGE))
  ;; Document Title, (\huge)
  (org-document-title ((t (:height 2.074 :foreground unspecified :inherit org-level-8))))
  ;; Ensure org modern labels use fixed pitch font
  (org-modern-label ((t (:height 1.0 :inherit fixed-pitch))))
  (org-modern-todo ((t (:inherit (org-modern-label org-todo org-level-8)))))
  (org-modern-priority ((t (:inherit (org-priority org-modern-label org-level-8)))))
  (org-modern-tag ((t (:inherit (secondary-selection org-modern-label org-level-8)))))
  ;; Fix indentation when variable pitch fonts are used
  (org-hide ((t (:inherit fixed-pitch))))
  :custom
  (org-cycle-level-faces nil)
  (org-n-level-faces 4)
  ;; org-modern draws todo keywords itself and ignores
  ;; `org-todo-keyword-faces'; mirror them here, keeping the label look.
  (org-modern-todo-faces
   '(("WAIT" :inherit (my/org-todo-onhold-face org-modern-todo))
     ("PROJ" :inherit (my/org-todo-project-face org-modern-todo))))
  (org-modern-fold-stars '(("⯈" . "⯆") ("▸" . "▾")))
  (org-modern-block-name
   '((t . t)
     ("src" "" "")
     ("example" "»" "«")
     ("quote" "❝" "❞")
     ("export" "" "")))

  (org-modern-keyword
   '((t . t)
     ;; Metadata
     ("title" . "𝙏")
     ("subtitle" . "𝙩")
     ("author" . "𝘼")
     ("email" . "@")
     ("date" . "𝘿")
     ("description" . "󰋽")
     ("keywords" . "󱋷")

     ;; Configuration & Settings
     ("options" . "")
     ("property" . "")
     ("columns" . "")
     ("language" . "")
     ("filetags" . "󱋷")
     ("tags" . "󰓹")
     ("setupfile" . "󰒓")
     ("include" . "󰌹")
     ("bind" . "󰌷")
     ("startup" . "⏻")
     ("identifier" . "󰰅")
     ("exclude_tags" . "󰤐")
     ("auto_tangle" . "")

     ;; Macros & Bibliography
     ("macro" . "𝑴")
     ("bibliography" . "")
     ("print_bibliography" . "󰌱")
     ("cite_export" . "󱚃")

     ;; LaTeX (using superscript modifiers for clean differentiation)
     ("latex" . "𝑳")
     ("latex_class" . "𝑳ᶜ")
     ("latex_class_options" . "𝑳ᵒ")
     ("latex_header" . "𝑳ʰ")
     ("latex_header_extra" . "𝑳⁺")
     ("latex_compiler" . "𝑳")

     ;; Beamer (superscripts)
     ("beamer" . "𝑩")
     ("beamer_header" . "𝑩ʰ")
     ("beamer_theme" . "𝑩ᵗ")
     ("beamer_color_theme" . "𝑩ᶜ")
     ("beamer_font_theme" . "𝑩ᶠ")

     ;; HTML
     ("html_head" . "𝑯ʰ")
     ("html_head_extra" . "𝑯⁺")

     ;; Attributes
     ("attr_html" . "𝑯ᵃ")
     ("attr_latex" . "𝑳ᵃ")
     ("attr_beamer" . "𝑩ᵃ")

     ;; Elements & Code Blocks
     ("name" . "𝙉")
     ("header-args" . "")
     ("caption" . "☰")
     ("results" . "󱞩")
     ("call" . "󰆍")
     ("toc" . "")))

  ;; Edit settings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)

  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-remove-tags t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-deadline-leaders '("Heute      " "in %2d T.   " "%2d T. über "))
  (org-agenda-scheduled-leaders '("Heute      " "seit %2d T. "))
  (org-agenda-category-icon-alist
   `(("TIDIT" (,(nerd-icons-mdicon "nf-md-briefcase_outline")))
     ("EMACS" (,(nerd-icons-sucicon "nf-custom-emacs")))
     ("CHORA" (,(nerd-icons-mdicon "nf-md-home_outline")))
     ("VIBE CODING" (,(nerd-icons-mdicon "nf-md-robot_outline")))
     (".*" (,(nerd-icons-mdicon "nf-md-checkbox_blank_circle_outline")))))
  (org-agenda-block-separator nil)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── jetzt")
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;; [[https://github.com/jdtsmith/org-modern-indent.git][org-modern-indent]]
;; Modern block styling with org-indent.

(use-package org-modern-indent
  :ensure (:host github :repo "jdtsmith/org-modern-indent")
  :hook
  (org-indent-mode . org-modern-indent-mode))

;; [[https://github.com/org-noter/org-noter.git][org-noter]]
;; Emacs document annotator, using Org-mode.

(use-package org-noter
  :custom
  ;; The WM can handle splits
  ;; org-noter-notes-window-location 'other-frame
  ;; Please stop opening frames
  (org-noter-always-create-frame nil)
  ;; I want to see the whole file
  (org-noter-hide-other nil)
  ;; Everything is relative to the main notes file
  ;; org-noter-notes-search-path (list bibtex-completion-notes-path)
  (org-noter-highlight-selected-text t)
  :hook
  ;; Give each note heading an ID
  (org-noter-insert-heading . org-id-get-create))

;; [[https://github.com/rlister/org-present.git][org-present]]
;; Ultra-minimalist presentation minor-mode for Emacs org-mode.
;; Inspired by: https://systemcrafters.net/emacs-tips/presentations-with-org-present/

(use-package org-present
  :preface
  (defvar-local my/org-present--remaps nil
    "The face remaps of the presentation in this buffer.")
  (defun my/org-present-start ()
    (org-present-read-only)
    (org-link-preview-region)

    ;; Hide Property drawers
    (org-tidy-mode 1)

    ;; Tweak font sizes; added to the buffer's other remaps, not in place of them
    (setq-local my/org-present--remaps
                (mapcar (lambda (remap) (apply #'face-remap-add-relative remap))
                        '((header-line (:height 4.0) variable-pitch)
                          (org-document-title (:inherit variable-pitch :height 2.0))
                          (org-level-1 (:inherit variable-pitch :height 1.5))
                          (org-level-2 (:inherit variable-pitch :height 1.3))
                          (org-level-3 (:inherit variable-pitch :height 1.2))
                          (org-level-4 (:inherit variable-pitch :height 1.1))
                          (org-level-5 (:inherit variable-pitch :height 1.1))
                          (org-level-6 (:inherit variable-pitch :height 1.1))
                          (org-level-7 (:inherit variable-pitch :height 1.1))
                          (org-level-8 (:inherit variable-pitch :height 1.1))
                          (org-default (:inherit variable-pitch))
                          (org-table (:inherit fixed-pitch))
                          (org-code (:inherit fixed-pitch))
                          (org-verbatim (:inherit fixed-pitch)))))

    ;; Set a blank header line string to create blank space at the top
    (setq-local header-line-format " ")

    ;; Configure fill width
    (setq-local visual-fill-column-width 160
                visual-fill-column-center-text t)

    ;; Center the presentation and wrap lines
    (visual-fill-column-mode 1)

    ;; Switch to minimal ui mode
    (my/minimal-ui-mode 1))
  (defun my/org-present-quit ()
    (org-link-preview-clear)

    ;; Show Property drawers
    (org-tidy-mode 0)

    ;; Reset font customizations
    (mapc #'face-remap-remove-relative my/org-present--remaps)
    (kill-local-variable 'my/org-present--remaps)

    ;; `org-present-read-only' hid the cursor
    (org-present-show-cursor)

    ;; Clear the header line string so that it isn't displayed
    (kill-local-variable 'header-line-format)

    ;; Configure fill width
    (kill-local-variable 'visual-fill-column-width)
    (kill-local-variable 'visual-fill-column-center-text)

    ;; Stop centering the presentation and wrap lines
    (visual-fill-column-mode 0)

    ;; Disable minimal ui mode
    (my/minimal-ui-mode -1))
  (defun my/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)

    ;; Unfold the current entry
    (org-fold-show-entry)

    ;; Show only direct subheadings of the slide but don't expand them
    (org-fold-show-children))
  :bind
  (:map org-present-mode-keymap
        ("q"         . org-present-quit)
        ("C-<left>"  . org-present-prev)
        ("C-<right>" . org-present-next))
  :config
  (define-key org-present-mode-keymap (kbd "<left>") nil t)
  (define-key org-present-mode-keymap (kbd "<right>") nil t)
  (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)
  :hook
  ((org-present-mode . my/org-present-start)
   (org-present-mode-quit . my/org-present-quit)))

;; [[https://github.com/karthink/org-preview.git][org-preview]]

(use-package org-preview
  :ensure (:host github :repo "karthink/org-preview")
  :hook
  (org-mode . org-preview-mode))

;; [[https://github.com/jxq0/org-tidy.git][org-tidy]]
;; An Emacs minor mode to automatically tidy org-mode property drawers.

(use-package org-tidy)

;; ox :build_in:

(use-package ox
  :ensure nil
  :custom
  (org-export-backends '(md beamer odt latex icalendar html ascii)))

;; ox-beamer :build_in:

(use-package ox-beamer
  :ensure nil
  :config
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
  (add-to-list 'org-beamer-environments-extra
               '("overprint" "P" "\\begin{overprint}%a" "\\end{overprint}")))

;; [[https://github.com/larstvei/ox-gfm.git][ox-gfm]]
;; Github Flavored Markdown Back-End for Org Export Engine.

(use-package ox-gfm
  :after ox
  :demand t)

;; ox-latex :build_in:

(use-package ox-latex
  :ensure nil
  :config
  ;; https://orgmode.org/manual/LaTeX-specific-export-settings.html
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "xcolor"))
  (add-to-list 'org-latex-classes
               '("koma-article"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("koma-letter"
                 "\\documentclass{scrlttr2}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; [[https://github.com/emacsorphanage/ox-pandoc.git][ox-pandoc]]
;; Export Org files with pandoc.

(use-package ox-pandoc
  :if (executable-find "pandoc")
  :after ox
  :demand t
  :custom
  (org-pandoc-options
   '((standalone . t)
     (mathjax . t)
     (variable . "revealjs-url=https://revealjs.com"))))

;; [[https://github.com/snosov1/toc-org.git][toc-org]]
;; Keep an up-to-date table of contents in Org files without export, for READMEs on GitHub.


(use-package toc-org
  :hook
  (org-mode . toc-org-enable))

;; ZZ Library Footer

(provide 'my-org)
;;; my-org.el ends here
