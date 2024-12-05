;;; my-org.el --- Emacs configuration file  -*- no-byte-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-12-05
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://git.savannah.gnu.org/git/emacs/org-mode.git][org]]
;; Agenda view and task management has been inspired by https://github.com/rougier/emacs-gtd


(use-package org
  :init
  (setq org-directory (expand-file-name "Notes/org/" (getenv "HOME"))
        org-cite-global-bibliography (file-expand-wildcards (expand-file-name "bib/*.bib" org-directory)))
  :custom
  (org-ellipsis "…")
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 2)
  (org-hide-block-startup nil)
  (org-src-preserve-indentation nil)
  ;; Return or left-click with mouse follows link
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  ;; Display links as the description provided
  (org-link-descriptive t)

  ;; Todo
  (org-todo-keywords
   '((sequence
      "PROJ(p)"  ; A project, which usually contains other tasks
      "TODO(t)"  ; A task that needs doing & is ready to do
      "NEXT(n)"  ; Next task in a project
      "STRT(s)"  ; A task that is in progress
      "WAIT(w)"  ; Something external is holding up this task
      "HOLD(h)"  ; This task is paused/on hold because of me
      "|"
      "DONE(d)"  ; Task successfully completed
      "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
     (sequence
      "[ ](T)"   ; A task that needs doing
      "[-](S)"   ; Task is in progress
      "[?](W)"   ; Task is being held up or paused
      "|"
      "[X](D)"))) ; Task was completed
  (org-todo-keyword-faces
   '(("[-]"  . +org-todo-active)
     ("STRT" . +org-todo-active)
     ("[?]"  . +org-todo-onhold)
     ("WAIT" . +org-todo-onhold)
     ("HOLD" . +org-todo-onhold)
     ("PROJ" . +org-todo-project)))

  ;; Add timstamp to items when done
  (org-log-done 'time)

  ;; org capture
  (org-capture-templates
   `(("i" "Inbox" entry  (file "agenda/inbox.org")
      ,(concat "* TODO %?\n"
               "/Entered on/ %U"))
     ("m" "Meeting" entry  (file+headline "agenda/agenda.org" "Future")
      ,(concat "* <%<%Y-%m-%d %a %H:00>> %? :meeting:\n"))
     ("n" "Note" entry  (file "agenda/notes.org")
      ,(concat "* Note (%a)\n"
               "/Entered on/ %U\n" "\n" "%?"))))

  ;; org-agenda
  (org-agenda-files
   (mapcar 'file-truename
           (file-expand-wildcards (concat org-directory "agenda/*.org"))))
  ;; Refile and Archive
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets `((,(expand-file-name  "agenda/agenda.org" org-directory) :maxlevel . 3)
                        (,(expand-file-name  "agenda/projects.org" org-directory) :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
                        (,(expand-file-name  "agenda/literature.org" org-directory) :maxlevel . 2)
                        (,(expand-file-name  "agenda/scheduled.org" org-directory) :maxlevel . 2)))
  (org-agenda-custom-commands
   '(("g" "Get Things Done (GTD)"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-start-day "today")
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'deadline))
                (org-deadline-warning-days 0)))
       (todo "PROJ"
             ((org-agenda-skip-function
               '(org-agenda-skip-subtree-if 'nottodo '("NEXT" "STRT")))
              (org-agenda-overriding-header "Active Projects:")))
       (todo "STRT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-sorting-strategy '(priority-down category-keep effort-up))
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nActive Tasks\n")
              ))  ; Exclude entries with LITERATURE category
       (todo "NEXT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-sorting-strategy '(priority-down category-keep effort-up))
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nNext Tasks\n")))
       (agenda nil
               ((org-agenda-entry-types '(:deadline))
                (org-agenda-format-date "")
                (org-deadline-warning-days 7)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                (org-agenda-overriding-header "\nDeadlines")))
       (tags-todo "inbox"
                  ((org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-overriding-header "\nInbox\n")))
       (todo "HOLD|WAIT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-sorting-strategy '(priority-down category-keep effort-up))
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nPaused Tasks\n")))
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "\nCompleted today\n"))))
      ((org-agenda-category-filter-preset '("-LITERATURE"))))
     ("l" "Literature" tags-todo "literature"
      ((org-agenda-sorting-strategy '(priority-down category-keep effort-up))
       (org-agenda-prefix-format "  %i %-12:c [%e] ")))))

  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (shell . t)))
  (org-export-backends '(md beamer odt latex icalendar html ascii))
  (org-cite-biblatex-options "hyperref=true,url=true,backend=biber,natbib=true")

  ;; Use SVGs for latex previews -> No blur when scaling
  (org-preview-latex-default-process 'dvisvgm)
  :preface
  ;; https://github.com/rougier/emacs-gtd#activating-tasks
  (defun my/log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  ;; Save the corresponding buffers
  (defun my/gtd-save-org-buffers ()
    "Save `org-agenda-files' buffers without user confirmation.
            See also `org-save-all-org-buffers'"
    (interactive)
    (message "Saving org-agenda-files buffers...")
    (save-some-buffers t (lambda ()
                           (when (member (buffer-file-name) org-agenda-files)
                             t)))
    (message "Saving org-agenda-files buffers... done"))

  ;; archive all DONE tasks in subtree
  ;; https://stackoverflow.com/questions/6997387
  (defun my/org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'tree))

  :hook
  (org-after-todo-state-change . my/log-todo-next-creation-date)
  :bind
  (:map my/toggle-map
        ("c" . org-capture)
        :map my/open-map
        ("a" . org-agenda))
  :config
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (my/gtd-save-org-buffers))))

(use-package ox-latex
  :straight nil
  :after org
  :config
  ;; https://orgmode.org/manual/LaTeX-specific-export-settings.html
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
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

(use-package ox-beamer
  :straight nil
  :after org
  :config
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}")))

(use-package ox-extra
  :straight nil
  :after org
  :config
  (ox-extras-activate '(ignore-headlines)))

;; [[https://github.com/awth13/org-appear.git][org-appear]]
;; Toggle visibility of hidden Org mode element parts upon entering and leaving an element.

;; *test* /aaa/ =babab=

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;; [[https://github.com/yilkalargaw/org-auto-tangle.git][org-auto-tangle]]
;; A simple emacs package to allow org file tangling upon save.

(use-package org-auto-tangle
  :after org
  :hook (org-mode . org-auto-tangle-mode))

;; [[https://github.com/rexim/org-cliplink.git][org-cliplink]]
;; A simple command that takes a URL from the clipboard and inserts an org-mode link with a title of a page found by the URL into the current buffer.


(use-package org-cliplink
  :after org)

;; [[https://github.com/minad/org-modern.git][org-modern]]
;; This package implements a modern style for your Org buffers using font locking and text properties. The package styles headlines, keywords, tables and source blocks.


(use-package org-modern
  :custom
  (org-modern-fold-stars '(("▶" . "▼") ("▹" . "▿") ("▸" . "▾")))
  (org-modern-star 'fold)
  (org-modern-label-border 0.3)

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
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;; [[https://github.com/jdtsmith/org-modern-indent.git][org-modern-indent]]
;; Modern block styling with org-indent.

(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :hook
  (org-indent-mode . org-modern-indent-mode))

;; [[https://github.com/org-noter/org-noter.git][org-noter]]
;; Emacs document annotator, using Org-mode.

(use-package org-noter
  :after org
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
  ;; Org-noter’s purpose is to let you create notes that are kept in sync when
  ;; you scroll through the [PDF etc] document
  (org-noter-insert-heading . org-id-get-create))

;; [[https://github.com/marcinkoziej/org-pomodoro.git][org-pomodoro]]
;; Pomodoro technique for org-mode.

(use-package org-pomodoro
  :custom
  (org-pomodoro-audio-player (or (executable-find "paplay")
                                 org-pomodoro-audio-player))
  :config
  (use-package alert
    :config
    (alert-add-rule :category "org-pomodoro"
                    :style (cond (alert-growl-command
                                  'growl)
                                 (alert-notifier-command
                                  'notifier)
                                 (alert-libnotify-command
                                  'libnotify)
                                 (alert-default-style))))
  :bind
  (:map org-mode-map
        ("C-c p" . org-pomodoro)
        :map org-agenda-keymap
        ("p" . org-pomodoro)))

;; [[https://github.com/rlister/org-present.git][org-present]]
;; Ultra-minimalist presentation minor-mode for Emacs org-mode.
;; Inspired by: https://systemcrafters.net/emacs-tips/presentations-with-org-present/

(use-package org-present
  :after org
  :preface
  (defun my/org-present-start ()
    (org-present-read-only)
    (org-display-inline-images)

    ;; Hide Property drawers
    (org-tidy-mode 1)

    ;; Tweak font sizes
    (setq-local face-remapping-alist '((header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:inherit variable-pitch :height 2.0) org-document-title)
                                       (org-level-1 (:inherit variable-pitch :height 1.5) org-level-1)
                                       (org-level-2 (:inherit variable-pitch :height 1.3) org-level-2)
                                       (org-level-3 (:inherit variable-pitch :height 1.2) org-level-3)
                                       (org-level-4 (:inherit variable-pitch :height 1.1) org-level-4)
                                       (org-level-5 (:inherit variable-pitch :height 1.1) org-level-5)
                                       (org-level-6 (:inherit variable-pitch :height 1.1) org-level-6)
                                       (org-level-7 (:inherit variable-pitch :height 1.1) org-level-7)
                                       (org-level-8 (:inherit variable-pitch :height 1.1) org-level-8)
                                       (org-default (:inherit variable-pitch) org-default)
                                       (org-table (:inherit fixed-pitch) org-table)
                                       (org-code (:inherit fixed-pitch) org-code)
                                       (org-verbatim (:inherit fixed-pitch) org-verbatim)
                                       ;; (org-hide (:inherit fixed-pitch) org-hide)
                                       ;; (default (:inherit variable-pitch))
                                       ))

    ;; Set a blank header line string to create blank space at the top
    (setq-local header-line-format " ")

    ;; Configure fill width
    (setq-local visual-fill-column-width 160
                visual-fill-column-center-text t)

    ;; Remove org modern borders from blocks
    (setq-local org-modern-block-fringe nil)

    ;; Center the presentation and wrap lines
    (visual-fill-column-mode 1)

    ;; Swicth to minimal ui mode
    (my/minimal-ui-mode 1)

    ;; disable fringes
    (set-fringe-mode 0)

    ;; Increase font size
    ;;(org-present-big)
    )
  (defun my/org-present-quit ()
    (org-present-read-write)
    (org-remove-inline-images)

    ;; Show Property drawers
    (org-tidy-untidy-buffer)
    (org-tidy-mode 0)

    ;; Reset font customizations
    (kill-local-variable 'face-remapping-alist)

    ;; Clear the header line string so that it isn't displayed
    (kill-local-variable 'header-line-format)

    ;; Configure fill width
    (kill-local-variable 'visual-fill-column-width)
    (kill-local-variable 'visual-fill-column-center-text)

    ;; Reset org modern borders from blocks
    (kill-local-variable 'org-modern-block-fringe)

    ;; Stop centering the presentation and wrap lines
    (visual-fill-column-mode 0)

    ;; Disable minimal ui mode
    (my/minimal-ui-mode -1)

    ;; reset fringes to default style
    (set-fringe-mode nil)

    ;; Restore font size
    ;;(org-present-small)
    )
  (defun my/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)

    ;; Unfold the current entry
    (org-fold-show-entry)

    ;; Show only direct subheadings of the slide but don't expand them
    (org-fold-show-children))
  :bind
  (:map org-mode-map
        ("C-c p"         . org-present)
        :map org-present-mode-keymap
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
  :straight (:host github :repo "karthink/org-preview")
  :hook
  (org-mode . org-preview-mode))

;; [[https://github.com/jxq0/org-tidy.git][org-tidy]]
;; An Emacs minor mode to automatically tidy org-mode property drawers.

(use-package org-tidy
  :after org)

;; [[https://github.com/emacsorphanage/ox-pandoc.git][ox-pandoc]]
;; org-mode exporter via pandoc.

(use-package ox-pandoc
  :if (executable-find "pandoc")
  :after ox
  :demand t
  :custom
  (org-pandoc-options
   '((standalone . t)
     (mathjax . t)
     (variable . "revealjs-url=https://revealjs.com")))
  :config
  (add-to-list 'org-export-backends 'pandoc))

;; [[https://github.com/tarsius/orglink.git][orglink]]
;; Use Org Mode links in other modes.

(use-package orglink
  :hook
  (prog-mode . orglink-mode))

;; [[https://github.com/snosov1/toc-org.git][toc-org]]
;; Toc-org is an Emacs utility to have an up-to-date table of contents in the org files without exporting (useful primarily for readme files on GitHub).


(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-enable))

;; Library Footer

(provide 'my-org)
;;; my-org.el ends here
