;;; my-org.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; This file has been generated from emacs.org file. DO NOT EDIT.

;; Copyright (C) 2010-2021 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: internal
;; URL: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

(use-package org
  :custom
  (org-ellipsis " ▾")
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
  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'tree))
  :hook
  (org-after-todo-state-change . my/log-todo-next-creation-date)
  :config
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (my/gtd-save-org-buffers)))
  :bind
  (:map my/leader-map
        ("c" . org-capture)
        :map my/open-map
        ("a" . org-agenda)))

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

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-auto-tangle
  :after org
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-brain
  :after org org-noter
  :preface
  ;; from org brain README
  ;; Here’s a command which uses org-cliplink to add a link from the clipboard
  ;; as an org-brain resource.
  ;; It guesses the description from the URL title.
  ;; Here I’ve bound it to L in org-brain-visualize.
  (defun org-brain-cliplink-resource ()
    "Add a URL from the clipboard as an org-brain resource.
  Suggest the URL title as a description for resource."
    (interactive)
    (let ((url (org-cliplink-clipboard-content)))
      (org-brain-add-resource
       url
       (org-cliplink-retrieve-title-synchronously url)
       t)))

  (defun org-brain-open-org-noter (entry)
    "Open `org-noter' on the ENTRY.
  If run interactively, get ENTRY from context."
    (interactive (list (org-brain-entry-at-pt)))
    (org-with-point-at (org-brain-entry-marker entry)
      (org-noter)))

  (defun org-brain-insert-resource-icon (link)
    "Insert an icon, based on content of org-mode LINK."
    (insert (format "%s "
                    (cond ((string-prefix-p "brain:" link)
                           (nerd-icons-flicon "brain"))
                          ((string-prefix-p "info:" link)
                           (nerd-icons-octicon "info"))
                          ((string-prefix-p "help:" link)
                           (nerd-icons-material "help"))
                          ((string-prefix-p "http" link)
                           (nerd-icons-icon-for-url link))
                          (t
                           (nerd-icons-icon-for-file link))))))

  :config
  (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)
  :custom
  (org-id-track-globally t)
  (org-id-locations-file (expand-file-name "/org-id-locations" user-emacs-directory))
  (org-brain-visualize-default-choices 'all)
  (org-brain-title-max-length 24)
  (org-brain-include-file-entries t)
  (org-brain-file-entries-use-title t)
  :commands
  org-brain-visualize
  :hook
  (before-save . org-brain-ensure-ids-in-buffer))

(use-package org-cliplink
  :after org)

(use-package org-modern
  :hook (org-mode . global-org-modern-mode)
  :after (:any org org-agenda)
  :custom
  (org-modern-star '("◉" "○" "◇"))
  (org-modern-label-border 0.3)

  ;; Edit settings
  (org-auto-align-tags t)
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
   "⭠ now ─────────────────────────────────────────────────"))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :hook ; add late to hook
  (org-mode . org-modern-indent-mode))

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

(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-enable))

(provide 'my-org)
;;; my-org.el ends here
