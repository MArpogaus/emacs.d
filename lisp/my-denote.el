;;; my-denote.el --- Emacs configuration file  -*- no-byte-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-12-05
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/protesilaos/denote.git][denote]]
;; Simple note taking and file naming.

(use-package denote
  :custom
  ;; Configure the directory where your notes will be stored.
  (denote-directory (expand-file-name "denote/" org-directory))
  ;; If you want a controlled vocabulary of keywords, specify them here.
  (denote-known-keywords '("ENERGY" "STATS" "CS" "AI"))
  ;; If you want Denote to infer keywords from your note titles, set this to t.
  (denote-infer-keywords t)
  ;; If you want to sort keywords alphabetically, set this to t.
  (denote-sort-keywords t)
  ;; Specify a regular expression to exclude directories from being searched for notes.
  (denote-excluded-directories-regexp nil)
  ;; Configure the date format used in note file names.
  (denote-date-format nil)
  ;; Disable confirmation prompts when renaming files. Use with caution!
  (denote-rename-confirmations nil)
  ;; When displaying backlinks, don't show the surrounding context.
  (denote-backlinks-show-context nil)
  ;; Configure the format used for renaming Denote buffers.
  (denote-rename-buffer-format "[D] %t%b")
  ;; String to indicate that a buffer has backlinks.
  (denote-buffer-has-backlinks-string " (<--->)")
  ;; Define templates for notes
  (denote-templates
   '((minutes . "minutes")
     (plain . nil)))
  :preface
  (defvar my/denote-map (make-sparse-keymap) "key-map for denote commands")
  :init
  (define-key my/leader-map (kbd "n") (cons "denote" my/denote-map))
  :bind
  (:map global-map
        :map my/denote-map
        ("n" . denote)
        ("N" . denote-type)
        ("r" . denote-rename-file)
        ("R" . denote-rename-file-using-front-matter)
        ("i" . denote-link)
        ("I" . denote-add-links)
        ("b" . denote-backlinks)
        ;; :map org-mode-map
        ;; ("l" . denote-org-extras-dblock-insert-links)
        ;; ("b" . denote-org-extras-dblock-insert-links)
        :map dired-mode-map
        ("i" . denote-link-dired-marked-notes)
        ("r" . denote-dired-rename-marked-files)
        ("k" . denote-dired-rename-marked-files-with-keywords)
        ("f" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (add-hook 'context-menu-functions #'denote-context-menu)
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode)
   (after-init . denote-rename-buffer-mode)))

;; [[https://github.com/emacs-straight/denote-menu.git][denote-menu]]

(use-package denote-menu
  :after denote
  :bind
  (:map my/denote-map
        ("m" . list-denotes)))

;; [[https://github.com/pprevos/citar-denote.git][citar-denote]]

(use-package citar-denote
  :after denote citar
  :custom
  (citar-denote-template 'biblio)
  (citar-denote-subdir "bib_notes")
  :config
  (add-to-list 'denote-templates
               `(biblio . ,(concat
                            "#+cite_export: biblatex ieee\n"
                            (concat "#+bibliography: " (car citar-bibliography) "\n\n")
                            "* Notes :ignore:\n"
                            ":PROPERTIES:\n"
                            ":NOTER_DOCUMENT: ${file} \n"
                            ":END:\n\n"
                            "* Summary :childless:showchildren:export:\n"
                            "This is a summary of [cite/t:@${=key=}].\n"
                            "** Bibliography :ignore:\n")))
  :init
  (citar-denote-mode))

;; [[https://github.com/emacs-straight/consult-denote.git][consult-denote]]

(use-package consult-denote
  :after denote consult
  :bind
  (:map my/denote-map
        ("f" . consult-denote-find)
        ("g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

;; Library Footer

(provide 'my-denote)
;;; my-denote.el ends here
