;;; my-denote.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-08-06
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

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
  :bind
  (:map my/denote-map
        ("f" . consult-denote-find)
        ("g" . consult-denote-grep))
  :config
  (consult-denote-mode))

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
  ;; Specify a regular expression to exclude keywords from being searched for notes.
  (denote-excluded-keywords-regexp nil)
  ;; Pick dates, where relevant, with Org's advanced interface:
  (denote-date-prompt-use-org-read-date t)
  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode t)
  ;; Define templates for notes
  (denote-templates
   '((minutes . "minutes")
     (plain . nil)))
  :preface
  (defun my/kill-denote-buffers ()
    "Kill all denote buffers."
    (interactive)
    (dolist (buf (buffer-list))
      (when (or (string-prefix-p "[D]" (buffer-name buf))
                (string-prefix-p "*Denote" (buffer-name buf)))
        (kill-buffer buf))))
  :bind
  (:map my/denote-map
        ("L" . denote-add-links)
        ("N" . denote-type)
        ("R" . denote-rename-file-using-front-matter)
        ("b" . denote-backlinks)
        ("d" . denote-dired)
        ("g" . denote-grep)
        ("i" . denote-link)
        ("k" . my/kill-denote-buffers)
        ("n" . denote)
        ("r" . denote-rename-file)
        ("q c" . denote-query-contents-link) ; create link that triggers a grep
        ("q f" . denote-query-filenames-link) ; create link that triggers a dired
        :map dired-mode-map
        ("i" . denote-link-dired-marked-notes)
        ("r" . denote-dired-rename-marked-files)
        ("k" . denote-dired-rename-marked-files-with-keywords)
        ("f" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode)
   (elpaca-after-init . denote-rename-buffer-mode)))

;; [[https://github.com/emacs-straight/denote-menu.git][denote-menu]]

(use-package denote-menu
  :after denote
  :bind
  (:map my/denote-map
        ("m" . list-denotes)))

;; [[https://github.com/protesilaos/denote-sequence][denote-sequence]]

(use-package denote-sequence
  :after denote
  :demand t
  :bind
  ( :map my/denote-map
    ;; - `denote-sequence-new-parent'
    ;; - `denote-sequence-new-sibling'
    ;; - `denote-sequence-new-child'
    ;; - `denote-sequence-new-child-of-current'
    ;; - `denote-sequence-new-sibling-of-current'
    ("s s" . denote-sequence)
    ("s f" . denote-sequence-find)
    ("s l" . denote-sequence-link)
    ("s d" . denote-sequence-dired)
    ("s r" . denote-sequence-reparent)
    ("s c" . denote-sequence-convert))
  :custom
  ;; The default sequence scheme is `numeric'.
  (denote-sequence-scheme 'alphanumeric))

;; Library Footer

(provide 'my-denote)
;;; my-denote.el ends here
