;;; my-denote.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-09
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
  ;; Absolute: outside a silo `denote-directory' is a list.
  (citar-denote-subdir (expand-file-name "bib_notes/" (car (default-value 'denote-directory))))
  :config
  (add-to-list 'denote-templates `(biblio . ,(my/bib-note-template)))
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
  ;; Silos set their own value in .dir-locals.el; the list is for
  ;; everything outside them and for Org export, which ignores dir-locals.
  (denote-directory (list (expand-file-name "denote/work/" org-directory)
                          (expand-file-name "denote/private/" org-directory)))
  (denote-prompts '(title keywords subdirectory))
  ;; Pick dates, where relevant, with Org's advanced interface:
  (denote-date-prompt-use-org-read-date t)
  :preface
  (defun my/kill-denote-buffers ()
    "Kill all denote buffers."
    (interactive)
    (dolist (buf (buffer-list))
      (when (or (string-prefix-p "[D]" (buffer-name buf))
                (string-prefix-p "*Denote" (buffer-name buf)))
        (kill-buffer buf))))
  (defun my/denote-save-buffer (&optional title keywords file-type directory date template signature identifier)
    "Save the current buffer as a Denote note or just save if already denoted.
Prompts for metadata only if the file is not already a Denote file."
    (interactive)
    (if (and buffer-file-name (not (denote-file-has-denoted-filename-p buffer-file-name)))
        ;; A file of its own: rename it rather than write a copy.
        (progn (save-buffer) (call-interactively #'denote-rename-file))
      (unless buffer-file-name
        ;; If not denoted, get data (prompts user if called interactively)
        (pcase-let* ((`(,title ,keywords ,file-type ,directory ,date ,identifier ,template ,signature)
                      (if (called-interactively-p 'any)
                          (denote--creation-get-note-data-from-prompts)
                        (list title keywords file-type directory date identifier template signature))))
          ;; Proceed with Denote creation logic
          (let* ((data (denote--creation-prepare-note-data
                        title keywords file-type directory date identifier template signature))
                 (extension (denote--file-extension (nth 2 data)))
                 (path (denote-format-file-name (nth 3 data) (nth 5 data) (nth 1 data) (nth 0 data) extension (nth 7 data)))
                 (header (denote--format-front-matter (nth 0 data) (nth 4 data) (nth 1 data) (nth 5 data) (nth 7 data) (nth 2 data))))
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (insert header)
                (insert (cond ((stringp (nth 6 data)) (nth 6 data))
                              ((functionp (nth 6 data)) (funcall (nth 6 data)))
                              (t "")))))
            (set-visited-file-name path))))
      (save-buffer)))
  :bind
  (:map my/denote-map
        ("L" . denote-add-links)
        ("N" . denote-type)
        ("R" . denote-rename-file-using-front-matter)
        ("b" . denote-backlinks)
        ("d" . denote-dired)
        ("i" . denote-link)
        ("k" . my/kill-denote-buffers)
        ("s" . my/denote-save-buffer)
        ("n" . denote)
        ("r" . denote-rename-file)
        ("q c" . denote-query-contents-link) ; create link that triggers a grep
        ("q f" . denote-query-filenames-link) ; create link that triggers a dired
        :map dired-mode-map
        ("i" . denote-link-dired-marked-notes)
        ("r" . denote-dired-rename-marked-files)
        ("k" . denote-dired-rename-marked-files-with-keywords)
        ("M-r" . denote-dired-rename-marked-files-using-front-matter))
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
  ((text-mode . denote-fontify-links-mode)
   (dired-mode . denote-dired-mode)
   (elpaca-after-init . denote-rename-buffer-mode)))

;; [[https://github.com/protesilaos/denote-silo][denote-silo]]
;; Commands that ask for a silo first.

(use-package denote-silo
  :custom
  ;; The global value: the package may load in a silo buffer.
  (denote-silo-directories (default-value 'denote-directory))
  :bind
  (:map my/denote-map
        ("c" . denote-silo-create-note)
        ("o" . denote-silo-open-or-create)
        ("D" . denote-silo-dired)))

;; [[https://github.com/emacs-straight/denote-menu.git][denote-menu]]

(use-package denote-menu
  :after denote
  :custom
  (denote-menu-initial-regex (rx "." (or "org" "pdf" "svg" "png" "jpg") line-end))
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
    ("S s" . denote-sequence)
    ("S f" . denote-sequence-find)
    ("S l" . denote-sequence-link)
    ("S d" . denote-sequence-dired)
    ("S r" . denote-sequence-reparent)
    ("S c" . denote-sequence-convert))
  :custom
  ;; The default sequence scheme is `numeric'.
  (denote-sequence-scheme 'alphanumeric))

;; ZZ Library Footer

(provide 'my-denote)
;;; my-denote.el ends here
