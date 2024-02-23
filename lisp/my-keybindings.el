;;; my-keybindings.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-02-23
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/meow-edit/meow.git][meow]]
;; Meow is yet another modal editing mode for Emacs.

(use-package meow
  :demand t
  :custom
  (meow-keypad-start-keys . ())
  (meow-keypad-meta-prefix . nil)
  (meow-keypad-ctrl-meta-prefix . nil)
  (meow-keypad-literal-prefix . nil)
  (meow-keypad-self-insert-undefined . nil)
  ;; use system clipboard
  (meow-use-clipboard t)
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :preface
  ;; Buffer-local variable to specify the desired Meow state
  (defvar my/meow-desired-state nil
    "Buffer-local variable to specify the desired Meow state.")

  ;; Function to set the buffer-local value of my/meow-desired-state
  (defun my/meow-set-desired-state (state)
    "Set the buffer-local variable =my/meow-desired-state= to the specified state."
    (setq-local my/meow-desired-state state))

  ;; Advice function to modify 'meow--mode-get-state' based on 'my/meow-desired-state'
  (defun my/meow-mode-get-state-advice (orig-func &rest args)
    "Advice function to modify =meow--mode-get-state= based on =my/meow-desired-state=."
    (if my/meow-desired-state
        my/meow-desired-state
      (apply orig-func args)))

  ;; Hook to set my/meow-desired-state to 'motion' when entering git-timemachine mode
  (defun my/meow-git-timemachine-hook ()
    "Hook to set my/meow-desired-state to =motion= when entering git-timemachine mode."
    (my/meow-set-desired-state 'motion))

  :config
  (with-eval-after-load 'nerd-icons
    (setq meow-replace-state-name-list
          '((normal . "󰆾")
            (motion . "󰷢")
            (keypad . "󰌌")
            (insert . "󰏫")
            (beacon . "󰩬")))
    (setq meow-indicator-face-alist
          '((normal . meow-normal-indicator)
            (motion . nerd-icons-lred)
            (keypad . meow-normal-indicator)
            (insert . nerd-icons-lgreen)
            (beacon . nerd-icons-orange))))
  ;; Apply advice to 'meow--mode-get-state'
  (advice-add 'meow--mode-get-state :around #'my/meow-mode-get-state-advice)
  (add-to-list 'meow-keymap-alist `(leader . ,my/leader-map))
  :bind
  (:map meow-motion-state-keymap
        ("<escape>" . meow-cancel-selection)
        ("," . meow-inner-of-thing)
        ("." . meow-bounds-of-thing)
        ("b" . meow-back-word)
        ("e" . meow-next-word)
        ("f" . meow-find)
        ("o" . meow-block)
        ("q" . meow-quit)
        ("t" . meow-till)
        ("v" . meow-visit)
        ("w" . meow-mark-word)
        ("x" . meow-line)
        ("y" . meow-save)
        ("E" . meow-next-symbol)
        ("W" . meow-mark-symbol)
        ("X" . meow-goto-line)
        :map my/leader-map
        ;; ("j" . "H-j")
        ;; ("k" . "H-k")
        ;; ("x" ("C-x" . ctl-x-map))
        ;;("m" . (lambda ()(meow--execute-kbd-macro "C-c")))
        ;; Use SPC (0-9) for digit arguments.
        ("SPC" . project-list-buffers)
        ("1" . meow-digit-argument)
        ("2" . meow-digit-argument)
        ("3" . meow-digit-argument)
        ("4" . meow-digit-argument)
        ("5" . meow-digit-argument)
        ("6" . meow-digit-argument)
        ("7" . meow-digit-argument)
        ("8" . meow-digit-argument)
        ("9" . meow-digit-argument)
        ("0" . meow-digit-argument)
        ("/" . meow-keypad-describe-key)
        ("?" . meow-cheatsheet)
        :map meow-normal-state-keymap
        ("'" . repeat)
        ("," . meow-inner-of-thing)
        ("-" . negative-argument)
        ("." . meow-bounds-of-thing)
        ("0" . meow-expand-0)
        ("1" . meow-expand-1)
        ("2" . meow-expand-2)
        ("3" . meow-expand-3)
        ("4" . meow-expand-4)
        ("5" . meow-expand-5)
        ("6" . meow-expand-6)
        ("7" . meow-expand-7)
        ("8" . meow-expand-8)
        ("9" . meow-expand-9)
        (";" . meow-reverse)
        ("<escape>" . meow-cancel-selection)
        ("=" . meow-indent)
        ("A" . meow-open-below)
        ("B" . meow-back-symbol)
        ("C" . meow-comment)
        ("D" . meow-backward-delete)
        ("E" . meow-next-symbol)
        ("G" . meow-grab)
        ("H" . meow-left-expand)
        ("I" . meow-open-above)
        ("J" . meow-next-expand)
        ("K" . meow-prev-expand)
        ("L" . meow-right-expand)
        ("O" . meow-to-block)
        ("Q" . meow-goto-line)
        ("R" . undo-redo)
        ("U" . meow-undo-in-selection)
        ("W" . meow-mark-symbol)
        ("X" . meow-goto-line)
        ("Y" . meow-sync-grab)
        ("[" . meow-beginning-of-thing)
        ("]" . meow-end-of-thing)
        ("a" . meow-append)
        ("b" . meow-back-word)
        ("c" . meow-change)
        ("d" . meow-delete)
        ("e" . meow-next-word)
        ("f" . meow-find)
        ("h" . meow-left)
        ("i" . meow-insert)
        ("j" . meow-next)
        ("k" . meow-prev)
        ("l" . meow-right)
        ("m" . meow-join)
        ("n" . meow-search)
        ("o" . meow-block)
        ("p" . meow-yank)
        ("q" . meow-quit)
        ("r" . meow-replace)
        ("s" . meow-kill)
        ("t" . meow-till)
        ("u" . meow-undo)
        ("v" . meow-visit)
        ("w" . meow-mark-word)
        ("x" . meow-line)
        ("y" . meow-save)
        ("z" . meow-pop-selection))
  :hook
  ((git-timemachine-mode . my/meow-git-timemachine-hook)
   (after-init . meow-global-mode)))

;; [[https://github.com/justbur/emacs-which-key.git][which-key]]
;; The mode displays the key bindings following your currently entered incomplete command (a ;; prefix) in a popup.

(use-package which-key
  :custom
  (which-key-idle-delay 0.0)
  :hook
  (meow-mode . which-key-mode))

;; Library Footer

(provide 'my-keybindings)
;;; my-keybindings.el ends here
