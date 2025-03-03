;;; my-keybindings.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-03-03
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/meow-edit/meow.git][meow]]
;; Meow is yet another modal editing mode for Emacs.

(use-package meow
  ;; :ensure (:build (:not autoloads))
  :demand t
  :custom
  ;; use system clipboard
  (meow-use-clipboard t)
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :preface
  ;; Here we define some helper variables and functions to mimic the bahaviour
  ;; of =meow-mode-state-list= for minor modess
  (defvar my/meow-desired-state nil
    "Buffer-local variable to specify the desired Meow state.")
  (defun my/meow-set-desired-state (state)
    "Set the buffer-local variable =my/meow-desired-state= to the specified state."
    (setq-local my/meow-desired-state state))
  (defun my/meow-mode-get-state-advice (orig-func &rest args)
    "Advice function to modify =meow--mode-get-state= based on =my/meow-desired-state=."
    (if my/meow-desired-state
        my/meow-desired-state
      (apply orig-func args)))
  (defun my/meow-git-timemachine-hook ()
    "Hook to set my/meow-desired-state to =motion= when entering git-timemachine mode."
    (my/meow-set-desired-state 'motion))
  :config
  ;; Apply advice to 'meow--mode-get-state'
  (advice-add #'meow--mode-get-state :around #'my/meow-mode-get-state-advice)
  (define-key meow-normal-state-keymap (kbd "SPC") my/leader-map)
  (define-key meow-motion-state-keymap (kbd "SPC") my/leader-map)
  (with-eval-after-load 'tab-line
    (advice-add #'meow-quit :override #'my/tab-line-close-tab-function))
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
   (elpaca-after-init . meow-global-mode)))

;; [[https://github.com/justbur/emacs-which-key.git][which-key]]
;; The mode displays the key bindings following your currently entered incomplete command (a ;; prefix) in a popup.

(use-package which-key
  :custom
  (which-key-idle-delay 0.1)
  (which-key-compute-remaps t)
  (which-key-prefix-prefix "󰜄 ")
  (which-key-separator " ")
  :config
  (which-key-setup-minibuffer)
  :hook
  (meow-mode . which-key-mode))

;; Library Footer

(provide 'my-keybindings)
;;; my-keybindings.el ends here
