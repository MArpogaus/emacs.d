;;; my-keybindings.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-11-11
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/DevelopmentCool2449/standard-keys-mode.git][standard-keys-mode]]
;; Emulate standard keybindings from modern editors.

(use-package standard-keys-mode
  :ensure (:host github :repo "DevelopmentCool2449/standard-keys-mode")
  ;; Enable standard-keys-mode after initializing the Emacs session.
  :hook elpaca-after-init
  :autoload standard-keys-C-x-dynamic-prefix standard-keys-C-c-dynamic-prefix
  :preface
  (defvar-keymap my/standard-keys-keymap
    :doc "Minimal and basic CUA-like keymap for `standard-keys-map-style'.")
  :custom
  ;; select keymap theme to use
  (standard-keys-map-style 'my/standard-keys-keymap)

  ;; Make the C-x and C-c bindings are properly overriden (this is optional)
  (standard-keys-override-new-C-x-and-C-c-commands nil)

  ;; Enable hungry deletion in programming modes
  (backward-delete-char-untabify-method 'all)

  ;; Better isearch movement
  (isearch-repeat-on-direction-change t)
  :config
  (define-key my/standard-keys-keymap (kbd "C-y") standard-keys-C-x-dynamic-prefix)
  (define-key my/standard-keys-keymap (kbd "C-d") standard-keys-C-c-dynamic-prefix)
  :bind
  (:map my/standard-keys-keymap
        ("C-o"   . find-file)
        ("C-S-o" . revert-buffer)
        ;; ("C-w"   . kill-current-buffer)
        ("C-q"   . save-buffers-kill-terminal)
        ("C-x"   . standard-keys-cut-region-or-line)
        ("C-c"   . standard-keys-copy-region-or-line)
        ("C-v"   . yank)
        ("C-z"   . undo-only)
        ("C-S-z" . undo-redo)
        ("C-f"   . isearch-forward)
        ("C-S-f" . isearch-backward)
        ("C-r"   . query-replace)
        ("C-S-r" . query-replace-regexp)
        ("C-s"   . save-buffer)
        ("C-p"   . print-buffer)
        ("C-a"   . mark-whole-buffer)
        ("C-+"   . text-scale-increase)
        ("C--"   . text-scale-decrease)
        ("C-="   . text-scale-adjust)
        ("C-;"   . comment-line)
        ("C-S-<return>" . standard-keys-newline-and-indent-before-point)
        ("C-b"      . switch-to-buffer)
        ("<home>"   . standard-keys-move-beginning-of-line-or-indentation)
        :map context-menu-mode-map
        ;; Bind Context Menu to `Apps' button
        ;; (requires context-menu-mode enabled)
        ("<apps>" . context-menu-open)

        ;; Make isearch easy to use
        :map isearch-mode-map
        ("<up>"   . isearch-repeat-backward)
        ("<down>" . isearch-repeat-forward)
        ("<remap> <yank>" . isearch-yank-kill)

        ;; Use RET as y (yes) action in y-or-n prompts
        :map y-or-n-p-map
        ("<return>" . y-or-n-p-insert-y)))

;; [[https://github.com/meow-edit/meow.git][meow]]
;; Meow is yet another modal editing mode for Emacs.

(use-package meow
  ;; :ensure (:build (:not autoloads))
  :demand t
  :custom
  ;; use system clipboard
  (meow-use-clipboard t)
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :config
  (define-key meow-normal-state-keymap (kbd "SPC") my/leader-map)
  (define-key meow-motion-state-keymap (kbd "SPC") my/leader-map)
  (with-eval-after-load 'tab-line
    (advice-add #'meow-quit :override #'my/tab-line-close-tab-function))
  (setopt meow-mode-state-list
          (append meow-mode-state-list '((comint-mode . insert)
                                         (eshell-mode . insert)
                                         (vterm-mode  . insert))))
  :bind
  (:map meow-motion-state-keymap
        ("<escape>" . meow-cancel-selection)
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
  ((git-timemachine-mode . meow-motion-mode)
   (elpaca-after-init . meow-global-mode)))

;; [[https://github.com/justbur/emacs-which-key.git][which-key]]
;; The mode displays the key bindings following your currently entered incomplete command (a ;; prefix) in a popup.

(use-package which-key
  :custom
  (which-key-idle-delay 0.1)
  (which-key-compute-remaps t)
  (which-key-prefix-prefix "󰜄 ")
  (which-key-separator "  ")
  :config
  (which-key-setup-minibuffer)
  :hook
  (meow-mode . which-key-mode))

;; Library Footer

(provide 'my-keybindings)
;;; my-keybindings.el ends here
