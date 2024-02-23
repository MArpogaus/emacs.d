;;; my-tools.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2024-02-23
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/radian-software/ctrlf.git][ctrlf]]
;; Emacs finally learns how to ctrl+F.

(use-package ctrlf
  :hook
  ((after-init . ctrlf-mode)
   (pdf-isearch-minor-mode . (lambda () (ctrlf-local-mode -1)))))

;; dired :build_in:

(use-package dired
  :straight nil
  :custom
  ;; inspired by doom
  ;; https://github.com/doomemacs/doomemacs/blob/c2818bcfaa5dc1a0139d1deff7d77bf42a08eede/modules/emacs/dired/config.el#L9C1-L25C36
  (dired-dwim-target t)  ; suggest a target for moving/copying intelligently
  (dired-hide-details-hide-symlink-targets nil)
  ;; don't prompt to revert, just do it
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  ;; Always copy/delete recursively
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)
  ;; Ask whether destination dirs should get created when copying/removing files.
  (dired-create-destination-dirs 'ask)
  ;; Screens are larger nowadays, we can afford slightly larger thumbnails
  (image-dired-thumb-size 150)
  (delete-by-moving-to-trash t)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; kill all session buffers on quit
  (dirvish-reuse-session nil)
  ;; Enable mouse drag-and-drop support
  (dired-mouse-drag-files t)                   ; added in Emacs 29
  (mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  :bind
  (:map my/open-map
        ("d" . dired)))

(use-package dired-x
  :straight nil
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$"))
  :hook
  (dired-mode . dired-omit-mode))

;; [[https://github.com/purcell/diredfl.git][diredfl]]
;; Extra Emacs font lock rules for a more colourful dired.

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   (dirvish-directory-view-mode . diredfl-mode)))

;; [[https://github.com/alexluigit/dirvish.git][dirvish]]
;; A polished Dired with batteries included.

(use-package dirvish
  :after dired
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (vc-info yank index)))
  (dirvish-attributes
   '(vc-state file-size git-msg subtree-state nerd-icons collapse file-time))
  (dirvish-use-header-line nil)
  ;; (dirvish-use-mode-line nil)
  :config
  (dirvish-override-dired-mode)
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (with-eval-after-load 'doom-modeline
    (setq dirvish-mode-line-height doom-modeline-height)
    (setq dirvish-header-line-height
          doom-modeline-height))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map my/open-map
   ("D" . dirvish)
   :map my/toggle-map
   ("d" . dirvish-side)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ;; ("<mouse-1>" . dirvish-subtree-toggle-or-open)
   ("<mouse-2>" . dired-mouse-find-file-other-window)
   ("F" . dirvish-toggle-fullscreen)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)
   ("M-f" . dirvish-history-go-forward)
   ("M-j" . dirvish-fd-jump)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-s" . dirvish-setup-menu)
   ("M-t" . dirvish-layout-toggle)
   ("N"   . dirvish-narrow)
   ("TAB" . dirvish-subtree-toggle)
   ("^"   . dirvish-history-last)
   ("a"   . dirvish-quick-access)
   ("b"   . dirvish-goto-bookmark)
   ("f"   . dirvish-file-info-menu)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("y"   . dirvish-yank-menu)
   ("z" . dirvish-show-history)))

;; ediff :build_in:
;; The ediff package is utilized to handle file differences in emacs.
;; We will tweak the Emacs built-in ediff configuration a bit.
;; [[https://panadestein.github.io/emacsd/#org5917c00][Emacs literate configuration]]


(use-package ediff
  :straight nil
  :preface
  (defvar my-ediff-original-windows nil)
  (defun my/store-pre-ediff-winconfig ()
    "This function stores the current window configuration before opening ediff."
    (setq my/ediff-original-windows (current-window-configuration)))
  (defun my/restore-pre-ediff-winconfig ()
    "This function resets the original window arrangement."
    (set-window-configuration my/ediff-original-windows))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :hook
  ((ediff-before-setup . my/store-pre-ediff-winconfig)
   (ediff-quit . my/restore-pre-ediff-winconfig)))

;; [[https://github.com/skeeto/elfeed.git][elfeed]]
;; An Emacs web feeds client.

(use-package elfeed
  :bind
  (:map my/open-map
        ("f" . elfeed))
  :config
  (setq elfeed-feeds
        (split-string (shell-command-to-string "for d in ~/.emacs.d/straight/repos/*; do git -C $d remote get-url origin; done | grep -P '(github)' | sed 's:\\.git:/releases.atom:'"))))

;; [[https://github.com/purcell/exec-path-from-shell.git][exec-path-from-shell]]
;; Make Emacs use the $PATH set up by the user's shell.

(use-package exec-path-from-shell
  :config
  (defun my/copy-ssh-env ()
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
    (exec-path-from-shell-initialize))
  :hook
  (magit-credential . my/copy-ssh-env))

;; flyspell :build_in:

(use-package flyspell
  :straight nil
  :custom
  ;; Doom: https://github.com/doomemacs/doomemacs/blob/dbb48712eea6dfe16815a3e5e5746b31dab6bb2f/modules/checkers/spell/config.el#L195C11-L198C42
  (flyspell-issue-welcome-flag nil)
  ;; Significantly speeds up flyspell, which would otherwise print
  ;; messages for every word when checking the entire buffer
  (flyspell-issue-message-flag nil)
  :preface
  (defun my/restart-flyspell-mode ()
    (when flyspell-mode
      (flyspell-mode-off)
      (flyspell-mode-on)
      (flyspell-buffer)))
  :hook
  (((text-mode org-mode LaTeX-mode) . flyspell-mode)
   ((prog-mode conf-mode) . flyspell-prog-mode)
   (ispell-change-dictionary . restart-flyspell-mode)))

;; [[https://github.com/d12frosted/flyspell-correct.git][flyspell-correct]]
;; Distraction-free words correction with flyspell via selected interface.

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)
              :map flyspell-mouse-map ("RET" . flyspell-correct-at-point)
              ([mouse-1] . flyspell-correct-at-point)))

;; [[https://github.com/karthink/gptel.git][gptel]]
;; A simple LLM client for Emacs.

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  :bind
  (:map my/open-map
        ("g". gptel))
  :commands (gptel gptel-send))

;; [[https://github.com/Wilfred/helpful.git][helpful]]
;; [[https://github.com/Wilfred/helpful][Helpful]] is an alternative to the built-in Emacs help that provides much more contextual information.
;; It is a bit slow to load so we do need load it explicitely.


(use-package helpful
  :bind
  (([remap describe-function] . helpful-function)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command] . helpful-command)
   ([remap describe-key] . helpful-key)
   ("C-h K" . describe-keymap)
   :map helpful-mode-map
   ([remap revert-buffer] . helpful-update)))

;; ispell :build_in:

(use-package ispell
  :straight nil
  :after flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US,de_DE")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,de_DE"))

;; [[https://github.com/vedang/pdf-tools.git][pdf-tools]]
;; Emacs support library for PDF files.

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil))

;; re-builder :build_in:
;; Change re-builder syntax

;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(use-package re-builder
  :straight nil
  :commands re-builder
  :custom
  (reb-re-syntax 'string))

;; term :build_in:
;; Major mode for interacting with a terminal

(use-package term
  :straight nil
  :commands term
  :unless (not (file-exists-p "/bin/zsh")) ; we only use it if shell exists
  :custom
  (shell-file-name "/bin/zsh")
  (explicit-shell-file-name "/bin/zsh"))

;; tramp :build_in:
;; remote file editing through ssh/scp.

(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  (tramp-encoding-shell "/usr/bin/zsh")
  (remote-file-name-inhibit-cache nil)
  (vc-ignore-dir-regexp
   (format "%s\\|%s"
           vc-ignore-dir-regexp
           tramp-file-name-regexp))
  :config
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:user@host:")
                     "remote-shell" "/bin/bash")))

;; [[https://github.com/akermu/emacs-libvterm.git][vterm]]
;; Emacs libvterm integration.

;; https://www.reddit.com/r/emacs/comments/wu5rxi/comment/ilagtzv/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(use-package vterm
  :bind
  (:map my/open-map
        ("v" . vterm)
        :map project-prefix-map
        ("t" . my/project-vterm)
        :map vterm-mode-map
        ("C-<escape>" . vterm-send-escape))
  :preface
  (defun my/project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root     (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
        (vterm))))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands     '(my/project-vterm "Vterm") t)
    (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode)))
  :custom
  (vterm-copy-exclude-prompt t)
  (vterm-max-scrollback 100000)
  (vterm-tramp-shells '(("ssh" "/bin/bash")
                        ("podman" "/bin/bash"))))

;; [[https://github.com/emacs-straight/vundo.git][vundo]]
;; Vundo (visual undo) displays the undo history as a tree and lets you move in the tree to go back to previous buffer states.

(use-package vundo
  :bind
  (:map my/open-map
        ("u". vundo))
  :config
  (when (display-graphic-p)
    (setq vundo-glyph-alist vundo-unicode-symbols)))

;; Library Footer

(provide 'my-tools)
;;; my-tools.el ends here
