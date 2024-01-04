;;; my-tools.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; This file has been generated from emacs.org file. DO NOT EDIT.

;; Copyright (C) 2010-2024 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: internal
;; URL: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

(use-package ctrlf
  :hook
  ((after-init . ctrlf-mode)
   (pdf-isearch-minor-mode . (lambda () (ctrlf-local-mode -1)))))

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
  :config
  (use-package dired-x
    :straight nil
    :config
    ;; Make dired-omit-mode hide all "dotfiles"
    (setq dired-omit-files
          (concat dired-omit-files "\\|^\\..*$"))
    :hook
    (dired-mode . dired-omit-mode))
  :bind
  (:map my/open-map
        ("d" . dired)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package ediff
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

(use-package elfeed
  :bind
  (:map my/open-map
        ("f" . elfeed))
  :config
  (setq elfeed-feeds
        (split-string (shell-command-to-string "for d in straight/repos/*; do git -C $d remote get-url origin; done | grep -P '(github)' | sed 's:\\.git:/releases.atom:'"))))

(use-package exec-path-from-shell
  :config
  (defun my/copy-ssh-env ()
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
    (exec-path-from-shell-initialize))
  :hook
  (magit-credential . my/copy-ssh-env))

(use-package flyspell
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
  :config
  (use-package ispell
    :custom
    (ispell-program-name "hunspell")
    (ispell-dictionary "en_US,de_DE")
    :config
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,de_DE"))
  (use-package flyspell-correct
    :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)
                :map flyspell-mouse-map ("RET" . flyspell-correct-at-point)
                ([mouse-1] . flyspell-correct-at-point)))
  :hook
  (((text-mode org-mode LaTeX-mode) . flyspell-mode)
   ((prog-mode conf-mode) . flyspell-prog-mode)
   (ispell-change-dictionary . restart-flyspell-mode)))

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  :bind
  (:map my/open-map
        ("g". gptel))
  :commands (gptel gptel-send))

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

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil))

;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(use-package re-builder
  :commands re-builder
  :custom
  (reb-re-syntax 'string))

(use-package term
  :commands term
  :unless (not (file-exists-p "/bin/zsh")) ; we only use it if shell exists
  :custom
  (shell-file-name "/bin/zsh")
  (explicit-shell-file-name "/bin/zsh"))

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

(use-package vundo
  :bind
  (:map my/open-map
        ("u". vundo))
  :config
  (when (display-graphic-p)
    (setq vundo-glyph-alist vundo-unicode-symbols)))

(provide 'my-tools)
;;; my-tools.el ends here