;;; my-ux.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-03
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/emacscollective/auto-compile.git][auto-compile]]
;; Automatically compile outdated Emacs Lisp libraries.

(use-package auto-compile
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  :init
  (auto-compile-on-load-mode)
  :hook
  (emacs-lisp-mode . auto-compile-on-save-mode))

;; autorevert :build_in:
;; Revert buffers when the underlying file has changed

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose t)
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  ;; Avoid polling for changes and rathe get notified by the system
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list "."))
  :hook
  (elpaca-after-init . global-auto-revert-mode))

;; [[https://github.com/MArpogaus/window-box][window-box]] :own:
;; A rectangular box around a window: side windows read as panels.  The
;; mode line and the header line stay as configured; the box only adds
;; the edges the window does not have.

(use-package window-box
  :ensure (:host github :repo "MArpogaus/window-box")
  :custom
  ;; The header line and the mode line inside the box, a tab line
  ;; outside it: `window-box-encloses' was one set of rows and is now
  ;; these two, so that the inside cannot be asked for in pieces.
  (window-box-enclose-top 'header-line)
  (window-box-enclose-mode-line t)
  ;; The mode belongs to the buffer, and Emacs can show one buffer in a
  ;; panel and in an ordinary window at the same time.  A box belongs to
  ;; the place: the side windows wear it and the ordinary windows do not.
  (window-box-window-predicate
   (lambda (window)
     (window-parameter window 'window-side))))

;; [[https://github.com/MArpogaus/auto-side-windows.git][auto-side-window]]

(use-package auto-side-windows
  :preface
  (defun my/get-header-line-icon-for-buffer (buffer)
    (with-current-buffer buffer
      (unless (boundp 'header-line-icon)
        (setq-local header-line-icon
                    (cond
                     ((buffer-match-p (lambda (buffer) (with-current-buffer buffer (or (bound-and-true-p gptel-mode)
                                                                                       (bound-and-true-p agent-shell-ui-mode))))
                                      buffer)
                      '(" AI " . mode-line-emphasis))
                     ((buffer-match-p "Warning" buffer)
                      '(" ! " . warning))
                     ((buffer-match-p '(or "^\\*Backtrace\\*$" ".*[Ee]rror.*") buffer)
                      '(" ! " . error))
                     ((buffer-match-p '(or "^COMMIT_EDITMSG$" "^\\*diff-hl\\*$" (derived-mode . magit-mode)) buffer)
                      '(" VC " . success))
                     ((buffer-match-p '(derived-mode . dired-mode) buffer)
                      '(" DIR " . mode-line-emphasis))
                     ((buffer-match-p "^\\*Org Src.*\\*" buffer)
                      '(" SRC " . mode-line-emphasis))
                     ((buffer-match-p '(or (derived-mode . shell-mode)
                                           (derived-mode . comint-mode)
                                           (derived-mode . term-mode)
                                           (derived-mode . vterm-mode)) buffer)
                      '("   " . default))
                     ((buffer-match-p "^\\*Org Agenda\\*$" buffer)
                      '(" AGENDA " . mode-line-emphasis))
                     (t '(" ? " . mode-line-inactive)))))
      header-line-icon))
  (defvar my/side-window-drag-map
    (let ((map (make-sparse-keymap)))
      (define-key map [header-line down-mouse-1]
                  #'auto-side-windows-drag-slot)
      map)
    "Keymap that turns part of a header line into a drag handle.
  Emacs binds a press on a header line to `mouse-drag-header-line', which
  resizes the window, so the press is the event to take: a keymap on the
  text takes it first, and `auto-side-windows-drag-slot' follows the mouse
  from there.")
  (defun my/side-window-button (label help command)
    "Return LABEL as a header-line button running COMMAND on mouse-1."
    (propertize label 'mouse-face 'highlight 'help-echo help
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [header-line mouse-1] command)
                             map)))
  (defun my/side-window-minimize (event)
    "Delete the clicked side window; its buffers stay alive."
    (interactive "e")
    (delete-window (posn-window (event-start event))))
  (defun my/side-window-kill (event)
    "Delete the clicked side window and kill the buffers it displayed."
    (interactive "e")
    (let* ((win (posn-window (event-start event)))
           (bufs (delete-dups (cons (window-buffer win)
                                    (mapcar #'car (window-prev-buffers win))))))
      (dolist (buf bufs)
        ;; Keep buffers that are still visible somewhere else.
        (unless (length> (get-buffer-window-list buf nil t) 1)
          (kill-buffer buf)))
      (when (window-valid-p win)
        (delete-window win))))
  (defface my/panel-header-line
    '((t :inherit bold :underline nil))
    "The look of a side window's header row.
`my/header-line-format-top\=' remaps it over the header line faces, so
the whole row wears it and a mode's own header colours do not.  It
names as little as it can: window-box remaps those faces too, and what
is named here outranks its overline.")

  (defun my/panel-label ()
      "Return what a panel header calls the current buffer.
A dired or dirvish buffer is named by its directory: their own header
lines carry a stretch to the window's right edge, which took the whole
row and pushed the panel's buttons out of it.  The project's crumbs
where the directory is inside a project — the same ones the mode line
shows — and the path itself where it is not."
      (if (not (derived-mode-p 'dired-mode))
          (format-mode-line (or header-line-format "%b") nil nil
                            (current-buffer))
        (let* ((here (directory-file-name
                      (abbreviate-file-name default-directory)))
               (project (project-current nil default-directory))
               (root (and project
                          (directory-file-name
                           (abbreviate-file-name (project-root project))))))
          (cond ((null project) here)
                ((equal here root) (project-name project))
                (t (concat (project-name project) "/"
                           (file-relative-name here root)))))))

  (defvar-local my/header-line-faces-overloaded nil
    "Whether the remap below has been added to this buffer already.
A relative remap stacks, and redisplay comes round again.")

  (defvar my/header-line-format-top
    '(:eval
      (pcase-let* ((`(,prefix . ,icon)
                    (my/get-header-line-icon-for-buffer (current-buffer)))
                   (row (if (mode-line-window-selected-p)
                            'header-line-active
                          'header-line-inactive))
                   (face (list :inherit (list 'my/panel-header-line row)
                               :background (face-background icon nil 'default)
                               :foreground (face-foreground icon)))
                   (own (my/panel-label))
                   ;; Padding of an exact width, and the row's height.
                   (pad (and (display-graphic-p)
                             (my/get-bar-image my/modeline-height 6
                                               (face-foreground icon))))
                   (badge (if pad
                              (concat pad (string-trim prefix) pad)
                            prefix))
                   (buttons
                    (propertize
                     (concat (my/side-window-button
                              "─" "Hide this side window"
                              #'my/side-window-minimize)
                             " "
                             (my/side-window-button
                              "✕" "Close this side window and kill its buffers"
                              #'my/side-window-kill))
                     'face face)))
        ;; The blanks of the row are the panel's too, so the look goes
        ;; on the faces themselves.
        (unless my/header-line-faces-overloaded
          (setq-local my/header-line-faces-overloaded t)
          (dolist (name '(header-line header-line-active
                                      header-line-inactive))
            (face-remap-add-relative
             name (list :inherit 'my/panel-header-line
                        :background (face-background name nil 'default)
                        :foreground (face-foreground name nil 'default)))))
        ;; Content only: window-box owns the ends of the row.
        (concat
         (propertize badge 'face (append '(:inverse-video t) face)
                     'local-map my/side-window-drag-map
                     'help-echo "Drag to another slot")
         ;; The buffer's own header brings faces of its own —
         ;; `magit-header-line\=' for one — which the remap does not
         ;; reach: it names the header line faces, not theirs.
         (propertize (concat " " own) 'face face)
         (propertize " " 'display `(space :align-to
                                          (- right ,(string-width buttons))))
         buttons))))

  :custom
  ;; If non of our rules apply try the following strategies to dispaly new buffers
  (display-buffer-base-action '((display-buffer-in-previous-window
                                 display-buffer-reuse-mode-window
                                 display-buffer-use-some-window
                                 display-buffer-pop-up-window)
                                ;;. ((inhibit-same-window . t))
                                ))
  ;; Respects display actions when switching buffers
  (switch-to-buffer-obey-display-actions t)

  ;; A jump shows its target through `display-buffer' under the
  ;; `xref-jump' category.  The definition is what the jump is for, so it
  ;; lands in an ordinary window and never in a panel.
  (display-buffer-alist
   '(((category . xref-jump)
      (display-buffer-reuse-window display-buffer-use-some-window)
      (some-window . mru))))

  ;; Dont resue sidewindows if there are still free slots
  (auto-side-windows-reuse-mode-window nil)

  ;; The package ships no taste of its own, so the panels get theirs
  ;; here: a side window is not a place to land in with `other-window',
  ;; and it wears neither a tab line nor a mode line.
  (auto-side-windows-common-window-parameters '((no-other-window . t)
                                                (tab-line-format . none)
                                                (mode-line-format . none)))

  ;; A side keeps the size I give it, per tab.
  (auto-side-windows-remember-sizes t)

  ;; How wide the right side starts.
  (auto-side-windows-right-width 80)

  ;; Top side window configurations
  ;; The size of a side is its own option now; the alists carry the
  ;; rest of the action.
  (auto-side-windows-top-height 15)
  (auto-side-windows-top-buffer-names
   '("^COMMIT_EDITMSG$"
     "^\\*Agenda Commands\\*$"
     "^\\*Async-native-compile-log\\*$"
     "^\\*Backtrace\\*$"
     "^\\*Compile-Log\\*$"
     "^\\*Messages\\*$"
     "^\\*Multiple Choice Help\\*$"
     "^\\*Org Select\\*"
     "^\\*Org-Babel Error Output\\*"
     "^\\*Process List\\*$"
     "^\\*Quick Help\\*$"
     "^\\*TeX Help\\*$"
     "^\\*TeX errors\\*$"
     "^\\*Warnings\\*$"
     "^\\*diff-hl\\*$"
     "^\\*gptel-system\\*$"
     "^\\*jinx module compilation\\*$"
     "^ \\*Install vterm\\* $"))
  (auto-side-windows-top-buffer-modes
   '(compilation-mode
     flymake-diagnostics-buffer-mode
     grep-mode
     locate-mode
     occur-mode
     xref--xref-buffer-mode))

  ;; Bottom side window configurations
  (auto-side-windows-bottom-buffer-names
   '("^\\*eshell\\*"
     "^\\*shell\\*"
     "^\\*term\\*"
     "^\\*.*vterm\\*"))
  (auto-side-windows-bottom-buffer-modes
   '(eshell-mode
     shell-mode
     term-mode
     vterm-mode
     comint-mode
     debugger-mode))

  ;; Left side window configurations
  (auto-side-windows-left-buffer-names
   '("^\\*toc*\\*$"
     "^ \\*SIDE ::"))
  (auto-side-windows-left-buffer-modes
   '(reftex-toc-mode
     symbols-outline-mode))

  ;; Right side window configurations
  (auto-side-windows-right-buffer-names
   '("^\\*Org Agenda\\*$"
     "^\\*Outline .+\.pdf\\*$"
     "^\\*eldoc.*\\*$"
     "^\\*info\\*$"
     "^\\*Org Src.*\\*"
     "^magit-diff:.*$"
     "^magit-process:.*$"
     "^\\*Metahelp\\*$"))
  (auto-side-windows-right-buffer-modes
   '(Info-mode
     TeX-output-mode
     pdf-view-mode
     eldoc-mode
     elpaca-info-mode
     elpaca-log-mode
     help-mode
     helpful-mode
     magit-status-mode
     magit-log-mode
     magit-diff-mode
     magit-process-mode
     pdf-outline-buffer-mode
     shortdoc-mode))

  ;; Window parameters
  (auto-side-windows-top-window-parameters `((mode-line-format . none)
                                             (header-line-format . ,my/header-line-format-top)))
  (auto-side-windows-right-window-parameters `((mode-line-format . none)
                                               (header-line-format . ,my/header-line-format-top)))
  (auto-side-windows-left-window-parameters `((mode-line-format . none)
                                              (header-line-format . ,my/header-line-format-top)))
  (auto-side-windows-bottom-window-parameters `((mode-line-format . none)
                                                (header-line-format . ,my/header-line-format-top)))
  ;; The box goes on the buffer once it appears on a side, and
  ;; `window-box-window-predicate' keeps it to the side windows from
  ;; there: detaching the buffer into an ordinary window takes the box
  ;; away by itself, and putting it back on a side brings it back.  No
  ;; hook has to take the mode off again.
  (auto-side-windows-after-display-hook '((lambda (buffer &rest _)
                                            (with-current-buffer buffer
                                              (window-box-mode 1)))))
  (window-combination-resize t)
  (window-sides-vertical t)
  (window-sides-slots '(2 1 5 2)) ; maximum number of side windows on the left, top, right and bottom
  (window-persistent-parameters
   (append window-persistent-parameters
           '((tab-line-format . t)
             (header-line-format . t)
             (mode-line-format . t))))
  (org-src-window-setup 'plain)
  :bind
  (:map my/toggle-map
        ("w" .  window-toggle-side-windows)
        ("W" .  auto-side-windows-toggle-side-window)
        :map my/window-map
        ("s" . auto-side-windows-display-buffer-on-side)
        :map my/buffer-map
        ("B"  . auto-side-windows-switch-to-buffer)
        :repeat-map my/window-map
        ("N" . auto-side-windows-move-to-next-slot)
        ("P" . auto-side-windows-move-to-previous-slot))
  :config
  (with-eval-after-load 'magit
    (setopt magit-display-buffer-function #'display-buffer
            magit-commit-diff-inhibit-same-window t))
  (with-eval-after-load 'transient
    (setopt transient-display-buffer-action
            `(auto-side-windows--display-buffer
              (side . top))
            transient-mode-line-format nil))
  (with-eval-after-load 'gptel
    (setopt gptel-display-buffer-action
            '(auto-side-windows--display-buffer
              (side . right)
              (body-function . select-window))))
  (with-eval-after-load 'agent-shell
    (setopt agent-shell-display-action
            '((auto-side-windows--display-buffer)
              .
              ((side . right)
               (body-function . select-window)))))
  :hook
  (elpaca-after-init . auto-side-windows-mode))

;; bookmark :build_in:

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1))

;; [[https://github.com/emacs-straight/comint-mime.git][comint-mime]]
;; Mirror of the comint-mime package from GNU ELPA, current as of 2024-01-18.
;; Provides a mechanism for REPLs (or comint buffers, in Emacs parlance) to display graphics and other types of special content.

(use-package comint-mime
  :hook
  (inferior-python-mode . comint-mime-setup))

;; delsel :build_in:
;; Replace selected text when typing

(use-package delsel
  :ensure nil
  :hook
  ((prog-mode conf-mode text-mode) . delete-selection-mode))

;; elec-pair :build_in:
;; Automatically add closing parentheses, quotes, etc.

(use-package elec-pair
  :ensure nil
  :hook
  ((prog-mode conf-mode) . electric-pair-mode))

;; face-remap :build_in:
;; Keybindings and optimizations for text-scale-mode.
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4278C1-L4303C64
;; https://karthinks.com/software/scaling-latex-previews-in-emacs/

(use-package face-remap
  :ensure nil
  :preface
  (defvar my/buffer-scale-map (make-sparse-keymap) "key-map for buffer text scale commands")

  (defun my/text-scale-adjust-latex-previews ()
    "Adjust the size of latex preview fragments when changing the
buffer's text scale."
    (pcase major-mode
      ((or 'latex-mode (guard (bound-and-true-p org-auctex-mode)))
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'category)
                 'preview-overlay)
             (my/zoom-latex-preview ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'org-overlay-type)
                 'org-latex-overlay)
             (my/zoom-latex-preview ov))))))

  (defun my/zoom-latex-preview (ov)
    (overlay-put
     ov 'display
     (cons 'image
           (plist-put
            (cdr (overlay-get ov 'display))
            :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))
  :init
  (define-key my/buffer-map (kbd "z") (cons "scale" my/buffer-scale-map))
  :bind
  (:repeat-map my/buffer-scale-map
               ("+" . text-scale-increase)
               ("-" . text-scale-decrease)
               ("=" . text-scale-adjust))
  :hook
  (text-scale-mode . my/text-scale-adjust-latex-previews))

;; [[https://github.com/dengste/minimap.git][minimap]]
;; Sidebar showing a "mini-map" of a buffer.

(use-package minimap
  :custom
  (minimap-window-location 'right)
  (minimap-hide-fringes t)
  (minimap-minimum-width 25)
  (minimap-width-fraction 0)
  (minimap-major-modes '(prog-mode conf-mode))
  :bind
  (:map my/toggle-map
        ("m" . minimap-mode)))

;; [[https://github.com/magnars/multiple-cursors.el.git][multiple-cursors]]

(use-package multiple-cursors
  :preface
  (defvar my/mc-map (make-sparse-keymap) "key-map for multiple cursor commands")
  :init
  (define-key my/leader-map (kbd "m") (cons "mc" my/mc-map))
  :bind
  (("C-S-<mouse-1>" . mc/add-cursor-on-click)
   :map mc/keymap
   ("<escape>" . mc/keyboard-quit)
   :repeat-map my/mc-map
   ("n" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-like-this)
   :exit
   ("a" . mc/mark-all-like-this)
   ("m" . mc/edit-lines))
  :config
  (advice-add 'mouse-set-point :after (lambda (&rest _) (mc/disable-multiple-cursors-mode))))

;; outline :build_in:
;; Outline-mode helps to fold and transform headers. Org-mode itself uses outline-mode for its headlines.

(use-package outline
  :ensure nil
  :autoload outline-minor-mode-cycle--bind
  :preface
  (defvar my/outline-repeat-map (make-sparse-keymap) "key-map for outline-mode commands")
  (define-minor-mode my/outline-minor-mode
    "Customize `outline-minor-mode' for non-org buffers."
    :lighter nil
    (if my/outline-minor-mode
        (unless (eq major-mode 'org-mode)
          (outline-minor-mode-cycle--bind nil (kbd "M-<up>") #'outline-move-subtree-up)
          (outline-minor-mode-cycle--bind nil (kbd "M-<down>") #'outline-move-subtree-down)
          (outline-minor-mode-cycle--bind nil (kbd "M-<right>") #'outline-demote)
          (outline-minor-mode-cycle--bind nil (kbd "M-<left>") #'outline-promote)
          (setq-local outline-minor-mode-use-buttons 'in-margins)
          (outline-minor-mode 1)
          (reveal-mode 1))
      (progn
        (outline-minor-mode -1)
        (reveal-mode -1))))
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-highlight t)
  :init
  (define-key my/leader-map (kbd "TAB") (cons "outline" my/outline-repeat-map))
  :config
  (define-key my/outline-repeat-map (kbd "e") (cons "edit" outline-editing-repeat-map))
  (define-key my/outline-repeat-map (kbd "n") (cons "navigate" outline-navigation-repeat-map))
  :bind
  (:repeat-map my/outline-repeat-map
               ("SPC"         . outline-mark-subtree)
               ("TAB"         . outline-cycle)
               ("S-<tab>"     . outline-cycle-buffer)
               ("<backtab>"   . outline-cycle-buffer)
               ("a"           . outline-show-all))
  :hook
  ((text-mode prog-mode conf-mode) . my/outline-minor-mode))

;; [[https://github.com/jamescherti/outline-indent.el.git][outline-indent]]

(use-package outline-indent
  :ensure t
  :hook
  ((yaml-ts-mode) . outline-indent-minor-mode))

;; paren :build_in:
;; Paren mode for highlighting matcing paranthesis


(use-package paren
  :ensure nil
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren nil)
  :hook
  (prog-mode . show-paren-mode))

;; recentf :build_in:

;; 50 Recents files with some exclusion (regex patterns).


(use-package recentf
  :ensure nil
  :custom
  (recentf-keep '(file-remote-p file-readable-p))
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 100)
  ;; The list is saved on a timer, so a crash costs at most this much of it.
  (recentf-autosave-interval 300)
  (recentf-show-messages nil)
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (setq recentf-save-file-header
        ";;; Automatically generated by `recentf' on %s. -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*- \n")
  :bind
  (:map my/open-map
        ("r" . recentf-open))
  :hook elpaca-after-init)

;; repeat :build_in:
;; Enable repeat maps

(use-package repeat
  :ensure nil
  :preface
  ;; https://karthinks.com/software/it-bears-repeating/#adding-repeat-mode-support-to-keymaps
  (defun my/repeatize-keymap (keymap &optional unset)
    "Add `repeat-mode' support to a KEYMAP. If UNSET is true remove repeat property"
    (map-keymap
     (lambda (_key cmd)
       (when (symbolp cmd)
         (put cmd 'repeat-map (unless unset keymap))))
     (symbol-value keymap)))
  :config
  (with-eval-after-load 'which-key
    (setopt repeat-echo-function #'ignore)
    ;; Spawn or hide a which-key popup
    (advice-add 'repeat-post-hook :after
                (defun repeat-help--which-key-popup ()
                  (if-let* ((cmd (or this-command real-this-command))
                            (keymap (or repeat-map
                                        (repeat--command-property 'repeat-map))))
                      (run-at-time
                       0 nil
                       (lambda ()
                         (which-key--create-buffer-and-show
                          nil (symbol-value keymap))))
                    (which-key--hide-popup)))))
  :hook elpaca-after-init)

;; savehist :build_in:

(use-package savehist
  :ensure nil
  :custom
  (kill-ring-max 500)
  (history-length 500)
  (savehist-additional-variables
   '(bookmark-history
     command-history
     custom-variable-history
     face-name-history
     file-name-history
     kill-ring
     minibuffer-history
     query-replace-history
     read-char-history
     read-expression-history
     regexp-search-ring
     search-ring
     set-variable-value-history))
  ;; No duplicates in history
  (history-delete-duplicates t)
  :config
  (put 'minibuffer-history         'history-length 500)
  (put 'file-name-history          'history-length 500)
  (put 'set-variable-value-history 'history-length 250)
  (put 'custom-variable-history    'history-length 250)
  (put 'query-replace-history      'history-length 250)
  (put 'read-expression-history    'history-length 250)
  (put 'read-char-history          'history-length 250)
  (put 'face-name-history          'history-length 250)
  (put 'bookmark-history           'history-length 250)
  :hook
  ;;Start history mode.
  (elpaca-after-init . savehist-mode))

;; saveplace :build_in:
;; Record cursor position from one session to the other

(use-package saveplace
  :ensure nil
  :custom
  ;; The places are saved on a timer, not only when Emacs exits.
  (save-place-autosave-interval 300)
  :preface
  (defun my/saveplace-recenter (&rest _)
    (when buffer-file-name (ignore-errors (recenter))))
  :hook
  ((elpaca-after-init . save-place-mode)
   (save-place-after-find-file . my/saveplace-recenter)))

;; time-stamp :build_in:
;; Automatically update file timestamps when file is saved

(use-package time-stamp
  :ensure nil
  :custom
  (time-stamp-active t)
  (time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S (%U)")
  :hook
  (before-save . time-stamp))

;; [[https://github.com/mhayashi1120/Emacs-wgrep.git][wgrep]]
;; Writable grep buffer and apply the changes to files.

(use-package wgrep
  :demand t
  :custom
  (wgrep-auto-save-buffer t))

;; window :build_in:
;; Window sizes, splits and whole layouts: =R= rotates the layout and =W=
;; rotates the windows through it.

(use-package window
  :ensure nil
  :custom
  (window-resize-pixelwise t)   ; Resize windows pixelwise
  (frame-resize-pixelwise t)    ; Resize frame pixelwise
  ;; `kill-buffer' hands the window to `quit-restore-window', so killing
  ;; a buffer undoes the display that showed it instead of leaving the
  ;; window behind with something arbitrary in it.
  (kill-buffer-quit-windows t)
  ;; And `q' kills the buffers that are worth nothing once read.
  (quit-window-kill-buffer '(help-mode helpful-mode compilation-mode grep-mode))
  :config
  (define-key my/leader-map (kbd "w") (cons "window" my/window-map))
  :bind
  (("M-o" . other-window-prefix)
   ("M-t" . other-tab-prefix)
   ("M-f" . other-frame-prefix)
   :repeat-map my/window-map
   ("n" . next-window-any-frame)
   ("p" . previous-window-any-frame)
   ("k" . delete-window)
   ("K" . kill-buffer-and-window)
   ("+" . enlarge-window)
   ("-" . shrink-window)
   ("*" . enlarge-window-horizontally)
   ("_" . shrink-window-horizontally)
   ("r" . split-window-right)
   ("b" . split-window-below)
   ("v" . split-window-vertically)
   ("h" . split-window-horizontally)
   ("m" . delete-other-windows)
   ("M" . delete-other-windows-vertically)
   ("R" . window-layout-rotate-clockwise)
   ("W" . rotate-windows)
   :exit
   ("=" . balance-windows)))

;; [[https://github.com/joostkremers/writeroom-mode.git][writeroom-mode]]
;; Distraction-free writing for Emacs.

(use-package writeroom-mode
  :config
  (setopt writeroom-global-effects (append writeroom-global-effects '(my/minimal-ui-mode)))
  :bind (:map my/toggle-map ("z" . writeroom-mode)))

;; ZZ Library Footer

(provide 'my-ux)
;;; my-ux.el ends here
