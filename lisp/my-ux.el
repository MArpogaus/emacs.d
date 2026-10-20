;;; my-ux.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file.  DO NOT EDIT.

;;; Code:

;; [[https://github.com/emacscollective/auto-compile.git][auto-compile]]
;; Automatically compile outdated Emacs Lisp libraries.

(use-package auto-compile
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  :hook
  ((elpaca-after-init . auto-compile-on-load-mode)
   (emacs-lisp-mode . auto-compile-mode)))

;; autorevert :build_in:
;; Revert buffers when the underlying file has changed.

(use-package autorevert
  :ensure nil
  :custom
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  ;; Get notified by the system instead of polling.
  (auto-revert-avoid-polling t)
  :hook
  (elpaca-after-init . global-auto-revert-mode))

;; [[https://github.com/MArpogaus/auto-side-windows.git][auto-side-window]]

(use-package auto-side-windows
  :ensure (:host github :repo "MArpogaus/auto-side-windows")
  :preface
  (defun my/get-header-line-icon-for-buffer (buffer)
    (with-current-buffer buffer
      (unless (boundp 'header-line-icon)
        (setq-local header-line-icon
                    (cond
                     ((buffer-match-p (lambda (buffer) (with-current-buffer buffer (or (bound-and-true-p gptel-mode)
                                                                                       (bound-and-true-p agent-shell-ui-mode))))
                                      buffer)
                      '("AI" . mode-line-emphasis))
                     ((buffer-match-p "Warning" buffer)
                      '("WARN" . warning))
                     ((buffer-match-p '(or "^\\*Backtrace\\*$" ".*[Ee]rror.*") buffer)
                      '("ERR" . error))
                     ((buffer-match-p '(or "^COMMIT_EDITMSG$" "^\\*diff-hl\\*$" (derived-mode . magit-mode)) buffer)
                      '("VC" . success))
                     ((buffer-match-p '(derived-mode . dired-mode) buffer)
                      '("DIR" . mode-line-emphasis))
                     ((buffer-match-p "^\\*Org Src.*\\*" buffer)
                      '("SRC" . mode-line-emphasis))
                     ((buffer-match-p "^\\*Python.*\\*" buffer)
                      (cons (my/glyph "󰌠 " "PY")
                            'default))
                     ((buffer-match-p '(or (derived-mode . shell-mode)
                                           (derived-mode . comint-mode)
                                           (derived-mode . term-mode)
                                           (derived-mode . vterm-mode)) buffer)
                      (cons (my/glyph " " "TERM")
                            'default))
                     ((buffer-match-p "^\\*Org Agenda\\*$" buffer)
                      '("AGENDA" . mode-line-emphasis))
                     (t '("?" . mode-line-inactive)))))
      header-line-icon))
  (defvar my/side-window-drag-map
    (let ((map (make-sparse-keymap)))
      (define-key map [header-line down-mouse-1]
                  #'auto-side-windows-drag-slot)
      map)
    "Keymap that turns part of a header line into a drag handle.
A keymap on the text takes the press before `mouse-drag-header-line'.")
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
    "Face of the header row of a side window.
`my/header-line-format-top' remaps the header line faces to it.  Keep
it small: its attributes outrank the overline of window-box.")

  (defun my/panel-label ()
    "Return the name a panel header shows for the current buffer.
A dired buffer shows its directory, relative to the project root if
any; its own header line stretches to the right edge of the window."
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


  (defvar my/header-line-format-top
    '(:eval
      (pcase-let* ((`(,prefix . ,icon)
                    (my/get-header-line-icon-for-buffer (current-buffer)))
                   (row (if (mode-line-window-selected-p)
                            'header-line-active
                          'header-line-inactive))
                   ;; A terminal leaves colors unspecified: nil is no attribute.
                   (bg (face-background icon nil 'default))
                   (fg (face-foreground icon))
                   (face (append (list :inherit (list 'my/panel-header-line row))
                                 (and bg (list :background bg))
                                 (and fg (list :foreground fg))))
                   (own (my/panel-label))
                   ;; Padding of an exact width, and the row's height.
                   (pad (and (display-graphic-p)
                             (my/get-bar-image my/tabline-height (/ my/tabline-height 3)
                                               (face-foreground icon))))
                   (badge (if pad
                              (concat pad prefix pad)
                            prefix))
                   (buttons
                    (propertize
                     (concat (my/side-window-button
                              ;; One hairline family; the plain `✕' is heavier than `─'.
                              (my/glyph "\uEABA" "─" "hide")
                              "Hide this side window"
                              #'my/side-window-minimize)
                             " "
                             (my/side-window-button
                              (my/glyph "\uEA76" "✕" "close")
                              "Close this side window and kill its buffers"
                              #'my/side-window-kill))
                     'face face)))
        ;; The whole row is the panel's, so the faces carry the look.
        ;; Asked of the remaps, not of a flag: an agenda redo kills
        ;; local variables but keeps the remaps.
        (unless (member '(:inherit my/panel-header-line)
                        (assq 'header-line face-remapping-alist))
          (dolist (name '(header-line header-line-active
                                      header-line-inactive))
            (face-remap-add-relative name '(:inherit my/panel-header-line))))
        (setq-local window-box-color (face-foreground icon))
        ;; Content only: window-box owns the ends of the row.
        (concat
         (propertize badge 'face (append '(:inverse-video t) face)
                     'local-map my/side-window-drag-map
                     'help-echo "Drag to another slot")
         ;; The remap misses a buffer's own header faces (`magit-header-line').
         (propertize (concat " " own) 'face face)
         ;; window-box moves `right' past the buffer's margin.
         (propertize " " 'display `(space :align-to
                                          (- right ,(string-width buttons))))
         buttons))))

  :custom
  ;; If no rule applies, try these strategies to display new buffers
  (display-buffer-base-action '((display-buffer-in-previous-window
                                 display-buffer-reuse-mode-window
                                 display-buffer-use-some-window
                                 display-buffer-pop-up-window)))
  ;; Respects display actions when switching buffers
  (switch-to-buffer-obey-display-actions t)

  ;; Xref targets go to an ordinary window, never to a panel.
  (display-buffer-alist
   '(((category . xref-jump)
      (display-buffer-reuse-window display-buffer-use-some-window)
      (some-window . mru))))

  ;; Reuse on the left: dirvish kills the window of the old listing.
  (auto-side-windows-reuse-mode-window '((left . t)))

  ;; Panels: no `other-window', no tab line, no mode line.
  (auto-side-windows-common-window-parameters `((no-other-window . t)
                                                (tab-line-format . none)
                                                (mode-line-format . none)
                                                (header-line-format . ,my/header-line-format-top)))

  ;; A side keeps the size I give it, per tab.
  (auto-side-windows-remember-sizes t)

  ;; How wide the right side starts.
  (auto-side-windows-right-width 80)

  ;; Top side window configurations
  (auto-side-windows-top-height 15)
  (auto-side-windows-top-buffer-names
   '("^CAPTURE-.*\\.org$"
     "^COMMIT_EDITMSG$"
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
     term-mode
     vterm-mode
     comint-mode))

  ;; Left side window configurations
  (auto-side-windows-left-buffer-names
   '("^ \\*SIDE ::"))
  (auto-side-windows-left-buffer-modes
   '(reftex-toc-mode
     symbols-outline-mode))

  ;; Right side window configurations
  (auto-side-windows-right-buffer-names
   '("^\\*Org Agenda\\*$"
     "^\\*Outline .+\.pdf\\*$"
     "^\\*eldoc.*\\*$"
     "^\\*Org Src.*\\*"
     "^\\*Metahelp\\*$"
     ;; overblock's pop-out and edit buffers
     "^\\*overblock-[a-z]+\\(?: md\\)?: .*\\*$"))
  (auto-side-windows-right-buffer-modes
   '(Info-mode
     TeX-output-mode
     pdf-view-mode
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

  (window-combination-resize t)
  (window-sides-vertical t)
  (window-sides-slots '(2 1 5 2)) ; maximum number of side windows on the left, top, right and bottom
  (window-persistent-parameters
   (append window-persistent-parameters
           '((tab-line-format . t)
             (header-line-format . t)
             (mode-line-format . t))))
  (org-src-window-setup 'plain)
  ;; auto-side-windows places the agenda; quit only kills the buffer and
  ;; deletes no window, which fails beside side windows.
  (org-agenda-window-setup 'current-window)
  (dape-buffer-window-arrangement 'right)
  (symbols-outline-window-position 'left)
  (minimap-window-location 'right)
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-diff-inhibit-same-window t)
  (transient-display-buffer-action
   '(auto-side-windows--display-buffer
     (auto-side-windows-side . top)))
  (transient-mode-line-format nil)
  (gptel-display-buffer-action
   '(auto-side-windows--display-buffer
     (auto-side-windows-side . right)
     (body-function . select-window)))
  (agent-shell-display-action
   '((auto-side-windows--display-buffer)
     .
     ((auto-side-windows-side . right)
      (body-function . select-window))))
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
  :hook elpaca-after-init)

;; bookmark :build_in:

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1))

;; [[https://github.com/emacs-straight/comint-mime.git][comint-mime]]
;; Display graphics and other special content in REPLs (comint buffers).

(use-package comint-mime
  :hook
  (inferior-python-mode . comint-mime-setup))

;; delsel :build_in:
;; Replace selected text when typing.

(use-package delsel
  :ensure nil
  :hook
  (elpaca-after-init . delete-selection-mode))

;; elec-pair :build_in:
;; Automatically add closing parentheses, quotes, etc.

(use-package elec-pair
  :ensure nil
  :hook
  (elpaca-after-init . electric-pair-mode))

;; face-remap :build_in:
;; Keybindings and optimizations for text-scale-mode.
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4278C1-L4303C64
;; https://karthinks.com/software/scaling-latex-previews-in-emacs/

(use-package face-remap
  :ensure nil
  :preface
  (defvar my/buffer-scale-map (make-sparse-keymap) "key-map for buffer text scale commands")

  (defun my/text-scale-adjust-latex-previews ()
    "Scale the LaTeX previews of the buffer with its text scale."
    (pcase major-mode
      ((guard (derived-mode-p 'LaTeX-mode))
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
    "Scale the image of preview overlay OV like the text.
An opened preview shows its source and has no image to scale."
    (when (eq (car-safe (overlay-get ov 'display)) 'image)
      (overlay-put
       ov 'display
       (cons 'image
             (plist-put
              (cdr (overlay-get ov 'display))
              :scale (expt text-scale-mode-step text-scale-mode-amount))))))
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
  (defun my/mc-quit-on-click (&rest _)
    "Leave multiple cursors on a click, if they are on."
    (when (bound-and-true-p multiple-cursors-mode)
      (mc/disable-multiple-cursors-mode)))
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
  (advice-add 'mouse-set-point :after #'my/mc-quit-on-click))

;; outline :build_in:
;; Fold and transform headings. Org mode uses outline for its headlines.

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
  ;; outline's own editing repeat map claims these commands when it loads.
  (my/repeatize-keymap 'my/navigation-map)
  :bind
  (:repeat-map my/navigation-map
               ("<" . outline-promote)
               (">" . outline-demote)
               ("P" . outline-move-subtree-up)
               ("N" . outline-move-subtree-down)
   :repeat-map my/outline-repeat-map
               ("SPC"         . outline-mark-subtree)
               ("TAB"         . outline-cycle)
               ("S-<tab>"     . outline-cycle-buffer)
               ("<backtab>"   . outline-cycle-buffer)
               ("a"           . outline-show-all))
  :hook
  ((text-mode prog-mode conf-mode) . my/outline-minor-mode))

;; [[https://github.com/jamescherti/outline-indent.el.git][outline-indent]]

(use-package outline-indent
  :hook
  ((yaml-ts-mode python-base-mode) . outline-indent-minor-mode))

;; paren :build_in:
;; Highlight matching parentheses.


(use-package paren
  :ensure nil
  :custom
  (show-paren-when-point-in-periphery t))

;; recentf :build_in:

;; Keep a list of recently opened files.


(use-package recentf
  :ensure nil
  ;; Something loads it before the package queue reaches no-littering.
  :after no-littering
  :demand t
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
;; Enable repeat maps.

(use-package repeat
  :ensure nil
  :preface
  ;; https://karthinks.com/software/it-bears-repeating/#adding-repeat-mode-support-to-keymaps
  (defun my/repeatize-keymap (keymap &optional unset via)
    "Add `repeat-mode' support to a KEYMAP. If UNSET is true remove repeat property.
With VIA, the commands name the variable VIA instead of KEYMAP, so a
buffer-local VIA picks the map per buffer."
    (map-keymap
     (lambda (_key cmd)
       (when (symbolp cmd)
         (put cmd 'repeat-map (unless unset (or via keymap)))))
     (symbol-value keymap)))
  ;; Both notebooks share the overblock-run commands; each buffer sets its map.
  (defvar-local my/notebook-repeat-map nil
    "The repeat map of the notebook in this buffer.")
  (defun my/repeat-echo-which-key (keymap)
    "Show the repeat keys of KEYMAP in which-key."
    (if keymap
        ;; Not from here: the popup would be drawn inside
        ;; `post-command-hook' and taken away again right after.
        (run-at-time 0 nil #'which-key--create-buffer-and-show nil keymap)
      (which-key--hide-popup)))
  :custom
  (repeat-echo-function #'my/repeat-echo-which-key)
  :hook elpaca-after-init)

;; savehist :build_in:

(use-package savehist
  :ensure nil
  :custom
  (savehist-additional-variables
   ;; Minibuffer histories are saved anyway.
   '(command-history
     kill-ring
     regexp-search-ring
     search-ring))
  :config
  (put 'set-variable-value-history 'history-length 250)
  (put 'custom-variable-history    'history-length 250)
  (put 'query-replace-history      'history-length 250)
  (put 'read-expression-history    'history-length 250)
  (put 'read-char-history          'history-length 250)
  (put 'face-name-history          'history-length 250)
  (put 'bookmark-history           'history-length 250)
  :hook
  ;; Start history mode.
  elpaca-after-init)

;; saveplace :build_in:
;; Record cursor position from one session to the other.

(use-package saveplace
  :ensure nil
  :custom
  ;; The places are saved on a timer, not only when Emacs exits.
  (save-place-autosave-interval 300)
  :hook
  (elpaca-after-init . save-place-mode))

;; time-stamp :build_in:
;; Automatically update file timestamps when file is saved.

(use-package time-stamp
  :ensure nil
  :custom
  (time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S (%L)")
  :hook
  (before-save . time-stamp))

;; ultra-scroll

(use-package ultra-scroll
  :custom
  (pixel-scroll-precision-interpolate-mice nil) ; Disable interpolation (causes weird jumps)
  :hook elpaca-after-init)

;; [[https://github.com/mhayashi1120/Emacs-wgrep.git][wgrep]]
;; Edit grep buffers and apply the changes to files.

(use-package wgrep
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
  ;; Killing a buffer restores the window from before it was shown.
  (kill-buffer-quit-windows t)
  ;; And `q' kills the buffers that are worth nothing once read.
  (quit-window-kill-buffer '(help-mode helpful-mode compilation-mode grep-mode))
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
   ("v" . split-window-vertically)
   ("h" . split-window-horizontally)
   ("m" . delete-other-windows)
   ("M" . delete-other-windows-vertically)
   ("R" . window-layout-rotate-clockwise)
   ("W" . rotate-windows)
   :exit
   ("=" . balance-windows)))

;; [[https://github.com/MArpogaus/window-box][window-box]] :own:
;; Draw a box around side windows, so they read as panels.
;; The box adds only the edges the window does not have.

(use-package window-box
  :ensure (:host github :repo "MArpogaus/window-box")
  :custom
  ;; Header and mode line inside the box, tab line outside.
  (window-box-enclose-top 'header-line)
  (window-box-enclose-mode-line t)
  ;; Box side windows, whatever buffer they show.
  (window-box-window-predicate
   (lambda (window)
     (window-parameter window 'window-side)))
  :hook
  (elpaca-after-init . global-window-box-mode))

;; [[https://github.com/joostkremers/writeroom-mode.git][writeroom-mode]]
;; Distraction-free writing for Emacs.

(use-package writeroom-mode
  :config
  (setopt writeroom-global-effects (append writeroom-global-effects '(my/minimal-ui-mode)))
  :bind (:map my/toggle-map ("z" . writeroom-mode)))

;; ZZ Library Footer

(provide 'my-ux)
;;; my-ux.el ends here
