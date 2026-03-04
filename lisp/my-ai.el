;;; my-ai.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-03-04
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; agante-shell

(use-package agent-shell
  :ensure-system-package
  ;; Add agent installation configs here
  ((npm . "sudo dnf install -y npm")
   (claude . "curl -fsSL https://claude.ai/install.sh | bash")
   (claude-agent-acp . "sudo npm install -g @zed-industries/claude-agent-acp")))

;; [[https://github.com/karthink/gptel.git][gptel]]
;; A simple LLM client for Emacs.

(use-package gptel

  :preface
  (defun my/gptel-save-to-denote ()
    "Save the current gptel buffer as a Denote note in the gptel/ subdirectory."
    (interactive)
    (let ((denote-use-title (buffer-name))
          (denote-use-keywords '("gptel" "ai"))
          (denote-use-directory (expand-file-name "gptel/" denote-directory)))
      (call-interactively #'my/denote-save-buffer)))
  (defun my/gptel-auto-save-to-denote-h (&rest _)
    "Hook to automatically save gptel buffers to Denote.
Triggered after the first response is received in a new buffer."
    (my/gptel-save-to-denote)
    (remove-hook 'gptel-post-response-functions #'my/gptel-auto-save-to-denote-h t))
  :commands (gptel gptel-send)
  :bind
  (:map my/open-map
        ("g". gptel))
  :hook
  (gptel-mode . (lambda () (add-hook 'gptel-post-response-functions #'my/gptel-auto-save-to-denote-h nil t)))
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-make-gemini "Gemini" :key #'gptel-api-key-from-auth-source :stream t)
  (setq gptel-model 'google/gemini-3-flash-preview ;; default model to select
        gptel-backend (gptel-make-openai "OpenRouter"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key #'gptel-api-key-from-auth-source
                        :models '(anthropic/claude-haiku-4.5
                                  anthropic/claude-opus-4.6
                                  anthropic/claude-sonnet-4-6
                                  deepseek/deepseek-chat-v3.1
                                  deepseek/deepseek-v3.2
                                  deepseek/deepseek-v3.2-speciale
                                  google/gemini-2.5-flash
                                  google/gemini-2.5-flash-lite
                                  google/gemini-2.5-pro
                                  google/gemini-3-flash-preview
                                  google/gemini-3.1-pro-preview
                                  google/gemini-flash-1.5
                                  google/gemini-pro-1.5
                                  minimax/minimax-m2.5
                                  openai/chatgpt-4o-latest
                                  openai/gpt-4.1
                                  openai/gpt-4.1-mini
                                  openai/gpt-4o
                                  openai/gpt-4o-mini
                                  openai/gpt-5
                                  openai/gpt-5-mini
                                  openai/gpt-oss-120b))))

;; [[https://github.com/karthink/gptel-agent.git][gptel-agent]]
;; Agent mode for gptel.

(use-package gptel-agent
  :demand t
  :after gptel
  ;; Read files from agents directories
  :config (gptel-agent-update))

;; [[https://github.com/kmontag/macher.git][macher]]

(use-package macher
  :ensure (:host github :repo "kmontag/macher")
  :demand t
  :after gptel
  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'org)
  :config
  ;; Adjust buffer positioning to taste.
  (with-eval-after-load 'auto-side-windows
    (add-to-list 'auto-side-windows-top-buffer-names "\\*macher:.*\\*")
    (add-to-list 'auto-side-windows-right-buffer-names "\\*macher-patch:.*\\*"))
  :init
  (macher-install))

;; Library Footer

(provide 'my-ai)
;;; my-ai.el ends here
