;;; my-ai.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2025 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2025-11-27
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/karthink/gptel.git][gptel]]
;; A simple LLM client for Emacs.

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  :bind
  (:map my/open-map
        ("g". gptel))
  :commands (gptel gptel-send)
  :config
  (gptel-make-gemini "Gemini" :key #'gptel-api-key-from-auth-source :stream t)
  ;; OpenRouter offers an OpenAI compatible API
  (gptel-make-openai "OpenRouter"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(anthropic/claude-sonnet-4
              anthropic/claude-opus-4
              anthropic/claude-opus-4.1
              anthropic/claude-3.5-haiku
              deepseek/deepseek-chat-v3-0324:free
              openai/gpt-5
              openai/gpt-5-mini
              openai/gpt-4.1
              openai/gpt-4.1-mini
              openai/gpt-4o-mini
              openai/gpt-oss-120b
              openai/gpt-4o
              openai/chatgpt-4o-latest
              google/gemini-2.5-pro
              google/gemini-2.5-flash
              google/gemini-2.5-flash-lite
              google/gemini-2.0-flash-exp:free
              google/gemini-flash-1.5-8b
              google/gemini-pro-1.5
              google/gemini-flash-1.5)))

;; [[https://github.com/tttuuu888/inf-gptel.git][inf-gptel]]
;; Interactive Gptel shell for Emacs.

(use-package inf-gptel
  :ensure (:host github :repo "tttuuu888/inf-gptel")
  :bind
  (:map my/open-map
        ("G" . inf-gptel)))

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
