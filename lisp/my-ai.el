;;; my-ai.el --- Emacs configuration file  -*- no-byte-compile: t; no-native-compile: t; lexical-binding: t; -*-
;; Copyright (C) 2023-2026 Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Created: 2026-09-09
;; Keywords: configuration
;; Homepage: https://github.com/MArpogaus/emacs.d/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from emacs.org file. DO NOT EDIT.

;;; Code:

;; [[https://github.com/xenodium/agent-shell.git][agent-shell]]
;; A native Emacs buffer to interact with LLM agents powered by ACP.

(use-package agent-shell
  :preface
  (defvar my/gtd-prompts
    '(("Tag"   . "Tagesreview nach AGENTS.md.")
      ("Inbox" . "Inbox-Review nach AGENTS.md.")
      ("Woche" . "Wochenreview nach AGENTS.md."))
    "Prompts for `my/gtd'; the first one starts a new session.
The rules of each review are in the AGENTS.md of the agenda.")
  (defun my/gtd-refresh-agenda (&rest _)
    "Revert the agenda files the agent changed and redo the agenda."
    (dolist (file (org-agenda-files))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (unless (verify-visited-file-modtime buffer)
            (revert-buffer t t t)))))
    (when-let* ((window (get-buffer-window org-agenda-buffer-name t)))
      (with-selected-window window (org-agenda-redo-all))))
  (defun my/gtd (&optional prompt)
    "Show the GTD agenda beside a Claude agent in the agenda directory.
A new agent gets the first of `my/gtd-prompts'; with a prefix argument,
pick PROMPT, which also goes to an agent that runs already."
    (interactive
     (list (when current-prefix-arg
             (cdr (assoc (completing-read "Review: " my/gtd-prompts nil t)
                         my/gtd-prompts)))))
    (require 'agent-shell)
    (let* ((dir (expand-file-name "agenda/" org-directory))
           (shell (seq-find (lambda (buffer)
                              (file-equal-p (buffer-local-value 'default-directory buffer) dir))
                            (agent-shell-buffers))))
      ;; In a main window: shown the usual way, auto-side-windows puts
      ;; the agenda in a panel.
      (save-window-excursion (org-agenda nil "g"))
      (let ((main (seq-find (lambda (window) (not (window-parameter window 'window-side)))
                            (window-list nil 'no-mini))))
        (set-window-buffer main org-agenda-buffer-name)
        (select-window main))
      (if shell
          (pop-to-buffer shell agent-shell-display-action)
        (let ((default-directory dir)
              (agent-shell-cwd-function (lambda () dir)))
          (setq shell (agent-shell-start :config (agent-shell-anthropic-make-claude-code-config))))
        (agent-shell-subscribe-to :shell-buffer shell :event 'turn-complete
                                  :on-event #'my/gtd-refresh-agenda)
        (setq prompt (or prompt (cdar my/gtd-prompts))))
      (when prompt
        (agent-shell-insert :text prompt :submit t :shell-buffer shell))))
  :custom
  (agent-shell-header-style 'text)
  :bind
  (:map my/ai-map
        ("s" . agent-shell)
        :map my/open-map
        ("g" . my/gtd)))

;; [[https://github.com/karthink/gptel.git][gptel]]
;; A simple LLM client for Emacs.

(use-package gptel
  :preface
  (defun my/gptel-save-to-denote ()
    "Save the current gptel buffer as a Denote note in the gptel/ subdirectory."
    (interactive)
    (with-current-buffer (current-buffer)
      (let ((previous-dir default-directory)
            (denote-use-title (buffer-name))
            (denote-use-keywords '("gptel" "ai"))
            (denote-use-directory (expand-file-name "gptel/" (car (denote-directories)))))
        (add-file-local-variable 'default-directory previous-dir t)
        (add-file-local-variable 'auto-side-windows-side 'right t)
        (add-file-local-variable 'eval '(gptel-mode) t)
        (call-interactively #'my/denote-save-buffer)
        (setq-local default-directory previous-dir))))
  (defun my/gptel-auto-save-to-denote-h (&rest _)
    "Hook to automatically save gptel buffers to Denote.
Triggered after the first response is received in a new buffer."
    (my/gptel-save-to-denote)
    (remove-hook 'gptel-post-response-functions #'my/gptel-auto-save-to-denote-h t))
  :init
  ;; Saved chats turn `gptel-mode' on through an `eval' local variable.
  (add-to-list 'safe-local-eval-forms '(gptel-mode))
  :commands (gptel gptel-send)
  :bind
  (:map my/ai-map
        ("a". gptel)
        ("c". gptel-add)
        ("m". gptel-menu))
  :hook
  (gptel-mode . (lambda () (add-hook 'gptel-post-response-functions #'my/gptel-auto-save-to-denote-h nil t)))
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-make-gemini "Gemini" :key #'gptel-api-key-from-auth-source :stream t)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '((anthropic/claude-haiku-4.5
               :description "Fast and affordable for light tasks."
               :input-cost 1.00
               :output-cost 5.00
               :context-window 200)
              (anthropic/claude-opus-4.6
               :description "Powerful for complex reasoning."
               :input-cost 5.00
               :output-cost 25.00
               :context-window 1000)
              (anthropic/claude-sonnet-4-6
               :description "Balanced for enterprise AI deployments."
               :input-cost 3.00
               :output-cost 15.00
               :context-window 1000)
              (deepseek/deepseek-v3.2
               :description "General-purpose with large context."
               :input-cost 0.32
               :output-cost 0.89
               :context-window 164)
              (google/gemini-2.5-flash
               :description "Best price/performance, well-rounded."
               :input-cost 0.30
               :output-cost 2.50
               :context-window 1048)
              (google/gemini-2.5-flash-lite
               :description "Fastest & cheapest 2.5, high-volume, latency-sensitive."
               :input-cost 0.10
               :output-cost 0.40
               :context-window 1048)
              (google/gemini-2.5-pro
               :description "Most powerful Gemini, state-of-the-art performance."
               :input-cost 1.25
               :output-cost 10.00
               :context-window 1048)
              (google/gemini-3.1-flash-lite-preview
               :description "Most intelligent Gemini model built for speed."
               :input-cost 0.50
               :output-cost 3.00
               :context-window 1048)
              (google/gemini-3.1-pro-preview
               :description "Most intelligent Gemini with SOTA reasoning and multimodal understanding."
               :input-cost 2.00
               :output-cost 12.00
               :context-window 1048)
              (google/gemini-flash-1.5
               :description "Best price/performance, well-rounded capabilities."
               :input-cost 0.30
               :output-cost 2.50
               :context-window 1048)
              (google/gemini-pro-1.5
               :description "Most powerful Gemini thinking model, always points to latest version."
               :input-cost 1.25
               :output-cost 10.00
               :context-window 1048)
              (openai/gpt-4.1
               :description "Flagship model for complex tasks."
               :input-cost 2.00
               :output-cost 8.00
               :context-window 1024)
              (openai/gpt-4.1-mini
               :description "Balance intelligence, speed, and cost."
               :input-cost 0.40
               :output-cost 1.60
               :context-window 1024)
              (openai/gpt-4.1-nano
               :description "Fastest, most cost-effective GPT-4.1."
               :input-cost 0.10
               :output-cost 0.40
               :context-window 1024)
              (openai/gpt-5.3-chat
               :description "Flagship for coding, reasoning, agentic tasks."
               :input-cost 1.25
               :output-cost 10.00
               :context-window 400)
              (openai/gpt-5.3-codex
               :description "Flagship for coding, reasoning, agentic tasks."
               :input-cost 1.25
               :output-cost 10.00
               :context-window 400)
              (openai/o3
               :description "Well-rounded and powerful across domains."
               :input-cost 2.00
               :output-cost 8.00
               :context-window 200)
              (openai/o3-deep-research
               :description "Well-rounded and powerful, optimized for deep research."
               :input-cost 2.00
               :output-cost 8.00
               :context-window 200)
              (openai/o3-pro
               :description "Well-rounded and powerful, professional-grade capabilities."
               :input-cost 2.00
               :output-cost 8.00
               :context-window 200)
              (openai/o4-mini
               :description "Fast, effective reasoning for coding and visual tasks."
               :input-cost 1.10
               :output-cost 4.40
               :context-window 200)
              (openai/o4-mini-deep-research
               :description "Fast, effective reasoning for coding and visual tasks, optimized for deep research."
               :input-cost 1.10
               :output-cost 4.40
               :context-window 200)))
  (setq gptel-model 'arcee-ai/trinity-large-preview:free ;; default model to select
        gptel-backend (gptel-make-openai "OpenRouter (free)"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key #'gptel-api-key-from-auth-source
                        :models '((arcee-ai/trinity-large-preview:free
                                   :description "Free, large, and powerful Arcee AI model."
                                   :input-cost 0.00
                                   :output-cost 0.00
                                   :context-window 131)
                                  (google/gemma-3-27b-it:free
                                   :description "Free instruction-tuned Gemma model (27B parameters)."
                                   :input-cost 0.00
                                   :output-cost 0.00
                                   :context-window 131)
                                  (google/gemma-3-12b-it:free
                                   :description "Free instruction-tuned Gemma model (12B parameters)."
                                   :input-cost 0.00
                                   :output-cost 0.00
                                   :context-window 33)
                                  (qwen/qwen3-coder:free
                                   :description "Free Qwen3 model optimized for coding."
                                   :input-cost 0.00
                                   :output-cost 0.00
                                   :context-window 262)
                                  (qwen/qwen3-next-80b-a3b-instruct:free
                                   :description "Free, large instruction-tuned Qwen3 model (80B parameters)."
                                   :input-cost 0.00
                                   :output-cost 0.00
                                   :context-window 262)
                                  (stepfun/step-3.5-flash:free
                                   :description "Free Stepfun 3.5 Flash model."
                                   :input-cost 0.00
                                   :output-cost 0.00
                                   :context-window 256)))))

;; [[https://github.com/karthink/gptel-agent.git][gptel-agent]]
;; Agent mode for gptel.

(use-package gptel-agent
  :bind
  (:map my/ai-map
        ("A". gptel-agent))
  ;; Read files from agents directories
  :preface
  (with-eval-after-load 'gptel
    (gptel-agent-update)))

;; ZZ Library Footer

(provide 'my-ai)
;;; my-ai.el ends here
