;;; opencode-config.el --- Configuration for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Configuration module for OpenCode using Emacs customize groups.

;;; Code:

(defgroup opencode nil
  "OpenCode AI coding agent for Emacs."
  :prefix "opencode-"
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/anomalyco/opencode"))

(defcustom opencode-provider 'anthropic
  "LLM provider to use for AI requests.
Supported providers: 'anthropic, 'openai, 'openai-compatible."
  :type '(choice (const :tag "Anthropic" anthropic)
                 (const :tag "OpenAI" openai)
                 (const :tag "OpenAI Compatible" openai-compatible))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'opencode-api-set-credentials)
           (opencode-api-set-credentials)))
  :initialize 'custom-initialize-default
  :group 'opencode)

(defcustom opencode-api-key nil
  "API key for the LLM provider.
For Anthropic: Get from https://console.anthropic.com/
For OpenAI: Get from https://platform.openai.com/api-keys"
  :type '(string :tag "API Key")
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'opencode-api-set-credentials)
           (opencode-api-set-credentials)))
  :initialize 'custom-initialize-default
  :group 'opencode)

(defcustom opencode-base-url nil
  "Base URL for OpenAI-compatible API.
Only used when `opencode-provider' is set to 'openai-compatible."
  :type '(string :tag "Base URL")
  :group 'opencode)

(defcustom opencode-model "claude-sonnet-4-20250514"
  "Model identifier to use for AI requests.
Examples:
- Anthropic: claude-sonnet-4-20250514, claude-3-5-sonnet-20241022
- OpenAI: gpt-4o, gpt-4o-mini, o1"
  :type 'string
  :group 'opencode)

(defcustom opencode-max-tokens 8192
  "Maximum tokens for AI responses."
  :type 'integer
  :group 'opencode)

(defcustom opencode-temperature 0.7
  "Temperature for AI response generation (0.0 to 1.0).
Lower values make responses more focused and deterministic.
Higher values make responses more creative and varied."
  :type 'number
  :group 'opencode)

(defcustom opencode-system-prompt
  "You are OpenCode, an AI coding agent that helps users with software development tasks.

You have access to tools for reading files, editing files, searching code, and running commands. Use these tools to accomplish the user's goals.

When making code changes:
1. Always read existing files before editing them
2. Explain your changes clearly
3. Follow the existing code style and conventions
4. Be concise and direct

If you're unsure about something, ask the user for clarification."
  "System prompt for the AI agent."
  :type 'string
  :group 'opencode)

(defcustom opencode-buffer-name "*opencode*"
  "Name of the buffer used for OpenCode output."
  :type 'string
  :group 'opencode)

(defcustom opencode-auto-scroll t
  "Automatically scroll output buffer when new content arrives."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-enable-tools '(read write grep glob bash)
  "List of enabled tools.
Available tools: read, write, grep, glob, bash."
  :type '(set (const :tag "Read files" read)
              (const :tag "Write files" write)
              (const :tag "Search content (grep)" grep)
              (const :tag "Find files (glob)" glob)
              (const :tag "Run shell commands" bash))
  :group 'opencode)

(defcustom opencode-project-root-function nil
  "Function to find project root directory.
The function should take no arguments and return the project root path as a string.
If nil, uses `projectile-project-root' when available, or `default-directory'."
  :type '(choice (function :tag "Custom function")
                 (const :tag "Default" nil))
  :group 'opencode)

(defcustom opencode-max-history 100
  "Maximum number of messages to keep in session history."
  :type 'integer
  :group 'opencode)

;;;###autoload
(defun opencode-get-api-key ()
  "Get the API key for current provider.
First checks `opencode-api-key', then falls back to environment variable."
  (or opencode-api-key
      (let ((env-var (pcase opencode-provider
                       ('anthropic "ANTHROPIC_API_KEY")
                       ('openai "OPENAI_API_KEY")
                       ('openai-compatible "OPENAI_API_KEY"))))
        (when env-var (getenv env-var)))))

(provide 'opencode-config)
;;; opencode-config.el ends here
