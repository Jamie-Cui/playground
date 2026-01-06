;;; opencode.el --- OpenCode AI coding agent for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, copilot
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/anomalyco/opencode

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; opencode.el is an Emacs Lisp implementation of OpenCode, an open-source
;; AI coding agent.  It provides intelligent code assistance by integrating
;; with Large Language Models (LLMs) like Anthropic's Claude and OpenAI's GPT.
;;
;; Features:
;; - Direct API integration with Anthropic and OpenAI
;; - File operations (read, write, grep, glob)
;; - Shell command execution
;; - Session management with conversation history
;; - Minibuffer interface for quick prompts
;;
;; Configuration:
;; Customize the `opencode' group to set your API key, model, and other options.
;;
;;   M-x customize-group RET opencode RET
;;
;; Usage:
;;   M-x opencode-prompt        - Send a prompt to the AI
;;   M-x opencode-prompt-region - Send the selected region to the AI
;;   M-x opencode-ask-at-point  - Ask about the symbol at point
;;   M-x opencode-clear-session - Clear the current session
;;
;; Setup:
;; 1. Set your API key:
;;    (setq opencode-api-key "your-api-key-here")
;;    or set the ANTHROPIC_API_KEY or OPENAI_API_KEY environment variable.
;;
;; 2. Choose your provider:
;;    (setq opencode-provider 'anthropic)  ; or 'openai or 'openai-compatible
;;
;; 3. Optionally set a custom model:
;;    (setq opencode-model "claude-sonnet-4-20250514")

;;; Code:

;; Required modules
(require 'opencode-config)
(require 'opencode-api)
(require 'opencode-session)
(require 'opencode-tools)
(require 'opencode-agent)
(require 'opencode-ui)

;;; Initialization

;;;###autoload
(defun opencode-mode ()
  "Minor mode for OpenCode AI coding agent.
When enabled, OpenCode commands are available.

\\{opencode-mode-map}"
  :init-value nil
  :lighter " OpenCode"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Keybindings
            (define-key map (kbd "C-c o p") #'opencode-prompt)
            (define-key map (kbd "C-c o r") #'opencode-prompt-region)
            (define-key map (kbd "C-c o a") #'opencode-ask-at-point)
            (define-key map (kbd "C-c o c") #'opencode-clear-session)
            (define-key map (kbd "C-c o s") #'opencode-show-session)
            map)
  (if opencode-mode
      (progn
        (opencode-api-set-credentials)
        (message "OpenCode mode enabled"))
    (message "OpenCode mode disabled")))

;;;###autoload
(define-globalized-minor-mode global-opencode-mode opencode-mode
  (lambda () (opencode-mode 1))
  :group 'opencode)

;; Auto-enable on load
(add-hook 'after-init-hook #'opencode-api-set-credentials)

(provide 'opencode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; opencode.el ends here
