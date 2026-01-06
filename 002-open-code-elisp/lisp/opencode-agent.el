;;; opencode-agent.el --- Agent logic and tool calling for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'cl-lib)
(require 'opencode-tools)
(require 'opencode-session)
(require 'opencode-api)

;;; Agent execution

(defun opencode-agent-process (user-prompt &optional callback)
  "Process USER-PROMPT through the AI agent with tool calling.
Calls CALLBACK with the final response when complete."
  (let ((session (opencode-session-get)))
    ;; Add user message to session
    (opencode-session-add-message session 'user user-prompt)

    ;; Start the agent loop
    (opencode-agent--loop session callback)))

(defun opencode-agent--loop (session callback &optional iteration)
  "Main agent loop for handling tool calls.
SESSION is the current session.
CALLBACK is called with final response.
ITERATION tracks loop depth to prevent infinite loops."
  (let ((iter (or iteration 0))
        (max-iterations 10))
    (when (>= iter max-iterations)
      (error "Agent exceeded maximum iterations (%d)" max-iterations))

    (let ((messages (opencode-session-get-messages session)))
      ;; Make API request
      (opencode-api-chat
       messages
       :tools (opencode-tools-get-definitions)
       :stream nil
       :callback (lambda (response)
                   (opencode-agent--handle-response
                    session response callback iter))))))

(defun opencode-agent--handle-response (session response callback iteration)
  "Handle API RESPONSE, executing tools or calling CALLBACK."
  (let ((tool-uses (opencode-api--extract-tool-uses response)))
    (if tool-uses
        ;; Execute tools and continue loop
        (opencode-agent--execute-tools session tool-uses response callback iteration)
      ;; No tools, done - add assistant message and call callback
      (let ((content (opencode-api--extract-content response)))
        (opencode-session-add-message session 'assistant content)
        (when callback
          (funcall callback content))))))

(defun opencode-agent--execute-tools (session tool-uses response callback iteration)
  "Execute TOOL-USES and continue agent loop."
  (let ((assistant-msg `((role . "assistant")
                         (content . ,(cdr (assq 'content response))))))
    ;; Add assistant message with tool uses to session
    (opencode-session-add-message session 'assistant
                                 (cdr (assq 'content response)))

    ;; Execute each tool and collect results
    (dolist (tool-use tool-uses)
      (let* ((tool-id (cdr (assq 'id tool-use)))
             (tool-name (cdr (assq 'name tool-use)))
             (tool-input (cdr (assq 'input tool-use)))
             (result (opencode-tools-execute tool-name tool-input)))
        (opencode-session-add-tool-result session tool-id result)))

    ;; Continue the loop
    (opencode-agent--loop session callback (1+ iteration))))

;;; Streaming support

(defun opencode-agent-process-stream (user-prompt callback)
  "Process USER-PROMPT with streaming response.
CALLBACK is called with each content chunk."
  (let ((session (opencode-session-get)))
    (opencode-session-add-message session 'user user-prompt)

    (let ((messages (opencode-session-get-messages session)))
      (opencode-api-chat
       messages
       :tools (opencode-tools-get-definitions)
       :stream t
       :callback (lambda (chunk)
                   (funcall callback chunk)
                   ;; Also accumulate and add to session when done
                   )))))

;;; Tool use formatting

(defun opencode-agent--format-tool-use (tool-use)
  "Format a tool use for display."
  (let ((name (cdr (assq 'name tool-use)))
        (input (cdr (assq 'input tool-use))))
    (format "🔧 Using: %s\n   Args: %s"
            name
            (if (stringp input)
                input
              (json-encode input)))))

(defun opencode-agent--format-tool-result (result)
  "Format a tool result for display."
  (let ((truncated (if (> (length result) 500)
                      (concat (substring result 0 500) "...")
                    result)))
    (format "   Result: %s" truncated)))

;;; Context injection

(defun opencode-agent--inject-context (session)
  "Inject relevant context into the session.
This adds information about the current project."
  (when opencode-project-root-function
    (let* ((root (funcall opencode-project-root-function))
           (files (opencode-session-get-project-files session root)))
      (when files
        (let ((context-msg
               (format "Current project files:\n%s"
                       (string-join (take 20 files) "\n"))))
          ;; This could be added as a system message or context
          context-msg)))))

;;; Error handling

(defun opencode-agent--handle-error (err session callback)
  "Handle an error during agent execution."
  (let ((error-msg (format "Error: %s" (error-message-string err))))
    (opencode-session-add-message session 'assistant error-msg)
    (when callback
      (funcall callback error-msg))))

;;; Agent configuration

(defvar opencode-agent--system-prompts
  `((default . ,opencode-system-prompt))
  "Alist of agent system prompts.")

(defun opencode-agent-get-system-prompt (&optional agent-name)
  "Get the system prompt for AGENT-NAME (or default)."
  (let ((prompt (cdr (assq (or agent-name 'default)
                           opencode-agent--system-prompts))))
    (or prompt opencode-system-prompt)))

(defun opencode-agent-set-system-prompt (agent-name prompt)
  "Set the system prompt for AGENT-NAME to PROMPT."
  (setf (alist-get agent-name opencode-agent--system-prompts)
        prompt))

(provide 'opencode-agent)
;;; opencode-agent.el ends here
