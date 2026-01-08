;;; opencode-api.el --- HTTP API client for LLM providers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; HTTP API client for communicating with LLM providers (Anthropic, OpenAI).

;;; Code:

(require 'json)
(require 'url)
(require 'opencode-config)
(eval-when-compile (require 'cl-lib))

;;; Variables

(defvar opencode-api--anthropic-url "https://api.anthropic.com/v1/messages"
  "Endpoint for Anthropic Messages API.")

(defvar opencode-api--openai-url "https://api.openai.com/v1/chat/completions"
  "Endpoint for OpenAI Chat Completions API.")

(defvar opencode-api--request-timeout 120
  "Timeout in seconds for API requests.")

;;; Helper functions

(defun opencode-api--json-encode (object)
  "Encode OBJECT to JSON string."
  (json-encode object))

(defun opencode-api--json-decode (string)
  "Decode JSON STRING to Lisp object."
  (json-read-from-string string))

(defun opencode-api--get-endpoint ()
  "Get the API endpoint URL based on current provider."
  (pcase opencode-provider
    ('anthropic opencode-api--anthropic-url)
    ('openai opencode-api--openai-url)
    ('openai-compatible opencode-base-url)
    (_ (error "Unknown provider: %s" opencode-provider))))

(defun opencode-api--get-headers ()
  "Get HTTP headers for API request."
  (let ((api-key (opencode-get-api-key)))
    (unless api-key
      (error "API key not set. Configure opencode-api-key or set environment variable"))
    (pcase opencode-provider
      ('anthropic
       `(("x-api-key" . ,api-key)
         ("anthropic-version" . "2023-06-01")
         ("content-type" . "application/json")))
      (_
       `(("authorization" . ,(concat "Bearer " api-key))
         ("content-type" . "application/json"))))))

;;; Credentials

;;;###autoload
(defun opencode-api-set-credentials ()
  "Validate and set API credentials.
Checks if API key is available from config or environment."
  (interactive)
  (let ((api-key (opencode-get-api-key)))
    (if api-key
        (message "OpenCode: API key configured for %s" opencode-provider)
      (message "OpenCode: API key not configured. Set opencode-api-key or %s"
               (pcase opencode-provider
                 ('anthropic "ANTHROPIC_API_KEY")
                 ('openai "OPENAI_API_KEY")
                 (_ "API_KEY env"))))))

;;; Core API functions

(defun opencode-api--convert-messages (messages)
  "Convert internal MESSAGE format to provider-specific format.
Internal format: ((role . \"user\") (content . \"text\")) or
                 ((role . \"user\") (content . ((\"type\" . \"text\") (\"text\" . \"...\")))))"
  (mapcar (lambda (msg)
            (let ((role (cdr (assq 'role msg)))
                  (content (cdr (assq 'content msg))))
              (pcase opencode-provider
                ('anthropic
                 `((role . ,role)
                   (content . ,(if (listp content)
                                   content
                                 `((type . "text") (text . ,content))))))
                (_ ; OpenAI format
                 `((role . ,(if (equal role "assistant") "assistant" "user"))
                   (content . ,(if (listp content)
                                   (let ((text-parts (cl-loop for c in content
                                                              when (equal (cdr (assq 'type c)) "text")
                                                              collect (cdr (assq 'text c))))
                                         (image-parts (cl-loop for c in content
                                                               when (equal (cdr (assq 'type c)) "image")
                                                               collect c)))
                                     (if (null image-parts)
                                         (mapconcat #'identity text-parts "\n")
                                       ;; TODO: Handle images for OpenAI
                                       (mapconcat #'identity text-parts "\n")))
                                 content)))))))
          messages))

(cl-defun opencode-api-chat (messages &key
                                      (tools nil)
                                      (stream nil)
                                      (model opencode-model)
                                      (max-tokens opencode-max-tokens)
                                      (temperature opencode-temperature)
                                      (callback nil))
  "Send chat completion request to LLM provider.
MESSAGES is a list of message objects with 'role and 'content keys.
TOOLS is a list of available tool definitions (optional).
STREAM if non-nil enables streaming responses.
CALLBACK is a function called with the response when complete.
Returns immediately; response is delivered via CALLBACK."
  (let* ((endpoint (opencode-api--get-endpoint))
         (headers (opencode-api--get-headers))
         (converted-messages (opencode-api--convert-messages messages))
         (body (pcase opencode-provider
                 ('anthropic
                  `((model . ,model)
                    (max_tokens . ,max-tokens)
                    (messages . ,converted-messages)
                    (temperature . ,temperature)
                    ,@(when tools `((tools . ,tools)))))
                 (_ ; OpenAI format
                  `((model . ,model)
                    (max_tokens . ,max-tokens)
                    (messages . ,converted-messages)
                    (temperature . ,temperature)
                    ,@(when tools `((tools . ,(opencode-api--convert-tools tools))))))))
         (request-data (opencode-api--json-encode body))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data request-data))

    (url-retrieve endpoint (if callback
                               (lambda (status &optional args)
                                 (let* ((buffer (current-buffer))
                                        (response (opencode-api--parse-response buffer)))
                                   (kill-buffer buffer)
                                   (funcall callback response)))
                             #'opencode-api--handle-response)
                  (list (list :messages messages
                              :stream stream
                              :on-complete callback))
                  t t)))

(defun opencode-api--convert-tools (tools)
  "Convert internal tool definitions to OpenAI format."
  (mapcar (lambda (tool)
            `((type . "function")
              (function . ((name . ,(cdr (assq 'name tool)))
                           (description . ,(cdr (assq 'description tool)))
                           (parameters . ,(cdr (assq 'parameters tool)))))))
          tools))

(defun opencode-api--handle-response (status &optional args)
  "Handle API response.
STATUS is the HTTP status.
ARGS contains context data."
  (let* ((buffer (current-buffer))
         (response (opencode-api--parse-response buffer)))
    (kill-buffer buffer)
    (message "Response received: %S" response)))

(defun opencode-api--parse-response (buffer)
  "Parse JSON response from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t) ; Skip headers
    (let* ((json-string (buffer-substring (point) (point-max)))
           (response (opencode-api--json-decode json-string)))
      response)))

(defun opencode-api--extract-content (response)
  "Extract content from provider RESPONSE."
  (pcase opencode-provider
    ('anthropic
     (let* ((content (cdr (assq 'content response)))
            (text-blocks (cl-loop for block in content
                                  when (equal (cdr (assq 'type block)) "text")
                                  collect (cdr (assq 'text block)))))
       (mapconcat #'identity text-blocks "\n")))
    (_ ; OpenAI
     (let* ((choices (cdr (assq 'choice response)))
            (message (cdr (assq 'message (aref choices 0))))
            (content (cdr (assq 'content message))))
       content))))

(defun opencode-api--extract-tool-uses (response)
  "Extract tool use blocks from provider RESPONSE."
  (pcase opencode-provider
    ('anthropic
     (cl-loop for block in (cdr (assq 'content response))
              when (equal (cdr (assq 'type block)) "tool_use")
              collect `((id . ,(cdr (assq 'id block)))
                        (name . ,(cdr (assq 'name block)))
                        (input . ,(cdr (assq 'input block))))))
    (_ ; OpenAI
     (cl-loop for call in (cdr (assq 'tool_calls response))
              collect `((id . ,(cdr (assq 'id call)))
                        (name . ,(cdr (assq 'function call)))
                        (input . ,(opencode-api--json-decode
                                   (cdr (assq 'arguments call)))))))))

;;; Tool calling support

(defun opencode-api--make-tool-definition (name description parameters)
  "Create a tool definition.
NAME is the tool name.
DESCRIPTION is what the tool does.
PARAMETERS is a JSON schema for tool arguments."
  `((name . ,name)
    (description . ,description)
    (input_schema . ,parameters))) ; Anthropic format

(defun opencode-api-send-tool-results (tool-use-id results)
  "Send tool execution results back to the API.
TOOL-USE-ID is the ID of the tool use being responded to.
RESULTS is the string result of tool execution."
  ;; This would append a tool_result message and call chat again
  ;; Implementation depends on session management
  )

;;; Streaming support (basic)

(defvar-local opencode-api--stream-accumulator nil
  "Accumulator for streaming response chunks.")

(defun opencode-api--handle-stream-chunk (chunk callback)
  "Handle a streaming response CHUNK.
CALLBACK is called with each content delta."
  (let ((lines (split-string chunk "\n")))
    (dolist (line lines)
      (when (string-prefix-p "data: " line)
        (let* ((json-str (substring line 6))
               (data (when (> (length json-str) 0)
                       (condition-case nil
                           (opencode-api--json-decode json-str)
                         (error nil)))))
          (when data
            (let ((delta (opencode-api--extract-stream-delta data)))
              (when delta
                (funcall callback delta)))))))))

(defun opencode-api--extract-stream-delta (data)
  "Extract content delta from streaming DATA."
  (pcase opencode-provider
    ('anthropic
     (when (equal (cdr (assq 'type data)) "content_block_delta")
       (let* ((delta (cdr (assq 'delta data)))
              (text (cdr (assq 'text delta))))
         text)))
    (_ ; OpenAI
     (let* ((choices (cdr (assq 'choices data)))
            (delta (cdr (assq 'delta (aref choices 0))))
            (content (cdr (assq 'content delta))))
       content))))

(provide 'opencode-api)
;;; opencode-api.el ends here
