;;; opencode-ui.el --- User interface for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; User interface for OpenCode including minibuffer commands and output buffer.

;;; Code:

(require 'opencode-session)
(require 'opencode-agent)

;;; Buffer management

(defvar-local opencode-ui--output-buffer nil
  "The buffer used for OpenCode output.")

(defun opencode-ui-get-buffer ()
  "Get or create the OpenCode output buffer."
  (let ((buffer (get-buffer-create opencode-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'opencode-output-mode)
        (opencode-output-mode)))
    buffer))

(defun opencode-ui-display-buffer ()
  "Display the OpenCode output buffer."
  (let ((buffer (opencode-ui-get-buffer)))
    (display-buffer buffer
                   '((display-buffer-reuse-window
                      display-buffer-in-direction)
                     (direction . bottom)
                     (window-height . 0.3))))
  (select-window (get-buffer-window opencode-buffer-name)))

(defun opencode-ui-clear-buffer ()
  "Clear the OpenCode output buffer."
  (interactive)
  (with-current-buffer (opencode-ui-get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;; Output mode

(define-derived-mode opencode-output-mode fundamental-mode "OpenCode"
  "Major mode for OpenCode output."
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (setq-local display-fill-column-indicator-column nil))

;;; Rendering functions

(defun opencode-ui-insert-user-message (text)
  "Insert user message TEXT into output buffer."
  (with-current-buffer (opencode-ui-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "\n❯ %s\n" text)
                         'face '(bold font-lock-keyword-face)))
      (when opencode-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

(defun opencode-ui-insert-assistant-message (text)
  "Insert assistant message TEXT into output buffer."
  (with-current-buffer (opencode-ui-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize "\n🤖 " 'face 'font-lock-string-face))
      (insert (opencode-ui--render-markdown text))
      (insert "\n")
      (when opencode-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

(defun opencode-ui-insert-tool-call (tool-name input)
  "Insert tool call notification into output buffer."
  (with-current-buffer (opencode-ui-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "\n🔧 %s" tool-name)
                         'face 'font-lock-builtin-face))
      (insert (propertize (format " %s\n"
                                 (if (stringp input)
                                     input
                                   (truncate-string-to-width
                                    (json-encode input) 100 nil nil "...")))
                         'face 'font-lock-comment-face))
      (when opencode-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

(defun opencode-ui-insert-error (error-text)
  "Insert ERROR-TEXT into output buffer."
  (with-current-buffer (opencode-ui-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "\n⚠ Error: %s\n" error-text)
                         'face '(bold font-lock-warning-face)))
      (when opencode-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

(defun opencode-ui-insert-streaming (text)
  "Insert streaming TEXT into output buffer."
  (with-current-buffer (opencode-ui-get-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert text))
      (when opencode-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

;;; Basic markdown rendering

(defun opencode-ui--render-markdown (text)
  "Basic markdown rendering for TEXT.
Handles code blocks, bold, and inline code."
  (let ((result text))
    ;; Code blocks
    (setq result (replace-regexp-in-string
                  "```\\([^\n]*\\)\n\\([^`]*?\\)\n```"
                  (lambda (match)
                    (let ((lang (match-string 1 match))
                          (code (match-string 2 match)))
                      (propertize code
                                 'face 'font-lock-constant-face
                                 'display `(margin left-margin ,(concat " " lang "\n")))))
                  result t t))
    ;; Inline code
    (setq result (replace-regexp-in-string
                  "`\\([^`]+\\)`"
                  (lambda (match)
                    (propertize (match-string 1 match)
                               'face 'font-lock-constant-face))
                  result t t))
    ;; Bold
    (setq result (replace-regexp-in-string
                  "\\*\\*\\([^*]+\\)\\*\\*"
                  (lambda (match)
                    (propertize (match-string 1 match)
                               'face 'bold))
                  result t t))
    result))

;;; Minibuffer interface

;;;###autoload
(defun opencode-prompt ()
  "Prompt for input and send to OpenCode agent."
  (interactive)
  (let ((input (read-string "OpenCode: ")))
    (when (not (string-blank-p input))
      (opencode-ui-clear-buffer)
      (opencode-ui-display-buffer)
      (opencode-ui-insert-user-message input)
      (opencode-ui-process input))))

;;;###autoload
(defun opencode-prompt-region (begin end)
  "Send region from BEGIN to END to OpenCode agent."
  (interactive "r")
  (let ((input (buffer-substring begin end)))
    (opencode-ui-clear-buffer)
    (opencode-ui-display-buffer)
    (opencode-ui-insert-user-message (format "[Region] %s" input))
    (opencode-ui-process input)))

;;;###autoload
(defun opencode-ask-at-point ()
  "Ask about the symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (let ((input (format "Explain this code: %s" symbol)))
        (opencode-ui-clear-buffer)
        (opencode-ui-display-buffer)
        (opencode-ui-insert-user-message input)
        (opencode-ui-process input)))))

;;; Processing

(defvar opencode-ui--processing nil
  "Whether a request is currently being processed.")

(defun opencode-ui-process (input)
  "Process INPUT through the agent."
  (when opencode-ui--processing
    (error "Already processing a request"))
  (setq opencode-ui--processing t)
  (message "OpenCode: Processing...")

  (condition-case err
      (progn
        ;; Add a loading indicator
        (with-current-buffer (opencode-ui-get-buffer)
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (insert (propertize "▌" 'face 'font-lock-comment-face)))))

        (opencode-agent-process
         input
         (lambda (response)
           (opencode-ui--finish-processing response))))
    (error
     (opencode-ui-insert-error (error-message-string err))
     (setq opencode-ui--processing nil)
     (message "OpenCode: Error"))))

(defun opencode-ui--finish-processing (response)
  "Finish processing with RESPONSE."
  (setq opencode-ui--processing nil)
  ;; Remove loading indicator
  (with-current-buffer (opencode-ui-get-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (when (looking-back "▌" 1)
          (delete-char -1)))))
  (opencode-ui-insert-assistant-message response)
  (message "OpenCode: Done"))

;;; Session management commands

;;;###autoload
(defun opencode-clear-session ()
  "Clear the current session."
  (interactive)
  (opencode-session-reset)
  (opencode-ui-clear-buffer)
  (message "OpenCode: Session cleared"))

;;;###autoload
(defun opencode-show-session ()
  "Show the current session summary."
  (interactive)
  (let ((session (opencode-session-get)))
    (with-output-to-temp-buffer "*OpenCode Session*"
      (princ (opencode-session-summarize session)))))

(provide 'opencode-ui)
;;; opencode-ui.el ends here
