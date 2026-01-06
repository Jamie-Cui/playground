;;; opencode-session.el --- Session management for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Session management for storing conversation history and state.

;;; Code:

(require 'cl-lib)

;;; Session state structure

(cl-defstruct (opencode-session
               (:constructor opencode-session-create)
               (:copier nil))
  messages
  (max-history opencode-max-history)
  (id nil))

;;; Session management

(defvar opencode--current-session nil
  "The current active session.")

(defun opencode-session-get ()
  "Get the current session, creating one if needed."
  (unless opencode--current-session
    (setq opencode--current-session (opencode-session-create)))
  opencode--current-session)

(defun opencode-session-reset ()
  "Reset the current session, clearing all messages."
  (interactive)
  (setq opencode--current-session nil)
  (message "OpenCode session cleared."))

(defun opencode-session-get-id (session)
  "Get or generate a unique ID for SESSION."
  (or (opencode-session-id session)
      (let ((id (format "session-%s" (format-time-string "%Y%m%d-%H%M%S"))))
        (setf (opencode-session-id session) id)
        id)))

;;; Message management

(defun opencode-session-add-message (session role content)
  "Add a message to SESSION.
ROLE is either 'user', 'assistant', or 'tool'.
CONTENT can be a string or a list of content blocks."
  (let ((messages (opencode-session-messages session)))
    (push (list (cons 'role role)
                (cons 'content content))
          messages)
    ;; Trim to max history
    (when (> (length messages) (opencode-session-max-history session))
      (setf messages (butlast messages (- (length messages)
                                          (opencode-session-max-history session))))
      (setf (opencode-session-messages session) messages))
    session))

(defun opencode-session-get-messages (session)
  "Get all messages from SESSION in chronological order."
  (reverse (opencode-session-messages session)))

(defun opencode-session-add-tool-result (session tool-use-id result)
  "Add a tool result message to SESSION.
TOOL-USE-ID is the ID of the tool use being responded to.
RESULT is the string result of tool execution."
  (opencode-session-add-message
   session 'tool
   `((type . "tool_result")
     (tool_use_id . ,tool-use-id)
     (content . ,result))))

(defun opencode-session-get-context-size (session)
  "Calculate approximate token count of SESSION messages.
This is a rough estimate assuming ~4 chars per token."
  (let ((total-chars 0))
    (dolist (msg (opencode-session-get-messages session))
      (let ((content (cdr (assq 'content msg))))
        (cl-incf total-chars
               (if (stringp content)
                   (length content)
                 ;; For structured content, count text fields
                 (let ((chars 0))
                   (dolist (block content)
                     (when (equal (cdr (assq 'type block)) "text")
                       (cl-incf chars (length (cdr (assq 'text block))))))
                   chars)))))
    (/ total-chars 4))) ; Rough estimate

;;; Session persistence

(defun opencode-session-save (session &optional file)
  "Save SESSION to FILE.
If FILE is nil, uses a default location based on session ID."
  (let* ((session-id (opencode-session-get-id session))
         (default-dir (expand-file-name "opencode-sessions" user-emacs-directory))
         (default-file (expand-file-name (concat session-id ".json") default-dir))
         (filename (or file default-file)))
    (make-directory default-dir t)
    (with-temp-file filename
      (insert (json-encode `((id . ,session-id)
                            (messages . ,(opencode-session-messages session))
                            (timestamp . ,(format-time-string "%Y-%m-%dT%T%z"))))))
    filename))

(defun opencode-session-load (file)
  "Load session from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((data (json-read))
           (session (opencode-session-create)))
      (setf (opencode-session-id session) (cdr (assq 'id data)))
      (setf (opencode-session-messages session) (cdr (assq 'messages data)))
      session)))

(defun opencode-session-list-saved ()
  "List all saved session files."
  (let ((session-dir (expand-file-name "opencode-sessions" user-emacs-directory)))
    (when (file-directory-p session-dir)
      (directory-files session-dir t "\\.json$"))))

;;; Session context helpers

(defun opencode-session-get-project-files (session &optional directory)
  "Get a list of relevant project files for context.
Uses DIRECTORY or current project root."
  (let* ((default-directory (or directory
                               (when (fboundp 'projectile-project-root)
                                 (projectile-project-root))
                               default-directory))
         (files (condition-case nil
                    (all-completions "" (directory-files default-directory))
                  (error nil))))
    files))

(defun opencode-session-summarize (session)
  "Create a summary of SESSION messages.
Returns a condensed version of the conversation."
  (let ((messages (opencode-session-get-messages session)))
    (when messages
      (with-temp-buffer
        (insert "Session Summary:\n\n")
        (dolist (msg (take-last messages 20))
          (let ((role (cdr (assq 'role msg)))
                (content (cdr (assq 'content msg))))
            (insert (format "[%s] " (upcase (symbol-name role))))
            (if (stringp content)
                (insert (truncate-string-to-width content 80 nil nil "..."))
              ;; Handle structured content
              (let ((text-parts (cl-loop for block in content
                                        when (equal (cdr (assq 'type block)) "text")
                                        collect (cdr (assq 'text block))))
                (insert (string-join text-parts " "))))
            (insert "\n\n")))
        (buffer-string))))))

(provide 'opencode-session)
;;; opencode-session.el ends here
