;;; claude-chat.el --- Chat with Claude directly from your code files -*- lexical-binding: t; -*-

;;; Commentary:
;; This package allows you to write chat instructions as comments in your code
;; and have Claude perform work on the file directly.
;;
;; Usage:
;;
;; Installation:
;; 1. Save this file to your Emacs configuration directory (e.g., ~/.emacs.d/lisp/)
;; 2. Add the following to your init.el or .emacs file:
;;
;;    (add-to-list 'load-path "~/.emacs.d/lisp/")
;;    (require 'claude-chat)
;;
;; 3. Enable claude-chat-mode automatically for any programming mode:
;;
;;    ;; For all programming modes:
;;    (add-hook 'prog-mode-hook 'claude-chat-mode)
;;
;;    ;; Or for specific file types:
;;    (add-hook 'python-mode-hook 'claude-chat-mode)
;;    (add-hook 'js-mode-hook 'claude-chat-mode)
;;    (add-hook 'typescript-mode-hook 'claude-chat-mode)
;;
;; 4. Make sure you have the Claude CLI installed and available in your PATH:
;;    - Install from: https://github.com/anthropics/claude-code
;;    - Verify with: claude --version
;;
;; Basic usage:
;; 1. Enable claude-chat-mode in a buffer: M-x claude-chat-mode
;; 2. Add instructions as comments prefixed with "CLAUDE:" (customizable)
;;    Example: // CLAUDE: Add error handling to this function
;; 3. Process the buffer: C-c C-a p
;; 4. Remove instructions when done: C-c C-a r
;;
;; Customization:
;; - Change the instruction prefix: (setq claude-chat-comment-prefix "AI:")
;; - Change the Claude command: (setq claude-chat-claude-command "claude")

;;; Code:

(require 'json)

(defgroup claude-chat nil
  "Chat with Claude from your code files."
  :group 'tools)

(defcustom claude-chat-comment-prefix "CLAUDE:"
  "Prefix for Claude chat instructions in comments."
  :type 'string
  :group 'claude-chat)

(defcustom claude-chat-claude-command "claude"
  "Command to run Claude CLI."
  :type 'string
  :group 'claude-chat)

(defvar claude-chat-instruction-regexp nil
  "Regexp to match Claude instructions.")

(defvar claude-chat-status ""
  "Current status of Claude processing.")

(defvar claude-chat-original-modeline nil
  "Store original modeline format.")

(defun claude-chat--update-instruction-regexp ()
  "Update the instruction regexp based on current major mode."
  (let ((comment-start-escaped (regexp-quote (or comment-start "#"))))
    (setq claude-chat-instruction-regexp
          (concat "^\\s-*" comment-start-escaped "\\s-*"
                  (regexp-quote claude-chat-comment-prefix)
                  "\\s-*\\(.+\\)$"))))

(defun claude-chat-extract-instructions ()
  "Extract all Claude instructions from current buffer."
  (claude-chat--update-instruction-regexp)
  (save-excursion
    (goto-char (point-min))
    (let ((instructions '()))
      (while (re-search-forward claude-chat-instruction-regexp nil t)
        (push (match-string 1) instructions))
      (nreverse instructions))))

(defun claude-chat-create-prompt (instructions file-content file-name)
  "Create a prompt for Claude with INSTRUCTIONS and FILE-CONTENT."
  (concat "I'm working on the file " file-name "\n\n"
          "Here's the current content:\n"
          "```\n" file-content "\n```\n\n"
          "Please perform the following tasks:\n"
          (mapconcat (lambda (inst) (concat "- " inst)) instructions "\n")
          "\n\nReturn the complete updated file content."))

(defun claude-chat-update-status (status)
  "Update Claude status in modeline."
  (setq claude-chat-status status)
  (force-mode-line-update t))

(defun claude-chat-call-claude-async (prompt callback)
  "Call Claude CLI with PROMPT asynchronously, then call CALLBACK with result."
  (let* ((temp-file (make-temp-file "claude-prompt-" nil ".txt"))
         (output-buffer (generate-new-buffer " *claude-output*"))
         (process nil)
         (start-time (current-time))
         (timer nil))
    ;; Write prompt to temp file
    (with-temp-file temp-file
      (insert prompt))
    
    ;; Start async process with unbuffered output
    (claude-chat-update-status "🤖 Claude starting...")
    (setq process 
          (start-process-shell-command 
           "claude-chat" 
           output-buffer
           (format "cat %s | unbuffer %s --print 2>&1"
                   (shell-quote-argument temp-file)
                   claude-chat-claude-command)))
    
    ;; Store temp-file in process property for cleanup
    (process-put process 'temp-file temp-file)
    
    ;; Start timer to show elapsed time
    (setq timer (run-with-timer 0 0.5
                                (lambda ()
                                  (when (process-live-p process)
                                    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                                      (claude-chat-update-status 
                                       (format "🤖 Claude: %.1fs..." elapsed)))))))
    (process-put process 'timer timer)
    
    ;; Set up process filter to capture output
    (set-process-filter process
                        (lambda (proc string)
                          ;; Remove ANSI escape sequences
                          (setq string (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" string))
                          ;; Remove other terminal control sequences
                          (setq string (replace-regexp-in-string "\\[\\?[0-9]+[hl]" "" string))
                          (with-current-buffer (process-buffer proc)
                            (goto-char (point-max))
                            (insert string)
                            ;; When we start getting output, update status
                            (when (> (buffer-size) 0)
                              (claude-chat-update-status 
                               (format "🤖 Claude: receiving %d chars..." (buffer-size)))))))
    
    ;; Set up sentinel for completion
    (set-process-sentinel process
                          (lambda (proc status)
                            (when (memq (process-status proc) '(exit signal))
                              (let ((output (with-current-buffer (process-buffer proc)
                                              (buffer-string)))
                                    (temp-file (process-get proc 'temp-file))
                                    (timer (process-get proc 'timer)))
                                ;; Clean up
                                (when timer (cancel-timer timer))
                                (claude-chat-update-status "")
                                (when (file-exists-p temp-file)
                                  (delete-file temp-file))
                                (kill-buffer (process-buffer proc))
                                ;; Call callback
                                (if (string-empty-p output)
                                    (message "Error: No output from Claude")
                                  (funcall callback output))))))))

(defun claude-chat-extract-code-from-response (response)
  "Extract code block from Claude's RESPONSE."
  (if (string-match "```\\(?:[a-zA-Z]+\n\\)?\\(\\(?:.\\|\n\\)*?\\)```" response)
      (match-string 1 response)
    response))

(defun claude-chat-process-current-buffer ()
  "Process Claude instructions in current buffer."
  (interactive)
  (let* ((instructions (claude-chat-extract-instructions)))
    (if (null instructions)
        (message "No Claude instructions found (looking for comments starting with %s)"
                 claude-chat-comment-prefix)
      (let* ((file-name (buffer-file-name))
             (file-content (buffer-string))
             (prompt (claude-chat-create-prompt instructions file-content
                                                (or file-name "untitled")))
             (target-buffer (current-buffer)))
        (claude-chat-update-status "📋 Preparing prompt...")
        ;; Call Claude asynchronously
        (claude-chat-call-claude-async 
         prompt
         (lambda (response)
           (let ((new-content (claude-chat-extract-code-from-response response)))
             (claude-chat-update-status "✏️ Updating buffer...")
             (with-current-buffer target-buffer
               (erase-buffer)
               (insert new-content))
             (claude-chat-update-status "")
             (message "Buffer updated with Claude's response"))))))))

(defun claude-chat-remove-instructions ()
  "Remove all Claude instruction comments from current buffer."
  (interactive)
  (claude-chat--update-instruction-regexp)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward claude-chat-instruction-regexp nil t)
        (beginning-of-line)
        (kill-line 1)
        (setq count (1+ count)))
      (message "Removed %d Claude instruction(s)" count))))

;;;###autoload
(define-minor-mode claude-chat-mode
  "Minor mode for chatting with Claude in code files."
  :lighter (:eval (if (string-empty-p claude-chat-status)
                      " Claude"
                    (format " %s" claude-chat-status)))
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-a p") 'claude-chat-process-current-buffer)
            (define-key map (kbd "C-c C-a r") 'claude-chat-remove-instructions)
            map)
  (if claude-chat-mode
      (progn
        ;; Store original modeline
        (setq claude-chat-original-modeline mode-line-format))
    ;; Restore on disable
    (setq claude-chat-status "")))

(provide 'claude-chat)
;;; claude-chat.el ends here
