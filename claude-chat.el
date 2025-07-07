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
;; 5. (Optional) For better real-time output, install 'expect' which provides 'unbuffer':
;;    - macOS: brew install expect
;;    - Ubuntu/Debian: sudo apt-get install expect
;;    - The package will work without it, but output may be buffered
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
;; - Disable diff confirmation: (setq claude-chat-confirm-changes nil)

;;; Code:

(require 'json)
(require 'cl-lib)

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

(defcustom claude-chat-confirm-changes t
  "Whether to show diff and require confirmation before applying changes."
  :type 'boolean
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
  (concat "You are an AI assistant helping to edit code. You must:\n"
          "1. Make the requested changes directly without asking for permission\n"
          "2. ONLY modify the content of the current file shown below\n"
          "3. Do NOT suggest creating or modifying any other files\n"
          "4. Return ONLY the updated content of THIS file\n\n"
          "Current file: " file-name "\n\n"
          "File content:\n"
          "```\n" file-content "\n```\n\n"
          "Tasks to perform on THIS file:\n"
          (mapconcat (lambda (inst) (concat "- " inst)) instructions "\n")
          "\n\nReturn the complete updated content of " file-name " with all requested changes applied. "
          "Output ONLY the file content, nothing else."))

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
    
    ;; Start async process (use unbuffer if available for real-time output)
    (claude-chat-update-status "🤖 Claude starting...")
    (let ((unbuffer-cmd (if (executable-find "unbuffer") "unbuffer " "")))
      (setq process 
            (start-process-shell-command 
             "claude-chat" 
             output-buffer
             (format "cat %s | %s%s --print 2>&1"
                     (shell-quote-argument temp-file)
                     unbuffer-cmd
                     claude-chat-claude-command))))
    
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


(defface claude-chat-added-face
  '((t :background "#2f4f2f" :foreground "#90ee90"))
  "Face for added lines in diff display."
  :group 'claude-chat)

(defface claude-chat-removed-face
  '((t :background "#4f2f2f" :foreground "#ffb6c1"))
  "Face for removed lines in diff display."
  :group 'claude-chat)

(defface claude-chat-context-face
  '((t :foreground "#808080"))
  "Face for context lines in diff display."
  :group 'claude-chat)

(defun claude-chat-compute-line-diff (original-lines new-lines)
  "Compute line-by-line differences between ORIGINAL-LINES and NEW-LINES.
Returns a list of (type . line) where type is 'added, 'removed, or 'unchanged."
  (let ((orig-idx 0)
        (new-idx 0)
        (result '()))
    ;; Simple line-by-line comparison
    (while (or (< orig-idx (length original-lines))
               (< new-idx (length new-lines)))
      (cond
       ;; Both lines exist and are equal
       ((and (< orig-idx (length original-lines))
             (< new-idx (length new-lines))
             (string= (nth orig-idx original-lines)
                     (nth new-idx new-lines)))
        (push (cons 'unchanged (nth orig-idx original-lines)) result)
        (setq orig-idx (1+ orig-idx))
        (setq new-idx (1+ new-idx)))
       ;; Try to find matching line ahead
       ((and (< orig-idx (length original-lines))
             (< new-idx (length new-lines)))
        ;; Check if current new line matches any upcoming original line
        (let ((match-pos (cl-position (nth new-idx new-lines) 
                                     original-lines 
                                     :start orig-idx
                                     :test #'string=)))
          (if (and match-pos (< (- match-pos orig-idx) 5)) ; Look ahead max 5 lines
              ;; Found match ahead - mark intervening lines as removed
              (progn
                (while (< orig-idx match-pos)
                  (push (cons 'removed (nth orig-idx original-lines)) result)
                  (setq orig-idx (1+ orig-idx)))
                (push (cons 'unchanged (nth new-idx new-lines)) result)
                (setq orig-idx (1+ orig-idx))
                (setq new-idx (1+ new-idx)))
            ;; No nearby match - check if original line matches ahead in new
            (let ((new-match-pos (cl-position (nth orig-idx original-lines)
                                            new-lines
                                            :start new-idx
                                            :test #'string=)))
              (if (and new-match-pos (< (- new-match-pos new-idx) 5))
                  ;; Found match in new lines - mark intervening as added
                  (progn
                    (while (< new-idx new-match-pos)
                      (push (cons 'added (nth new-idx new-lines)) result)
                      (setq new-idx (1+ new-idx)))
                    (push (cons 'unchanged (nth orig-idx original-lines)) result)
                    (setq orig-idx (1+ orig-idx))
                    (setq new-idx (1+ new-idx)))
                ;; No match - one removed, one added
                (push (cons 'removed (nth orig-idx original-lines)) result)
                (push (cons 'added (nth new-idx new-lines)) result)
                (setq orig-idx (1+ orig-idx))
                (setq new-idx (1+ new-idx)))))))
       ;; Only original lines left - all removed
       ((< orig-idx (length original-lines))
        (push (cons 'removed (nth orig-idx original-lines)) result)
        (setq orig-idx (1+ orig-idx)))
       ;; Only new lines left - all added
       ((< new-idx (length new-lines))
        (push (cons 'added (nth new-idx new-lines)) result)
        (setq new-idx (1+ new-idx)))))
    (nreverse result)))

(defun claude-chat-show-diff-and-confirm (original-content new-content file-name original-buffer)
  "Show differences with color highlighting and ask for confirmation."
  (let* ((diff-buffer (generate-new-buffer (format "*Claude Changes: %s*" file-name)))
         (original-lines (split-string original-content "\n"))
         (new-lines (split-string new-content "\n"))
         (diff-data (claude-chat-compute-line-diff original-lines new-lines))
         (confirmed nil))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            ;; Set up the buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Claude's proposed changes to: %s\n" file-name))
              (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
              (insert "Legend: ")
              (insert (propertize "[-removed]" 'face 'claude-chat-removed-face))
              (insert "  ")
              (insert (propertize "[+added]" 'face 'claude-chat-added-face))
              (insert "  ")
              (insert (propertize "[unchanged]" 'face 'claude-chat-context-face))
              (insert "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
              
              ;; Add line numbers and content with appropriate faces
              (let ((line-num 1)
                    (removed-count 0)
                    (added-count 0))
                (dolist (item diff-data)
                  (let ((type (car item))
                        (line (cdr item)))
                    (pcase type
                      ('unchanged
                       (insert (propertize (format "%4d  %s\n" line-num line)
                                         'face 'claude-chat-context-face))
                       (setq line-num (1+ line-num)))
                      ('removed
                       (insert (propertize (format "    - %s\n" line)
                                         'face 'claude-chat-removed-face))
                       (setq removed-count (1+ removed-count)))
                      ('added
                       (insert (propertize (format "%4d+ %s\n" line-num line)
                                         'face 'claude-chat-added-face))
                       (setq line-num (1+ line-num))
                       (setq added-count (1+ added-count))))))
                
                (insert "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
                (insert (format "Summary: %d lines added, %d lines removed\n" added-count removed-count))
                (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))
              
              ;; Make buffer read-only
              (read-only-mode 1)
              (goto-char (point-min))
              
              ;; Apply syntax highlighting from original mode
              (when original-buffer
                (let ((mode (buffer-local-value 'major-mode original-buffer)))
                  (when (and mode (not (eq mode 'fundamental-mode)))
                    (funcall mode))))))
          
          ;; Display the diff buffer
          (pop-to-buffer diff-buffer)
          
          ;; Ask for confirmation
          (setq confirmed (yes-or-no-p "Apply Claude's changes to the file? ")))
      
      ;; Clean up
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer)))
    confirmed))

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
             (target-buffer (current-buffer))
             (saved-point (point))
             (saved-window-start (window-start)))
        (claude-chat-update-status "📋 Preparing prompt...")
        ;; Call Claude asynchronously
        (claude-chat-call-claude-async 
         prompt
         (lambda (response)
           (let ((new-content (claude-chat-extract-code-from-response response)))
             (claude-chat-update-status "")
             (message "Received response from Claude, preparing diff...")
             ;; Show diff and ask for confirmation if enabled
             (if (or (not claude-chat-confirm-changes)
                     (claude-chat-show-diff-and-confirm file-content new-content 
                                                        (or file-name "untitled")
                                                        target-buffer))
                 (progn
                   (claude-chat-update-status "✏️ Applying changes...")
                   (with-current-buffer target-buffer
                     (let ((old-size (buffer-size)))
                       (erase-buffer)
                       (insert new-content)
                       ;; Restore cursor position
                       (goto-char (min saved-point (point-max)))
                       ;; Restore window position if visible
                       (when (get-buffer-window target-buffer)
                         (set-window-start (get-buffer-window target-buffer) 
                                           (min saved-window-start (point-max))))))
                   (claude-chat-update-status "")
                   (message "Changes applied successfully"))
               ;; User rejected changes
               (message "Changes rejected - buffer unchanged")))))))))

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
