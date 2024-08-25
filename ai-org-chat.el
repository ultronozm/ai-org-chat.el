;;; ai-org-chat.el --- Threaded chat with AI agent in org buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/ai-org-chat.el
;; Package-Requires: ((emacs "29.1") (llm "0.17.0"))
;; Keywords: convenience, ai, chat

;; This program is free software; you can redistribute it and/or modify
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

;; This is a simple Emacs package that supports threaded AI chat
;; inside any org-mode buffer.  See the README for configuration and
;; usage instructions.


;;; Code:

(require 'org)
(require 'llm)
(require 'json)

(defgroup ai-org-chat nil
  "Threaded chat with AI agent in org buffers."
  :group 'hypermedia)

(defcustom ai-org-chat-provider nil
  "The LLM provider to use for AI chat.
This should be an instance of an LLM provider created using the `llm'
package (e.g., via `make-llm-openai')."
  :type 'symbol)

(defcustom ai-org-chat-user-name "User"
  "User name to insert into buffer."
  :type 'string)

(defcustom ai-org-chat-ai-name "AI"
  "AI name to insert into buffer."
  :type 'string)

(defcustom ai-org-chat-system-message
  "You are a brilliant and helpful assistant living in Emacs.

When quoting source blocks, DON'T USE MARKDOWN SYNTAX, but instead use Emacs org-mode syntax, e.g.
#+begin_src elisp
  (number-sequence 0 9)
#+end_src

Similarly, when mentioning keywords, DON'T use markdown syntax like `keyword`, but instead org-mode syntax like =keyword=.

You can directly affect the user's computing environment by requesting that the user execute elisp or sh source blocks.

Look at the system message LAST -- it contains the most up-to-date contents, more recent than the previous chat messages.

Use double spaces between sentences (an Emacs convention).
"
  "System message to use, if any."
  :type '(choice string (const nil)))

(defcustom ai-org-chat-dir "~/gpt"
  "Directory for storing files created by `ai-org-chat-new'."
  :type 'string)

;;;###autoload
(define-minor-mode ai-org-chat-minor-mode
  "Toggle `ai-org-chat-minor-mode'.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            map))

(defconst ai-org-chat-local-variables
  "# -*- eval: (ai-org-chat-minor-mode 1); -*-
"
  "Local variables to insert into new AI chat buffers.")

;;;###autoload
(defun ai-org-chat-setup-buffer ()
  "Set up the current buffer for AI chat.
Ensure the buffer is in `org-mode', enable `ai-org-chat-minor-mode',
insert local variables, and add initial heading."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Buffer must be in org-mode to set up AI chat"))
  (ai-org-chat-minor-mode)
  (goto-char (point-min))
  (insert ai-org-chat-local-variables)
  (ai-org-chat-branch))

;;; Message collection

(defun ai-org-chat--current-body ()
  "Return text of current entry, excluding children and properties."
  (let* ((content (save-excursion
                    (org-back-to-heading)
                    (buffer-substring-no-properties
                     (point)
                     (save-excursion
                       (outline-next-heading)
                       (point)))))
         (lines (split-string content "\n"))
         (beg-re "^[ \t]*:PROPERTIES:[ \t]*$")
         (end-re "^[ \t]*:END:[ \t]*$")
         (beg (cl-position-if (apply-partially #'string-match-p beg-re) lines))
         (end (cl-position-if (apply-partially #'string-match-p end-re) lines))
         (newlines (cdr (if (and beg end)
                            (append (cl-subseq lines 0 beg)
                                    (cl-subseq lines (1+ end)
                                               (length lines)))
                          lines))))
    (mapconcat 'identity newlines "\n")))

(defun ai-org-chat--ancestor-messages ()
  "Return list of ancestor messages for the current entry."
  (let ((ancestors '()))
    (push (ai-org-chat--current-body) ancestors)
    (save-excursion
      (while (org-up-heading-safe)
        (push (ai-org-chat--current-body) ancestors)))
    ancestors))

;;; Response generation

(defcustom ai-org-chat-streaming-p t
  "Whether to use streaming responses if available.
If the LLM provider doesn't support streaming, then this will
have no effect."
  :type 'boolean)

(defun ai-org-chat--insert-text (start end response)
  "Insert RESPONSE at START, updating END marker."
  (with-current-buffer (marker-buffer start)
    (cond
     ((stringp response)
      (save-excursion
        (goto-char start)
        (let* ((current-text (buffer-substring-no-properties start end))
               (common-prefix (fill-common-string-prefix
                               current-text response))
               (prefix-length (length common-prefix)))
          (when (> prefix-length 0)
            (goto-char (+ start prefix-length)))
          (delete-region (point) end)
          (insert (substring response prefix-length)))))
     ((listp response)
      (message "Response is a list: %s" response)))))

(defun ai-org-chat--handle-final-response (prompt response start end remaining-depth provider)
  "Handle the final RESPONSE from the LLM.
PROMPT is the original prompt used for the query.
START and END are markers for insertion.
REMAINING-DEPTH determines how many more recursive calls are allowed.
PROVIDER is the LLM service provider."
  (cond
   ((stringp response)
    (ai-org-chat--insert-text start end response))
   ((and (listp response) (> remaining-depth 0))
    (ai-org-chat-call-with-functions
     provider prompt (marker-buffer end) (marker-position end) (1- remaining-depth)))
   (t
    (with-current-buffer (marker-buffer end)
      (save-excursion
        (goto-char end)
        (insert "\nMaximum recursive depth reached or unknown response type"))))))

(defun ai-org-chat-call-with-functions (provider prompt buffer point remaining-depth)
  "Call the LLM, optionally streaming output to buffer.
PROVIDER supplies the LLM service.  PROMPT is the input for the LLM.
BUFFER and POINT specify where to insert the response.
REMAINING-DEPTH determines how many more recursive calls are allowed."
  (let* ((start (with-current-buffer buffer (copy-marker point nil)))
         (end (with-current-buffer buffer (copy-marker point t)))
         (captured-buffer buffer)
         (partial-cb (when ai-org-chat-streaming-p
                       (lambda (response)
                         (ai-org-chat--insert-text start end response))))
         (final-cb
          (lambda (response)
            (ai-org-chat--handle-final-response
             prompt response start end remaining-depth provider)))
         (error-cb
          (lambda (err msg)
            (with-current-buffer captured-buffer
              (goto-char end)
              (insert (format "\nError: %s - %s\n" err msg)))
            (ai-org-chat--handle-final-response
             prompt (format "Error: %s - %s" err msg) start end remaining-depth provider))))
    (if ai-org-chat-streaming-p
        (llm-chat-streaming provider prompt partial-cb final-cb error-cb)
      (llm-chat-async provider prompt final-cb error-cb))))

(defun ai-org-chat--insert-function-call (func args)
  "Insert FUNC call with ARGS into the current org buffer."
  (insert "\n:FUNCTION_CALL:\n"
          (json-encode `((name . ,(llm-function-call-name func))
                         (arguments . ,args)))
          "\n:END:\n"))

(defun ai-org-chat--insert-function-result (result)
  "Insert RESULT of a function call into the current org buffer."
  (insert "\n:FUNCTION_RESULT:\n"
          (format "%s" result)
          "\n:END:\n\n"))

(defun ai-org-chat--wrapped-function-call (orig-func func marker args)
  "Call ORIG-FUNC with ARGS, logging call and result at MARKER.
FUNC is the llm-function-call object."
  (let ((result (apply orig-func args)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (ai-org-chat--insert-function-call func args)
        (ai-org-chat--insert-function-result result)
        (set-marker marker (point))))
    result))

(defun ai-org-chat--wrap-function (func marker)
  "Wrap FUNC to log its calls and results in the org buffer at MARKER."
  (let* ((orig-func (llm-function-call-function func))
         (wrapped-func
          (lambda (&rest args)
            (let ((result (apply orig-func args)))
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char (marker-position marker))
                  (ai-org-chat--insert-function-call func args)
                  (ai-org-chat--insert-function-result result)
                  (set-marker marker (point))))
              result)
            ;; (ai-org-chat--wrapped-function-call orig-func func marker args)
            )))
    (make-llm-function-call
     :function wrapped-func
     :name (llm-function-call-name func)
     :description (llm-function-call-description func)
     :args (llm-function-call-args func))))

(defun ai-org-chat--new-subtree (heading)
  "Create new subtree with HEADING as heading."
  (org-insert-heading-after-current)
  (insert heading)
  (org-demote-subtree))

(defvar ai-org-chat-max-recursion-depth 3
  "Maximum number of recursive calls allowed in ai-org-chat queries.")

(defun ai-org-chat-respond ()
  "Insert response from llm after current heading in org buffer."
  (interactive)
  (let* ((system ai-org-chat-system-message)
         (context (ai-org-chat--context))
         (system-context (concat system "\n" context))
         (messages (ai-org-chat--ancestor-messages))
         (point (save-excursion
                  (ai-org-chat--new-subtree ai-org-chat-ai-name)
                  (insert "\n")
                  (save-excursion
                    (ai-org-chat--new-subtree ai-org-chat-user-name))
                  (point-marker)))
         (tools (mapcar (lambda (tool-symbol)
                          (ai-org-chat--wrap-function
                           (symbol-value (intern tool-symbol))
                           point))
                        (org-entry-get-multivalued-property (point) "TOOLS")))
         (prompt (llm-make-chat-prompt
                  messages
                  :context system-context
                  :functions tools)))
    (ai-org-chat-call-with-functions
     ai-org-chat-provider
     prompt
     (marker-buffer point)
     (marker-position point)
     ai-org-chat-max-recursion-depth)))

;;; Setting up new chats

(defun ai-org-chat-new-empty ()
  "Create new AI chat buffer.
Create org buffer with timestamped filename and set it up for AI chat."
  (interactive)
  (let* ((dir ai-org-chat-dir)
         (file (format-time-string "gpt-%Y%m%dT%H%M%S.org"))
         (path (expand-file-name file dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (find-file path)
    (ai-org-chat-setup-buffer)))

(defcustom ai-org-chat-region-filter-functions
  '(ai-org-chat--ensure-trailing-newline
    ai-org-chat--enclose-in-src-block)
  "List of functions to call on quoted region contents.
These functions are applied as preprocessing steps to the region passed
to `ai-org-chat-new-region'.  Each function should accept two arguments:
the region as a string, and the major-mode for the buffer from which it
came.  It should return the processed string."
  :type '(repeat function))

(defun ai-org-chat--ensure-trailing-newline (content _mode)
  "Ensure that CONTENT ends with a newline."
  (if (string-match "\n\\'" content)
      content
    (concat content "\n")))

(defcustom ai-org-chat-modes-for-src-blocks '(tex-mode latex-mode LaTeX-mode)
  "List of modes for which to use src blocks."
  :type '(repeat symbol))

(defun ai-org-chat--enclose-in-src-block (content mode)
  "Enclose CONTENT in a src block, if appropriate.
A src block is used if MODE is a programming mode
or belongs to `ai-org-chat-modes-for-src-blocks'."
  (if (or (provided-mode-derived-p mode 'prog-mode)
          (memq mode ai-org-chat-modes-for-src-blocks))
      (let ((mode-name (replace-regexp-in-string
                        "-mode\\'"
                        ""
                        (symbol-name mode))))
        (concat
         (format "#+begin_src %s\n" mode-name)
         content
         "#+end_src"))
    (concat
     "#+begin_example\n"
     content
     "#+end_example")))

(defun ai-org-chat-new-region (beg end)
  "Start new AI chat, quoting region between BEG and END.
Send user to an AI chat buffer.  Copy current region contents
into that buffer, applying the filters in the variable
`ai-org-chat-region-filter-functions'."
  (interactive "r")
  (let ((region-contents
         (buffer-substring-no-properties beg end)))
    (dolist (filter ai-org-chat-region-filter-functions)
      (setq region-contents (funcall filter region-contents major-mode)))
    (ai-org-chat-new-empty)
    (save-excursion
      (newline 2)
      (insert region-contents))))

;;;###autoload
(defun ai-org-chat-new ()
  "Start a new AI chat buffer, optionally including the active region.

If the mark is active, this function copies the region contents into a new
buffer, enclosing them in an appropriate source block.  The source block type
is determined based on the major mode of the original buffer.  This is useful
for starting a conversation about a specific piece of code or text.

The new buffer is created with a timestamped filename in the directory
specified by `ai-org-chat-dir'.  The buffer is set up with org-mode and
`ai-org-chat-minor-mode' enabled.

If no region is active, it creates an empty buffer ready for AI chat interaction.

In both cases, the new buffer is set up with `ai-org-chat-minor-mode' enabled."
  (interactive)
  (if (region-active-p)
      (ai-org-chat-new-region (region-beginning) (region-end))
    (ai-org-chat-new-empty)))

;;; Context

(defcustom ai-org-chat-context-style nil
  "Type of editor context to send to the AI.
This can be either nil, `visible-contents', or `visible-buffers'."
  :type '(choice (const nil) (const visible-contents) (const visible-buffers))
  :local t
  :safe #'symbolp)

(declare-function custom-variable-type "cus-edit")

(defun ai-org-chat-set-context-style ()
  "Set the value of `ai-org-chat-context-style' as a file-local variable.
Using the prop line, at top of file."
  (interactive)
  (require 'wid-edit)
  (require 'cus-edit)
  (let* ((variable 'ai-org-chat-context-style)
         (type (custom-variable-type variable))
         (current-value (if (local-variable-p variable)
                            (buffer-local-value variable (current-buffer))
                          (default-value variable)))
         (prompt (format "Set %s to: " variable))
         (value (widget-prompt-value type prompt current-value nil)))
    ;; Set as file-local variable
    (save-excursion
      (add-file-local-variable-prop-line variable value))
    ;; Set as buffer-local variable
    (set (make-local-variable variable) value)
    (message "Set %s to %s (file-locally and buffer-locally)" variable value)))

(defun ai-org-chat--buffer-contents (window point-functions)
  "Given a WINDOW, use POINT-FUNCTIONS to extract the buffer contents.
Here POINT-FUNCTIONS is a list of two functions that should return the
beginning and end of the desired region, respectively."
  (with-current-buffer (window-buffer window)
    (let* ((beg
            (max
             (point-min)
             (funcall (nth 0 point-functions))))
           (end
            (min
             (point-max)
             (funcall (nth 1 point-functions))))
           (content (buffer-substring-no-properties beg end))
           (name (buffer-name)))
      (format "%s\n%s\n" name
              (ai-org-chat--enclose-in-src-block content major-mode)))))

(defun ai-org-chat--get-permanent-context-items ()
  "Get list of permanent context buffer names or file paths.
Uses current and ancestor nodes."
  (let ((items (org-entry-get-multivalued-property (point-min) "CONTEXT")))
    (save-excursion
      (let ((not-done t))
        (while not-done
          (let ((context
                 (org-entry-get-multivalued-property (point) "CONTEXT")))
            (when context
              (setq items (append items context))))
          (setq not-done (org-up-heading-safe)))))
    (delete-dups items)))

(defun ai-org-chat--get-context-content ()
  "Get context content."
  (let ((windows (seq-remove
                  (lambda (window)
                    (eq (window-buffer window) (current-buffer)))
                  (window-list))))
    ;; remove redundant windows
    (setq windows (seq-uniq
                   windows
                   (lambda (a b)
                     (eq (window-buffer a) (window-buffer b)))))
    ;; remove permanent context buffers
    (setq windows (seq-remove
                   (lambda (window)
                     (member (buffer-name (window-buffer window))
                             (ai-org-chat--get-permanent-context-items)))
                   windows))
    (mapconcat
     (lambda (window)
       (ai-org-chat--buffer-contents
        window
        (if (eq ai-org-chat-context-style 'visible-contents)
            '(window-start window-end)
          '(point-min point-max))))
     windows
     "\n")))

(defun ai-org-chat--get-permanent-context-content ()
  "Get content of permanent context buffers, files, and functions."
  (let ((permanent-items (ai-org-chat--get-permanent-context-items)))
    (mapconcat
     (lambda (item)
       (cond
        ;; Check for existing buffer
        ((get-buffer item)
         (with-current-buffer (get-buffer item)
           (format "%s\n%s\n" item
                   (ai-org-chat--enclose-in-src-block
                    (buffer-substring-no-properties (point-min) (point-max))
                    major-mode))))
        ;; Check for file (absolute path or relative to current directory)
        ((file-exists-p item)
         (ai-org-chat--get-file-content item))
        ;; Check for file in project
        ((and (project-current)
              (let ((file (ai-org-chat--find-file-in-project item)))
                (when file
                  (ai-org-chat--get-file-content file)))))
        ;; Check for Elisp function
        ((functionp (intern-soft item))
         (format "%s\n%s\n" item (funcall (intern-soft item))))
        (t
         (warn "Item %s not found as buffer, file, or function" item)
         nil)))
     permanent-items
     "\n")))

(defun ai-org-chat--find-file-in-project (filename)
  "Find FILENAME in the current project.
Returns the full path if found, nil otherwise."
  (when-let* ((project (project-current))
              (root (project-root project))
              (files (project-files project)))
    (seq-find (lambda (file)
                (string= (file-name-nondirectory file) filename))
              files)))


(defun ai-org-chat--get-file-content (file)
  "Get content of FILE, enclosed in appropriate src block."
  (let* ((mode (ai-org-chat--get-mode-for-file file))
         (content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
    (format "%s\n%s\n" file
            (ai-org-chat--enclose-in-src-block content mode))))

(defun ai-org-chat--get-mode-for-file (filename)
  "Determine the major mode that would be used for FILENAME."
  (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
    (if (and mode (symbolp mode))
        mode
      'fundamental-mode)))

(defun ai-org-chat--context ()
  "Wrap MESSAGE in a system message, adding context if appropriate.
The context depends on the value of `ai-org-chat-context-style'."
  (let ((permanent-context (ai-org-chat--get-permanent-context-content)))
    (concat
     (when (not (string-empty-p permanent-context))
       (format "Selected buffers contents:\n\n%s\n" permanent-context))
     (pcase ai-org-chat-context-style
       ('visible-contents
        (format "Visible buffer contents:\n\n%s\n"
                (ai-org-chat--get-context-content)))
       ('visible-buffers
        (format "Contents of visible buffers:\n\n%s\n"
                (ai-org-chat--get-context-content)))
       (_ "")))))  ; No additional context for other cases


(defun ai-org-chat--add-context (items)
  "Helper function to add context to the current org node.
ITEMS is a list of strings to add to the context."
  (let* ((current-context
          (org-entry-get-multivalued-property (point) "CONTEXT"))
         (new-items (delete-dups (append current-context items))))
    (apply #'org-entry-put-multivalued-property (point) "CONTEXT" new-items)
    (message "Added %d item(s) to permanent context"
             (length items))))

;;;###autoload
(defun ai-org-chat-add-buffer-context ()
  "Add selected buffers as context for current org node."
  (interactive)
  (let ((selected-buffers
         (completing-read-multiple
          "Select buffers to add to permanent context: "
          (mapcar #'buffer-name (buffer-list)))))
    (ai-org-chat--add-context selected-buffers)))

;;;###autoload
(defun ai-org-chat-add-visible-buffers-context ()
  "Add all visible buffers as context for current org node.
Excludes current buffer."
  (interactive)
  (let* ((visible-windows (seq-remove
                           (lambda (window)
                             (eq (window-buffer window) (current-buffer)))
                           (window-list)))
         (visible-buffers (delete-dups
                           (mapcar #'window-buffer visible-windows)))
         (buffer-names (mapcar #'buffer-name visible-buffers)))
    (ai-org-chat--add-context buffer-names)
    (message "Added %d visible buffer(s) to permanent context"
             (length buffer-names))))

(defun ai-org-chat-add-file-context ()
  "Add selected files as context for current org node."
  (interactive)
  (let ((selected-files
         (completing-read-multiple
          "Enter file paths to add to permanent context: "
          #'completion-file-name-table)))
    (ai-org-chat--add-context selected-files)))

(require 'project)

(defun ai-org-chat-add-project-files-context (dir)
  "Add all files from a selected project as context for current org node.
Prompts for the project to use and excludes the current file.  DIR is
the directory of the selected project."
  (interactive (list (funcall project-prompter)))
  (let ((project (project-current nil dir))
        (current-file (buffer-file-name)))
    (if (not project)
        (error "No project found for directory %s" dir)
      (let* ((project-files (project-files project))
             (relative-files
              (mapcar (lambda (file)
                        (file-relative-name file default-directory))
                      project-files))
             (filtered-files
              (remove (and current-file
                           (file-relative-name current-file default-directory))
                      relative-files)))
        (ai-org-chat--add-context filtered-files)
        (message "Added %d files from project %s as context"
                 (length filtered-files)
                 (project-root project))))))

;;; Convenience

;;;###autoload
(defun ai-org-chat-branch ()
  "Create new chat branch.
Find the first parent AI heading, and insert a new user heading
below that, except when there are no parent AI headings (e.g.,
we're at (point-min)), in which case insert a new top-level user
heading."
  (interactive)
  (let ((not-at-top t))
    (while
        (and
         (not (equal (org-get-heading t t) ai-org-chat-ai-name))
         (setq not-at-top (org-up-heading-safe))))
    (if not-at-top
        (ai-org-chat--new-subtree ai-org-chat-user-name)
      (goto-char (point-max))
      (org-insert-heading t nil t)
      (insert (concat ai-org-chat-user-name))
      (insert "\n"))))

(declare-function ediff-cleanup-mess "ediff")
(declare-function ace-window "ace-window")

;;;###autoload
(defun ai-org-chat-compare ()
  "Compare a source block with a selected window using ediff.
This function duplicates the current tab, opens the source block at
point in a separate buffer, prompts the user to select a window, and
then runs ediff to compare the source block buffer with the selected
window's buffer.  When ediff is finished, it automatically closes the
source buffer and the duplicated tab."
  (interactive)
  (require 'ace-window)
  (let ((org-src-window-setup 'current-window))
    (tab-duplicate)
    (condition-case err
        (progn
          (org-edit-special)
          (let ((buf2 (current-buffer))
                (ai-window (selected-window)))
            (when (> (count-windows) 2)
              (call-interactively #'ace-window))
            (delete-window ai-window)
            (let ((buf1 (current-buffer)))
              (let ((ediff-buf (ediff-buffers buf1 buf2)))
                (with-current-buffer ediff-buf
                  (add-hook 'ediff-quit-hook
                            (lambda ()
                              (when (buffer-live-p buf2)
                                (with-current-buffer buf2
                                  (org-edit-src-exit)))
                              (ediff-cleanup-mess)
                              (tab-bar-close-tab))
                            nil t))))))
      (error
       (tab-bar-close-tab)
       (signal (car err) (cdr err))))))

(defun ai-org-chat-convert-markdown-blocks-to-org ()
  "Convert Markdown style code blocks in current buffer to org."
  (interactive)
  (save-excursion
    (while (re-search-forward
            "```\\([^[:space:]]+\\)?\\(\n\\|\r\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
      (let ((lang (match-string 1))
            (code (match-string 3)))
        (replace-match (format "#+begin_src %s\n%s\n#+end_src"
                               (or lang "")
                               (string-trim-right code))
                       t t)))))


(provide 'ai-org-chat)
;;; ai-org-chat.el ends here
