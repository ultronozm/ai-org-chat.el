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

(declare-function gptel-request "gptel")
(defvar gptel-backend)
(defvar gptel--openai)

(defgroup ai-org-chat nil
  "Threaded chat with AI agent in org buffers."
  :group 'hypermedia)

(defcustom ai-org-chat-provider nil
  "The LLM provider to use for AI chat.
This should be either

- an instance of an LLM provider created using the `llm'
package (e.g., via `make-llm-openai'), or

- the symbol `gptel'."
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

Similarly, DON'T use markdown syntax like `keyword`, but instead org-mode syntax like =keyword=.

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

(defun ai-org-chat--current-heading-and-body ()
  "Return cons cell with heading and body of current entry.
The heading excludes tags and TODO keywords.  The body consists
of all text between the heading and the first subtree, but
excluding the :PROPERTIES: drawer, if any."
  (let* ((heading (org-get-heading t t))
         (body (ai-org-chat--current-body)))
    (cons heading body)))

(defun ai-org-chat--ancestor-messages ()
  "Return list of ancestor messages for the current entry.
Each message is a cons cell (heading . body)."
  (let ((ancestors '()))
    (push (ai-org-chat--current-heading-and-body) ancestors)
    (save-excursion
      (while (org-up-heading-safe)
        (push (ai-org-chat--current-heading-and-body) ancestors)))
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

(defcustom ai-org-chat-ai-name "AI"
  "AI name to insert into buffer."
  :type 'string)

(defun ai-org-chat--format-messages (messages system-context)
  "Format MESSAGES and SYSTEM-CONTEXT according to the selected provider."
  (if (eq ai-org-chat-provider 'gptel)
      (let ((formatted-messages
             (mapcar (lambda (msg)
                       (let ((role (if (equal (car msg) ai-org-chat-ai-name)
                                       (if (and (boundp 'gptel-model)
                                                (string-match-p "gemini" gptel-model))
                                           "model"
                                         "assistant")
                                     "user")))
                         (if (and (boundp 'gptel-model)
                                  (string-match-p "gemini" gptel-model))
                             `((role . ,role)
                               (parts . ((text . ,(cdr msg)))))
                           `((role . ,role)
                             (content . ,(cdr msg))))))
                     messages)))
        (if (and (boundp 'gptel-backend)
                 (boundp 'gptel-openai)
                 (eq gptel-backend gptel--openai))
            (cons `((role . "system")
                    (content . ,system-context))
                  formatted-messages)
          formatted-messages))
    (mapcar #'cdr messages)))

(defun ai-org-chat--call-with-functions (provider prompt buffer point remaining-depth)
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
    (ai-org-chat--call-with-functions
     provider prompt (marker-buffer end) (marker-position end) (1- remaining-depth)))
   (t
    (with-current-buffer (marker-buffer end)
      (save-excursion
        (goto-char end)
        (insert "\nMaximum recursive depth reached or unknown response type"))))))

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

(defvar ai-org-chat-max-recursion-depth 10
  "Maximum number of recursive calls allowed in ai-org-chat queries.")

(defun ai-org-chat--get-response (messages point system-context)
  "Get response from the selected backend.
MESSAGES is the list of conversation messages.
POINT is where to insert the response.
SYSTEM-CONTEXT is the system message with context."
  (if (eq ai-org-chat-provider 'gptel)
      (progn
        (unless (featurep 'gptel)
          (require 'gptel))
        (gptel-request
         (ai-org-chat--format-messages messages system-context)
         :position point
         :stream t
         :in-place t))
    (let ((prompt (llm-make-chat-prompt
                   (ai-org-chat--format-messages messages system-context)
                   :context system-context
                   :functions (mapcar (lambda (tool-symbol)
                                        (ai-org-chat--wrap-function
                                         (symbol-value (intern tool-symbol))
                                         point))
                                      (ai-org-chat--get-tools)))))
      (ai-org-chat--call-with-functions
       ai-org-chat-provider
       prompt
       (marker-buffer point)
       (marker-position point)
       ai-org-chat-max-recursion-depth))))

(defun ai-org-chat-respond ()
  "Insert response from AI after current heading in org buffer."
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
                  (point-marker))))
    (ai-org-chat--get-response messages point system-context)))

;;; Setting up new chats

(defun ai-org-chat-new-empty ()
  "Create new AI chat buffer.
Create org buffer with timestamped filename and set it up for AI chat."
  (interactive)
  (let* ((dir ai-org-chat-dir)
         (file (format-time-string "%Y%m%dT%H%M%S--ai-chat.org"))
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

(defcustom ai-org-chat-modes-for-src-blocks '(tex-mode latex-mode LaTeX-mode Texinfo-mode)
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
(defun ai-org-chat-new (arg)
  "Start a new AI chat buffer, optionally including the active region.

If a region is selected, its contents are copied into a new buffer
within an appropriate source block.  Otherwise, creates an empty buffer.

The new buffer is created with a timestamped filename in `ai-org-chat-dir',
and set up with `org-mode' and `ai-org-chat-minor-mode' enabled.

With prefix argument ARG, immediately call `ai-org-chat-add-visible-buffers-context'
on the new file, with point positioned at the top of the document."
  (interactive "P")
  (let ((original-buffer (current-buffer)))
    (if (region-active-p)
        (ai-org-chat-new-region (region-beginning) (region-end))
      (ai-org-chat-new-empty))
    (when arg
      (save-excursion
        (goto-char (point-min))
        (ai-org-chat-add-visible-buffers-context)
        (ai-org-chat--add-context (list (buffer-name original-buffer)))))))

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

(defun ai-org-chat--buffer-contents (buf point-functions)
  "Use POINT-FUNCTIONS to extract contents of buffer BUF.
Here POINT-FUNCTIONS is a list of two functions that should return the
beginning and end of the desired region, respectively."
  (with-current-buffer buf
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

(defun ai-org-chat--get-tools ()
  "Get list of tools specified for current and ancestor nodes."
  (let ((items (org-entry-get-multivalued-property (point-min) "TOOLS")))
    (save-excursion
      (let ((not-done t))
        (while not-done
          (let ((context
                 (org-entry-get-multivalued-property (point) "TOOLS")))
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
        (window-buffer window)
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

(defun ai-org-chat--filter-files-by-wildcard (files wildcard)
  "Filter FILES by WILDCARD pattern.
If WILDCARD is nil or \"*\", return all FILES.
Otherwise, return only files matching the WILDCARD pattern."
  (if (or (null wildcard) (string= wildcard "*"))
      files
    (let ((regexp (wildcard-to-regexp wildcard)))
      (seq-filter (lambda (file)
                    (string-match-p regexp file))
                  files))))

(defun ai-org-chat-add-project-files-context (dir &optional wildcard)
  "Add files from a selected project as context for current org node.
Prompts for the project to use and excludes the current file.  DIR is
the directory of the selected project.  WILDCARD, if provided, is used
to filter the files (e.g., \"*.py\" for Python files)."
  (interactive
   (list (funcall project-prompter)
         (read-string "File wildcard (optional, e.g., *.py): " nil nil "*")))
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
                      relative-files))
             (final-files
              (ai-org-chat--filter-files-by-wildcard filtered-files wildcard)))
        (ai-org-chat--add-context final-files)
        (message "Added %d files from project %s as context (wildcard: %s)"
                 (length final-files)
                 (project-root project)
                 wildcard)))))

;;; Convenience

;;;###autoload
(defun ai-org-chat-branch ()
  "Create new chat branch.

This function creates a new branch in the AI chat conversation by
inserting a new user heading.  It behaves as follows:

- If there's a parent AI heading, insert a new user heading below it.
- If there are no parent AI headings (e.g., we're at the beginning of the
  buffer), insert a new top-level user heading at the end of the buffer.

This allows for creating new conversation threads or continuing existing
ones."
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

(defun ai-org-chat-replace-backticks-with-equal-signs ()
  "Replace markdown backtick quotes with `org-mode' verbatim quotes."
  (interactive)
  (query-replace-regexp "`\\([^`]+\\)`" "=\\1="))

;;; Comparison

(declare-function ediff-cleanup-mess "ediff")
(declare-function ace-window "ace-window")

(defun ai-org-chat--single-defun-p ()
  "Check if the current buffer contains a single defun.
Returns non-nil if a single defun is found, nil otherwise."
  (save-excursion
    (goto-char (point-max))
    (beginning-of-defun)
    (and (= (point) (point-min))
         (< (point) (point-max)))))

(require 'imenu)

(defun ai-org-chat--search-imenu-alist (alist pos)
  "Search the imenu ALIST for item closest to but not after POS.
Returns a cons cell (TYPE . NAME) where TYPE is the imenu type and
NAME is the function or variable name. Returns nil if no item is found."
  (let ((found-item nil)
        (found-type nil))
    (cl-labels ((search-alist
                  (alist current-type)
                  (cl-loop for item in alist
                           do (cond
                               ((and (consp item) (consp (cdr item)) (not (numberp (cdr item))))
                                (let ((result (search-alist (cdr item) (car item))))
                                  (when result (cl-return result))))
                               ((and (consp item) (number-or-marker-p (cdr item))
                                     (<= (cdr item) pos)
                                     (or (null found-item)
                                         (> (cdr item) (cdr found-item))))
                                (setq found-item item
                                      found-type current-type))))))
      (search-alist alist nil))
    (when found-item
      (cons found-type (car found-item)))))

(defun ai-org-chat--extract-defun-signature ()
  "Extract the signature of the function or variable definition at point.
Uses imenu to analyze the buffer and find the current definition.
Returns a cons cell (TYPE . NAME) where TYPE is the imenu type (e.g.,
\"Variables\" or nil) and NAME is the function or variable name.
Returns nil if no definition is found or if imenu is unavailable."
  (require 'imenu)
  (imenu-flush-cache)
  (condition-case err
      (let* ((index-alist (imenu--make-index-alist))
             (pos (point)))
        (ai-org-chat--search-imenu-alist index-alist pos))
    (imenu-unavailable
     (message "Imenu is unavailable in this buffer")
     nil)
    (error
     (message "An error occurred: %s" (error-message-string err))
     nil)))

(defun ai-org-chat--flatten-imenu-index (index)
  "Flatten the imenu INDEX into a single list of items.

Each item in the returned list is a cons cell of the form:
 (TYPE . (NAME . POSITION))

where:
- TYPE is a string representing the hierarchy (e.g., \"Functions/Helpers\")
- NAME is the item name
- POSITION is the buffer position of the item

The TYPE string uses \"/\" as a separator for nested categories.
Top-level items have TYPE set to nil.

This function recursively traverses the imenu index structure,
preserving the hierarchical information in the TYPE field."
  (let ((result '()))
    (cl-labels ((flatten
                  (items prefix)
                  (dolist (item items)
                    (if (imenu--subalist-p item)
                        (let ((new-prefix (if prefix
                                              (concat prefix "/" (car item))
                                            (car item))))
                          (flatten (cdr item) new-prefix))
                      (unless (equal (car item) "*Rescan*")
                        (push (cons prefix (cons (car item) (cdr item))) result))))))
      (flatten index nil))
    (nreverse result)))

(defun ai-org-chat--match-signature-in-buffer (signature buffer)
  "Find a definition in BUFFER matching the given SIGNATURE.
SIGNATURE should be a cons cell (TYPE . NAME) where:
  TYPE is the imenu type (e.g., \"Variables\", \"Functions\", or nil)
  NAME is the function or variable name.
BUFFER is the buffer to search in.

Returns a list (BUFFER START END) if a match is found, where:
  START is the beginning position of the matched definition
  END is the ending position of the matched definition
Returns nil if no match is found."
  (with-current-buffer buffer
    (imenu-flush-cache)
    (let ((index-alist
           (ai-org-chat--flatten-imenu-index (imenu--make-index-alist))))
      (cl-loop for item in index-alist
               when (and (equal (car item) (car signature))
                         (equal (cadr item) (cdr signature)))
               return (list buffer
                            (cddr item)
                            (save-excursion
                              (goto-char (cddr item))
                              (end-of-defun)
                              (point)))))))

(defun ai-org-chat--find-matching-defun (signature buffers)
  "Find definition matching SIGNATURE in BUFFERS.
SIGNATURE should be a cons cell (TYPE . NAME) as returned by
`ai-org-chat--extract-defun-signature'.
BUFFERS is a list of buffers to search for the matching definition.

Returns the first matching result from `ai-org-chat--match-signature-in-buffer',
which is a list (BUFFER START END), or nil if no match is found."
  (cl-loop for buf in buffers
           for match = (ai-org-chat--match-signature-in-buffer signature buf)
           when match return match))

(defun ai-org-chat--setup-ediff (buf1 buf2 &optional narrowing-info)
  "Set up an ediff session comparing BUF1 and BUF2.
BUF1 and BUF2 are the buffers to be compared.
Optional NARROWING-INFO is a list (START END) for narrowing BUF1.

This function handles:
- Setting up the ediff session
- Applying narrowing to BUF1 if NARROWING-INFO is provided
- Adding a quit hook to restore original buffer states
- Cleaning up the ediff mess and closing the tab when done"
  (let ((original-narrowing-start
         (with-current-buffer buf1
           (point-min-marker)))
        (original-narrowing-end
         (with-current-buffer buf1
           (point-max-marker))))
    (set-marker-insertion-type original-narrowing-start nil)
    (set-marker-insertion-type original-narrowing-end t)
    (when narrowing-info
      (with-current-buffer buf1
        (narrow-to-region (nth 0 narrowing-info) (nth 1 narrowing-info))))
    (let ((ediff-buf (ediff-buffers buf1 buf2)))
      (with-current-buffer ediff-buf
        (add-hook 'ediff-quit-hook
                  (lambda ()
                    (when (and (buffer-live-p buf1)
                               original-narrowing-start
                               original-narrowing-end)
                      (with-current-buffer buf1
                        (widen)
                        (narrow-to-region original-narrowing-start
                                          original-narrowing-end)))
                    (when (buffer-live-p buf2)
                      (with-current-buffer buf2
                        (org-edit-src-exit)))
                    (ediff-cleanup-mess)
                    (tab-bar-close-tab))
                  nil t)))))

(defun ai-org-chat--compare-impl (src-buf aux-bufs)
  "Implement comparison logic for SRC-BUF against AUX-BUFS and visible buffers.
SRC-BUF is the buffer containing the source code to be compared.
AUX-BUFS is a list of auxiliary buffers to search for matching definitions.

This function:
1. Checks if SRC-BUF contains a single definition
2. If so, tries to find a matching definition in AUX-BUFS and visible buffers
3. Sets up an ediff session for the matched definitions or whole buffers
4. Handles window management for the comparison"
  (with-current-buffer src-buf
    (goto-char (point-min))
    (let ((comparison-set-up nil)
          (visible-buffers (seq-uniq
                            (mapcar #'window-buffer
                                    (seq-remove
                                     (lambda (window)
                                       (eq (window-buffer window) src-buf))
                                     (window-list))))))
      (when (ai-org-chat--single-defun-p)
        (when-let* ((signature (ai-org-chat--extract-defun-signature))
                    (matching-info (ai-org-chat--find-matching-defun
                                    signature
                                    (seq-uniq
                                     (append
                                      (seq-remove
                                       (lambda (buf) (eq src-buf buf))
                                       aux-bufs)
                                      visible-buffers)))))
          (ai-org-chat--setup-ediff (nth 0 matching-info) src-buf
                                    (list (nth 1 matching-info)
                                          (nth 2 matching-info)))
          (setq comparison-set-up t)))

      (unless comparison-set-up
        (require 'ace-window)
        (let* ((all-candidate-buffers (seq-uniq (append visible-buffers aux-bufs)))
               (buf-to-compare
                (if (= (length all-candidate-buffers) 1)
                    (car all-candidate-buffers)
                  (if (> (length visible-buffers) 0)
                      (if (> (length visible-buffers) 1)
                          (window-buffer
                           (aw-select "Select window for comparison"))
                        (car visible-buffers))
                    (get-buffer
                     (completing-read "Select buffer to compare: "
                                      (mapcar #'buffer-name all-candidate-buffers)
                                      nil t))))))
          (ai-org-chat--setup-ediff buf-to-compare src-buf))))))

(defun ai-org-chat--get-context-buffers ()
  "Get list of buffers specified in CONTEXT properties."
  (let ((buffer-names (ai-org-chat--get-permanent-context-items)))
    (cl-remove-if-not #'identity
                      (mapcar (lambda (name)
                                (or (get-buffer name)
                                    (find-buffer-visiting name)))
                              buffer-names))))

(declare-function org-element-type "org")

;;;###autoload
(defun ai-org-chat-compare ()
  "Compare a source block with a selected window using ediff.
If the source block is a single function or class definition, it tries
to find a matching definition in other visible buffers and compares them
directly."
  (interactive)
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (when (memq type '(src-block example-block))
      (let ((org-src-window-setup 'current-window)
            (aux-bufs (ai-org-chat--get-context-buffers)))
        (condition-case err
            (progn
              (tab-duplicate)
              (org-edit-special)
              (ai-org-chat--compare-impl (current-buffer) aux-bufs))
          (error
           (tab-bar-close-tab)
           (signal (car err) (cdr err))))))))


(provide 'ai-org-chat)
;;; ai-org-chat.el ends here
