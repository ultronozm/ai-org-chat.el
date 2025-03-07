;;; ai-org-chat.el --- Threaded chat with AI agent in org buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/ai-org-chat.el
;; Package-Requires: ((emacs "29.1") (llm "0.17.0") (ace-window "0.10.0"))
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

(defvar ai-org-chat-provider 'gptel
  "The LLM provider to use for AI chat.
This should be either
- the symbol `gptel', or
- an instance of an LLM provider created using the `llm' package.")

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

Use double spaces between sentences (an Emacs convention)."
  "System message to use, if any."
  :type '(choice string (const nil)))

(defcustom ai-org-chat-dir "~/gpt"
  "Directory for storing files created by `ai-org-chat-new'."
  :type 'string)

(defvar ai-org-chat-minor-mode)

(defconst ai-org-chat-local-variables
  "# -*- eval: (ai-org-chat-minor-mode 1); -*-\n"
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

;;; Collecting messages

(defun ai-org-chat--get-entry-text ()
  "Get text of current entry, excluding properties drawer."
  (let* ((content (save-excursion
                    (org-back-to-heading)
                    (buffer-substring-no-properties
                     (point)
                     (save-excursion
                       (outline-next-heading)
                       (point)))))
         (lines (split-string content "\n"))
         (drawer-start (cl-position-if
                        (lambda (line)
                          (string-match-p "^[ \t]*:PROPERTIES:[ \t]*$" line))
                        lines))
         (drawer-end (cl-position-if
                      (lambda (line)
                        (string-match-p "^[ \t]*:END:[ \t]*$" line))
                      lines))
         (lines-without-properties (if (and drawer-start drawer-end)
                                       (append
                                        (cl-subseq lines 0 drawer-start)
                                        (cl-subseq lines (1+ drawer-end)))
                                     lines))
         (lines-without-properties-or-heading
          (cdr lines-without-properties)))
    (mapconcat 'identity lines-without-properties-or-heading "\n")))

(defun ai-org-chat--get-entry-heading-and-text ()
  "Get cons cell of current entry's clean heading and text."
  (cons (org-get-heading t t)
        (ai-org-chat--get-entry-text)))

(defun ai-org-chat--get-conversation-history ()
  "Get list of conversation messages up to current entry.
Each message is a cons cell (heading . body)."
  (let ((messages '()))
    (push (ai-org-chat--get-entry-heading-and-text) messages)
    (save-excursion
      (while (org-up-heading-safe)
        (push (ai-org-chat--get-entry-heading-and-text) messages)))
    messages))

;;; Collecting context and tools entries

(defun ai-org-chat--collected-inherited-properties (property)
  "Get unique values for PROPERTY from current and ancestor nodes."
  (let ((items (org-entry-get-multivalued-property (point-min) property)))
    (save-excursion
      (let ((not-done t))
        (while not-done
          (let ((context
                 (org-entry-get-multivalued-property (point) property)))
            (when context
              (setq items (append items context))))
          (setq not-done (org-up-heading-safe)))))
    (delete-dups items)))

(defun ai-org-chat--collect-context-sources ()
  "Get list of context items from CONTEXT properties up the tree."
  (ai-org-chat--collected-inherited-properties "CONTEXT"))

(defun ai-org-chat--collect-tools ()
  "Get list of tools specified by TOOLS proprerties up the tree."
  (ai-org-chat--collected-inherited-properties "TOOLS"))

;;; Assembling context strings

(declare-function custom-variable-type "cus-edit")

(defcustom ai-org-chat-modes-for-src-blocks '(tex-mode latex-mode LaTeX-mode Texinfo-mode)
  "List of modes for which to use src blocks."
  :type '(repeat symbol))

(defun ai-org-chat--wrap-org (content-plist)
  "Wrap CONTENT-PLIST in an `org-mode' source block."
  (let ((mode (or (plist-get content-plist :mode) 'fundamental-mode))
        (name (plist-get content-plist :name)))
    (if (or (provided-mode-derived-p mode 'prog-mode)
            (memq mode ai-org-chat-modes-for-src-blocks))
        (format "%s\n#+begin_src %s\n%s#+end_src\n"
                (if (string-empty-p name) "" name)
                (plist-get content-plist :language)
                (plist-get content-plist :content))
      (format "%s\n#+begin_example\n%s#+end_example\n"
              (if (string-empty-p name) "" name)
              (plist-get content-plist :content)))))

(defun ai-org-chat--wrap-markdown (content-plist)
  "Wrap CONTENT-PLIST in a markdown code block."
  (let ((name (plist-get content-plist :name)))
    (format "%s\n```%s\n%s```\n"
            (if (string-empty-p name) "" name)
            (plist-get content-plist :language)
            (plist-get content-plist :content))))

(defun ai-org-chat--wrap-xml (content-plist)
  "Wrap CONTENT-PLIST in XML tags following Anthropic's recommendations."
  (let ((name (plist-get content-plist :name)))
    (format "<document>
  <source>%s</source>
  <language>%s</language>
  <document_content>
%s  </document_content>
</document>\n"
            (if (string-empty-p name) "unspecified" name)
            (plist-get content-plist :language)
            (plist-get content-plist :content))))

(defcustom ai-org-chat-content-wrapper #'ai-org-chat--wrap-org
  "Function used to wrap content in appropriate quotation format.
The function should accept a plist with these properties:
- :content  - The text content
- :language - String indicating the content's language
- :name     - String identifying the source
It should return the wrapped content as a string."
  :type '(choice
          (const :tag "Org Mode" ai-org-chat--wrap-org)
          (const :tag "Markdown" ai-org-chat--wrap-markdown)
          (const :tag "XML" ai-org-chat--wrap-xml)
          (function :tag "Custom function")))

(defun ai-org-chat--try-buffer (source)
  "Try to extract content from SOURCE as a buffer."
  (when-let ((buffer (get-buffer source)))
    (with-current-buffer buffer
      (list :name (format "Buffer %s" (buffer-name))
            :mode major-mode
            :language (replace-regexp-in-string
                       "-mode$" "" (symbol-name major-mode))
            :content (buffer-substring-no-properties
                      (point-min) (point-max))))))

(defun ai-org-chat--find-project-file (filename)
  "Find FILENAME in current project."
  (when-let* ((project (project-current))
              (file (seq-find
                     (lambda (f)
                       (string= (file-name-nondirectory f) filename))
                     (project-files project))))
    file))

(defun ai-org-chat--try-file (source)
  "Try to extract content from SOURCE as a file."
  (when (or (file-exists-p source)
            (when-let ((project-file (ai-org-chat--find-project-file source)))
              (setq source project-file)))
    (let ((mode (let ((mode (assoc-default
                             source auto-mode-alist 'string-match)))
                  (if (and mode (symbolp mode))
                      mode
                    'fundamental-mode))))
      (list :name (format "File %s" source)
            :mode mode
            :language (replace-regexp-in-string
                       "-mode$" "" (symbol-name mode))
            :content (with-temp-buffer
                       (insert-file-contents source)
                       (buffer-string))))))

(defun ai-org-chat--try-function (source)
  "Try to extract content from SOURCE as a function."
  (when-let ((func (intern-soft source)))
    (when (functionp func)
      (list :name (format "Generated by function %s" source)
            :mode 'emacs-lisp-mode
            :language "emacs-lisp"
            :content (funcall func)))))

(defcustom ai-org-chat-source-methods
  '(ai-org-chat--try-buffer
    ai-org-chat--try-file
    ai-org-chat--try-function)
  "List of functions that attempt to extract content from a source.
Each function should take a source string and return either nil or a
content-plist with :name, :mode, :language, and :content keys."
  :type '(repeat function)
  :group 'ai-org-chat)

(defun ai-org-chat--extract-source-content (source)
  "Get wrapped content from SOURCE.
SOURCE can be a buffer name, file path, or function name.  Return a
string containing the wrapped content."
  (when-let* ((content-plist
               (or (seq-some (lambda (method) (funcall method source))
                             ai-org-chat-source-methods)
                   (progn (warn "Item %s not found as buffer, file, or function" source)
                          nil))))
    (setf (plist-get content-plist :content)
          (ai-org-chat--ensure-trailing-newline
           (plist-get content-plist :content)))
    (funcall ai-org-chat-content-wrapper content-plist)))

(defun ai-org-chat--assemble-full-context ()
  "Get all context items and concatenate them into a single string."
  (let* ((items (ai-org-chat--collect-context-sources))
         (contexts (delq nil (mapcar #'ai-org-chat--extract-source-content items))))
    (when contexts
      (concat "Context:\n\n"
              (string-join contexts "\n")))))

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
         (tools (and (llm-chat-prompt-p prompt)
                     (llm-chat-prompt-tools prompt)))
         ;; Disable streaming if tools are present
         (streaming-p (and ai-org-chat-streaming-p (null tools)))
         (partial-cb (when streaming-p
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
    (if streaming-p
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

(defun ai-org-chat--create-logging-tool (tool marker)
  "Create a version of TOOL that logs its calls and results at MARKER.
TOOL is an llm-tool-function object.  Returns a new llm-tool-function
object with logging behavior added."
  (let* ((orig-func (llm-tool-function-function tool))
         (tool-marker (make-marker)))
    (set-marker tool-marker (marker-position marker))
    (set-marker-insertion-type tool-marker t)
    (let ((wrapped-func
           (lambda (&rest args)
             (let ((result nil))
               (let ((wrapped-callback
                      (lambda (r)
                        (setq result r)
                        (with-current-buffer (marker-buffer tool-marker)
                          (save-excursion
                            (goto-char tool-marker)
                            (insert ":TOOL_CALL:\n"
                                    (json-encode `((name . ,(llm-tool-function-name tool))
                                                   (arguments . ,(cdr args))))
                                    "\n:END:\n")
                            (insert ":TOOL_RESULT:\n"
                                    (format "%s" result)
                                    "\n:END:\n\n")))
                        (funcall (car args) r))))
                 (apply orig-func wrapped-callback (cdr args))
                 result)))))
      (llm-make-tool-function
       :function wrapped-func
       :name (llm-tool-function-name tool)
       :description (llm-tool-function-description tool)
       :args (llm-tool-function-args tool)
       :async (llm-tool-function-async tool)))))

(defun ai-org-chat--create-heading (heading)
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
    (let* ((tools (ai-org-chat--collect-tools))
           (prompt (llm-make-chat-prompt
                    (ai-org-chat--format-messages messages system-context)
                    :context system-context
                    :tools (mapcar (lambda (tool-symbol)
                                     (ai-org-chat--create-logging-tool
                                      (symbol-value (intern tool-symbol))
                                      point))
                                   tools))))
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
         (context (ai-org-chat--assemble-full-context))
         (system-context (concat system "\n" context))
         (messages (ai-org-chat--get-conversation-history))
         (point (save-excursion
                  (ai-org-chat--create-heading ai-org-chat-ai-name)
                  (insert "\n")
                  (save-excursion
                    (ai-org-chat--create-heading ai-org-chat-user-name))
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

(defun ai-org-chat--ensure-trailing-newline (content)
  "Ensure that CONTENT ends with a newline."
  (if (string-match "\n\\'" content)
      content
    (concat content "\n")))

(defun ai-org-chat-new-region (beg end)
  "Start new AI chat, quoting region between BEG and END.
Send user to an AI chat buffer.  Copy current region contents into that buffer."
  (interactive "r")
  (let* ((content (ai-org-chat--ensure-trailing-newline
                   (buffer-substring-no-properties beg end)))
         (region-contents
          (ai-org-chat--wrap-org
           (list :name ""
                 :mode major-mode
                 :language (replace-regexp-in-string
                            "-mode$" "" (symbol-name major-mode))
                 :content content))))
    (ai-org-chat-new-empty)
    (save-excursion
      (newline)
      (insert region-contents))))

;;;###autoload
(defun ai-org-chat-new (arg)
  "Start a new AI chat buffer, optionally including the active region.

If a region is selected, its contents are copied into a new buffer
within an appropriate source block.  Otherwise, creates an empty buffer.

The new buffer is created with a timestamped filename in
`ai-org-chat-dir', and set up with `org-mode' and
`ai-org-chat-minor-mode' enabled.

With prefix argument ARG, immediately call
`ai-org-chat-add-visible-buffers-context' on the new file, with point
positioned at the top of the document."
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

;;; Convenience functions for populating CONTEXT and TOOLS

(defun ai-org-chat--add-context (items)
  "Helper function to add context to the current org node.
ITEMS is a list of strings to add to the context."
  (let* ((current-context
          (org-entry-get-multivalued-property (point) "CONTEXT"))
         (new-items (delete-dups (append current-context items))))
    (apply #'org-entry-put-multivalued-property (point) "CONTEXT" new-items)
    (message "Added %d item(s) to context"
             (length items))))

;;;###autoload
(defun ai-org-chat-add-buffer-context ()
  "Add selected buffers as context for current org node."
  (interactive)
  (let ((selected-buffers
         (completing-read-multiple
          "Select buffers to add to context: "
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
    (message "Added %d visible buffer(s) to context"
             (length buffer-names))))

(defun ai-org-chat-add-file-context ()
  "Add selected files as context for current org node."
  (interactive)
  (let ((selected-files
         (completing-read-multiple
          "Enter file paths to add to context: "
          #'completion-file-name-table)))
    (ai-org-chat--add-context selected-files)))

(defun ai-org-chat-add-function-context ()
  "Add selected function symbols as context for current org node."
  (interactive)
  (let* ((function-symbols
          (completing-read-multiple
           "Select functions to add to context: "
           (let (symbols)
             (mapatoms
              (lambda (sym)
                (when (fboundp sym)
                  (push (symbol-name sym) symbols))))
             symbols))))
    (ai-org-chat--add-context function-symbols)))

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

(defun ai-org-chat-add-directory-files-context (dir &optional wildcard
                                                    recursive)
  "Add files from DIR as context for current org node.
Optional WILDCARD (e.g., \"*.py\") filters files by pattern.
With prefix arg RECURSIVE, include subdirectories recursively."
  (interactive
   (list (read-directory-name "Select directory: ")
         (read-string "File wildcard (optional, e.g., *.py): " nil nil "*")
         current-prefix-arg))
  (let* ((files (directory-files-recursively
                 dir
                 (if (or (null wildcard) (string= wildcard "*"))
                     ".*"
                   (wildcard-to-regexp wildcard))
                 recursive))
         (relative-files
          (mapcar (lambda (file)
                    (file-relative-name file default-directory))
                  files)))
    (ai-org-chat--add-context relative-files)
    (message "Added %d files from directory %s as context (wildcard: %s)"
             (length relative-files)
             dir
             wildcard)))

(defun ai-org-chat-add-project-files-context (dir &optional wildcard)
  "Add files from a selected project as context for current org node.
Prompts for the project to use and excludes the current file.  DIR is
the directory of the selected project.  WILDCARD, if provided, is used
to filter the files (e.g., \"*.py\" for Python files)."
  (interactive
   (list (funcall project-prompter)
         (read-string "File wildcard (optional, e.g., *.py): " nil nil "*")))
  (let* ((project (project-current nil dir))
         (current-file (buffer-file-name))
         (project-vc-include-untracked nil)  ; don't include untracked files
         (project-ignore-filenames-regexp
          (concat "\\."
                  "\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\|tiff\\|ico\\|svg"
                  "\\|pdf\\|doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx"
                  "\\|zip\\|tar\\|gz\\|rar\\|7z"
                  "\\|exe\\|dll\\|so\\|dylib"
                  "\\|pyc\\|pyo\\|pyd"
                  "\\|class\\|jar"
                  "\\|mp3\\|mp4\\|avi\\|mov\\|wav"
                  "\\)$")))
    (if (not project)
        (error "No project found for directory %s" dir)
      (let* ((project-files (project-files project))
             (relative-files
              (mapcar (lambda (file)
                        (file-relative-name file default-directory))
                      project-files))
             (filtered-files
              (seq-remove
               (lambda (file)
                 (or (string-match-p project-ignore-filenames-regexp file)
                     (and current-file
                          (string= file (file-relative-name
                                         current-file default-directory)))))
               relative-files))
             (final-files
              (ai-org-chat--filter-files-by-wildcard filtered-files wildcard)))
        (ai-org-chat--add-context final-files)
        (message "Added %d files from project %s as context (wildcard: %s)"
                 (length final-files)
                 (project-root project)
                 wildcard)))))

(defun ai-org-chat-add-tools ()
  "Add selected tools to the current org node's TOOLS property.
Prompts for tool functions (which must be `llm-tool-function' objects) to add.
Only allows selection of symbols that are bound to `llm-tool-function' objects."
  (interactive)
  (let* ((tool-symbols
          (let (symbols)
            (mapatoms
             (lambda (sym)
               (when (and (boundp sym)
                          (symbol-value sym)
                          (llm-tool-function-p (symbol-value sym)))
                 (push (symbol-name sym) symbols))))
            symbols))
         (selected-tools
          (completing-read-multiple
           "Select tools to add: "
           tool-symbols)))
    (when selected-tools
      (let* ((current-tools
              (org-entry-get-multivalued-property (point) "TOOLS"))
             (new-tools (delete-dups (append current-tools selected-tools))))
        (apply #'org-entry-put-multivalued-property (point) "TOOLS" new-tools)
        (message "Added %d tool(s)" (length selected-tools))))))

;;; Convenience function for creating a new branch

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
        (ai-org-chat--create-heading ai-org-chat-user-name)
      (goto-char (point-max))
      (org-insert-heading t nil t)
      (insert (concat ai-org-chat-user-name))
      (insert "\n"))))

;;; Convenience functions for postprocessing AI responses

;;;###autoload
(defun ai-org-chat-convert-markdown-blocks-to-org ()
  "Convert Markdown style code blocks in current buffer to org."
  (interactive)
  (save-excursion
    (while (re-search-forward
            "^\\([ \t]*\\)```\\([^[:space:]]+\\)?\\(\n\\|\r\\)\\(\\(?:.\\|\n\\)*?\\)\\1```" nil t)
      (let ((indent (match-string 1))
            (lang (match-string 2))
            (code (match-string 4)))
        (replace-match (format "%s#+begin_src %s\n%s%s\n%s#+end_src"
                               indent
                               (or lang "")
                               indent
                               (replace-regexp-in-string
                                (format "^%s" indent)
                                indent
                                (string-trim-right code))
                               indent)
                       t t)))))

;;;###autoload
(defun ai-org-chat-replace-backticks-with-equal-signs ()
  "Replace markdown backtick quotes with `org-mode' verbatim quotes."
  (interactive)
  (query-replace-regexp "`\\([^`]+\\)`" "=\\1="))

;;; Comparison

(declare-function ediff-cleanup-mess "ediff")

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
Return a cons cell (TYPE . NAME) where TYPE is the imenu type and NAME
is the function or variable name.  Return nil if no item is found."
  (let ((found-item nil)
        (found-type nil))
    (cl-labels
        ((search-alist
           (alist current-type)
           (cl-loop for item in alist
                    do (cond
                        ((and (consp item)
                              (consp (cdr item))
                              (not (numberp (cdr item))))
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
    (when-let ((index-alist
                (condition-case nil
                    (ai-org-chat--flatten-imenu-index (imenu--make-index-alist))
                  (imenu-unavailable nil))))
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

(require 'ace-window)

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
  (let ((buffer-names (ai-org-chat--collect-context-sources)))
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

;;; Model selection convenience functions

(defcustom ai-org-chat-models
  '(("sonnet 3.5" .
     (:package llm-claude
               :provider make-llm-claude
               :key-env "ANTHROPIC_KEY"
               :chat-model "claude-3-5-sonnet-20241022"))
    ("haiku 3.5" .
     (:package llm-claude
               :provider make-llm-claude
               :key-env "ANTHROPIC_KEY"
               :chat-model "claude-3-5-haiku-latest"))
    ("o1-preview" .
     (:package llm-openai
               :provider make-llm-openai
               :key-env "OPENAI_KEY"
               :chat-model "o1-preview"))
    ("o1-mini" .
     (:package llm-openai
               :provider make-llm-openai
               :key-env "OPENAI_KEY"
               :chat-model "o1-mini"))
    ("gpt4" .
     (:package llm-openai
               :provider make-llm-openai
               :key-env "OPENAI_KEY"
               :chat-model "gpt-4"))
    ("gpt4o" .
     (:package llm-openai
               :provider make-llm-openai
               :key-env "OPENAI_KEY"
               :chat-model "gpt-4o-2024-08-06"))
    ("gpt4o-mini" .
     (:package llm-openai
               :provider make-llm-openai
               :key-env "OPENAI_KEY"
               :chat-model "gpt-4o-mini"))
    ("opus 3" .
     (:package llm-claude
               :provider make-llm-claude
               :key-env "ANTHROPIC_KEY"
               :chat-model "claude-3-opus-20240229"))
    ("gemini-1.5-pro-latest" .
     (:package llm-gemini
               :provider make-llm-gemini
               :key-env "GEMINI_KEY"
               :chat-model "gemini-1.5-pro-latest"))
    ("gemini-2.0-flash-exp" .
     (:package llm-gemini
               :provider make-llm-gemini
               :key-env "GEMINI_KEY"
               :chat-model "gemini-2.0-flash-exp"))
    ("llama 3.1" .
     (:package llm-ollama
               :provider make-llm-ollama
               :chat-model "llama3.1:latest"))
    ("mistral" .
     (:package llm-ollama
               :provider make-llm-ollama
               :chat-model "mistral:latest")))
  "Alist of LLM models and their configurations for ai-org-chat.
Each entry is of the form (NAME . PLIST) where NAME is a string
identifying the model, and PLIST is a property list with the following keys:
:provider - The function to create the LLM provider
:key-env - The environment variable name for the API key (optional)
:chat-model - The specific model name to use with the provider"
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp)))

(defcustom ai-org-chat-default-model "sonnet 3.5"
  "The default LLM model to use for ai-org-chat.
This should be one of the keys in `ai-org-chat-models'."
  :type 'string)

;;;###autoload
(defun ai-org-chat-select-model (model)
  "Select and configure an LLM model for ai-org-chat.
MODEL is a string key from `ai-org-chat-models'."
  (interactive
   (list (completing-read "Select LLM model: "
                          (mapcar #'car ai-org-chat-models))))
  (let* ((config (alist-get model ai-org-chat-models nil nil #'string=))
         (package (plist-get config :package))
         (provider (plist-get config :provider))
         (key-env (plist-get config :key-env))
         (chat-model (plist-get config :chat-model)))
    (if (not (require package nil t))
        (user-error "Package %s is not available" package)
      (setq ai-org-chat-provider
            (if key-env
                (funcall provider
                         :chat-model chat-model
                         :key
                         (if (fboundp 'exec-path-from-shell-getenv)
                             (exec-path-from-shell-getenv key-env)
                           (getenv key-env)))
              (funcall provider
                       :chat-model chat-model)))
      (message "Selected model for ai-org-chat: %s" model))))

;;; Menu

(defvar ai-org-chat-mode-menu nil
  "Menu for `ai-org-chat-minor-mode'.")

(easy-menu-define ai-org-chat-mode-menu nil
  "Menu for `ai-org-chat-minor-mode'."
  (list "AI Chat"
        ["New Chat" ai-org-chat-new
         :help "Start a new AI chat buffer"]
        ["Branch Conversation" ai-org-chat-branch
         :help "Create a new branch in the conversation"]
        ["Get Response" ai-org-chat-respond
         :help "Get AI response for current entry"]
        ["Compare Source Block" ai-org-chat-compare
         :help "Compare a source block with a selected window using ediff"]
        ["Select Model" ai-org-chat-select-model
         :help "Select and configure an LLM model"]
        "-"
        (list "Context"
              ["Add Buffer Context" ai-org-chat-add-buffer-context
               :help "Add selected buffers"]
              ["Add Visible Buffers Context" ai-org-chat-add-visible-buffers-context
               :help "Add all visible buffers"]
              ["Add File Context" ai-org-chat-add-file-context
               :help "Add selected files"]
              ["Add Function Context" ai-org-chat-add-function-context
               :help "Add selected function symbols"]
              ["Add Directory Files Context" ai-org-chat-add-directory-files-context
               :help "Add files from a directory"]
              ["Add Project Files Context" ai-org-chat-add-project-files-context
               :help "Add files from a project"])
        (list "Tools"
              ["Add Tools" ai-org-chat-add-tools
               :help "Add tool functions to the current node"])
        (list "Format"
              ["Convert Markdown Blocks" ai-org-chat-convert-markdown-blocks-to-org
               :help "Convert Markdown style code blocks to org"]
              ["Replace Backticks" ai-org-chat-replace-backticks-with-equal-signs
               :help "Replace markdown backticks with org-mode verbatim quotes"])))

(defvar ai-org-chat-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [menu-bar ai-chat] (cons "AI Chat" ai-org-chat-mode-menu))
    map)
  "Keymap for `ai-org-chat-minor-mode'.")

;;;###autoload
(define-minor-mode ai-org-chat-minor-mode
  "Toggle `ai-org-chat-minor-mode'.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :lighter " AI-Chat"
  :keymap ai-org-chat-minor-mode-map)

(provide 'ai-org-chat)
;;; ai-org-chat.el ends here
