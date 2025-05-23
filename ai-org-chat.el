;;; ai-org-chat.el --- Threaded chat with AI agent in org buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.2
;; URL: https://github.com/ultronozm/ai-org-chat.el
;; Package-Requires: ((emacs "29.1") (llm "0.17.0") (ace-window "0.10.0") (transient "0.4.0"))
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
;;
;; To get started, use M-x ai-org-chat-select-model to choose an LLM model,
;; then create a new chat with M-x ai-org-chat-new.

;;; Code:

(require 'org)
(require 'llm)
(require 'json)
(require 'transient)

(declare-function org-element-lineage "org-element" (object types &optional with-self))
(declare-function org-element-begin "org-element" (element))
(declare-function org-element-end "org-element" (element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-context "org-element" (&optional element))

(defgroup ai-org-chat nil
  "Threaded chat with AI agent in org buffers."
  :group 'hypermedia)

(defvar ai-org-chat-provider nil
  "The LLM provider to use for AI chat.
This should be an instance of an LLM provider created using the
`llm' package.")

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

You can directly affect the user's computing environment by requesting that the user execute elisp or sh source blocks.  When using elisp, better to code blocks that can be evaluated directly than functions that must be executed manually; I DON'T want a bunch of junk named functions (defun fix-foo-blah ...) polluting my namespace.  Just let-bind temporary lambda's, instead.  DON'T provide code blocks when it's inappropriate, e.g., if you're replacing huge amounts of text (rather than just making small modifications); in such cases, just give that text in a source block.

Look at the system message LAST -- it contains the most up-to-date contents, more recent than the previous chat messages.

Use double spaces between sentences (an Emacs convention)."
  "System message to use, if any."
  :type '(choice string (const nil)))

(defcustom ai-org-chat-tools nil
  "List of available tools.

Each tool should be a `llm-tool' struct.  The `:name' field of each
tool is used to reference it in the :TOOLS: property of org headings."
  :group 'ai-org-chat
  :type '(repeat llm-tool))

(defcustom ai-org-chat-fold-tool-drawers t
  "Whether to automatically fold tool-related property drawers.
When non-nil, the :TOOL_CALL: and :TOOL_RESULT: property drawers
are folded automatically when inserted."
  :group 'ai-org-chat
  :type 'boolean)

(defcustom ai-org-chat-dir "~/ai-chats"
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

(defun ai-org-chat--clean-text (text)
  "Clean TEXT by removing protective commas before org headings.
This function removes commas that were added by
`ai-org-chat--process-response-text' to protect `org-mode' structure:
- removes comma if text starts with ',*'
- removes comma in all occurrences of '\n,*'

This is used when preparing conversation history to send to the LLM."
  (let ((cleaned-text
         (if (string-match-p "\\`,[*]" text)
             (substring text 1)
           text)))
    (replace-regexp-in-string "\n,\\(*\\)" "\n\\1" cleaned-text)))

(defcustom ai-org-chat-enable-images t
  "Whether to enable image processing in AI chat conversations.
When non-nil, standalone images in the conversation will be included in
the context sent to the AI model.  When nil, images will be treated as
regular `org-mode' links."
  :type 'boolean
  :group 'ai-org-chat)

(defcustom ai-org-chat-enable-pdfs t
  "Whether to enable PDF processing in AI chat conversations.
When non-nil, standalone PDF links in the conversation will be included
in the context sent to the AI model.  When nil, PDFs will be treated as
regular `org-mode' links."
  :type 'boolean
  :group 'ai-org-chat)

(defun ai-org-chat--split-entry-content (beg end)
  "Split org content between BEG and END into a list of text and media parts.
Only includes images and PDFs that appear standalone (on their own line with
surrounding whitespace lines).  Images are only processed if
`ai-org-chat-enable-images' is non-nil, and PDFs if `ai-org-chat-enable-pdfs'
is non-nil."
  (let ((ai-org-chat-enable-pdfs
         (and ai-org-chat-enable-pdfs
              (member 'pdf-input (llm-capabilities ai-org-chat-provider))))
        (ai-org-chat-enable-images
         (and ai-org-chat-enable-images
              (member 'image-input (llm-capabilities ai-org-chat-provider)))))
    (if (not (or ai-org-chat-enable-images ai-org-chat-enable-pdfs))
        (list `(:text ,(buffer-substring-no-properties beg end)))
      (let ((parts nil)
            (pos beg)
            (file-extension-re (concat (if ai-org-chat-enable-images
                                           (image-file-name-regexp)
                                         "\\`\\`") ; never matches
                                       (if ai-org-chat-enable-pdfs
                                           "\\|\\.pdf\\'"
                                         ""))))
        (save-excursion
          (goto-char beg)
          (while (re-search-forward "\\[\\[\\(?:file\\|attachment\\):" end t)
            (let* ((link (org-element-lineage
                          (org-element-context)
                          'link t))
                   (link-beg (org-element-begin link))
                   (link-end (org-element-end link))
                   (path (org-element-property :path link)))
              (when (and path (string-match-p file-extension-re path))
                (save-excursion
                  (goto-char link-beg)
                  (beginning-of-line)
                  (let ((line-beg (point)))
                    (when (let ((line-content-before (buffer-substring-no-properties
                                                      line-beg link-beg))
                                (line-content-after (buffer-substring-no-properties
                                                     link-end
                                                     (line-end-position)))
                                (prev-line-empty (save-excursion
                                                   (forward-line -1)
                                                   (looking-at-p "^[ \t]*$")))
                                (next-line-empty (save-excursion
                                                   (forward-line 1)
                                                   (looking-at-p "^[ \t]*$"))))
                            ;; Check if the link is standalone
                            (and (string-match-p "^[ \t]*$" line-content-before)
                                 (string-match-p "^[ \t]*$" line-content-after)
                                 prev-line-empty
                                 next-line-empty))
                      (when (< pos line-beg)
                        (let ((text (buffer-substring-no-properties pos line-beg)))
                          (unless (string-match-p "\\`[ \t\n]*\\'" text)
                            (push `(:text ,text) parts))))
                      (let ((full-path (expand-file-name path)))
                        (push (if (string-match-p "\\.pdf\\'" full-path)
                                  `(:pdf ,(with-temp-buffer
                                            (set-buffer-multibyte nil)
                                            (insert-file-contents-literally full-path)
                                            (buffer-string)))
                                `(:image ,(create-image full-path)))
                              parts))
                      (setq pos (save-excursion
                                  (goto-char link-end)
                                  (forward-line 1)
                                  (point)))))))))
          (when (< pos end)
            (let ((text (buffer-substring-no-properties pos end)))
              (unless (string-match-p "\\`[ \t\n]*\\'" text)
                (push `(:text ,text) parts))))
          (nreverse parts))))))

(defcustom ai-org-chat-excluded-drawers '("PROPERTIES" "TOOL_CALL" "TOOL_RESULT" "QUOTED_REGION")
  "List of drawer names that should be excluded from chat messages.
Each entry should be a string without the colon delimiters."
  :type '(repeat string)
  :group 'ai-org-chat)

(defun ai-org-chat--filter-drawers (text excluded-drawers)
  "Filter out drawers listed in EXCLUDED-DRAWERS from TEXT.
EXCLUDED-DRAWERS should be a list of drawer names without colons."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (drawer-re (concat "^[ \t]*:\\("
                             (mapconcat #'regexp-quote excluded-drawers "\\|")
                             "\\):[ \t]*$"))
          (in-code-block nil)
          (code-block-type nil))
      (while (not (eobp))
        (let ((continue-loop t))
          ;; Process the current line
          (cond
           ;; Check for org code blocks (src, example)
           ((looking-at "^[ \t]*#\\+begin_\\(src\\|example\\)")
            (setq in-code-block t
                  code-block-type 'org))

           ((and in-code-block
                 (eq code-block-type 'org)
                 (looking-at "^[ \t]*#\\+end_\\(src\\|example\\)"))
            (setq in-code-block nil
                  code-block-type nil))

           ;; Check for markdown code blocks (triple backticks)
           ((looking-at "^[ \t]*```")
            (if in-code-block
                (when (eq code-block-type 'markdown)
                  (setq in-code-block nil
                        code-block-type nil))
              (setq in-code-block t
                    code-block-type 'markdown)))

           ;; Check for drawers, but only when not in code blocks
           ((and (not in-code-block)
                 (looking-at drawer-re))
            (let ((drawer-start (point)))
              (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                (forward-line 1)
                (delete-region drawer-start (point))
                ;; Go back to the start so we don't skip lines
                (goto-char drawer-start)
                ;; Don't process any further conditions for this iteration
                (setq continue-loop nil)))))

          ;; Only move to next line if we didn't do a special operation
          (when continue-loop
            (forward-line 1)))))
    (buffer-string)))

(defun ai-org-chat--filter-tool-drawers (text)
  "Filter out tool-related drawers from TEXT.
Removes drawers listed in `ai-org-chat-excluded-drawers'."
  (ai-org-chat--filter-drawers text ai-org-chat-excluded-drawers))

(defun ai-org-chat--get-entry-message ()
  "Get message from current entry.
Returns either a string for text-only content or an llm-multipart object
for content with media (images and/or PDFs).  Filters out tool-related
drawers from text content."
  (save-excursion
    (org-back-to-heading)
    (let* ((beg (line-beginning-position 2))
           (end (save-excursion
                  (outline-next-heading)
                  (point)))
           (parts (ai-org-chat--split-entry-content beg end)))
      (if (and (= (length parts) 1)
               (eq (car (car parts)) :text))
          (ai-org-chat--filter-tool-drawers (plist-get (car parts) :text))
        (apply #'llm-make-multipart
               (mapcar (lambda (part)
                         (pcase (car part)
                           (:text (ai-org-chat--filter-tool-drawers (plist-get part :text)))
                           (:image (plist-get part :image))
                           (:pdf (make-llm-media
                                  :mime-type "application/pdf"
                                  :data (plist-get part :pdf)))))
                       parts))))))

(defun ai-org-chat--get-conversation-history ()
  "Get list of conversation messages up to current entry.
Each message is a cons cell (heading . content) where content is
either a string or llm-multipart object."
  (let ((messages nil))
    (save-excursion
      (push (ai-org-chat--get-entry-message) messages)
      (while (org-up-heading-safe)
        (push (ai-org-chat--get-entry-message) messages)))
    messages))

;;; Collecting context and tools entries

(defun ai-org-chat--property-values (property)
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

(defun ai-org-chat--lookup-tool (name)
  "Look up tool by NAME in `ai-org-chat-tools'.
NAME is a string that should match the :name field of a tool."
  (seq-find (lambda (tool)
              (string= name (llm-tool-name tool)))
            ai-org-chat-tools))

(defun ai-org-chat-register-tool (tool)
  "Register TOOL, making it available for use in AI chat.
If a tool with the same name already exists, it is replaced.
TOOL must be a `llm-tool' struct."
  (unless (llm-tool-p tool)
    (error "TOOL must be a llm-tool struct"))
  (setq ai-org-chat-tools
        (cons tool (seq-remove
                    (lambda (existing)
                      (string= (llm-tool-name existing)
                               (llm-tool-name tool)))
                    ai-org-chat-tools))))

(defun ai-org-chat-register-tool-spec (tool)
  "Register tool SPEC, making it available for use in AI chat.
SPEC must be a plist that can be passed to `llm-make-tool'."
  (ai-org-chat-register-tool (apply #'llm-make-tool tool)))

;;;###autoload
(defun ai-org-chat-register-llm-tool-collection (&optional category)
  "Register tools from `llm-tool-collection' for use with `ai-org-chat'.
When CATEGORY is specified, only register tools from that category.
Otherwise, register all available tools."
  (interactive (list (when current-prefix-arg
                       (completing-read "Category: "
                                        (delete-dups
                                         (mapcar (lambda (tool)
                                                   (plist-get tool :category))
                                                 (llm-tool-collection-get-all)))))))
  (require 'llm-tool-collection)
  (declare-function llm-tool-collection-get-all "llm-tool-collection")
  (declare-function llm-tool-collection-get-category "llm-tool-collection")
  (let* ((tools (if category
                    (llm-tool-collection-get-category category)
                  (llm-tool-collection-get-all)))
         (count 0))
    (dolist (tool-spec tools)
      (ai-org-chat-register-tool
       (apply #'llm-make-tool tool-spec))
      (cl-incf count))
    (message "Registered %d tools with ai-org-chat%s"
             count
             (if category (format " from category '%s'" category) ""))))

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
  (when-let* ((buffer (get-buffer source)))
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
            (when-let* ((project-file (ai-org-chat--find-project-file source)))
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
  (when-let* ((func (intern-soft source)))
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

(defun ai-org-chat--ensure-trailing-newline (content)
  "Ensure that CONTENT ends with a newline."
  (if (string-match "\n\\'" content)
      content
    (concat content "\n")))

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

(defun ai-org-chat--property-content (property header)
  "Get content from sources referenced by PROPERTY, formatted with HEADER.
Returns a formatted string with all contents, or nil if none found."
  (let* ((items (ai-org-chat--property-values property))
         (contents (delq nil (mapcar #'ai-org-chat--extract-source-content items))))
    (when contents
      (concat header "\n\n"
              (string-join contents "\n")))))

;;; Response generation

(defcustom ai-org-chat-streaming-p t
  "Whether to use streaming responses if available.
If the LLM provider doesn't support streaming, then this will
have no effect."
  :type 'boolean)

(defun ai-org-chat--escape-asterisks (text escape-first)
  "Core function to escape asterisks in TEXT.
When ESCAPE-FIRST is non-nil, escape asterisk at beginning of TEXT."
  (let ((processed-text
         (if escape-first
             (if (and (> (length text) 0)
                      (= (aref text 0) ?*))
                 (concat "," text)
               text)
           text)))
    (replace-regexp-in-string "\n\\*" "\n,*" processed-text)))

(defun ai-org-chat--process-response-text (text start-pos)
  "Process TEXT to be inserted at START-POS, escaping org headings."
  (let ((escape-first
         (with-current-buffer (marker-buffer start-pos)
           (save-excursion
             (goto-char start-pos)
             (bolp)))))
    (ai-org-chat--escape-asterisks text escape-first)))

(defun ai-org-chat--escape-org-mode-syntax (text)
  "Escape `org-mode' syntax in TEXT."
  (ai-org-chat--escape-asterisks text t))

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
               (prefix-length (length common-prefix))
               (processed-response (ai-org-chat--process-response-text response start)))
          (when (> prefix-length 0)
            (goto-char (+ start prefix-length)))
          (delete-region (point) end)
          (insert (substring processed-response prefix-length)))))
     ;; Silently ignore nil responses from streaming API
     ((and (listp response) response)
      ;; Only log non-nil lists (tool call responses)
      (message "Processing tool response: %s" response)))))

(defcustom ai-org-chat-ai-name "AI"
  "AI name to insert into buffer."
  :type 'string)

(defcustom ai-org-chat-response-finished-functions nil
  "List of functions called after AI response has completed.
Each function is called in the buffer containing the response with two
arguments, the start and end positions of the response."
  :type '(repeat function)
  :group 'ai-org-chat)

(defun ai-org-chat--handle-final-response (prompt response start end remaining-depth provider)
  "Handle the final RESPONSE from the LLM.
PROMPT is the original prompt used for the query.
START and END are markers for insertion.
REMAINING-DEPTH determines how many more recursive calls are allowed.
PROVIDER is the LLM service provider."
  (cond
   ((stringp response)
    (ai-org-chat--insert-text start end response)
    (when ai-org-chat-response-finished-functions
      (with-current-buffer (marker-buffer start)
        (save-excursion
          (run-hook-with-args 'ai-org-chat-response-finished-functions
                              (marker-position start)
                              (marker-position end))))))

   ;; Tool call response - continue conversation after handling tool calls
   ((and (listp response) (> remaining-depth 0))
    ;; For tool calls, we need to recursively restart the conversation
    ;; with the updated prompt that contains tool results
    (ai-org-chat--call-with-functions
     provider prompt (marker-buffer end) (marker-position end) (1- remaining-depth)))
   (t
    (with-current-buffer (marker-buffer end)
      (save-excursion
        (goto-char end)
        (insert "\nMaximum recursive depth reached or unknown response type"))))))

(defun ai-org-chat--call-with-functions (provider prompt buffer point remaining-depth)
  "Call the LLM, optionally streaming output to buffer.
PROVIDER supplies the LLM service.  PROMPT is the input for the LLM.
BUFFER and POINT specify where to insert the response.
REMAINING-DEPTH determines how many more recursive calls are allowed."
  (let* ((start (with-current-buffer buffer (copy-marker point nil)))
         (end (with-current-buffer buffer (copy-marker point t)))
         (captured-buffer buffer)
         (streaming-p ai-org-chat-streaming-p)
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

(defun ai-org-chat--insert-tool-result (tool args result)
  "Insert tool call and result information at point.
TOOL is the tool object, ARGS are the arguments passed to the tool,
and RESULT is the result returned by the tool.

Inserts the tool call and result as property drawers.
When `ai-org-chat-fold-tool-drawers' is non-nil, drawers are folded."
  (unless (bolp) (insert "\n"))
  (let ((tool-call-start (point))
        (json-escaped (ai-org-chat--escape-org-mode-syntax
                       (json-encode
                        `((name . ,(llm-tool-name tool))
                          (arguments . ,(if (llm-tool-async tool)
                                            (cdr args)
                                          args)))))))
    (insert ":TOOL_CALL:\n"
            json-escaped
            "\n:END:\n")
    (let ((tool-result-start (point))
          (escaped-result (ai-org-chat--escape-org-mode-syntax
                           (format "%s" result))))
      (insert ":TOOL_RESULT:\n"
              escaped-result
              "\n:END:\n")
      (unless (looking-at "\n")
        (insert "\n"))
      (when ai-org-chat-fold-tool-drawers
        (save-excursion
          (goto-char tool-call-start)
          (org-fold-hide-drawer-toggle t)
          (goto-char tool-result-start)
          (org-fold-hide-drawer-toggle t))))))

(defun ai-org-chat--create-logging-tool (tool marker)
  "Create a version of TOOL that logs its calls and results at MARKER.
TOOL is an llm-tool object.  Returns a new llm-tool object with logging
behavior added.

This wraps the original tool function to:
1. Execute the tool with the provided arguments
2. Insert tool call and result at the marker position
3. Return the result so the conversation can continue
4. Properly handle and log any errors that occur during tool execution"
  (let* ((orig-func (llm-tool-function tool))
         (tool-result-insertion-marker (make-marker)))
    (set-marker tool-result-insertion-marker (marker-position marker))
    (set-marker-insertion-type tool-result-insertion-marker t)

    (let ((wrapped-func
           (lambda (&rest args)
             (let ((result nil)
                   (async (llm-tool-async tool)))
               ;; Define a callback that inserts tool results and handles async forwarding
               (let ((wrapped-callback
                      (lambda (r)
                        (setq result r)
                        (with-current-buffer (marker-buffer tool-result-insertion-marker)
                          (save-excursion
                            (goto-char tool-result-insertion-marker)
                            (ai-org-chat--insert-tool-result tool args result)))
                        (when async ; TODO: maybe some error-handling here?
                          (funcall (car args) r)))))
                 (if async
                     (apply orig-func wrapped-callback (cdr args))
                   (funcall
                    wrapped-callback
                    (condition-case err
                        (apply orig-func args)
                      (error
                       (let ((error-message
                              (format "Tool error: %s" (error-message-string err))))
                         error-message)))))
                 result)))))
      (llm-make-tool
       :function wrapped-func
       :name (llm-tool-name tool)
       :description (llm-tool-description tool)
       :args (llm-tool-args tool)
       :async (llm-tool-async tool)))))

(defcustom ai-org-chat-max-recursion-depth 20
  "Maximum number of recursive calls allowed in ai-org-chat queries.
This limits the depth of recursive tool calls to prevent infinite loops.
Higher values allow more complex tool interactions but may increase
processing time and resource usage."
  :type 'integer
  :group 'ai-org-chat)

;;; Convenience functions for populating CONTEXT and TOOLS

(defun ai-org-chat--add-to-property (property items &optional silent)
  "Helper function to add ITEMS to the PROPERTY of the current org node.
ITEMS is a list of strings to add to the property.
When SILENT is non-nil, suppress the default message."
  (let* ((current-values (org-entry-get-multivalued-property (point) property))
         (new-values (delete-dups (append current-values items)))
         (added-count (length items)))
    (apply #'org-entry-put-multivalued-property (point) property new-values)
    (unless silent
      (message "Added %d item(s) to %s" added-count property))))

(defun ai-org-chat--add-context (items &optional silent)
  "Helper function to add context to the current org node.
ITEMS is a list of strings to add to the context.
When SILENT is non-nil, suppress the default message."
  (ai-org-chat--add-to-property "CONTEXT" items silent))

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
(defun ai-org-chat--get-other-window-buffer-names ()
  "Get buffer names from all windows except the current one."
  (let* ((current-buf (current-buffer))
         (other-windows (seq-remove
                         (lambda (window)
                           (eq (window-buffer window) current-buf))
                         (window-list)))
         (other-buffers (mapcar #'window-buffer other-windows)))
    (mapcar #'buffer-name other-buffers)))

(defun ai-org-chat-add-visible-buffers-context ()
  "Add all visible buffers as context for current org node.
Excludes current buffer."
  (interactive)
  (let ((buffer-names (ai-org-chat--get-other-window-buffer-names)))
    (ai-org-chat--add-context buffer-names)))

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
    (ai-org-chat--add-context relative-files t)
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
        (ai-org-chat--add-context final-files t)
        (message "Added %d files from project %s as context (wildcard: %s)"
                 (length final-files)
                 (project-root project)
                 wildcard)))))

(defun ai-org-chat-add-tools ()
  "Add selected tools to the current org node's TOOLS property.
Prompts for tool names from `ai-org-chat-tools'."
  (interactive)
  (let* ((tool-names (mapcar #'llm-tool-name ai-org-chat-tools))
         (selected-tools (completing-read-multiple
                          "Select tools to add: "
                          tool-names)))
    (when selected-tools
      (ai-org-chat--add-to-property "TOOLS" selected-tools "tools"))))

(defun ai-org-chat-add-source-buffer ()
  "Add buffers as sources for the current org node.
Creates a region indirect buffer if region is active, otherwise prompts
for buffers."
  (interactive)
  (let ((selected-buffers
         (completing-read-multiple
          "Select buffers to add as sources: "
          (mapcar #'buffer-name (buffer-list)))))
    (when selected-buffers
      (ai-org-chat--add-to-property "SOURCE_BUFFER" selected-buffers "source buffer"))))

(defun ai-org-chat-clear-ancestor-source-buffers ()
  "Clear SOURCE_BUFFER properties from direct ancestor nodes.
This removes all SOURCE_BUFFER entries from parent, grandparent, etc. nodes
of the current heading, but does not affect the current node's properties."
  (interactive)
  (save-excursion
    (let ((count 0))
      (while (org-up-heading-safe)
        (when (org-entry-get (point) "SOURCE_BUFFER")
          (org-entry-delete (point) "SOURCE_BUFFER")
          (cl-incf count)))
      (message "Cleared SOURCE_BUFFER from %d ancestor node(s)" count))))

;;; Setting up new chats

;;;###autoload
(defun ai-org-chat-new-empty ()
  "Create new AI chat buffer.
Create org buffer with timestamped filename and set it up for AI chat."
  (interactive)
  (let* ((dir ai-org-chat-dir)
         (file (format-time-string "%Y%m%dT%H%M%S--ai-chat.org"))
         (path (expand-file-name file dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (pop-to-buffer (find-file-noselect path))
    (ai-org-chat-setup-buffer)))

(defun ai-org-chat--make-source-buffer (beg end)
  "Create an indirect buffer for region BEG..END of current buffer."
  (let* ((source-buf (current-buffer))
         (buf-name (format "*ai-org-chat-source-%s-%s*"
                           (buffer-name source-buf)
                           (format-time-string "%Y%m%dT%H%M%S")))
         (indirect-buf (make-indirect-buffer
                        source-buf buf-name t 'share-markers)))
    (with-current-buffer indirect-buf
      (narrow-to-region beg end))
    indirect-buf))

;;;###autoload
(defun ai-org-chat-new (arg)
  "Start a new AI chat buffer, optionally including the active region.

If a region is selected, its contents are both copied into the chat buffer
and maintained as a live connection via an indirect buffer.  Otherwise,
creates an empty buffer.

The new buffer is created with a timestamped filename in
`ai-org-chat-dir', and set up with `org-mode' and
`ai-org-chat-minor-mode' enabled.

With prefix argument ARG, immediately add visible buffers and the
original buffer to the context."
  (interactive "P")
  (let ((original-buffer (current-buffer)))
    (if (region-active-p)
        (let* ((reg-beg (region-beginning))
               (reg-end (region-end))
               (region-contents
                (ai-org-chat--escape-asterisks
                 (ai-org-chat--ensure-trailing-newline
                  (buffer-substring-no-properties reg-beg reg-end))
                 t))
               (wrapped-region-contents
                (ai-org-chat--wrap-org
                 (list :name ""
                       :mode major-mode
                       :language (replace-regexp-in-string
                                  "-mode$" "" (symbol-name major-mode))
                       :content region-contents))))
          (deactivate-mark)
          (let ((source-buf (ai-org-chat--make-source-buffer reg-beg reg-end))
                (source-buf-name nil))
            (ai-org-chat-new-empty)
            (goto-char (point-max))
            (insert ":QUOTED_REGION:"
                    wrapped-region-contents
                    ":END:\n")
            (setq source-buf-name (buffer-name source-buf))
            (org-entry-put (point-min) "SOURCE_BUFFER" source-buf-name)
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when (buffer-live-p source-buf)
                          (kill-buffer source-buf)))
                      nil t)))
      (ai-org-chat-new-empty))
    (when arg
      (save-excursion
        (goto-char (point-min))
        (let* ((other-buffer-names (ai-org-chat--get-other-window-buffer-names))
               (orig-buf-name (buffer-name original-buffer))
               (all-buffers (cons orig-buf-name other-buffer-names)))
          (ai-org-chat--add-context all-buffers))))))

;;; Convenience function for creating a new branch

(defun ai-org-chat--create-user-heading ()
  "Create new user heading at current position."
  (org-insert-heading t nil t)
  (insert (concat ai-org-chat-user-name))
  (insert "\n"))

(defun ai-org-chat--find-parent-ai-heading ()
  "Find parent AI heading from current position.
Returns non-nil if found, nil otherwise."
  (let ((not-at-top t))
    (while (and
            (not (equal (org-get-heading t t) ai-org-chat-ai-name))
            (setq not-at-top (org-up-heading-safe))))
    not-at-top))

(defun ai-org-chat--create-heading (heading)
  "Create new subtree with HEADING as heading."
  (org-insert-heading-after-current)
  (insert heading)
  (org-demote-subtree))

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
  (if (ai-org-chat--find-parent-ai-heading)
      (ai-org-chat--create-heading ai-org-chat-user-name)
    (goto-char (point-max))
    (ai-org-chat--create-user-heading)))

;;;###autoload
(defun ai-org-chat-branch-top-level ()
  "Create new chat branch at top level of buffer.
This function first navigates to the beginning of the buffer,
widening if necessary, then creates a new top-level chat branch."
  (interactive)
  (save-restriction
    (widen)
    (goto-char (point-max))
    (ai-org-chat--create-user-heading)))

;;; Convenience functions for postprocessing AI responses

;;;###autoload
(defun ai-org-chat-convert-markdown-blocks-to-org ()
  "Convert Markdown style code blocks in current buffer to org."
  (interactive)
  (save-excursion
    (while (re-search-forward
            "^\\([ \t]*\\)```\\([^[:space:]]+\\)?\\(\n\\|\r\\)\\(\\(?:.\\|\n\\)*?\\)\\(\n\\|\r\\)\\1```" nil t)
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
  "Replace markdown backtick quotes with `org-mode' verbatim quotes.
If consecutive backticks appear on the same line, then only the first
counts as opening and only the last counts as closing."
  (interactive)
  (query-replace-regexp
   (rx (seq (group (or bol (not (any "`"))))
            "`" (group (one-or-more (not (any "`\n\r"))))
            "`" (group (or eol (not (any "`"))))))
   "\\1=\\2=\\3"))

(defun ai-org-chat--replace-backticks-internal (beg end)
  "Replace a single markdown backtick span between BEG and END.
The closing backtick is defined as the first backtick not immediately
followed by another."
  (goto-char beg)
  (while (re-search-forward (rx (seq (group (or (not (any "`"))
                                                bol))
                                     (group "`")))
                            end t)
    (let ((open (match-beginning 2)))
      (goto-char (match-end 2))
      (let ((content-start (point))
            (bound (min end (line-end-position)))
            closing found)
        (while (and (not found)
                    (< (point) bound)
                    (search-forward "`" bound t))
          (unless (eq (char-after (point)) ?`)
            (setq closing (1- (point)))
            (setq found t)))
        (when found
          (let ((content (buffer-substring-no-properties content-start closing)))
            (goto-char open)
            (delete-region open (1+ closing))
            (insert (concat "=" content "="))))))))

(defun ai-org-chat-replace-backticks-non-interactive (beg end)
  "Replace markdown quotes with `org-mode' quotes between BEG and END.
Skips source blocks, example blocks, and property drawers."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((protected-regions '())
            (case-fold-search t))
        (goto-char (point-min))
        (dolist (pattern '(;; Source and example blocks
                           ("^[ \t]*#\\+begin_\\(src\\|example\\)"
                            . "^[ \t]*#\\+end_\\(src\\|example\\)")
                           ;; Markdown code fences
                           ("^[ \t]*```" . "^[ \t]*```")
                           ;; Property drawers
                           ("^[ \t]*:\\([A-Za-z0-9_]+\\):[ \t]*$"
                            . "^[ \t]*:END:[ \t]*$")))
          (let ((start-regexp (car pattern))
                (end-regexp   (cdr pattern)))
            (goto-char (point-min))
            (while (re-search-forward start-regexp nil t)
              (let ((block-start (match-beginning 0)))
                (when (re-search-forward end-regexp nil t)
                  (push (cons block-start (match-end 0))
                        protected-regions))))))
        (setq protected-regions
              (sort protected-regions
                    (lambda (a b) (< (car a) (car b)))))
        (let ((unprotected-regions '())
              (last-end (point-min)))
          (dolist (region protected-regions)
            (when (< last-end (car region))
              (push (cons last-end (car region)) unprotected-regions))
            (setq last-end (cdr region)))
          (when (< last-end (point-max))
            (push (cons last-end (point-max)) unprotected-regions))
          (setq unprotected-regions (nreverse unprotected-regions))
          (dolist (region unprotected-regions)
            (ai-org-chat--replace-backticks-internal
             (car region) (cdr region))))))))


(defun ai-org-chat-auto-format-response (start end)
  "Automatically format the AI response between START and END.
Converts markdown code blocks to `org' format and replaces backticks
with `org-mode' verbatim markers, avoiding source blocks and property
drawers.

When called interactively, operates on the active region. If no region
is active, operates from point to the end of buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point-max))))
  (save-excursion
    (save-restriction
      (undo-boundary)
      (narrow-to-region start end)
      (goto-char (point-min))
      (ai-org-chat-convert-markdown-blocks-to-org)
      (ai-org-chat-replace-backticks-non-interactive (point-min) (point-max))
      (undo-boundary))))

;;;###autoload
(defun ai-org-chat-format-current-response ()
  "Auto-format the AI response at point.
When called with point inside an AI response, this command applies
formatting to convert markdown code blocks to org-mode syntax and
replace backticks with org-mode verbatim markers throughout the
entire response entry."
  (interactive)
  (save-excursion
    (let ((is-in-response nil))
      ;; First check if we're in an AI response by examining the heading
      (save-excursion
        (org-back-to-heading t)
        (when (string= (org-get-heading t t t t) ai-org-chat-ai-name)
          (setq is-in-response t)))
      
      (if (not is-in-response)
          (user-error "Not in an AI response - point must be under an \"%s\" heading" 
                      ai-org-chat-ai-name)
        ;; Get the entry boundaries
        (org-back-to-heading t)
        (let* ((beg (line-beginning-position 2))
               (end (save-excursion
                      (outline-next-heading)
                      (point))))
          ;; Apply the auto-formatting to the region
          (ai-org-chat-auto-format-response beg end)
          (message "Formatted AI response"))))))

;;;###autoload
(defun ai-org-chat-add-auto-formatting ()
  "Add automated formatting hooks to responses.
This adds a function to `ai-org-chat-response-finished-functions' that
automatically formats AI responses to proper `org-mode' syntax."
  (interactive)
  (add-hook 'ai-org-chat-response-finished-functions
            #'ai-org-chat-auto-format-response
            t))

;;;###autoload
(defun ai-org-chat-reset-auto-formatting ()
  "Remove automated formatting hooks from responses."
  (interactive)
  (setq ai-org-chat-response-finished-functions nil))

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
    (when-let* ((index-alist
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

(defun ai-org-chat--get-context-buffers ()
  "Get list of buffers specified in CONTEXT properties."
  (let ((buffer-names (ai-org-chat--property-values "CONTEXT")))
    (cl-remove-if-not #'identity
                      (mapcar (lambda (name)
                                (or (get-buffer name)
                                    (find-buffer-visiting name)))
                              buffer-names))))

(declare-function org-element-type "org")

;;;###autoload
(defun ai-org-chat-compare ()
  "Compare a source block with appropriate content using ediff.
If there are live source buffers defined via SOURCE_BUFFER properties,
compares against the first matching one.  Otherwise, tries to find a
matching definition or prompts for a buffer to compare with."
  (interactive)
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (unless (memq type '(src-block example-block))
      (user-error "Point must be in a source block or example block"))

    (let* ((org-src-window-setup 'current-window)
           (aux-bufs (ai-org-chat--get-context-buffers))
           (source-buffer-names (ai-org-chat--property-values "SOURCE_BUFFER"))
           (source-buf (seq-find (lambda (name)
                                   (when-let* ((buf (get-buffer name)))
                                     (buffer-live-p buf)))
                                 source-buffer-names)))

      (let ((edited-buf))
        (condition-case err
            (progn
              (tab-duplicate)
              (org-edit-special)
              (setq edited-buf (current-buffer))
              ;; PRIORITY 1: Use source buffer from properties if available
              (if source-buf
                  (ai-org-chat--setup-ediff (get-buffer source-buf) edited-buf)

                ;; Otherwise, we need to find something to compare with
                (let* ((visible-buffers (seq-remove
                                         (lambda (buf) (eq buf edited-buf))
                                         (seq-uniq (mapcar #'window-buffer (window-list)))))
                       (all-candidate-buffers (seq-uniq (append visible-buffers aux-bufs)))
                       (single-defun-p (with-current-buffer edited-buf
                                         (ai-org-chat--single-defun-p))))

                  (cond
                   ;; PRIORITY 2: Find matching definition for a single defun
                   ((and single-defun-p
                         (with-current-buffer edited-buf
                           (when-let* ((signature (ai-org-chat--extract-defun-signature))
                                       (matching-info
                                        (ai-org-chat--find-matching-defun
                                         signature
                                         (seq-remove (lambda (buf) (eq buf edited-buf))
                                                     all-candidate-buffers))))
                             (ai-org-chat--setup-ediff
                              (nth 0 matching-info) edited-buf
                              (list (nth 1 matching-info) (nth 2 matching-info)))
                             t))))

                   ;; PRIORITY 3: Only one candidate - use it directly
                   ((= (length all-candidate-buffers) 1)
                    (ai-org-chat--setup-ediff (car all-candidate-buffers) edited-buf))

                   ;; PRIORITY 4: Multiple visible buffers - use ace-window
                   ((> (length visible-buffers) 0)
                    (let ((selected-window
                           (if (= (length visible-buffers) 1)
                               (get-buffer-window (car visible-buffers))
                             (aw-select "Select window for comparison"))))
                      ;; If aw-select returns nil (user aborted with C-g), signal quit
                      (if selected-window
                          (ai-org-chat--setup-ediff (window-buffer selected-window) edited-buf)
                        (signal 'quit nil))))

                   ;; PRIORITY 5: Only aux buffers available - use completing-read
                   ((> (length all-candidate-buffers) 0)
                    (let ((buf-to-compare
                           (get-buffer
                            (completing-read "Select buffer to compare: "
                                             (mapcar #'buffer-name all-candidate-buffers)
                                             nil t))))
                      (ai-org-chat--setup-ediff buf-to-compare edited-buf)))

                   ;; No candidates at all
                   (t
                    (message "No buffer available for comparison")
                    (tab-bar-close-tab))))))
          ((quit error)
           ;; Clean up the edited buffer if it exists
           (when (buffer-live-p edited-buf)
             (with-current-buffer edited-buf
               (org-edit-src-exit))
             (kill-buffer edited-buf))
           (tab-bar-close-tab)
           (signal (car err) (cdr err))))))))

;;; Model selection convenience functions

(defcustom ai-org-chat-models
  '(("opus 4" .
     (:package llm-claude
               :provider make-llm-claude
               :key-env "ANTHROPIC_KEY"
               :chat-model "claude-opus-4-20250514")) 
    ("sonnet 4" .
     (:package llm-claude
               :provider make-llm-claude
               :key-env "ANTHROPIC_KEY"
               :chat-model "claude-sonnet-4-20250514"))
    ("sonnet 3.7" .
     (:package llm-claude
               :provider make-llm-claude
               :key-env "ANTHROPIC_KEY"
               :chat-model "claude-3-7-sonnet-20250219"))
    ("sonnet 3.5" .
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
    ("gpt-4.1" .
     (:package llm-openai
               :provider make-llm-openai
               :key-env "OPENAI_KEY"
               :chat-model "gpt-4.1"))
    ("opus 3" .
     (:package llm-claude
               :provider make-llm-claude
               :key-env "ANTHROPIC_KEY"
               :chat-model "claude-3-opus-20240229"))
    ("deepseek-chat" .
     (:package llm-openai
               :provider (lambda (&rest args)
                           (apply #'make-llm-openai-compatible
                                  :url "https://api.deepseek.com/"
                                  args))
               :key-env "DEEPSEEK_KEY"
               :chat-model "deepseek-chat"))
    ("deepseek-coder" .
     (:package llm-openai
               :provider (lambda (&rest args)
                           (apply #'make-llm-openai-compatible
                                  :url "https://api.deepseek.com/"
                                  args))
               :key-env "DEEPSEEK_KEY"
               :chat-model "deepseek-coder"))
    ("deepseek-reasoner" .
     (:package llm-openai
               :provider (lambda (&rest args)
                           (apply #'make-llm-openai-compatible
                                  :url "https://api.deepseek.com/"
                                  args))
               :key-env "DEEPSEEK_KEY"
               :chat-model "deepseek-reasoner"))
    ("gemini 2.0 flash-lite" .
     (:package llm-gemini
               :provider make-llm-gemini
               :key-env "GEMINI_KEY"
               :chat-model "gemini-2.0-flash-lite"))
    ("gemini 2.0 flash" .
     (:package llm-gemini
               :provider make-llm-gemini
               :key-env "GEMINI_KEY"
               :chat-model "gemini-2.0-flash"))
    ("gemini 2.5 pro preview" .
     (:package llm-gemini
               :provider make-llm-gemini
               :key-env "GEMINI_KEY"
               :chat-model "gemini-2.5-pro-preview-03-25"))
    ("llama 3.1" .
     (:package llm-ollama
               :provider make-llm-ollama
               :chat-model "llama3.1:latest"))
    ("mistral" .
     (:package llm-ollama
               :provider make-llm-ollama
               :chat-model "mistral:latest"))
    ("phi4" .
     (:package llm-ollama
               :provider make-llm-ollama
               :chat-model "phi4")))
  "Alist of LLM models and their configurations for ai-org-chat.
Each entry is of the form (NAME . PLIST) where NAME is a string
identifying the model, and PLIST is a property list with the following keys:
:provider - The function to create the LLM provider
:key-env - The environment variable name for the API key (optional)
:chat-model - The specific model name to use with the provider"
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp)))

(defcustom ai-org-chat-default-model "sonnet 4"
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
    (unless config
      (user-error "Model %s is not available" model))
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
        (list "Source Buffer"
              ["Add Source Buffer" ai-org-chat-add-source-buffer
               :help "Add a buffer as source for the current node"]
              ["Clear Ancestor Source Buffers" ai-org-chat-clear-ancestor-source-buffers
               :help "Clear SOURCE_BUFFER properties from all ancestor nodes"])
        (list "Tools"
              ["Add Tools" ai-org-chat-add-tools
               :help "Add tool functions to the current node"])
        (list "Format"
              ["Convert Markdown Blocks" ai-org-chat-convert-markdown-blocks-to-org
               :help "Convert Markdown style code blocks to org"]
              ["Replace Backticks" ai-org-chat-replace-backticks-with-equal-signs
               :help "Replace markdown backticks with org-mode verbatim quotes"]
              ["Format Current Response" ai-org-chat-format-current-response
               :help "Auto-format the current AI response"])

        ["Copy Conversation" ai-org-chat-copy-conversation-to-clipboard
         :help "Copy the conversation as formatted XML to clipboard"]))

;;; Transient interface

(defclass ai-org-chat--model (transient-infix)
  ((key         :initform "-m")
   (argument    :initform "--model=")
   (reader      :initform (lambda (_prompt _init _hist)
                            (completing-read "Model: "
                                             (mapcar #'car ai-org-chat-models))))))

(transient-define-infix ai-org-chat--infix-model ()
  "Select the model for ai-org-chat."
  :class 'ai-org-chat--model
  :description "Model"
  :always-read t)

(defun ai-org-chat--transient-convert-markdown ()
  "Convert markdown blocks and keep transient active."
  (interactive)
  (ai-org-chat-convert-markdown-blocks-to-org)
  (transient-setup 'ai-org-chat-menu))

(transient-define-prefix ai-org-chat-menu ()
  "Control ai-org-chat operations."
  [:description
   (lambda ()
     (format "Model [%s]"
             (if ai-org-chat-provider
                 (slot-value ai-org-chat-provider 'chat-model)
               "none")))
   ("M" "Select model" ai-org-chat-select-model)]
  [["Context"
    ("b" "Add buffer" ai-org-chat-add-buffer-context)
    ("v" "Add visible buffers" ai-org-chat-add-visible-buffers-context)
    ("f" "Add file" ai-org-chat-add-file-context)
    ("F" "Add function" ai-org-chat-add-function-context)
    ("d" "Add directory files" ai-org-chat-add-directory-files-context)
    ("p" "Add project files" ai-org-chat-add-project-files-context)]
   ["Source"
    ("s" "Add source buffer" ai-org-chat-add-source-buffer)
    ("S" "Clear ancestor source buffers" ai-org-chat-clear-ancestor-source-buffers)]
   ["Tools"
    ("t" "Add tools" ai-org-chat-add-tools)]
   ["Format"
    ("m" "Convert markdown blocks" ai-org-chat--transient-convert-markdown)
    ("q" "Replace backticks" ai-org-chat-replace-backticks-with-equal-signs)
    ("a" "Enable auto-formatting" ai-org-chat-add-auto-formatting)
    ("r" "Reset auto-formatting" ai-org-chat-reset-auto-formatting)
    ("f" "Format current response" ai-org-chat-format-current-response)]
   ["Operations"
    ("B" "New branch" ai-org-chat-branch)
    ("T" "New top-level branch" ai-org-chat-branch-top-level)
    ("c" "Compare source block" ai-org-chat-compare)
    ("y" "Copy conversation" ai-org-chat-copy-conversation-to-clipboard)]]
  [["Actions"
    ("RET" "Get response" ai-org-chat-respond)]])

(defun ai-org-chat--ensure-utf8-encoding (text)
  "Ensure TEXT is properly encoded as UTF-8.
TEXT can be either a string or an llm-multipart object.
Returns the input with strings properly decoded as UTF-8."
  (cond
   ((stringp text)
    (let* ((unibyte (if (multibyte-string-p text)
                        (encode-coding-string text 'utf-8-auto)
                      text))
           (decoded (decode-coding-string unibyte 'utf-8-auto)))
      decoded))
   ((and (recordp text) (eq (type-of text) 'llm-multipart))
    (let ((processed-parts
           (mapcar (lambda (part)
                     (if (stringp part)
                         (ai-org-chat--ensure-utf8-encoding part)
                       part))
                   (llm-multipart-parts text))))
      (make-llm-multipart :parts processed-parts)))
   (t (error "ai-org-chat--ensure-utf8-encoding: Expected string or llm-multipart, got %S" text))))


;;;###autoload
(defun ai-org-chat-respond (&optional arg)
  "Insert response from AI after current heading in org buffer.
With prefix ARG, show the transient interface instead."
  (interactive "P")
  (cond
   (arg
    (ai-org-chat-menu))
   (t
    (unless ai-org-chat-provider
      (user-error "No LLM provider set.  Use `ai-org-chat-select-model' to choose a model"))
    (let* ((system ai-org-chat-system-message)
           (context (ai-org-chat--property-content "CONTEXT" "Context:"))
           (system-context (concat system "\n" context))
           (messages (ai-org-chat--get-conversation-history))
           (start (save-excursion
                    (ai-org-chat--create-heading ai-org-chat-ai-name)
                    (insert "\n")
                    (save-excursion
                      (ai-org-chat--create-heading ai-org-chat-user-name))
                    (point-marker)))
           (end (copy-marker start t))
           (tool-names (ai-org-chat--property-values "TOOLS"))
           (tools (delq nil (mapcar #'ai-org-chat--lookup-tool tool-names)))
           (wrapped-tools (mapcar (lambda (tool)
                                    (ai-org-chat--create-logging-tool tool start))
                                  tools))
           (source-content (ai-org-chat--property-content "SOURCE_BUFFER" "Active source buffer contents:"))
           ;; Add source buffer content to the user's last message
           (final-message-contents
            (if (and source-content (> (length messages) 0))
                (append
                 (butlast messages)
                 (list (concat source-content "\n\n" (car (last messages)))))
              messages))
           (prompt (llm-make-chat-prompt
                    (mapcar #'ai-org-chat--ensure-utf8-encoding
                            final-message-contents)
                    :context (ai-org-chat--ensure-utf8-encoding system-context)
                    :tools wrapped-tools)))
      (let ((partial-cb
             (lambda (response)
               (ai-org-chat--insert-text start end response)))
            (final-cb
             (lambda (response)
               (ai-org-chat--handle-final-response
                prompt response start end
                ai-org-chat-max-recursion-depth ai-org-chat-provider)))
            (error-cb
             (lambda (err msg)
               (with-current-buffer (marker-buffer start)
                 (goto-char start)
                 (insert (format "\nError: %s - %s\n" err msg)))
               (ai-org-chat--handle-final-response
                prompt (format "Error: %s - %s" err msg)
                start end ai-org-chat-max-recursion-depth ai-org-chat-provider))))
        (if ai-org-chat-streaming-p
            (llm-chat-streaming ai-org-chat-provider prompt partial-cb final-cb error-cb)
          (llm-chat-async ai-org-chat-provider prompt final-cb error-cb)))))))

;;; Minor mode

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

;;; String capture

(defun ai-org-chat-copy-conversation-to-clipboard ()
  "Copy the current conversation as formatted XML to the clipboard.
This includes the system message, context, and all exchanges between
user and assistant."
  (interactive)
  (let* ((system-msg ai-org-chat-system-message)
         (context (ai-org-chat--property-content "CONTEXT" "Context:"))
         (messages (ai-org-chat--get-conversation-history))
         (indexed-messages (seq-map-indexed #'cons messages))
         (formatted-string
          (concat
           "System Message and Context:\n\n"
           system-msg "\n\n"
           (if context (concat context "\n\n") "")
           "Conversation:\n\n"
           (mapconcat
            (lambda (indexed-msg)
              (let ((index (cdr indexed-msg))
                    (content (car indexed-msg)))
                (format "<exchange>\n  <role>%s</role>\n  <content>\n%s\n  </content>\n</exchange>"
                        (if (eq (% index 2) 1)
                            "assistant"
                          "user")
                        (if (stringp content)
                            content
                          (format "%S" content)))))
            indexed-messages
            "\n\n"))))
    (kill-new formatted-string)
    (message "Conversation copied to clipboard")))

(provide 'ai-org-chat)
;;; ai-org-chat.el ends here
