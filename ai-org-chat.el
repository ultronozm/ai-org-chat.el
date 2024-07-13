;;; ai-org-chat.el --- Threaded chat with AI agent in org buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/ai-org-chat.el
;; Package-Requires: ((emacs "29.1") (gptel "0.9.0"))
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
(require 'gptel)

(defgroup ai-org-chat nil
  "Threaded chat with AI agent in org buffers."
  :group 'hypermedia)

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

Look at the system message LAST -- it contains the most up-to-date contents, more recent than the previous chat messages."
  "System message to use, if any.
If this is nil, then a system message will be provided by `gptel'."
  :type '(choice string (const nil)))

(defcustom ai-org-chat-dir "~/gpt"
  "Directory for storing files created by `ai-org-chat-new'."
  :type 'string)

(defcustom ai-org-chat-request-fn #'ai-org-chat--request
  "Function to call to get a response from OpenAI.
See the docstring for `ai-org-chat--request'.  Modify this if you want
to use some backend other than `gptel'."
  :type 'function)

(cl-defun ai-org-chat--gptel-request
    (&optional prompt &key callback
               (buffer (current-buffer))
               position context dry-run
               (stream nil) (in-place nil)
               (system gptel--system-message))
  "Wrapper for `gptel-request' that handles system messages appropriately.

This function behaves like `gptel-request', but handles the system message
differently based on the backend:

- For gptel--openai backend: Appends the system message as a \"system\"
  role entry in the list of messages.

- For other backends: Passes the system message in the usual way.

All arguments are the same as in `gptel-request'. See its documentation
for details."
  (let* ((is-openai (eq gptel-backend gptel--openai))
         (adjusted-prompt
          (if (and is-openai system (listp prompt))
              (append `(((role . "system")
                         (content . ,system)))
                      prompt)
            prompt))
         (adjusted-system (unless is-openai system)))
    (gptel-request adjusted-prompt
      :callback callback
      :buffer buffer
      :position position
      :context context
      :dry-run dry-run
      :stream stream
      :in-place in-place
      :system adjusted-system)))

(defun ai-org-chat--request (messages point &optional system)
  "Use `gptel' library to get a response from OpenAI.
MESSAGES is a list of alists, each of which has a `role' and a `content'
key.  `role' is either \"user\" or \"assistant\" (see the OpenAI API
docs).  POINT is a marker indicating where the response should be
inserted.  SYSTEM is the system message."
  (ai-org-chat--gptel-request
   messages :position point :stream t :in-place t
   :system (or system gptel--system-message)))

;; The property drawer would be a natural place to store metadata
;; related to the query (model, parameters, ...), which motivates
;; excluding it from the "conversation".
(defun ai-org-chat--content-minus-properties (content)
  "Remove properties drawer (if any) from CONTENT.
CONTENT is text from an `org-mode' entry, excluding the heading and
any subtrees.  The properties drawer is a sequence of lines
delimited by \":PROPERTIES:\" and \":END:\"."
  (let* ((beg-re "^[ \t]*:PROPERTIES:[ \t]*$")
         (end-re "^[ \t]*:END:[ \t]*$")
         (lines (split-string content "\n"))
         (beg (cl-position-if (apply-partially #'string-match-p beg-re) lines))
         (end (cl-position-if (apply-partially #'string-match-p end-re) lines))
         (newlines (cdr (if (and beg end)
                            (append (cl-subseq lines 0 beg)
                                    (cl-subseq lines (1+ end)
                                               (length lines)))
                          lines))))
    (mapconcat 'identity newlines "\n")))

(defun ai-org-chat--current-entry-minus-children ()
  "Return text of current entry, excluding children.
The text includes the heading, but excludes any subtrees."
  (save-excursion
    (org-back-to-heading)
    (buffer-substring-no-properties
     (point)
     (save-excursion
       (outline-next-heading)
       (point)))))

(defun ai-org-chat--current-heading-and-body ()
  "Return cons cell with heading and body of current entry.
The heading excludes tags and TODO keywords.  The body consists
of all text between the heading and the first subtree, but
excluding the :PROPERTIES: drawer, if any."
  (let* ((heading (org-get-heading t t))
  (content (ai-org-chat--current-entry-minus-children))
  (body (ai-org-chat--content-minus-properties content)))
    (cons heading body)))

(defun ai-org-chat--get-ancestors ()
  "Return list of ancestors of the current entry.
Each ancestor is represented by a cons cell (heading . body),
where heading and body are as in the docstring for
`ai-org-chat--current-heading-and-body'."
  (let ((ancestors '()))
    (push (ai-org-chat--current-heading-and-body) ancestors)
    (save-excursion
      (while (org-up-heading-safe)
        (push (ai-org-chat--current-heading-and-body) ancestors)))
    ancestors))

(defun ai-org-chat--ancestor-messages ()
  "Return list of ancestor messages for the current entry.
Each message is formatted as an alist with `role' and `content'
keys, suitable for passing to `ai-org-chat-request-fn'.
The `role' is either \"user\" or \"assistant\", depending upon
whether the heading is equal to `ai-org-chat-ai-name'.  The
`content' is the body of the heading."
  (mapcar
   (lambda (ancestor)
     (let* ((heading (car ancestor))
     (content (cdr ancestor))
     (role (if (equal heading ai-org-chat-ai-name)
        "assistant" "user")))
       `((role . ,role) (content . ,content))))
   (ai-org-chat--get-ancestors)))

(defun ai-org-chat--new-subtree (heading)
  "Create new subtree with HEADING as heading."
  (org-insert-heading-after-current)
  (insert heading)
  (org-demote-subtree))

(defcustom ai-org-chat-context-style nil
  "Type of editor context to send to the AI.
This can be either nil, `visible-contents', or `visible-buffers'."
  :type '(choice (const nil) (const visible-contents) (const visible-buffers))
  :local t)

(declare-function custom-variable-type "cus-edit")

(defun ai-org-chat-set-context-style ()
  "Set the value of `ai-org-chat-context-style'.
Uses the same interface as `customize-set-variable'."
  (interactive)
  (require 'wid-edit)
  (require 'cus-edit)
  (let* ((variable 'ai-org-chat-context-style)
         (type (custom-variable-type variable))
         (current-value (symbol-value variable))
         (prompt (format "Set %s to: " variable))
         (value (widget-prompt-value type prompt current-value nil))
         (comment (when current-prefix-arg
                    (read-string "State comment: "))))
    (customize-set-variable variable value comment)))

(defun ai-org-chat--buffer-contents (window point-functions)
  "Given a WINDOW, use POINT-FUNCTIONS to extract the buffer contents.
Here POINT-FUNCTIONS is a list of two functions that should return the
beginning and end of the desired region, respectively."
  (with-current-buffer (window-buffer window)
    (let* ((beg (funcall (nth 0 point-functions)))
           (end (funcall (nth 1 point-functions)))
           (content (buffer-substring-no-properties beg end))
           (name (buffer-name)))
      (format "%s\n%s\n" name
              (ai-org-chat--enclose-in-src-block content (current-buffer))))))

(defun ai-org-chat--get-permanent-context-buffers ()
  "Get list of permanent context buffer names from current and ancestor nodes."
  (let ((buffers (org-entry-get-multivalued-property (point-min) "CONTEXT")))
    (save-excursion
      (let ((not-done t))
        (while not-done
          (let ((context
                 (org-entry-get-multivalued-property (point) "CONTEXT")))
            (when context
              (setq buffers (append buffers context))))
          (setq not-done (org-up-heading-safe)))))
    (delete-dups buffers)))

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
                             (ai-org-chat--get-permanent-context-buffers)))
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
  "Get content of permanent context buffers."
  (let ((permanent-buffers (ai-org-chat--get-permanent-context-buffers)))
    (mapconcat
     (lambda (buffer-name)
       (if (get-buffer buffer-name)
           (with-current-buffer (get-buffer buffer-name)
             (format "%s\n%s\n" buffer-name
                     (ai-org-chat--enclose-in-src-block
                      (buffer-substring-no-properties (point-min) (point-max))
                      (current-buffer))))
         (warn "Buffer %s not found" buffer-name)
         nil))
     permanent-buffers
     "\n")))

(defun ai-org-chat--wrap-system-message (message)
  "Wrap MESSAGE in a system message, adding context if appropriate.
The context depends on the value of `ai-org-chat-context-style'."
  (let ((permanent-context (ai-org-chat--get-permanent-context-content)))
    (concat
     message
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

;;;###autoload
(defun ai-org-chat-respond ()
  "Insert response from OpenAI after current heading.
Retrieve conversation history via
`ai-org-chat--ancestor-messages', then call
`ai-org-chat-request-fn' to get a response from OpenAI.  The
response is inserted after the next \"AI\" heading and before the
next \"User\" heading."
  (interactive)
  (let* ((system (ai-org-chat--wrap-system-message
                  ai-org-chat-system-message))
         (messages (ai-org-chat--ancestor-messages))
         (point (save-excursion
                  (ai-org-chat--new-subtree ai-org-chat-ai-name)
                  (insert "\n")
                  (save-excursion
                    (ai-org-chat--new-subtree ai-org-chat-user-name))
                  (point-marker))))
    (funcall ai-org-chat-request-fn messages point system)))

;;;###autoload
(define-minor-mode ai-org-chat-minor-mode
  "Toggle `ai-org-chat-minor-mode'.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            map))

;;;###autoload
(defun ai-org-chat-new ()
  "Start new AI chat buffer, possibly with region contents.
If the mark is active, then copy the region contents into the new
buffer, enclosed by an appropriate source block.  Otherwise,
create an empty buffer."
  (interactive)
  (if (region-active-p)
      (ai-org-chat-new-region (region-beginning) (region-end))
    (ai-org-chat-new-empty)))

(defconst ai-org-chat-local-variables
  "# -*- eval: (ai-org-chat-minor-mode 1); -*-
"
  "Local variables to insert into new AI chat buffers.")

(defun ai-org-chat-new-empty ()
  "Create new AI chat buffer.
Create org buffer with timestamped filename.  Enable
`ai-chat-minor-mode'.  Insert a top-level heading."
  (interactive)
  (let ((dir ai-org-chat-dir)
        (file (format-time-string "gpt-%Y%m%dT%H%M%S.org")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((path (expand-file-name file dir)))
      (find-file path)))
  (ai-org-chat-minor-mode)
  (insert ai-org-chat-local-variables)
  (goto-char (point-min))
  (org-hide-entry)
  (ai-org-chat-branch))

(defcustom ai-org-chat-region-filter-functions
  '(ai-org-chat--ensure-trailing-newline
    ai-org-chat--enclose-in-src-block)
  "List of functions to call on quoted region contents.
These functions are applied as preprocessing steps to the region
passed to `ai-org-chat-new-region'.  Each function should accept
two arguments: the region as a string, and the buffer from which
it came.  It should return the processed string."
  :type '(repeat function))

(defun ai-org-chat--ensure-trailing-newline (content _buffer)
  "Ensure that CONTENT ends with a newline."
  (if (string-match "\n\\'" content)
      content
    (concat content "\n")))

(defcustom ai-org-chat-modes-for-src-blocks '(latex-mode LaTeX-mode)
  "List of modes for which to use src blocks."
  :type '(repeat symbol))

(defun ai-org-chat--enclose-in-src-block (content buffer)
  "Enclose CONTENT in a src block, if appropriate.
A src block is used if BUFFER's major mode is a programming mode
or belongs to `ai-org-chat-modes-for-src-blocks'."
  (with-current-buffer buffer
    (if (or (derived-mode-p 'prog-mode)
            (memq major-mode ai-org-chat-modes-for-src-blocks))
        (let ((mode (replace-regexp-in-string
                     "-mode\\'"
                     ""
                     (symbol-name major-mode))))
          (concat
           (format "#+begin_src %s\n" mode)
           content
           "#+end_src"))
      (concat
       (format "#+begin_example\n")
       content
       "#+end_example"))))

(defun ai-org-chat-new-region (beg end)
  "Start new AI chat, quoting region between BEG and END.
Send user to an AI chat buffer.  Copy current region contents
into that buffer, applying the filters in the variable
`ai-org-chat-region-filter-functions'."
  (interactive "r")
  (let ((region-contents
         (buffer-substring-no-properties beg end)))
    (dolist (filter ai-org-chat-region-filter-functions)
      (setq region-contents (funcall filter region-contents (current-buffer))))
    (ai-org-chat-new-empty)
    (save-excursion
      (newline 2)
      (insert region-contents))))

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

(defun ai-org-chat-add-context ()
  "Add selected buffers as context for the current org node."
  (interactive)
  (let* ((all-buffers (mapcar #'buffer-name (buffer-list)))
         (selected-buffers (completing-read-multiple
                            "Select buffers to add to permanent context: "
                            all-buffers))
         (current-context
          (org-entry-get-multivalued-property (point) "CONTEXT"))
         (new-buffers (delete-dups (append current-context selected-buffers))))
    (apply #'org-entry-put-multivalued-property (point) "CONTEXT" new-buffers)
    (message "Added %d buffer(s) to permanent context"
             (length selected-buffers))))

(provide 'ai-org-chat)
;;; ai-org-chat.el ends here
