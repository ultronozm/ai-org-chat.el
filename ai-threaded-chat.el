;;; ai-threaded-chat.el --- Threaded chat with AI agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/ai-threaded-chat.el
;; Package-Requires: ((emacs "29.1") (gptel "0.3.5"))
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

;; This is a simple Emacs package that supports a threaded AI chat
;; inside any org-mode buffer.
;;
;; The "plumbing" is outsourced to the `gptel' library, which requires
;; the user to provide an OpenAI API key, set via the `gptel-api-key'
;; variable.
;;
;; The user is encouraged to bind a key to `ai-threaded-chat-new' in
;; `global-map' and keys to `ai-threaded-chat-respond' and
;; `ai-threaded-chat-append-top-level-heading' in
;; `ai-threaded-chat-minor-mode-map'.  Here's a sample use-package
;; declaration, which assumes that the API key is set in the
;; OPENAI_API_KEY environment variable:
;; 
;; (use-package exec-path-from-shell
;;   :ensure
;;   :init
;;   (exec-path-from-shell-initialize))
;;   
;; (use-package gptel
;;   :ensure
;;   :after exec-path-from-shell
;;   :config
;;   (setq gptel-api-key (exec-path-from-shell-getenv "OPENAI_API_KEY")))
;;
;; (use-package ai-threaded-chat
;;   :bind
;;   (:map global-map
;; 	("C-c /" . ai-threaded-chat-new))
;;   (:map ai-threaded-chat-minor-mode
;; 	("C-c <return>" . ai-threaded-chat-respond)
;; 	("C-c n" . ai-threaded-chat-append-top-level-heading))
;;   :commands (ai-threaded-chat-minor-mode) ; for manual activation in an org-mode buffer
;;   :custom
;;   (ai-threaded-chat-user-name "Paul")
;;   (ai-threaded-chat-dir "~/gpt")
;;   (ai-threaded-chat-prompt-preamble "You are a brilliant and helpful assistant.")) ; modify to suit your needs
;; 
;; `ai-threaded-chat-new' creates a new org-mode file (by default, in
;; the directory "~/gpt/"), adds a top-level entry, and activates the
;; minor mode `ai-threaded-chat-minor-mode'.  If the region is active,
;; then the region contents are added to the top-level entry, enclosed
;; in src blocks if appropriate.
;; 
;; `ai-threaded-chat-respond' inserts a response from the AI agent as a new
;; heading.  Parent entries are treated as conversation history; they
;; are viewed as responses from the AI when their heading is equal to
;; "AI", and otherwise viewed as user messages.
;; 
;; `ai-threaded-chat-append-top-level-heading' appends a top-level
;; heading to the current buffer, with the user name as the heading.
;; This has the effect of starting a new top-level chat thread.


;;; Code:

(require 'org)
(require 'gptel)

(defcustom ai-threaded-chat-user-name "User"
  "User name to insert into buffer."
  :type 'string
  :group 'ai-threaded-chat)

(defcustom ai-threaded-chat-ai-name "AI"
  "AI name to insert into buffer."
  :type 'string
  :group 'ai-threaded-chat)

(defcustom ai-threaded-chat-prompt-preamble "You are a brilliant and helpful assistant."
  "Preamble to insert before the prompt."
  :type 'string
  :group 'ai-threaded-chat)

(defun ai-threaded-chat--request (messages point)
  "Use `gptel' library to get a response from OpenAI.
MESSAGES and POINT are as in the docstring for
`ai-threaded-chat-respond-internal-fn'."
  (gptel-request
   messages :position point :stream t :in-place t))

(defcustom ai-threaded-chat-request-fn #'ai-threaded-chat--request
  "Function to call to get a response from OpenAI.

This function takes two arguments: a list of messages and a point
marker.  The messages are formatted as a list of alists, each of
which has a `role' and a `content' key.  `role' will be either
\"system\", \"user\" or \"assistant\" (see the OpenAI API docs).
The point marker is the point at which the response should be
inserted.

Modify this if you want to use some backend other than `gptel'."
  :type 'function
  :group 'ai-threaded-chat)

;;;###autoload
(defun ai-threaded-chat-respond ()
  "Insert a heading with a response from OpenAI after the current heading.)

Retrieve conversation history via
`ai-threaded-chat--ancestor-messages', then call
`ai-threaded-chat-request-fn' to get a response from OpenAI.
The response is inserted after the current heading."
  (interactive)
  (let ((messages (append
		   `(((role . "system")
		      (content . ,ai-threaded-chat-prompt-preamble)))
		   (ai-threaded-chat--ancestor-messages)))
	(point (save-excursion
		 (org-insert-heading-after-current)
		 (org-demote-subtree)
		 (insert (concat ai-threaded-chat-ai-name "\n"))
		 (save-excursion
		   (org-insert-heading-after-current)
		   (org-demote-subtree)
		   (insert (concat ai-threaded-chat-user-name)))
		 (point-marker))))
    (funcall ai-threaded-chat-request-fn messages point)))

(defun ai-threaded-chat--ancestor-messages ()
  "Return list of ancestor messages for the current entry.
Each message is formatted as an alist with `role' and `content'
keys, suitable for passing to `ai-threaded-chat-respond-internal-fn'.
The `role' is either \"user\" or \"assistant\", depending upon
whether the heading is equal to `ai-threaded-chat-ai-name'.  The
`content' is the body of the heading."
  (mapcar
   (lambda (ancestor)
     (let* ((heading (car ancestor))
	    (content (cadr ancestor))
	    (role (if (equal heading ai-threaded-chat-ai-name)
		      "assistant" "user")))
       `((role . ,role) (content . ,content))))
   (ai-threaded-chat--get-ancestors)))

(defun ai-threaded-chat--get-ancestors ()
  "Return list of ancestor headings for the current entry.
Each heading is formatted as \"<heading>: <body>\"."
  (cl-assert (eq major-mode 'org-mode)
	     nil
	     "This function only works in org-mode buffers.")
  (let ((ancestors '()))
    (push (ai-threaded-chat--current-heading-and-body) ancestors)
    (save-excursion
      (while (org-up-heading-safe)
        (push (ai-threaded-chat--current-heading-and-body) ancestors)))
    ancestors))

(defun ai-threaded-chat--org-entry-minus-properties (entry)
  "Return ENTRY with properties drawer removed (if any).
The properties drawer is not used in the current version of this
library, but I had experimented with putting metadata there, and
found it convenient to exclude such metadata from the text
submitted to OpenAI."
  (let ((lines-without-property-drawer
         (let* ((lines (split-string entry "\n"))
                (property-drawer-start
                 (cl-position-if (lambda (line)
                                   (string-match-p "^[ \t]*:PROPERTIES:[ \t]*$"
                                                   line))
                                 lines))
                (property-drawer-end
	         (when property-drawer-start
	           (cl-position-if (lambda (line)
			             (string-match-p "^[ \t]*:END:[ \t]*$" line))
			           lines :from-end t))))
           (if (and property-drawer-start property-drawer-end)
	       (append (cl-subseq lines 0 property-drawer-start)
		       (cl-subseq lines (1+ property-drawer-end) (length lines)))
             lines))))
    (mapconcat 'identity
	       (cdr lines-without-property-drawer)
	       "\n")))

(defun ai-threaded-chat--current-heading-and-body ()
  "Get current heading and body text as a list."
  (let* ((heading (org-get-heading t t))
	 (content
	  ;; content of current entry, excluding children
	  (save-excursion
	    (org-back-to-heading)
	    (buffer-substring-no-properties
	     (point)
	     (save-excursion
	       (outline-next-heading)
	       (point)))))
	 (body (ai-threaded-chat--org-entry-minus-properties content)))
    (list heading body)))

(define-minor-mode ai-threaded-chat-minor-mode
  "Toggle ai-threaded-chat-minor-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :lighter " OpenAI Chat"
  :keymap (let ((map (make-sparse-keymap)))
	    map))

(defcustom ai-threaded-chat-dir "~/gpt"
  "Directory for storing files created by `ai-threaded-chat-new'."
  :type 'string
  :group 'ai-threaded-chat)

;;;###autoload
(defun ai-threaded-chat-new ()
  "Start new AI chat buffer, possibly with region contents."
  (interactive)
  (if mark-active
      (ai-threaded-chat-new-region (region-beginning) (region-end))
    (ai-threaded-chat-new-empty)))

;;;###autoload
(defun ai-threaded-chat-new-empty ()
  "Create new AI chat buffer.

Create org buffer with timestamped filename.  Enable
`ai-chat-minor-mode'.  Insert a top-level heading."
  (interactive)
  (let ((dir ai-threaded-chat-dir)
	(filename (format-time-string "gpt-%Y-%m-%d-%H%M%S.org")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((filepath (expand-file-name filename dir)))
      (find-file filepath)))
  (ai-threaded-chat-minor-mode)
  (ai-threaded-chat-append-top-level-heading))

;;;###autoload
(defun ai-threaded-chat-new-region (beg end)
  "Start new AI chat, quoting region between BEG and END.

Send user to an AI chat buffer and copy current region contents
into that buffer, enclosing with an appropriate source block."
  (interactive "r")
  (let ((region-contents
         (buffer-substring-no-properties beg end))
        (prog (or (derived-mode-p 'prog-mode)
		  (eq major-mode 'latex-mode)))
        (mode (replace-regexp-in-string
	       "-mode\\'" ""
	       (symbol-name major-mode))))
    (ai-threaded-chat-new-empty)
    (save-excursion
      (newline 2)
      (insert (concat
	       (if prog (format "#+begin_src %s\n" mode))
	       region-contents
	       (if prog "#+end_src"))))))

;;;###autoload
(defun ai-threaded-chat-append-top-level-heading ()
  "Create top level chat thread at bottom of buffer."
  (interactive)
  (goto-char (point-max))
  (org-insert-heading t nil t)
  (insert (concat ai-threaded-chat-user-name))
  ;; (save-excursion (insert "\n"))
  (insert "\n"))

(provide 'ai-threaded-chat)
;;; ai-threaded-chat.el ends here
