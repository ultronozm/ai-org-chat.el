;;; ai-org-chat-apply.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Keywords: 

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

;; 

;;; Code:

;;; Apply changes

(defcustom ai-org-chat-auxiliary-provider nil
  "The LLM provider to use for auxiliary processing in AI chat.
This should be an instance of an LLM provider created using the `llm'
package (e.g., via `make-llm-openai')."
  :type 'symbol
  :group 'ai-org-chat)

(cl-defstruct ai-org-chat-buffer-change
  type buffer diff)

(cl-defstruct ai-org-chat-file-creation
  filename content)

(defun ai-org-chat--llm-make-buffer-change ()
  "Create an LLM function call for generating diffs.

This function returns an `llm-function-call' object that represents
a function for generating universal diffs. When called, this function
will create an `ai-org-chat-buffer-change' struct with the provided
buffer name and diff.

The generated function has two arguments:
- buffer: The name of the buffer to modify (string, required)
- diff: The universal diff describing the changes to apply (string, required)

This is primarily used in the context of the auxiliary LLM for
applying changes to buffers based on natural language instructions."
  (make-llm-function-call
   :function (lambda (buffer diff)
               (make-ai-org-chat-buffer-change
                :type 'apply-diff
                :buffer buffer
                :diff diff))
   :name "generate_diff"
   :description "Generate a universal diff for changes to be applied to the buffer."
   :args (list (make-llm-function-arg
                :name "buffer"
                :description "The name of the buffer to modify."
                :type 'string
                :required t)
               (make-llm-function-arg
                :name "diff"
                :description "The universal diff describing the changes to be applied."
                :type 'string
                :required t))))

(defun ai-org-chat--llm-make-file-creation ()
  "Create an LLM function call for file creation.

This function returns an `llm-function-call' object that represents
a function for creating new files. When called, this function will
create an `ai-org-chat-file-creation' struct with the provided
filename and content.

The generated function has two arguments:
- filename: The name of the file to create (string, required)
- content: The content to write to the new file (string, required)

This is primarily used in the context of the auxiliary LLM for
creating new files based on natural language instructions."
  (make-llm-function-call
   :function (lambda (filename content)
               (make-ai-org-chat-file-creation
                :filename filename
                :content content))
   :name "create_file"
   :description "Describe the creation of a new file with content."
   :args (list (make-llm-function-arg
                :name "filename"
                :description "The name of the file to create."
                :type 'string
                :required t)
               (make-llm-function-arg
                :name "content"
                :description "The content to write to the new file."
                :type 'string
                :required t))))

(defun ai-org-chat--generate-auxiliary-llm-prompt (direction buffers)
  "Generate a prompt for the auxiliary LLM based on DIRECTION and BUFFERS.

This function creates a comprehensive prompt for the auxiliary Language
Model (LLM) that includes:
1. A system message defining the LLM's role and available functions.
2. The user-provided DIRECTION for modifications.
3. The context, which consists of the contents of all provided BUFFERS.

Arguments:
- DIRECTION: A string containing the user's instructions for modifications.
- BUFFERS: A list of buffer objects to be included in the context.

The generated prompt is structured to guide the LLM in producing
a list of buffer modifications and file creations based on the
given directions, using the provided function calls (generate_diff
and create_file)."
  (let* ((context (mapconcat
                   (lambda (buf)
                     (format "Buffer: %s\nContents:\n%s"
                             (buffer-name buf)
                             (concat
                              "```\n"
                              (with-current-buffer buf
                                (buffer-string))
                              "```")))
                   buffers
                   "\n\n"))
         ;; taken from aider [[https://github.com/paul-gauthier/aider]]
         (system-message "You are an assistant that generates a list of buffer modifications and file creations based on given directions. Use the provided function calls, namely, generate_diff to describe buffer changes using universal diff format and create_file to create new files.  Notes:

For each file that needs to be changed, write out the changes similar to a unified diff like `diff -U0` would produce.

Make sure you include the first 2 lines with the file paths.
Don't include timestamps with the file paths.

Start each hunk of changes with a `@@ ... @@` line.
Don't include line numbers like `diff -U0` does.
The user's patch tool doesn't need them.

The user's patch tool needs CORRECT patches that apply cleanly against the current contents of the file!
Think carefully and make sure you include and mark all lines that need to be removed or changed as `-` lines.
Make sure you mark all new or modified lines with `+`.
Make sure to mark lines that need to be removed with `-`.
And make SURE to mark context lines with an empty space ` `!
Don't leave out any lines or the diff patch won't apply correctly.

Indentation matters in the diffs!

Be sure to give the correct number of parentheses and braces in the diff.

Start a new hunk for each section of the file that needs changes.

Only output hunks that specify changes with `+` or `-` lines.
Skip any hunks that are entirely unchanging ` ` lines.

Output hunks in whatever order makes the most sense.
Hunks don't need to be in any particular order.

When editing a function, method, loop, etc use a hunk to replace the *entire* code block.
Delete the entire existing version with `-` lines and then add a new, updated version with `+` lines.
This will help you generate correct code and correct diffs.

To move code within a file, use 2 hunks: 1 to delete it from its current location, 1 to insert it in the new location."))
    (llm-make-chat-prompt
     direction
     :context (concat system-message "\n\n" context)
     :functions (list (ai-org-chat--llm-make-buffer-change)
                      (ai-org-chat--llm-make-file-creation)))))

(defun ai-org-chat--parse-llm-response (response)
  "Parse the LLM RESPONSE and return a list of change objects."
  (if (stringp response)
      (progn (message "String response from LLM: %s" response)
             nil)
    (cl-loop for (func-name . result) in response
             when result
             collect (pcase func-name
                       ("generate_diff"
                        (unless (ai-org-chat-buffer-change-p result)
                          (error "Invalid result for generate_diff: %S" result))
                        result)
                       ("create_file"
                        (unless (ai-org-chat-file-creation-p result)
                          (error "Invalid result for create_file: %S" result))
                        result)
                       (_ (error "Unknown function call: %S" func-name))))))

(defun ai-org-chat--diff-apply (diff)
  "Apply diffs to some buffers."
  (interactive "sDiff to apply: ")
  (dolist (hunks (split-string diff "--- .*\n\\+\\+\\+ " t))
    (setq hunks (split-string hunks "\n" t))
    (let ((buf-name (car hunks)))
      (setq hunks (string-join (cdr hunks) "\n"))
      (if-let (buf (get-buffer buf-name))
          (with-current-buffer buf
            (dolist (hunk (split-string hunks "^@@.*\n" t))
              (let (before after)
                (dolist (line (split-string hunk "\n" t))
                  (cond
                   ((string-prefix-p " " line)
                    (push (substring line 1) before)
                    (push (substring line 1) after))
                   ((string-prefix-p "-" line)
                    (push (substring line 1) before))
                   ((string-prefix-p "+" line)
                    (push (substring line 1) after))
                   (t
                    ;; Try to recover from "omitted space" error
                    (push line before)
                    (push line after))))
                (setq before (string-join (nreverse before) "\n"))
                (setq after (string-join (nreverse after) "\n"))
                ;; Find the start position of the old content
                (goto-char (point-min))
                (if (re-search-forward (regexp-quote before) nil t)
                    (let ((start (match-beginning 0))
                          (end (match-end 0)))
                      (delete-region start end)
                      (goto-char start)
                      (insert after)
                      (message "Applied hunk to buffer %s" buf-name))
                  (message "Hunk not found in buffer %s\n\
Before:\n%s\n\
After:\n%s" buf-name before after)))))
        (message "Buffer %s not found" buf-name)))))

(defun ai-org-chat--diff-apply-2 (diff buf)
  "Apply DIFFs to BUF."
  (interactive "sDiff to apply: ")
  (dolist (hunks (split-string diff "--- .*\n\\+\\+\\+ " nil))
    (setq hunks (split-string hunks "\n" nil))
    (setq hunks (string-join (cdr hunks) "\n"))
    (with-current-buffer buf
      (dolist (hunk (split-string hunks "^@@.*\n" nil))
        (let (before after)
          (dolist (line (split-string hunk "\n" nil))
            (cond
             ((string-prefix-p " " line)
              (push (substring line 1) before)
              (push (substring line 1) after))
             ((string-prefix-p "-" line)
              (push (substring line 1) before))
             ((string-prefix-p "+" line)
              (push (substring line 1) after))
             (t
              ;; Try to recover from "omitted space" error
              (push line before)
              (push line after))))
          (setq before (string-join (nreverse before) "\n"))
          (setq after (string-join (nreverse after) "\n"))
          ;; Find the start position of the old content
          (goto-char (point-min))
          (if (re-search-forward (regexp-quote before) nil t)
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (delete-region start end)
                (goto-char start)
                (insert after)
                (message "Applied hunk to buffer %s" buf))
            (message "Hunk not found in buffer %s\nBefore:\n%s\n" buf before)))))))

(defvar ai-org-chat--actually-apply nil)

(defun ai-org-chat--apply-changes (changes)
  "Apply the list of CHANGES to buffers and create new files."
  (dolist (change changes)
    (cond
     ((ai-org-chat-buffer-change-p change)
      (let ((buffer (get-buffer (ai-org-chat-buffer-change-buffer change))))
        (if buffer
            (with-current-buffer buffer
              (let* ((diff (ai-org-chat-buffer-change-diff change))
                     (temp-diff-buffer (generate-new-buffer "*temp-diff*")))
                (unwind-protect
                    (progn
                      (with-current-buffer temp-diff-buffer
                        (insert diff)
                        (setq-local default-directory
                                    (or (file-name-directory
                                         (buffer-file-name buffer))
                                        default-directory))
                        (diff-mode)
                        (ai-org-chat--diff-apply-2 (buffer-string) buffer))
                      ;; (if ai-org-chat--actually-apply
                      ;;     (save-window-excursion
                      ;;       (with-current-buffer temp-diff-buffer
                      ;;         (while (re-search-forward diff-hunk-header-re nil t)
                      ;;           (diff-apply-hunk))))
                      ;;   (display-buffer temp-diff-buffer))
                      )
                  ;; (kill-buffer temp-diff-buffer)
                  )))
          (message "Buffer not found: %s" (ai-org-chat-buffer-change-buffer change)))))
     ((ai-org-chat-file-creation-p change)
      (with-temp-file (ai-org-chat-file-creation-filename change)
        (insert (ai-org-chat-file-creation-content change))))
     (t (error "Unknown change type: %S" change)))))

(defun ai-org-chat-process-directions (direction buffers)
  "Process DIRECTION for BUFFERS using the auxiliary LLM."
  (let* ((prompt (ai-org-chat--generate-auxiliary-llm-prompt direction buffers))
         (provider (or ai-org-chat-auxiliary-provider ai-org-chat-provider))
         (response (llm-chat provider prompt))
         (changes (ai-org-chat--parse-llm-response response)))
    (if changes
        (progn
          (ai-org-chat--apply-changes changes)
          (message "Applied %d change(s) to buffers and/or files" (length changes)))
      (message "No changes to apply"))))

(defun ai-org-chat--get-context-buffers ()
  "Get list of buffers specified in CONTEXT properties."
  (let ((buffer-names (ai-org-chat--get-permanent-context-items)))
    (cl-remove-if-not #'identity
                      (mapcar (lambda (name)
                                (or (get-buffer name)
                                    (find-buffer-visiting name)))
                              buffer-names))))

(defun ai-org-chat-modify-buffers ()
  "Modify buffers using an auxiliary LLM.
Directions for doing so comes from current subtree's content.  This
function should be called in an org buffer with `ai-org-chat-minor-mode'
activated."
  (interactive)
  (unless ai-org-chat-minor-mode
    (user-error "This function requires ai-org-chat-minor-mode to be active"))
  (let ((direction
         (concat
          "Here are the instructions for you to follow:\n\n"
          (ai-org-chat--current-body)))
        (buffers (ai-org-chat--get-context-buffers)))
    (if buffers
        (ai-org-chat-process-directions direction buffers)
      (user-error "No context buffers found.  Add CONTEXT properties to specify buffers"))))

(defun ai-org-chat-modification-tester (directions)
  "Test buffer modifications by applying DIRECTIONS to the current buffer.

This function serves as a convenient way to test the auxiliary LLM's
ability to interpret and apply modifications to a buffer. It takes
a string of DIRECTIONS as input and applies them to the current buffer
using the auxiliary LLM.

When called interactively, it prompts the user for directions in the
minibuffer. This is useful for quick experiments or debugging the
buffer modification process.

Arguments:
- DIRECTIONS: A string containing instructions for modifying the buffer.

The function wraps the DIRECTIONS in a standard format before passing
them to the auxiliary LLM, ensuring consistency with other parts of
the ai-org-chat system."
  (interactive "sDirections: ")
  (let ((buffers (list (current-buffer))))
    (ai-org-chat-process-directions
     (concat
      "Here are the instructions for you to follow.\n\n"
      directions)
     buffers)))


(provide 'ai-org-chat-apply)
;;; ai-org-chat-apply.el ends here
