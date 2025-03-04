;;; ai-org-chat-tests.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'ai-org-chat)


(ert-deftest test-ai-org-chat--extract-defun-signature ()
  "Test the ai-org-chat--extract-defun-signature function."

  ;; Test with a simple defun
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun test-func ()\n  (message \"Hello\"))")
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (ai-org-chat--extract-defun-signature) '(nil . "test-func"))))

  ;; Test with a defvar
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defvar test-var 10\n  \"A test variable.\")")
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (ai-org-chat--extract-defun-signature) '("Variables" . "test-var"))))

  ;; Test with a defcustom
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defcustom test-custom 'default\n  \"A test custom variable.\"\n  :type 'symbol)")
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (ai-org-chat--extract-defun-signature) '("Variables" . "test-custom"))))

  ;; Test with a nested defun
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun outer-func ()\n  (defun inner-func ()\n    (message \"Inner\")))")
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (ai-org-chat--extract-defun-signature) '(nil . "inner-func"))))

  ;; Test with a defclass
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defclass test-class ()\n  ((slot1 :initarg :slot1)))")
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (ai-org-chat--extract-defun-signature) '("Types" . "test-class"))))

  ;; ;; Test when point is not inside any definition
  ;; (with-temp-buffer
  ;;   (emacs-lisp-mode)
  ;;   (insert "(defun test-func ()\n  (message \"Hello\"))\n\n(defvar test-var 20)")
  ;;   (goto-char (point-max))
  ;;   (forward-line -1)
  ;;   (should (eq (ai-org-chat--extract-defun-signature) nil)))

  ;; Test in a non-Lisp buffer
  (with-temp-buffer
    (insert "This is not a Lisp buffer.")
    (goto-char (point-min))
    (should (eq (ai-org-chat--extract-defun-signature) nil))))

(ert-deftest test-ai-org-chat--filter-files-by-wildcard ()
  "Test the ai-org-chat--filter-files-by-wildcard function."
  (let ((test-files '("file1.py" "file2.py" "file3.txt" "file4.el" "file5.py")))
    ;; Test with no wildcard
    (should (equal (ai-org-chat--filter-files-by-wildcard test-files nil)
                   test-files))

    ;; Test with "*" wildcard
    (should (equal (ai-org-chat--filter-files-by-wildcard test-files "*")
                   test-files))

    ;; Test with "*.py" wildcard
    (should (equal (ai-org-chat--filter-files-by-wildcard test-files "*.py")
                   '("file1.py" "file2.py" "file5.py")))

    ;; Test with "*.el" wildcard
    (should (equal (ai-org-chat--filter-files-by-wildcard test-files "*.el")
                   '("file4.el")))

    ;; Test with non-matching wildcard
    (should (equal (ai-org-chat--filter-files-by-wildcard test-files "*.js")
                   nil))

    ;; Test with partial filename wildcard
    (should (equal (ai-org-chat--filter-files-by-wildcard test-files "file[1-3]*")
                   '("file1.py" "file2.py" "file3.txt")))))

(ert-deftest test-ai-org-chat--search-imenu-alist ()
  "Test the ai-org-chat--search-imenu-alist function."
  ;; Test with a simple alist
  (let ((alist '(("func1" . 10) ("func2" . 20) ("func3" . 30)))
        (pos 25))
    (should (equal (ai-org-chat--search-imenu-alist alist pos)
                   '(nil . "func2"))))

  ;; Test with a nested alist
  (let ((alist '(("Functions" . (("func1" . 10) ("func2" . 20)))
                 ("Variables" . (("var1" . 30) ("var2" . 40)))))
        (pos 35))
    (should (equal (ai-org-chat--search-imenu-alist alist pos)
                   '("Variables" . "var1"))))

  ;; Test with a position before all items
  (let ((alist '(("func1" . 10) ("func2" . 20)))
        (pos 5))
    (should (eq (ai-org-chat--search-imenu-alist alist pos) nil)))

  ;; Test with a position after all items
  (let ((alist '(("func1" . 10) ("func2" . 20)))
        (pos 30))
    (should (equal (ai-org-chat--search-imenu-alist alist pos)
                   '(nil . "func2"))))

  ;; Test with an empty alist
  (let ((alist '())
        (pos 10))
    (should (eq (ai-org-chat--search-imenu-alist alist pos) nil))))

(ert-deftest test-ai-org-chat-register-tool ()
  "Test tool registration functionality."
  (let ((ai-org-chat-tools nil)
        (tool1 (llm-make-tool
                :function #'ignore
                :name "tool1"
                :description "First test tool"
                :args nil))
        (tool2 (llm-make-tool
                :function #'ignore
                :name "tool2"
                :description "Second test tool"
                :args nil)))

    ;; Test registering a single tool
    (ai-org-chat-register-tool tool1)
    (should (equal (length ai-org-chat-tools) 1))
    (should (string= (llm-tool-name (car ai-org-chat-tools)) "tool1"))

    ;; Test registering a second tool
    (ai-org-chat-register-tool tool2)
    (should (equal (length ai-org-chat-tools) 2))

    ;; Test updating an existing tool
    (let ((updated-tool1 (llm-make-tool
                          :function #'ignore
                          :name "tool1"
                          :description "Updated first tool"
                          :args nil)))
      (ai-org-chat-register-tool updated-tool1)
      (should (equal (length ai-org-chat-tools) 2))
      (should (string= (llm-tool-description
                        (seq-find (lambda (tool)
                                    (string= (llm-tool-name tool) "tool1"))
                                  ai-org-chat-tools))
                       "Updated first tool")))

    ;; Test error on non-tool input
    (should-error (ai-org-chat-register-tool "not-a-tool"))))

(ert-deftest test-ai-org-chat--lookup-tool ()
  "Test tool lookup functionality."
  (let* ((ai-org-chat-tools nil)
         (tool1 (llm-make-tool
                 :function #'ignore
                 :name "tool1"
                 :description "First test tool"
                 :args nil))
         (tool2 (llm-make-tool
                 :function #'ignore
                 :name "tool2"
                 :description "Second test tool"
                 :args nil)))

    ;; Setup tools
    (setq ai-org-chat-tools (list tool1 tool2))

    ;; Test finding existing tools
    (should (eq (ai-org-chat--lookup-tool "tool1") tool1))
    (should (eq (ai-org-chat--lookup-tool "tool2") tool2))

    ;; Test non-existent tool
    (should (eq (ai-org-chat--lookup-tool "nonexistent") nil))))

(ert-deftest test-ai-org-chat--collect-tools ()
  "Test collection of tools from org properties."
  (let* ((ai-org-chat-tools nil)
         (tool1 (llm-make-tool
                 :function #'ignore
                 :name "tool1"
                 :description "First test tool"
                 :args nil))
         (tool2 (llm-make-tool
                 :function #'ignore
                 :name "tool2"
                 :description "Second test tool"
                 :args nil)))

    ;; Setup tools
    (setq ai-org-chat-tools (list tool1 tool2))

    (with-temp-buffer
      (insert "* First heading
:PROPERTIES:
:TOOLS: tool1
:END:

** Second heading
:PROPERTIES:
:TOOLS: tool2
:END:

*** Third heading
")
      (org-mode)
      (goto-char (point-max))

      ;; Test collecting tools from current and ancestor nodes
      (should (equal (ai-org-chat--collect-tools)
                     (list tool1 tool2)))

      ;; Test with non-existent tool
      (goto-char (point-min))
      (org-set-property "TOOLS" "tool1 nonexistent tool2")
      (goto-char (point-max))
      (should (equal (ai-org-chat--collect-tools)
                     (list tool1 tool2))))))

(ert-deftest test-ai-org-chat--filter-drawers ()
  "Test filtering of specific drawers from text."
  ;; Test basic drawer filtering
  (let ((text "Some text before drawer
:TEST_DRAWER:
Drawer content that should be removed
:END:
Text after drawer")
        (excluded '("TEST_DRAWER")))
    (should (equal (ai-org-chat--filter-drawers text excluded)
                   "Some text before drawer
Text after drawer")))

  ;; Test with multiple drawers
  (let ((text "Start text
:DRAWER1:
Content in drawer 1
:END:
Middle text
:DRAWER2:
Content in drawer 2
:END:
End text")
        (excluded '("DRAWER1" "DRAWER2")))
    (should (equal (ai-org-chat--filter-drawers text excluded)
                   "Start text
Middle text
End text")))

  ;; Test with selective exclusion
  (let ((text "Start text
:DRAWER1:
Content in drawer 1
:END:
Middle text
:DRAWER2:
Content in drawer 2
:END:
End text")
        (excluded '("DRAWER1")))
    (should (equal (ai-org-chat--filter-drawers text excluded)
                   "Start text
Middle text
:DRAWER2:
Content in drawer 2
:END:
End text")))

  ;; Test with indented drawers
  (let ((text "Start text
  :INDENTED:
  Indented drawer content
  :END:
End text")
        (excluded '("INDENTED")))
    (should (equal (ai-org-chat--filter-drawers text excluded)
                   "Start text
End text")))

  ;; Test with no drawers
  (let ((text "Text with no drawers present")
        (excluded '("TEST_DRAWER")))
    (should (equal (ai-org-chat--filter-drawers text excluded)
                   "Text with no drawers present")))

  ;; Test with empty text
  (let ((text "")
        (excluded '("TEST_DRAWER")))
    (should (equal (ai-org-chat--filter-drawers text excluded)
                   "")))

  ;; Test with nested content resembling drawers (should not be affected)
  (let ((text "Some code example:
```
:FAKE_DRAWER:
This is not a real drawer
:END:
```
Real text")
        (excluded '("FAKE_DRAWER")))
    (should (equal (ai-org-chat--filter-drawers text excluded)
                   "Some code example:
```
:FAKE_DRAWER:
This is not a real drawer
:END:
```
Real text")))

  ;; Test with case-insensitivity
  (let ((text "Some text
:properties:
Key: Value
:end:
More text")
        (excluded '("PROPERTIES")))
    (should (equal (ai-org-chat--filter-drawers text excluded)
                   "Some text
More text"))))

(ert-deftest test-ai-org-chat--filter-tool-drawers ()
  "Test filtering of tool drawers using the configured excluded drawers list."
  ;; Save original value to restore after test
  (let ((original-excluded-drawers ai-org-chat-excluded-drawers))
    (unwind-protect
        (progn
          ;; Set to known values for testing
          (setq ai-org-chat-excluded-drawers '("TOOL_CALL" "TOOL_RESULT"))
          
          ;; Test standard tool drawer filtering
          (let ((text "Text before
:TOOL_CALL:
{\"name\":\"test_tool\",\"arguments\":{\"arg1\":\"value1\"}}
:END:
Middle text
:TOOL_RESULT:
Some result data
:END:
Text after"))
            (should (equal (ai-org-chat--filter-tool-drawers text)
                         "Text before
Middle text
Text after")))
          
          ;; Test with only one type of drawer
          (let ((text "Text with only result
:TOOL_RESULT:
Tool result data
:END:
End text"))
            (should (equal (ai-org-chat--filter-tool-drawers text)
                         "Text with only result
End text")))
          
          ;; Test with other drawers that should not be filtered
          (let ((text "Text with other drawer
:OTHER_DRAWER:
Other content
:END:
End text"))
            (should (equal (ai-org-chat--filter-tool-drawers text)
                         "Text with other drawer
:OTHER_DRAWER:
Other content
:END:
End text"))))
      
      ;; Restore original value
      (setq ai-org-chat-excluded-drawers original-excluded-drawers))))

(provide 'ai-org-chat-tests)
