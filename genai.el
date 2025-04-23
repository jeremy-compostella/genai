;;; genai.el --- Client Interface to interact with Generative AI models. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: April 2025
;; Keywords: extensions AI
;; Homepage: https://github.com/jeremy-compostella/genai
;; Package-Version: 1.0
;; Package-Requires: ((emacs "29.4") (magit "3.3.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The GenAI Emacs module provides a client interface to interact with
;; generative AI models directly from within Emacs. Using engineered AI
;; prompt, it allows users to perform various tasks such as:

;; - Text manipulation: Correcting grammar, enhancing clarity,
;;   shortening, and expanding text.
;; - Code modification: Simplifying, documenting, fixing, completing,
;;   and refactoring code.
;; - Generating commit messages: Automatically creates detailed commit
;;   messages based on the changes in a Git repository.
;; - Document summarization: Generates summaries of selected text or
;;   documents.
;; - Email replies: Assists in drafting replies to emails.
;; - Fixing compilation errors: Attempts to fix code based on
;;   compilation error messages.
;; - General AI prompting: Allows users to directly query the AI model
;;   with custom system and user prompts.

;; The module uses "transient" menus for easy access to these features
;; and allows configuration of available AI models, prompts, and other
;; settings. It also includes debugging capabilities with logging. It
;; supports different modes like text, code, compilation, commit, and
;; message, adapting its functionality to the current context. The
;; module also supports extending acronyms, replacing redundant words
;; with synonyms, and stripping markdown syntax.

;; It also comes with a genai-overlay module which is specifically
;; designed to help visualize and navigate the changes made by the AI
;; model.

;; This module provides an abstraction layer for interacting with AI
;; model services.  It allows switching between different AI service
;; providers (e.g., OpenAI, Google AI) with minimal code changes.

;;; Code:

(require 'cl)
(require 'cl-lib)
(require 'genai-overlay)
(require 'magit)
(require 'org)
(require 'pulse)
(require 'transient)

;; Generative AI model support
(defstruct genai-model
  "Generative AI model data structure."
  (name "Name of the model.")
  (request "Function to place a request the model."))

(defcustom genai-models '()
  "The list of available Generative AI model."
  :type 'list)

(defcustom genai-pandoc-command "pandoc"
  "The pandoc command to use to convert markdown to org-mode."
  :type 'string)

;; Debug
(defcustom genai-debug-log-buffer "*genai-logs*"
  "The name of the buffer to log debug information."
  :type 'string)

(defcustom genai-debug-log nil
  "Whether to log debug information systematically. Logs are
appended to the `genai-debug-log-buffer'."
  :type 'boolean)

;; Transient Arguments
(defcustom genai-transient-generic-arguments
  ["Generic arguments:"
   ("-f" genai--read-model)
   ("-m" genai--read-maximum-word-number)
   ("!r" "Do not replace the original content (append instead)"
    ("!r" "do-not-replace"))
   ("-s" "Edit the system prompt" ("-s" "edit-system-prompt"))
   ("-u" "Edit the user prompt" ("-e" "edit-user-prompt"))
   ("-D" "Log the user prompt, system prompt, and response"
    ("-d" "debug-log"))]
  "Generic arguments for the GenAI transient.")

;; Text
(defcustom genai-prompt-correct-text "Provide an orthographically and\
 grammatically correct version this text. Only return the replacement\
 text. If there is no needed correction, return the same text as-is."
  "System prompt to be used when correcting text."
  :type 'string)

(defcustom genai-prompt-enhance-text "Provide a slightly different\
 version of this text. Only return the replacement text."
  "Base system prompt to be used to enhance text."
  :type 'string)

(defcustom genai-acronyms-alist '()
  "Associative list of acronyms and definitions. It is the database\
 used to expand acronyms when requested by the user."
  :type 'alist)

(defcustom genai-transient-text-modification
  ["Perform action on the selected text or current paragraph:"
   ("f" "Correct text" genai-correct-text)
   ("c" "Enhance clarity" genai-clarify-text)
   ("s" "Shrink text" genai-shorten-text)
   ("e" "Extend text" genai-expand-text)]
  "Generic text modification options.")

(defcustom genai-transient-text-options
  ["Text options:"
   ("-x" "Expand acronyms (see genai-acronyms-alist)"
    ("-x" "extend-acronyms"))
   ("-d" "Replace redundant words with synonyms"
    ("-d" "replace-duplicate"))
   ("-S" "Strip mardown syntax" ("-S" "strip-markdown"))]
  "Text arguments for the GenAI transient.")

;; Commit message
(defcustom genai-prompt-commit-message "As an experienced developer,\
 write a detailed commit message at the present tense. Include a title\
 line limited to 72 characters. Include information about the issue\
 being addressed. Do not include a diff statistic. Do not add any\
 header or footer to the generated text. Copy tags such as `BUG=...`\
 and `TEST=...` as-is if already present."
  "System prompt to be used when writing a commit message."
  :type 'string)

(defcustom genai-commit-insert-user-sign-off t
  "Whether to insert a user Signed-off-by in the commit message."
  :type 'boolean)

;; Document
(defcustom genai-prompt-summary "As an educated technical engineer,\
 write a short and concise summary emphasizing the results and the
 impacts."
  "System prompt to be used when generating a summary."
  :type 'string)

(defcustom genai-doc-mode-list '(org-mode markdown-mode)
  "List of major modes for which document capabilities is desired."
  :type 'list)

;; Email
(defcustom genai-prompt-email-reply "Reply to the following\
 email. Do not include the subject, sender, or any other email\
 metadata. Do not say Hi or Regards."
  "System prompt to be used when replying to an email."
  :type 'string)

(defcustom genai-message-mode-list '(message-mode org-msg-edit-mode)
  "List of major modes for which message capabilities are desired."
  :type 'list)

;; Code
(defcustom genai-prompt-simplify-code "Provide a simplified replacement\
 of this code."
  "System prompt to be used when simplifying code."
  :type 'string)

(defcustom genai-prompt-document-code "Provide a replacement for this\
 code with inline documentation."
  "System prompt to be used when documenting code."
  :type 'string)

(defcustom genai-prompt-fix-code "Provide a fixed version of the\
 following code. If the code is already correct, return the same code\
 as-is."
  "System prompt to be used when fixing code."
  :type 'string)

(defcustom genai-prompt-complete-code "Complete this code by providing\
 a full replacement for it."
  "Initial system prompt to be used when completing code."
  :type 'string)

(defcustom genai-prompt-refactor-code "Refactor this code and offer a\
 complete substitute for it."
  "Initial system prompt to be used when refactoring code."
  :type 'string)

(defcustom genai-code-mode-list '(c-mode emacs-lisp-mode python-mode
				  shell-script-mode)
  "List of major modes for which code capabilities is desired."
  :type 'list)

;; Compilation
(defcustom genai-prompt-fix-compilation-error "As a developer, return\
 a code replacement for the following code. This replacement code must\
 fix the specific following compilation error. It should only fix this\
 unique compilation error."
  "System prompt to be used when fixing code."
  :type 'string)

;; Generative AI entry point
(defcustom genai-entry-key "C-c g"
  "Key binding for GenAI transient entry point."
  :type 'string)

;; Internal variables
(defvar genai-system-prompt-history '()
  "History of system prompts.")

(defvar genai-user-prompt-history '()
  "History of user prompts.")

(defun genai-debug-log (entry data)
  "Log ENTRY and DATA into the `genai-debug-log-buffer'.

The buffer uses `org-mode' for better structure and readability.

ENTRY is the title or identifier of the log entry and DATA the
content to be logged, formatted as markdown."
  (with-current-buffer (get-buffer-create genai-debug-log-buffer)
    (when (= (point-min) (point-max))
      (org-mode))
    (save-excursion
      (goto-char (point-max))
      (if (stringp data)
	  (insert (format "* %s\n#+begin_src markdown\n%s\n#+end_src\n"
			  entry data))
	(insert (format "* %s\n#+begin_src\n%S\n#+end_src\n"
			entry data))))))

(defun genai-error-callback (data)
  "Callback function printing error in a failure situation."
  (genai-debug-log "Error" data)
  (error "GenAI server response error, consult %s buffer for details"
	 genai-debug-log-buffer))

(defun genai--markdown-to-org (markdown)
  "Concert MARKDOWN string to org-mode string.
The function creates a temporary file with the markdown content and uses
pandoc to convert the markdown content to org-mode. The function returns
the org-mode content as string."
  (if (executable-find genai-pandoc-command)
      (let* ((tmp-md-file (make-temp-file "genai" nil ".md"))
	     (tmp-org-file (concat tmp-md-file ".org")))
	(with-temp-buffer
	  (insert markdown)
	  (write-file tmp-md-file))
	(call-process genai-pandoc-command nil nil nil
		      "-f" "markdown" "-t" "org"
		      "--wrap=preserve" "-o" tmp-org-file tmp-md-file)
	(with-temp-buffer
	  (insert-file-contents-literally tmp-org-file)
	  (buffer-string)))
    (warn "genai: %s command not found on the system, mardown to\
 org-mode conversion aborted." genai-pandoc-command)
    markdown))

(defun genai--get-arg (name)
  "Retrieves argument NAME from `genai--arguments' or falls back
to 'genai-' prefixed variable value, converting to number if paired with
a value."
  (if-let ((found (cl-find name (genai--arguments) :key #'car
			   :test #'string=)))
      (if (cdr found)
	  (cond ((string-match-p "^[0-9]+$" (cadr found))
		 (string-to-number (cadr found)))
		((cadr found)))
	t)
    (when-let ((symb (intern-soft (concat "genai-" name))))
      (when (boundp symb)
	(symbol-value symb)))))

(defun genai--acronym-extension-prompt ()
  "Return the prompt to extend acronyms."
  (concat " Add the full name of used acronyms, keep the acronym in\
 parentheses, and here is a list of acronyms to consider:"
	  (mapconcat (lambda (x) (concat (car x) "=" (cdr x)))
		     genai-acronyms-alist ", ")
	  "."))

(defun genai-request (system-prompt user-prompt callback
				    &optional model)
  "Ask a question to a Generative AI Model service."
  (lexical-let ((model (or (when-let ((name (genai--get-arg "model")))
			     (cl-find name genai-models
				      :key #'genai-model-name
				      :test #'string=))
			   model (car (last genai-models)))))
    (unless model
      (error "No available Generative AI model."))
    (setq genai-user-prompt-history (cons user-prompt
					  genai-user-prompt-history))
    (let ((max-words (genai--get-arg "maximum-number-of-words")))
      (when (genai--get-arg "edit-system-prompt")
	(setf system-prompt (read-string "System prompt: "
					 system-prompt)))
      (when (genai--get-arg "edit-user-prompt")
	(setf user-prompt (read-string "User prompt: " user-prompt)))
      (when (and max-words (< max-words genai-maximum-number-of-words))
	(setf system-prompt (format "%s. Limit the response to at most\
 %d words."
				    system-prompt max-words)))
      (when (genai--get-arg "extend-acronyms")
	(setf system-prompt (concat system-prompt
				    (genai--acronym-extension-prompt))))
      (when (genai--get-arg "replace-duplicate")
	(setf system-prompt
	      (concat system-prompt " Replace duplicate word with\
 synonyms.")))
      (lexical-let* ((progress (make-progress-reporter
				(format "Waiting for %s AI model\
 response"
					(genai-model-name model))))
		     (debug-log (genai--get-arg "debug-log"))
		     (update-timer
		      (run-at-time .5 .5
				   (lambda ()
				     (progress-reporter-update
				      progress))))
		     (callback-wrapper
		      (lambda (data)
			(cancel-timer update-timer)
			(progress-reporter-done progress)
			(when debug-log
			  (genai-debug-log "Response" data))
			(funcall callback data)))
		     (error-callback-wrapper
		      (lambda (data)
			(cancel-timer update-timer)
			(progress-reporter-done progress)
			(genai-error-callback data))))
	(when debug-log
	  (genai-debug-log "System Prompt" system-prompt)
	  (genai-debug-log "User Prompt" user-prompt))
	(condition-case err
	    (funcall (genai-model-request model) system-prompt user-prompt
		     callback-wrapper error-callback-wrapper)
	  (error
	   (cancel-timer update-timer)
	   (error (cadr err))))))))

;;;###autoload
(defun genai--org-mode-to-markdown (text)
  "Convert an org-mode TEXT to markdown."
  (with-temp-buffer
    (let ((org-export-show-temporary-export-buffer nil))
      (insert "#+options: toc:nil\n")
      (insert text)
      (org-previous-visible-heading 0)
      (org-md-export-as-markdown))
    (with-current-buffer "*Org MD Export*"
      (goto-char (point-min))
      (dolist (replacement '(("<sub>" . "_") ("</sub>" . "")))
	(save-excursion
	  (while (search-forward (car replacement) nil t)
	    (replace-match (cdr replacement)))))
      (buffer-string))))

(defun genai--fill-column-on-text (text fill-column-value)
  "Format TEXT to a specified fill column width."
  (with-temp-buffer
    (org-mode)
    (setq-local fill-column fill-column-value)
    (insert text)
    (goto-char (point-max))
    (dolist (tag '("BUG=" "TEST=" "Signed-off-by:" "Change-Id:"))
      (when (re-search-backward (concat "^" tag) nil t)
	(forward-line -1)))
    (fill-region (point-min) (point))
    (buffer-string)))

(defun genai--replace-with-differences (orig-beg orig-end new-text)
  "Replace text between ORIG-BEG and ORIG-END with NEW-TEXT and
highlight differences."
  (let ((save-point (point)))
    (goto-char orig-end)
    (let ((newline (= (line-beginning-position) (line-end-position)))
	  (beg (point)))
      (insert new-text)
      (genai--overlay-compare-regions orig-beg orig-end orig-end (point))
      (let ((end (point)))
	(if newline
	    (when (not (= (line-beginning-position) (line-end-position)))
	      (insert "\n"))
	  (when (= (line-beginning-position) (line-end-position))
	    (delete-char -1)))
	(when (pulse-available-p)
	  (when-let ((overlays (genai--overlays beg end)))
	    (let ((beg (overlay-start (car overlays)))
		  (end (overlay-end (car (last overlays)))))
	      (pulse-momentary-highlight-region beg end))))
	(delete-region orig-beg orig-end)))
    (goto-char save-point)))

(defun genai--org-modep ()
  "Return t if the current buffer is in org-mode or a well-known
org-mode derived mode."
  (or (eq major-mode 'org-mode) (eq major-mode 'org-msg-edit-mode)))

(defun genai--extract-code-block (markdown
				  &optional fallback-to-original)
  "Extract code block from the given MARKDOWN text.
If no code block is found and FALLBACK-TO-ORIGINAL is non-nil,
return the original MARKDOWN text."
  (with-temp-buffer
    (insert markdown)
    (goto-char (point-min))
    (save-excursion
      (delete-trailing-whitespace (point-min) (point-max)))
    (if (re-search-forward "^```[a-zA-Z]*" nil t)
 	(progn
 	  (forward-line)
	  (let ((beg (point)))
	    (when (re-search-forward "^```" nil t)
	      (buffer-substring-no-properties
	       beg (line-beginning-position)))))
      (when fallback-to-original
	markdown))))

(defun genai--markdown-to-text (markdown)
  "Convert MARKDOWN to plain text.
The function removes specific markdown syntax elements."
  (with-temp-buffer
    (insert markdown)
    (dolist (syntax-string '("**" "`" "_" "~"))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward syntax-string nil t)
          (replace-match ""))))
    (buffer-string)))

(defun genai-text-callback (strip-markdown beg end data)
  "Handles GenAI text generation responses.

This function is designed to be called with the response from an
asynchronous text generation service. It inserts or replaces text.

Arguments:
BEG - The beginning marker where text replacement should start.  If BEG
      is nil, the function will simply insert text at the current point.
END - The ending marker in the buffer where text replacement should end.
      If END is nil, and BEG is also nil, the function will simply
      insert text.
DATA - An alist containing the response data from the text generation
       service."
  (with-current-buffer (marker-buffer beg)
    (when (eq major-mode 'text-mode)
      (setf data (genai--extract-code-block data t))
      (when strip-markdown
	(message "Strip markdown syntax")
	(setf data (genai--markdown-to-text data)))
      (setf data (genai--fill-column-on-text data fill-column)))
    (when (genai--org-modep)
      (setf data (genai--markdown-to-org data)))
    (let ((beg (marker-position beg))
	  (end (marker-position end)))
      (if (and beg end)
	  (genai--replace-with-differences beg end data)
	(save-excursion
	  (goto-char beg)
	  (insert data)
	  (when (pulse-available-p)
	    (pulse-momentary-highlight-region beg (point))))))))

(defun genai--get-current-text-region ()
  "Retrieve text either from the selected region or from the
current paragraph if no region is active."
  (cl-flet ((format-text (text)
	      (format "Here is the text:\n%s" text)))
    (if (use-region-p)
	(let* ((beg (region-beginning))
               (end (region-end))
               (text (buffer-substring-no-properties beg end)))
          (deactivate-mark)
          (when (genai--org-modep)
            (setf text (genai--org-mode-to-markdown text)))
          (list (format-text text) beg end))
      (save-excursion
	(if (genai--is-in-commit-message-buffer)
	    (cl-multiple-value-bind (beg end)
		(genai--current-commit-message)
	      (if beg
		  (list (format-text (buffer-substring beg end))
			beg end)
		(error "No text to work on.")))
	  (let ((beg (line-beginning-position)))
            (forward-paragraph)
            (backward-char)
            (list (format-text (buffer-substring-no-properties beg
							       (point)))
		  beg (point))))))))

(defun genai--prep-text-callback (beg end)
  "Generate the text callback function."
  (apply-partially #'genai-text-callback
		   (genai--get-arg "strip-markdown")
		   (copy-marker beg) (copy-marker end)))

(defun genai-enhance-text (text beg end &optional extra-system-prompt)
  "Rewrite existing text. When a region is active, the selected
text becomes the default for rewriting. The newly generated text
is inserted at the cursor's location unless a replacement
operation has been requested."
  (interactive (genai--get-current-text-region))
  (let ((replace (not (genai--get-arg "do-not-replace")))
	(system-prompt genai-prompt-enhance-text))
    (when extra-system-prompt
      (setf system-prompt (concat system-prompt extra-system-prompt)))
    (genai-request system-prompt text
		   (if replace
		       (genai--prep-text-callback beg end)
		     (genai--prep-text-callback (point) nil)))))

(defun genai-clarify-text (text beg end)
  "Replace TEXT with clarified version."
  (interactive (genai--get-current-text-region))
  (genai-enhance-text text beg end " Improve clarity."))

(defun genai-shorten-text (text beg end percent)
  "Replace TEXT with a shortened version."
  (interactive (append (genai--get-current-text-region)
		       (list (read-number "Percent of current text: "
					  50))))
  (let ((prompt (format " Shrink the text to %d %% of the original."
			percent)))
    (genai-enhance-text text beg end prompt)))

(defun genai-expand-text (text beg end percent)
  "Replace TEXT with an expanded version."
  (interactive (append (genai--get-current-text-region)
		       (list (read-number "Percent of current text: "
					  150))))
  (let ((prompt (format " Expand the text to %d %% of the original."
			percent)))
    (genai-enhance-text text beg end prompt)))

(defun genai-correct-text (text beg end)
  "Correct existing text. When a region is active, the selected text
becomes the default text for correction. The correction is
inserted at the cursor's position unless a replacement has been
specified."
  (interactive (genai--get-current-text-region))
  (let ((replace (not (genai--get-arg "do-not-replace"))))
    (genai-request genai-prompt-correct-text text
		   (if replace
		       (genai--prep-text-callback beg end)
		     (genai--prep-text-callback (point) nil)))))

(defun genai--current-commit-message ()
  "Retrieve the start and end positions of the commit message text.

Returns a list of two elements: the starting and ending positions
of the commit message text. Returns nil if no valid commit
message is found."
  (when (genai--is-in-commit-message-buffer)
    (let ((pattern "# Please enter the commit message for your change"))
      (save-excursion
	(goto-char (point-min))
	(when (search-forward pattern nil t)
	  (let* ((end (1- (line-beginning-position)))
		 (msg (buffer-substring-no-properties (point-min) end)))
	    (unless (string-blank-p msg)
	      (cl-values (point-min) end))))))))

(defun genai--create-files-context (files directory &optional progress)
  (with-temp-buffer
    (let ((default-directory directory))
      (dolist (file files)
	(when progress
	  (progress-reporter-update progress
				    (format "Adding file %s" file)))
	(insert "================================================\n")
	(insert "File: " file "\n")
	(insert "================================================\n")
	(process-file "git" nil t nil "show" (concat "HEAD:" file)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun genai--create-diff-context (buffer progress)
  "Create a diff context for the given change."
  (let ((files '())
	(directory default-directory))
    (with-current-buffer buffer
      (goto-char (point-max))
      (while (re-search-backward "^modified " nil t)
	(forward-line -1))
      (while (not (= (point) (point-min)))
	(setf files (cons (magit-diff--file-at-point) files))
	(forward-line -1))
      (setf files (cl-delete-if-not #'stringp files)))
    (genai--create-files-context files directory progress)))

(defun genai-insert-commit-message ()
  "Generate detailed Git commit messages.

This function interacts with Magit to collect the changes committed. I
also build a context by including the modified files in their original
form."
  (interactive)
  (let* ((progress (make-progress-reporter
		    "Creating commit message context..."))
	 (diff-buffer (save-window-excursion
			(magit-diff-while-committing)))
	 (change (with-current-buffer diff-buffer
		   (buffer-string)))
	 (context (genai--create-diff-context diff-buffer progress))
	 (system-prompt)
	 (rewrite nil)
	 (original-message ""))
    (cl-flet ((add-to-prompt (&rest sequences)
		(setf system-prompt
		      (apply #'concat (append (list system-prompt)
					      sequences)))))
      (cl-multiple-value-bind (beg end)
	  (genai--current-commit-message)
	(add-to-prompt "\nConsider the following original files:\n"
		       context)
	(add-to-prompt  genai-prompt-commit-message)
	(if beg
	    (setf original-message
		  (buffer-substring-no-properties beg end))
	  (setf beg (point-min)))
	(when genai-commit-insert-user-sign-off
	  (add-to-prompt (format "\nInclude Signed-off-by tag for %s <%s>"
				 user-full-name user-mail-address)))
	(progress-reporter-done progress)
	(genai-request system-prompt
		       (format "%s%s\n" original-message change)
		       (genai--prep-text-callback beg end))))))

(defun genai-insert-summary (system-prompt user-prompt)
  "Insert a summary of the current region at the cursor's"
  (interactive (list (read-string "System prompt: "
				  genai-prompt-summary
				  'genai-system-prompt-history)
		     (if (use-region-p)
			 (buffer-substring-no-properties
			  (region-beginning) (region-end))
		       (read-string "Text to summarize prompt: "))))
  (when (use-region-p)
    (deactivate-mark))
  (genai-request system-prompt
		 (genai--org-mode-to-markdown user-prompt)
		 (genai--prep-text-callback (point) nil)))

(defun genai-reply-to-email (system-prompt)
  "Reply to an email. The user is prompted for a system prompt."
  (interactive (list (read-string "System prompt: "
				  genai-prompt-email-reply
				  'genai-system-prompt-history)))
  (when (eq major-mode 'org-msg-edit-mode)
    (genai-request system-prompt
		   (buffer-substring-no-properties
		    (point-min) (point-max))
		   (genai--prep-text-callback (point) nil))))

(defun genai-ask-and-insert (system-prompt user-prompt)
  "Prompts for a system prompt and a user prompt to ask the AI model
a question. The result is inserted at point."
  (interactive (list (read-string "System prompt: " ""
				  'genai-system-prompt-history "")
		     (read-string "User prompt: " ""
				  'genai-user-prompt-history "")))
  (genai-request system-prompt user-prompt
		 (genai--prep-text-callback (point) nil)))

(defun genai--read-maximum-word-number (prompt default-time _history)
  (string-to-number
   (transient-read-number-N+ "Maximum number of words to generate: "
			     (number-to-string
			      genai-maximum-number-of-words)
			     nil)))

(defun genai-code-callback (beg end data)
  "Handles code generation responses from GenAI."
  (with-current-buffer (marker-buffer beg)
    (let* ((code (genai--extract-code-block data)))
      (unless code
	(setf code data))
      (when (string-empty-p code)
	(error "No code generated."))
      (select-window (display-buffer (current-buffer)))
      (let ((beg (marker-position beg))
	    (end (marker-position end)))
	(if (and beg end)
	    (genai--replace-with-differences beg end code)
	  (save-excursion
	    (goto-char beg)
	    (indent-region beg (point))
	    (when (pulse-available-p)
	      (pulse-momentary-highlight-region beg (point)))))))))

(defun genai--get-current-code-region ()
  "Retrieve the current code region from the buffer.

This function determines whether there is an active region in the
buffer.  If a region is active, it extracts the code within the region,
formats it with markdown code block syntax, and returns it along with
its beginning and end positions, then deactivates the region.

If no region is active, the function identifies the bounds of the
current function using `beginning-of-defun' and `end-of-defun'. It
extracts the code within these bounds, formats it similarly, and returns
it along with its beginning and end positions."
  (cl-flet ((format-code (code)
              "Wrap CODE with markdown code block syntax."
              (format "```\n%s```\n" code)))
    (if (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (code (buffer-substring-no-properties beg end)))
          (deactivate-mark)
          (list (format-code code) (copy-marker beg) (copy-marker end)))
      (save-excursion
	(beginning-of-defun)
	(let ((beg (point)))
          (end-of-defun)
          (list (format-code
		 (buffer-substring-no-properties beg (point)))
                (copy-marker beg) (point-marker)))))))

(defun genai--git-diff (default-directory)
  "Generate a Git diff for the given DIRECTORY."
  (when (file-exists-p (concat default-directory ".git"))
    (with-temp-buffer
      (process-file "git" nil t nil "diff" "HEAD")
      (unless (= (point-min) (point-max))
	(buffer-string)))))

(defun genai--modify-code (code beg end system-prompt)
  (let* ((top-level (magit-toplevel))
	 (git-diff (genai--git-diff top-level))
	 (files (when (magit-current-file)
		  (genai--create-files-context
		   (list (magit-current-file)) top-level))))
    (when (genai--get-arg "do-not-replace")
      (setf end nil))
    (when git-diff
      (setf system-prompt
	    (concat system-prompt
		    "I am currently working on making some changes."
		    "Here are the current changes:\n"
		    (format "```diff\n%s```\n" git-diff))))
    (when files
      (setf system-prompt
	    (concat "\nHere is the full file content for context:\n"
		    files
		    system-prompt)))
    (genai-request system-prompt
		   (concat "Here is the code we want a replacement\
 for:\n"
		       code)
		   (apply-partially #'genai-code-callback beg end))))

(defun genai-simplify-code (code beg end)
  "Replace CODE with a simplified version."
  (interactive (genai--get-current-code-region))
  (genai--modify-code code beg end genai-prompt-simplify-code))

(defun genai-document-code (code beg end)
  "Replace CODE with a documented version."
  (interactive (genai--get-current-code-region))
  (genai--modify-code code beg end genai-prompt-document-code))

(defun genai-fix-code (code beg end)
  "Replaced CODE with a fixed version."
  (interactive (genai--get-current-code-region))
  (genai--modify-code code beg end genai-prompt-fix-code))

(defun genai-complete-code (code beg end instructions)
  (interactive (append (genai--get-current-code-region)
		       (list (read-string "Instructions: "
					  genai-prompt-complete-code))))
  (genai--modify-code code beg end instructions))

(defun genai-refactor-code (code beg end instructions)
  (interactive (append (genai--get-current-code-region)
		       (list (read-string "Instructions: "
					  genai-prompt-refactor-code))))
  (genai--modify-code code beg end instructions))

(defun genai--compile-error-location ()
  "Return the location of the file being compiled.

This function can be advised to handle situations where figuring
out the filename path is out of the ordinary."
  (let* ((message (get-text-property (point) 'compilation-message))
	 (loc (compilation--message->loc message))
	 (filename (car (caaddr loc)))
	 (line (cadr loc)))
    (when (and filename line)
      (list (expand-file-name filename) line))))

(defun genai-fix-compilation-error ()
  (interactive)
  "Replaced CODE with a fixed version."
  (unless (eq major-mode 'compilation-mode)
    (error "Not in a compilation buffer"))
  (let ((err (buffer-substring-no-properties (line-beginning-position)
					     (line-end-position))))
    (cl-multiple-value-bind (filename line)
	(genai--compile-error-location)
      (unless (file-exists-p filename)
	(error "%s does not exist" filename))
      (with-current-buffer (find-file-noselect filename)
	(cl-multiple-value-bind (code beg end)
	    (genai--get-current-code-region)
	  (let ((instr (format "%s\nCompilation error: '%s'\n"
			       genai-prompt-fix-compilation-error err)))
	  (genai--modify-code code beg end instr)))))))

(transient-define-argument genai--read-maximum-word-number ()
  :description "Maximum number of words to generate"
  :class 'transient-option
  :argument "maximum-number-of-words="
  :allow-empty t
  :reader #'transient-read-number-N+)

(defun genai--model-reader (prompt initial-input history)
  "Select a generative AI model from the available models."
  (completing-read prompt (mapcar #'genai-model-name genai-models)))

(transient-define-argument genai--read-model ()
  "Define a transient argument for selecting an alternative AI model."
  :description "Force an alternative model (instead of default)"
  :class 'transient-option
  :argument "model="
  :allow-empty t
  :reader #'genai--model-reader)

(transient-define-prefix genai-doc-entry ()
  "Defines a transient menu for doc-related actions using GenAI."
  genai-transient-generic-arguments
  genai-transient-text-options
  genai-transient-text-modification
  ["Generate"
   ("S" "Summarize the selected text" genai-insert-summary)
   ("A" "Ask a question and insert the answer" genai-ask-and-insert)]
  (interactive)
  (transient-setup 'genai-doc-entry))

(transient-define-prefix genai-text-entry ()
  "Defines a transient menu for text-related actions using GenAI."
  genai-transient-generic-arguments
  genai-transient-text-options
  genai-transient-text-modification
  (interactive)
  (transient-setup 'genai-text-entry))

(transient-define-prefix genai-code-entry ()
  "Defines a transient menu for code-related actions using GenAI."
  genai-transient-generic-arguments
  ["Perform action on the selected code (or current function):"
   ("f" "Fix" genai-fix-code)
   ("s" "Simplify" genai-simplify-code)
   ("d" "Document" genai-document-code)
   ("c" "Complete" genai-complete-code)
   ("r" "Refactor" genai-refactor-code)]
  (interactive)
  (transient-setup 'genai-code-entry))

(transient-define-prefix genai-compilation-entry ()
  "Defines a transient menu for compile-related actions using GenAI."
  genai-transient-generic-arguments
  ["Perform action on error at the current line:"
   ("f" "Fix" genai-fix-compilation-error)]
  (interactive)
  (transient-setup 'genai-compilation-entry))

(transient-define-prefix genai-commit-entry ()
  "Defines a transient menu for commit-related actions using GenAI."
  genai-transient-generic-arguments
  genai-transient-text-options
  genai-transient-text-modification
  ["Action:"
   ("g" "Generate commit message" genai-insert-commit-message)]
  (interactive)
  (transient-setup 'genai-commit-entry))

(transient-define-prefix genai-message-entry ()
  "Defines a transient menu for message-related actions using GenAI."
  genai-transient-generic-arguments
  genai-transient-text-options
  genai-transient-text-modification
  ["Action:"
   ("r" "Reply" genai-reply-to-email)]
  (interactive)
  (transient-setup 'genai-message-entry))

(defun genai--arguments nil
  (let* ((entries '(genai-doc-entry genai-text-entry genai-code-entry
		    genai-commit-entry genai-message-entry
		    genai-compilation-entry))
	 (args (nconc (delq nil (mapcar 'transient-args entries)))))
    (mapcar (lambda (x) (split-string x "=")) (car args))))

(defun genai--is-in-commit-message-buffer ()
  "Check if the current buffer is a commit message buffer."
  (when-let (filename (buffer-file-name))
    (string-prefix-p "COMMIT_EDITMSG"
		     (file-name-nondirectory filename))))

;;;###autoload
(defun genai--region-is-inside-string-literal-p ()
  "Confirm the active region resides within a string literal."
  (and (thing-at-point 'string)
       (use-region-p)
       (let ((beg (region-beginning))
             (end (region-end)))
         (and (>= beg (save-excursion (beginning-of-thing 'string)))
	      (<= end (save-excursion (end-of-thing 'string)))))))

(defun genai-entry-code-triage ()
  "Triage entry point for genai-code minor code.

It evaluates the current position within the buffer to determine
if the context is within a comment or a documentation string. If
it is, it calls `genai-text-entry'. If not, it calls
`genai-code-entry'."
  (interactive)
  (if (or (nth 4 (syntax-ppss))
	  (let* ((point (if (use-region-p)
			    (+ (region-beginning)
			       (/ (- (region-end) (region-beginning) 2)))
			  (point)))
		 (face (get-text-property point 'face)))
	    (memq face '(font-lock-doc-face font-lock-comment-face)))
	  (genai--region-is-inside-string-literal-p))
      (genai-text-entry)
    (genai-code-entry)))

(defun genai-entry ()
  "Entry point for GenAI mode.

This function determines the appropriate GenAI operation to perform
based on the current buffer's context or major mode. It supports
different operations for commit messages, email or message editing, code
triage, and document editing.

This function should be bound to a convenient keybinding to quickly
access GenAI features relevant to the current buffer."
  (interactive)
  (cond
   ((and (boundp 'read-only-mode--state) read-only-mode--state)
    (error "Current buffer is read-only."))
   ((genai--is-in-commit-message-buffer)
    (genai-commit-entry))
   ((memq major-mode genai-message-mode-list)
    (genai-message-entry))
   ((memq major-mode genai-code-mode-list)
    (genai-entry-code-triage))
   ((memq major-mode genai-doc-mode-list)
    (genai-doc-entry))
   ((eq major-mode 'compilation-mode)
    (genai-compilation-entry))))

(define-minor-mode genai-mode
  "Toggle GenAI mode."
  :global t
  :init-value nil
  :lighter " GenAI"
  :keymap `((,(kbd genai-overlay-entry-key) . genai-overlay-entry)
	    (,(kbd genai-entry-key) . genai-entry)))

(provide 'genai)
