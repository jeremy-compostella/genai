;;; genai-gemini.el --- Google Gemini GenAI support. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: April 2025
;; Keywords: extensions AI
;; Homepage: https://github.com/jeremy-compostella/genai
;; Package-Version: 1.0
;; Package-Requires: ((emacs "29.4") (request "0.3.3"))

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

;; This module provides backend support for the genai module to interact
;; with Google's Gemini Generative AI model. It allows users to send
;; prompts to the Gemini API and receive responses directly within
;; Emacs. The module includes customizable options like the maximum
;; number of tokens, the specific Gemini model to use, and retrieves the
;; API key securely from the user's auth-source file.

;; It then registers the Google Gemini model as a supported model within
;; the genai framework, making it available for various tasks like code
;; generation, text completion, documentation, and commit message
;; creation.

;;; Code:
(require 'cl-macs)
(require 'genai)
(require 'json)
(require 'request)

(defcustom genai-gemini-maximum-number-of-tokens 800
  "Maximum number of token to generate."
  :type 'integer)

(defcustom genai-gemini-host "generativelanguage.googleapis.com"
  "URL prefix for the Google Gemini model service."
  :type 'string)

(defcustom genai-gemini-model "gemini-2.0-flash"
  "Model to use with the Google Gemini AI model service."
  :type 'string)

(defun genai-gemini-key ()
  "Return the key to use with the Google Gemini service.
The key should be in your auth-source file and bound to the
`genai-gemini-host' machine."
  (if-let ((auth-info
            (auth-source-search :max 1
                                :host genai-gemini-host
                                :require '(:secret))))
      (let ((secret (plist-get (car auth-info) :secret)))
	(if (functionp secret)
	    (funcall secret)
	  secret))))

(defun genai-gemini--callback (callback error-callback data)
  "Handle the response from the Google Gemini Model service."
  (if-let ((err (assoc-default 'error data)))
      (funcall error-callback err)
    (let ((candidates (assoc-default 'candidates data)))
      (dotimes (i (length candidates))
	(let* ((candidate (aref candidates i))
	       (content (assoc-default 'content candidate))
	       (parts (assoc-default 'parts content)))
	  (let ((markdown (assoc-default 'text (aref parts 0))))
	    (funcall callback markdown)))))))

(defun genai-gemini-request (system-prompt user-prompt callback
					   error-callback)
  "Ask a question to the Google Gemini Model service."
  (let ((prompt (concat system-prompt "\n" user-prompt))
	(key (genai-gemini-key))
	(url "https://%s/v1beta/models/%s:generateContent?key=%s"))
    (unless key
      (error "No Google Gemini key found, cf. `genai-gemini-key'"))
    (request (format url genai-gemini-host genai-gemini-model key)
      :type "POST"
      :headers `(("Content-Type" . "application/json"))
      :data (json-encode
           `(("contents" . [(("parts" . [(("text" . ,prompt))]))])
             ("safetySettings" . [(("category" .
				    "HARM_CATEGORY_DANGEROUS_CONTENT")
                                   ("threshold" . "BLOCK_ONLY_HIGH"))])
             ("generationConfig" .
              (("stopSequences" . nil)
               ("temperature" . 0.95)
               ("maxOutputTokens" .
		,genai-gemini-maximum-number-of-tokens)
               ("topP" . 0.85)
               ("topK" . nil)))))
      :error (lexical-let ((error-callback error-callback))
	       (cl-function
		(lambda (&key data &allow-other-keys)
		  (funcall error-callback data))))
      :parser 'json-read
      :complete (lexical-let ((callback callback)
			      (error-callback error-callback))
		  (cl-function
		   (lambda (&key data &allow-other-keys)
		     (genai-gemini--callback callback
					     error-callback data)))))))

;;;###autoload
(add-to-list 'genai-models
	     (make-genai-model :name "Google Gemini"
			       :request #'genai-gemini-request))

(provide 'genai-gemini)
