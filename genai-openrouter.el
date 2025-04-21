;;; genai-openrouter.el --- Openrouter GenAI support. -*- lexical-binding: t; -*-

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

;; This module provides backend support for the `genai' module, enabling
;; interaction with Openrouter Generative AI models. Users can send prompts to
;; the Openrouter API and receive responses directly within Emacs. The module
;; offers customizable options, such as the specific model to use, and retrieves
;; the API key securely from the user's `auth-source' file. Before installing
;; this module, the user must specify the `genai-openrouter-model' and the list
;; of supported material types (text, message, or code) on which the model is
;; expected to excel.

;;; Code:
(require 'cl-macs)
(require 'genai)
(require 'json)
(require 'request)

(defcustom genai-openrouter-host "openrouter.ai"
  "URL prefix for the Google Openrouter model service."
  :type 'string)

(defcustom genai-openrouter-model nil
  "Model to use with the Openrouter AI model service."
  :type 'string)

(defun genai-openrouter-key ()
  "Return the key to use with the Google Openrouter service.
The key should be in your auth-source file and bound to the
`genai-openrouter-host' machine."
  (if-let ((auth-info
            (auth-source-search :max 1
                                :host genai-openrouter-host
                                :require '(:secret))))
      (let ((secret (plist-get (car auth-info) :secret)))
	(if (functionp secret)
	    (funcall secret)
	  secret))))

(defun genai-openrouter--callback (callback error-callback data)
  "Handle the response from the Google Openrouter Model service."
  (let* ((choice (aref (assoc-default 'choices data) 0))
	 (message (assoc-default 'message choice))
	 (content (assoc-default 'content message)))
    (funcall callback content)))

(defun genai-openrouter-request (system-prompt user-prompt callback
					       error-callback)
  "Ask a question to the Google Openrouter Model service."
  (let ((prompt (concat system-prompt "\n" user-prompt))
	(key (genai-openrouter-key)))
    (unless key
      (error "No Google Openrouter key found, cf. `genai-openrouter-key'"))
    (request (format "https://%s/api/v1/chat/completions"
		     genai-openrouter-host)
      :type "POST"
      :data (json-encode
	     `((model . ,genai-openrouter-model)
	       (stop . nil)
	       (messages . [((role . system)
			     (content . ,system-prompt))
			    ((role . user)
			     (content . ,user-prompt))])))
      :headers `(("Content-Type" . "application/json")
		 ("Authorization" . ,(format "Bearer %s"
					     (genai-openrouter-key))))
      :error (cl-function
	      (lambda (&key data &allow-other-keys)
		(genai-error-callback data)))
      :parser 'json-read
      :complete (lexical-let ((callback callback)
			      (error-callback error-callback))
		  (cl-function
		   (lambda (&key data &allow-other-keys)
		     (genai-openrouter--callback callback error-callback data)))))))

;;;###autoload
(add-to-list 'genai-models
	     (make-genai-model :name (format "Openrouter %s" genai-openrouter-model)
			       :request #'genai-openrouter-request))

(provide 'genai-openrouter)
