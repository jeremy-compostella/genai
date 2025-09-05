;;; genai-github.el --- Github Copilot GenAI support. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: September 2025
;; Keywords: extensions AI
;; Homepage: https://github.com/jeremy-compostella/genai
;; Package-Version: 1.0
;; Package-Requires: ((emacs "29.4") (request "0.3.3")
;;                    (copilot-chat "20241125.852"))

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

;; This module provides backend support for the genai module to interact with
;; GitHub Copilot. It allows users to send prompts to the GitHub Copilot API and
;; receive responses directly within Emacs.

;; The module relies on the `copilot-chat' Emacs module which must be properly
;; set up.

;;; Code:

(require 'genai)
(require 'copilot-chat)
(require 'copilot-chat-copilot)

(defvar genai-github-instance nil
  "The GitHub Copilot instance to use.")

(defun genai-github-callback (temp-buffer callback instance token)
  (if (string= token "#cc#done#!$")
      (let ((response (with-current-buffer temp-buffer
			(buffer-string))))
	(kill-buffer temp-buffer)
	(funcall callback response))
    (with-current-buffer temp-buffer
      (goto-char (point-max))
      (insert token))))

(defun genai-github-request (system-prompt user-prompt callback
					   error-callback)
  (unless genai-github-instance
    (copilot-chat--auth)
    (setq genai-github-instance (copilot-chat--create default-directory)))
  (let ((temp-buffer (get-buffer-create (make-temp-name "genai-github"))))
    (copilot-chat--request-ask genai-github-instance
			       (concat system-prompt user-prompt)
			       (apply-partially #'genai-github-callback
						temp-buffer callback)
			       t)))

;;;###autoload
(add-to-list 'genai-models
	     (make-genai-model :name "Github Copilot"
			       :request #'genai-github-request))

(provide 'genai-github)
