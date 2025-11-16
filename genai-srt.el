;;; genai-srt.el --- SubRip Text translation tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: November 2025
;; Keywords: extensions AI
;; Package-Version: 1.0
;; Package-Requires: ((emacs "29.4") (genai"))

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

;; The package provides the `srt-translate-to` function, which allows users to
;; translate an SRT file from a source language to a destination language using
;; a GenAI model.

;; Key features and design decisions:

;; - Chunking: The code translates the SRT file in chunks, determined by
;;   `srt-translate-max-word`, to avoid exceeding API limits and to provide a
;;   better user experience.

;; - Sanity Checks: The `srt-translate--sanity-checks` function verifies that
;;   the frame numbers and timestamps are preserved during translation, ensuring
;;   the integrity of the translated SRT file.

;; - Markers: Uses buffer-local markers (`srt-marker`, `genai-srt-prev-marker`)
;;   to keep track of the current position in the SRT file and to handle
;;   potential rollbacks in case of translation errors.

;; - Customization: Introduces `srt-translate-prompt-fmt` to allow users to
;;   customize the prompt used for translation.

;;; Code:

(require 'genai)

(defcustom srt-translate-prompt-fmt
  "Please translate the following movie subtitles into %s. Preserve\
 the original formatting and timestamps. Return only the\
 translated subtitles."
  "Format string for the translation prompt.  The \"%s\" will be
replaced with the target language.")

(defcustom srt-translate-max-word 2000
  "Maximum number of words to translate in a single SRT chunk.
This value determines the maximum size of text segments sent to
the translation service.  Larger values may improve translation
quality but can also increase latency and potentially exceed API
limits leading to incorrect data.  Smaller values reduce latency
but may degrade translation quality."
  :type 'integer)

(defvar-local srt-marker nil
  "Marker indicating the current position in the SRT file.
This variable is buffer-local and used to keep track of the
current position while parsing or processing an SRT (SubRip Text)
file.")

(defvar-local genai-srt-prev-marker nil
  "Marker indicating the previous subtitle region.
Used to rollback in case of incorrect translation detection.")

(defun srt-translate--sanity-checks (src translation)
  "Perform sanity checks on the translated SRT content.

This function compares the frame numbers and timestamps in the
source SRT file with those in the translated SRT content to
ensure consistency. It returns `t' if no errors are detected, and
`nil' otherwise.  If discrepancies are found, an error message is
displayed in the minibuffer.

It iterates through each SRT entry in the translated content and
verifies that a corresponding entry with the same frame number and
timestamps exists in the source SRT file.  Any mismatch is flagged
as an error.

SRC is the buffer containing the original SRT content.
TRANSLATION is the string containing the translated SRT content."
  (with-current-buffer src
    (save-excursion
      (goto-char srt-prev-marker)
      (let* ((num-fmt "[0-9]+")
	     (ts-fmt "[0-9]+:[0-9]+:[0-9]+,[0-9]+")
	     (frame-fmt (format "^%s\n%s --> %s" num-fmt ts-fmt ts-fmt))
	     (tmp-pos 0)
	     err)
	(while (and (not err) (re-search-forward frame-fmt srt-marker t))
	  (let ((current (match-string 0)))
	    (with-temp-buffer
	      (insert translation)
	      (goto-char tmp-pos)
	      (if (re-search-forward frame-fmt nil t)
		  (unless (string= current (match-string 0))
 		    (setf err (format "'%s' is not '%s'."
				      current (match-string 0))))
		(setf err (format "'%s' is missing." current)))
	      (setf tmp-pos (point)))))
	(when err
	  (message "Invalid translation detected, %s." err))
	(not err)))))

(defun srt-translate--insert-translation (src dst language translation)
  "Insert the TRANSLATION into DST buffer.

If sanity checks pass, inserts the TRANSLATION into the DST
buffer after the point indicated by `srt-marker'.

If sanity checks fail, resets `srt-marker' in the SRC buffer to
its previous position (`srt-prev-marker`).

Finally, calls `srt-translate--to' to continue the translation
process."
  (if (srt-translate--sanity-checks src translation)
      (with-current-buffer dst
	(save-excursion
	  (goto-char srt-marker)
	  (insert "\n" translation)
	  (set-marker srt-marker (point))))
    (with-current-buffer src
      (set-marker srt-marker srt-prev-marker)))
  (srt-translate--to src dst language))

(defun srt-translate--to (src dst language)
  "Translate an SRT buffer from its current position to the next
segment.

This function translates a portion of the SRT buffer SRC, from
the current position (marked by `srt-marker') up to
`srt-translate-max-word' words limit. It uses `genai-request' to
perform the translation, utilizing the `srt-translate-prompt-fmt'
to format the prompt with the target LANGUAGE.  It also respects
the stopping the translation if the next segment is too far away."
  (with-current-buffer src
    (save-excursion
      (goto-char srt-marker)
      (let ((from (number-at-point))
	    (start (point))
	    (end (point))
	    (looking t))
	(while (and looking
		    (re-search-forward "^[0-9]+\n[0-9]+:[0-9]+:[0-9]+,[0-9]+ " nil t))
	  (if (< (count-words start (match-beginning 0)) srt-translate-max-word)
	      (setf end (match-beginning 0))
	    (setf looking nil)))
	(if (= end start)
	    (message "Done translating")
	  (goto-char end)
	  (set-marker srt-prev-marker srt-marker)
	  (set-marker srt-marker end)
	  (message "Translating from %d till %d\n" from (number-at-point))
	  (genai-request (format srt-translate-prompt-fmt language)
			 (buffer-substring start end)
			 (apply-partially #'srt-translate--insert-translation
					  src dst language)))))))

(defun srt-translate-to (src dst language)
  "Translate an SRT file from source to destination language.

SRC is the path to the original SRT file.  DST is the path to the
destination SRT file where the translated content will be
written.  LANGUAGE is the target language for the
translation (e.g., \"English\", \"French\", \"Spanish\").

This function reads the content of the source SRT file,
translates each subtitle block to the specified destination
language, and writes the translated content to the destination
SRT file.

It handles file opening and buffer management for both source and
destination files, ensuring that the destination file is cleared
before writing the translated content."
  (interactive (list (read-file-name "Original SRT file: ")
		     (read-file-name "Destination file: ")
		     (read-string "Destination language: " "English")))
  (let ((dst-buffer (find-file-noselect dst))
	(src-buffer (find-file-noselect src)))
    (with-current-buffer dst-buffer
      (erase-buffer)
      (setq srt-marker (point-max-marker)))
    (with-current-buffer src-buffer
      (setq srt-marker (point-min-marker)
	    srt-prev-marker (point-min-marker))
      (srt-translate--to (current-buffer) dst-buffer language))))

(provide 'genai-srt)
