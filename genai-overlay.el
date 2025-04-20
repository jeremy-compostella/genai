;;; genai-overlay.el --- Comparison overlay -*- lexical-binding: t; -*-

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

;;; Commentary:

;; This module provides a sophisticated system for comparing and
;; visualizing differences between two regions of text within a
;; buffer. This tool utilizes overlays to highlight changes, making it
;; easier for users to identify deletions, additions, and
;; modifications. With customizasble keybindings and interactive
;; functions, users can efficiently navigate through overlays, toggle
;; their visibility, and make decisions about retaining or rejecting
;; changes. This module is designed to enhance text editing workflows by
;; offering a dynamic and intuitive interface for text comparison.

;;; Code:

(require 'cl-macs)
(require 'cl-seq)
(require 'pulse)
(require 'transient)

(defcustom genai-hide-before-string-ratio .5
  "Ratio of the original text to hide when displaying a change.")

(defcustom genai-overlay-entry-key "C-c l"
  "Keyboard shortcut for overlay entry."
  :type 'string)

(defvar genai--overlay-region-A '()
  "Stores the current region markers as region 'A'.
This variable is used by `genai-overlay-compare-regions' to
facilitate text comparison between two marked regions in the
buffer.")

(defun genai--overlays (&optional beg end)
  "Return a list of change overlays."
  (let ((overlays (cond ((use-region-p)
			 (overlays-in (region-beginning) (region-end)))
			((and beg end)
			 (overlays-in beg end))
			((overlays-in  (max (1- (point)) (point-min))
				       (min (1+ (point)) (point-max)))))))
    (cl-delete-if-not (lambda (x) (memq x '(delete-only replacement)))
		      overlays :key (lambda (ov)
				      (overlay-get ov 'category)))))

(defun genai-overlay-keep ()
  "Discard the original text in the current region or at the point."
  (interactive)
  (dolist (ov (genai--overlays))
    (let ((undo-inhibit-record-point t))
      (put-text-property (overlay-start ov) (overlay-end ov)
			 'invisible nil)
      (delete-overlay ov))))

(defun genai-overlay-reject ()
  "Restore the original text in the current region or at the point."
  (interactive)
  (dolist (ov (genai--overlays))
    (let ((original-text (overlay-get ov 'before-string))
	  (delete-onlyp (eq (overlay-get ov 'category) 'delete-only)))
      (goto-char (overlay-start ov))
      (delete-region (overlay-start ov) (overlay-end ov))
      (when delete-onlyp
	(delete-overlay ov))
      (insert original-text))))

(defun genai--overlay-set-transient-map (&optional N)
  "Set up a transient keymap for navigating overlays.

This function creates a temporary keymap for interacting with overlays,
providing a set of keybindings for navigation and actions on
overlays. The user is informed about the available keys via a message in
the echo area.  Optional argument N represents the number of overlays to
move when navigating forward or backward, defaulting to 1 if not
specified."
  (let ((move N)
 	(N (or N 1)))
    (message (format "Press %s for the next overlay, %s for the\
 previous overlay, %s to keep the change, %s to reject the change."
		     (propertize "n"'face 'help-key-binding)
		     (propertize "p"'face 'help-key-binding)
		     (propertize "k"'face 'help-key-binding)
		     (propertize "r"'face 'help-key-binding)))
    ;; Create a transient keymap for navigation
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (lexical-let ((move move))
 	 (dolist (key '("n" "p" "r" "k" "c"))
	   (define-key map (kbd key)
		       (lambda ()
			 (interactive)
 			 (pcase key
			   ("n" (genai-forward-overlay))
 			   ("p" (genai-forward-overlay -1))
 			   ("r" (genai-overlay-reject)
			    (genai-forward-overlay N))
 			   ("k" (genai-overlay-keep)
			    (genai-forward-overlay N))
 			   ("c" (genai-overlay-cycle)
			    (when move
			      (genai-forward-overlay 0))))))))
       map))))

(defun genai-overlay-cycle ()
  "Toggle the visibility of overlays in the current region or at point.
This function alternates the invisibility of overlay text between three
states:

1. Fully visible: Both the original and new text are shown.
2. Original invisible: Only the new text is visible.
3. New text invisible: Only the original text is visible.

The function cycles through these states for each overlay within the
region or at the point, and sets a transient keymap for easy
navigation."
  (interactive)
  (save-excursion
    (let (before-invisible after-invisible set)
      (dolist (ov (genai--overlays))
	(let ((old (overlay-get ov 'before-string)))
	  (unless (or set (string-empty-p old)
		      (= (overlay-start ov) (overlay-end ov)))
	    (setf before-invisible (get-text-property 0 'invisible old))
	    (setf after-invisible (get-text-property (overlay-start ov)
						     'invisible))
	    (cond ((and (not before-invisible) (not after-invisible))
		   (setf before-invisible nil
			 after-invisible t))
		  ((and (not before-invisible) after-invisible)
		   (setf before-invisible t
			 after-invisible nil))
		  ((setf before-invisible nil
			 after-invisible nil)))
 	    (setf set t))))
      (dolist (ov (genai--overlays))
	(let ((old (overlay-get ov 'before-string)))
	  (with-silent-modifications
 	    (when set
	      (let ((undo-inhibit-record-point t))
		(put-text-property (overlay-start ov) (overlay-end ov)
				   'invisible after-invisible))
	      (when old
		(put-text-property 0 (length old)
				   'invisible before-invisible old))))))
      (when set (genai--overlay-set-transient-map)))))

(defun genai-forward-overlay (&optional N)
  "Navigate between comparative overlays in the buffer.
Moves the point forward or backward over overlays based on N. Highlights
the current overlay using `pulse-momentary-highlight-region' if
available. N is the number of overlays to move: positive to move
forward, negative to move backward."
  (interactive)
  (let* ((N (or N 1))
	 (ov (car (genai--overlays)))
         (start (cond ((and (> N 0) ov) (+ 1 (overlay-end ov)))
                      ((< N 0) (point-min))
                      (t (point))))
         (end (cond ((and (< N 0) ov) (1- (overlay-start ov)))
                    ((> N 0) (point-max))
                    (t (point))))
         (overlays (genai--overlays start end)))
    (when (< N 0)
      (setf overlays (nreverse overlays)))
    (let ((current (or (car overlays) ov)))
      (if (not current)
	  (message "No overlay found.")
	;; Inform the user about navigation keys
	(genai--overlay-set-transient-map N)
        (goto-char (overlay-start current))
        (when (pulse-available-p)
          (pulse-momentary-highlight-region (overlay-start current)
                                            (overlay-end current)))))))

(defun genai--overlay-set-before-string (ov str invisible)
  (put-text-property 0 (length str) 'face 'diff-refine-removed str)
  (put-text-property 0 (length str) 'invisible invisible str)
  (overlay-put ov 'before-string str))

(defun genai--overlay-create-deletion (beg2 m4)
  (with-current-buffer (marker-buffer beg2)
    (let ((ov (make-overlay (+ (marker-position beg2) m4)
			    (+ (marker-position beg2) m4)
			    nil 'front-advance nil)))
      (overlay-put ov 'category 'delete-only)
      (put-text-property (overlay-start ov) (overlay-end ov)
			 'invisible nil)
      ov)))

(defun genai--overlay-create-change (beg2 m4 m5)
  (with-current-buffer (marker-buffer beg2)
    (let ((ov (make-overlay (1- (+ (marker-position beg2) m4))
			    (+ (marker-position beg2) (or m5 m4))
			    nil 'front-advance nil)))
      (overlay-put ov 'evaporate t)
      (put-text-property (overlay-start ov) (overlay-end ov)
			 'invisible nil)
      (overlay-put ov 'category 'replacement)
      (overlay-put ov 'face 'diff-refine-added)
      ov)))

(defun genai--overlay-compare-regions (beg1 end1 beg2 end2)
  "Compare two regions and emphasize differences using overlays.
This function is inspired by the `smerge-refine-regions'
function."
  (unless (markerp beg1)
    (setq beg1 (copy-marker beg1)))
  (unless (markerp beg2)
    (setq beg2 (copy-marker beg2)))
  (let ((pos (point))
        (words (with-current-buffer (marker-buffer beg1)
		 (count-words beg1 end1)))
        (file1 (make-temp-file "diff1"))
        (file2 (make-temp-file "diff2"))
        deactivate-mark)
    (let ((write-region-inhibit-fsync t)
	  (backup (and (boundp 'smerge--refine-long-words)
		       smerge--refine-long-words)))
      (setq smerge--refine-long-words (make-hash-table :test #'equal))
      (smerge--refine-chopup-region beg1 end1 file1 nil)
      (smerge--refine-chopup-region beg2 end2 file2 nil)
      (setq smerge--refine-long-words backup))
    (unwind-protect
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-emacs))
            (call-process diff-command nil t nil "-ad" file1 file2))
          (goto-char (point-min))
	  (let* ((changes (float (count-matches "^[0-9]")))
		 (before-string-invisible
		  (> (/ changes words)
		     genai-hide-before-string-ratio)))
            (while (not (eobp))
              (if (not (looking-at "\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?\\([acd]\\)\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?$"))
                  (error "Unexpected patch hunk header: %s"
			 (buffer-substring (point)
					   (line-end-position))))
	      (cl-flet ((match-string-number (n)
			  (when-let ((str (match-string n)))
			    (string-to-number str))))
		(let* ((op (char-after (match-beginning 3)))
                       (m1 (match-string-number 1))
                       (m2 (match-string-number 2))
                       (m4 (match-string-number 4))
                       (m5 (match-string-number 5))
		       (ov (cond ((eq op ?d)
				  (genai--overlay-create-deletion
				   beg2 m4))
				 ((memq op '(?c ?a))
				  (genai--overlay-create-change
				   beg2 m4 m5)))))
		  (let ((str (when (memq op '(?c ?d))
			       (with-current-buffer (marker-buffer beg1)
				 (buffer-substring-no-properties
				  (1- (+ (marker-position beg1) m1))
				  (+ (marker-position beg1)
				     (or m2 m1)))))))
		    (genai--overlay-set-before-string
		     ov (or str "") before-string-invisible))))
              (forward-line 1)
              (and (re-search-forward "^[0-9]" nil 'move)
                   (goto-char (match-beginning 0))))))
      (goto-char pos)
      (delete-file file1)
      (delete-file file2))))

(defun genai-overlay-compare-regions (region)
  "Compute and highlight differences between two text regions.

This function uses overlays to compare and highlight differences between
text regions 'A' and 'B' in the current buffer.

REGION specifies whether to store the current region as 'A' or compare
with stored region 'A' using 'B'.

Usage:
- Select a region in the buffer.
- Call the function interactively and choose 'A' to store the
  region or 'B' to compute differences.

Raises an error if no region is selected or if region 'B' is
chosen without a previously stored region 'A'."
  (interactive (list (completing-read "Region A or B: " '("A" "B"))))
  (unless (use-region-p)
    (error "No region selected."))
  (when (and (string= region "B") (not genai--overlay-region-A))
    (error "No region A is stored."))
  (let ((beg (copy-marker (region-beginning)))
        (end (copy-marker (region-end))))
    (if (string= region "A")
        (progn
          (deactivate-mark)
          (setq genai--overlay-region-A (cons beg end))
          (pulse-momentary-highlight-region beg end))
      (let* ((orig-beg (car genai--overlay-region-A))
             (orig-end (cdr genai--overlay-region-A)))
        (genai--overlay-compare-regions orig-beg orig-end beg end)
        (pulse-momentary-highlight-region beg end)
        (goto-char beg)))))

(transient-define-prefix genai-overlay-entry ()
  "Defines a transient menu for comparison overlays."
  ["Navigation:"
   ("n" "Go to next overlay" genai-forward-overlay)
   ("p" "Go to previous overlay"
    (lambda () (interactive) (genai-forward-overlay -1)))
   ("c" "Cycle overlay visibility" genai-overlay-cycle)]
  ["Action:"
   ("a" "Add overlay" genai-overlay-compare-regions)
   ("k" "Keep change" genai-overlay-keep)
   ("r" "Reject change" genai-overlay-reject)]
  ["New comparison:"
   ("A" "Store region A for comparison"
    (lambda () (interactive) (genai-overlay-compare-regions "A")))
   ("B" "Compare current region with region A"
    (lambda () (interactive) (genai-overlay-compare-regions "B")))]
  (interactive)
  (transient-setup 'genai-overlay-entry))

(provide 'genai-overlay)
