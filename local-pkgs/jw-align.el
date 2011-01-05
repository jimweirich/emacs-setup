;;; jw-align.el --- Align on operator

;; Copyright (C) 2010 by Jim Weirich

;; Author: Jim Weirich <jim.weirich@gmail.com>
;; Version 1.0
;; Keywords: coding, files, extensions, convenience
;; Created:  6/Sep/10

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Purpose: To align assignments, hash literals, etc.
;;
;; Usage:
;;
;; (1) Highlight the region to be aligned
;; (2) Run jw-align
;; (3) Enter the alignment operator (default is "=")

(require 'cl)

(defun jw-next-line-point ()
  (save-excursion
    (move-beginning-of-line 1)
    (forward-line)
    (point)))

(defun jw-find-operator (op)
  (let ((col (search-forward op (jw-next-line-point) t)))
    (if col
        (progn (goto-char (- (point) (length op))) (current-column))
      (move-beginning-of-line 1)
      nil)))

(defun jw-extra-white-p ()
  "Is there extra white space beforethe current point?"
  (and (> (point) (+ 1 (point-min)))
       (string-equal "  " (buffer-substring (- (point) 2) (point)))))

(defun jw-anywhite-p ()
  "Is there any white space before the current point?"
  (and (> (point) (point-min))
       (string-equal " " (buffer-substring (- (point) 1) (point)))))

(defun jw-backup-over-white ()
  (interactive)
  (while (jw-extra-white-p)
    (backward-char)))

(defun jw-find-op-position (op)
  "Determine the alignment column for operator op on the current line."
  (if (jw-find-operator op)
      (progn
        (jw-backup-over-white)
        (if (jw-anywhite-p)
            (current-column)
          (+ 1 (current-column))))
    nil))

(defun jw-determine-alignment-column (op)
  "Determine the alignment column for operator op in the current buffer."
  (let ((result 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((col (jw-find-op-position op)))
          (if (and col (> col result))
              (setq result col)))
        (forward-line)))
    result))

(defun jw-align-line (op to-col)
  "Align op to column to-col on the current line."
  (if (jw-find-operator op)
      (progn
        (just-one-space)
        (while (< (current-column) to-col)
          (insert-string " ")))))

(defun jw-align-lines-in-buffer (op to-col)
  "Align all lines in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (jw-align-line op to-col)
      (forward-line))))

(defun jw-align (op start end)
  "Align the operator op in the selected region."
  (interactive "sOperator (=): \nr")
  (if (string-equal "" op) (setq op "="))
  (narrow-to-region start end)
  (let ((to-col (jw-determine-alignment-column op)))
    (if to-col (jw-align-lines-in-buffer op to-col)))
  (widen))

(provide 'jw-align)
