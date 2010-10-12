;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-rails
;;; Purpose: Setups for the ido package
;;; ==================================================================

(require 'ido)
(ido-mode t)

(defun jw-select (func items)
  (let ((result ()))
    (while (not (null items))
      (if (funcall func (car items))
          (setq result (cons (car items) result)))
      (setq items (cdr items)))
    (reverse result)))

(defun jw-interesting-file-name-p (file-name)
  (not (string-match "^\([/.]|vendor\)" file-name)))

(defun jw-tag-files ()
  (save-excursion
    (set-buffer (get-buffer "TAGS"))
    (jw-select #'jw-interesting-file-name-p (tags-table-files))))

(defun jw-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (jw-tag-files) nil t)))))

(defun ffp () (interactive) (ido-find-file-in-tag-files))
