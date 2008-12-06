;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-rails
;;; Purpose: Setups for the ido package
;;; ==================================================================

(require 'ido)
(load-file "x-ido.el")
(ido-mode t)

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable_recursive-minibuffers t))
      (visit-tags-table-buffer))
    (ido-completing-read "Project files: "
                         (tags-table-files)
                         nil t)))

(defun ffp () (interactive) (ido-find-file-in-tag-files))
(global-set-key "\C-c\C-f" 'ido-find-file-in-tag-files)
