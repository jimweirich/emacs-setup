;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    load-ini.el
;;; Purpose: Load the initialization files
;;; ==================================================================

;;; Debug Messags ====================================================

(defun msg (msg-text)
  "Write a message to the scratch buffer"
  (interactive "sMessage: ")
  (save-excursion
    (set-buffer (get-buffer-create "*msg*"))
    (goto-char (point-max))
    (insert-string msg-text)
    (insert-string "\n") ))

;;; Setup the load path ==============================================
(defun add-to-load-path (fn)
  "Add expanded file name to load path.
Trailing slashes are stripped and duplicate names are not added."
  (msg fn)
  (let ((ffn (expand-file-name fn)))
    (if (eq (substring ffn -1) "/")
	(setq ffn (substring ffn 0 -1)))
    (if (not (member ffn load-path))
	(setq load-path (cons ffn load-path)))))

(defun add-to-info-path (fn)
  "Add expanded file name to load path.
Trailing slashes are stripped and duplicate names are not added."
  (msg fn)
  (require 'info)
  (let ((ffn (expand-file-name fn)))
    (if (eq (substring ffn -1) "/")
	(setq ffn (substring ffn 0 -1)))
    (if (not (member ffn Info-directory-list))
	(setq Info-directory-list (cons ffn Info-directory-list)))))

(defun lib-is-available (lib-name)
  "Return the containing directory if the library name can be found in the load-path."
  (let ((paths load-path)
	(found nil))
    (while (and paths (not found))
      (if (or (file-readable-p (concat (car paths) "/" lib-name))
	      (file-readable-p (concat (car paths) "/" lib-name ".el"))
	      (file-readable-p (concat (car paths) "/" lib-name ".elc")))
	  (setq found (car paths)))
      (setq paths (cdr paths)))
    found))

;;; Loading Function =================================================

(defvar ini-directory (concat elisp-directory "/ini"))

(defvar ini-loaded ()
  "List of files loaded during initialization.")

(defvar ini-not-loaded ()
  "List of files that failed to load during initialization.")

(defun ini-try-load (inifn ext)
  "Attempt to load an ini-type elisp file."
  (let ((fn (concat ini-directory "/" inifn ext)))
    (if (file-readable-p fn)
	(progn
	  (message (concat "Loading " inifn))
	  (load-file fn)
	  (setq ini-loaded (cons inifn ini-loaded)) ))))

(defun ini-load (inifn)
  "Load a ini-type elisp file"
  (cond ((ini-try-load inifn ".elc"))
	((ini-try-load inifn ".el"))
	(t (setq ini-not-loaded (cons inifn ini-not-loaded))
	   (message (concat inifn " not found")))))

(require 'font-lock)

(if (file-readable-p "/usr/local/share/emacs/site-lisp")
    (add-to-load-path "/usr/local/share/emacs/site-lisp") )

;;; Now load all the ini-xxx files in the initialization directory

(let ((ini-directory (concat elisp-directory "/ini"))
      (files (directory-files ini-directory nil "^.*\\.el$")))
  (while (not (null files))
    (ini-load (substring (car files) 0 -3))
    (setq files (cdr files)) ))

