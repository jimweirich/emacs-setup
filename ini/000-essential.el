;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-essential.el
;;; Purpose: Essential Emacs Functions and bindings
;;; ==================================================================

;;; Detect emacs version =============================================

(defun is-emacs-19 ()
  (string-equal (substring emacs-version 0 2) "19"))

(defun is-emacs-19-25 ()
  (string-equal (substring emacs-version 0 5) "19.25"))

(defun is-emacs-20 ()
  (string-equal (substring emacs-version 0 2) "20"))

(defun is-emacs-21 ()
  (string-equal (substring emacs-version 0 2) "21"))

(defun is-emacs-22 ()
  (string-equal (substring emacs-version 0 2) "22"))

(defun is-emacs-23 ()
  (string-equal (substring emacs-version 0 2) "23"))

(defun is-xemacs ()
  (string-match "XEmacs" emacs-version))

(defun is-aquamacs ()
  (boundp 'aquamacs-version))

;;; Location Detection ===============================================
;;; This code is used to detect where emacs is running.  The location
;;; test functions allow customization of the setup file.

(setq jw-site
      (cond ((file-readable-p "/vmlinuz") 'home)
	    ((file-readable-p "/boot/vmlinuz") 'home)
	    (t 'unknown)))

(defun at-home () (eq jw-site 'home))
(defun at-who-knows-where () (eq jw-site 'unknown))

(if (and (or (is-emacs-19) (is-emacs-20)) (not (is-xemacs)))
    (transient-mark-mode t))

;;; Define autolist ==================================================

(defun make-auto (pattern mode)
  "Add a pattern to the auto-mode alist."
  (let ((ans (assoc pattern auto-mode-alist)))
    (if (and ans (equal mode (cdr ans)))
	(print "Do Nothing")
      (print "Added")
      (setq auto-mode-alist
	    (cons (cons pattern mode) auto-mode-alist)))))

;;; Backspace Rebinding ==============================================

;; The following swaps the functions of the delete and backspace keys

(defun fix-backspace ()
  (interactive)
    (let ((the-table (make-string 128 0)))
      (let ((i 0))
	(while (< i 128)
	  (aset the-table i i)
	  (setq i (1+ i))))
      ;; Swap ^H and DEL
      (aset the-table ?\177 ?\^h)
      (aset the-table ?\^h ?\177)
      (setq keyboard-translate-table the-table)) )

(if (and (not (is-emacs-19))
	 (not (is-xemacs))
	 (not (is-emacs-21))
	 (not (is-emacs-22)))
    (fix-backspace))

(defun toggle-bs-mode ()
  (interactive)
  (if keyboard-translate-table
      (progn (setq keyboard-translate-table nil)
	    (message "Normal Key Translation"))
    (fix-backspace)
    (message "C-h / DEL Swapped") ))

;;; Customized Variables ===============================================

(setq rlogin-initially-track-cwd t)	; track dirs in rlogin
(setq next-line-add-newlines nil)	; C-n will not add lines
(setq require-final-newline t)		; require files end with newline
(setq auto-save-default nil)		; don't auto-save (it annoys me)

(setq Info-default-directory-list
      (cons "~/.elisp/info" Info-default-directory-list))

(defvar compile-command "rake ")	; set the default make command
(make-variable-buffer-local 'compile-command)
					; make the compile command buffer local
					; (this allows each buffer to have its
					;  own custom compile command)

(put 'narrow-to-region 'disabled nil)	; narrow enabled
(put 'upcase-region 'disabled nil)	; change case enabled
(put 'eval-expression 'disabled nil)	; allow eval commands

(setq shell-dirstack-query "resync_dirs")

(require 'compile)
(setq bad-re "\\([^ :]+\\):\\([0-9]+\\):in")
