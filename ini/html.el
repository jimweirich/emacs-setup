;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-html
;;; Purpose: Setups for HTML Helper Modes
;;; ==================================================================

;;; HTML Mode Setups =================================================

(setq html-helper-do-write-file-hooks t) ; modify date on write
(setq html-helper-build-new-buffer t)	; initialize new buffer
(setq html-helper-address-string	; not really used in this setup
      (concat "<a href=\"http://onestepback.org\">Jim Weirich</a> / \n"
	      "<a href=\"mailto:jim@weirichhouse.org\">jim@weirichhouse.org</a>\n"))


;;; Fix the binding of "\C-c " in html mode --------------------------

(defun fix-html-helper-map-hooker ()
  (define-key html-helper-mode-map "\C-c " 'multi-shell)
  (define-key html-helper-mode-map "\C-cs"
    'tempo-template-html-nonbreaking-space))

(if (boundp 'html-helper-mode-map)
    (fix-cc-map-hookfunc)
  (add-hook 'html-helper-mode-hook 'fix-html-helper-map-hooker))


;;; This is the template for new html files --------------------------

(setq html-helper-new-buffer-template
      '(
	"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
	"  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
	"\n"
	"<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">\n"
	"  <head>\n"
	"    <title>" p "</title>\n"
	"  </head>\n"
	"\n"
	"  <body>\n"
	"    " p "\n"
	"  </body>\n"
	"</html>\n"
	))

;;; get-file-contenst ------------------------------------------------
;;; get-file-contents can be used in HTML templates to insert code
;;; located in files.

(defun get-file-contents (fn)
  "Return the contents of a file as a string."
  (let* ((ffn (expand-file-name fn))
	 (buf (generate-new-buffer "*file-contents*"))
	 (result ()))
    (if (file-readable-p ffn)
	(save-excursion
	  (set-buffer buf)
	  (insert-file ffn)
	  (setq result (buffer-string))))
    (kill-buffer buf)
    result))

;;; get-file-contents-default ----------------------------------------
;;; Same as above, but supplies a default string if the file is not
;;; found.  

(defun get-file-contents-default (fn default)
  "Return the contents of a file as a string or default if file not found."
  (cond ((get-file-contents fn))
	(t default)))

