;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-mail
;;; Purpose: Mail and RMail Setups
;;; ==================================================================

;;; Mail Mode Customizations =========================================

(setq mail-archive-file-name "~/Mail/outgoing")
(if (at-home) (setq mail-default-reply-to "jim@weirichhouse.org"))
(setq mail-yank-ignored-headers
      "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:\\|^cc:")

(defun jw-mail-signature (arg)
  "Flexible mail signature inserter.
No Prefix   = full signature at end.
num Prefix  = full signature at point.
^u prefix   = small signature at end.
^u^u prefix = small signaure at point."
  (interactive "P")
  (cond ((null arg) (mail-signature nil))
	((atom arg) (mail-signature arg))
	(t (save-excursion
	     (let ((atpoint (equal (car arg) 16)))
	       (or atpoint 
		   (goto-char (point-max)))
	       (skip-chars-backward " \t\n")
	       (end-of-line)
	       (or atpoint
		   (delete-region (point) (point-max)))
	       (insert "\n\n-- \n")
	       (insert-file-contents (expand-file-name "~/.sig")))) )))
  
(defun jw-tag-line (tag name email)
  "Insert a Super-Cite Style tag line."
  (interactive "stag: \nsname: \nsE-Mail: ")
  (insert ">>>>> \"")
  (insert tag)
  (insert "\" == ")
  (insert name)
  (insert " <")
  (insert email)
  (insert "> writes:\n\n"))


(defun jw-cite-filled (start end) 
  "Recite region with fill always enabled."
  (interactive "r")
  (let ((sc-auto-fill-region-p t))
    (sc-recite-region start end)) )


(defun jw-define-mail-keys (map)
  (if (is-xemacs) 
      (require 'supercite)
    (require 'sc))
  (define-key map "\C-cq" 'jw-cite-filled) 
  (define-key map "\C-c\C-w" 'jw-mail-signature)
  (define-key map "\C-c\C-t" 'jw-tag-line)
  (if (not (is-xemacs))
      (progn
	(define-key map "\C-n" 'mail-abbrev-next-line)
	(define-key map "\M->" 'mail-abbrev-end-of-buffer) ))
  )

(defun jw-mail-hooker () (jw-define-mail-keys mail-mode-map))
(add-hook 'mail-mode-hook 'jw-mail-hooker)
(if (boundp 'mail-abbrevs-setup)
    (add-hook 'mail-setup-hook 'mail-abbrevs-setup) )


;;; RMail Mode Fixin's ===============================================

;;; One of the most annoying things about RMAIL mode is the default
;;; replay action to to send a replay to all the CC addresses too.
;;; This can be embarrassing when writing a reply meant for only a
;;; single person.  Standard RMAIL mode allows the CC addresses to be
;;; omitted if the "r" command is given a prefix mode.  My version of
;;; this key binding reverses the default action to *not* reply to the
;;; CC addresses.

(defun jw-rmail-reply (all-cc)
  "Reply to the current message. (jnw's rmail-reply replacement)
Normally exclude CC: to all other recipients of original message;
prefix argument means include them.  While composing the reply,
use \\[mail-yank-original] to yank the original message into it."
  (interactive "P")
  (if (null all-cc) (rmail-reply '(4))
    (rmail-reply nil)))

;;; Provide the hook to install the r command

(defun jw-rmail-hook ()
  (define-key rmail-mode-map "r" 'jw-rmail-reply))
(add-hook 'rmail-mode-hook 'jw-rmail-hook)

;;; Make sure we don't send a reply to the jweirich address
;;; (if sending from home)

(setq rmail-dont-reply-to-names
      (if (at-home) "info-\\|jweirich" "info-"))
