;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-gnus
;;; Purpose: Setups for (ding) GNUs
;;; ==================================================================

;;; Set defaults -----------------------------------------------------

(if (is-xemacs)
    (load "mime-setup"))

(setq gnus-list-identifiers '("\\[ruby-talk:[0-9]+\\]"))
(setq gnus-use-long-file-name t)
(setq gnus-read-active-file t)
(setq gnus-summary-line-format "%U%R%5i%z%I%(%[%4L: %-20,20n%]%) %s
")
(setq gnus-carpal nil)
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-subject gnus-thread-sort-by-total-score))

;;; Make sure ^C-Q is bound in news replies --------------------------

(defun jw-define-gnus-keys (map)
  (if (is-xemacs)
      (require 'supercite)
    (require 'sc) )
  (define-key map "\C-cq" 'jw-cite-filled) )

(defun jw-gnus-hooker ()
  (cond ((boundp 'message-mode-map)  (jw-define-gnus-keys message-mode-map))
	((boundp 'news-reply-mode-map)  (jw-define-gnus-keys news-reply-mode-map)) ))
	
(add-hook 'message-mode-hook 'jw-gnus-hooker)

;;; In case the hook must be run manually ...

(defun jwg () (interactive) (jw-gnus-hooker))

;;; Redefine that pesky 'r' key --------------------------------------

(defun jnw-summary-reply (n)
  (interactive "P")
  (if (y-or-n-p "Reply via E-Mail? ")
      (gnus-summary-reply n)
    (gnus-summary-followup n)))

(defun jnw-fixup-gnus () 
  (define-key gnus-summary-mode-map "r" 'jnw-summary-reply))

(add-hook 'gnus-summary-mode-hook 'jnw-fixup-gnus)
