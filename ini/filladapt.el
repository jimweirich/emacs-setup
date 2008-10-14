;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-filladapt
;;; Purpose: Setups for Fill Adapt Mode
;;; ==================================================================

;;; You will need filladapt installed.  It comes with xemacs.  For
;;; regular emacs, make sure the emacs-goodies-el debian package is
;;; installed.

(defun fam ()
  (interactive)
  (filladapt-mode))

;;(setq-default filladapt-mode t)

;; Use the following as apropriate.
;; (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
;; (add-hook 'c-mode-hook 'turn-off-filladapt-mode)
