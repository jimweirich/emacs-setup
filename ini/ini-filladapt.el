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
