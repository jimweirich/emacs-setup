;;; ==================================================================
;;; Author: Jim Weirich
;;; File:   ini-oo-browser.el
;;; Purpose: Initialize the OO Browser
;;; ==================================================================

(setq oobr-path
      (cond ((at-home) "/usr/local/lib/oo-browser/")
	    (t "/no-path/")))

(if (jw-check-file oobr-path)
    (progn (add-to-load-path oobr-path)
	   (add-to-load-path (concat oobr-path "hypb/"))
	   (load "br-start")
	   (global-set-key "\C-c\C-o" 'oo-browser)
	   ))
