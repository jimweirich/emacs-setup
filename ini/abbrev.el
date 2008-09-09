;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-abbrev
;;; Purpose: Setup for Abbreviation Mode
;;; ==================================================================

(defun ab ()
  "Toggle abbreviation mode."
  (interactive)
  (abbrev-mode))

;(setq-default abbrev-mode nil)

;;(cond ((file-exists-p "~/.emacs-abbrevs")
;;       (read-abbrev-file "~/.emacs-abbrevs") ))
      
;;(define-abbrev-table 'ruby-mode-abbrev-table '(
;;    ("bt" "belongs_to" nil 1)
;;    ("hm" "has_many" nil 1)
;;     ("ho" "has_one" nil 1)
;;     ))

