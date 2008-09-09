;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    cc-aid
;;; Purpose: Additional Aids for C/C++ Programming
;;; ==================================================================

;;; This is a set of helps (aids) for C and C++ programming.  It is
;;; designed to work with the cc-mode.el package that comes with
;;; emacs.  This package includes some templates for file comments and
;;; class declarations, and some (very simple) automatic code
;;; generation.

;;; The tempo package is used to provide boiler-plate template support
;;; for C++ coding. 


(require 'tempo)			; require the template expansion package

;;; ccaid-fname-re -----------------------------------------------------
;;; Regular expression used to pick the function name from a line
;;; containing the function declaration.  This RE will handle operator
;;; function names (e.g. operator=).

(setq ccaid-fname-re "^.*[: 	]\\(operator[ 	]*[][+*/!%^&|~,()<>=-]*\\|~?\\w+\\)[ 	]*(")


;;; ccaid-add-mode-comment ---------------------------------------------
;;; Insert the emacs mode comment at the beginning of a file.

(defun ccaid-add-mode-comment ()
  (interactive)
  (goto-char (point-min))
  (insert "// -*-Mode: c++; -*-\n\n")
  (c++-mode) )
   

;;; ccaid-add-sep ------------------------------------------------------
;;; Add a function name separator.

(defun ccaid-add-sep (name)
  "Insert '// name ------' comment."
  (beginning-of-line)
  (open-line 1)
  (insert "// ")
  (insert name)
  (cmt-insert-bar-light)
  (insert "\n"))


;;; ccaid-add-method-separator -----------------------------------------
;;; Insert separator comment infront of the method at the point.

(defun ccaid-add-method-separator ()
  "Add a function header comment before the following function."
  (interactive)
  (let ((startpos (point)))
    (if (not (re-search-forward ccaid-fname-re))
	(error "Start of Function Not Found"))
    (let ((name (buffer-substring (match-beginning 1) (match-end 1))))
      (if (string-match "^\\(if\\|for\\|while\\|do\\|switch\\)$" name)
	  (progn (goto-char startpos)
		 (error "Can't find start of function")) )
      (ccaid-add-sep name) ) ) )


;;; ccaid-find-class-name ----------------------------------------------
;;; Determine the name of the class declaration surrounding the point.

(defun ccaid-find-class-name ()
  "Return the name of the class declaration surrounding the point."
  (save-excursion
    (if (not (re-search-backward "^class[ 	]+\\(\\w+\\)" nil t))
	(error "Class Name Not Found")
      (buffer-substring (match-beginning 1) (match-end 1)))))


;;; ccaid-create-implementation ----------------------------------------
;;; Create a implementation body for the method declaration at point.
;;; The implementation is placed in the corresponding implementation
;;; file.  (e.g. methods declared in XXX.h are put in XXX.cc)

(defun ccaid-create-implementation ()
  "Create a implementation of the member function at point."
  (interactive)
  (beginning-of-line)
  (let* ((cname (ccaid-find-class-name))
	 (start (point))
	 (end (save-excursion (search-forward ";") (point)))
	 (decl (buffer-substring start end)))
    (find-mate)
    (goto-char (point-max))
    (save-excursion
      (insert decl)
      (insert "\n"))
    (ccaid-add-method-separator)
    (re-search-forward ccaid-fname-re)
    (goto-char (match-beginning 1))
    (insert (concat cname "::"))
    (beginning-of-line)
    (if (looking-at "[ 	]*\\(virtual\\|static\\)\\b")
	(kill-word 1))
    (while (< (point) (point-max))
      (c-indent-line)
      (next-line 1))
    (previous-line 1)
    (re-search-forward ";")
    (backward-delete-char 1)
    (insert "\n{\n\n}\n\n\n")
    (previous-line 4)
    ))
    


;;; ==================================================================
;;; Key Bindings
;;; ==================================================================

(global-set-key [f7] 'tempo-complete-tag)
