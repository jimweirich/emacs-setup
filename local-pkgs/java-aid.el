;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    java-aid
;;; Purpose: Additional Aids for Java Programming
;;; ==================================================================

;;; This is a set of helps (aids) for Java programming.  It is
;;; designed to work with the java-mode.el package that comes with
;;; emacs.  This package includes some templates for file comments and
;;; class declarations, and some (very simple) automatic code
;;; generation.


(require 'tempo)			; require the template expansion pkg

;;; jaid-fname-re -----------------------------------------------------
;;; Regular expression used to pick the function name from a line
;;; containing the function declaration.  This RE will handle operator
;;; function names (e.g. operator=).

(setq jaid-fname-re "^.*[: 	]\\(operator[ 	]*[][+*/!%^&|~,()<>=-]*\\|~?\\w+\\)[ 	]*(")


;;; jaid-add-mode-comment ---------------------------------------------
;;; Insert the emacs mode comment at the beginning of a file.

(defun jaid-add-mode-comment ()
  (interactive)
  (goto-char (point-min))
  (insert "// -*-Mode: c++; -*-\n\n")
  (c++-mode) )
   

;;; jaid-add-sep ------------------------------------------------------
;;; Add a function name separator.

(defun jaid-add-sep (name)
  "Insert '// name ------' comment."
  (beginning-of-line)
  (open-line 1)
  (insert "// ")
  (insert name)
  (cmt-insert-bar-light)
  (insert "\n"))


;;; jaid-add-method-separator -----------------------------------------
;;; Insert separator comment infront of the method at the point.

(defun jaid-add-method-separator ()
  "Add a function header comment before the following function."
  (interactive)
  (let ((startpos (point)))
    (if (not (re-search-forward jaid-fname-re))
	(error "Start of Function Not Found"))
    (let ((name (buffer-substring (match-beginning 1) (match-end 1))))
      (if (string-match "^\\(if\\|for\\|while\\|do\\|switch\\)$" name)
	  (progn (goto-char startpos)
		 (error "Can't find start of function")) )
      (jaid-add-sep name) ) ) )


;;; jaid-find-class-name ----------------------------------------------
;;; Determine the name of the class declaration surrounding the point.

(defun jaid-find-class-name ()
  "Return the name of the class declaration surrounding the point."
  (save-excursion
    (if (not (re-search-backward "^class[ 	]+\\(\\w+\\)" nil t))
	(error "Class Name Not Found")
      (buffer-substring (match-beginning 1) (match-end 1)))))


;;; jaid-create-implementation ----------------------------------------
;;; Create a implementation body for the method declaration at point.
;;; The implementation is placed in the corresponding implementation
;;; file.  (e.g. methods declared in XXX.h are put in XXX.cc)

(defun jaid-create-implementation ()
  "Create a implementation of the member function at point."
  (interactive)
  (beginning-of-line)
  (let* ((cname (jaid-find-class-name))
	 (start (point))
	 (end (save-excursion (search-forward ";") (point)))
	 (decl (buffer-substring start end)))
    (find-mate)
    (goto-char (point-max))
    (save-excursion
      (insert decl)
      (insert "\n"))
    (jaid-add-method-separator)
    (re-search-forward jaid-fname-re)
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
    

;;; jaid-add-header-protection ----------------------------------------
;;; Add the ifdef/endif protection around a header file.

(defun jaid-add-header-protection ()
  (interactive)
  (let ((hname (upcase (concat (substring (buffer-name) 0 -2) "_H"))))
    (goto-char (point-min))
    (while (and (looking-at "//") (< (point) (point-max)))
      (next-line 1))
    (insert "\n#ifndef ")
    (insert hname)
    (insert "\n#define ")
    (insert hname)
    (insert "\n\n")
    (save-excursion (goto-char (point-max))
		    (insert "\n#endif\n") )))


;;; new-java-class --------------------------------------------------------
;;; Template for creating new C++ classes.

(tempo-define-template
 "new-jaid-class"
 '("// class " (p "Class Name: " cname) n
   "// " (cmt-insert-bar-heavy) n n
   "class " (s cname) " {\n"
   "// CTOR\n" (cmt-insert-bar-light)
   (s cname) "() { }" > (indent-for-comment) "default CTOR" n n
   "}" n)
 "class"
 "Insert a new class declaration")


;;; jaid-new-class ----------------------------------------------------
;;; Wrapper code for using the new class template.

(defun jaid-new-class ()
  (interactive)
  (let ((tempo-interactive t))
    (tempo-template-new-jaid-class)
    (re-search-backward "// =====")
    (end-of-line)
    (insert "\n// ") ))


;;; tempo-template-file-comments --------------------------------------
;;; Insert a set of file comments at the point.

(tempo-define-template
 "jaid-file-comments"
 '((open-line 1) 
   "// " (cmt-insert-bar-heavy) n
   "// File Name:     " (buffer-name) n
   "// Revision:      $Revision: 1.1 $" n
   "// Class:         " p n
   "// Purpose:       " p n
   "// Author:        " (user-full-name) n
   "// Date:          " (dt) n
   "// " (cmt-insert-bar-light) n
   )
 "documentation"
 "Insert File Level Comments")


;;; ==================================================================
;;; Key Bindings
;;; ==================================================================

(global-set-key [f5] 'tempo-forward-mark)
(global-set-key [f6] 'tempo-backward-mark)
(global-set-key [f7] 'tempo-complete-tag)

(define-key java-mode-map "\C-cnc" 'jaid-new-class)
(define-key java-mode-map "\C-cnd" 'tempo-template-jaid-file-comments)
(define-key java-mode-map "\C-cnh" 'jaid-add-header-protection)
(define-key java-mode-map "\C-cnm" 'jaid-add-mode-comment)
(define-key java-mode-map "\C-cnv" 'jaid-add-method-separator)
(define-key java-mode-map "\C-cnx" 'jaid-create-implementation)

(global-set-key "\C-cnm" 'jaid-add-mode-comment)


