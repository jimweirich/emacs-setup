;;; *****************************************************************
;;; C++ Code Templates
;;; Date:    07/Jan/98
;;; Author:  Jim Weirich
;;; Purpose: This file provides several templates for C++ programming.
;;; *****************************************************************

(provide 'cc-templates)
(require 'jw-templates)
(setq tempo-interactive t)

;;; ==================================================================
;;; C++ Specific helper functions
;;; ==================================================================

;;; Query for base classes to the current class.

(defun jw-cc-determine-ancestors ()
  "Return the ancestor string for a class."
  (interactive)
  (let ((ancestors ())
	(ans nil)
	(outstr ""))
    (while (progn (setq ans (read-string "Ancestor (cr if done): "))
		  (not (string-equal ans "")))
      (setq ancestors (cons ans ancestors)))
    (if ancestors
	(progn
	  (setq outstr (concat outstr " :"))
	  (while ancestors
	    (setq outstr (concat outstr " public " (car ancestors)))
	    (if (cdr ancestors)
		(setq outstr (concat outstr ",")))
	    (setq ancestors (cdr ancestors)) )))
    outstr) )


;;; Return a list of arg descriptors.  Each descriptor looks like
;;; (type name declaration description).

(defun jw-cc-get-args ()
  "Get the arguments for a function."
  (interactive)
  (let ((ptype "")			;arg type ("in"/"out"/"both")
	(pname "")			;arg name (string)
	(pdesc "")			;arg description (string)
	(pdecl "")			;arg declaration
	(plist nil)			;list of arguments
	(count 0)			;
        (uselongfmt t))			;TRUE if long format is to be used
    (while (progn
	     (setq count (+ count 1))
	     (setq pdecl
		   (read-string (concat "Declaration of Arg #" count " (cr if none): ")))
	     (not (string-equal pdecl "")))
      (if (string-match "\\([A-Za-z0-9_]+\\)\\( *\\[\\]\\)?$" pdecl)
	  (setq pname (match-string 1 pdecl))
	(setq pname (read-string "Parameter Name: ")))
      (if uselongfmt
	  (progn
	    (setq pdesc (read-string (concat "Short Description of Arg #"
					     count ": ")))
	    (if (string-equal pdesc "")
		(setq uselongfmt nil))
	    (if uselongfmt
		(setq ptype (jw-read-keyword
			     (concat "Type of '" pname "' (in, out, or both)? ")
			     '(("in")("out")("both")) )) )
	    (if (string-equal ptype "")
		(setq ptype "in")) )
	(setq pdesc "")
	(setq ptype "in") )
	
      (setq plist (cons (list ptype pname pdecl pdesc) plist)) )
    (reverse plist) ) )

(defun jw-cc-arg-length (funargs)
  "Return the length (number of chars) of the longest argument name."
  (let ((maxlen 0))
    (while funargs
      (if (> (length (cadr (car funargs))) maxlen)
	  (setq maxlen (length (cadr (car funargs)))))
      (setq funargs (cdr funargs)))
    maxlen))

(defun jw-cc-arg-decl-length (funargs)
  "Return the length (number of chars) of the longest argument declaration."
  (let ((maxlen 0))
    (while funargs
      (if (> (length (jw-3rdarg (car funargs))) maxlen)
	  (setq maxlen (length (jw-3rdarg (car funargs)))))
      (setq funargs (cdr funargs)))
    maxlen))

(defun jw-cc-arg-comments (funargs)
  "Generate the argument comments for a function."
  (let ((outstr "")
	(fmt (concat "    // %-9s %-"
		     (+ 1 (jw-cc-arg-length funargs))
		     "s %s\n")))
    (while funargs
      (let ((argspec (car funargs)))
	(setq outstr (concat outstr
			     (format fmt
				     (concat "[" (car argspec) "]")
				     (jw-2ndarg argspec)
				     (jw-4tharg argspec)))))
      (setq funargs (cdr funargs)))
    outstr))

;;; Format the argument list in the long, multiline format.

(defun jw-cc-arg-list-long (funargs)
  (let ((outstr "(\n")
	(sep ",")
	(fmt (concat "        %-"
		     (+ 2 (jw-cc-arg-decl-length funargs))
		     "s    // [%s] %s\n")))
    (while funargs
      (let ((argspec (car funargs)))
	(if (cdr funargs) (setq sep ",") (setq sep " "))
	(setq outstr (concat outstr
			     (format fmt
				     (concat (jw-3rdarg argspec) sep)
				     (jw-1starg argspec)
				     (jw-4tharg argspec)))))
      (setq funargs (cdr funargs)) )
    (setq outstr (concat outstr "        )"))
    outstr) )


;;; Format the arguments in the "short" format: (int a, int b)

(defun jw-cc-arg-list-short (funargs)
  "Generate the argument list for a function."
  (let ((outstr "(")
	(prefix ""))
    (while funargs
      (let ((argspec (car funargs)))
	(setq outstr (concat outstr prefix (jw-3rdarg argspec)))
	(setq prefix ", ") )
      (setq funargs (cdr funargs)) )
    (concat outstr ")")))


;;; Format the argument list.

(defun jw-cc-arg-list (funargs)
  (if (and funargs
	   (not (string-equal (jw-3rdarg (car funargs)) "")))
      (jw-cc-arg-list-long funargs)
      (jw-cc-arg-list-short funargs) ))


;;; ==================================================================
;;; Insert Command Support
;;; ==================================================================

(setq jw-cc-insert-list ())

(defun jw-cc-new-insert (name func)
  (setq jw-cc-insert-list (cons (list name func) jw-cc-insert-list)))

(defun jw-cc-insert-template () 
  (interactive)
  (let* ((tname (jw-read-keyword "Insert: " jw-cc-insert-list))
	 (funcpair (assoc tname jw-cc-insert-list)))
    (if funcpair (eval (cdr funcpair)))))


;;; ==================================================================
;;; Templates
;;; ==================================================================

(jw-cc-new-insert "header" 'jw-cc-new-header)
(defun jw-cc-new-header ()
  (interactive)
  (c++-mode)
  (tempo-template-jw-cc-header))
(tempo-define-template "jw-cc-header"
'(
  "// -*-Mode: c++; -*-" n
  "// File: " (buffer-name) n
  "// Copyright " (current-year-string) " by " (jw-get-author-name) n
  n
  "#ifndef " (jw-get-guard) n
  "#define " (jw-get-guard) n
  n
  "// Dependencies" (cmt-insert-bar-heavy) n
  n
  p n
  "#endif // " (jw-get-guard) n
))


(jw-cc-new-insert "class" 'tempo-template-jw-cc-class)
(tempo-define-template "jw-cc-class"
'(
  "// Class " (p "Class Name: " cname) (cmt-insert-bar-heavy) n
  "// Purpose: " p "TBD" n
  "//" n
  "// Description: " n
  "//" n
  "// " p "TBD" n
  n
  "class " p (s cname) 
  (jw-determine-ancestors)
  n

  "{" n
  "public:" n
  "    " (s cname) "();" (indent-for-comment) " Default CTOR" n
  "    virtual ~" (s cname) "();" (indent-for-comment) " DTOR" n
  n
  "    // Accessors " (cmt-insert-bar-light) n
  n
  p n
  n
  "    // Modifiers " (cmt-insert-bar-light) n
  n
  p n
  n
  "private:" n
  "    // Private Data " (cmt-insert-bar-light) n
  p n
  n
  "    // Disabled copy semantics" (cmt-insert-bar-light) n
  "    " (s cname) " (const " (s cname) "& value);"
  "            // Copy CTOR" n
  "    " (s cname) "& operator= (const " (s cname) "& value);"
  " // Assignment Op" n
  "};" n
))


(jw-cc-new-insert "member" 'tempo-template-jw-cc-member)
(tempo-define-template "jw-cc-member"
'(
  (progn
    (setq jw-funname (read-string "Function Name: "))
    (setq jw-funcmt  (read-string "Short Comment: "))
    (if (not (string-match "\\.$" jw-funcmt))
	(setq jw-funcmt (concat jw-funcmt ".")))
    (setq jw-funconst 
	  (if (y-or-n-p "Does this member function modify the object? ")
	      ""
	    " const"))
    (setq jw-funvirt "")
    (setq jw-funpure "")
    (if (y-or-n-p "Is this member funtion virtual? ")
	(progn (setq jw-funvirt "virtual ")
	       (if (y-or-n-p "Is it Pure Virtual? ")
		   (setq jw-funpure " = 0"))))
	
    (setq jw-funargs (jw-get-args))
    (setq jw-funret  (read-string "Return Type: "))
    (if (string-equal jw-funret "")
	(setq jw-funret "void"))
    (setq jw-funret-value (not (string-equal jw-funret "void")))
    (if jw-funret-value
	(setq jw-funret-comment (read-string "Return Value Comment: ")))
    "")

  "    " p (eval 'jw-funvirt) (eval 'jw-funret) " "
  (eval 'jw-funname) " "
  (jw-arg-list jw-funargs)
  (eval 'jw-funconst)
  (eval 'jw-funpure) ";" n
  "    // " p (eval 'jw-funcmt) n
  n
  p (progn (message "Member Declaration Inserted.") "")
))

(jw-cc-new-insert "group" 'tempo-template-jw-cc-group)
(tempo-define-template "jw-cc-group"
'(
  (beginning-of-line) (open-line 1)
  "    // " p (p "Group Name: " gname) (cmt-insert-bar-light) n
  n
  p n
))


(jw-cc-new-insert "include-implementation" 'tempo-template-jw-cc-impl-include)
(tempo-define-template "jw-cc-impl-include"
'(
  "// Dependencies" (cmt-insert-bar-heavy) n
  n
  "#include \"" (jw-get-class-name) ".h\""
  n
))

(jw-cc-new-insert "class-implementation" 'tempo-template-jw-cc-impl-class)
(tempo-define-template "jw-cc-impl-class"
'(
  "// Class " (jw-get-class-name) " Implementation" (cmt-insert-bar-heavy) n
  n
  p (jw-get-class-name) "::" (jw-get-class-name) "()  { }" n
  (jw-get-class-name) "::~" (jw-get-class-name) "() { }" n
  n
))

(jw-cc-new-insert "implementation" 'tempo-template-jw-cc-implementation)
(tempo-define-template "jw-cc-implementation"
'(
  "// File: " (buffer-name) n
  "// Implementation for: " (file-name-sans-extension (buffer-name)) ".h" n
  "// Copyright " (current-year-string) " by " (jw-get-author) n
  n
  (tempo-template-jw-cc-impl-include)
  n
  (tempo-template-jw-cc-impl-class)
))

(jw-cc-new-insert "create" 'ccaid-create-implementation)
