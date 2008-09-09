;;; *****************************************************************
;;; Tempo Template Helper Functions
;;; Date:    07/Jan/98
;;; Author:  Jim Weirich
;;; Purpose: Provide extenstions to the tempo template facility.
;;; *****************************************************************

(provide 'jw-templates)
(require 'tempo)
(setq tempo-interactive t)

;;; ==================================================================
;;; Help Functions
;;; ==================================================================


(defun jw-1starg (lst) (car lst))
(defun jw-2ndarg (lst) (car (cdr lst)))
(defun jw-3rdarg (lst) (car (cdr (cdr lst))))
(defun jw-4tharg (lst) (car (cdr (cdr (cdr lst)))))

;;; Read a keyword from the keyboard using an a-list of names.

(defun jw-read-keyword (prompt keylist)
  "Read a single keywords."
  (completing-read prompt keylist () t))


;;; Read a list of keywords from an alist.  Return all selected
;;; keywords in a single blank delimited string;

(defun jw-read-keywords (prompt keylist)
  "Read a list of keywords.
The keywords are returned in a blank delimited string."
  (let ((keywords "")
	(kw "") )
    (while (progn
	     (setq kw (completing-read prompt keylist () t))
	     (not (equal "" kw)))
      (setq keywords (concat keywords kw " ")) )
    keywords) )


;;; Update the list of Math keywords (used only in the Math group)

(defun jw-update-keywords ()
  (interactive)
  (message "Updating Math Keyword List ...")
  (call-process "/grp/gc/gcmstr/EMACSOOBROWSER/updatekeywords")
  (message "Rereading Math Keyword List ...")
  (load-library "math-keywords")
  (message "DONE") )


;;; Replace all spaces in str with underscores.

(defun jw-no-spaces (str)
  "Replace spaces with _."
  (let ((i 0)
	(newstr (substring str 0)) )
    (while (< i (length newstr))
      (if (eq (aref newstr i) ? )
	  (aset newstr i ?_))
      (setq i (+ i 1)) )
    newstr ))


;;; Get the name of the author.  Return the name as entered.  Sets
;;; jw-author-name with name and jw-author to same with spaces
;;; replaced.

(defun jw-get-author-name ()
  "Return the Author name."
  (if (not (boundp 'jw-author-name))
      (progn
	(setq jw-author-name (read-string "Author (cr if none): "))
	(setq jw-author (jw-no-spaces jw-author-name)) ))
  jw-author-name)


;;; Get the name of the author.  Return the name as entered.  Sets
;;; jw-author-name with name and jw-author to same with spaces
;;; replaced.

(defun jw-get-author-email ()
  "Return the Author's EMail address."
  (if (not (boundp 'jw-author-email))
      (progn
	(setq jw-author-email (read-string "EMail (cr if none): ")) ))
  jw-author-email)


;;; Ask the user for the class name (if no already set).

(defun jw-ask-class-name ()
  "Return the class name."
  (if (not (boundp 'jw-class-name))
      (progn
	(setq jw-class-name (read-string "Class: "))
  jw-class-name) ))


;;; Get the class name from the current file name.
;;; (e.g.  "Object.h" => "Object")

(defun jw-get-class-name ()
  "Return the class name based on the file name."
  (file-name-sans-extension (buffer-name)) )

;;; Return the ifdef symbol guard based on the file name.

(defun jw-get-guard ()
  "Return the ifdef symbol guard based on the file name."
  (upcase (concat (jw-get-class-name) "_H")) )
  
;;; Same as above, but returns the name with the spaces reversed. 

(defun jw-get-author ()
  "Return the Author name with spaces replaced by '_'."
  (jw-get-author-name)
  jw-author)


;;; Query for base classes to the current class.

(defun jw-determine-ancestors ()
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

(defun jw-get-args ()
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

(defun jw-arg-length (funargs)
  "Return the length (number of chars) of the longest argument name."
  (let ((maxlen 0))
    (while funargs
      (if (> (length (cadr (car funargs))) maxlen)
	  (setq maxlen (length (cadr (car funargs)))))
      (setq funargs (cdr funargs)))
    maxlen))

(defun jw-arg-decl-length (funargs)
  "Return the length (number of chars) of the longest argument declaration."
  (let ((maxlen 0))
    (while funargs
      (if (> (length (jw-3rdarg (car funargs))) maxlen)
	  (setq maxlen (length (jw-3rdarg (car funargs)))))
      (setq funargs (cdr funargs)))
    maxlen))

(defun jw-arg-comments (funargs)
  "Generate the argument comments for a function."
  (let ((outstr "")
	(fmt (concat "    // %-9s %-"
		     (+ 1 (jw-arg-length funargs))
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

(defun jw-arg-list-long (funargs)
  (let ((outstr "(\n")
	(sep ",")
	(fmt (concat "        %-"
		     (+ 2 (jw-arg-decl-length funargs))
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

(defun jw-arg-list-short (funargs)
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

(defun jw-arg-list (funargs)
  (if (and funargs
	   (not (string-equal (jw-3rdarg (car funargs)) "")))
      (jw-arg-list-long funargs)
      (jw-arg-list-short funargs) ))

;;; ==================================================================
;;; Tempo Interface Functions
;;; ==================================================================

(defun jw-get-named (name)
  "Retrieve an insertion saved under a named specified in NAME."
  (let* ((insertion (cdr (assq name tempo-named-insertions))))
    (if insertion
	insertion
      (error "Named insertion not found"))))


;;; ==================================================================
;;; Infostructure Functions
;;; ==================================================================

(defun jw-packagize-name (name)
  (concat "jw-" *jw-template-package-name* "-" template-name))

(defun jw-tempoize-name (name)
  (concat "tempo-template-" (jw-packagize-name template-name)))

(defun make-insert-func (package-name)
   (list
    'defun
    (intern (concat "jw-" package-name "-insert-template"))
    ()
    '(interactive)
    (list 'jw-generic-insert-template (intern (concat "*jw-" package-name "-insert-list*")))))

(defun jw-generic-insert-template (insert-list) 
  (let* ((tname (jw-read-keyword "Insert: " insert-list))
	 (funcpair (assoc tname insert-list)))
    (if funcpair (eval (cdr funcpair)))))


(defun jw-template (template-name template-body)
  (let* ((tempo-name (jw-tempoize-name template-name))
         (list-name (intern (concat "*jw-" *jw-template-package-name* "-insert-list*"))) )
    (set list-name
         (cons
          (list template-name (intern (jw-tempoize-name template-name)))
          (eval (intern (concat "*jw-" *jw-template-package-name* "-insert-list*"))) ))
    (tempo-define-template
     (jw-packagize-name template-name)
     template-body)))

(defun jw-template-package (package-name)
  (setq *jw-template-package-name* package-name)
  (let* ((mode-map (eval (intern(concat *jw-template-package-name* "-mode-map"))))
         (insert-func-name (intern (concat "jw-" package-name "-insert-template"))) )
    (define-key mode-map "\C-ci" insert-func-name)
    (set (intern (concat "*jw-" package-name "-insert-list*")) ())
    (eval (make-insert-func package-name)) ))

;;; ==================================================================
;;; Menus and Hooks
;;; ==================================================================

(global-set-key [f8] 'tempo-forward-mark)
(global-set-key [f7] 'tempo-backward-mark)
