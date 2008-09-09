;;; *****************************************************************
;;; Eiffel Code Templates
;;; Date:     3/Dec/98
;;; Author:  Jim Weirich
;;; Purpose: This file provides several templates for Eiffel programming.
;;; *****************************************************************

(provide 'eiffel-templates)
(require 'jw-templates.el)
(setq tempo-interactive t)

;;; ==================================================================
;;; Eiffel Specific helper functions
;;; ==================================================================

;;; Query for base classes to the current class.

(defun jw-eiffel-determine-ancestors ()
  "Return the ancestor string for a class."
  (interactive)
  (let ((ancestors ())
	(ans nil)
	(outstr ""))
    (while (progn (setq ans (read-string "Ancestor (cr if done): "))
		  (not (string-equal ans "")))
      (setq ancestors (cons (upcase ans) ancestors)))
    (if ancestors
	(progn
	  (setq ancestors (reverse ancestors))
	  (setq outstr "inherit\n")
	  (while ancestors
	    (setq outstr (concat outstr "   " (car ancestors) "\n"))
	    (setq ancestors (cdr ancestors)) )
	  (setq outstr (concat outstr "\n")) ))
    outstr) )


(defun jw-eiffel-determine-creation ()
  "Return a Creation class/feature section for creation."
  (interactive)
  (let ((create-function (read-string "Creation Function: ")))
    (if (string-equal create-function "")
	""
      (concat "creation\n"
	      "    " create-function "\n"
	      "\n"
	      "feature {NONE} -- Creation\n"
	      "\n"
	      "    " create-function " is \n"
	      "            -- Creation function.\n"
	      "        do\n"
	      "        end\n"
	      "\n") ))
  )

;;; ==================================================================
;;; Insert Command Support
;;; ==================================================================

(setq jw-eiffel-insert-list ())

(defun jw-eiffel-new-insert (name func)
  (setq jw-eiffel-insert-list (cons (list name func) jw-eiffel-insert-list)))

(defun jw-eiffel-insert-template () 
  (interactive)
  (let* ((tname (jw-read-keyword "Insert: " jw-eiffel-insert-list))
	 (funcpair (assoc tname jw-eiffel-insert-list)))
    (if funcpair (eval (cdr funcpair)))))


;;; ==================================================================
;;; Templates
;;; ==================================================================

(jw-eiffel-new-insert "class" 'tempo-template-jw-eiffel-class)
(tempo-define-template "jw-eiffel-class"
'(
  (progn (makunbound 'jw-class-name)
	 (jw-ask-class-name) "")
  "indexing" n
  "    description: \"" (p "Description: " description) "\"" n
  "    author:      \"" (jw-get-author-name)    "\"" n
  "    email:       \"" (jw-get-author-email)   "\"" n 
  "    copyright:   \"Copyright " (current-year-string)
  " by " (jw-get-author-name) "\"" n
  "    license:     \"Eiffel Forum Freeware License (see file 'forum.txt')\"" n
  "    version:     \"$Revision: 1.2 $\"" n
  "    date:        \"$Date: 2000/08/18 11:40:05 $\"" n
  n
  "class" n
  "    " (upcase jw-class-name) n
  n
  (jw-eiffel-determine-ancestors)
  (jw-eiffel-determine-creation)
  "feature -- Queries" n
  n
  "feature -- Commands" n
  n
  "feature {NONE} -- Implemenation" n
  n
  "end -- " (upcase jw-class-name) n))

(jw-eiffel-new-insert "make" 'tempo-template-jw-eiffel-make)
(tempo-define-template "jw-eiffel-make"
'(
  "    make" p " is" n
  "            -- Make a " (eval 'jw-class-name) n
  "        require" n
  "            " p n
  "        do" n
  "            " p n
  "        ensure" n
  "            " p n
  "        end" n
  n)
)
(jw-eiffel-new-insert "ancestor-clause" 'tempo-template-jw-eiffel-ancestor-clause)
(tempo-define-template "jw-eiffel-ancestor-clause"
'(
  "        rename" n
  "            " p n
  "        export" n
  "            " p n
  "        redefine" n
  "            " p n
  "        undefine" n
  "            " p n
  "        select" n
  "            " p n
  "        end" n p)
)


(jw-eiffel-new-insert "function" 'tempo-template-jw-eiffel-function)
(tempo-define-template "jw-eiffel-function"
'(
  (progn
    (setq jw-fundecl (read-string "Function Decl: "))
    (setq jw-funcmt  (read-string "Short Comment: "))
    "")
  
  "    " (eval 'jw-fundecl) " is" n
  "                -- " (eval 'jw-funcmt) n
  "        require" n
  "            " p n
  "        do" n
  "            " p n
  "        ensure" n
  "            " p n
  "        end" n
  n p 
))

(jw-eiffel-new-insert "deferred-function" 'tempo-template-jw-eiffel-deferred-function)
(tempo-define-template "jw-eiffel-deferred-function"
'(
  (progn
    (setq jw-fundecl (read-string "Deferred Function Decl: "))
    (setq jw-funcmt  (read-string "Short Comment: "))
    "")

  "    " (eval 'jw-fundecl) " is" n
  "                -- " (eval 'jw-funcmt) n
  "        require" n
  "            " p n
  "        deferred" n
  "        ensure" n
  "            " p n
  "        end" n
  n p 
))

(jw-eiffel-new-insert "attribute" 'tempo-template-jw-eiffel-attribute)
(tempo-define-template "jw-eiffel-attribute"
'(
  (progn
    (setq jw-attrdecl (read-string "Attribute Decl: "))
    (setq jw-attrcmt  (read-string "Short Comment: "))
    "")

  "    " (eval 'jw-attrdecl) ";" n
  "            -- " (eval 'jw-attrcmt) n p
))

(jw-eiffel-new-insert "group" 'tempo-template-jw-eiffel-public-features)
(tempo-define-template "jw-eiffel-public-features"
'(
  "feature -- " (p "Group Name: ") > n
  )
)

(jw-eiffel-new-insert "implementation-group" 'tempo-template-jw-eiffel-private-features)
(tempo-define-template "jw-eiffel-private-features"
'(
  "feature {NONE} -- Implementation" > n
  )
)

(jw-eiffel-new-insert "loop" 'tempo-template-jw-eiffel-loop)
(tempo-define-template "jw-eiffel-loop"
'(
  "from" > n>
  p > n>
  "invariant" > n>
  p > n>
  "variant" > n>
  p > n>
  "until" > n>
  p > n>
  "loop" > n>
  p > n>
  "end" > n>
  p >
))

(jw-eiffel-new-insert "test" 'tempo-template-jw-eiffel-test)
(tempo-define-template "jw-eiffel-test"
'(
  (progn (makunbound 'jw-class-name)
	 (jw-ask-class-name) "")
  "class" n
  "    TEST_" (upcase jw-class-name) n
  n
  "inherit" n
  "    ETEST_FIXTURE" n
  "        redefine set_up, tear_down end" n
  n
  "feature -- Support" n
  n
  "    set_up is" n
  "        do" n
  "        end" n
  n
  "    tear_down is" n
  "        do" n
  "        end" n
  n
  "feature -- Tests" n
  n
  "    t_create is" n
  "        do" n
  "            assert (\"create\", obj /= Void)" n
  "        end" n
  n
  "end -- TEST_" (upcase jw-class-name) n
))

(jw-eiffel-new-insert "do-loop" 'tempo-template-jw-eiffel-do-loop)
(tempo-define-template "jw-eiffel-do-loop"
'(
  "from " p " until " p " loop" > n>
  p > n>
  "end" > n
))

(define-key eiffel-mode-map "\C-ci"  'jw-eiffel-insert-template)
