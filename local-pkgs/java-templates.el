;;; *****************************************************************
;;; C++ Code Templates
;;; Date:    07/Jan/98
;;; Author:  Jim Weirich
;;; Purpose: This file provides several templates for C++ programming.
;;; *****************************************************************

(provide 'java-templates)
(require 'jw-templates)
(setq tempo-interactive t)

;;; Needed templates: member function, ctor, 

;;; ==================================================================
;;; Java Specific helper functions
;;; ==================================================================

;;; ==================================================================
;;; Insert Command Support
;;; ==================================================================

(setq jw-java-insert-list ())

(defun jw-java-new-insert (name func)
  (setq jw-java-insert-list (cons (list name func) jw-java-insert-list)))

(defun jw-java-insert-template () 
  (interactive)
  (let* ((tname (jw-read-keyword "Insert: " jw-java-insert-list))
	 (funcpair (assoc tname jw-java-insert-list)))
    (if funcpair (eval (cdr funcpair)))))


;;; ==================================================================
;;; Templates
;;; ==================================================================

(setq jw-copyright-list
 '(
   "// Copyright " (current-year-string) " by " (jw-get-author-name)
   " (" (jw-get-author-email) ")." n
   "// All rights reserved." n n
   "// Permission is granted for use, copying, modification, distribution," n
   "// and distribution of modified versions of this work as long as the" n
   "// above copyright notice is included." n
   ))

(setq jw-filename-list
'(
  "// File: " (buffer-name) n
  ) )

(jw-java-new-insert "filename" 'jw-java-new-filename)
(defun jw-java-new-filename ()
  (interactive)
  (tempo-template-jw-java-filename))
(tempo-define-template "jw-java-filename"
  jw-filename-list )

(jw-java-new-insert "class" 'tempo-template-jw-java-class)
(tempo-define-template "jw-java-class"
'(
  "/**" n
  " * <dl>" n
  " * <dt><b>Class:</b><dd>       " (p "Class: " cname) n
  " * <dt><b>Purpose:</b><dd>     " (p "Purpose: " purp) n
  " * <dt><b>Description:</b><dd>" n
  " *" n
  " * " p "TBD" n
  " *" n
  " * </dl>" n
  " *" n
  " * @author " (jw-get-author-name) n
  " * @version " (p "Version: " version) n
  " */" n
  n
  "public class " (s cname) n
  "{" n
  p n
  "}" n
  ))



(jw-java-new-insert "interface" 'tempo-template-jw-java-interface)
(tempo-define-template "jw-java-interface"
'(
  "/**" n
  " * <dl>" n
  " * <dt><b>Interface:</b><dd>   " (p "Interface: " cname) n
  " * <dt><b>Purpose:</b><dd>     " (p "Purpose: " purp) n
  " * <dt><b>Description:</b><dd>" n
  " *" n
  " * " p "TBD" n
  " *" n
  " * </dl>" n
  " *" n
  " * @author " (jw-get-author-name) n
  " * @version " (p "Version: " version) n
  " */" n
  n
  "public interface " (s cname) n
  "{" n
  p n
  "}" n
))


(jw-java-new-insert "qclass" 'tempo-template-jw-java-quick-class)
(tempo-define-template "jw-java-quick-class"
`(
  "// " (p "Class: " cname) " -- " (p "Purpose: " purp) n
  n
  ,@jw-copyright-list
  n
  "// " p "TBD" n
  n
  "public class " (s cname) n
  "{" n
  p n
  "}" n
  ))


(jw-java-new-insert "qinterface" 'tempo-template-jw-java-quick-interface)
(tempo-define-template "jw-java-quick-interface"
`(
  "// " (p "Class: " cname) " -- " (p "Purpose: " purp) n
  n
  ,@jw-copyright-list
  n
  "// " p "TBD" n
  n
  "public interface " (s cname) n
  "{" n
  p n
  "}" n
))

(jw-java-new-insert "main" 'tempo-template-jw-java-main)
(tempo-define-template "jw-java-main"
'(
  "    public static void main (String args[]) {" n
  "        " p n
  "    }" n
))

(jw-java-new-insert "copyright" 'tempo-template-jw-java-copyright)
(tempo-define-template "jw-java-copyright"
  jw-copyright-list)

(jw-java-new-insert "setup" 'tempo-template-jw-java-setup)
(tempo-define-template "jw-java-setup"
'(
  "    protected void setUp () {" n
  "    }" n
  n
  ))

(jw-java-new-insert "teardown" 'tempo-template-jw-java-teardown)
(tempo-define-template "jw-java-teardown"
'(
  "    protected void tearDown () {" n
  "    }" n
  n
  ))

(jw-java-new-insert "testcase" 'tempo-template-jw-java-testcase)
(tempo-define-template "jw-java-testcase"
'(
  "// Test" (p "Test Class Name: Test" tcname) " -- Test Case for" (s tcname) n
  n
  "import junit.framework.TestCase;" n
  "import junit.framework.TestSuite;" n
  "import junit.framework.Test;" n
  n
  "public class Test" (s tcname) n
  "    extends TestCase" n
  "{" n
  "    // JUnit Framework -----------------------------------------------" n
  n
  "    public Test" (s tcname) " (String name) { super (name); }" n
  n
  "    public static Test suite () {" n
  "        TestSuite suite = new TestSuite (Test" (s tcname) ".class);" n
  "        return suite;" n
  "    }" n
  n
  "    // Tests ---------------------------------------------------------" n
  n
  "    public void testCreation () {" n
  "        " p "assert (\"created\", obj != null);" n 
  "    }" n
  "}" n
))

(defun jw-reindent-comment-block ()
  (save-excursion
    (search-backward "/**")
    (let ((begin (point)))
      (search-forward "*/")
      (indent-region begin (point) nil) )))

(jw-java-new-insert "doc-comment" 'jw-insert-java-doc)
(defun jw-insert-java-doc ()
  (tempo-template-jw-java-doc)
  (jw-reindent-comment-block) )
  
(tempo-define-template "jw-java-doc"
'(
  (beginning-of-line)
  (open-line 1)
  " /**" n
  "  * " p n
  "  */" 
))

(jw-java-new-insert "group" 'tempo-template-jw-java-group)
(tempo-define-template "jw-java-group"
'(
  "    // " p (p "Group Name: " gname) (cmt-insert-bar-light) n
))


(jw-java-new-insert "implementation-group" 'tempo-template-jw-java-implementation-group)
(tempo-define-template "jw-java-implementation-group"
'(
  "    // == Implementation " (cmt-insert-bar-heavy) n
))


;; JavaDoc Tags ...

(jw-java-new-insert "tag-author" 'tempo-template-jw-java-tag-author)
(tempo-define-template "jw-java-tag-author"
'(
  "     * @author " (jw-get-author-name) n
  ))

(jw-java-new-insert "tag-version" 'tempo-template-jw-java-tag-version)
(tempo-define-template "jw-java-tag-version"
'(
  "     * @version " (p "Version: ") n
  ))

(jw-java-new-insert "tag-param" 'tempo-template-jw-java-tag-param)
(tempo-define-template "jw-java-tag-param"
'(
  "     * @param " (p "Param Name: ") " " (p "Param Description: ") n
  ))

(jw-java-new-insert "tag-return" 'tempo-template-jw-java-tag-return)
(tempo-define-template "jw-java-tag-return"
'(
  "     * @return " (p "Return Description: ") n
  ))

(jw-java-new-insert "tag-exception" 'tempo-template-jw-java-tag-exception)
(tempo-define-template "jw-java-tag-exception"
'(
  "     * @exception " (p "Exception Name: ")
  " if " (p "Thrown if ... ") n
  ))

(jw-java-new-insert "tag-see" 'tempo-template-jw-java-tag-see)
(tempo-define-template "jw-java-tag-see"
'(
  "     * @see " (p "See Class: ") n
  ))

(jw-java-new-insert "tag-since" 'tempo-template-jw-java-tag-since)
(tempo-define-template "jw-java-tag-since"
'(
  "     * @since " (p "Since: ") n
  ))

(jw-java-new-insert "tag-deprecated" 'tempo-template-jw-java-tag-deprecated)
(tempo-define-template "jw-java-tag-deprecated"
'(
  "     * @deprecated " (p "Deprecated: ") n
  ))

