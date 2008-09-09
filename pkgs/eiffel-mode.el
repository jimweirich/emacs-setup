;;; eiffel-mode.el --- major mode for editing Eiffel files.

;; Copyright (C) 1989, 1990, 1993, 1994, 1995, 1996, 1999
;;                         Tower Technology Corporation,
;;                         Free Software Foundation,
;;                         Martin Schwenke

;; Authors: 1989-1990 Stephen Omohundro, ISE and Bob Weiner
;;          1993-1996 Tower Technology Corporation
;;          1999      Martin Schwenke <martin@meltin.net>
;; Version: $Id: eiffel-mode.el,v 1.1 2000/08/18 11:01:11 jim Exp $
;; Keywords: eiffel languages oop

;; This file is derived from eiffel4.el from Tower Technology Corporation.
;;
;; Martin Schwenke did these things:
;;
;; * Simplified the font-lock support to use standard GNU Emacs
;;   font-lock faces.
;; * Added the compilation support for GNU Eiffel.
;; * Added the menu.
;;
;; Known bugs:
;;
;; * eif-short buffer doesn't get font locked under Emacs 19.34.
;;
;; Planned improvements:
;;
;; * Fix doc-strings so that they obey the GNU Emacs doc-string standard.

;; This file is distributed under the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  EIFFEL3  : GNU Emacs mode for Eiffel Version 3
;;
;;  INSTALLATION
;;    To install, simply copy this file into a directory in your
;;    load-path and add the following two commands in your .emacs file:
;;
;;    (add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))
;;    (autoload 'eiffel-mode "eiffel-mode" "Major mode for Eiffel programs" t)
;;

;;; Code:

;; Indentation amount variables.
;;
;; The default values correspond to style used in ``Eiffel: The
;; Language''.  Note: for TowerEiffel users the values below will be
;; superceded by the values in either tcustom.el or ~/.tcustom.el if
;; it is present.

(defvar eif-indent-increment                   3 
  "Default indentation interval (in spaces)")

(defvar eif-class-level-kw-indent        0 
  "Indentation amount for Class level keywords (in number of
eif-indent-increments) (see eif-class-level-keywords variable).")
(defvar eif-extra-class-level-kw-indent        0 
  "Number of SPACES to add to eif-class-level-kw-indent to get the
actual indentation of a class level keyword. Can be negative.")

(defvar eif-class-level-comment-indent   0 
  "Indentation of comments at the beginning of the class (in number of
eif-indent-increments)")
(defvar eif-extra-class-level-comment-indent   0 
  "Number of SPACES to add to eif-class-level-comment-indent to get the
actual indentation of a class level comment. Can be negative.")

(defvar eif-inherit-level-kw-indent      2 
  "Indentation of keywords falling under the Inherit clause (in number of
eif-indent-increments) (see eif-inherit-level-keywords variable.")
(defvar eif-extra-inherit-level-kw-indent      0 
  "Number of SPACES to add to eif-inherit-level-kw-indent to get the
actual indentation of an inherit level keyword. Can be negative.")

(defvar eif-feature-level-indent         1 
  "Indentation amount of features. (in number of eif-indent-increments)")
(defvar eif-extra-feature-level-indent         0 
  "Number of SPACES to add to eif-feature-level-indent to get the
actual indentation of a feature. Can be negative.")

(defvar eif-feature-level-kw-indent      2 
  "Indentation of keywords belonging to individual features. (in number of
eif-indent-increments) (see eif-feature-level-keywords variable)")
(defvar eif-extra-feature-level-kw-indent      0 
  "Number of SPACES to add to eif-feature-level-kw-indent to get the
actual indentation of a feature level keyword. Can be negative.")

(defvar eif-feature-level-comment-indent 3 
  "Indentation of comments at the beginning of a feature. (in number of
eif-indent-increments)")
(defvar eif-extra-feature-level-comment-indent 0 
  "Number of SPACES to add to eif-feature-level-comment-indent to get the
actual indentation of a feature level comment. Can be negative.")

(defvar eif-body-comment-indent 0 
  "Indentation of comments in the body of a routine. (in number of
eif-indent-increments)")
(defvar eif-extra-body-comment-indent 0 
  "Number of SPACES to add to eif-body-comment-indent to get the
actual indentation of a routine body comment. Can be negative.")

(defvar eif-check-keyword-indent         0
  "Extra indentation for the check clause as described in ETL. (in number of
eif-indent-increments). Default is 0, which is different than in ETL's 1.")
(defvar eif-extra-check-keyword-indent         0
  "Number of SPACES to add to eif-check-keyword-indent to get the
actual indentation of a check keyword. Can be negative.")

(defvar eif-rescue-keyword-indent         -1
  "Extra indentation for the rescue clause as described in ETL. (in number of
eif-indent-increments). Default is -1.")
(defvar eif-extra-rescue-keyword-indent         0
  "Number of SPACES to add to eif-rescue-keyword-indent to get the
actual indentation of a rescue keyword. Can be negative.")

(defvar eif-then-indent                  0
  "Indentation for a `then' appearing on a line by itself rather 
than on the same line as an `if'. (in number of eif-indent-increments)")
(defvar eif-extra-then-indent                  1
  "Number of SPACES to add to eif-then-indent to get the
actual indentation of a `then' appearing on a line by itself. Can be 
negative.")

(defvar eif-continuation-indent                1
  "Extra indentation for a continued statement line. (in number of eif-indent-increments)")
(defvar eif-extra-continuation-indent          0
  "Number of SPACES to add to eif-continuation-indent to get the
actual indentation of a continued statement line. Can be 
negative.")

;;
;; Font-lock support.
;;
(defconst eiffel-font-lock-keywords
  '(;; major keywords
    ("\\(\\(^[ \t]*\\|[ \t]+\\)creation\\|^deferred[ \t]+class\\|^expanded[ \t]+class\\|^class\\|^feature\\|^indexing\\|\\(^[ \t]*\\|[ \t]+\\)inherit\\|^obsolete\\)[ \t\n]" 0 font-lock-keyword-face nil)
    ;; assertions
    ("\\(^\\|[^_\n]\\<\\)\\(check\\|ensure then\\|ensure\\|invariant\\|require else\\|require\\|variant\\)\\($\\|\\>[^_\n]\\)" 2 font-lock-keyword-face nil)
    ;; minor keywords
    ("\\(^\\|[^_\n]\\<\\)\\(alias\\|all\\|and not\\|and then\\|and\\|as\\|create\\|debug\\|deferred\\|do\\|else\\|elseif\\|end\\|export\\|external\\|from\\|frozen\\|if not\\|if\\|implies not\\|implies\\|infix\\|inspect\\|is deferred\\|is unique\\|is\\|like\\|local\\|loop\\|not\\|obsolete\\|old\\|once\\|or else\\|or not\\|or\\|prefix\\|redefine\\|rename\\|rescue\\|retry\\|select\\|strip\\|then\\|undefine\\|unique\\|until\\|when\\|xor\\)\\($\\|\\>[^_\n]\\)" 2 font-lock-keyword-face nil)
    ;; hidden comments
    ("--|.*" 0 font-lock-reference-face t)
    ;; quoted expr's in comments
    ("`[^`'\n]*'" 0 font-lock-string-face t)
    )
  "Regular expressions to use with font-lock mode.")

(defconst eiffel-font-lock-defaults
  '((eiffel-font-lock-keywords)
    nil nil nil nil))

(require 'font-lock)
;(add-to-list 'font-lock-defaults-alist
;	     (cons 'eiffel-mode
;		   eiffel-font-lock-defaults))

;;
;; Compilation support for GNU Eiffel.
;; 

(defvar eif-use-gnu-eiffel t
  "*Setup to use GNU Eiffel for compilation.")

(defvar eif-compile-command "compile"
  "*Program to use for compiling Eiffel programs.")

(defvar eif-short-command "short"
  "*Program to use for producing sshort form of Eiffel classes.")

(defvar eif-compile-options ""
  "*Options to use for compiling Eiffel programs.")

(defvar eif-compile-dir nil
  "Current directory where Eiffel compilations are taking place.
Possibly used for error location.")

(defvar eif-compile-target nil
  "Current Eiffel compilation target.")

(defvar eif-root-proc "make"
  "Current Eiffel root procedure.")

(defvar eif-run-command nil
  "Current command to run after Eiffel compile.")

(defun eif-compile ()
  "Compile an Eiffel root class."

  (interactive)

  (let ((temp last-nonmenu-event))
	 
    (setq eif-compile-dir (file-name-directory (buffer-file-name)))
    (setq eif-compile-target
	  (file-name-sans-extension
	   (read-string "Name of root class: "
			(or eif-compile-target
			    (file-name-sans-extension
			     (file-name-nondirectory (buffer-file-name)))))))
    (setq eif-root-proc
	  (read-string "Name of root procedure: "
		       eif-root-proc))
    (let* ((cmd (concat eif-compile-command
			" "    eif-compile-options
			" -o " eif-compile-target
			(if (eq system-type 'windows-nt) ".exe")
			" "    eif-compile-target
			" "    eif-root-proc))
	   (last-nonmenu-event temp)
	   (compilation-read-command nil))
      (compile cmd))))

(defun eif-set-compile-options ()
  "Set Eiffel compiler options."

  (interactive)
  (setq eif-compile-options
	(read-string "Eiffel compiler options: " eif-compile-options)))

;; Taken from Emacs 20.3 subr.el (just in case we're running under Emacs 19).
(defun eif-split-string (string &optional separators)
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun eif-run ()
  "Run a compiled Eiffel program."
  (interactive)

  (setq eif-run-command
	(read-string "Command to run: "
		     (or eif-run-command
			 eif-compile-target
			 (file-name-sans-extension
			   (file-name-nondirectory (buffer-file-name))))))

  (let* ((tmp-buf (current-buffer))
	 (words   (eif-split-string eif-run-command))
	 (cmd     (expand-file-name (car words))))

    (apply 'make-comint cmd cmd nil (cdr words))
    (switch-to-buffer tmp-buf)
    (switch-to-buffer-other-window (concat "*" cmd "*"))))

(require 'compile)
;; The description part of an error can be more than 1 line long.  We
;; make the gross assumption that it can't be spread over more than 3
;; lines.
(add-to-list 'compilation-error-regexp-alist
	     '("\\*+ \\(Fatal.Error\\|Warning\\).*\n?.*\n?.*\nLine \\([0-9]+\\) column \\([0-9]+\\) in [^ ]+ (\\([^)]+\\))" 4 2))

(defun eif-short ()
  "Display the short form of an Eiffel class."
  (interactive)

  (let* ((class	(read-string
		 "Class or file: "
		 (if (buffer-file-name)
		     (file-name-nondirectory (buffer-file-name)))))
	 (buf (get-buffer-create (concat "*Eiffel - short " class "*"))))

    (shell-command (concat eif-short-command " " class) buf)
    (save-excursion
      (set-buffer buf)
      (let ((font-lock-defaults eiffel-font-lock-defaults))
	(font-lock-fontify-buffer))
      (toggle-read-only 1))))

;;
;; No user-customizable definitions below this point.
;;

(defmacro eif-class-level-kw-indent-m () 
  "Indentation amount for Class level keywords (in number of spaces)."
  '(+ (* eif-class-level-kw-indent eif-indent-increment) 
     eif-extra-class-level-kw-indent)
)

(defmacro eif-class-level-comment-indent-m () 
  "Indentation amount for Class level comments (in number of spaces)."
  '(+ (* eif-class-level-comment-indent eif-indent-increment) 
     eif-extra-class-level-comment-indent)
)

(defmacro eif-inherit-level-kw-indent-m () 
  "Indentation amount for Inherit level keywords (in number of spaces)."
  '(+ (* eif-inherit-level-kw-indent eif-indent-increment) 
     eif-extra-inherit-level-kw-indent)
)

(defmacro eif-feature-level-indent-m () 
  "Indentation amount for features (in number of spaces)."
  '(+ (* eif-feature-level-indent eif-indent-increment) 
     eif-extra-feature-level-indent)
)

(defmacro eif-feature-level-kw-indent-m () 
  "Indentation amount for Feature level keywords (in number of spaces)."
  '(+ (* eif-feature-level-kw-indent eif-indent-increment) 
     eif-extra-feature-level-kw-indent)
)

(defmacro eif-body-comment-indent-m () 
  "Indentation amount for comments in routine bodies (in number of spaces)."
  '(+ (* eif-body-comment-indent eif-indent-increment) 
     eif-extra-body-comment-indent)
)

(defmacro eif-feature-level-comment-indent-m () 
  "Indentation amount for Feature level comments (in number of spaces)."
  '(+ (* eif-feature-level-comment-indent eif-indent-increment) 
     eif-extra-feature-level-comment-indent)
)

(defmacro eif-check-keyword-indent-m ()
  "Indentation amount for Check keyword (in number of spaces)."
  '(+ (* eif-check-keyword-indent eif-indent-increment) 
     eif-extra-check-keyword-indent)
)

(defmacro eif-rescue-keyword-indent-m ()
  "Indentation amount for Rescue keyword (in number of spaces)."
  '(+ (* eif-rescue-keyword-indent eif-indent-increment) 
     eif-extra-rescue-keyword-indent)
)

(defmacro eif-then-indent-m ()
  "Indentation amount for `then' appearing on a line by itself (in number of spaces)."
  '(+ (* eif-then-indent eif-indent-increment) 
     eif-extra-then-indent)
)

(defmacro eif-continuation-indent-m ()
  "Indentation amount for a statement continuation line (in number of spaces)."
  '(+ (* eif-continuation-indent eif-indent-increment) 
     eif-extra-continuation-indent)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Keyword Regular Expression Variables.               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eif-all-keywords-regexp 
  "\\(indexing\\|class\\|inherit\\|creation\\|feature\\|invariant\\|rename\
\\|redefine\\|undefine\\|select\\|export\\|require\\|local\\|deferred\
\\|do\\|once\\|ensure\\|alias\\|external\\|check\\|rescue\\|debug\\|if\
\\|inspect\\|from\\|else\\|elseif\\|when\\|until\\|variant\\|loop\\|then\
\\|obsolete\\|end\\)[^a-z0-9_]"
  "Regular Expression to identify the presence of any eiffel keyword in a line.
Does not include `is'."
  )

;; Note invariant is handled as a special case since it is both a 
;; class-level and a from-level keyword
;; Note obsolete is handled as a special case since it is both a 
;; class-level and a feature-level keyword
(defvar eif-class-level-keywords 
  "\\(indexing\\|class\\|deferred[ \t]*class\\|expanded[ \t]*class\\|inherit\\|creation\\|feature\\)[^a-z0-9_]" 
  "Those keywords introducing class-level clauses. Note that `invariant'
and `obsolete' are not included here since can function as more than one type of keyword. "
  )

(defvar eif-inherit-level-keywords 
  "\\(rename\\|redefine\\|undefine\\|select\\|export\\)" 
  "Those keywords which introduce subclauses of the inherit clause."
  )

(defvar eif-feature-level-keywords 
  "\\(require\\|local\\|deferred\\|do\\|once\\|ensure\\|alias\\|external\\)[^a-z0-9_]"
  "Those keywords which are internal to features (in particular, routines)."
  )

(defvar eif-end-keyword "end" "The `end' keyword.")

(defvar eif-end-on-current-line ".*[ \t]end[ \t]*;?[ \t]*\\(--.*\\)?$" 
  "Regular expression to identify lines ending with the `end' keyword")

(defvar eif-non-id-char-regexp "[^a-z0-9_]" 
  "The characters that are not part of identifiers.")

(defvar eif-end-keyword-regexp "[^a-z0-9_]end[^a-z0-9_]" 
  "The `end' keyword with context.")

(defvar eif-end-matching-keywords
  "\\(check\\|class\\|feature\\|rename\\|redefine\\|undefine\\|select\\|export\\|do\\|once\\|deferred\\|external\\|alias\\|if\\|inspect\\|from\\|debug\\)[^a-z0-9_]"
  "Those keywords whose clause is terminated by an `end' keyword."
  )

(defvar eif-control-flow-keywords 
  "\\(if\\|inspect\\|from\\|debug\\)"
  "Keywords which introduce control-flow constructs."
  )

(defvar eif-control-flow-matching-keywords
  "\\(deferred\\|do\\|once\\|if\\|inspect\\|from\\|debug\\)[^a-z0-9_]" 
  "Keywords whose occurrence prior to a control-flow-keyword causes the
indentation of the control-flow-keyword. Note that technically,
`end' is part of this list but it is handled separately in the
functions: eif-matching-indent and eif-matching-kw."
  )

(defvar eif-check-keyword "check"  "The `check' keyword.")

(defvar eif-check-keywords   "\\(check\\)[^a-z0-9_]"  
  "The `check' keyword (with trailing context).")

(defvar eif-check-matching-keywords 
  "\\(deferred\\|do\\|once\\|if\\|inspect\\|from\\|debug\\)[^a-z0-9_]"
  "Keywords whose occurrence prior to a check-keyword causes the
indentation of the check-keyword. Note that technically, `end' is
part of this list but it is handled separately in the functions:
eif-matching-indent and eif-matching-kw. (see also
eif-control-flow-matching-keywords)"
  )

(defvar eif-rescue-keyword "rescue"  "The `rescue' keyword.")

(defvar eif-obsolete-keyword "obsolete"  "The `obsolete' keyword.")

(defvar eif-rescue-keywords   "\\(rescue\\)[^a-z0-9_]"  
  "The `rescue' keyword (with trailing context).")

(defvar eif-rescue-matching-keywords 
  "\\(deferred\\|do\\|once\\)[^a-z0-9_]"
  "Keywords whose occurrence prior to a rescue-keyword causes the
indentation of the rescue-keyword. Note that technically, `end' is
part of this list but it is handled separately in the functions:
eif-matching-indent and eif-matching-kw. (see also
eif-control-flow-matching-keywords)"
  )

(defvar eif-from-level-keywords 
  "\\(until\\|variant\\|loop\\)[^a-z0-9_]"
  "Keywords occuring inside of a from clause."
  )

(defvar eif-from-keyword  "from" "The keyword `from'.")

(defvar eif-if-or-inspect-level-keywords "\\(elseif\\|else\\|when\\)[^a-z0-9_]"
  "Keywords occuring inside of an if or inspect clause."
  )

(defvar eif-if-or-inspect-keyword "\\(if\\|inspect\\)[^a-z0-9_]"
  "The `if' or `inspect' keywords."
  )

(defvar eif-then-keyword ".*[ \t)]then[ \t]*$" 
  "The keyword `then' with possible leading text.")

(defvar eif-solitary-then-keyword "then" "The keyword `then'.")

(defvar eif-then-matching-keywords "\\(if\\|elseif\\|when\\)"
  "Keywords whose occurrence prior to a then-keyword sets the
indentation of the then-keyword. Note that technically, `end' is
part of this list but it is handled separately in the functions:
eif-matching-indent and eif-matching-kw. (see also
eif-control-flow-matching-keywords)"
  )

(defvar eif-invariant-keyword "invariant" "The `invariant' keyword.")

(defvar eif-invariant-matching-keywords 
  "\\(from\\|feature\\)"
  "Keywords whose occurrence prior to an invariant-keyword causes the
indentation of the invariant-keyword. Note that technically, `end'
is part of this list but it is handled separately in the functions:
eif-matching-indent and eif-matching-kw. (see also
eif-control-flow-matching-keywords)"
  )

(defvar eif-obsolete-matching-keywords 
  "\\(is\\|class\\)"
  "Keywords whose occurrence prior to an obsolete-keyword causes the
indentation of the obsolete-keyword."
  )

(defvar eif-white-space-regexp       "[ 	]*"
  "RE to locate whitespace.")

(defvar eif-comment-line-regexp      "[ 	]*\\(--.*\\)$" 
  "RE to match a line with a comment on it.")

(defvar eif-non-source-line          "[ 	]*\\(--.*\\)?$" 
  "RE to match a line with a only a comment or whitespace.")

(defvar eif-variable-or-const-regexp "[^()\n]*:[^=].*" 
  "RE to match a variable or constant declaration.")

(defvar eif-indentation-keywords-regexp 
  "\\(indexing\\|class\\|check\\|rescue\\|inherit\\|creation\\|feature\\|invariant\\|rename\\|redefine\\|undefine\\|select\\|export\\|require\\|local\\|deferred\\|do\\|once\\|ensure\\|alias\\|external\\|if\\|inspect\\|from\\|debug\\|else\\|elseif\\|when\\|until\\|variant\\|invariant\\|loop\\|obsolete\\)[^a-z0-9_]"
  "RE to identify the presence of any eiffel keyword triggering indentation"
  )

(defvar eif-feature-indentation-keywords-regexp 
  "\\(creation\\|feature\\)[^a-z0-9_]"
  "Keywords which denote the presence of features following them."
  )

(defvar eif-is-keyword-regexp "\\(.*[ 	)]\\)?is[ 	]*\\(--.*\\)?$"
  "The `is' keyword (with some context)."
  )

(defvar eif-multiline-routine-is-keyword-regexp
  ".*([^)]*)\\([ \t\n]*\\|[ \t\n]*:[][ \t\nA-Za-x0-9_,]*\\)is[ 	]*\\(--.*\\)?$"
  "The `is' keyword (with some context)."
  )

(defvar eif-operator-regexp
  "[ 	]*\\([@*/+]\\|-[^-]\\|\\<and[ 	(]\\|\\<or[ 	(]\\)"
  "Eiffel operators - used to identify continuation lines"
  )

(defvar eif-operator-eol-regexp
  ".*\\([@*/+-]\\|\\<and\\|\\<or\\|:=\\)[ 	]*$"
  "Eiffel operators - used to identify continuation lines"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eif-matching-indent -1 
  "The indentation of the keyword found on the last call to eif-matching-kw. 
-1 if no match was found."
  )

(defvar eif-matching-kw-for-end nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Indentation Functions.                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eif-calc-indent ()
  "Calculate the indentation of the current line of eiffel code. This
function generally assumes that the preceding line of code is
indented properly, although lines containing certain class-level
constructs do not require correct indentation of the preceding line."
  (let ((indent   0)
	(line-end 0)
	(originally-looking-at-comment nil)
	(kw-match nil)
	(continuation)
	(id-colon)
	)
    
    (save-excursion
      
      ;; Save location of line-end and skip past leading white space.
      (end-of-line)
      (setq line-end   (point))
      (beginning-of-line)
      (re-search-forward eif-white-space-regexp line-end t)
      
      ;; Is the line we are trying to indent a comment line?
      (setq originally-looking-at-comment (looking-at eif-comment-line-regexp))
      
      ;; Look for a keyword on the current line
      (if (looking-at eif-all-keywords-regexp)
	  
	  ;; Then we are looking at a keyword
	  (cond ((looking-at eif-class-level-keywords)
		 ;; File level keywords (indent defaults to 0)
		 (setq indent (eif-class-level-kw-indent-m))
		 )
		((looking-at eif-inherit-level-keywords)
		 ;; Inherit level keywords (indent defaults to 
		 ;; 2*eif-indent-increment)
		 (setq indent (eif-inherit-level-kw-indent-m))
		 )
		((looking-at eif-feature-level-keywords)
		 ;; Feature level keywords (indent defaults to 
		 ;; (eif-feature-level-indent-m) + eif-indent-increment)
		 (setq indent (eif-feature-level-kw-indent-m))
		 )
		((looking-at eif-end-keyword)
		 ;; End keyword (indent to level of matching keyword)
		 (if (string-match "end" 
				   (eif-matching-kw eif-end-matching-keywords))
		     ;; Then 
		     (if (= eif-matching-indent 
			    (eif-feature-level-kw-indent-m))
			 ;; Then
			 (setq indent (eif-class-level-kw-indent-m))
		       ;; Else
		       (setq indent 
			     (- eif-matching-indent eif-indent-increment))
		       )
		   ;; Else
		   (setq indent eif-matching-indent)
		   )
		 (if (<= indent (eif-feature-level-indent-m))
		     (save-excursion
		       (end-of-line)
		       (while (and (< (point) (point-max))
				   (or (forward-char 1) t)
				   (looking-at eif-non-source-line)
				   )
			 (end-of-line)
			 )
		       (if (not (looking-at eif-non-source-line))
			   (setq indent (eif-inherit-level-kw-indent-m))
			 (setq indent (eif-class-level-kw-indent-m))
			 )
		       )
		   )
		 )
		((looking-at eif-control-flow-keywords)
		 ;; Control flow keywords 
		 ;;  Indent to same level as a preceding "end" or
		 ;;  if no preceding "end" is found, indent to the level
		 ;;  of the preceding "do" plus the value of 
		 ;;  eif-indent-increment
		 (setq kw-match 
		       (eif-matching-kw eif-control-flow-matching-keywords)) 
		 (cond ((string-match "end" kw-match)
			(setq indent eif-matching-indent)
			)
		       (t
			(setq indent 
			      (+ eif-matching-indent eif-indent-increment)
			      )
			)
		       )
		 )
		((looking-at eif-check-keywords)
		 ;; Check keyword
		 ;;  Indent to level of preceding "end"+eif-indent-increment or
		 ;;  if no preceding "end" is found, indent to the level
		 ;;  of the preceding eif-check-matching-keywords plus the 
		 ;;  value (eif-indent-increment + eif-check-keyword-indent).
		 (setq kw-match (eif-matching-kw eif-check-matching-keywords)) 
		 (cond ((string-match "end" kw-match)
			(setq indent (+ eif-matching-indent 
					(eif-check-keyword-indent-m)
					)
			      )
			)
		       (t
			(setq indent 
			      (+ eif-matching-indent 
				 (+ eif-indent-increment 
				    (eif-check-keyword-indent-m)
				    )
				 )
			      )
			)
		       )
		 )
		((looking-at eif-rescue-keywords)
		 ;; Rescue keyword
		 ;;  Indent to level of preceding "end"+eif-indent-increment or
		 ;;  if no preceding "end" is found, indent to the level
		 ;;  of the preceding eif-rescue-matching-keywords plus the 
		 ;;  value (eif-indent-increment + eif-rescue-keyword-indent).
		 (setq kw-match (eif-matching-kw eif-rescue-matching-keywords)) 
		 (cond ((string-match "end" kw-match)
			(setq indent (+ eif-matching-indent 
					(eif-rescue-keyword-indent-m)
					)
			      )
			)
		       (t
			(setq indent eif-matching-indent)
			)
		       )
		 )
		((looking-at eif-from-level-keywords)
		 ;; From level keywords (indent to level of matching "From")
		 (if (string-match "end" (eif-matching-kw eif-from-keyword))
		     ;; Closest matching KW is `end'.
		     (setq indent (- eif-matching-indent eif-indent-increment))
		   ;; Closest matching KW is one of `eif-from-keyword'.
		   (setq indent eif-matching-indent)
		   )
		 )
		((looking-at eif-if-or-inspect-level-keywords)
		 ;; If level keywords (indent to level of matching 
		 ;; "If" or "Inspect")
		 (if (string-match "end" 
				   (eif-matching-kw eif-if-or-inspect-keyword)
				   )
		     ;; Closest matching KW is `end'.
		     (setq indent (- eif-matching-indent eif-indent-increment))
		   ;; Closest matching KW is one of `eif-if-or-inspect-keyword'.
		   (setq indent eif-matching-indent)
		   )
		 )
		((looking-at eif-solitary-then-keyword)
		 ;; Handles case where "then" appears on a line by itself
		 ;;   (Indented to the level of the matching if, elseif or when)
		 (setq indent (+ (eif-matching-indent eif-then-matching-keywords) 
				 (eif-then-indent-m)
				 )
		       )
		 )
		((looking-at eif-invariant-keyword)
		 ;; Invariant keyword
		 ;;   (Indented to the level of the matching from or feature)
		 (if (string-match "from" 
				   (eif-matching-kw eif-invariant-matching-keywords)
				   )
		     ;; Then - loop invariant
		     (setq indent eif-matching-indent)
		   ;; Else - class invariant
		   (setq indent (eif-class-level-kw-indent-m))
		   )
		 )
		((looking-at eif-obsolete-keyword)
		 ;; Obsolete keyword
		 ;;   (Indented to the level of the matching from or feature)
		 (if (string-match "is" 
				   (eif-matching-kw eif-obsolete-matching-keywords)
				   )
		     ;; Then - feature obsolete
		     (setq indent (eif-feature-level-kw-indent-m))
		   ;; Else - class obsolete
		   (setq indent (eif-class-level-kw-indent-m))
		   )
		 )		;; end of cond
		)
	
	;; Else no keyword on current line, 
	;;   are we in a multi-line parenthesis expression
	
	(if (or (and (> (eif-in-paren-expression) 0)
		     (> (setq indent (eif-indent-multi-line)) -1)
		     )
		(setq indent (eif-manifest-array-indent))
		)
	    
	    ;; multi-line parenthesis expression
	    ;; Move string continuation lines one column to the left 
	    (if (looking-at "%")
		(setq indent (1- indent))
	      )
	  
	  ;; Else Find the first preceding line with non-comment source on it
	  ;; that is not a continuation line of a multi-line parnethesized
	  ;; expression.

	  ;; Record whether this line begins with an operator. We assume 
	  ;; that the line is a continuation line if it begins with an operator
	  (beginning-of-line)
	  (if (looking-at eif-operator-regexp)
	      (setq continuation t)
	    (setq continuation nil)
	    )
	  ;; Record whether the line being indented begins with an "<id> :"
	  ;; This is used in indenting assertion tag expressions.
	  (if (looking-at "[ 	]*[a-zA-Z0-9_]+[ 	]*:")
	      (setq id-colon t)
	    (setq id-colon nil)
	    )
	  
	  (forward-line -1)
	  (beginning-of-line)
	  (while (and (looking-at eif-non-source-line) (not (= (point) 1)))
	    (previous-line 1)
	    (beginning-of-line)
	    )
	  (if (eif-line-contains-close-paren)
	      (backward-sexp)
	    )
	  (end-of-line)
	  (setq line-end (point))
	  (beginning-of-line)
	  (re-search-forward eif-white-space-regexp line-end t)
	  
	  (cond ((and (= (point) 1)
		      originally-looking-at-comment
		      (setq indent (eif-class-level-comment-indent-m))
		      )
		 )
		;; 'eif-is-keyword-regexp' case must precede 
		;; '(not eif-all-keywords-regexp)' case since "is" is not 
		;; part of 'eif-all-keywords-regexp'
		((or (looking-at eif-is-keyword-regexp)
		     (looking-at eif-multiline-routine-is-keyword-regexp)
		     (looking-at eif-obsolete-keyword) 
		     )
		 (if originally-looking-at-comment
		     ;; Then  the line we are trying to indent is a comment
		     (setq indent (eif-feature-level-comment-indent-m))
		   ;; Else  the line being indented is not a comment
		   (setq indent (eif-feature-level-kw-indent-m))
		   )
		 )
		((looking-at eif-feature-indentation-keywords-regexp)
		 (setq indent (eif-feature-level-indent-m))
		 )
		((looking-at eif-indentation-keywords-regexp)
		 (if (looking-at eif-end-on-current-line)
		     (setq indent (eif-current-line-indent))
		   (setq indent 
			 (+ (eif-current-line-indent) eif-indent-increment))
		   )
		 )
		((looking-at eif-solitary-then-keyword)
		 (setq indent (- (+ (eif-current-line-indent) eif-indent-increment)
				 (eif-then-indent-m)
				 )
		       )
		 )
		((looking-at eif-then-keyword)
		 (setq indent (eif-current-line-indent))
		 )
		((looking-at (concat eif-end-keyword eif-non-id-char-regexp))
		 (if (= (setq indent (eif-current-line-indent)) 
			(eif-feature-level-kw-indent-m)
			)
		     (setq indent (eif-feature-level-indent-m))
		   (eif-matching-line)
		   (if (string-match eif-check-keyword eif-matching-kw-for-end)
		       (setq indent (- indent (eif-check-keyword-indent-m)))
		     )
		   )
		 )
		((looking-at eif-variable-or-const-regexp)
		 ;;Either a variable declaration or a pre or post condition tag
		 (if originally-looking-at-comment
		     ;; Then  the line we are trying to indent is a comment
		     (if (= (setq indent (eif-current-line-indent)) 
			    (eif-feature-level-indent-m)
			    )
			 ;; Then - a feature level comment
			 (setq indent (eif-feature-level-comment-indent-m))
		       ;; Else - some other kind of comment
		       (setq indent (+ indent (eif-body-comment-indent-m)))
		       )
		   ;; Else  the line being indented is not a comment
		   (if (setq indent (eif-indent-assertion-continuation id-colon))
		       indent
		     (setq indent (eif-current-line-indent))
		     )
		   )
		 )
		((setq indent (eif-manifest-array-start))
		 indent
		 )
		((not (looking-at eif-all-keywords-regexp))
		 (if originally-looking-at-comment
		     ;; Then  the line we are trying to indent is a comment
		     (cond ((eif-continuation-line)
			    (setq indent 
				  (+ (- (eif-current-line-indent) 
					eif-indent-increment
					)
				     (eif-body-comment-indent-m)
				     )
				  )
			    )
			   ;; preceding line is at eif-feature-level-indent - 
			   ;; assume that the preceding line is a parent 
			   ;; class in an inherit clause
			   ((= (eif-current-line-indent) 
			       (eif-feature-level-indent-m)
			       )
			    (setq indent 
				  (+ (eif-inherit-level-kw-indent-m)
				     (eif-body-comment-indent-m)
				     )
				  )
			    )
			   (t
			    (setq indent 
				  (+ (eif-current-line-indent)
				     (eif-body-comment-indent-m)
				     )
				  )
			    )
			   )
		   ;; Else line being indented is not a comment

		   ;; The line the point is on is the one above the line being
		   ;; indented
		   (beginning-of-line)
		   (if (or continuation (looking-at eif-operator-eol-regexp))
		       ;; Then the line being indented is a continuation line
		       (if  (eif-continuation-line)
			   ;; The line preceding the line being indented is 
			   ;; also a continuation line. Indent to the current
			   ;; line indentation.
			   (setq indent (eif-current-line-indent))
			 ;; Else The line preceding the line being indented is 
			 ;; not a continuation line. Indent an extra 
			 ;; eif-continuation-indent
			 (setq indent (+ (eif-current-line-indent)
					 (eif-continuation-indent-m)))
			 )
		     ;; Else the line being indented is not a continuation line.
		     (if (eif-continuation-line)
			 (if id-colon
			     ;; Then the line preceding the one being indented
			     ;; is an assertion continuation. Indent the current
			     ;; line to the same level as the preceding assertion
			     ;; tag.
			     (setq indent (eif-indent-assertion-tag))
			   ;; Then the line preceding the one being indented is 
			   ;; a continuation line. Un-indent by an 
			   ;; eif-continuation-indent.
			   (setq indent (- (eif-current-line-indent) 
					   eif-indent-increment
					   )
				 )
			   )
		       ;; Else the line preceding the line being indented is
		       ;; also not a continuation line. Use the current indent.
		       (setq indent (eif-current-line-indent))
		       )
		     )
		   )
		 )
		) ;; cond
	  ) ;; if
	) ;; if
      ) ;; save-excursion
    indent
    ) ;; let
  )

(defun eif-continuation-line ()
  (or (looking-at eif-operator-regexp)
      (save-excursion 
	(forward-line -1)
	(beginning-of-line)
	(looking-at eif-operator-eol-regexp)
	)
      )
  )

(defun eif-indent-assertion-continuation (id-colon)
  "Are we inside a pre or a post condition clause on a line that is a
continuation of a multi-line assertion beginning with a tag?  If so, return
the indentation of the continuation line."
  (let ((limit (point)))
    (if (save-excursion 
	  (if (re-search-backward (concat eif-feature-level-keywords "\\|"
					  eif-end-keyword-regexp) nil t) 
	      (if (looking-at "ensure\\|require")
		  (setq limit (point))
		nil
		)
	    nil
	    )
	  )
	(save-excursion
	  (end-of-line)
	  (if (and (not id-colon) (re-search-backward ": *" limit t))
	      (progn
		(goto-char (match-end 0))
		(current-column)
		)
	    nil
	    )
	  )
      nil
      )
    )
  )

(defun eif-indent-assertion-tag ()
  "Are we inside a pre or a post condition clause on a line that is a
continuation of a multi-line assertion beginning with a tag?  If so, return
the indentation of the continuation line."
  (let (indent)
    (save-excursion 
      (if (re-search-backward "ensure\\|require\\|variant\\|invariant" nil t)
	  (setq indent (+ (eif-current-line-indent) eif-indent-increment))
	;; This option should not occur
	(error "Could not find assertion tag.")
	)
      )
    )
  )

(defun eif-matching-indent (matching-keyword-regexp)
  "Search backward from the point looking for one of the keywords
in the MATCHING-KEYWORD-REGEXP. Return the indentation of the
keyword found. If an `end' keyword occurs prior to finding one of
the keywords in MATCHING-KEYWORD-REGEXP and it terminates a check
clause, return the indentation of the `end' minus the value of
eif-check-keyword-indent."
  (let ((search-regexp (concat "[^a-z0-9A-Z_]"
			       eif-end-keyword 
			       "[^a-z0-9A-Z_]\\|[^a-z0-9A-Z_]" 
			       matching-keyword-regexp
			       )
		       )
	(indent 0)
	(continue t)
	)
    (save-excursion
      (while (and (re-search-backward search-regexp 1 t)
		  (or (eif-in-quoted-string-p)
		      (eif-in-comment-p)
		      )
		  )
	)
      (if (looking-at search-regexp)
	  ;; Then
	  (if (and (looking-at eif-end-keyword-regexp)
		   (eif-matching-line)
		   (string-match eif-check-keyword eif-matching-kw-for-end)
		   )
	      ;; The keyword "end" was found that terminated a "check" clause
	      (setq indent (- (eif-current-line-indent) (eif-check-keyword-indent-m)))
	    ;; Else a keyword in "matching-keyword-regexp" or a normal 
	    ;; "end"was found
	    (setq indent (eif-current-line-indent))
	    )
	;; Else
	(message "No matching indent keyword was found")
	)
      indent
    
      )
    )
  )

(defun eif-matching-kw (matching-keyword-regexp)
  "Search backward from the point looking for one of the keywords in
the MATCHING-KEYWORD-REGEXP. Return the keyword found. Also set the
value of eif-matching-indent to the indentation of the keyword
found.  If an `end' keyword occurs prior to finding one of the
keywords in MATCHING-KEYWORD-REGEXP and it terminates a check
clause, set the value of eif-matching-indent to the indentation of
the `end' minus the value of eif-check-keyword-indent."
  (let ((search-regexp (concat "[^a-z0-9A-Z_.]" 
			       eif-end-keyword 
			       "[^a-z0-9A-Z_.]\\|[^a-z0-9A-Z_.]" 
			       matching-keyword-regexp
			       )
		       )
	(keyword "")
	)
    (save-excursion
      ;; Search backward for a matching keyword.
      (while (and (re-search-backward search-regexp 1 t)
		  (or (eif-in-quoted-string-p)
		      (eif-in-comment-p)
		      )
		  )
	)
      (if (looking-at search-regexp)
	  ;; Then - a keyword was found
	  (progn
	    (setq keyword 
		  (buffer-substring (match-beginning 0) (match-end 0))
		  )
	    (if (and (looking-at eif-end-keyword-regexp)
		     (eif-matching-line)
		     (string-match eif-check-keyword eif-matching-kw-for-end)
		     )
		;; Then
		(setq eif-matching-indent 
		      (- (eif-current-line-indent) (eif-check-keyword-indent-m))
		      )
	      ;; Else
	      (setq eif-matching-indent (eif-current-line-indent))
	      )
	    )
	;; Else no keyword was found. I think this is an error
	(setq eif-matching-indent 0)
	(message "No matching indent keyword was found")
	)
      keyword
      )
    )
  )

(defun eif-line-contains-close-paren ()
  "This function returns t if the current line contains a close paren and
nil otherwise. If a close paren is found, the point is placed immediately
after the last close paren on the line. If no paren is found, the point is
placed at the beginning of the line."
  (let ((search-min 0))
    (beginning-of-line)
    (setq search-min (point))
    (end-of-line)
    (if (search-backward ")" search-min t)
	;; Then
	(progn
	  (forward-char 1)
	  t
	  )
      ;; Else
      (beginning-of-line)
      nil
      )
    )
  )

;; Not Currently Used
;;(defun eif-quoted-string-on-line-p ()
;;  "t if an Eiffel quoted string begins, ends, or is continued 
;;   on current line."
;;  (save-excursion
;;    (beginning-of-line)
;;    ;; Line must either start with optional whitespace immediately followed
;;    ;; by a '%' or include a '\"'.  It must either end with a '%' character
;;    ;; or must include a second '\"' character.
;;    (looking-at "^\\([ \t]*%\\|[^\"\n]*\"\\)[^\"\n]*\\(%$\\|\"\\)")
;;  )
;;)

(defvar eif-opening-regexp 
  "\\<\\(external\\|check\\|deferred\\|do\\|once\\|from\\|if\\|inspect\\)\\>"
  "Keywords that open eiffel nesting constructs."
  )
(defvar eif-closing-regexp "\\<end\\>"
  "Keywords that close eiffel nesting constructs."
  )
(defvar eif-do-regexp "\\<do\\|once\\|external\\>"
  "Keyword that opens eiffel routine body."
  )
(defvar eif-opening-or-closing-regexp 
  (concat "\\(" eif-opening-regexp "\\|" eif-closing-regexp "\\)") 
  "Keywords that open or close eiffel nesting constructs."
  )

;;
;; Code to allow indenting whole eiffel blocks
;;

(defun eif-matching-line (&optional return-line-break direction)
  "Return the character position of the keyword matching the eiffel
keyword on the current line. (e.g. a line containing the keyword
'do' is matched by a line containing the keyword 'end' and a line
containing 'end' may be matched by a number of opening keywords.
If the optional parameter 'return-line-break' is not nil, the
character position returned is the beginning (or end) of the line
containing the matching keyword instead of the position of the
keyword itself. If the second optional parameter, direction, is 
non-null, the current line is not searched for a keyword. Instead, 
if the value of direction is 'forward, the function acts as if
an eif-opening-regexp is on the current line. If the value of direction
is 'backward, the function acts as if a eif-closing-regexp is on the 
current line. The effect of using the direction parameter is to 
locate either the opening or closing keyword of the syntactic 
construct containing the point."
  (let ((nesting-level 0)
	(matching-point nil)
	(search-end 0)
	(opening-keyword nil)
	(match-start nil)
	(match-end nil)
	(success   nil)
	(start-point nil)
	)
    (unwind-protect
	(save-excursion
	  (modify-syntax-entry ?_  "w  ")
	  (setq eif-matching-kw-for-end "");; public variable set by this function
	  (setq start-point (point))
	  (end-of-line)
	  (setq search-end (point))
	  (beginning-of-line)
	  ;; Set starting state: If direction was specified use it.
	  ;; If direction is nil, search for a keyword on the current line
	  ;; If the keyword is in eif-opening-regexp, set the search 
	  ;; direction to 'forward, if the keyword on the current line is `end' 
	  ;; set the search direction to 'backward.
	  (cond ((eq direction 'forward)
		 (end-of-line);; So we wont see any keywords on the current line
		 (setq nesting-level 1)
		 )
		((eq direction 'backward)
		 (beginning-of-line);; So we wont see any keywords on the current line
		 (setq nesting-level -1)
		 )
		((and (re-search-forward eif-opening-regexp search-end t)
		      (not (or (eif-in-quoted-string-p)
			       (eif-in-comment-p)
			       )
			   )
		      )
		 (setq match-start (match-beginning 0))
		 (goto-char match-start) 
		 (if (not (or (eif-in-quoted-string-p) (eif-in-comment-p)))
		     (setq nesting-level 1)
		   )
		 (setq opening-keyword 
		       (cons (buffer-substring match-start (match-end 0))
			     opening-keyword
			     )
		       )
		 (goto-char (match-end 0))
		 )
		((and (progn (beginning-of-line) t)
		      (re-search-forward eif-closing-regexp search-end t)
		      (not (or (eif-in-quoted-string-p)
			       (eif-in-comment-p)
			       )
			   )
		      )
		 (goto-char (match-beginning 0))
		 (if (not (or (eif-in-quoted-string-p) (eif-in-comment-p)))
		     (setq nesting-level -1)
		   )
		 )
		)
	  ;; Perform the search
	  (while (not (= nesting-level 0))
	    (if (> nesting-level 0)
		;; Then search forward for the next keyword not in a comment
		(while (and (re-search-forward eif-opening-or-closing-regexp nil 1)
			    (goto-char (setq match-start (match-beginning 0)))
			    (setq match-end   (match-end 0))
			    (setq success t)
			    (or (eif-in-quoted-string-p) (eif-in-comment-p))
			    )
		  (goto-char match-end)
		  (setq success nil)
		  )
	      ;; Else search backward for the next keyword not in a comment
	      (while (and (re-search-backward eif-opening-or-closing-regexp nil 1)
			  (goto-char (setq match-start (match-beginning 0)))
			  (setq success t)
			  (or (eif-in-quoted-string-p) (eif-in-comment-p))
			  )
		(setq success nil)
		)
	      )
	    (cond ((and (looking-at eif-opening-regexp) success)
		   ;; Found an opening keyword
		   (if (> nesting-level 0)
		       ;; Then
		       (if (looking-at eif-do-regexp)
			   ;; Then
			   (setq nesting-level -1)
			 ;; Else
			 (setq opening-keyword 
			       (cons (buffer-substring match-start (match-end 0))
				     opening-keyword
				     )
			       )
			 (goto-char (match-end 0))
			 )
		     ;; Else
		     (if (= nesting-level -1)
			 ;; Then
			 (progn
			   (setq eif-matching-kw-for-end
				 (buffer-substring match-start (match-end 0))
				 )
			   (if (looking-at "[ \t\n]+") (goto-char (match-end 0)))
			   )
		       ;; Else
		       (if (looking-at eif-do-regexp)
			   ;; Then
			   (progn
			     (goto-char (eif-matching-line nil 'forward))
			     (setq nesting-level -1)
			     )
			 )
		       )
		     (setq opening-keyword (cdr opening-keyword))
		     (if return-line-break
			 (beginning-of-line)
		       )
		     )
		   (setq nesting-level (1+ nesting-level))
		   )
		  ((and (looking-at eif-closing-regexp) success)
		   ;; Found an opening keyword
		   (if (> nesting-level 0)
		       ;; Then
		       (progn
			 (setq opening-keyword (cdr opening-keyword))
			 (if return-line-break
			     (end-of-line)
			   )
			 (goto-char (match-end 0))
			 )
		     ;; Else
		     (setq opening-keyword 
			   (cons (buffer-substring (match-beginning 0) 
						   (match-end 0)
						   )
				 opening-keyword
				 )
			   )
		     )
		   (setq nesting-level (1- nesting-level))
		   )
		  (t (message (concat "Could not find match"
				      (if (car opening-keyword) 
					  (concat " for: " (car opening-keyword))
					)
				      )
			      )
		     (goto-char start-point)
		     (setq nesting-level 0)
		     )
		  );; cond
	    );; while
	  (setq matching-point (point))      
	  );; save-excursion
      (modify-syntax-entry ?_  "_  ")
      );; unwind-protect
    (set-mark matching-point)
    );; let
  );; eif-matching-line 

;; ENHANCEME: Make this function correctly indent more than just routine 
;;            bodies and their sub-constructs. At the least it should 
;;            handle whole routines also.
(defun eif-indent-construct ()
  "Indents an entire eiffel syntactic construct. It is assumed that
the point is within a nesting construct ('do', `once', 'check', 'if',
'from', or 'inspect'). The whole construct is indented up to the
matching end. If the point is not within such a construct, then
only that line is indented"
  (interactive)
  (let ((end-point 0))
    (save-excursion
      (end-of-line)
      (if (not (= (point) (point-max))) (forward-char 1))
      (goto-char (eif-matching-line t 'backward))
      (setq end-point (eif-matching-line t 'forward))
      (while (< (point) end-point)
	(eif-indent-line)
	(next-line 1)
	(beginning-of-line)
	)
      )
    )
  )

(defun eif-indent-region (&optional start end)
  "Indents the lines in the current region"
  (interactive)
  (let ((start-point (or start (region-beginning))) 
	(end-point   (copy-marker (or end (region-end))))
	)
    (save-excursion
      (goto-char start-point)
      (cond ((eq major-mode 'eiffel-mode)
	     (while (< (point) end-point)
	       (if (not (looking-at "[ 	]*$"))
		   (eif-indent-line)
		 )
	       (next-line 1)
	       (beginning-of-line)
	       )
	     )
	    (t (error "Buffer must be in eiffel mode."))
	    )
      )
    )
  )

;;(defun eif-goto-matching-line (&optional direction)
;;  "Place the cursor on the line which closes(opens) the current
;;opening(closing) syntactic construct. For example if the point
;;is on `from', executing goto-matching-line places the point
;;on the matching `end' and vice-versa."
;;  (interactive)
;;  (if (not direction)
;;      (progn
;;	(cond ((save-excursion (beginning-of-line) (looking-at "[ 	]*end.*$"))
;;	       (goto-char (eif-matching-line nil 'backward))
;;	       )
;;	      ((looking-at "(")
;;	       (forward-sexp)
;;	       )
;;	      ((save-excursion (backward-char 1) (looking-at ")"))
;;	       (backward-sexp)
;;	       )
;;	      (t
;;	       (goto-char (eif-matching-line nil 'forward))
;;	       )
;;	      )
;;	)
;;    )
;;  )

(defun eif-forward-sexp ()
  "Place the cursor on the line which closes the current opening syntactic construct. For example if the point is  on `from', executing eif-forward-sexp places the point on the matching `end'. This also does matching of parens ala forward-sexp."
  (interactive)
  (cond ((looking-at "[[(]")
	 (forward-sexp)
	 )
	(t
	 (goto-char (eif-matching-line nil 'forward))
	 )
	)
  )

(defun eif-backward-sexp ()
  "Place the cursor on the line which opens  the current closing syntactic construct. For example if the point is  on the terminating `end' of an `if' statement, executing  eif-backward-sexp places the point on the opening `if'.  This also does matching of parens ala backward-sexp."
  (interactive)
  (cond ((save-excursion (backward-char 1) (looking-at "[])]"))
	 (backward-sexp)
	 )
	(t
	 (goto-char (eif-matching-line nil 'backward))
	 )
	)
  )

(defun eif-local-indent (amount)
  "Set the value of eif-indent-increment to amount and make the change local to this buffer."
  (interactive "NNumber of spaces for eif-indent-increment: ")
  (make-local-variable 'eif-indent-increment)
  (setq eif-indent-increment amount)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Utility Functions.                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eif-feature-quote ()
  "Put a `' around the current feature name"
  (interactive)
  (save-excursion
    (backward-sexp)
    (insert "`")
    (forward-sexp)
    (insert "'")
    )
  (if (looking-at "'")
      (forward-char 1))
  )

(defvar eiffel-mode-abbrev-table nil)
(define-abbrev-table 'eiffel-mode-abbrev-table ())

;; ----------------------------------------------------------------------
;; This next portion of the file is derived from "eiffel.el"
;; Copyright (C) 1989, 1990 Free Software Foundation, Inc. and Bob Weiner
;; Available for use and distribution under the same terms as GNU Emacs.
;; ----------------------------------------------------------------------

(defvar eiffel-mode-map nil 
  "Keymap for Eiffel mode.")

(defvar eiffel-mode-syntax-table nil
  "Syntax table in use in Eiffel-mode buffers.")

(if eiffel-mode-syntax-table
    nil
  (let ((table (make-syntax-table))
	(i 0))
    (while (< i ?0)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (modify-syntax-entry ?  "    " table)
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?_  "_  " table)
    (modify-syntax-entry ?\t "    " table)
    (modify-syntax-entry ?\n ">   " table)
    (modify-syntax-entry ?\f ">   " table)
    (modify-syntax-entry ?\" "\"    " table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "\\" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?. "." table)
    (setq eiffel-mode-syntax-table table))
    )

(if eiffel-mode-map
    nil  
  (setq eiffel-mode-map (make-sparse-keymap))
  (define-key eiffel-mode-map [(tab)] 'eif-indent-line)
  (define-key eiffel-mode-map [(control j)] 'eif-newline)
  (define-key eiffel-mode-map [(return)] 'eif-indent-and-newline)
  (define-key eiffel-mode-map [(meta control q)] 'eif-indent-construct)
  (define-key eiffel-mode-map [(meta \')] 'eif-feature-quote)
  (define-key eiffel-mode-map [(meta q)] 'eif-fill-paragraph)
  )

;;;###autoload
(defun eiffel-mode ()
  "Major mode for editing Eiffel programs."

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'eiffel-mode)
  (setq mode-name "Eiffel")

  (if eif-use-gnu-eiffel
      (progn
	(define-key eiffel-mode-map "\C-c\C-c" 'eif-compile)
	(define-key eiffel-mode-map "\C-c\C-o" 'eif-set-compile-options)
	(define-key eiffel-mode-map "\C-c\C-r" 'eif-run)
	(define-key eiffel-mode-map "\C-c\C-s" 'eif-short))
    (define-key eiffel-mode-map "\C-c\C-c" nil)
    (define-key eiffel-mode-map "\C-c\C-r" nil)
    (define-key eiffel-mode-map "\C-c\C-o" nil))
    

  (use-local-map eiffel-mode-map)
  (set-syntax-table eiffel-mode-syntax-table)

  ;; Make local variables.
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  ;; Now set their values.
  (setq paragraph-start (concat "^$\\|" page-delimiter)
	paragraph-separate paragraph-start
	paragraph-ignore-fill-prefix t
	require-final-newline 'ask
	parse-sexp-ignore-comments t
	indent-line-function 'eif-indent-line
	indent-region-function 'eif-indent-region
	comment-start "-- "
	comment-end ""
	comment-column 32
	comment-start-skip "--+ *")

  (require 'easymenu)
  (easy-menu-define
   eiffel-mode-menu
   eiffel-mode-map
   "Menu for eiffel-mode."
   (append (list "Eiffel")
	   (if eif-use-gnu-eiffel
	       (list
		["Compile..."            eif-compile t]
		["Compiler Options..."   eif-set-compile-options t]
		["Next Compile Error..." next-error  t]
		["Run..."                eif-run     t]
		["Short..."              eif-short   t]
		["----------" nil nil]))
	   (list
	    ["Indent Construct"     eif-indent-construct t]
	    ["----------" nil nil]
	    (list "Comments"
		  ["Feature Quote" eif-feature-quote  (eif-in-comment-p)]
		  ["Fill         " eif-fill-paragraph (eif-in-comment-p)]))))

  (setq local-abbrev-table eiffel-mode-abbrev-table)
  (setq auto-fill-function 'eif-auto-fill)
  (run-hooks 'eiffel-mode-hook)
  )

(defun eif-in-comment-p ()
  "t if point is in a comment."
  (save-excursion
    (and (/= (point) (point-max)) (forward-char 1))
    (search-backward "--" (save-excursion (beginning-of-line) (point)) t)))


;; ENHANCEME: Currently eif-beginning-of-feature only works for routines. 
;;            It should be made more general.
;;

(defun eif-beginning-of-feature (&optional arg)
  "Move backward to next feature beginning. With argument, do this that many 
times. Returns t unless search stops due to beginning of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (if (or (re-search-backward eif-multiline-routine-is-keyword-regexp 
			      nil t (or arg 1))
	  (re-search-backward eif-is-keyword-regexp 
			      nil 'move (or arg 1))	  
	  )
      (progn
	(backward-sexp 1)
	(if (looking-at "(")
	    (backward-word 1)
	  )
	(beginning-of-line)
	)
    nil
    )
  )

(defun eif-current-line-indent ()
  "Return the indentation of the line containing the point."
  (save-excursion
    (let ((line-end 0)
	  (indent   0)
	  )
      (end-of-line)
      (setq line-end (point))
      (beginning-of-line)
      (re-search-forward eif-white-space-regexp line-end t)
      (setq indent (current-column))
      )
    )
  )

(defun eif-in-quoted-string-p (&optional non-strict-p)
  "t if point is in a quoted string. Optional argument NON-STRICT-P if true
causes the function to return true even if the point is located in leading
white space on a continuation line. Normally leading white space is not
considered part of the string."
  (let ((initial-regexp "^[ \t]*%\\|[^%]\"\\|%[ \t]*$")
	(search-limit (point))
	(count 0)
	)
    (save-excursion
      ;; Line must either start with optional whitespace immediately followed
      ;; by a '%' or include a '\"' before the search-limit.
      (beginning-of-line)
      (while (re-search-forward initial-regexp search-limit t)
	(setq count (1+ count))
	(if (= count 1) (setq search-limit (1+ search-limit)))
	)
      ;; If the number of quotes (including continuation line markers) is odd, 
      ;; then we are inside of a string. Also if non-strict-p and we are in 
      ;; the leading white space of a continuation line, then we are in a quote.
      (if (= (% count 2) 1)
	  t
	(beginning-of-line)
	(if non-strict-p
	    (if (looking-at "^[ \t]*%")
		t
	      nil
	      )
	  nil
	  );; if
	);; if
      );; save-excursion
    );; let
  );; e-in-quoted-string

;; ----------------------------------------------------------------------
;; End of portion derived from "eiffel.el"
;; ----------------------------------------------------------------------

(defun eif-comment-prefix ()
  "Prefix that starts a comment that begins a line.
   Comments that are not the only thing on a line return nil as their prefix."
  (save-excursion
    (end-of-line)
    (let ((limit (point)) len
	  (in-string (eif-in-quoted-string-p))
	  )
      (beginning-of-line)
      (cond ((re-search-forward "^[ \t]*--|?[ \t]*" limit t)
	     (buffer-substring (match-beginning 0) (match-end 0))
	     )
	    ;; Handle string-literal continuation lines
	    (in-string
	     (end-of-line)
	     (re-search-backward "^[ \t]*%\\|[^%]\"" nil t)
	     (re-search-forward "%\\|\"" nil t)
	     (setq len (1- (current-column)))
	     (concat (make-string len ? ) "%")
	     )
	    (t	nil)
	    )
      )
    )
  )


(defun eif-auto-fill ()
  (let ((fill-prefix (eif-comment-prefix)) (pm (point-marker)))
    (if (and (> (current-column) fill-column) fill-prefix)
	(if (string-match "^[ \t]*%" fill-prefix)
	    (progn
	      (backward-char 1)
	      (re-search-backward "[^][a-zA-Z0-9]" nil t)
	      (forward-char 1)
	      (insert "%\n")
	      (insert fill-prefix)
	      (goto-char pm)
	      )
	  ;; (do-auto-fill)
	  (backward-char 1)
	  (re-search-backward "\\s-" nil t)
	  (forward-char 1)
	  (insert "\n")
	  (insert fill-prefix)
	  (goto-char pm)
	  )
      )
    )
  )

(defun eif-fill-paragraph ()
  "Textually fills Eiffel comments ala fill-paragraph"
  (interactive)
  (save-excursion
    (let ((current-point (point))
	  (last-point nil)
	  (para-begin nil)
	  (para-end   nil)
	  (fill-prefix (eif-comment-prefix))
	  )
      (if fill-prefix
	  (progn
	    (setq last-point (point))
	    (forward-line -1)
	    (end-of-line)
	    (while (and (not (= (point) last-point))
			(eif-comment-prefix)
			)
	      (setq last-point (point))
	      (forward-line -1)
	      (end-of-line)
	      )
	    (if (= (point) last-point)
		(setq para-begin (save-excursion (beginning-of-line) (point)))
	      (setq para-begin (1+ (point)))
	      )
	    (goto-char current-point)
	    (setq last-point (point))
	    (next-line 1)
	    (end-of-line)
	    (while (and (not (= (point) last-point))
			(eif-comment-prefix)
			)
	      (setq last-point (point))
	      (next-line 1)
	      (end-of-line)
	      )
	    (if (= (point) last-point)
		(setq para-end (point))
	      (beginning-of-line)
	      (setq para-end (point))
	      )
	    (fill-region para-begin para-end)
	    )
	)
      )
    )
  )
  
(defun eif-newline ()
  "Insert a newline and indent the new line."
  (interactive)
  (insert "\n")
  (eif-indent-line)
  )

(defun eif-indent-and-newline ()
  "Indent the current line. Insert a newline and indent the new line."
  (interactive)
  (eif-indent-line)
  (insert "\n")
  (eif-indent-line)
  )

(defun eif-indent-line (&optional whole-exp)
  "Indent the current line as Eiffel code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not implemented yet)."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((indent (eif-calc-indent)))
      (if (not (= indent (current-column)))
	  (progn
	    (delete-horizontal-space)
	    (indent-to indent)
	    )
	)
      )
    )
  (skip-chars-forward " \t"))

(defun eif-move-to-prev-non-blank ()
  "Moves point to previous line excluding blank lines. 
Returns t if successful, nil if not."
  (beginning-of-line)
  (re-search-backward "^[ \t]*[^ \t\n]" nil t))

(defvar eif-last-feature-level-indent -1)
(defvar eif-feature-level-indent-regexp nil)
(defun eif-in-paren-expression ()
  "Determine if we are inside of a parenthesized expression"
  (interactive)
  (let ((paren-count 0) (limit 0))
    (save-excursion
      (if (= eif-last-feature-level-indent (eif-feature-level-indent-m))
	  (setq limit 
		(re-search-backward eif-feature-level-indent-regexp nil t))
	(setq eif-last-feature-level-indent (eif-feature-level-indent-m))
	(setq eif-feature-level-indent-regexp
	      (concat "^" (make-string eif-last-feature-level-indent ? ) 
		      "[^ \t\n]")
	      )
	(setq limit 
	      (or (re-search-backward eif-feature-level-indent-regexp nil t)
		  0)
	      )
	)
      )
    (save-excursion
      (while (re-search-backward "[][()]" limit t)
	(if (looking-at "[[(]")
	    (setq paren-count (1+ paren-count))
	  (setq paren-count (1- paren-count))
	  )
	)
      )
    paren-count
    )
)

(defun eif-manifest-array-indent ()
  "Determine if we are inside of a manifest array"
  (interactive)
  (let ((paren-count 0) (indent nil)
	(limit 0))
    (save-excursion
      (if (= eif-last-feature-level-indent (eif-feature-level-indent-m))
	  (setq limit 
		(re-search-backward eif-feature-level-indent-regexp nil t))
	(setq eif-last-feature-level-indent (eif-feature-level-indent-m))
	(setq eif-feature-level-indent-regexp
	      (concat "^" (make-string eif-last-feature-level-indent ? ) 
		      "[^ \t\n]")
	      )
	(setq limit 
	      (or (re-search-backward eif-feature-level-indent-regexp nil t)
		  0)
	      )
	)
      )
    (save-excursion
      (while (and (<= paren-count 0) (re-search-backward "<<\\|>>" nil t))
	(if (looking-at "<<")
	    (setq paren-count (1+ paren-count))
	  (setq paren-count (1- paren-count))
	  )
	)
      (if (> paren-count 0) 
	  (let ((eol (save-excursion (end-of-line) (point))))
	    (setq indent 
		  (or (and (re-search-forward "[^< \t]" eol t)
			   (1- (current-column)))
		      (+ (current-column) 2)
		      )
		  )
	    )
	)
      )
    indent
    )
  )

(defun eif-manifest-array-start ()
  "Determine the indentation of the statement containing a manifest array"
  (interactive)
  (let ((paren-count 0) (indent nil)
	(limit 0))
    (save-excursion
      (if (= eif-last-feature-level-indent (eif-feature-level-indent-m))
	  (setq limit 
		(re-search-backward eif-feature-level-indent-regexp nil t))
	(setq eif-last-feature-level-indent (eif-feature-level-indent-m))
	(setq eif-feature-level-indent-regexp
	      (concat "^" (make-string eif-last-feature-level-indent ? ) 
		      "[^ \t\n]")
	      )
	(setq limit 
	      (or (re-search-backward eif-feature-level-indent-regexp nil t)
		  0)
	      )
	)
      )
    (save-excursion
      (while (and (<= paren-count 0) (re-search-backward "<<\\|>>" nil t))
	(if (looking-at "<<")
	    (setq paren-count (1+ paren-count))
	  (setq paren-count (1- paren-count))
	  )
	)
      (if (> paren-count 0) 
	  (let ((limit (progn (end-of-line) (point))))
	    (beginning-of-line)
	    (if (re-search-forward "^[ \t]*<<" limit t)
		(setq indent (- (current-column) 2 eif-indent-increment))
	      (re-search-forward "^[ \t]*" limit t)
	      (setq indent (current-column))
	      )
	    )
	)
      )
    indent
    )
  )

;; ----------------------------------------------------------------------
;; The function below is derived from "eif-mult-fmt.el"
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Copyright (C) 1990 Bob Weiner, Motorola Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;; ----------------------------------------------------------------------

(defun eif-indent-multi-line (&optional parse-start)
  "Return integer giving appropriate indentation for current Eiffel code
line between parentheses or double quotes, otherwise -1.  Optional
PARSE-START is buffer position at which to begin parsing, default is to begin
at the feature enclosing or preceding point."
  (let ((eif-opoint (point))
	(indent-point (progn (beginning-of-line) (point)))
	(eif-ind-val -1)
	(eif-in-str nil)
	(eif-paren-depth 0)
	(retry t)
	state
	;; setting this to a number inhibits calling hook
	last-sexp containing-sexp)
    (if parse-start
	(goto-char parse-start)
      (eif-beginning-of-feature))
    ;; Find outermost containing sexp
    (while (< (point) indent-point)
      (setq state (parse-partial-sexp (point) indent-point 0)))
    ;; Find innermost containing sexp
    (while (and retry
		state
		(> (setq eif-paren-depth (elt state 0)) 0))
      (setq retry nil)
      (setq last-sexp (elt state 2))
      (setq containing-sexp (elt state 1))
      ;; Position following last unclosed open.
      (goto-char (1+ containing-sexp))
      ;; Is there a complete sexp since then?
      (if (and last-sexp (> last-sexp (point)))
	  ;; Yes, but is there a containing sexp after that?
	  (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
	    (if (setq retry (car (cdr peek))) (setq state peek)))))
    (if retry
	nil
      ;; Innermost containing sexp found
      (goto-char (1+ containing-sexp))
      (if (not last-sexp)
	  ;; indent-point immediately follows open paren.
	  nil
	;; Find the start of first element of containing sexp.
	(parse-partial-sexp (point) last-sexp 0 t)
	(cond ((looking-at "\\s(")
	       ;; First element of containing sexp is a list.
	       ;; Indent under that list.
	       )
	      ((> (save-excursion (forward-line 1) (point))
		  last-sexp)
	       ;; This is the first line to start within the containing sexp.
	       (backward-prefix-chars))
	      (t
	       ;; Indent beneath first sexp on same line as last-sexp.
	       ;; Again, it's almost certainly a routine call.
	       (goto-char last-sexp)
	       (beginning-of-line)
	       (parse-partial-sexp (point) last-sexp 0 t)
	       (backward-prefix-chars))))
      (setq eif-ind-val (current-column))
      )
    ;; Point is at the point to indent under unless we are inside a string.
    (setq eif-in-str (elt state 3))
    (goto-char eif-opoint)
    (if (not eif-in-str)
	nil
      ;; Inside a string, indent 1 past string start
      (setq eif-paren-depth 1);; To account for being inside string
      (save-excursion
	(if (re-search-backward "\"" nil t)
	    (setq eif-ind-val (1+ (current-column)))
	  (goto-char indent-point)
	  (if (looking-at "^[ \t]*[^ \t\n]")
	      (eif-move-to-prev-non-blank))
	  (skip-chars-forward " \t")
	  (setq eif-ind-val (current-column)))))
    (if (> eif-paren-depth 0) eif-ind-val -1)
    ))

(provide 'eiffel-mode)
;;; eiffel-mode.el ends here
