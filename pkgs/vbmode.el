;; visual-basic-mode.el --- A mode for editing Visual Basic programs.

;; Copyright (C) 1996, Fred White <fwhite at std dot com>

;; Author: Fred White
;; Version: 1.3 (May 1, 1996)
;; Keywords: languages basic

;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.


;; Purpose of this package:
;;  This is a mode for editing programs written in The World's Most
;;  Successful Programming Language.  It features automatic
;;  indentation, font locking, keyword capitalization, and some minor
;;  convenience functions.

;; Installation instructions
;;  Put visual-basic-mode.el somewhere in your path, compile it, and add the
;;  following to your init file:

;;  (setq any mode-customization variables you want here)
;;  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
;;  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" . 
;;                                  visual-basic-mode)) auto-mode-alist))

;; Of course, under Windows 3.1, you'll have to name this file
;; something shorter than visual-basic-mode.el

;; Revisions:
;; 1.0 18-Apr-96  Initial version
;; 1.1 Accomodate emacs 19.29+ font-lock-defaults
;;     Simon Marshall <Simon.Marshall@esrin.esa.it>
;  1.2 Rename to visual-basic-mode
;; 1.3 Fix some indentation bugs.


;; Known bugs:
;;  Doesn't know about ":" separated stmts
;;  Doesn't know about single-line IF stmts


;; todo:
;;  fwd/back-compound-statement
;;  completion over OCX methods and properties.
;;  ensure Then at the end of IF statements.
;;  IDE integration
;;  etc.


(provide 'visual-basic-mode)

(defvar visual-basic-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar visual-basic-winemacs-p (string-match "Win-Emacs" (emacs-version)))
(defvar visual-basic-win32-p (eq window-system 'win32))

;; Variables you may want to customize.
(defvar visual-basic-mode-indent 2 "*Default indentation per nesting level")
(defvar visual-basic-fontify-p t "*Whether to fontify Basic buffers.")
(defvar visual-basic-capitalize-keywords-p t
  "*Whether to capitalize BASIC keywords.")
(defvar visual-basic-wild-files "*.frm *.bas *.cls"
  "*Wildcard pattern for BASIC source files")
(defvar visual-basic-ide-pathname nil
  "*The full pathname of your Visual Basic exe file, if any.")


(defvar visual-basic-keywords-to-highlight
  '("Dim" "If" "Then" "Else" "ElseIf" "End If")
  "*A list of keywords to highlight in Basic mode, or T, meaning all keywords")

(defvar visual-basic-defn-templates
  (list "Public Sub ()\nEnd Sub\n\n"
	"Public Function () As Variant\nEnd Function\n\n"
	"Public Property Get ()\nEnd Property\n\n")
  "*List of function templates though which visual-basic-new-sub cycles.")



(defvar visual-basic-mode-syntax-table nil)
(if visual-basic-mode-syntax-table
    ()
  (setq visual-basic-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" visual-basic-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" visual-basic-mode-syntax-table)
  (modify-syntax-entry ?_ "w" visual-basic-mode-syntax-table))


(defvar visual-basic-mode-map nil)
(if visual-basic-mode-map
    ()
  (setq visual-basic-mode-map (make-sparse-keymap))
  (define-key visual-basic-mode-map "\t" 'visual-basic-indent-line)
  (define-key visual-basic-mode-map "\r" 'visual-basic-newline-and-indent)
  (define-key visual-basic-mode-map "\M-\C-a" 'visual-basic-beginning-of-defun)
  (define-key visual-basic-mode-map "\M-\C-e" 'visual-basic-end-of-defun)
  (define-key visual-basic-mode-map "\M-\C-h" 'visual-basic-mark-defun)
  (define-key visual-basic-mode-map "\M-\C-\\" 'visual-basic-indent-region)
  (define-key visual-basic-mode-map "\M-q" 'visual-basic-fill-or-indent)
  (cond (visual-basic-winemacs-p
	 (define-key visual-basic-mode-map '(control C) 'visual-basic-start-ide))
	(visual-basic-win32-p
	 (define-key visual-basic-mode-map (read "[?\\S-\\C-c]") 'visual-basic-start-ide)))
  (if visual-basic-xemacs-p
      (progn
	(define-key visual-basic-mode-map "\M-G" 'visual-basic-grep)
	(define-key visual-basic-mode-map '(meta backspace) 'backward-kill-word)
	(define-key visual-basic-mode-map '(control meta /) 'visual-basic-new-sub))))


;; These abbrevs are valid only in a code context.
(defvar visual-basic-mode-abbrev-table nil)

(defvar visual-basic-mode-hook ())


;; Is there a way to case-fold all regexp matches?

(defconst visual-basic-defun-start-regexp
  (concat
   "^[ \t]*\\([Pp]ublic \\|[Pp]rivate \\|[Ss]tatic \\)*"
   "\\([Ss]ub\\|[Ff]unction\\|[Pp]roperty +[GgSsLl]et\\|[Tt]ype\\)"
   "[ \t]+\\(\\w+\\)[ \t]*(?"))

(defconst visual-basic-defun-end-regexp
  "^[ \t]*[Ee]nd \\([Ss]ub\\|[Ff]unction\\|[Pp]roperty\\|[Tt]ype\\)")


;; Includes the compile-time #if variation.
(defconst visual-basic-if-regexp "^[ \t]*#?[Ii]f")
(defconst visual-basic-else-regexp "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
(defconst visual-basic-endif-regexp "[ \t]*#?[Ee]nd[ \t]*[Ii]f")

(defconst visual-basic-continuation-regexp "^.*\\_[ \t]*$")
(defconst visual-basic-label-regexp "^[ \t]*[a-zA-Z0-9_]+:$")

(defconst visual-basic-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase")
(defconst visual-basic-case-regexp "^[ \t]*[Cc]ase")
(defconst visual-basic-select-end-regexp "^[ \t]*[Ee]nd[ \t]+[Ss]elect")

(defconst visual-basic-for-regexp "^[ \t]*[Ff]or\\b")
(defconst visual-basic-next-regexp "^[ \t]*[Nn]ext\\b")

(defconst visual-basic-do-regexp "^[ \t]*[Dd]o\\b")
(defconst visual-basic-loop-regexp "^[ \t]*[Ll]oop\\b")

(defconst visual-basic-while-regexp "^[ \t]*[Ww]hile\\b")
(defconst visual-basic-wend-regexp "^[ \t]*[Ww]end\\b")

(defconst visual-basic-with-regexp "^[ \t]*[Ww]ith\\b")
(defconst visual-basic-end-with-regexp "^[ \t]*[Ee]nd[ \t]+[Ww]ith\\b")

(defconst visual-basic-blank-regexp "^[ \t]*$")
(defconst visual-basic-comment-regexp "^[ \t]*\\s<.*$")


;; This is some approximation of the set of reserved words in Visual Basic.
(defconst visual-basic-all-keywords
  '("Aggregate" "And" "App" "AppActivate" "Application" "Array" "As"
    "Asc" "AscB" "Atn" "Beep" "BeginTrans" "Boolean" "ByVal" "CBool" "CByte" "CCur"
    "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr" "CVErr" "CVar" "Call"
    "Case" "ChDir" "ChDrive" "Character" "Choose" "Chr" "ChrB"
    "ClassModule" "Clipboard" "Close" "Collection" "Column" "Columns"
    "Command" "CommitTrans" "CompactDatabase" "Component" "Components"
    "Const" "Container" "Containers" "Cos" "CreateDatabase" "CreateObject"
    "CurDir" "Currency" "DBEngine" "DDB" "Data" "Database" "Databases"
    "Date" "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue" "Day"
    "Debug" "Declare" "Deftype" "DeleteSetting" "Dim" "Dir" "Do" "Domain"
    "Double" "Dynaset" "EOF" "Each" "Else" "End" "Environ" "Erase" "Err"
    "Error" "Exit" "Exp" "FV" "False" "Field" "Fields" "FileAttr"
    "FileCopy" "FileDateTime" "FileLen" "Fix" "Font" "For" "Form"
    "FormTemplate" "Format" "Forms" "FreeFile" "FreeLocks" "Function"
    "Get" "GetAllSettings" "GetAttr" "GetObject" "GetSetting" "GoSub"
    "GoTo" "Group" "Groups" "Hex" "Hour" "IIf" "IMEStatus" "IPmt" "IRR"
    "If" "InStr" "Input" "Int" "Integer" "Is" "IsArray" "IsDate" "IsEmpty"
    "IsError" "IsMissing" "IsNull" "IsNumeric" "IsObject" "Kill" "LBound"
    "LCase" "LOF" "LSet" "LTrim" "Left" "Len" "Let" "Like" "Line" "Load"
    "LoadPicture" "LoadResData" "LoadResPicture" "LoadResString" "Loc"
    "Lock" "Log" "Long" "Loop" "MDIForm" "MIRR" "Me" "MenuItems"
    "MenuLine" "Mid" "Minute" "MkDir" "Month" "MsgBox" "NPV" "NPer" "Name"
    "New" "Next" "Now" "Oct" "On" "Open" "OpenDatabase" "Operator"
    "Option" "PPmt" "PV" "Parameter" "Parameters" "Partition" "Picture"
    "Pmt" "Print" "Printer" "Printers" "Private" "ProjectTemplate"
    "Properties" "Public" "Put" "QBColor" "QueryDef" "QueryDefs"
    "RSet" "RTrim" "Randomize" "Rate" "ReDim" "Recordset" "Recordsets"
    "RegisterDatabase" "Relation" "Relations" "Rem" "RepairDatabase"
    "Reset" "Resume" "Return" "Right" "RmDir" "Rnd" "Rollback" "RowBuffer"
    "SLN" "SYD" "SavePicture" "SaveSetting" "Screen" "Second" "Seek"
    "SelBookmarks" "Select" "SelectedComponents" "SendKeys" "Set"
    "SetAttr" "SetDataAccessOption" "SetDefaultWorkspace" "Sgn" "Shell"
    "Sin" "Single" "Snapshot" "Space" "Spc" "Sqr" "Static" "Stop" "Str"
    "StrComp" "StrConv" "String" "Sub" "SubMenu" "Switch" "Tab" "Table"
    "TableDef" "TableDefs" "Tan" "Then" "Time" "TimeSerial" "TimeValue"
    "Timer" "To" "Trim" "True" "Type" "TypeName" "UBound" "UCase" "Unload"
    "Unlock" "Val" "VarType" "Verb" "Weekday" "Wend"
    "While" "Width" "With" "Workspace" "Workspaces" "Write" "Year"))


(defun visual-basic-word-list-regexp (keys)
  (let ((re "\\b\\(")
	(key nil))
    (while keys
      (setq key (car keys)
	    keys (cdr keys))
      (setq re (concat re key (if keys "\\|" ""))))
    (concat re "\\)\\b")))

(defun visual-basic-keywords-to-highlight ()
  (if (eq visual-basic-keywords-to-highlight t)
      visual-basic-all-keywords
    visual-basic-keywords-to-highlight))


(defvar visual-basic-font-lock-keywords
  (list
   ;; Names of functions.
   (list visual-basic-defun-start-regexp 3 'font-lock-function-name-face)

   ;; Statement labels
   (cons visual-basic-label-regexp 'font-lock-keyword-face)

   ;; Case values
   ;; String-valued cases get font-lock-string-face regardless.
   (list "^[ \t]*[Cc]ase[ \t]+\\([^'\n]+\\)" 1 'font-lock-keyword-face t)

   ;; Any keywords you like.
   (cons (visual-basic-word-list-regexp (visual-basic-keywords-to-highlight))
	 'font-lock-keyword-face)))


(put 'visual-basic-mode 'font-lock-keywords 'visual-basic-font-lock-keywords)

(defun visual-basic-mode ()
  "A mode for editing Microsoft Visual Basic programs.
Features automatic  indentation, font locking, keyword capitalization, 
and some minor convenience functions.
Commands:
\\{visual-basic-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map visual-basic-mode-map)
  (setq major-mode 'visual-basic-mode)
  (setq mode-name "Visual Basic")
  (set-syntax-table visual-basic-mode-syntax-table)

  (add-hook 'write-file-hooks 'visual-basic-untabify)

  (setq local-abbrev-table visual-basic-mode-abbrev-table)
  (if visual-basic-capitalize-keywords-p
      (progn
	(make-local-variable 'pre-abbrev-expand-hook)
	(add-hook 'pre-abbrev-expand-hook 'visual-basic-pre-abbrev-expand-hook)
	(abbrev-mode 1)))

  (make-local-variable 'comment-start)
  (setq comment-start "' ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "'+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'visual-basic-indent-line)

  (if visual-basic-fontify-p
      (visual-basic-enable-font-lock))

  (run-hooks 'visual-basic-mode-hook))


(defun visual-basic-enable-font-lock ()
  ;; Emacs 19.29 requires a window-system else font-lock-mode errs out.
  (cond ((or visual-basic-xemacs-p window-system)

	 ;; In win-emacs this sets font-lock-keywords back to nil!
	 (if visual-basic-winemacs-p
	     (font-lock-mode 1))

	 ;; Accomodate emacs 19.29+
	 ;; From: Simon Marshall <Simon.Marshall@esrin.esa.it>
	 (cond ((boundp 'font-lock-defaults)
		(make-local-variable 'font-lock-defaults)
		(setq font-lock-defaults '(visual-basic-font-lock-keywords)))
	       (t
		(make-local-variable 'font-lock-keywords)
		(setq font-lock-keywords visual-basic-font-lock-keywords)))

	 (if visual-basic-winemacs-p
	     (font-lock-fontify-buffer)
	   (font-lock-mode 1)))))


(defun visual-basic-construct-keyword-abbrev-table ()
  (if visual-basic-mode-abbrev-table
      nil
    (let ((words visual-basic-all-keywords)
	  (word nil)
	  (list nil))
      (while words
	(setq word (car words)
	      words (cdr words))
	(setq list (cons (list (downcase word) word) list)))

      (define-abbrev-table 'visual-basic-mode-abbrev-table list))))

;; Would like to do this at compile-time.
(visual-basic-construct-keyword-abbrev-table)


(defun visual-basic-in-code-context-p ()
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
      (null (buffer-syntactic-context))
    ;; Attempt to simulate buffer-syntactic-context
    ;; I don't know how reliable this is.
    (let* ((beg (save-excursion
		  (beginning-of-line)
		  (point)))
	   (list
	    (parse-partial-sexp beg (point))))
      (and (null (nth 3 list))		; inside string.
	   (null (nth 4 list))))))	; inside cocmment

(defun visual-basic-pre-abbrev-expand-hook ()
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
	(if (visual-basic-in-code-context-p)
	    visual-basic-mode-abbrev-table)))
	 
	

(defun visual-basic-newline-and-indent (&optional count)
  "Insert a newline, updating indentation."
  (interactive)
  (expand-abbrev)
  (save-excursion
    (visual-basic-indent-line))
  (call-interactively 'newline-and-indent))
  
(defun visual-basic-beginning-of-defun ()
  (interactive)
  (re-search-backward visual-basic-defun-start-regexp))

(defun visual-basic-end-of-defun ()
  (interactive)
  (re-search-forward visual-basic-defun-end-regexp))

(defun visual-basic-mark-defun ()
  (interactive)
  (beginning-of-line)
  (visual-basic-end-of-defun)
  (set-mark (point))
  (visual-basic-beginning-of-defun)
  (if visual-basic-xemacs-p
      (zmacs-activate-region)))

(defun visual-basic-indent-defun ()
  (interactive)
  (save-excursion
    (visual-basic-mark-defun)
    (call-interactively 'visual-basic-indent-region)))


(defun visual-basic-fill-long-comment ()
  "Fills block of comment lines around point."
  ;; Derived from code in ilisp-ext.el.
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-re "^[ \t]*\\s<+[ \t]*"))
      (if (looking-at comment-re)
	  (let ((fill-prefix
		 (buffer-substring
		  (progn (beginning-of-line) (point))
		  (match-end 0))))

	    (while (and (not (bobp))
			(looking-at visual-basic-comment-regexp))
	      (forward-line -1))
	    (if (not (bobp)) (forward-line 1))

	    (let ((start (point)))

	      ;; Make all the line prefixes the same.
	      (while (and (not (eobp))
			  (looking-at comment-re))
		(replace-match fill-prefix)
		(forward-line 1))

	      (if (not (eobp))
		  (beginning-of-line))

	      ;; Fill using fill-prefix
	      (fill-region-as-paragraph start (point))))))))


(defun visual-basic-fill-or-indent ()
  "Fill long comment around point, if any, else indent current definition."
  (interactive)
  (cond ((save-excursion
	   (beginning-of-line)
	   (looking-at visual-basic-comment-regexp))
	 (visual-basic-fill-long-comment))
	(t
	 (visual-basic-indent-defun))))


(defun visual-basic-new-sub ()
  "Insert template for a new subroutine. Repeat to cycle through alternatives."
  (interactive)
  (beginning-of-line)
  (let ((templates (cons visual-basic-blank-regexp
			 visual-basic-defn-templates))
	(tem nil)
	(bound (point)))
    (while templates
      (setq tem (car templates)
	    templates (cdr templates))
      (cond ((looking-at tem)
	     (replace-match (or (car templates)
				""))
	     (setq templates nil))))

    (search-backward "()" bound t)))


(defun visual-basic-untabify ()
  "Do not allow any tabs into the file"
  (if (eq major-mode 'visual-basic-mode)
      (untabify (point-min) (point-max)))
  nil)

(defun visual-basic-default-tag ()
  (if (and (not (bobp))
	   (save-excursion
	     (backward-char 1)
	     (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
	(e (save-excursion
	     (forward-word 1)
	     (point))))
    (buffer-substring s e)))

(defun visual-basic-grep (tag)
  "Search BASIC source files in current directory for tag."
  (interactive
   (list (let* ((def (visual-basic-default-tag))
		(tag (read-string
		      (format "Grep for [%s]: " def))))
	   (if (string= tag "") def tag))))
  (grep (format "grep -n %s %s" tag visual-basic-wild-files)))


;;; IDE Connection.

(defun visual-basic-buffer-project-file ()
  "Return a guess as to the project file associated with the current buffer."
  (car (directory-files (file-name-directory (buffer-file-name)) t "\\.vbp")))

(defun visual-basic-start-ide ()
  "Start Visual Basic (or your favorite IDE, (after Emacs, of course))
on the first project file in the current directory.
Note: it's not a good idea to leave Visual Basic running while you
are editing in emacs, since Visual Basic has no provision for reloading
changed files."
  (interactive)
  (let (file)
    (cond ((null visual-basic-ide-pathname)
	   (error "No pathname set for Visual Basic. See visual-basic-ide-pathname"))
	  ((null (setq file (visual-basic-buffer-project-file)))
	   (error "No project file found."))
	  ((fboundp 'win-exec)
	   (iconify-emacs)
	   (win-exec visual-basic-ide-pathname 'win-show-normal file))
	  ((fboundp 'start-process)
	   (iconify-frame (selected-frame))
	   (start-process "*VisualBasic*" nil visual-basic-ide-pathname file))
	  (t
	   (error "No way to spawn process!")))))



;;; Indentation-related stuff.

(defun visual-basic-indent-region (start end)
  "Perform visual-basic-indent-line on each line in region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
		(< (point) end))
      (if (not (looking-at visual-basic-blank-regexp))
	  (visual-basic-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
	 (zmacs-deactivate-region))
	((fboundp 'deactivate-mark)
	 (deactivate-mark))))



(defun visual-basic-previous-line-of-code ()
  (if (not (bobp))
      (forward-line -1))	; previous-line depends on goal column
  (while (and (not (bobp))
	      (or (looking-at visual-basic-blank-regexp)
		  (looking-at visual-basic-comment-regexp)))
    (forward-line -1)))


(defun visual-basic-find-original-statement ()
  ;; If the current line is a continuation from the previous, move
  ;; back to the original stmt.
  (let ((here (point)))
    (visual-basic-previous-line-of-code)
    (while (and (not (bobp))
		(looking-at visual-basic-continuation-regexp))
      (setq here (point))
      (visual-basic-previous-line-of-code))
    (goto-char here)))

(defun visual-basic-find-matching-stmt (open-regexp close-regexp)
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (visual-basic-previous-line-of-code)
      (visual-basic-find-original-statement)
      (cond ((looking-at close-regexp)
	     (setq level (+ level 1)))
	    ((looking-at open-regexp)
	     (setq level (- level 1)))))))

(defun visual-basic-find-matching-if ()
  (visual-basic-find-matching-stmt visual-basic-if-regexp visual-basic-endif-regexp))

(defun visual-basic-find-matching-select ()
  (visual-basic-find-matching-stmt visual-basic-select-regexp visual-basic-select-end-regexp))

(defun visual-basic-find-matching-for ()
  (visual-basic-find-matching-stmt visual-basic-for-regexp visual-basic-next-regexp))

(defun visual-basic-find-matching-do ()
  (visual-basic-find-matching-stmt visual-basic-do-regexp visual-basic-loop-regexp))

(defun visual-basic-find-matching-while ()
  (visual-basic-find-matching-stmt visual-basic-while-regexp visual-basic-wend-regexp))

(defun visual-basic-find-matching-with ()
  (visual-basic-find-matching-stmt visual-basic-with-regexp visual-basic-end-with-regexp))


(defun visual-basic-calculate-indent ()
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond ((or (looking-at visual-basic-defun-start-regexp)
		 (looking-at visual-basic-label-regexp)
		 (looking-at visual-basic-defun-end-regexp))
	     0)

	    ;; The outdenting stmts, which simply match their original.
	    ((or (looking-at visual-basic-else-regexp)
		 (looking-at visual-basic-endif-regexp))
	     (visual-basic-find-matching-if)
	     (current-indentation))

	    ;; All the other matching pairs act alike.
	    ((looking-at visual-basic-next-regexp) ; for/next
	     (visual-basic-find-matching-for)
	     (current-indentation))

	    ((looking-at visual-basic-loop-regexp) ; do/loop
	     (visual-basic-find-matching-do)
	     (current-indentation))

	    ((looking-at visual-basic-wend-regexp) ; while/wend
	     (visual-basic-find-matching-while)
	     (current-indentation))

	    ((looking-at visual-basic-end-with-regexp) ; with/end with
	     (visual-basic-find-matching-with)
	     (current-indentation))

	    ((looking-at visual-basic-select-end-regexp) ; select case/end select
	     (visual-basic-find-matching-select)
	     (current-indentation))

	    ;; A case of a select is somewhat special.
	    ((looking-at visual-basic-case-regexp)
	     (visual-basic-find-matching-select)
	     (+ (current-indentation) visual-basic-mode-indent))

	    (t
	     ;; Other cases which depend on the previous line.
	     (visual-basic-previous-line-of-code)

	     ;; Skip over label lines, which always have 0 indent.
	     (while (looking-at visual-basic-label-regexp)
	       (visual-basic-previous-line-of-code))

	     (cond 
	      ((looking-at visual-basic-continuation-regexp)
	       (visual-basic-find-original-statement)
	       ;; Indent continuation line under matching open paren,
	       ;; or else one word in.
	       (let* ((orig-stmt (point))
		      (matching-open-paren
		       (condition-case ()
			   (save-excursion
			     (goto-char original-point)
			     (beginning-of-line)
			     (backward-up-list 1)
			     ;; Only if point is now w/in cont. block.
			     (if (<= orig-stmt (point))
				 (current-column)))
			 (error nil))))
		 (cond (matching-open-paren
			(1+ matching-open-paren))
		       (t
			;; Else, after first word on original line.
			(back-to-indentation)
			(forward-word 1)
			(while (looking-at "[ \t]")
			  (forward-char 1))
			(current-column)))))
	      (t
	       (visual-basic-find-original-statement)

	       (let ((indent (current-indentation)))
		 ;; All the various +indent regexps.
		 (cond ((looking-at visual-basic-defun-start-regexp)
			(+ indent visual-basic-mode-indent))

		       ((or (looking-at visual-basic-if-regexp)
			    (looking-at visual-basic-else-regexp))
			(+ indent visual-basic-mode-indent))

		       ((or (looking-at visual-basic-select-regexp)
			    (looking-at visual-basic-case-regexp))
			(+ indent visual-basic-mode-indent))
			
		       ((or (looking-at visual-basic-do-regexp)
			    (looking-at visual-basic-for-regexp)
			    (looking-at visual-basic-while-regexp)
			    (looking-at visual-basic-with-regexp))
			(+ indent visual-basic-mode-indent))

		       (t
			;; By default, just copy indent from prev line.
			indent))))))))))

(defun visual-basic-indent-to-column (col)
  (let* ((bol (save-excursion
		(beginning-of-line)
		(point)))
	 (point-in-whitespace
	  (<= (point) (+ bol (current-indentation))))
	 (blank-line-p
	  (save-excursion
	    (beginning-of-line)
	    (looking-at visual-basic-blank-regexp))))

    (cond ((/= col (current-indentation))
	   (save-excursion
	     (beginning-of-line)
	     (back-to-indentation)
	     (delete-region bol (point))
	     (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
	   (end-of-line))
	  (point-in-whitespace
	   (back-to-indentation)))))


(defun visual-basic-indent-line ()
  "Indent current line for BASIC"
  (interactive)
  (visual-basic-indent-to-column (visual-basic-calculate-indent)))
