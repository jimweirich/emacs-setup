;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-cc
;;; Purpose: Setups for C/C++ Mode
;;; ==================================================================

;;; C++ Mode Setups ===========================================

;;(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
;;(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
;;(autoload 'java-mode "cc-mode" "Java Editing Mode" t)

(defun fix-cc-map-hookfunc ()
  (define-key c-mode-map "\e\C-h" 'backward-kill-word)
  (define-key c-mode-map "\C-C\C-h" 'c-mark-function))

(if (boundp 'c-mode-map)
    (fix-cc-map-hookfunc)
  (add-hook 'c-mode-hook 'fix-cc-map-hookfunc)
  (add-hook 'c++-mode-hook 'fix-cc-map-hookfunc))


;;; Java Mode Extras =================================================

(defun jw-sort-imports () 
  "Sort the import lines in a java file."
  (interactive)
  (beginning-of-line)
  (while (not (looking-at "import"))
    (previous-line 1))
  (search-backward-regexp "^\\w*$")
  (search-forward-regexp "^import")
  (beginning-of-line)
  (let ((here (point)))
    (search-forward-regexp "^\\w*$")
    (sort-lines nil here (point)) ) )

;;; C/C++ Mode Setups ==================================================

(defun jnw-substatement (langelem) (message "HI") 0)
;;;  (save-excursion
;;;    (beginning-of-line)
;;;    (if (looking-at "\\s-*{") 0 c-basic-offset)))

(setq jw-cc-style
      '((c-hanging-comment-starter-p . nil)
	(c-hanging-comment-ender-p . nil)
	(c-offsets-alist . ((string               . -1000)
			    (topmost-intro        . 0)
			    (topmost-intro-cont   . 0)
			    (inline-open          . 0)
			    (substatement         . +)     
			    (substatement-open    . 0)
			    (member-init-intro    . 0)
			    ))
	))

(setq jw-java-style
      '("jw-cc"
	(c-offsets-alist . ((string . -1000)
			    (member-init-intro . 0)
			    (topmost-intro-cont . +)
			    ))
	))

(defun jw-cc-define-styles ()
  (c-add-style "jw-cc" jw-cc-style)
  (c-add-style "jw-java" jw-java-style)
  (c-set-style "jw-cc") )

(add-hook 'c-mode-common-hook 'jw-cc-define-styles)
(add-hook 'c-mode-hook     '(lambda () (c-set-style "jw-cc")) )
(add-hook 'c++-mode-hook   '(lambda () (c-set-style "jw-cc")) )
(add-hook 'java-mode-hook  '(lambda () (c-set-style "jw-java")) )
(add-hook 'java-mode-hook  '(lambda () (setq c-access-key nil)))

;(add-hook 'java-mode-hook  '(lambda () (require 'jde)))


;;; CC Aid Autoloads -------------------------------------------------

(autoload 'ccaid-new-class               "cc-aid" "Create a new class" t)
(autoload 'tempo-template-file-comments  "cc-aid" "Add File comments" t)
(autoload 'ccaid-add-header-protection   "cc-aid" "Add header protection" t)
(autoload 'ccaid-add-mode-comment        "cc-aid" "Add a C++ mode comment" t)
(autoload 'ccaid-add-method-separator    "cc-aid" "Add a Method Separator" t)
(autoload 'ccaid-create-implementation   "cc-aid" "Create an implementation body" t)
(autoload 'jw-cc-insert-template    "cc-templates" "Insert a C++ Template")
(autoload 'jw-java-insert-template  "java-templates" "Insert a Java Template")

(defun ccaid-init ()
  (define-key c++-mode-map "\C-cnc" 'ccaid-new-class)
  (define-key c++-mode-map "\C-cnd" 'tempo-template-file-comments)
  (define-key c++-mode-map "\C-cnh" 'ccaid-add-header-protection)
  (define-key c++-mode-map "\C-cnm" 'ccaid-add-mode-comment)
  (define-key c++-mode-map "\C-cnv" 'ccaid-add-method-separator)
  (define-key c++-mode-map "\C-cnx" 'ccaid-create-implementation)
  (define-key c++-mode-map "\C-ci"  'jw-cc-insert-template)
  (define-key java-mode-map "\C-ci"  'jw-java-insert-template)
  (define-key java-mode-map "\C-cs"  'jw-sort-imports)
  )

(add-hook 'c-mode-common-hook 'ccaid-init)
  
(global-set-key "\C-cnm" 'ccaid-add-mode-comment)

