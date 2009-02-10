;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-utils
;;; Purpose: Misc Utility Functions for Editing
;;; ==================================================================

;;; Utility Commands ===================================================

;;; The following are substiutes for when ^s/^q are not available

;;; The following is needed for the forth-mode stuff

(defun sleep-for-millisecs (n)
  (sleep-for (/ n 1000)))

;;; Finding errors in the shell

(defconst find-errors-pattern "^[][A-Za-z*-]*\\$")

(defun find-errors ()
  "Find the Errors in the current shell buffer."
  (interactive)
  (save-excursion 
    (re-search-backward find-errors-pattern)
    (set-mark (point))
    (re-search-backward find-errors-pattern)
    (find-errors-in-region)
    (next-error '(4)))
 )

(defun find-errors-in-region ()
  "Use highlighted errors as compile errors."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end))
  (goto-char (point-max))
  (let ((oldbuf (get-buffer "*shell errors*")))
    (if oldbuf (kill-buffer oldbuf)))
  (let ((outbuf (get-buffer-create "*shell errors*")) )
    (switch-to-buffer outbuf)
    (insert-string "\n\n\n\n")
    (yank)
    (setq compilation-last-buffer outbuf)
    (compilation-mode) ))

(global-set-key "\C-ce" 'find-errors)

(defun refresh ()
  "Refresh the current buffer from disk"
  (interactive)
  (revert-buffer t t) )

(defun old-refresh ()
  "Refresh the current buffer from disk"
  (interactive)
  (let ((fn (buffer-file-name))
	(lineno (count-lines 1 (point))))
    (kill-buffer (current-buffer))
    (find-file fn)
    (goto-line lineno)))

(defun undos ()
  "Remove the <CR> in DOS files"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-string "" "") 
    (goto-char (point-min))
    (replace-string "" "") ))

(defun unansi ()
  "Remove the ansi markup in files"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "[\\[0-9;\\]*m" "") 
    ))

(defun unhtml ()
  "Remove the HTML tags in a file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "<[^>]*>" "") 
    (goto-char (point-min))
    (replace-string "&nbsp;" " ")
    (goto-char (point-min))
    (replace-string "&lt;" "<")
    (goto-char (point-min))
    (replace-string "&gt;" ">")
    (goto-char (point-min))
    (replace-string "&amp;" "&") ))

(defun af ()
  "Toggle Auto-Fill Mode"
  (interactive)
  (auto-fill-mode))

(defun dt ()
  "Insert the Date."
  (interactive)
  (insert-string (current-date-string)))

(defun current-date-string ()
  (let ((time-string (current-time-string)))
    (concat (substring time-string 8 10)
	    "/"
	    (substring time-string 4 7)
	    "/"
	    (substring time-string 22 24))))

(defun current-year-string ()
  (let ((time-string (current-time-string)))
    (concat (substring time-string 20 24))))

(defun encrypt (password)
  "Encrypt/Decrypt the current buffer"
  (interactive "sPassword: ")
  (call-process-region
   (point-min) (point-max)
   "crypt"
   t
   (current-buffer)
   nil
   password))


(defun cmt-insert-bar-dots ()
  (interactive)
  (cmt-insert-bar-line ". "))
;;;(global-set-key "\C-c." 'cmt-insert-bar-dots)

(defun cmt-insert-bar-heavy ()
  (interactive)
  (cmt-insert-bar-line "="))
(global-set-key "\C-c=" 'cmt-insert-bar-heavy)

(defun cmt-insert-bar-hash ()
  (interactive)
  (cmt-insert-bar-line "#"))
(global-set-key "\C-c#" 'cmt-insert-bar-hash)

(defun cmt-insert-bar-light ()
  (interactive)
  (cmt-insert-bar-line "-"))
(global-set-key "\C-c-" 'cmt-insert-bar-light)

(defun cmt-insert-bar-star ()
  (interactive)
  (cmt-insert-bar-line "*"))
(global-set-key "\C-c*" 'cmt-insert-bar-star) 

(defun cmt-insert-bar-hash ()
  (interactive)
  (cmt-insert-bar-line "#"))
(global-set-key "\C-c#" 'cmt-insert-bar-hash)

(defun snip ()
  (interactive)
  (insert-string "--><--snip--><---")
  (cmt-insert-bar-light)
  (insert-string "\n"))

(defvar cmt-bar-column 70
  "Column to extend comment bars to")

(defun cmt-insert-bar-line (char)
  (end-of-line)
  (if (< (current-column) cmt-bar-column)
      (progn
	(if (> (current-column) 0)
	    (progn
	      (backward-char)
	      (if (looking-at (concat "[ \t" char "]"))
		  (end-of-line)
		(end-of-line)
		(insert-string " "))))
	(while (< (current-column) cmt-bar-column)
	  (insert-string char))))
  (while (and (> (current-column) cmt-bar-column)
	      (save-excursion 
		(backward-char)
		(looking-at char)))
    (backward-delete-char-untabify 1)))

;;; Shell Buffer Selection ===========================================

(defun multi-shell (n)
  (interactive "P")
  (cond ((null n) (mshell 0))		; no prefix
	((atom n) (mshell n))		; numeric prefix
	(t (mshell 1)) ))		; ^U prefix

(defun mshell (n)
    (let* ((shell-name (concat "*shell-" (number-to-string n) "*"))
	  (buf (get-buffer shell-name)))
      (if (null buf)
	  (progn
	    (setq buf (shell))
	    (rename-buffer shell-name)))
      (pop-to-buffer buf)
      (goto-char (point-max))
      (pop-to-buffer buf)
      (goto-char (point-max))
      ))

(global-set-key "\C-c " 'multi-shell)	; override the default binding here
(global-set-key [M-f1] (lambda () (interactive) (mshell 1)))
(global-set-key [M-f2] (lambda () (interactive) (mshell 2)))
(global-set-key [M-f3] (lambda () (interactive) (mshell 3)))
(global-set-key [M-f4] (lambda () (interactive) (mshell 4)))
(global-set-key [M-f5] (lambda () (interactive) (mshell 5)))
(global-set-key [M-f6] (lambda () (interactive) (mshell 6)))
(global-set-key [M-f7] (lambda () (interactive) (mshell 7)))
(global-set-key [M-f12] 'jw-select-gud-buffer)

(defun send-shell-command (str)
  "Send commands to a shell process"
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (insert-string str)
    (insert-string "\n")
    (set-marker pmark (point))
    (comint-send-string proc str)
    (comint-send-string proc "\n")))

(defun ttyemacs ()
  "Send the unix command to setup emacs mode"
  (interactive)
  (send-shell-command "TERM=emacs; stty -onlcr -echo"))

;;; Resource Editing

(defun display-host ()
  "Return the Display Host"
  (let ((disp (getenv "DISPLAY")))
    (if (equal (substring disp -2) ".0")
	(setq disp (substring disp 0 -2)))
    disp))

(defun get-resources ()
  "Get the Current X Resources from the X Server"
  (interactive)
  (let ((vfile (substitute-in-file-name 
		(concat "$HOME/.vue/"
			(display-host)
			"/home/vue.resources")))
	(cfile (substitute-in-file-name 
		(concat "$HOME/.vue/"
			(display-host)
			"/current/vue.resources")))
	(xfile (substitute-in-file-name "$HOME/.Xdefaults")))
    (cond ((file-writable-p vfile) (find-file vfile))
	  ((file-writable-p cfile) (find-file cfile))
	  ((file-writable-p xfile) (find-file xfile))
	  (t (message "Can not find X Resource file")))))
	
;  (shell-command "xrdb -q")

(defun save-resources ()
  "Save the file as the current X Resources on the X Server"
  (interactive)
  (save-buffer)
  (save-excursion
    (end-of-buffer)
    (shell-command-on-region 1 (point) "xrdb -load" nil)))


;;; Find Mate for Source/Header switching ============================

(defvar fm-header-extensions  '(".h" ".hh" ".hxx" ".hpp" ".H"))
(defvar fm-header-directories '("." "../include"))
(defvar fm-source-extensions  '(     ".cc" ".cxx" ".cpp" ".C" ".c"))
(defvar fm-source-directories '("." "../hcs" "../hcsLib"))

;;; fm-submatch ------------------------------------------------------
;;; Return the nth substring match (or "" if no match).

(defun fm-submatch (str n) 
  (if (match-beginning n)
      (substring str (match-beginning n) (match-end n))
    ""))

;;; fm-split-base ----------------------------------------------------
;;; A helper function for fm-split-path.  Given the directory and the
;;; file name, split the filename into base and extension.

(defun fm-split-basename (dir fn)
  (if (string-match "^\\(.+\\)\\(\\.[a-zA-Z0-9]+\\)$" fn)
      (list dir (fm-submatch fn 1) (fm-submatch fn 2))
    (list dir fn "")))

;;; fm-split-path ----------------------------------------------------
;;; Split a pathname into the directory, basename and extension.
;;; Returns a list of the form (dir base ext).  Missing parts will be
;;; the empty string (""). 

(defun fm-split-path (fn)
  "Split a path name into its component parts."
  (if (string-match "^\\(.*/\\)\\([^/]*\\)" fn)
      (fm-split-basename (fm-submatch fn 1) (fm-submatch fn 2))
    (fm-split-basename "" fn)))

;;; fm-try-a-buffer --------------------------------------------------
;;; Try buffer buf-name.  Return nil if not found.

(defun fm-try-a-buffer (buf-name)
  (let ((buf (get-buffer buf-name)))
    (if (null buf)
	()
      (switch-to-buffer-other-window buf)
      t)))

;;; fm-try-buffers ---------------------------------------------------
;;; Search for base.X in a currently loaded buffer.  Select the buffer
;;; and return non-nil if found.  Return nil if buffer not found.

(defun fm-try-buffers (base tails)
  (cond ((null tails) nil)
	((fm-try-a-buffer (concat base (car tails))))
	(t (fm-try-buffers base (cdr tails))) ))

;;; fm-try-path ------------------------------------------------------
;;; Try to find a file in the given path ending with one of the
;;; suffixs in tails.  Return non-nill if successful, nil otherwise.

(defun fm-try-path (path tails)
  (if (null tails) nil
    (let ((fn (expand-file-name (concat path (car tails)))))
      (if (file-exists-p fn) (progn (find-file-other-window fn) t)
	(fm-try-path path (cdr tails)) ) ) ) )

;;; fm-search-directories --------------------------------------------
;;; Try to find a file named base.X in any of the given directories.
;;; Return non-nill if successful, nil otherwise. 

(defun fm-search-directories (base tails dirs)
  (cond ((null dirs) nil)
	((fm-try-path (concat (car dirs) "/" base) tails))
	(t (fm-search-directories base tails (cdr dirs))) ))
	 
;;; fm-search -----------------------------------------------------------
;;; Search for a candidate for a file mate.  First search the loaded
;;; buffers.  Then try each directory in the directory list.  Finally
;;; create a new file in the current directory with the default tail. 

(defun fm-search (base tails dirs default)
  (cond ((fm-try-buffers base tails))
	((fm-search-directories base tails dirs))
	(t (find-file (concat base default))) ))	

;;; find-mate --------------------------------------------------------
;;; Find the mate to the file in the current buffer.

(defun find-mate ()
  "Find the Mate to the C/C++ Source/Header file"
  (interactive)
  (let* ((fn (buffer-file-name))
	 (split (fm-split-path fn))
	 (dir (car split))
	 (base (nth 1 split))
	 (tail (nth 2 split)))
    (cond ((null tail)(message (concat "Unknown File Type for " fn)))
	  ((member tail fm-header-extensions)
	   (fm-search base 
		      fm-source-extensions 
		      fm-source-directories
		      (car fm-source-extensions)))
	  ((member tail fm-source-extensions)
	   (fm-search base
		      fm-header-extensions
		      fm-header-directories
		      (car fm-header-extensions)))
	  (t (message (concat "Don't know how to handle file type " tail))) )))


(defun fm ()
  "Abbreviation for find-mate"
  (interactive)
  (find-mate))

(global-set-key "\M-o" 'fm)


;;; Common Programming Abbreviations and Shortcuts ===================

(defun ac ()
  "Add Comment for C++ mode"
  (interactive)
  (goto-char (point-min))
  (insert-string "// -*-Mode: c++; -*-\n"))

(defun acm ()
  "Add Comment for C++ mode and switch to c++ mode"
  (interactive)
  (ac)
  (c++-mode))

(defun apm ()
  "Add Initial Code for a perl file"
  (interactive)
  (goto-char (point-min))
  (insert-string "#!/bin/sh -- # -*- perl -*-\n")
  (insert-string "eval 'exec perl -S $0 ${1+\"$@\"}'\n")
  (insert-string "  if $runnning_under_a_shell;\n\n")
  (perl-mode))

(defun insert-jim-banner ()
  (interactive)
  (insert-string ">>>>> \"Jim\" == Jim Weirich <jim@weirichhouse.org> writes:\n"))
(defun jb () (interactive) (insert-jim-banner))


;;; Other mode abbreviations =========================================

(defun itm () "Abbreviation for Indented-Text-Mode"    (interactive) (indented-text-mode))
(defun mm  () "Abbreviation for Mail-Mode"             (interactive) (mail-mode))
(defun nrm () "Abbreviation for News-Reply-Mode"       (interactive) (news-reply-mode))
(defun pm  () "Abbreviation for Perl-Mode"             (interactive) (perl-mode))
(defun tm  () "Abbreviation for Tcl-Mode"              (interactive) (tcl-mode))
(defun ccm () "Abbreviation for C++-Mode"              (interactive) (c++-mode))
(defun cm  () "Abbreviation for C-Mode"                (interactive) (c-mode))
(defun rm  () "Abbreviation for Ruby-Mode"             (interactive) (ruby-mode))
(defun lim () "Abbreviation for Lisp-Interaction-Mode" (interactive) (lisp-interaction-mode))

;;; Mouse Mode Line Dragging ===========================================

(defsubst frame-coordinates (event)
  "Return frame-relative (x . y) coordinates for EVENT."
  (let ((edges (window-edges (posn-window event)))
	(position (posn-col-row event)))
    (cons (+ (car position) (car edges))
	  (+ (cdr position) (car (cdr edges))))))

(defun drag-mode-line (event)
  "Resize a window by dragging its mode line."
  (interactive "e")
  (let ((now (selected-window))
	(window (posn-window (event-start event)))
	(offset (-
		 (cdr (frame-coordinates (event-start event)))
		 (cdr (frame-coordinates (event-end event))))))
    (select-window (let ((edges (window-edges window)))
		     (window-at (car edges) (nth 3 edges))))
    (let* ((min-height (if (eq (selected-window) (minibuffer-window)) 1
			 window-min-height))
	   (y (max
	       (min offset (- (window-height window) min-height))
	       (- min-height (window-height)))))
      (if (eq (selected-window) (minibuffer-window))
	  (enlarge-window y)
	(select-window window)
	(shrink-window y)))
    (select-window now)))

(cond ((is-emacs-19-25)
       (define-key global-map [mode-line drag-mouse-1] 'drag-mode-line)) )

(defun jw-show-key-binding (key)
  (interactive "kEnter Key: ")
  (insert
   "("
   (prin1-to-string (key-binding key))
   ")" ))

(global-set-key "\C-ch" 'jw-show-key-binding)

;;; ==================================================================
;;; ANSI Terminal Clutter Zapper

(defun jw-zap-ansi-clutter () 
  (interactive)
  (re-search-forward "\\[[0-9;]*m")
  (replace-match "") )

(defun jw-zap-all-ansi ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while t (jw-zap-ansi-clutter))  ))

