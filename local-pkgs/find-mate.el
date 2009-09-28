;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    find-mate
;;; Purpose: Find the C/C++ file mate for the current file.
;;; ==================================================================

;;; NOTE:
;;; This is a bunch of C/C++ functionality that I haven't used for
;;; quite some time.  If I were to revisit this, I'd look to see if
;;; there were some way of leveraging toggle.el functionality.

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

