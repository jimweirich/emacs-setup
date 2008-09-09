;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-visit-source
;;; Purpose: Visit Source Implementation
;;; ==================================================================

;;; Source File Visiting ---------------------------------------------

(defun jw-current-line ()
  "Return the current line."
  (let
      ((bol (save-excursion (beginning-of-line)(point)))
       (eol (save-excursion (end-of-line)(point))))
    (dbg-inline "Current line: " (buffer-substring bol eol)) ))

(defun jw-generate-candidates (line)
  "Extract a list of file/line pairs from the given line of text."
  (let*
      ((unix_fn "[^ \t\n\r\"'([<{]+")
       (dos_fn  "[a-zA-Z]:[^\t\n\r\"'([<{]+")
       (flre (concat "\\(" unix_fn "\\|" dos_fn "\\):\\([0-9]+\\)"))
       (start nil)
       (result nil))
    (while (string-match flre line start)
      (setq start (match-end 0))
      (setq result
            (cons (list 
                   (substring line (match-beginning 1) (match-end 1))
                   (string-to-int (substring line
                                             (match-beginning 2)
                                             (match-end 2))))
                  result)))
    (dbg "Generated Candidates: " result)
    result))

(defun jw-vs-parent-dir (path)
  "Return parent directory of path.
The parent of / is nil."
  (cond ((string-equal "/" path) nil)
        (t (file-name-directory (directory-file-name path))) ))

(defun jw-try-candidate (candidate)
  "Return a modified candidate that matches a real file, nil otherwise."
  (let ((candidate-file (car candidate)))
    (cond ((not (file-name-absolute-p candidate-file))
           (jw-try-candidate-in candidate (file-name-as-directory default-directory)))
          ((file-readable-p candidate-file) candidate)
          (t nil) )))

(defun jw-try-candidate-in (candidate dir)
  "Try the candidate dir and all its parents."
  (dbg "Trying candidate: " candidate " in " dir)
  (let ((candidate-file (concat dir (car candidate))))
    (cond ((null dir) nil)
          ((file-readable-p candidate-file)
           (list candidate-file (cadr candidate)))
          (t (jw-try-candidate-in candidate (jw-vs-parent-dir dir))) )))

(defun jw-select-file-line (candidates)
  "Select a file/line candidate that references an existing file."
  (cond ((null candidates) nil)
        ((jw-try-candidate (car candidates)))
        (t (jw-select-file-line (cdr candidates))) ))

(defun jw-visit-source ()
  "If the current line contains text like '../src/program.rb:34', visit 
that file in the other window and position point on that line."
  (interactive)
  (let* ((line (jw-current-line))
         (candidates (jw-generate-candidates line))
         (file-line (jw-select-file-line candidates)))
    (cond (file-line
           (find-file-other-window (car file-line))
           (goto-line (cadr file-line)) )
          (t 
           (error "No source location on line.")) )))

;;; Key Bindings -----------------------------------------------------

(global-set-key [f2] 'jw-visit-source)
