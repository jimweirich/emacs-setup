;; TODO -- need a license boiler-plate

;; Handy RHTML functions
;; (C) 2006 Phil Hagelberg

;; Ripped from the previous rhtml-mode, sorry about making it break
;; too :( -- pst

(defun rhtml-controller-name-from-view ()
  (concat (rails-root) 
	  "app/controllers/"
	   (file-name-nondirectory 
	    (expand-file-name "."))
	  "_controller.rb"))

(defun rhtml-find-action ()
  (interactive)
  (let ((action (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (find-file (rhtml-controller-name-from-view))
    (beginning-of-buffer)
    (search-forward-regexp (concat "def *" action))
    (recenter)))

(defun rinari-find-by-context ()
 (interactive)
 (mapc (lambda (rule) (if (string-match (car rule) (current-line))
		      (apply (cdr rule) (match-strings (current-line)))))
       ;; rules (warning; ALL matches will be acted upon, not just first!)
	'((":partial +=> +['\":]\\([a-zA-Z_]+\\)['\" ]" . rhtml-find-partial)
	  (":controller +=> +['\":]\\([a-zA-Z_]+\\)['\"].*:action +=> +['\":]\\([a-zA-Z_]+\\)['\" ]" . rinari-find-action)
	  (":action +=> +['\":]\\([a-zA-Z_]+\\)['\"]" . rinari-find-action))))

(defun rhtml-find-partial (partial)
  (interactive "MPartial: ")
  (find-file (concat "_" partial ".rhtml")))

;; utility functions

(defun current-line ()
  (save-excursion
    (beginning-of-line)
    (set-mark-command nil)
    (end-of-line)
    (buffer-substring-no-properties (mark) (point))))

(defun match-strings (string &optional n)
  (let* ((n (or n 1))
	 (this-match (match-string n string)))
    (when this-match
      (append (list this-match) (match-strings string (+ 1 n))))))

(provide 'rhtml-navigation)
