;; Find the top of a project

(defconst project-top-default-top-files '(".git" "Rakefile" "rakelib" "lib" "config"))

(defun project-top (&rest top-files)
  "Return the file-name to the top of the project containing the current buffer."
  (project-top-from
   (project-top-starting-point)
   (if (null top-files)
       project-top-default-top-files
     (project-top-from (project-top-starting-point) top-files))))

(defun project-top-find-file (&rest args)
  "Return the path to the file given a relative path from the project top.
Usage: (project-top-find-file config/database.yml)
       (project-top-find-file config/database.yml starting-file)
       (project-top-file-file config/database.yml starting-file top-files)"
  (let ((file-path (apply 'project-top-path-to args)))
    (if (and file-path (file-exists-p file-path))
        (find-file file-path)
      (message (concat "Can't find file " (car args))))))

(defun project-top-path-to (file-from-top &rest args)
  "Return the path to the file given a relative path from the project top.
Usage:   (project-path-to config/database.yml)
         (project-path-to config/database.yml starting-file)
         (project-path-to config/database.yml starting-file top-files)"
  (let* ((from-path (project-top-arg args (project-top-starting-point)))
         (top-files (project-top-arg (cdr args) project-top-default-top-files))
         (path (project-top-from from-path top-files)))
    (if path (concat (file-name-as-directory path) file-from-top))))

(defun project-top-from (path top-files)
  "Return the path to the top of the project containing path.
Returns nil if no project top found.  Use the list of possible
top-files to determine the top of the project."
  (cond ((null path) nil)
        ((project-top-at-top-p path top-files) path)
        (t (project-top-from (project-top-parent path) top-files))))

(defun project-top-at-top-p (path top-files)
  "Is the path at the top level of a project?"
  (cond ((null top-files) nil)
        ((project-top-contains-p path (car top-files)) t)
        (t (project-top-at-top-p path (cdr top-files)))))

(defun project-top-parent (path)
  "Return the parent directory of path.  The parent of / is nil."
  (cond ((string-equal "/" path) nil)
        (t (file-name-directory (directory-file-name path))) )  )

(defun project-top-contains-p (dir file)
  (file-exists-p (concat (file-name-as-directory dir) file)))

(defun project-top-arg (opt default-value)
  "Return the car of opt if it is not null, otherwise return the default value."
  (if (null opt) default-value (car opt)))

(defun project-top-starting-point ()
  (let ((cur (buffer-file-name (current-buffer))))
    (if cur
        cur 
      (directory-file-name (substring (pwd) 10)))))

(provide 'project-top)
