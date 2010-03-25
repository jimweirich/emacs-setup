;;; Author: Jim Weirich
;;; Find in Parent Dir -- Functions for finding files in a project.

(provide 'find-in-parent-dir)

(defun jw-dir-contains-p (path file)
  "Does the directory path contain the given file name?"
  (file-readable-p (concat (file-name-as-directory path) file)))

(defun jw-dir-contains-any-p (path files)
  "Does the directory path contain any of the given files?"
  (cond ((null files) nil)
        ((jw-dir-contains-p path (car files)) t)
        (t (jw-dir-contains-any-p path (cdr files)))))

(defconst jw-project-top-level-files
  '("Rakefile" "config/database.yml"))

(defun jw-project-top-p (path)
  "Are we at the top of a project?"
  (jw-dir-contains-any-p path jw-project-top-level-files))

(defun jw-root-p (path)
  "Is this the root of the file system?"
  (string-equal "/" path))

(defun jw-parent-dir (path)
  "Return the parent directory of path.  The parent of / is nil."
  (cond ((jw-root-p path) nil)
        (t (file-name-directory (directory-file-name path))) ))

;;; Public functions -------------------------------------------------

(defun jw-find-project-top (path)
  "Find the top level directory of a project starting with path.
Return original path if not project top not found."
  (setq dir path)
  (while (and dir (not (jw-project-top-p dir)))
    (setq dir (jw-parent-dir dir)))
  (if dir
      (file-name-as-directory (directory-file-name dir))
    path))

(defun jw-find-in-parent-dir (path file)
  "Find an improper parent of path that contains file."
  (while (and path
              (not (jw-dir-contains-p path file)))
    (setq path (jw-parent-dir path)))
  (if path
      (concat (file-name-as-directory path) file)
    nil))

(defun jw-project-env-file (path)
  "Find the project environment shell file, nil if none found."
  (jw-find-in-parent-dir path ".project_env.rc"))
