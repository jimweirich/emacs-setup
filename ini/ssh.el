(defun jw-reload-ssh-env ()
  "Reload the SSH environment if it exists."
  (interactive)
  (let ((env-file (expand-file-name "~/.ssh/environment.el")))
    (if (file-readable-p env-file)
        (load-file env-file))))

(defun rssh () (interactive) (jw-reload-ssh-env))
