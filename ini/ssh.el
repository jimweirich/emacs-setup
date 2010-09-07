(defun jw-reload-ssh-agent-env ()
  "Reload the SSH environment if it exists."
  (interactive)
  (let ((env-file (expand-file-name "~/.ssh/environment.el")))
    (if (file-readable-p env-file)
        (load-file env-file))))

(defun ra ()
  "Short-cut for jw-reload-ssh-agent-env."
  (interactive)
  (jw-reload-ssh-env))

