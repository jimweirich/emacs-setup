(defun face-at (regexp file)
  (save-excursion
    (find-file file)
    (switch-to-buffer (file-name-nondirectory file))
    (jit-lock-fontify-now)
    (let ((face (face-at-string regexp)))
      (kill-buffer (current-buffer))
      face)))

(defun face-at-string (regexp)
  (search-forward-regexp regexp)
  (backward-char)
  (get-text-property (point) 'face))


(provide 'behave-support)