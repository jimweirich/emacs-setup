(defun my-ruby-compile-hook ()
;;   (add-to-list 'compilation-error-regexp-alist 
;; 	       '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:"
;; 		 1 2))
;;   (add-to-list 'compilation-error-regexp-alist 
;; 	       '("^ *\\[?\\([^:\\n\\r]+\\):\\([0-9]+\\):in"
;; 		 1 2))
  (setq compile-command "rake"))

(add-hook 'ruby-mode-hook 'my-ruby-compile-hook)

;; run the current test function

(defun ruby-test-function ()
  "Test the current ruby function (must be runable via ruby <buffer> --name <test>)."
  (interactive)
  (let* ((funname (which-function))
	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname))))
    (compile (concat "ruby " (file-name-nondirectory (buffer-file-name)) " --name " fn))))

(defun ruby-test-file ()
  (interactive)
  (if (string-match "_test.rb$" buffer-file-name)
      (compile (concat "ruby " (file-name-nondirectory buffer-file-name)))
    (toggle-buffer)
    (compile (concat "ruby " (file-name-nondirectory buffer-file-name)))
    (toggle-buffer)))

(defun autotest ()
  (interactive)
  (let ((buffer (shell (concat "cd " (rails-root) ";autotest -rails"))))
    (compilation-shell-minor-mode)
    (define-key shell-mode-map "\C-c\C-a" 'autotest-switch)
    (comint-send-string buffer "autotest\n")))

(provide 'ruby-test)
