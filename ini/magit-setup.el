;; (add-to-load-path (concat elisp-directory "/pkgs/magit/etc/emacs/site-start.d/"))
(add-to-load-path (concat elisp-directory "/pkgs/magit/share/emacs/site-lisp"))

(autoload 'magit-status "magit" nil t)

(setq magit-git-executable "/usr/local/bin/git")

(global-set-key "\C-cgg" 'magit-status)
