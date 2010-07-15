;; Magit setup

(add-to-load-path (concat elisp-directory "/pkgs/magit/share/emacs/site-lisp"))
(autoload 'magit-status "magit" nil t)
(setq magit-git-executable "/usr/local/bin/git")
