(autoload 'magit-status "magit" nil t)

(setq magit-git-executable "/usr/local/bin/git")

(global-set-key "\C-cgg" 'magit-status)
