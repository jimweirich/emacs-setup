(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(defun mgbc () (interactive) (mo-git-blame-current))
(defun mgbf () (interactive) (mo-git-blame-file))

(defun jw-mo-git-blame (arg)
  (interactive "P")
  (if (null arg)
      (mo-git-blame-current)
    (mo-git-blame-file)))
