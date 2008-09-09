;; Bare minimum of elisp to do rinari testing in
(add-to-list 'load-path (expand-file-name "lisp"))
(add-to-list 'load-path (expand-file-name "spec"))
(add-to-list 'load-path (expand-file-name ".."))
(add-to-list 'load-path (expand-file-name "../rhtml"))

;; not strictly necessary, but extremely annoying to live without:
(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(show-paren-mode 1)
(global-set-key "\C-\M-h" 'backward-kill-word)
(setq inhibit-startup-message t)


;; go!
(require 'rinari)
(require 'rinari-spec)

(behave "rinari")
(find-file "/home/phil/projects/rinari/test/sample/app/views/foo/bar.rhtml")
