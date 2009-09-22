;;; Paredit setup

(require 'paredit)

(defun use-paredit ()
  (paredit-mode +1))

(add-hook 'emacs-lisp-mode-hook 'use-paredit)
(add-hook 'lisp-mode-hook 'use-paredit)
(add-hook 'inferior-lisp-mode-hook 'use-paredit)
(add-hook 'clojure-mode-hook 'use-paredit)
(add-hook 'scheme-mode-hook 'use-paredit)


;;; Paredit overshadows the C-j bindings in lisp interaction mode.

(define-key lisp-interaction-mode-map [S-return] 'eval-print-last-sexp)
