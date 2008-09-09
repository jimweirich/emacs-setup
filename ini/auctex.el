;;; ==================================================================
;;; Author: Jim Weirich
;;; File:   ini-auctex.el
;;; Purpose: Initialize auctex and general LaTeX stuff
;;; ==================================================================

(setq auc-path (if (at-home)
		   "/usr/lib/emacs/site-lisp/auctex/"
		 "/home/brute/work/hypchan/jnwtest/lib/lisp/auctex/"))
(add-to-load-path auc-path)

;; If the tex-site libarary is available

(if (lib-is-available "tex-site")
    (require 'tex-site))

(setq TeX-print-command "dvips -P%p %s -2")
(setq auto-mode-alist (cons '("\\.tex$"   . tex-mode) auto-mode-alist))

;;; Setup for LaTeX Outline Mode

(make-variable-buffer-local 'outline-prefix-char)
(setq-default outline-prefix-char "\C-l")
(make-variable-buffer-local 'outline-regexp)
(setq-default outline-regexp "[*\^l]+")
(make-variable-buffer-local 'outline-level-function)
(setq-default outline-level-function 'outline-level-default)
(setq-default TeX-master nil) ; Query for master file.


;;; Extra Latex Functions ============================================

(defun latex-small-margins ()
  "Insert the LaTeX commands that setup small margins."
  (interactive)
  (insert-string "% fixup margins to fit more text on page.\n")
  (insert-string "\\sloppy\n")
  (insert-string "\\setlength{\\textwidth}{6.5in}\n")
  (insert-string "\\setlength{\\oddsidemargin}{0pt}\n")
  (insert-string "\\setlength{\\evensidemargin}{0pt}\n")
  (insert-string "\\setlength{\\textheight}{8.5in}\n")
  (insert-string "\\setlength{\\topmargin}{-0.5in}\n")
  (insert-string "\\pagestyle{headings}\n") )

