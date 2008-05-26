;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-eiffel
;;; Purpose: Setups for Eiffel Mode
;;; ==================================================================

;;; Eiffel Mode Setups ===============================================


(autoload
  'jw-eiffel-insert-template
  "eiffel-templates"
  "Insert an Eiffel Template")

(setq eif-indent-increment 4)

(defun jw-eiffel-init ()
  (define-key eiffel-mode-map "\C-ci"  'jw-eiffel-insert-template)
  )

(add-hook 'eiffel-mode-hook 'jw-eiffel-init)
(add-hook 'eiffel-mode-hook 'font-lock-fontify-buffer)

(defun jw-short (class-name)
  (interactive "sClass Name: ")
  (save-excursion
    (let ((bufname (concat "*short-" class-name "*")))
      (if (get-buffer bufname)
	  (kill-buffer bufname))
      (get-buffer-create bufname)
      (call-process "jw-short" nil bufname nil "-no_warning" class-name)
      (switch-to-buffer-other-window bufname)
      (set-buffer bufname)
      (goto-char (point-min))
      (eiffel-mode) 
      (font-lock-mode)
      (toggle-read-only 1) )))

(defun jw-short-at-point ()
  (interactive)
  (progn
    (require 'etags)
    (jw-short (find-tag-default))) )
  
;(let 
;    ((reg '("^line \\([0-9]+\\) column \\([0-9]+\\)[^(]+(\\([^)]+\\)" 3
;1 2)))
;  (setq compilation-error-regexp-alist
;	(if (boundp 'compilation-error-regexp-alist)
;	    (cons reg compilation-error-regexp-alist)
;	  (list reg))))
;
(let 
    ((reg '("^line \\([0-9]+\\) column \\([0-9]+\\) +file \\(.*\\)$" 4 1 2)))
  (setq compilation-error-regexp-alist
	(if (boundp 'compilation-error-regexp-alist)
	    (cons reg compilation-error-regexp-alist)
	  (list reg))))
