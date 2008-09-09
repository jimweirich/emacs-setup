;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-aqua
;;; Purpose: Custom Setups for aqua emacs
;;; ==================================================================

;;; Disable some of the modes ========================================

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(cond ((is-aquamacs)
       (setq mac-option-modifier 'meta)

       (defun small-monaco()
         (interactive)
         (set-frame-font "-apple-monaco-medium-r-normal--14-0-72-72-m-0-iso10646-1" t))
       
       (defun big-monaco()
         (interactive)
         (set-frame-font "-apple-monaco-medium-r-normal--20-0-72-72-m-0-iso10646-1" t))
       
       (defun micro()
         (interactive)
         (set-frame-font "-*-inconsolata-*-r-normal--8-*-*-*-*-*-*-*" t))
       
       (defun tiny()
         (interactive)
         (set-frame-font "-*-inconsolata-*-r-normal--12-*-*-*-*-*-*-*" t))
       
       (defun small()
         (interactive)
         (set-frame-font "-*-inconsolata-*-r-normal--14-*-*-*-*-*-*-*" t))
       
       (defun normal()
         (interactive)
         (set-frame-font "-*-inconsolata-*-r-normal--16-*-*-*-*-*-*-*" t))
       
       (defun screencast()
         (interactive)
         (set-frame-font "-*-inconsolata-*-r-normal--18-*-*-*-*-*-*-*" t))
       
       (defun medium()
         (interactive)
         (set-frame-font "-*-inconsolata-*-r-normal--20-*-*-*-*-*-*-*" t))
       
       (defun big()
         (interactive)
         (set-frame-font "-*-inconsolata-*-r-normal--24-*-*-*-*-*-*-*" t))

       (defun huge()
         (interactive)
         (set-frame-font "-*-inconsolata-*-r-normal--36-*-*-*-*-*-*-*" t))
       ))
