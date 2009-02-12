;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-aqua
;;; Purpose: Custom Setups for aqua emacs
;;; ==================================================================

;;; Disable some of the modes ========================================

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(cond ((or (is-aquamacs) (is-emacs-23))
       (setq mac-option-modifier 'meta)

       (defun monaco (points)
         (interactive "NPoints: ")
         (set-frame-font
          (concat "-*-monaco-medium-r-normal--"
                  (number-to-string points)
                  "-0-72-72-m-0-iso10646-1")
          t))

       (defun inconsolata (points)
         (interactive "NPoints: ")
         (set-frame-font
          (concat "-*-inconsolata-*-r-normal--"
                  (number-to-string points)
                  "-0-72-72-m-0-iso10646-1")
          t))

       (defun micro()      (interactive) (inconsolata 8))
       (defun tiny()       (interactive) (inconsolata 12))
       (defun small()      (interactive) (inconsolata 14))
       (defun normal()     (interactive) (inconsolata 16))
       (defun screencast() (interactive) (inconsolata 18))
       (defun medium()     (interactive) (inconsolata 20))
       (defun big()        (interactive) (inconsolata 24))
       (defun huge()       (interactive) (inconsolata 36))
       ))
