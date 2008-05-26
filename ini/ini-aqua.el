;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-aqua
;;; Purpose: Custom Setups for aqua emacs
;;; ==================================================================

;;; Disable some of the modes ========================================

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defun small()
  (interactive)
  (set-frame-font "-apple-monaco-medium-r-normal--14-0-72-72-m-0-iso10646-1" t))

(defun big()
  (interactive)
  (set-frame-font "-apple-monaco-medium-r-normal--20-0-72-72-m-0-iso10646-1" t))
