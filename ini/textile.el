;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    textile
;;; Purpose: Setups for textile mode
;;; ==================================================================


(require 'textile-mode)

(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))
(add-to-list 'auto-mode-alist '("\\.red\\'" . textile-mode))
