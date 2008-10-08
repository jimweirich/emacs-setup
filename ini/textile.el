;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    textile
;;; Purpose: Setups for textile mode
;;; ==================================================================


(require 'textile-mode)

;; The default link face is too dark for a black background.
(defface textile-link-face
  '((t (:foreground "LightBlue")))
  "Face used to highlight links."
  :group 'textile-faces)

(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))
(add-to-list 'auto-mode-alist '("\\.red\\'" . textile-mode))
