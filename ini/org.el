;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-org
;;; Purpose: Setups for Org Mode
;;; ==================================================================

;;(add-to-list 'load-path "~/.elisp/packages/thirdparty/org-5.13i")
;;(require 'org-install)

;;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
