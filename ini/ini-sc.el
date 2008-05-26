;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-sc
;;; Purpose: Super-Cite Customizations
;;; ==================================================================

;;; Supercite Customizations =========================================

(autoload 'sc-cite-original     "supercite" "Supercite 3.1" t)
(autoload 'sc-submit-bug-report "supercite" "Supercite 3.1" t)
(add-hook 'mail-citation-hook 'sc-cite-original)

(setq message-cite-function 'sc-cite-original)

(setq news-reply-header-hook nil)
(setq sc-auto-fill-region-p nil)
(setq sc-fixup-whitespace-p t)
(setq sc-citation-leader "    ")
(setq sc-citation-delimiter ">")
(setq sc-citation-separator " ")
(setq sc-preferred-attribution-list
      '("sc-lastchoice" "x-attribution" "firstname" "initials" "lastname"))


