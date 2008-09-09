;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-uniquify
;;; Purpose: Setup uniquify
;;; ==================================================================

(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-min-dir-content 0)
(setq uniqify-after-kill-buffer-p t)
