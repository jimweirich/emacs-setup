;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-ant
;;; Purpose: Setups for compiling with Ant
;;; ==================================================================

(require 'compile)

;;; Ant adds a banner to error messages.  This should handle it.

(setq compilation-error-regexp-alist
  (append (list 
     ;; works for jikes
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
     ;; works for javac 
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
  compilation-error-regexp-alist))
