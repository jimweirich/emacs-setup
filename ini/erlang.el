;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-erlang
;;; Purpose: Setups for Erlang Mode
;;; ==================================================================

;;; Erlang Mode Setups ===============================================


(setq erlang-root-dir "/opt/local/lib/erlang")
(if (file-directory-p erlang-root-dir)
    (progn
      (setq load-path (cons  (concat erlang-root-dir "/lib/tools-2.5.2/emacs")
                             load-path))
      (setq exec-path (cons (concat erlang-root-dir "/bin") exec-path))
      (require 'erlang-start) ))
