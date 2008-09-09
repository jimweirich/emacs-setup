;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-p4
;;; Purpose: Setups for Perforce Integration
;;; ==================================================================

;;; P4 Interface Setups ===============================================

; (defvar p4-cmd "/Users/jim/bin/p4_cmd")
(defvar p4-cmd "p4")

(defun p4-edit () 
  "Open the file associated with the current buffer for perforce edit."
  (interactive)
  (call-process p4-cmd nil "*p4*" t "edit" (buffer-file-name (current-buffer)))
  (revert-buffer t t) )

(defun p4-revert ()
  "Revert the file associated with the current buffer."
  (interactive)
  (call-process p4-cmd nil "*p4*" t "revert" (buffer-file-name (current-buffer)))
  (revert-buffer t t) )

;; Pattern used to find code buffers that might need resyncing.
(setq p4-code-file-name-pattern "\\.\\(rb\\|rhtml\\|css\\|js\\)$")

(defun p4-sync-code-buffer (b)
  (cond ((null (buffer-file-name b)) ())
        ((not (file-exists-p (buffer-file-name b)))
         (kill-buffer b))
        ((string-match p4-code-file-name-pattern (buffer-file-name b))
         (set-buffer b)
         (revert-buffer t t)) ))

(defun p4-sync-code-buffers ()
  "Reload all code buffers after a p4 submit command.
Useful because p4 will mark editted files 'read-only' after a submit.  This
command will resync the buffers with their current state."
  (interactive)
  (mapc 'p4-sync-code-buffer (buffer-list))
  nil)

(defun p4-env ()
  (interactive)
  (call-process "/Users/jim/bin/p4_env" nil "*p4*" t) )

(global-set-key "\C-cpe" 'p4-edit)
(global-set-key "\C-cpr" 'p4-revert)
(global-set-key "\C-cps" 'p4-sync-code-buffers)
(global-set-key "\C-cpv" 'p4-env)
