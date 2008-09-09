;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-comint
;;; Purpose: Setups for common shell interpreters
;;; ==================================================================

;;; Comint setup =======================================================

(setq comint-input-ring-size 100)
(setq comint-password-prompt-regexp
  "\\(^[Pp]assword\\|^enter password\\|pass phrase\\|[Pp]assword for '[a-zA-Z0-9]+'\\):\\s *\\'")

(if (not (is-xemacs))
    (setq comint-output-filter-functions '(comint-watch-for-password-prompt))

(defun jnw-set-shell-prompt-hookcode ()
  (setq comint-prompt-regexp "^[^#$%>- \n]*[#$%>-] *")
  (setq telnet-prompt-pattern comint-prompt-regexp))

(add-hook 'telnet-mode-hook 'jnw-set-shell-prompt-hookcode)

(require 'shell)
(define-key shell-mode-map  "\C-c\C-i" 'send-invisible))

(require 'comint)
(define-key comint-mode-map "\C-c\C-i" 'send-invisible)
(define-key comint-mode-map "\C-c " 'multi-shell)

(defun jnw-add-invisible-telnethookcode ()
  (define-key telnet-mode-map "\C-c\C-i" 'send-invisible) )
(add-hook 'telnet-mode-hook 'jnw-add-invisible-telnethookcode)
(add-hook 'telnet-mode-hook 'jnw-fix-telnet-output-filter)

;; Fix Telnet to honor the output-filter filter stuff

(defun jnw-fix-telnet-output-filter ()
  (defun telnet-filter (proc string) (jnw-telnet-filter proc string)))

;; jnw-telnet-filter is just like the standard version except that
;; the output filter functions are run at the end.

(defun jnw-telnet-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let* ((last-insertion (marker-position (process-mark proc)))
	   (delta (- (point) last-insertion))
	   (ie (and comint-last-input-end
		    (marker-position comint-last-input-end)))
	   (w (get-buffer-window (current-buffer)))
	   (ws (and w (window-start w))))
      (goto-char last-insertion)
      (insert-before-markers string)
      (set-marker (process-mark proc) (point))
      (if ws (set-window-start w ws t))
      (if ie (set-marker comint-last-input-end ie))
      (while (progn (skip-chars-backward "^\C-m" last-insertion)
		    (> (point) last-insertion))
	(delete-region (1- (point)) (point)))
      (goto-char (process-mark proc))
      (and telnet-replace-c-g
	   (subst-char-in-region last-insertion (point) ?\C-g
				 telnet-replace-c-g t))
      ;; If point is after the insertion place, move it
      ;; along with the text.
      (if (> delta 0)
	  (goto-char (+ (process-mark proc) delta)))
      (let ((functions comint-output-filter-functions))
	(while functions
	  (funcall (car functions) string)
	  (setq functions (cdr functions)))) )))

(setq comint-process-echoes nil)

;;; Key sequence to toggle the process echo flag.
(global-set-key "\C-cs"
                (lambda ()
                  (interactive)
                  (cond
                   (comint-process-echoes 
                    (setq comint-process-echoes nil)
                    (message "shell not processing echoes"))
                   (t
                    (setq comint-process-echoes t)
                    (message "shell processing echoes")) )))
