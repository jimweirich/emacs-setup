;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    scheme.el
;;; Purpose: Setups for working in scheme
;;; ==================================================================

(require 'xscheme)

(defun jw-scheme-load-and-go ()
  (interactive)
  (xscheme-send-buffer)
  (jw-push-buffer "*scheme*"))

(add-hook 'scheme-mode-hook
          '(lambda ()
             (define-key scheme-mode-map "\C-c\C-p" 'scheme-trace-procedure)
             (define-key scheme-mode-map "\C-c\C-t" 'jw-split-or-toggle)
             (define-key scheme-mode-map "\C-c\C-g" 'jw-scheme-load-and-go)
             (define-key scheme-mode-map [S-return] 'xscheme-send-definition)))

(defun jw-scheme-send-expression ()
  (interactive)
  (move-end-of-line 1)
  (advertised-xscheme-send-previous-expression))

(add-hook 'scheme-interaction-mode-hook
          '(lambda ()
            (define-key scheme-interaction-mode-map [S-return] 'xscheme-send-definition)))


(setq scheme-program-name "/usr/local/bin/scheme")
