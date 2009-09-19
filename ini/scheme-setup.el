;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    scheme.el
;;; Purpose: Setups for working in scheme
;;; ==================================================================

(defun jw-scheme-load-and-go ()
  (interactive)
  (scheme-load-file (buffer-file-name (current-buffer)))
  (switch-to-scheme "scheme --emacs"))

(add-hook 'scheme-mode-hook
          '(lambda ()
             (require 'xscheme)
             (require 'cmuscheme)
             (define-key scheme-mode-map "\C-c\C-g" 'jw-scheme-load-and-go)))

(defun jw-scheme-send-expression ()
  (interactive)
  (move-end-of-line 1)
  (advertised-xscheme-send-previous-expression))

(add-hook 'scheme-interaction-mode-hook
          '(lambda ()
             (require 'xscheme)
             (require 'cmuscheme)
             (define-key scheme-interaction-mode-map [S-return]
               'jw-scheme-send-expression)))
