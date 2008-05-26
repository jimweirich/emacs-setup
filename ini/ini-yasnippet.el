;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-yasnippet
;;; Purpose: Setup yasnippet
;;; ==================================================================

(setq yas-dir (expand-file-name "~jim/.elisp/packages/thirdparty/yasnippet-0.5.4"))

(cond ((file-exists-p yas-dir)
       (add-to-load-path yas-dir)

       (require 'yasnippet) ;; not yasnippet-bundle
;;       (require 'dropdown-list)
       (yas/initialize)
       (yas/load-directory (concat yas-dir "/snippets"))
       
       (setq yas/root-directory (expand-file-name "~jim/.elisp/snippets/"))
       (yas/load-directory yas/root-directory)
       ))

(setq yas/text-popup-function          #'yas/dropdown-list-popup-for-template)
(setq yas/window-system-popup-function #'yas/dropdown-list-popup-for-template)

;;; Should snippets minimize parens where possible?
(setq tm_minimize_paren t)

(defun snippet-paren-start ()
  (if tm_minimize_paren " " "("))

(defun snippet-paren-end ()
  (if tm_minimize_paren "" ")"))

(defun snippet-remove-empty-parens (text)
  (if (string-equal text "()") "()" text) )

(defun jw-clear-overlays ()
  "Clear the snippet overlays that interfere with TAB."
  (interactive)
  (mapcar 'delete-overlay (overlays-in (point-min) (point-max))))

;;; Clear overlays and return to top level.
(global-set-key "\C-ccc"
                '(lambda () (interactive)
                   (jw-clear-overlays)
                   (top-level)))
