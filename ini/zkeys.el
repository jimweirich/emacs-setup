;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-zkeys
;;; Purpose: Global Function Key Setting
;;; ==================================================================

(defun jw-noop () (interactive))

(if (boundp 'osx-key-mode-map)
    (progn
      (define-key osx-key-mode-map (kbd "A-p") 'jw-noop)
      (define-key osx-key-mode-map (kbd "A-t") 'jw-noop)
      (define-key osx-key-mode-map (kbd "C-A-p") 'aquamacs-print) ))

(cond (t
       (global-set-key "\M-#" 'comment-region)
       (global-set-key "\C-c^" 'top-level)
       (global-set-key (if (is-xemacs) [(shift f2)] [S-f2]) "\C-?")
       (global-set-key [f3] 'jw-cite-filled)
       (if (is-xemacs)
           (progn (global-set-key [f4] 'server-start)
                  (global-set-key [(shift f4)] 'gnuserv-start) ))
       (global-set-key [f5] 'call-last-kbd-macro)
       (global-set-key [f6] 'refresh)
       (global-set-key [f10] 'call-last-kbd-macro)
       (global-set-key [f12] 'jw-zap-all-ansi)
       window-system ))
