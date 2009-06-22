;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    tags
;;; Purpose: Setup for Tags
;;; ==================================================================

(defun jw-extended-find-tag (tag)
  (interactive (find-tag-interactive "Extended find tag: "))
  (if (string= (substring tag 0 3) "be_")
      (find-tag (substring tag 3))
    (find-tag tag) ))

(global-set-key "\C-C." 'jw-extended-find-tag)
(if (boundp 'osx-key-mode-map)
    (progn
      (define-key osx-key-mode-map (kbd "A-.") 'jw-extended-find-tag) ))
