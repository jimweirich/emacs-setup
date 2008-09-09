;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-dbg
;;; Purpose: Elisp Debugging Tools
;;; ==================================================================

;;; Write debugging messages to *DBG*.
(defun dbg (&rest msgs)
  (let ((buf (current-buffer)))
    (set-buffer (get-buffer-create "*dbg*"))
    (goto-char (point-max))
    (insert (apply 'concat
                   (mapcar (lambda (a) (format "%s" a)) msgs) ))
    (insert "\n")
    (set-buffer buf)))

(defun dbg-inline (message result)
  (dbg message result)
  result)
