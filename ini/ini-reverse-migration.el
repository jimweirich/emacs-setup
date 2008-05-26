;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-reverse-migration
;;; Purpose: Reverse Migration
;;; ==================================================================

(setq reverse-migration-commands
      '((create_table . drop_table)
        (add_column . remove_column)))

(defun revmig-commands-aid (alist)
  (cond ((null alist) ())
        (t (cons (caar alist) 
                 (cons (cdar alist) (revmig-commands-aid (cdr alist))))) ))

(defun revmig-commands ()
  (revmig-commands-aid reverse-migration-commands))

(defun revmig-commands-re ()
  (concat "^ *\\("
          (mapconcat 'symbol-name (revmig-commands) "\\|")
          "\\) +\\([^ \n]+\\)") )

(defun do-one (limit)
   (interactive)
   (if (re-search-forward (revmig-commands-re) limit t) 
       (list (match-string-no-properties 1) 
             (match-string-no-properties 2))
     () ))

(defun do-all (commands limit)
  (let ((match (do-one limit)))
    (cond ((null match) commands)
          (t (do-all (cons match commands) limit)) )))

(defun find-up ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^ *def *self\\.up")
    (point) ))  

(defun find-down ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^ *def *self\\.down")
    (point) ))
  
(defun parse-up ()
  (interactive)
  (goto-char (point-min))
  (let ((start (find-up))
        (end (find-down)))
    (goto-char start)
    (do-all () end)))

(global-set-key "\C-cl" (lambda () (interactive) (setq xxx (parse-up))))
