;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-cd-tracking
;;; Purpose: Directory tracking for custom commands.
;;; ==================================================================

(defconst jw-directory-mappings ())                                             

(add-to-list
 'jw-directory-mappings                                             
 (list "^emar$"
       (jw-choose-file 
        '("~/projects/clients/delanor/git/development"
          "~/projects/clients/delanor/development"
          "~/projects/clients/medwiz/delanor/development"
          "/home/delanor/src/perforce/development")) ))

(add-to-list
 'jw-directory-mappings                                             
 (list "^emar\\( +\\([0-9]+\\)\\)$"
       "~/projects/clients/delanor/iterations/48"))

(defun jw-cd (dir)
  (shell-cd dir)
  (message dir) )

(defun jw-check-dir-mappings (cmd mappings)
  (while mappings
    (cond ((string-match (caar mappings) cmd)
           (jw-cd (cadar mappings))
           (setq mappings nil))
          (t (setq mappings (cdr mappings))) )))

(defun jw-track-go-directories (string)
  (let ((out (replace-regexp-in-string "\n$" "" string)))
    (if (string-match "\\[cd: \\(.*\\)\\]" out)
        (let ((dir (substring out (match-beginning 1) (match-end 1))))
          (jw-cd dir) ))))

(defun jw-tracker (string) 
  (let ((cmd (replace-regexp-in-string "\n$" "" string)))
    (jw-check-dir-mappings cmd jw-directory-mappings) ))

;;; Add the hooks

(add-hook 'comint-input-filter-functions 'jw-tracker)
(add-hook 'comint-output-filter-functions 'jw-track-go-directories)

;;; Debugging --------------------------------------------------------

(defun jw-debug-tracker (string)
  (message (concat "TRACKING STRING: '" string "'")))

