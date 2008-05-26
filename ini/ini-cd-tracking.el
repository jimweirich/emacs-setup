;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-cd-tracking
;;; Purpose: Directory tracking for custom commands.
;;; ==================================================================

(defun jw-choose-file (files) 
  (cond ((null files) nil) 
        ((file-readable-p (car files)) (car files)) 
        (t (jw-choose-file (cdr files))) )) 

(defconst jw-directory-mappings ())                                             

(add-to-list
 'jw-directory-mappings                                             
 (list "emaralt" (jw-choose-file                                       
               '("~/projects/clients/delanor/branches/alt"
                 "~/projects/clients/medwiz/delanor/branches/alt"
                 "/home/delanor/src/perforce/branches/alt")) ))
(add-to-list
 'jw-directory-mappings                                             
 (list "emar"
       (jw-choose-file 
        '("~/projects/clients/delanor/development"
          "~/projects/clients/medwiz/delanor/development"
          "/home/delanor/src/perforce/development")) ))

(add-to-list
 'jw-directory-mappings                                             
 (list "emar2" "~/delanor2/delanor/development"))

(defun jw-cd (dir)
  (shell-cd dir)
  (message dir) )

(defun jw-check-dir-mappings (cmd mappings)
  (while mappings
    (cond ((string-equal cmd (caar mappings)) 
           (jw-cd (cadar mappings))
           (setq mappings nil))
          (t (setq mappings (cdr mappings))) )))

(defun jw-tracker (string) 
  (let ((cmd (replace-regexp-in-string "\n$" "" string)))
    (jw-check-dir-mappings cmd jw-directory-mappings) ))

(add-hook 'comint-input-filter-functions 'jw-tracker)
