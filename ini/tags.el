;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    tags
;;; Purpose: Extended find-tag support.
;;; ==================================================================

(defun jw-ft-be-prefix-transform (tagname)
  "Transform the tagname if it matches the 'be_xxxx' pattern."
  (and tagname
       (> (length tagname) 3)
       (string= (substring tagname 0 3) "be_")
       (substring tagname 3) ))

(defun jw-ft-transform (tagname)
  "Transform the extended tagname to its actual name, or nil if there is no transform."
  (jw-ft-be-prefix-transform tagname))

(defun jw-ft-extended-find (tagname next-p regexp-p)
  "Call find-tag with a tranformed tagname."
  (let ((transformed-tagname (jw-ft-transform tagname)))
    (if transformed-tagname
        (find-tag transformed-tagname next-p regexp-p)
      (error (concat "No extended tags containing " tagname)) )))

(defun jw-extended-find-tag (tagname &optional next-p regexp-p)
  "Extended find-tag function to handle tags that don't literally match."
  (interactive (find-tag-interactive "Find extended tag: "))
  (condition-case nil
      (find-tag tagname next-p regexp-p)
    (error (jw-ft-extended-find tagname next-p regexp-p)) ))

;;; Remap the standard find-tag key to use the extended version.
(global-set-key "\M-." 'jw-extended-find-tag)
