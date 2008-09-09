;;; elunit-compat.el --- Compatability helper for XEmacs, etc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; 31 MAR 2007:
;; PST- added support for serializing strings with properties by using
;; `string-from-prin1' and `prin1-string-with-properties'.

;; Older versions require this to use `time-subtract'?
(ignore-errors
  (require 'time-date)) ;; bombs on a minial Emacs 21

;; XEmacs and Emacs 21?
(unless (fboundp 'line-number-at-pos)
  (defalias 'line-number-at-pos 'line-number))

;; XEmacs
(unless (fboundp 'propertize)
  (defun elunit-propertize (string &rest props)
    ;; from erc's erc-propertize
    (let ((string (copy-sequence string)))
      (while props
	(put-text-property 0 (length string)
			   (nth 0 props) (nth 1 props) string)
	(setq props (cddr props)))
      string))
  (defalias 'propertize 'elunit-propertize))

;; PST -- what doesn't support with-temp-message?
(unless (fboundp 'with-temp-message)
  (defmacro elunit-with-temp-message (&rest body)
    `(let ((old-message (with-current-buffer " *Echo Area*" (buffer-substring))))
       ,@body
       (with-current-buffer " *Echo Area*"
	 (delete-region (point-min) (point-max))
	 (insert old-message))))
  (defalias 'with-temp-message 'elunit-with-temp-message))

(defun elunit-format* (format &rest params)
  "A wrapper for `format' which ensures that `concat' is used internally
for strings using the \"%s\" format so that extents are copied in XEmacs."
  (let (segments)
    (save-match-data
      (let ((start 0)
            (original-params params))
	(while (string-match "%\\(?:%\\|.*?[a-z]\\)" format start)
	  (push (substring format start (match-beginning 0)) segments)
	  (setq start (match-end 0))
	  (let ((token (match-string 0 format)))
	    (push (if (string= token "%%")
                      "%"
                    (unless params ;show not enough arguments error in context
                      (apply 'format format original-params))
                    (let ((param (pop params)))
                      (if (and (string= token "%s") (stringp param))
                          param
                        (condition-case nil ;show errors in context
                            (format token param)
                          (error (apply 'format format original-params))))))
		  segments)))
	(push (substring format start) segments)))
    (apply 'concat (reverse segments))))
(defalias 'format* 'elunit-format*)


;; Serialize and unserialize strings with text properties. Since FSF
;; Emacs already correctly handles this, it's basically just a wrapper
;; for XEmacs.
f
(defun prin1-handles-text-properties-p ()
  "Returns non-nil if the emacs implementation of `prin1-to-string'
correctly handles strings with properties. (FSF Emacs 21 and 22 do while
XEmacs 21 does not.)"
  (let* ((test "test")) ; can't use `propertize'--not in XEmacs
    (put-text-property 0 (length test) 'test-prop 'test-val)
    (let ((reconstituted (eval (read (prin1-to-string test)))))
      (if (eq (plist-get (text-properties-at 0 reconstituted)
                         'text-prop)
              'text-value)
          (defun prin1-handles-text-properties-p () t)
        (defun prin1-handles-text-properties-p () nil)
        nil))))

(defun prin1-string-with-properties (string &optional disable-shortcut)
  "Returns a string (with no properties) in the same format as
returned by `prin1-to-string' in GNU Emacs. If DISABLE-SHORTCUT is
non-nil then GNU Emacs will be prohibitted from using
`prin1-to-string' directly."
  (if (and (prin1-handles-text-properties-p) (not disable-shortcut))
      (prin1-to-string string)
    (let* ((end (length string))
           (props (text-properties-at 0 string))
           (pos 0)
           found)
      (while (< pos end)
        (let ((change-pos (or (next-property-change pos string) end)))
          (push (list pos change-pos props) found)
          (setq props (text-properties-at change-pos string))
          (setq pos change-pos)))
      (concat "#"
              (prin1-to-string
               (cons (substring-no-properties string)
                     (apply 'nconc (reverse found))))))))

(defun string-from-prin1 (prin1 &optional allow-eval)
  "Returns a string (with properties) as reconstituted from PRIN1.
See `prin1-string-with-properties'. If ALLOW-EVAL is t then GNU
Emacs (and others that support the read syntax it does) can take a
shortcut."
  (if (and allow-eval (prin1-handles-text-properties-p))
      (eval (read prin1-string))
    (let* ((forms (read (substring prin1-string 1))) ;skip #
           (string (car forms))
           (prop-data (cdr forms)))
      (loop for (start end props) on prop-data by #'cdddr do
            (loop for (prop value) on props by #'cddr do 
                  (put-text-property start end prop value string)))
      string)))


(provide 'elunit-compat)