(defun read-all-forms (source)
  "Read all forms in SOURCE and return then as a list."
  (cdr (read (format "(read-all-forms %s)" source))))

(defun time-difference-in-seconds (new-time old-time)
  "Given two times, as from `current-time', returns a floating-point
representation of the time difference in seconds. This does not work with
large time differences."
  (let ((diff (time-subtract new-time old-time)))
    (if (/= 0 (car diff))
        (error "I can't handle this: %S" diff))
    (+ (nth 1 diff) (/ (float (nth 2 diff)) 1000000))))
(defalias 'td-in-seconds 'elunit-time-difference-in-seconds)

(defmacro skip-whitespace-backward (&optional method)
  "Skip whitespace backwards. Whitespace is determined by METHOD which can
be one of: h/horizontal, v/vertical, b/both (default)."
  `(skip-chars-backward
    ,(case method
       ((horizontal h) "\t ")
       ((vertical v) "\n\v\r")
       (t "\t \n\v\r"))))
(defalias 'skip-ws-backward 'skip-whitespace-backward)

(defun append! (list elm)
  "Appends ELM to LIST by setting the last cdr. Returns LIST."
  (setcdr (last list) (if (listp elm) elm (list elm)))
  list)

;; XXX - wth?
(defmacro push! (elm gvar)
  "Like `push' but ensures GVAR, which must be a generalized setf-able
variable, is updated."
  (declare (indent 0))
  `(setf ,gvar
         (push ,elm ,gvar)))

(defmacro delete-if! (function gvar)
  "Like `delete-if' but ensures GVAR, which must be a generalized setf-able variable,
is updated."
  (declare (indent 0))
  `(setf ,gvar (delete-if ,function ,gvar)))

(defmacro find-first (list function)
  "Return the first element in LIST for which FUNCTION returns
non-nil. Yes, I know about `find'."
  (declare (indent 1))
  (let ((x- (gensym))
        (i- (gensym)))
    `(catch ',x-
       (dolist (,i- ,list)
         (when (funcall ,function ,i-)
           (throw ',x- ,i-))))))

(defun elunit-make-unique (value lookup-function &optional format-function)
  (let ((formatter (or format-function
                       (lambda (v i) (format "%s(%s") v i))))
    (if (funcall lookup-function value)
        (let ((counter 1))
          (while (funcall lookup-function
                          (funcall formatter value counter))
          (incf counter))
          (funcall formatter value counter))
      value)))

(provide 'my-support)