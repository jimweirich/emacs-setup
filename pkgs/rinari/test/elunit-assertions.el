;;; elunit-support.el - Assertions and other support functions for `elunit'

;; Copyright (C) 2006 Paul Stickney

;; Inspired by regress.el by Wayne Mesard and Tom Breton, Test::Unit
;; by Nathaniel Talbott, and xUnit by Kent Beck

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

;;
;; These assertions are designed to work in a similar fashion to how
;; assertions work in Ruby's Test::Unit.
;;

;;; Log
;; 2006-SEP-23
;; - created

;; for `defun*'
(eval-when-compile
  (require 'cl))

;;; Assertion helpers

;;; XXX - has dependancy on elunit core/elunit-assertion-error
(defun* do-assert (assertion &key message expected actual info)
  "If ASSERTION is nil signal an `assertion-failed' error is signaled,
otherwise return t.  See `assert-object-to-string' for how
actual/expected values and forms are converted.  See
`elunit-assertion-error' for meaning of arguments."
  (unless assertion
    (signal 'assertion-failed
            (make-elunit-assertion-error
             :message (if info
                          (format "%s: %s" info message)
                        message)
             :expected (assert-object-to-string expected)
             :actual (assert-object-to-string actual))))
  ;; if no signal, must be OK
  t)

(defun assert-object-to-string (object)
  "Turns OBJECT into a string very much like (FORMAT \"%S\" OBJECT)
except in the case where OBJECT is a string and begins with \"===\". In
that case \"===\" is stripped and the remaining string is
returned."
  (if (and (stringp object)
           (eq (string-match "===" object) 0))
      (substring object (match-end 0))
    (format "%S" object)))


;;; Assertions

(defun assert-t (actual &optional info)
  (do-assert (eq t actual)
             :message "ACTUAL is t"
             :info info
             :expected "===t"
             :actual actual))

(defun assert-nil (actual &optional info)
  (do-assert (not actual)
             :message "ACTUAL is nil"
             :info info
             :expected "===nil"
             :actual actual))

(defun assert-non-nil (actual &optional info)
  (do-assert actual
             :message "ACTUAL is non-nil"
             :info info
             :expected "===non-nil"
             :actual actual))

(defun assert-eq (expected actual &optional info)
  (declare (indent 0))
  (do-assert (eq expected actual)
             :message "ACTUAL eq EXPECTED"
             :info info
             :expected expected
             :actual actual))

(defun assert-equal (expected actual &optional info)
  (declare (indent 0))
  (do-assert (equal expected actual)
             :message "ACTUAL equal EXPECTED"
             :info info
             :expected expected
             :actual actual))

(defun assert-not-equal (expected actual &optional info)
  (do-assert (not (equal expected actual))
             :message "ACTUAL not equal EXPECTED"
             :info info
             :expected (format "===not %S" expected)
             :actual actual))

;;; Wrapping assertions (the kind that take forms)

(defmacro assert-same-point (&rest forms)
  "Fails if FORMS move point."
  (declare (indent 1))
  (let ((start- (gensym)))
    `(let ((,start- (point)))
       ,@forms
       (do-assert (eq ,start- (point))
                  :message "POINT is not moved"
                  :expected ,start-
                  :actual (point))
       )))

(defmacro assert-no-errors (&rest forms)
  "Fails if FORMS signal any kind of error."
  (declare (indent 1))
  `(condition-case err
       (progn
         ,@forms)
     (error
      (do-assert nil
                 :message "No error signaled"
                 :expected "===no error"
                 :actual (error-to-pretty-string err))
      )))

(defmacro assert-error (error-type &rest forms)
  "Fails unless FORMS signals an error of ERROR-TYPE.  ERROR-TYPE should be
an unqoted symbol."
  (declare (indent 1))
  (let ((some-error- (gensym))
        (found- (gensym)))
    `(let (,some-error- ,found-)
       (condition-case err
           (progn
             ,@forms)
         (,error-type
          (setq ,found- t))
         (error
          (setq ,some-error- err)))
       (do-assert ,found-
                  :message (format "Error signaled: %S" ',error-type)
                  :expected ',error-type
                  :actual (if ,some-error-
                              (error-to-pretty-string ,some-error-)
                            "===No error signaled"))
       )))
       

(provide 'elunit-assertions)