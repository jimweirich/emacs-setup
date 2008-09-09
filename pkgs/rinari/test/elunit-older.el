;;; elunit.el --- Emacs Lisp Unit Testing framework

;; Copyright (C) 2006 Phil Hagelberg

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

;; See http://dev.technomancy.us/phil/wiki/ElUnit for usage details.


;; pst - 23SEP2006

;; Convention:
;;   suite-name, test-name, etc. refer to the NAME or the suite or test and
;;     should be a symbol
;;   suite - structure of `elunit-suite'
;;   result - structure of `elunit-result'
;;   test - structure of `elunit-test'

(require 'cl) ; defstruct, etc
(require 'compile)

(require 'elunit-support)

(defvar *elunit-suites*
  ()
  "LIST of zero or more suites (of `elunit-suite').")

(defvar *elunit-last-suite-ran*
  nil
  "Name of the last suite ran. Makes running the same suite again and again
a little nicer.")

(defvar elunit-show-test-form
  t
  "If non-nil, show part of the test-form in error and failure messages.")

;;;; Data structures

(defstruct elunit-suite
  "Structure describing a test suite, a collection of tests.  Created by `defsuite'.
The slots are:
NAME: suite name, a symbol
TESTS: list of zeror or more `elunit-test' structures"
  name
  tests)

(defstruct elunit-test
  "Structure describing a test.  Created by `elunit-make-test'.
The slots are:
NAME: test name, a symbol
DOCSTRING: test docstring, a string
DECLARATION-FORM: test declaration form
TEST-FORM: test form to execute for test (wrapped inside `progn')
FILE-NAME: file name test declaration is in, read from buffer
LINE-NUMBER: line number test declaration starts on, read from buffer"
  name
  docstring
  declaration-form
  test-form
  file-name
  line-number)

(defstruct elunit-result
  "Structure describing the results from running a test.  Created by `elunit-run-test'.
The slots are:
TEST-OBJECT: `elunit-test' containing information about the test.
STATUS: one of `success', `failure' or `error'.
ERROR-OBJECT: error object, or nil."
  test-object
  status
  error-object)


;;;; Public interface functions

(defmacro defsuite (suite-name &rest test-forms)
  "This is what you use to set things up."
  ;; One shot, one kill - reset entire test list, no questions asked
  ;; Downside is you can't "spread" a defsuite
  `(let* ((suite (elunit-find-or-create-suite ',suite-name))
          (tests (mapcar 'elunit-make-test
                 '(,@test-forms))))
     (setf (elunit-suite-tests suite) tests)
     ;; return test count for lack of better
     (length (elunit-suite-tests suite))))

;; XXX - why is this the only function that takes SUITE-NAME as a
;; string?
(defun elunit (suite-name)
  "Ask for a single suite, run all its tests, and display the results.
SUITE-NAME should be a string."
  (interactive (list (completing-read
                      (concat "Run test suite (default " *elunit-last-suite-ran* "): " )
                      (mapcar (lambda (suite) (symbol-name (car suite))) 
                              *elunit-suites*) nil t nil nil *elunit-last-suite-ran*)))
  ;; oops, suite not found!
  (unless (elunit-find-suite (intern suite-name))
    (error "No suite information for a suite `%s' -- not running tests" suite-name))
      
  (setq *elunit-last-suite-ran* suite-name) ;save choice for next time
  (setq *elunit-fail-count* 0)
  (setq *elunit-error-count* 0)
  (run-hooks (intern (concat suite-name "-setup-hook")))
  (with-output-to-temp-buffer "*elunit*"
    (princ (format "Loaded suite: %s\n\n" suite-name))
    (let* ((suite (elunit-find-suite (intern suite-name)))
           (tests (elunit-get-tests suite))
           (start-time (cadr (current-time)))
           ;; run the tests -- side effect of updating "status line"
           (results (elunit-run-tests tests))
           (test-duration (- (cadr (current-time)) start-time)))
      (princ "\n\n\n") ;one \n is used to end "status line"
      (elunit-report-results results)
      (princ "\n\n\n")
      (princ (concat (elunit-test-summary results)
                     (format " in %d seconds." test-duration)))))
  (run-hooks (intern (concat suite-name "-teardown-hook"))))

(defun elunit-remove-suite (suite-name)
  "Remove a suite with the name given by SUITE-NAME.  SUITE-NAME should be
a symbol."
  (setq *elunit-suites*
        (delete-if (lambda (suite)
                     (eq (elunit-suite-name suit) suite-name))
                   *elunit-suites*)))

(defun elunit-remove-all-suites ()
  "Remove all suites."
  (setq *elunit-suites* ()))



;;;; Support functions

(defun elunit-make-test (body)
  "Returns a list representing a test to conduct using information from
BODY.  Returns an structure of `elunit-test'."
  (let* ((test-name (pop body))
         (declaration-form body)
         (docstring  (if (stringp (car body)) (pop body) ""))
         (test-form `(progn ,@body))
         ;; not a foolproof heuristic to get line number, but good enough?
         ;; XXX- [how] will this worked for compiled elisp?
         (line-number (save-excursion
                        (save-match-data
                          (re-search-backward
                           (concat "(\\s-*" (symbol-name test-name)))
                          (line-number-at-pos)))))
    (make-elunit-test :name test-name
                      :docstring docstring
                      :declaration-form body ; XXX- not really full, modified
                      :test-form test-form
                      :file-name buffer-file-name
                      :line-number line-number)
    ))

(defun elunit-find-suite (suite-name)
  "Return the SUITE (of type `elunit-suite') identified by SUITE-NAME, or
nil on a lookup failure.  SUITE-NAME should be a symbol."
  (catch 'suite-found
    (dolist (suite *elunit-suites*)
      (when (eq (elunit-suite-name suite) suite-name)
        (throw 'suite-found suite)))
    nil))

(defun elunit-find-or-create-suite (suite-name)
  "Try to find a suite given by SUITE-NAME.  If that fails a new suite
will be created.  The suite data list is returned."
  (or (elunit-find-suite suite-name)
      (push (make-elunit-suite :name suite-name)
            *elunit-suites*)))

;;; Running the unit tests

(defun elunit-run-tests (tests &optional hide-progress)
  "Runs TESTS.  Updates the status status area unless HIDE-PROGRESS is
non-nil.  Returns the test RESULTS."
  (mapcar (lambda (test)
            (let ((result (elunit-run-test test)))
              ;; update status display
              (unless hide-progress
                (elunit-report-progress (elunit-result-status result)))
              result))
          tests))

(defun elunit-run-test (test)
  "Run a test.  The result will be an `elunit-result' object."
  (let* ((status 'success) ;until we are told otherwise...
	 (error-object (condition-case err
                           (save-excursion
                             (eval (elunit-test-test-form test))
                             nil) ;; no error on success
                         (assertion-failed ;normal test-failure
                          (setq status 'failure)
                          err)
                         (error ;oops!
                          (setq status 'error)
                          err))
                       ))
    ;; update error/failure counters
    (case status
      (failure (incf *elunit-fail-count*))
      (error   (incf *elunit-error-count*)))
    (make-elunit-result :test-object test :status status
                        :error-object error-object)))

;;; Output functions
;;  -- elunit-report* functions use `princ' for output.

(defun elunit-test-summary (results)
  "Return a summary string about the tests including information such as
total number, errors and failures."
  (format "%d tests total, %d errors, %d failures"
          (length results) *elunit-error-count* *elunit-fail-count*))

(defun elunit-report-progress (status)
  "Output progress for STATUS.  This works on the assertion that nothing
else will enter the print-stream between invocations."
  (let* ((symbol-lookup
          '((success . ".") (failure . "F") (error . "E")))
         (status-symbol (or (cdr (assq status symbol-lookup)) "?")))
    (princ status-symbol)
    ;; add distinction to errors
    (unless (eq status 'success)
      (switch-to-buffer "*elunit*")
      (overlay-put (make-overlay (1- (point)) (point)) 'face '(foreground-color . "red"))
      (switch-to-buffer nil))))

(defun elunit-report-results (results &optional test-separator)
  "Print details about all TESTS.  If TEST-SEPARATOR is a string it is used
  to seperate the results for each test.  If TEST-SEPARATOR is not a string
  it defaults to \"\n\n\"."
  (let ((problem-index 0)
        (test-separator (or test-separator "\n\n")))
    (dolist (result results)
      (unless (eq (elunit-result-status result) 'success)
        (incf problem-index)
        (when (> problem-index 1)
          (princ test-separator))
	(elunit-report-result result problem-index))
      )))
    
(defun elunit-report-result (result index)
  "Print a single test failure or error. RESULT is of `elunit-result'."
  (case (elunit-result-status result)
    (failure (elunit-report-failure result index))
    (error   (elunit-report-error result index))
    (t       (error "Don't know how to display status: %S"
                    (elunit-result-status result)))))

(defun elunit-report-failure (result index)
  "Print a test (assertion) failure.  RESULT should be a `elunit-result' object."
  (flet ((print-prop (prop value)
                     (princ (format "%10s: %s\n" prop value))))
    (let* ((test (elunit-result-test-object result))
           (error-data (cdr (elunit-result-error-object result)))
           (message  (elunit-assertion-error-message error-data))
           (expected (elunit-assertion-error-expected error-data))
           (actual   (elunit-assertion-error-actual error-data))
           (expected-form nil)
           (actual-form nil)
           (test-form (elunit-test-test-form test)))
      (princ (format "%d) Failure: %s [%s:%s]\n          %s\n"
                     index
                     (elunit-test-name test)
                     (elunit-test-file-name test)
                     (elunit-test-line-number test)
                     (elunit-test-docstring test)))
      (print-prop "Assertion" message)
      (print-prop "Expected" expected)
      (when expected-form
        (print-prop "E. Form" expected-form))
      (print-prop "Actual" actual)
      (when actual-form
        (print-prop "A. Form" actual-form))
      (when elunit-show-test-form
        (print-prop "Test form" test-form))
      )))

(defun elunit-report-error (result index)
  "Print a test error.  RESULT should be a `elunit-result' object."
  (flet ((print-prop (prop value)
                     (princ (format "%10s: %s\n" prop value))))
    (let* ((test (elunit-result-test-object result))
           (test-form (elunit-test-test-form test))
           (error-object (elunit-result-error-object result)))
      (princ (format "%d) Error: %s [%s:%s]\n          %s\n"
                     index
                     (elunit-test-name test)
                     (elunit-test-file-name test)
                     (elunit-test-line-number test)
                     (elunit-test-docstring test)))
      (print-prop "Error" (error-to-pretty-string error-object))
      (when elunit-show-test-form
        (print-prop "Test form" test-form))
      )))

(add-hook 'temp-buffer-show-hook 'compilation-minor-mode)
;(add-to-list 'compilation-error-regexp-alist '("\\[\\([^:]*\\):\\([0-9]+\\)" 1 2))
;(add-to-list 'compilation-error-regexp-alist '("\\[\\([^\]]*\\):\\([0-9]+\\)\\]" 1 2))


(provide 'elunit)
