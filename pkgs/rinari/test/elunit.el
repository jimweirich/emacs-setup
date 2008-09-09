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

;; Additional contributors:
;;   Paul Nathan Stickney <pstickne@gmail.com>

;;;
;;; Conventions:
;;;
;; suite-name/group-name/test-name - a string representing the name
;; suite - structure of `elunit-suite'
;; group - structure of `elunit-group'
;; result - structure of `elunit-result'
;; test - structure of `elunit-test'

;;;
;;; TODO:
;;;
;; - A way to load test suites, ensuring source can be found, given
;;   a filename.
;; - Better multiple-suites-at-once control and details
;; - Allow suites to depend on other suites.
;; - Different verbosity levels for running tests.
;; - "Fixtures"
;; - Modifications to `elunit-find-suite-source' to avoid "finding" in
;;   strings and comments

;;;
;;; Changes
;;;
;; 07 OCT 2006
;;  -Compilation-minor-mode support completely removed, handled natively
;; 08 OCT 2006
;;  -Duplicate tests are added after being appended with a uniquing identifier
;; 10 OCT 2006
;;  -Completed removal of princ for output, converted overlays to text
;;   properties
;; 14 OCT 2006
;;  -Tests can be now, once again, be added without direct access to source.
;;   However, features such as edebug and source jumping are disabled.
;;  -An error is signaled if extracted source does not match test forms.
;;  -Fixed bug where running tests without first deleting the buffer caused
;;   inconsistent output
;;  -Removed use of with-output-to-temp-buffer. This was needed so that
;;   updates can occur before all tests finish.
;;  -Simplified setups. Flow diagram:
;;   (TEST SUITE (suite-setup
;;       (EACH GROUP (group-setup
;;           (EACH TEST (suite-test-setup (group-test-setup
;;                 (RUN TEST)...
;; 15 OCT 2006
;;  -Tests names are no longer made unique.
;;  -Rewrote source extractors. They are simpler and more reliable.
;; 22 OCT 2006
;;  -Determined which functions are "below setup level" (those which
;;   are called after the suite setup but before the final test is
;;   run).  *prefixed variables with "el-" to minimize/avoid clashes
;;  -Setups no longer must manually call yield--it will be
;;   automatically invoked if needed
;; 25 OCT 2006
;;  -Compatability changes for XEmacs
;; 27 OCT 2006
;;  -Strings are now used consistently, instead of symbols, for suite, group
;;   and test names.
;; 28 OCT 2006
;;  -Bug fixes.

(require 'cl) ;; for what?
(require 'elunit-support)
(require 'elunit-assertions)
(require 'elunit-compat)
(require 'elunit-gen)
 ;; XXX -- needs to be merged/placed elsewhere
(require 'my-support)

;;;
;;; Variables/customizations
;;;

(defvar *elunit-suites* ()
  "LIST of zero or more suites (of `elunit-suite').")

(defvar *elunit-last-suite-ran* nil
  "The name, as a string, of the last suite ran.")

(defvar elunit-keep-window-selection t
  "*If non-nil the window selection will be preserved. Otherwise the output
window will be selected. This only makes sense if other-window is used.")

(defvar elunit-results-in-other-window t
  "*If non-nil results will be displayed in other-window.")
(defvar elunit-linked-results-in-other-window nil
  "*If non-nil results, from following a link, will be displayed in
other-window. Overrides `elunit-results-in-other-window'.")

(defvar elunit-source-in-other-window t
  "*If non-nil source will be displayed in other-window.")
(defvar elunit-linked-source-in-other-window t
  "*If non-nil source, from following a link, will be displayed in
other-window. Overrides `elunit-source-in-other-window'.")

(defvar elunit-progress-indicators
  '(success ("o" . elunit-success-face)
    failure ("F" . elunit-failure-face)
    error ("E" . elunit-error-face)
    not-implemented ("i" . elunit-not-implemented-face))
  "*Indicators used for displaying 'running status'; a plist where the keys
  refers to RESULT status types and values are a cons in the
  form (STATUS-STR . FACE)")

(defvar elunit-report-handlers
  '((failure . elunit-failure-report)
    (error . elunit-error-report)
    (not-implemented . elunit-not-implemented-report))
  "ALIST: keys are result status codes, values are report functions.")

;; Faces
(defface elunit-source-link-face
  '((t (:foreground "red" :underline t)))
  "Source link")

(defface elunit-debug-link-face
  '((t (:foreground "blue" :underline t)))
  "Debug link")

(defface elunit-run-suite-link-face
  '((t (:foreground "blue" :underline t)))
  "Run suite link")

(defface elunit-success-face
  '((t (:foreground "blue")))
  "Success")

(defface elunit-failure-face
  '((t (:foreground "red")))
  "Failure")

(defface elunit-error-face
  '((t (:foreground "red" :background "black")))
  "Error")

(defface elunit-not-implemented-face
  '((t (:foreground "red")))
  "Not implemented")

;;;
;;; Errors and error datastructures
;;;

;; Not-Implemented error
;; No data
(put 'not-implemented
     'error-conditions '(error elunit-error not-implemented))
(put 'not-implemented 'error-message "Not implemened")

;; Assertion error
;; Data is a `elunit-assertion-error' structure.
(put 'assertion-failed
     'error-conditions '(error elunit-error assertion-failed))
(put 'assertion-failed 'error-message "Assertion failed")

(defstruct elunit-assertion-error
  "Structure describing the data-structure associated with
`assertion-error' signals.
Slots are:
MESSAGE: String; assertion summary (what is being asserted for).  It
should not include the expected or actual results directly but reference
them symbolically such as in \"EXPECTED less than ACTUAL\".
EXPECTED: String; representation of expected value
ACTUAL: String; representation of actual value"
  message
  expected
  actual)

;;;
;;; Core datastructures and accessors
;;;

(defstruct elunit-suite
  "Structure describing a test suite: a collection of tests.  Created by
`defsuite'.
The slots are:
NAME: suite name, a string
DOCSTRING: suite docstring, a string
R-GROUPS: list of GROUPS stored in *reverse order*
SUITE-SETUP: function to run suite setup or nil
TEST-SETUP: function to run test setup or nil"
  name
  docstring
  r-groups
  suite-setup
  test-setup)

(defun create-elunit-suite (suite-name)
  (make-elunit-suite :name suite-name))

(defun elunit-suite-groups (suite)
  "Returns list of groups."
  (reverse (elunit-suite-r-groups suite)))

(defun elunit-suite-tests (suite)
  "Returns lists of all tests."
  (let (all-tests)
    (dolist (group (elunit-suite-groups suite))
      (dolist (test (elunit-group-tests group))
        (push test all-tests)))
    (reverse all-tests)))

(defun elunit-suite-add-group (suite group)
  "Add GROUP to a SUITE. All group names must be unique."
  (let ((group-name (elunit-group-name group)))
    (when (find-first (elunit-suite-r-groups suite)
            (lambda (g) (string= (elunit-group-name g) group-name)))
      (error "Group name is not unique (group=%s, suite=%s)"
             group-name (elunit-suite-name suite)))
    (push! group (elunit-suite-r-groups suite))))

(defstruct elunit-group
  "Structure describing a test.  Created by `create-elunit-group'.
The slots are:
NAME: group name, a string
DOCTRING: group docstring, a string
SUITE: suite to which this group belongs, elunit-suite
R-TESTS: list of TESTS, in reverse order
GROUP-SETUP: function to use, if any, to setup all tests in this group.
TEST-SETUP: function to use, if any, to wrap around each test"
  name
  docstring
  suite
  r-tests
  group-setup
  test-setup)

(defun create-elunit-group (group-name &rest rest)
  (apply 'make-elunit-group
         (append (list :name group-name) rest)))

(defun elunit-group-tests (group)
  (reverse (elunit-group-r-tests group)))

(defun elunit-group-add-test (group test)
  "Add TEST to a GROUP. All test names must be unique per group."
  (let ((test-name (elunit-test-name test)))
    (when (find-first (elunit-group-r-tests group)
            (lambda (i) (eq (elunit-test-name i) test-name)))
      (error "Test name is not unique (test=%s, group=%s, suite=%s)"
             test-name (elunit-group-name group)
             (elunit-suite-name (elunit-group-suite group))))
    (push! test (elunit-group-r-tests group))))

(defstruct elunit-test
  "Structure describing a test.  Created by `elunit-make-test'.
The slots are:
NAME: test name, a string
DOCSTRING: test docstring, a string
GROUP: group test belongs to, elunit-group
SOURCE: actual test source, as a string. may be nil if information
can't be extracted
TEST-FORMS: test forms, list of forms, fallback if source isn't available
FILE-NAME: file name test declaration is in, or nil
LINE-NUMBER: line number test declaration starts on, or nil"
  name
  docstring
  group
  source
  test-forms
  file-name
  line-number)

(defun elunit-test-suite (test)
  (elunit-group-suite (elunit-test-group test)))

(defun elunit-test-local-name (test)
  "Returns a string representation of the group and test within a suite."
  (concat (elunit-group-name (elunit-test-group test)) 
	  "+" (elunit-test-name test)))
  
(defun elunit-test-absolute-name (test)
  "Returns a string representation of a suite, group and test set."
  (concat (elunit-suite-name (elunit-test-suite test)) "+"
	  (elunit-test-local-name test)))

(defstruct elunit-result
  "Structure describing the results from running a test.  Created by
`elunit-run-test'.
The slots are:
TEST-OBJECT: test which results refer to, `elunit-test'
STATUS: one of `success', `failure', `not-implemented' or `error'.
ERROR-OBJECT: error data, if any."
  test-object
  status
  test-time
  error-object)

;;; XXX - move this somewhere else
(defmacro elunit-symbol-to-string (symbol-or-string)
  (let ((s* (gensym)))
    `(let ((,s* ,symbol-or-string))
       (if (symbolp ,s*)
	   (symbol-name ,s*)
	 ,s*))))
(defalias 'sym-to-str 'elunit-symbol-to-string)

;;;
;;; Public/Interactive
;;;

(defmacro defsuite (suite-name* &rest suite-forms)
  "Create a new suite.  Previous definitions are replaced.
The form is:

 (defsuite SUITE-NAME
  [DOCSTRING]
  SUITE-DECL*
  GROUP*)

SUITE-DECL := SUITE-SETUP | SUITE-TEST-SETUP
GROUP := (group NAME [DOCSTRING] GROUP-DECL* TEST*)
GROUP-DECL := GROUP-SETUP | GROUP-TEST-SETUP
TEST := (test NAME [DOCSTRING] TEST-FORMS*)"
  (let* ((suite-name (sym-to-str suite-name*))
	 (suite (elunit-process-suite suite-name suite-forms)))
    (message "Added suite %s with %d test"
             (elunit-suite-name suite) (length (elunit-suite-tests suite)))))

(defun elunit (suite-name)
  "Ask for a suite-name, run all suite tests, and display the results."
  (interactive
   (list (completing-read
          (format "Run suite (%s): " (elunit-sensible-default-suite))
          (elunit-list-of-suites)
          nil t nil nil (elunit-sensible-default-suite))))
  (unless (elunit-find-suite suite-name)
    (error "Can't find suite `%s' -- not running tests" suite-name))
  (setq *elunit-last-suite-ran* suite-name) ;save choice
  (elunit-run-suite suite-name))

(defun elunit-test-all-suites ()
  "Run all tests for all suites and display summary output."
  (interactive)
  (elunit-run-suites *elunit-suites*))

(defun elunit-remove-suite (suite-name)
  "Remove suite specified by SUITE-NAME."
  (interactive
   (list (completing-read
          "Remove suite: " (elunit-list-of-suites) nil)))
  (if (not (elunit-find-suite suite-name))
      (message "Not removing: non-existant suite (suite=%s)" suite-name)
    (elunit-remove-suite1 suite-name)
    (message "Removed suite (suite=%s)" suite-name)))

(defun elunit-remove-all-suites ()
  "Remove all suites."
  (interactive)
  (setq *elunit-suites* ())
  (message "Removed all suites"))

;;;
;;; General support
;;;

(defun elunit-sensible-default-suite ()
  "Returns a string of the name of a generally-sane default
suite (defaulting to the last suite ran, if it still exists) or nil."
  (or (and *elunit-last-suite-ran*
           (elunit-find-suite *elunit-last-suite-ran*)
           *elunit-last-suite-ran*)
      (and *elunit-suites*
	   (elunit-suite-name (first *elunit-suites*)))))

(defun elunit-list-of-suites (&optional suites)
  "Returns list of suite names identified by SUITS as strings.  If SUITES
is not specified all the suites are used."
  (mapcar (lambda (suite) (elunit-suite-name suite))
          (or suites *elunit-suites*)))

(defun elunit-find-suite (suite-name)
  "Returns a SUITE identified by SUITE-NAME or nil."
  (find-first *elunit-suites*
    (lambda (i) (string= (elunit-suite-name i) suite-name))))

(defun elunit-remove-suite1 (suite-name)
  "Removes SUITE identified by SUITE-NAME, if it exists."
  (delete-if! (lambda (suite)
		(string= (elunit-suite-name suite) suite-name))
	      *elunit-suites*))

(defun elunit-add-suite (suite)
  (elunit-remove-suite1 (elunit-suite-name suite))
  (push suite *elunit-suites*))

(defun elunit-show-source (file-name line-number)
  "Finds an existing FILE and moves to LINENO honoring
`elunit-show-source-in-other-window'.  If FILE does not exist an error is
signaled."
  (if (file-exists-p file-name)
      (if elunit-source-in-other-window
          (find-file-other-window file-name)
        (find-file file-name))
    (error "Can't find source: %s does not exist"))
  (goto-line line-number))

(defun elunit-debug-test (test)
  "Run a single TEST with edebug enabled."
  (let* ((debug-on-error t)
         (group (elunit-test-group test))
         (suite (elunit-group-suite group)))
    (elunit-run-setup-for-suite suite
      (elunit-run-setup-for-group group
        (elunit-run-test1 test 'debug))))
  ;; if we make it here, must have been successful
  (message "No errors for test: %s" (elunit-test-absolute-name test)))

(defmacro with-elunit-result-buffer (name &rest body)
  "Handle setup/display of a buffer named NAME and execute BODY.  Support
function `+insert' is provided via flet."
  (declare (indent 1))
  `(progn
    (unless (get-buffer ,name)
      (with-current-buffer (get-buffer-create ,name)
        (view-mode t)
        (setq view-exit-action 'kill-buffer)))
    (with-current-buffer ,name
      (let (buffer-read-only
            (old-window (selected-window)))
        (delete-region (point-min) (point-max))
        (if elunit-results-in-other-window
            (switch-to-buffer-other-window (current-buffer))
          (switch-to-buffer (current-buffer)))
        (flet ((+insert (format &rest data)
                        (with-current-buffer ,name
                          (goto-char (point-max))
                          (insert (apply 'format* format data))
                          (sit-for 0))))
          ,@body)
        (if elunit-keep-window-selection
            (select-window old-window))
        ))))

(defun elunit-time-difference-in-seconds (new-time old-time)
  "Given two times, as from `current-time', returns a floating-point
representation of the time difference in seconds."
  (let ((diff (time-subtract new-time old-time)))
    (if (/= 0 (car diff)) ; "avoid overflows"
        (error "I can't handle this: %S" diff))
    (+ (nth 1 diff) (/ (float (nth 2 diff)) 1000000))))
(defalias 'elunit-td-in-seconds 'elunit-time-difference-in-seconds)

(defun elunit-calculate-result-stats (results)
  "Calculates statistics for RESULTS.
Returns a PLIST with the keys:
errors, failures, test-time"
  (let ((failures 0)
        (errors 0)
        (test-time 0))
    (dolist (result results)
      (case (elunit-result-status result)
        (failure (incf failures))
        ((error not-implemented) (incf errors)))
      (incf test-time (elunit-result-test-time result)))
    (list 'errors errors 'failures failures 'test-time test-time)))

(defun elunit-run-suite (suite-name)
  (with-elunit-result-buffer (format "*elunit:%s*" suite-name)
    (+insert "Testing suite: %s\n\n" suite-name)
    (let* ((suite (elunit-find-suite suite-name))
           (start-time (current-time))
           (results (elunit-run-tests-for-suite 
                     suite
                     (lambda (status result)
                       (+insert (elunit-progress-indicator status)))))
           (test-duration (elunit-td-in-seconds (current-time) start-time))
           (stats (elunit-calculate-result-stats results)))
      (+insert "\n\n\n") ;one \n is used to end "status line"
      (elunit-report-results results
                             (lambda (result) (+insert result))
                             (lambda () (+insert "\n\n")))
      (+insert "\n\n\n")
      (+insert "%d tests, %d errors, %d failures in %.2fs (%.2fs in tests)."
               (length results)
               (plist-get stats 'errors)
               (plist-get stats 'failures)
               test-duration
               (plist-get stats 'test-time))
      )))

(defun elunit-run-suites (suite-list &optional show-details)
  (with-elunit-result-buffer "*elunit*"
    (let (all-results
          (start-time (current-time)))
      (+insert "Testing all suites:\n\n")
      (dolist (suite suite-list)
        (let ((suite-name (elunit-suite-name suite)))
          (+insert "%s: [%s]\n  " suite-name
		   (elink "run suite"
			  (lexical-let
			      ((suite-name* suite-name))
			    (lambda () (elunit suite-name*)))
                          :link-face 'elunit-run-suite-link-face))
          (push (elunit-run-tests-for-suite
                 suite
                 (lambda (status result)
                   (+insert (elunit-progress-indicator status))))
                all-results)
          (+insert "\n\n")))
      (let* ((results (apply 'append all-results))
             (stats (elunit-calculate-result-stats results))
             (test-duration (elunit-td-in-seconds (current-time) start-time)))
        (+insert "\n")
        (+insert "%d tests, %d errors, %d failures in %.2fs (%.2fs in tests)."
                 (length results)
                 (plist-get stats 'errors)
                 (plist-get stats 'failures)
                 test-duration
                 (plist-get stats 'test-time)))
      )))

;;;
;;; Suite processing and creation
;;;

;; XXX -- needs better integration
(defun elunit-process-setup (forms after-declarations)
  (when after-declarations
    (error "suite-setup must come in declaration section"))
  (unless forms
    (error "setup form is missing"))
  `(lambda () ,@forms))

(defun elunit-process-suite (suite-name* suite-forms)
  (let* ((suite-name (sym-to-str suite-name*))
	 (suite (create-elunit-suite suite-name))
         after-declarations)
    ;; capture doc-string, if any
    (setf (elunit-suite-docstring suite)
          (if (stringp (car suite-forms)) (pop suite-forms) ""))
    (dolist (form suite-forms)
      (let ((suite-directive (car-safe form))
            (directive-forms (cdr-safe form)))
        (case suite-directive
          (suite-setup
           (setf (elunit-suite-suite-setup suite)
                 (elunit-process-setup directive-forms after-declarations)))
          (suite-test-setup
           (setf (elunit-suite-test-setup suite)
                 (elunit-process-setup directive-forms after-declarations)))
          (group
	   (setq after-declarations t)
	   (elunit-suite-add-group suite
				   (elunit-process-group 
				    directive-forms suite suite-name)))
          (t
           (error "Unexpected directive (directive=%s suite=%s)"
                  suite-directive suite-name))
          )))
    (elunit-add-suite suite)
    suite))

(defun elunit-process-group (body suite suite-name)
  (let* ((group-name (sym-to-str (car body)))
         (docstring  (if (stringp (car body)) (pop body) ""))
         (group-forms (cdr body))
         (group (create-elunit-group group-name
                                     :docstring docstring
                                     :suite suite))
         after-declarations)
    (dolist (form group-forms)
      (case (car-safe form)
        (group-setup
         (setf (elunit-group-group-setup group)
               (elunit-process-setup (cdr form) after-declarations)))
        (group-test-setup
         (setf (elunit-group-test-setup group)
               (elunit-process-setup (cdr form) after-declarations)))
        (test
	 (setq after-declarations t)
	 (elunit-group-add-test group
				(elunit-process-test
				 (cdr form)
				 group group-name suite-name)))
	(t
         (error "Unexpected directive (directive=%s group=%s+%s)"
                (car form) (elunit-suite-name suite) group-name))))
    group))

(defun read-all-forms (source)
  (cdr (read (format "(read-all-forms %s)" source))))

(defun elunit-process-test (body group group-name suite-name)
  "Returns a new TEST."
  (let* ((test-name (sym-to-str (pop body)))
         (docstring (if (stringp (car body)) (pop body) ""))
         (test-forms body)
         (source-region (elunit-find-test-source
			 suite-name group-name test-name))
         (source (if source-region
                     (save-excursion
                       (goto-char (car source-region))
                       (buffer-substring-no-properties
                        (point) (cdr source-region)))))
         (file-name (and source ;only trust if we can read source
			 (buffer-file-name)))
         (line-number (if source-region
                          (save-excursion
                            (goto-char (car source-region))
                            (line-number-at-pos)))))
    (if (not source)
        (message "No source found (test=%s+%s+%s)"
                 suite-name group-name test-name)
      ;; make sure we grabbed the correct source
      (unless (equal (format "%S" test-forms)
                     (format "%S" (read-all-forms source)))
        (error "Extracted source mismatch (test=%s+%s+%s src=%S actual=%S)"
               suite-name group-name test-name
               (read-all-forms source) test-forms)))
    (make-elunit-test :name test-name
                      :docstring docstring
                      :group group
                      :source source
                      :test-forms test-forms
                      :file-name file-name
                      :line-number line-number)))

;;;
;;; Extract source code
;;;

(defun elunit-find-suite-source (suite-name)
  "Returns a cons of (SUITE-START . SUITE-END).
Issues:
1) Must be run in appropriate buffer context.
2) May potentially match something in a string or comment.
3) Will bail if multiple things match to provide early problem-detection."
  (save-excursion
    (save-match-data
      (goto-char (point-max)) ;search backwards, get "last defined first"
      (when
          (let ((suite-re
		 (format "(defsuite\\s-+[\"]?%s[\"]?\\(?:\\s-+\\|[\n)]\\)"
				  suite-name)))
            (if (re-search-backward suite-re nil 'move)
                (if (re-search-backward suite-re nil t)
                    (error "Duplicate suite-name detected (suite=%s)"
                           suite-name)
                  t)))
        ;; if here, point moved to suite-start, from above.
        (condition-case err
            (let ((start (point)))
              (read (current-buffer))
              (cons start (point)))
          (invalid-read-syntax nil))))))

(defun elunit-find-first-matching-sexp (function)
  "Returns (START-SEXPR . END-EXPR) of the first sexp for which FUNCTION
returns non-nil.  The expressions are read from the current buffer."
  (save-excursion
    (catch 'found-sexp
      (condition-case nil ;discard `end-of-file' errors
          (while t
            (let ((sexp (read (current-buffer))))
              (when (funcall function sexp)
                (throw 'found-sexp
                       (cons (save-excursion
                               (forward-sexp -1)
                               (point))
                             (point))))))
        (end-of-file)))))

;; XXX - relies on elunit-find-suite-source
(defun elunit-find-group-source (suite-name group-name)
  "Returns nil or cons of (GROUP-START . GROUP-END)."
  (let ((region (elunit-find-suite-source suite-name)))
    (when region
      (save-excursion
        (save-restriction
          (narrow-to-region (1+ (car region)) (1- (cdr region)))
          (goto-char (point-min))
          (elunit-find-first-matching-sexp
           (lambda (s)
             (and (listp sexp)
                  (eq (first sexp) 'group)
                  (string= (sym-to-str (second sexp)) group-name))))
          )))))

;; XXX - relies on elunit-find-group-srouce
(defun elunit-find-test-source (suite-name group-name test-name)
  "Returns nil or cons of (TEST-START . TEST-END)."
  (let ((region (elunit-find-group-source suite-name group-name)))
    (when region
      (save-excursion
        (save-restriction
          (narrow-to-region (1+ (car region)) (1- (cdr region)))
          (goto-char (point-min))
          (let ((test-region
                 (elunit-find-first-matching-sexp
                  (lambda (s)
                    (and (listp sexp)
                         (eq (first sexp) 'test)
                         (string= (sym-to-str (second sexp)) test-name))))))
            ;; do not include test directive, name, or docstring in source
            (when test-region
              (goto-char (1+ (car test-region))) ;skip (
              (read (current-buffer)) ;directive
              (read (current-buffer)) ;name
              (ignore-errors
                ;; skip docstring, if present
                (let ((prev-point (point)))
                  (unless (stringp (read (current-buffer)))
                    (goto-char prev-point)))
                ;; advance whitespace nicely
                (read (current-buffer))
                (forward-sexp -1)
                (skip-chars-backward "\t ")) ;XXX - use skip-ws-backward?
              (cons (point) (1- (cdr test-region))))) ;skip )
          )))))

;;;
;;; Running setups
;;;

;; XXX - why isn't body a &rest?
(defmacro elunit-run-setup (setup body)
  "Poor-mans form of inside-out calling.  SETUP if a function which should
do something and call `+yield', at which point BODY is execute.  If setup
does not manually yield an implict yield will be assumed."
  (declare (indent 2))
  (let ((setup-function- (gensym))
        (yielded- (gensym))
        (result- (gensym)))
    `(let ((,setup-function- ,setup)
           ,result- ,yielded-)
       (flet ((+yield
               ()
               (setq ,yielded- t)
               ;; sneak out result early
               (setq ,result- (progn ,body))))
         (when ,setup-function-
           (funcall ,setup-function-))
         (unless ,yielded-
           (+yield))
         ,result-))))

(defmacro elunit-run-individual-test-setups (test &rest body)
  "Run suite-test-setup and group-test-setup (in that order) for TEST."
  (declare (indent 1))
  (let ((group- (gensym))
        (suite- (gensym)))
    `(let* ((,group- (elunit-test-group ,test))
            (,suite- (elunit-group-suite ,group-)))
       (elunit-run-setup (elunit-suite-test-setup ,suite-)
           (elunit-run-setup (elunit-group-test-setup ,group-)
               ,@body)))))

(defmacro elunit-run-setup-for-suite (suite &rest body)
  (declare (indent 1))
  `(elunit-run-setup (elunit-suite-suite-setup ,suite)
       ,@body))

(defmacro elunit-run-setup-for-group (group &rest body)
  (declare (indent 1))
  `(elunit-run-setup (elunit-group-group-setup ,group)
       ,@body))

;;;
;;; Running tests
;;;

;; below setup level
(defun elunit-run-tests-for-suite (el-suite el-result-callback)
  (elunit-run-setup-for-suite el-suite
    (apply 'append
           (mapcar (lambda (el-group)
                     (elunit-run-tests-for-group el-group el-result-callback))
                   (elunit-suite-groups el-suite)))))

;; below setup level
(defun elunit-run-tests-for-group (el-group el-result-callback)
  "Run the tests for GROUP.  If RESULT-CALLBACK is non-nil it is taken to
be a function expecting two argument, the result status and results
object (`elunit-result').  Returns a list of RESULTS."
  (elunit-run-setup-for-group el-group
    ;; XXX - fix impl. (rebind-variables (variable-rebind-list)))
    (mapcar (lambda (el-test)
              (let ((result (elunit-run-test el-test)))
                ;; XXX - fix impl. (isolate-variables rebind-variables)
                (funcall el-result-callback
			 (elunit-result-status result) result)
                result))
            (elunit-group-tests el-group))))

;; below setup level
(defun elunit-defun-and-run (el-test &optional el-use-edebug)
  "Run TEST.  Uses a temporary buffer to enable edebug support.  If
USE-EDEBUG is non-nil then edebug is enabled.  Returns the result of
running the function; test problems are signaled as errors."
  (let ((original-buffer (current-buffer)))
    (with-temp-buffer
      (flet ((*elunit-test-defun*))
        (insert (format "(defun *elunit-test-defun* ()\n;;test: %s\n%s\n)"
                        (elunit-test-absolute-name el-test)
                        (elunit-test-source el-test))))
      (with-temp-message "" ;supress messages
        (eval-defun el-use-edebug))
      (with-current-buffer original-buffer ;restore test buffer
        (*elunit-test-defun*)))))

;; below setup level
(defun elunit-run-test1 (el-test &optional el-use-edebug el-ignore-source)
  "Run TEST.  Will try to use source first; if that fails test forms will
be used instead. If only test forms are available and USE-EDEBUG is non nil
an error is signaled.  If IGNORE-SOURCE is non-nil test source will, even
if present, be ignored."
  (elunit-isolation-cell
    (elunit-run-individual-test-setups el-test 
      ;; try to use source first, unless it's to be ignored,
      ;; fallback to using test forms
      (if (and (elunit-test-source el-test)
               (not el-ignore-source))
          (elunit-defun-and-run el-test el-use-edebug)
        (if el-use-edebug
            (error "Test has no source: can't edebug (test=%s)"
                   (elunit-test-absolute-name el-test))
          (eval (cons 'progn (elunit-test-test-forms el-test)))))
      )))

;; below setup level
(defun elunit-run-test (el-test)
  "Run TEST and return RESULT."
  (lexical-let
     ((start-time (current-time))
       (outcome (condition-case err
                    (progn
                      (elunit-run-test1 el-test)
                      (cons 'success nil))
                  (assertion-failed
                   (cons 'failure err))
                  (not-implemented
                   (cons 'not-implemented err))
                  (error ;oops!
                   (cons 'error err)))))
    (make-elunit-result :test-object el-test
                        :status (car outcome)
                        :test-time (elunit-td-in-seconds
                                    (current-time) start-time)
                        :error-object (cdr outcome))))

;;;
;;; Test isolation support
;;;

(defun constantp (sym)
  "Returns t if SYM, a symbol, refers a constant variable (or constant
symbol).  Normal errors apply if SYM is not bound."
  (or (condition-case nil
          (eval `(let ((,sym ,sym))))
        (setting-constant t))))

(defun variable-rebind-list ()
  "Returns a list suitable for use with `isolate-variables'.
Variables named -PROTECTED-, -PROTECT-LIST-, -SYM- and -BODY- will not
be protected."
  (let (-protected-)
    (mapatoms 
     (lambda (-sym-)
       (when (and (boundp -sym-)
                  (not (constantp -sym-))
                  ;; exist in macro only
                  (not (memq -sym-
			     '(-sym- -body- -protected- -protect-list-))))
         (push (list -sym- -sym-) -protected-))))
    -protected-))

(defmacro isolate-variables (-protect-list- &rest -body-)
  "Protect any changes to varibables made by forms in -BODY- by wrapping it
with a `let' rebinding all bound symbols.  If -PROTECT-LIST- is non-nil it
should be a list (not a variable!) of lists in the form (SYMBOL EXPR), such
as the result of `variable-rebind-list'.  The same rules as for
`variable-rebind-list' apply here."
  (declare (indent 0))
  (let ((-protected- (or -protect-list-
                         (variable-rebind-list))))
    `(let ((max-specpdl-size (+ max-specpdl-size ,(length -protected-))))
       (let ,-protected-
         ,@-body-))))
      
(defmacro elunit-isolation-cell (&rest body)
  "Provide basic test isolation."
  (declare (indent 0))
  `(save-match-data
     (save-selected-window
       (save-window-excursion
         (with-temp-buffer
           ,@body
           )))))

;;;
;;; Output/report functions
;;;

(defun elunit-progress-indicator (status)
  "Returns the appropriate 'progress indicator' for STATUS. See
`elunit-progress-indicators'."
  (let* ((props (plist-get elunit-progress-indicators status))
         (text (car props))
         (face (cdr props)))
    (if face (propertize text 'face face) text)))

(defun elunit-report-results (results report-callback separator-callback)
  "Generate a report for all RESULTS calling REPORT-CALLBACK (one argument,
the report as a string) or SEPARATOR-CALLBACK (no arguments) as
appropriate."
  (let ((problem-index 0))
    (dolist (result results)
      (unless (eq (elunit-result-status result) 'success)
        (when (> (incf problem-index) 1)
          (funcall separator-callback))
        (funcall report-callback
                 (elunit-report-for-result result problem-index)))
      )))
    
(defun elunit-report-for-result (result index)
  "Returns a report, as a string, for RESULT."
  (let ((handler (cdr (assoc (elunit-result-status result) elunit-report-handlers))))
    (funcall handler result index)))

(defun* elink (text callback &key link-face)
  (let ((map (make-sparse-keymap))
        (callback (if (and (functionp callback) ; XXX - a cleaner way?
                           (not (commandp callback)))
                      (lexical-let ((callback* callback))
                        (lambda () (interactive) (funcall callback*)))
                    callback)))
    (define-key map [return] callback)
    (define-key map [mouse-2] callback)
    (propertize text
                'face link-face
                'mouse-face 'highlight
                'keymap map)))
  
(defun elunit-report-attribute (name value)
  (format "%13s: %s\n" name value))

(defun elunit-report-heading (message index test)
  (let ((file-name (elunit-test-file-name test))
	(line-number (elunit-test-line-number test)))
    (let ((source-link
	   (if (elunit-test-source test)
	       (elink (format "%s:%s" file-name line-number)
		      (lexical-let ((file-name* file-name)
				    (line-number* line-number))
			(lambda ()
			  (elunit-show-source file-name* line-number*)))
		      :link-face 'elunit-source-link-face)
	     "UNKNOWN SOURCE")))
      (format* "%d) %s: %s [%s]\n"
	       index message (elunit-test-local-name test) source-link))))

(defun elunit-report-debug-link (test)
  (format* "   [%s]\n"
	   (if (elunit-test-source test)
	       (elink (format "debug %s"
			      (elunit-test-absolute-name test))
		      (lexical-let ((test* test))
			(lambda () (elunit-debug-test test*)))
		      :link-face 'elunit-debug-link-face)
	     "NO SOURCE")))

(defmacro elunit-generate-report (test &rest body)
  "Generate a report. Returns a string."
  (declare (indent 0))
  (let ((out- (gensym)))
    `(let ((,out- ""))
       (flet ((+out (more) (setq ,out- (concat ,out- more)))
              (+heading (message index)
                        (+out (elunit-report-heading message index ,test)))
              (+attribute (name value)
                          (+out (elunit-report-attribute name value)))
              (+debug-link ()
                           (+out (elunit-report-debug-link ,test))))
         ,@body
         ,out-))))

(defun elunit-failure-report (result index)
  (let* ((test (elunit-result-test-object result))
         (error-data (cdr (elunit-result-error-object result))))
    (elunit-generate-report test
      (+heading "Failure" index)
      (+attribute "Assertion" (elunit-assertion-error-message error-data))
      (+attribute "Expected" (elunit-assertion-error-expected error-data))
      (+attribute "Actual" (elunit-assertion-error-actual error-data))
      (if (elunit-test-source test)
          (+debug-link)))))

(defun elunit-error-report (result index)
  (let* ((test (elunit-result-test-object result))
         (error-object (elunit-result-error-object result)))
    (elunit-generate-report test
      (+heading "Error" index)
      (+attribute "Error" (error-to-pretty-string error-object))
      (if (elunit-test-source test)
          (+debug-link)))))

(defun elunit-not-implemented-report (result index)
  (let* ((test (elunit-result-test-object result)))
    (elunit-generate-report test
      (+heading "Not Implemented" index))))


(provide 'elunit)
