;;; elunit-gen.el --- Generate and maintain elunit tests

;; Copyright (C) 2006 Paul Nathan Stickney <pstickne@gmail.com>

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

;; Notes:
;; Very alpha. Will not work with tests.

;; Log:
;; 29 OCT 2006:
;;   Created.

(require 'elunit-support)

(defmacro testgen-config (&rest config)
  "Provided so buffer is evaluatable."
  `(apply 'testgen-config1 ',config))

(defun testgen-config1 (&rest config)
  (mapcar (lambda (source)
            (let ((file (car source))
                  (default (cadr source))
                  (fn-suite-map (cddr source)))
              (list file default
                    (mapcan (lambda (map)
                              (let ((suite (pop map)))
                                (mapcan (lambda (fn)
                                          (list fn suite))
                                        map)))
                            fn-suite-map))))
          (cdr (assoc 'sources config))))
  
(defun elunit-read-and-capture ()
  "Read an expression from the current buffer and return (SEXP . (START
. END). See `read' for what signals may be raised.  The point is moved."
  (cons (read (current-buffer))
        (cons (save-excursion
                (forward-sexp -1)
                (point))
              (point))))
(defalias 'read-and-capture 'elunit-read-and-capture)

(defun elunit-functions-to-test (sources)
  "Returns an ALIST of (suite-name . list-of-functions)."
  (let (to-test)
    (dolist (source sources)
      (let ((file (car source))
            (default-suite (cadr source))
            (fn-suite-map (cddr source)))
        (dolist (fn (elunit-functions-in-file file))
          (let ((suite (or (plist-get fn-suite-map fn)
                           default-suite)))
            ;; ensure suite container
            (unless (assoc suite to-test)
              (push (list suite) to-test))
            (append! (assoc suite to-test) fn)))))
    (reverse to-test)))

(defun elunit-groups-in-region (start end)
  "Returns a ALIST of groups in a specified region where each element
is (group . (start . end)).  This scans from the current buffer but
preserves the point."
  (save-excursion
    (save-restriction
      (let (groups)
        (narrow-to-region start end)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let* ((rc (read-and-capture))
                     (sexp (car rc))
                     (region (cdr rc)))
                (when (eq (car-safe sexp) 'group)
                  (let ((group (cadr sexp)))
                    (push (cons group region) groups)))))
          (end-of-file))
        (reverse groups)))))


(defun elunit-existing-tests ()
  "Returns a ALIST with elms of (suite . (?:(function . region))*).
This scans the buffer: the initial point is used and altered."
  (let (existing)
    (condition-case nil
        (while t
          (let* ((rc (read-and-capture))
                 (sexp (car rc))
                 (region (cdr rc)))
            (unless (eq (car-safe sexp) 'defsuite)
              (error "Expecting defsuite, found: %S" sexp))
            (let* ((suite (cadr sexp))
                   (functions-for-suite
                    (or (assoc suite existing)
                        (car (push (list suite) existing)))))
              (append! functions-for-suite
                       (mapcar (lambda (i) ;(fn . (start . end))
                                 (cons
                                  (car i)
                                  (cons (save-excursion
                                          (goto-char (cadr i))
                                          (skip-chars-backward " \t")
                                          (point))
                                        (cddr i))))
                               (elunit-groups-in-region (1+ (car region))
                                                        (1- (cdr region)))))
              )))
      (end-of-file))
    (reverse existing)))

(defun elunit-generate-stale-defsuite (suite existing)
  "EXISTING should be an ALIST of (function . (start . end))"
  (let ((out ""))
    (flet ((+write (text) (setq out (concat out text))))
      (+write ";;; == Stale Suite ==\n")
      (+write ";; No source specified to use this suite.\n")
      (+write ";; One or more tests may also be stale.\n")
      (+write (format "(defsuite %S\n\n" suite))
      (dolist (stale existing) ;(function . (start . end))
        (+write (buffer-substring (cadr stale) (cddr stale)))
        (+write (format " ;group %S\n\n" (car stale))))
      (+write (format "  ) ;suite %S" suite)))
    out))

(defun elunit-generate-defsuite (suite to-test existing)
  "Returns a string."
  (let ((out ""))
    (flet ((+write (text) (setq out (concat out text))))
      (+write ";;; == Automatically Generated/Updated ==\n")
      (+write (format "(defsuite %S\n\n" suite))
      (dolist (function to-test)
        (let ((region (cdr (assoc function existing))))
          (if region
              (progn
                (+write (buffer-substring (car region) (cdr region)))
                (+write (format " ;group %S" function))
                ;; remove used existing test
                (delete-if! (lambda (i)
                              (equal (car i) function))
                            existing))
            (+write (elunit-skeleton-test function)))
          (+write "\n\n")))
      ;; existing tests that are stale (renamed, moved, deleted)
      (when existing
        (+write "  ;; == Stale Tests ==\n")
        (+write "  ;; The corresponding functions for these\n")
        (+write "  ;; tests could not be found.\n")
        (+write (mapconcat
                 (lambda (stale)
                   (concat
                    (buffer-substring (cadr stale) (cddr stale))
                    (format " ;group %S" (car stale))))
                 existing "\n\n"))
        (+write "\n\n"))
      (+write (format "  ) ;suite %S" suite)))
    out))

(defun elunit-update-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((first-form
           (condition-case nil
               (let (found-first-form)
                 (while (not found-first-form)
                   (let ((sexp (read (current-buffer))))
                     (unless (eq (car-safe sexp) 'require)
                       (setq found-first-form sexp))))
                 found-first-form)
             (end-of-file))))
      (unless (eq (car-safe first-form) 'testgen-config)
        (error "This does not appear to be a valid auto-gen test file: %S"
               first-form))
      (let* ((sources (apply 'testgen-config1 (cdr first-form)))
             (to-test (elunit-functions-to-test sources))
             (after-config (point))
             (existing (elunit-existing-tests))
             (after-suites (point)))
        (let ((results
               (concat 
                (mapconcat (lambda (i)
                             (let ((suite (car i)))
                               (prog1
                                   (elunit-generate-defsuite 
                                    suite (cdr i) (cdr (assoc suite existing)))
                                 ;; remove used suite
                                 (setq existing 
                                       (remove-if (lambda (s)
                                                    (equal (car s) suite))
                                                  existing)))))
                           to-test
                           "\n\n\n")
                "\n\n"
                ;; existing was modified in previous mapconcat, eww
                (mapconcat (lambda (i)
                             (let ((suite (car i)))
                               (elunit-generate-stale-defsuite
                                (car i) (cdr i))))
                           existing
                           "\n\n\n"))))
          ;; if no errors, insert new contents
          (delete-region after-config after-suites)
          (insert (concat "\n\n" results)))
        ))))

(defun elunit-skeleton-test (function)
  (format (concat "  (group %S\n"
                  "    (test success (signal 'not-implemented nil))\n"
                  "    ) ;%S")
          function function))

(defun elunit-functions-in-file (file)
  "Returns list of functions declared with top-level defun in FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file) nil nil nil t)
    (elunit-functions-in-buffer)))

(defun elunit-functions-in-buffer ()
  "Returns list functions declared with top-level defun in the current
buffer."
  (save-excursion
    (let (found)
      (goto-char 0)
      (condition-case nil
          (while t
            (let ((sexp (read (current-buffer))))
              (when (and (eq 'defun (car-safe sexp))
                         (> (length sexp) 1))
                (push (cadr sexp) found))))
        (end-of-file))
      (reverse found))))

(provide 'elunit-gen)