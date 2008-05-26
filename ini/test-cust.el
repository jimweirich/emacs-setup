(setq test-failures ())
(setq test-name "")
(setq test-list ())

(defun assertion-failure (message)
  (setq test-failures (cons (list test-name message) test-failures)))

(defun assert-equal (expected actual)
  (if (equal expected actual) ()
    (assertion-failure
     (concat "Expected: " (format "%s" expected) ", got: " (format "%s" actual)))))

(defun run ()
  (setq test-failures ())
  (run-tests test-list)
  test-failures)

(defun run-test (test)
  (setq test-name test)
  (eval (list (car tests))) )
        
(defun run-tests (tests)
  (cond ((null tests) nil)
        (t (run-test (car tests))
           (run-tests (cdr tests)) )))

(defun test-extract-file-lines ()
  (assert-equal '(("a" 23)) (jw-extract-file-lines "a:23"))
  (assert-equal '(("/a" 155))
                (jw-extract-file-lines "    /a:155:in `test_verbose'")))
  (assert-equal '(("a/b" 155))
                (jw-extract-file-lines "    a/b:155:in `test_verbose'")))
  (assert-equal '(("/Users/jim/working/rubyforge/raketest/test_application.rb" 155))
                (jw-extract-file-lines "    /Users/jim/working/rubyforge/raketest/test_application.rb:155:in `test_verbose'")))
)

(setq test-list (list 'test-extract-file-lines))

(run)
