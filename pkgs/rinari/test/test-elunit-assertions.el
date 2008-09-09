(require 'elunit)

(testgen-config
 (sources ("elunit-assertions.el" elunit-assertions)))

;;; == Automatically Generated/Updated ==
(defsuite elunit-assertions

  (group assert-object-to-string
    (test success (signal 'not-implemented nil))
    ) ;assert-object-to-string

  (group assert-t
    (test pass-1
      (assert-t t))
    (test fail-1
      (assert-equal "!!!"
                    (condition-case nil
                        (assert-t "true but not t")
                      (assertion-failed "!!!"))))
    ) ;group assert-t

  (group assert-nil
    (test pass-1
      (assert-nil nil))
    (test fail-1
      (assert-equal "!!!"
                    (condition-case nil
                        (assert-nil t)
                      (assertion-failed "!!!"))))
    ) ;group assert-nil

  (group assert-non-nil
    (test pass-1
      (assert-non-nil 'foo))
    (test fail-1
      (assert-equal "!!!"
                    (condition-case nil
                        (assert-non-nil nil)
                      (assertion-failed "!!!"))))
    ) ;group assert-non-nil

  (group assert-eq
    (test pass-1
      (assert-eq 'foo 'foo))
    (test fail-1
      (assert-equal "!!!"
                    (condition-case nil
                        (assert-eq "foo" "foo")
                      (assertion-failed "!!!"))))
    ) ;group assert-eq

  (group assert-equal
    ;; If this passes, we can reliable use assert-equal later
    (test pass-1
      (assert-equal "foo" "foo"))
    (test fail-1
      (unless (equal "!!!"
                     (condition-case nil
                         (assert-equal "foo" "bar")
                       (assertion-failed "!!!")))
        (error "No assertion raised")))
    ) ;group assert-equal

  (group assert-not-equal
    (test pass-1
      (assert-eq 'foo 'foo))
    (test fail-1
      (assert-equal "!!!"
                    (condition-case nil
                        (assert-not-equal "foo" "foo")
                      (assertion-failed "!!!"))))
    ) ;assert-not-equal

  ;; == Stale Tests ==
  ;; The corresponding functions for these
  ;; tests could not be found.
  (group assert-same-point
    (test pass-1
      (with-test-buffer ("hello world" "w")
        (assert-same-point
            (save-excursion (goto-char (point-max))))))
    (test fail-1
      (with-test-buffer ("hello world" "w")
        (assert-equal "!!!"
                      (condition-case nil
                          (assert-same-point (goto-char (point-max)))
                        (assertion-failed "!!!")))))
    ) ;group assert-same-point

  (group assert-no-errors
    (test pass-1
      (assert-no-errors
          "this doesn't signal an error"))
    (test fail-1
      (assert-equal "!!!"
                    (condition-case nil
                        (assert-no-errors (error "this errors"))
                      (assertion-failed "!!!"))))
    ) ;group assert-no-errors

  (group assert-error
    (test pass-1
      (assert-error wrong-number-of-arguments
        (message))) ;argument error
    (test fail-when-no-error
      (assert-equal "!!!"
                    (condition-case nil
                        (assert-error end-of-file "no error")
                      (assertion-failed "!!!"))))
    (test fail-with-incorrect-error
      (assert-equal "!!!"
                    (condition-case nil
                        (assert-error end-of-file (error "error"))
                      (assertion-failed "!!!"))))
    ) ;group assert-error

  ) ;suite elunit-assertions

