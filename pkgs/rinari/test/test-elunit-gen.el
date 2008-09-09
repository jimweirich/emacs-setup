(require 'elunit)

(testgen-config
 (sources ("elunit-gen.el" elunit-gen)))

;;; == Automatically Generated/Updated ==
(defsuite elunit-gen

  (group testgen-config1
    (test success (signal 'not-implemented nil))
    ) ;group testgen-config1

  (group elunit-read-and-capture
    (test success (signal 'not-implemented nil))
    ) ;group elunit-read-and-capture

  (group elunit-functions-to-test
    (test success (signal 'not-implemented nil))
    ) ;group elunit-functions-to-test

  (group elunit-groups-in-region
    (test success (signal 'not-implemented nil))
    ) ;group elunit-groups-in-region

  (group elunit-existing-tests
    (test success (signal 'not-implemented nil))
    ) ;group elunit-existing-tests

  (group elunit-generate-stale-defsuite
    (test success (signal 'not-implemented nil))
    ) ;group elunit-generate-stale-defsuite

  (group elunit-generate-defsuite
    (test success (signal 'not-implemented nil))
    ) ;group elunit-generate-defsuite

  (group elunit-update-tests
    (test success (signal 'not-implemented nil))
    ) ;group elunit-update-tests

  (group elunit-skeleton-test
    (test success-basic
      (assert-t (stringp (elunit-skeleton-test 'a))))
    ) ;group elunit-skeleton-test

  (group elunit-functions-in-file
    (test success (signal 'not-implemented nil))
    ) ;group elunit-functions-in-file

  (group elunit-functions-in-buffer
    (test success
      (with-test-buffer "(defun a)(defun b)"
        (assert-equal '(a b) (elunit-functions-in-buffer))))
    ) ;group elunit-functions-in-buffer

  ) ;suite elunit-gen

