(require 'elunit)
(require 'elunit-support)

(require 'rhtml-erb)

(put 'test 'lisp-indent-function 1)
(put 'setup 'lisp-indent-function 1)
(put 'group 'lisp-indent-function 1)

(defsuite xyz
  (group c
    (test a))
  (test a)
  )

(defsuite rhtml-erb
  
  (setup SETUP
    (message "Suite setup: PRE")
    (*next*)
    (message "Suite setup: POST"))
  
  (setup TEST
    (message "Test setup: PRE")
    (*next*)
    (message "Suite setup: POST"))

  (setup buffer
    (with-test-buffer ((nth 0 *setup-options*) (nth 1 *setup-options*))
      (*next*)))
  
  (group at-erb-tag
    (test basic
      (-> valid-tag-data (buffer (a 2))) # too messy

  (group at-erb-tag
    (test valid-tags
      (dolist (a '("<% tag %>"))
        (with-test-buffer (a 2)
          )))
        

  ;; ideas
  ;; 1) group used for categorizing tests,
  ;;    names generated like group-name+test-name
  ;; 2) each test uses different data. a test which uses the
  ;;    same data may be combined if appropriate
  (group at-erb-tag
    (test tag-start
      (setups (buffer "<% tag %>"))
      (assert-equal (cons (point-min) (point-max))
                    (rhtml-at-erb-tag-p)))
    (test with-errors
      (options "<% at tag start%>")
      ddfdf))
  ) ;group

  (at-erb-tag-p-1
   (with-test-buffer "<% at tag start %>"
     (assert-equal (cons (point-min) (point-max))
                   (rhtml-at-erb-tag-p))))
  
  (at-erb-tag-p-2
   (with-test-buffer "<%at tag start \r\n\t\v -%> "
     (assert-equal (cons (point-min) (1- (point-max)))
                   (rhtml-at-erb-tag-p))))
  
  (at-erb-tag-p-3
   (with-test-buffer " <% not at tag start %>"
     (assert-nil (rhtml-at-erb-tag-p))))
  
  (at-erb-tag-p-4
   (with-test-buffer "<% invalid tag >"
     (assert-nil (rhtml-at-erb-tag-p))))

  )  ;xx

  (skip-erb-tag
   (with-test-buffer "<% tag %>"
     (assert-non-nil (rhtml-skip-erb-tag))
     (assert-eq (point) (point-max))))

  ;; XXX - should also check return type
  (scan-erb-tag
   (with-test-buffer "<% tag %>"
     (assert-non-nil (consp (rhtml-scan-erb-tag)))
     (assert-eq (point) (point-max))))
  
  (scan-for-erb-tags
   (error "Not implemented"))

  (erb-tag-region-1
   (with-test-buffer "  <% a tag %>  "
     (goto-char 1) ; outside of tag
     (assert-nil (rhtml-erb-tag-region))))

  (erb-tag-region-2
   (with-test-buffer "sss<% mmm %>  "
     (skip-chars-forward "s") ; start of delim
     (assert-equal (cons 4 13)
                   (rhtml-erb-tag-region))
     (skip-chars-forward " <") ; middle of delim
     (assert-equal (cons 4 13)
                   (rhtml-erb-tag-region))
     (skip-chars-forward " m") ; middle of tag
     (assert-equal (cons 4 13)
                   (rhtml-erb-tag-region))))

  (union-region-containing-erb-tags
   (with-test-buffer "  <% one %>  <% two %>  "
     (assert-equal (cons 3 25) ;expand to one side
                   (rhtml-union-region-containing-erb-tags 8 25))))

  (region-has-erb-tag-p
   (with-test-buffer "  <% one %>  "
     (assert-nil (rhtml-region-has-erb-tag-p 1 9))
     (assert-non-nil (rhtml-region-has-erb-tag-p 3 12))))

  (widen-to-region
   (error "Not implemented"))

  ) ;suite
