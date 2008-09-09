(elunit-clear-suites)

(setq sample-root (rails-root "sample/app/"))

(defsuite rinari-file-switch-suite
  (find-view-test
   "make sure switching works"
   (find-file "sample/app/controllers/foo_controller.rb")
   (search-forward "@foo")
   (rinari-find-view)
   (assert (equal (concat sample-root "app/views/foo/foo.rhtml") (buffer-file-name)))
   (kill-buffer "foo.rhtml")
   (kill-buffer "foo_controller.rb"))

  (project-files-table-includes-bar-model
    "Make sure project-files-table populates models"
    (assert (equal (concat sample-root "app/models/bar.rb") (cdr (assoc "bar.rb" (project-files "sample/app"))))))

  (project-files-table-includes-foo-controller
    "Make sure project-files-table populates controllers"
    (assert (equal (concat sample-root "app/controllers/foo_controller.rb") (cdr (assoc "foo_controller.rb" (project-files "sample/app"))))))

  (project-files-table-includes-bar-view
    "Make sure project-files-table populates ambiguous views"
    (assert (equal (concat sample-root "app/views/foo/bar.rhtml") (cdr (assoc "bar.rhtml - foo" (project-files "sample/app"))))))

  (project-files-table-includes-bar-layout
    "Make sure project-files-table populates layouts"
    (assert (equal (concat sample-root "app/views/layouts/bar.rhtml") (cdr (assoc "bar.rhtml - layouts" (project-files "sample/app"))))))

  )

(defsuite rinari-html-fetching-suite
  )

(add-hook 'rinari-file-switch-suite-setup-hook (lambda () (populate-project-files-table "sample/app")))

