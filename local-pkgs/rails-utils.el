(require 'project-top)

(defun schema ()
  "Open the schema file in this Rails project."
  (interactive)
  (project-top-find-file "db/schema.rb"))

(defun routes ()
  "Open the routes file in this Rails project."
  (interactive)
  (project-top-find-file "config/routes.rb"))
