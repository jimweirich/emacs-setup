;;;
;;;  Rinari
;;;
;;;  Rinari Is Not A Rails IDE
;;;  (c) 2006 Phil Hagelberg, Forrest Chang, Ryan Davis, Paul Stickne, and others
;;;  
;;; http://rinari.rubyforge.org
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ruby-mode)
(require 'inf-ruby)
(require 'rhtml-mode)

(require 'cl)
(require 'toggle)
(require 'which-func)
(require 'snippet)

(require 'rinari-abbrevs)
(require 'ruby-test)

;;; helpers for rinari-find-view
(defun rinari-name-components (name)
  (let ((case-fold-search nil))
	(labels ((rnc (in)
			(let ((ind (string-match "\\([A-Z][a-z0-9]+\\)[A-Z]" name in)))
			  (if (eq ind nil)
			      nil
			    (cons (downcase (match-string 1 name)) (rnc (match-end 1)))))))
	  (rnc 0))))

(defun rinari-make-dirname (comps)
  (reduce #'(lambda (str next) (concat str (concat "_" next))) comps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view toggling

(defun rinari-find-view ()
  (interactive)
  (let* ((funname (which-function))
 	 (cls (rinari-make-dirname (rinari-name-components funname)))
	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
 	 (appdir (file-name-directory (directory-file-name (file-name-directory (buffer-file-name))))))
    (find-file (concat appdir "views/" cls "/" fn ".rhtml"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-file-in-project

(defvar rinari-project-files-table ())

(defun populate-project-files-table (file)
  (if (file-directory-p file)
      (mapc 'populate-project-files-table (directory-files file t "^[^\.]"))
    (let* ((file-name (file-name-nondirectory file))
	   (existing-record (assoc file-name project-files-table))
	   (unique-parts (get-unique-directory-names file (cdr existing-record))))
      (if existing-record
	  (let ((new-key (concat file-name " - " (car unique-parts)))
		(old-key (concat (car existing-record) " - " (cadr unique-parts))))
	    (setf (car existing-record) old-key)
	    (setq project-files-table (acons new-key file project-files-table)))
	(setq project-files-table (acons file-name file project-files-table))))))

(defun get-unique-directory-names (path1 path2)
  (let* ((parts1 (and path1 (split-string path1 "/" t)))
	 (parts2 (and path2 (split-string path2 "/" t)))
	 (part1 (pop parts1))
	 (part2 (pop parts2))
	 (looping t))
    (while (and part1 part2 looping)
	   (if (equal part1 part2)
	       (setq part1 (pop parts1) part2 (pop parts2))
	     (setq looping nil)))
    (list part1 part2)))

(defun find-file-in-project (file)
  (interactive (list (if (functionp 'ido-completing-read)
			 (ido-completing-read "Find file in project: " (mapcar 'car (project-files)))
			 (completing-read "Find file in project: " (mapcar 'car (project-files))))))
  (find-file (cdr (assoc file project-files-table))))

(defun project-files (&optional file)
; uncomment these lines if it's too slow to load the whole project-files-table
;  (when (or (not project-files-table) ; initial load
;	    (not (string-match (rails-root) (cdar project-files-table)))) ; switched projects
    (setq project-files-table nil)
    (populate-project-files-table (or file (concat (rails-root) "/app")))
    project-files-table)

(defvar rinari-config-files
  '("config/environment.rb"
    "config/database.yml"
    "config/routes.rb"
    "config/deploy.rb"
    "db/schema.rb"))

(defun rinari-find-config-file (file)
  (interactive (list (if (functionp 'ido-completing-read)
			 (ido-completing-read "Find config file: " rinari-config-files)
			 (completing-read "Find config file: " rinari-config-files))))
  (find-file (concat (rails-root) file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rake

(defun rake-tasks()
   "Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
   (delq nil (mapcar '(lambda(line)
			(if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
		     (split-string (shell-command-to-string "rake -T") "[\n]"))))

(defun ruby-rake (task)
  "Run a rake command for the current project using compilation mode"
  (interactive (list (ido-completing-read "Run rake task: " (rake-tasks))))
  (compile (concat "rake " task)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; show-schema

(defun rinari-show-schema ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "set_table_name\\s['\"]\\([^'\"]+\\)" nil t))
  (let ((table-name (or (match-string 1)
			(substring (buffer-name) 0 -3))))
    (find-file (concat (rails-root) "/db/schema.rb"))
    (beginning-of-buffer)
    (search-forward-regexp (concat "create_table \"" table-name))))

(defun rinari-find-action (action &optional controller)
  (if controller
      (find-file (concat (rails-root "app/controllers/" controller ".rb"))))
  (beginning-of-buffer)
  (search-forward-regexp (concat "def\\s" action))
  (recenter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc

(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (if (equal dir  "/")
	nil
      (rails-root (expand-file-name (concat dir "../"))))))

(defun rinari-console ()
  (interactive)
  (run-ruby (concat (rails-root) "/script/console")))

(define-key ruby-mode-map
  "\C-c\C-s" 'rinari-console)
(define-key ruby-mode-map
  "\C-c\C-v" 'rinari-find-view)
(define-key ruby-mode-map
  "\C-c\C-t" 'toggle-buffer)
(define-key ruby-mode-map
  "\C-c\C-r" 'ruby-rake)
(define-key ruby-mode-map
  "\C-c\C-g" 'rinari-get-path)
(define-key ruby-mode-map
  "\C-c\C-f" 'rinari-find-config-file)
(define-key ruby-mode-map
  "\C-c\C-b" 'rinari-find-by-context)
(define-key ruby-mode-map
  "\C-x\C-\M-F" 'find-file-in-project)
(define-key ruby-mode-map
  [(control ?c) (control shift ?t)] 'ruby-test-function)
(define-key ruby-mode-map ; test this file
  [(control ?c) (control meta ?t)] (lambda nil (interactive) (compile (concat "ruby " (file-name-nondirectory buffer-file-name)))))
(define-key ruby-mode-map
  "\C-c\C-\M-t" 'ruby-test-file)

(defun rinari-get-path (path)
  (interactive "MPath: ")
  (switch-to-buffer (concat "rails-" path))
  (insert (shell-command-to-string (concat (rails-root) "/script/runner \"app = ActionController::Integration::Session.new; app.get '"
					   path "'; puts app.response.body\"")))
  (html-mode)
  ;; work around the fact that it inserts some random testing output
  (kill-region (search-backward "Loaded suite") (point-max)))

(provide 'rinari)
