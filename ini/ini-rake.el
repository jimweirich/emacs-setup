;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-rake
;;; Purpose: Rake / Emacs Integration
;;; ==================================================================

;;; Name of the buffer used to capture rake output.
(defconst jw-rake-buffer-name "*rake*")

;;; Name of the buffer used to capture rake tasks.
(defconst jw-rake-task-buffer-name "*rake-tasks*")

;;; Name of the rake command.
(defconst jw-rake-command "rake")

;;; Name of the rake process.
(defconst jw-rake-process-name "*rake-process*")

(defun jw-rake-tasks ()
  (if (get-buffer jw-rake-task-buffer-name)
      (kill-buffer jw-rake-task-buffer-name))
  (call-process "rake" nil jw-rake-task-buffer-name nil "-T"))

(defun jw-rake-parse-task-names ()
  (save-current-buffer
   (set-buffer jw-rake-task-buffer-name)
   (goto-char (point-min))
   (let ((result nil) (n 0))
     (while (re-search-forward "^rake \\([a-zA-Z0-9_:]+\\)" nil t)
       (setq n (+ n 1))
       (setq result (cons (list (buffer-substring (match-beginning 1) (match-end 1)) n)
                          result)) )
     result)))

(defun jw-rake-read-task ()
  "Fill the rake task buffer with the rake commands available at the moment."
  (interactive)
  (jw-rake-tasks)
  (completing-read "Rake Task: " (jw-rake-parse-task-names)))

(defun jw-rake-process-filter (proc string)
  "Process the output from the rake process.
Just insert into the rake output buffer, removing any carriage
returns along the way."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert (replace-regexp-in-string "" "" string))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun jw-start-rake-process (task)
  "Start the rake process using *cmd* and any optional *args*.
The process is run asynchronously and the output is placed in the
buffer identified by jw-rake-buffer-name."
  (let* ((process
          (start-process jw-rake-process-name
                         jw-rake-buffer-name jw-rake-command task)))
    (set-process-filter process 'jw-rake-process-filter) ))

(defun jw-run-rake ()
  (interactive)
  (if (get-buffer jw-rake-buffer-name) (kill-buffer jw-rake-buffer-name))
  (jw-start-rake-process (jw-rake-read-task))
  (pop-to-buffer jw-rake-buffer-name))

(global-set-key "\C-cr" 'jw-run-rake)
