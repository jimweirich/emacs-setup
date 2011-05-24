;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-testing
;;; Purpose: Quick and easy functions for running TDD tests.
;;; ==================================================================

(require 'compile)
(require 'toggle)
(require 'find-in-parent-dir)

;;; Name of the asynchronous test process.
(defconst jw-test-process-name "*test-process*")

;;; Name of the test process output buffer.
(defconst jw-test-buffer-name "*testing*")

;;; Path to Ruby
(defconst jw-ruby-program "ruby")

;;; Name of the ruby debugging command to run the tests in debug mode.
(defconst jw-rdebug-command "rdebug")

;;; Name of the command to remove ANSI terminal cruft
(defconst jw-noansi-command  (concat (file-name-as-directory elisp-directory) "bin/noansi"))

;;; NOANSI option string (may be empty, or a pipe to the noansi command)
;;;(defconst jw-noansi-option  (concat " | " jw-noansi-command))
(defconst jw-noansi-option  "")

(defun jw-shell-env-setup ()
  (concat (jw-project-env-file )))

;;; Name of the rake command ot run the rake based tests.
(defconst jw-rake-command "rake")

;;; Options to be added to the ruby based test commands.
(defconst jw-test-options "-Ilib:test:.")

;;; Options to be added to the spec command.
(defconst jw-spec-options "")

;;; If true, run the tests with warnings turned on.
(defvar jw-test-warnings t)

;;; If true, display the testing buffer in a single window (rather
;;; than a split window).
(defvar jw-test-single-window nil)

;;; If true, keep the mode line in the compilation buffer.
(defvar jw-test-keep-mode-line nil)

;;; Name of the last buffer running a file or method style test.
(defvar jw-test-last-test-buffer nil)

;;; Regexp for matching Given/When/Then keywords
(defvar jw-given-keywords-pattern "\\(Given\\|When\\|Then\\)[ ({]")

;;; Regexp for matching test unit test names
(defvar jw-test-test-unit-pattern "^ *def *\\(test_[a-zA-Z0-9_]+\\(!\\|\\?\\)?\\)")

;;; Regexp for matching test unit test names
(defvar jw-test-shoulda-pattern "^ *should +\\('[^']+'\\|\"[^\"]+\"\\)")

(defvar jw-test-name-pattern
  "^ *\\(def\\|should\\|context\\|test\\) +\\(\\(test_[a-zA-Z0-9_]+[!?]?$\\)\\|'\\([^']+\\)'\\|\"\\([^\"]+\\)\"\\)" )

(defvar jw-test-all-pattern
  (concat "\\(" jw-test-name-pattern "\\|\\(^ *" jw-given-keywords-pattern "\\)\\)") )
;;;   (concat jw-test-test-unit-pattern "\\|" jw-test-shoulda-pattern))

(set-face-attribute (make-face 'test-heading1) nil
                    :family "arial"
                    :height 240
                    :background "#000000"
                    :foreground "#9999ff"
                    :weight 'bold)

(set-face-attribute (make-face 'test-heading2) nil
                    :family "arial"
                    :height 180
                    :background "#000000"
                    :foreground "#9999ff"
                    :weight 'bold)

(set-face-attribute (make-face 'test-success) nil
                    :family "arial"
                    :height 240
                    :background (if window-system "black" "#001100")
                    :foreground (if window-system "#33ff33" "white")
                    :weight 'bold)

(set-face-attribute (make-face 'test-failure) nil
                    :family "arial"
                    :height 240
                    :background (if window-system "black" "#110000")
                    :foreground (if window-system "ff3333" "white")
                    :weight 'bold)

(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\([0-9]+ examples?, 0 failures?.*\n\\)"
               (1 'test-success)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\(.* 0 failures, 0 errors.*\n\\)"
               (1 'test-success)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\(.* [1-9][0-9]* \\(failures?\\|errors?\\).*\n\\)"
               (1 'test-failure)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\(mountains are merely mountains\\|learn the rules so you know how to break them properly\\|remember that silence is sometimes the best answer\\|sleep is the best meditation\\|when you lose, don't lose the lesson\\|things are not what they appear to be: nor are they otherwise\\)"
               (0 'test-failure)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^Mountains are again merely mountains"
               (0 'test-success)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^= \\(.*\n\\)"
               (1 'test-heading1)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^==+ \\(.*\n\\)"
               (1 'test-heading2)))

(defun jw-test-warning-options()
  (if jw-test-warnings "-w " "") )

(defun jw-test-option-string()
  (concat (jw-test-warning-options) jw-test-options))

(defun jw-test-remove-stupid-messages ()
  (save-excursion
    (goto-char (max (- (point) 10) (point-min)))
    (while (re-search-forward "\\(WARNING\\|CONTEXT\\|NOTICE\\):.*
" nil t)
      (replace-match "_"))))

(defun jw-test-remove-crud ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\|\\[[0-9]+[a-zA-Z]\\)" nil t)
      (replace-match "") )))

(defun jw-test-compilation-buffer-hook-function ()
  "Remove carriage returns that occasionally pollute the compile buffer."
  (save-current-buffer
    (set-buffer (get-buffer jw-test-buffer-name))
    (let ((buffer-read-only nil))
      (jw-test-remove-crud))))

;; (add-hook 'compilation-filter-hook 'jw-test-compilation-buffer-hook-function)

(defun jw-test-build-command-line (args)
  "Define the command line needed to run the given command and arguments."
  (let ((proj-env (jw-project-env-file default-directory))
        (command  (mapconcat (lambda (x) x) args " ")))
    (if proj-env
        (concat ". " proj-env "; " command jw-noansi-option)
      (concat command jw-noansi-option))))

(defun jw-test-start-process (&rest args)
  "Start the test process using the compilation package."
  (compilation-start
   (jw-test-build-command-line args)
   nil
   (lambda (x) jw-test-buffer-name)))

(defun jw-test-start-debugging (&rest args)
  (rdebug (mapconcat (lambda (x) x) args " ")) )

(defun jw-prep-test-buffer ()
  "Prepare the test buffer for inserting output from the test process."
  (let ((buf (get-buffer jw-test-buffer-name)))
    (if buf (kill-buffer buf))
    (setq buf (get-buffer-create jw-test-buffer-name))
    (jw-push-buffer buf)))

(defun jw-koan-file-name-p (file-name)
  "Is the given file name a koan file?"
  (string-match "\\about\\b" (file-name-nondirectory file-name)) )

(defun jw-selenium-rc-file-name-p (file-name)
  "Is the given file name a koan file?"
  (string-match " *class *\[A-Za-z0-9\]+ *< *SeleniumTest" (buffer-string)) )

(defun jw-spec-file-name-p (file-name)
  "Is the given file name a spec file?"
  (string-match "\\bspec\\b" (file-name-nondirectory file-name)) )

(defun jw-test-file-name-p (file-name)
  "Is the given file name a test file?"
  (string-match "\\btest\\b" (file-name-nondirectory file-name)) )

(defun jw-given-file-name-p (file-name)
  "Is the given file name a given behavior/contract file?"
  (string-match "\\b\\(behavior\\|contract\\)\\b" (file-name-nondirectory file-name)) )

(defun jw-runnable-test-file-p (file-name)
  "Is the given file name a test or spec file?"
  (or (jw-test-file-name-p file-name)
      (jw-spec-file-name-p file-name)
      (jw-koan-file-name-p file-name)
      (jw-given-file-name-p file-name)
      (jw-selenium-rc-file-name-p file-name) ))

(defun jw-target-file-name (file-name)
  "Return the test file name associated with the given file name."
  (cond ((jw-runnable-test-file-p file-name) file-name)
        ((toggle-filename file-name toggle-mappings))
        (t file-name) ))

(defun jw-extract-name ()
  "Extract the name of the test from the match."
  (cond
   ((match-beginning 3)
    (buffer-substring (match-beginning 3) (match-end 3)))
   ((match-beginning 4)
    (buffer-substring(match-beginning 4) (match-end 4)))
   ((match-beginning 5)
    (buffer-substring (match-beginning 5) (match-end 5))) ))

(defun jw-find-test-method-name ()
  "Return the name of the current test method."
  (save-excursion
    (forward-line)
    (re-search-backward jw-test-name-pattern)
    (jw-extract-name)))

(defun jw-keyword-target-char ()
  (cond ((looking-at "Given") "G")
        ((looking-at "When") "W")
        ((looking-at "Then") "T")
        (t "T")))

(defun jw-find-given-line-marker ()
  "Return the line marker of the current test method."
  (save-excursion
    (forward-line)
    (move-beginning-of-line 1)
    (re-search-backward jw-given-keywords-pattern)
    (concat (jw-keyword-target-char) (number-to-string (line-number-at-pos)))))

(defun jw-find-existing-file (files)
  "Return the first file name in the list of files that exists, or nil."
  (cond ((null files) ())
        ((file-exists-p (car files)) (car files))
        (t (jw-find-existing-file (cdr files)))))

(defun jw-spec-command (buffer)
  "Return the name of the appropriate spec command to run for the given buffer."
  (let* ((default-directory (jw-find-project-top (buffer-file-name buffer))))
    (or (msg (concat "---: '" (getenv "HOME") "' ---"))
        (getenv "RSPEC_CMD")
        (jw-find-existing-file
         (list (concat default-directory "script/spec")
               (concat default-directory "vendor/plugins/rspec/bin/spec")))
        "$SPEC_COMMAND")))

(defun jw-find-spec-name ()
  "Return the name of the current test method."
  (save-excursion
    (forward-line)
    (re-search-backward "^ *it +['\"]\\([^\"]*\\)['\"] +do")
    (buffer-substring (match-beginning 1) (match-end 1))))

(defun jw-take-down-test-buffer ()
  "If the test buffer is in the front, take it down.
Make an attempt to get back to the last buffer that was used in a
test."
  (if (string-equal jw-test-buffer-name (buffer-name))
      (progn
        (kill-buffer jw-test-buffer-name)
        (if jw-test-last-test-buffer (pop-to-buffer jw-test-last-test-buffer)) )))

(defun jw-test-deal-with-mode-line ()
  "Remove the mode line if so configured.
The compilation buffer by default gets a mode line.  Remove it
unless the jw-test-keep-mode-line variable is true.  Otherwise
just skip past it and insert an extra line in preparation for the
test headers."
    (if (and (looking-at "-*-") (not jw-test-keep-mode-line))
        (let
            ((bol (save-excursion (beginning-of-line)(point)))
             (eol (save-excursion (end-of-line)(point))))
          (delete-region bol (+ eol 1)))
      (forward-line)
      (insert "\n")) )

(defun jw-test-insert-headers (buffer-name &rest headers)
  "Insert the given strings into the test buffer."
  (save-current-buffer
    (set-buffer (get-buffer buffer-name))
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (jw-test-deal-with-mode-line)
    (apply 'insert headers)
    (setq buffer-read-only t)
    (goto-char (point-max))
    (if jw-test-single-window (delete-other-windows)) ))

;;; -- Test Run Commands ---------------------------------------------

(defun jw-run-test-rake ()
  "Run the default rake command as a test."
  (interactive)
  (jw-prep-test-buffer)
  (jw-test-start-process jw-rake-command)
  (jw-test-insert-headers
   jw-test-buffer-name
   "= Test Rake\n"
   "== Target: default\n\n") )

(defun jw-run-test-units ()
  "Run the test:units rake command as a test."
  (interactive)
  (jw-prep-test-buffer)
  (jw-test-start-process jw-rake-command "test:units")
  (jw-test-insert-headers
   jw-test-buffer-name
   "= Test Rake\n"
   "== Target: test:units\n\n") )

(defun jw-run-test-functionals ()
  "Run the test:functionals rake command as a test."
  (interactive)
  (jw-prep-test-buffer)
  (jw-test-start-process jw-rake-command "test:functionals")
  (jw-test-insert-headers
   jw-test-buffer-name
   "= Test Rake\n"
   "== Target: test:functionals\n\n") )

(defun jw-run-test-integration ()
  "Run the test:integration rake command as a test."
  (interactive)
  (jw-prep-test-buffer)
  (jw-test-start-process jw-rake-command "test:integration")
  (jw-test-insert-headers
   jw-test-buffer-name
   "= Test Rake\n"
   "== Target: test:integration\n\n") )

(defun jw-run-test-cruise ()
  "Run the cruise rake command as a test."
  (interactive)
  (jw-prep-test-buffer)
  (jw-test-start-process jw-rake-command "cruise")
  (jw-test-insert-headers
   jw-test-buffer-name
   "= Test Rake\n"
   "== Target: cruise\n\n") )

(defun jw-run-spec-method (arg)
  "Run the current file as a test.
If this file name does not include the string 'test' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (bookmark-set "test")
  (jw-take-down-test-buffer)
  (let* ((file-name (buffer-file-name))
         (default-directory (jw-find-project-top file-name))
         (test-buffer (current-buffer)) )
    (if (not (jw-spec-file-name-p file-name))
        (progn
          (jw-toggle-buffer)
          (setq file-name (buffer-file-name)) ))
    (save-buffer)
    (setq jw-test-last-test-buffer (buffer-name))
    (let ((line-number (int-to-string (line-number-at-pos))))
      (cond ((null default-directory) (message "Cannot find project top"))
            ((null arg)
             (jw-prep-test-buffer)
             (jw-test-start-process
              (jw-spec-command test-buffer) jw-spec-options
              (concat file-name ":" line-number))
             (jw-test-insert-headers
              jw-test-buffer-name
              "= Individual Spec ...\n"
              "== In:    " default-directory "\n"
              "== File:  " (file-name-nondirectory file-name) "\n"
              "== Line:  " line-number "\n\n"))
            (t (jw-prep-test-buffer)
               (jw-test-start-debugging
                jw-rdebug-command (jw-test-option-string)
                file-name "--" (concat "--name " method-name))) ))))

(defun jw-run-spec-file (arg)
  "Run the current file as a spec.
If this file name does not include the string 'spec' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (jw-take-down-test-buffer)
  (if (string-equal jw-test-buffer-name (buffer-name))
      (kill-buffer jw-test-buffer-name))
  (let* ((file-name (jw-target-file-name (buffer-file-name)))
         (default-directory (jw-find-project-top file-name))
         (test-buffer (current-buffer)) )
    (cond ((null default-directory) (message "Cannot find project top"))
          (t
           (save-buffer)
           (setq jw-test-last-test-buffer (buffer-name))
           (jw-prep-test-buffer)
           (cond ((null arg)
                  (jw-test-start-process
                   (jw-spec-command test-buffer) jw-spec-options file-name)
                  (jw-test-insert-headers
                   jw-test-buffer-name
                   "= Spec File ...\n"
                   "== In:   " default-directory "\n"
                   "== File: " (file-name-nondirectory file-name) "\n\n") )
                 (t (jw-test-start-debugging
                     jw-rdebug-command (jw-test-option-string) file-name)) )))))

(defun jw-run-test-method (arg)
  "Run the current file as a test.
If this file name does not include the string 'test' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (bookmark-set "test")
  (jw-take-down-test-buffer)
  (let* ((file-name (buffer-file-name))
         (default-directory (jw-find-project-top file-name)) )
    (if (not (jw-test-file-name-p file-name))
        (progn
          (jw-toggle-buffer)
          (setq file-name (buffer-file-name)) ))
    (save-buffer)
    (setq jw-test-last-test-buffer (buffer-name))
    (let ((invoke-given (save-excursion
                          (re-search-backward jw-test-all-pattern)
                          (looking-at (concat " *" jw-given-keywords-pattern)))))
    (if invoke-given
        (let ((line-marker (jw-find-given-line-marker)))
          (jw-test-invoking-given-by-line arg file-name line-marker))
        (let ((method-name (jw-find-test-method-name)))
          (jw-test-invoking-test-by-name arg
                                         file-name
                                         (jw-fixup-method-name method-name)))))))

(defun jw-fixup-method-name (name)
  "Add .* to method names with spaces"
  (while (string-match " " name)
    (setq name (replace-match ".*" nil nil name)))
  name)

(defun jw-run-test-command ()
  jw-ruby-program)

(defun jw-test-invoking-test-by-name (arg file-name method-name)
  (cond ((null default-directory) (message "Cannot find project top"))
        ((null arg)
         (jw-prep-test-buffer)
         (jw-test-start-process
          (jw-run-test-command) (jw-test-option-string)
          file-name (concat "--name \"/" method-name "/\""))
         (jw-test-insert-headers
          jw-test-buffer-name
          "= Test Method ...\n"
          "== In:     " default-directory "\n"
          "== File:   " (file-name-nondirectory file-name) "\n"
          "== Method: " method-name "\n\n"))
        (t (jw-prep-test-buffer)
           (jw-test-start-debugging
            jw-rdebug-command (jw-test-option-string)
            file-name "--" (concat "--name " method-name)))))

(defun jw-test-invoking-given-by-line (arg file-name line-marker)
  (cond ((null default-directory) (message "Cannot find project top"))
        ((null arg)
         (jw-prep-test-buffer)
         (jw-test-start-process
          (jw-run-test-command) (jw-test-option-string)
          file-name (concat "--name \"/_" line-marker "_/\""))
         (jw-test-insert-headers
          jw-test-buffer-name
          "= Test Method ...\n"
          "== In:     " default-directory "\n"
          "== File:   " (file-name-nondirectory file-name) "\n"
          "== Line: " line-marker "\n\n"))
        (t (jw-prep-test-buffer)
           (jw-test-start-debugging
            jw-rdebug-command (jw-test-option-string)
            file-name "--" (concat "--name \"/_" line-marker "_/\""))) ))

(defun jw-run-given-method (arg)
  "Run the test matching the current line of the current file.
If this file name does not include the string 'test' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (bookmark-set "test")
  (jw-take-down-test-buffer)
  (let* ((file-name (buffer-file-name))
         (default-directory (jw-find-project-top file-name)) )
    (if (not (jw-given-file-name-p file-name))
        (progn
          (jw-toggle-buffer)
          (setq file-name (buffer-file-name)) ))
    (save-buffer)
    (setq jw-test-last-test-buffer (buffer-name))
    (let ((line-marker (jw-find-given-line-marker)))
      (cond ((null default-directory) (message "Cannot find project top"))
            ((null arg)
             (jw-prep-test-buffer)
             (jw-test-start-process
              (jw-run-test-command) (jw-test-option-string)
              file-name (concat "--name \"/_" line-marker "_/\""))
             (jw-test-insert-headers
              jw-test-buffer-name
              "= Test Method ...\n"
              "== In:     " default-directory "\n"
              "== File:   " (file-name-nondirectory file-name) "\n"
              "== Line: " line-marker "\n\n"))
            (t (jw-prep-test-buffer)
               (jw-test-start-debugging
                jw-rdebug-command (jw-test-option-string)
                file-name "--" (concat "--name \"/_" line-marker "_/\""))) ))))

(defun jw-run-test-file (arg)
  "Run the current file as a test.
If this file name does not include the string 'test' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (bookmark-set "test-file")
  (jw-take-down-test-buffer)
  (if (string-equal jw-test-buffer-name (buffer-name))
      (kill-buffer jw-test-buffer-name))
  (let* ((file-name (jw-target-file-name (buffer-file-name)))
         (default-directory (jw-find-project-top file-name)) )
    (cond ((null default-directory) (message "Cannot find project top"))
          (t
           (save-buffer)
           (setq jw-test-last-test-buffer (buffer-name))
           (jw-prep-test-buffer)
           (cond ((null arg)
                  (jw-test-start-process
                   (jw-run-test-command) (jw-test-option-string) file-name)
                  (jw-test-insert-headers
                   jw-test-buffer-name
                   "= Test File ...\n"
                   "== In:   " default-directory "\n"
                   "== File: " (file-name-nondirectory file-name) "\n\n") )
                 (t (jw-test-start-debugging
                     jw-rdebug-command (jw-test-option-string) file-name)) )))))

(defun jw-run-test-or-spec-method (args)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (cond ((jw-test-file-name-p file-name) (jw-run-test-method args))
          ((jw-spec-file-name-p file-name) (jw-run-spec-method args))
          ((jw-koan-file-name-p file-name) (jw-run-test-method args))
          ((jw-given-file-name-p file-name) (jw-run-given-method args))
          ((jw-selenium-rc-file-name-p file-name) (jw-run-test-method args))
          (t (error "not a test nor a spec")) )))

(defun jw-run-last-test-or-spec-method (args)
  (interactive "P")
  (bookmark-jump "test")
  (jw-run-test-or-spec-method args))

(defun jw-run-test-or-spec-file (args)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (cond ((jw-test-file-name-p file-name) (jw-run-test-file args))
          ((jw-spec-file-name-p file-name) (jw-run-spec-file args))
          ((jw-koan-file-name-p file-name) (jw-run-test-file args))
          ((jw-given-file-name-p file-name) (jw-run-test-file args))
          (t (error "not a test nor a spec")) )))

(defun jw-run-last-test-or-spec-file (args)
  (interactive "P")
  (bookmark-jump "test-file")
  (jw-run-test-or-spec-file args))

(defun jw-mark-for-testing (n)
  (interactive "P")
  (cond ((null n) (bookmark-jump "test"))
        (t (bookmark-set "test")) ))

;;; -- Toggle Enhancements -------------------------------------------

(defvar jw-test-toggle-style nil
"Buffer local variable describing the buffer's toggle style.")
(make-variable-buffer-local 'jw-test-toggle-style)

(defun jw-test-load-project-toggle-style ()
  "Set the buffer's toggle style from the project defaults."
  (let* ((togglerc (jw-find-in-parent-dir (buffer-file-name) ".togglerc")))
    (if (file-readable-p togglerc)
        (load-file togglerc))
    (if (null jw-test-toggle-style)
        (setq jw-test-toggle-style toggle-mapping-style))  ))

(defun jw-test-select-buffer-toggle-style ()
  "Set the buffer's toggle style.
If no style is currently selected, load the style from the
project .togglerc file."
  (if (null jw-test-toggle-style)
      (jw-test-load-project-toggle-style) )
  (setq toggle-mappings (toggle-style jw-test-toggle-style)) )

(defun jw-toggle-buffer ()
  "Enhanced version of the Ryan Davis's toggle-buffer function
Check for a .togglerc file at the top level of the project
directory.  If found, the file will be loaded before toggling,
allowing per-project toggle customizations."
  (interactive)
  (jw-test-select-buffer-toggle-style)
  (toggle-buffer) )

(defun jw-toggle-clear-buffer-styles ()
  "Clear all the buffer toggle style settings."
  (interactive)
  (let ((buffers (buffer-list)))
    (while buffers
      (if (local-variable-p 'jw-test-toggle-style (car buffers))
          (save-current-buffer
            (set-buffer (car buffers))
            (setq jw-test-toggle-style nil) ))
      (setq buffers (cdr buffers)) )
    (message "All buffer toggle styles are reset") ))

(defun jw-add-or-replace (name pair)
  (let* ((key (car pair))
         (new-value (cdr pair))
         (alist (eval name))
         (old-pair (assoc key alist)))
    (cond ((null old-pair) (add-to-list name pair))
          ((equal (cdr old-pair) new-value) ())
          (t (set name (cons pair (assq-delete-all key alist)))) ))
  (eval name) )

(defun jw-test-buffer-p ()
  (string-match "_test\." (buffer-name)))

(defun jw-spec-buffer-p ()
  (string-match "_spec\." (buffer-name)))

(defun jw-test-or-spec-buffer-p ()
  (or (jw-test-buffer-p)
      (jw-spec-buffer-p)))

(defun jw-code-test-split ()
  "Horizontally split between the code and test"
  (interactive)
  (delete-other-windows)
  (if (jw-test-or-spec-buffer-p)
      (jw-toggle-buffer))
  (split-window-horizontally)
  (other-window 1)
  (jw-toggle-buffer)
  (other-window 1))

(defun jw-split-or-toggle (n)
  "Toggle code/test buffer, or split windows with code and test (if prefix)"
  (interactive "P")
  (cond ((null n) (jw-toggle-buffer))
        (t (jw-code-test-split))))

;;; .togglerc specific functions -------------------------------------

;; The following functions are intended to be used in the project
;; specific .togglerc file.
;;
;; Example -- Set the style only:
;;
;;   (buffer-toggle-style 'jw-rails)
;;
;; Example -- Define a mapping and then select it:
;;
;;   (buffer-toggle-mapping
;;    '(project-style    . (("test/\\1_test.rb" . "lib/\\1.rb")
;;                           ("\\1_test.rb"      . "\\1.rb") )))
;;   (buffer-toggle-style 'project-style)

(defun buffer-toggle-style (style-name)
  "Set the testing toggle style for this buffer.
Normally called in the .togglerc file at the project level."
  (setq jw-test-toggle-style style-name) )

(defun buffer-toggle-mapping (mapping)
  "Define a project specific mapping.
Note: Make sure the mapping name is unique and doesn't class with
mappings from other projects."
  (jw-add-or-replace 'toggle-mapping-styles mapping))

(defun jw-test-toggle-warnings ()
  "Toggle the 'use warnings' flag for when testing"
  (interactive)
  (setq jw-test-warnings (not jw-test-warnings))
  (if jw-test-warnings
      (message "Warnings enabled in tests")
    (message "Warnings disabled in tests") ))

;;; Add the toggle command to the compilation mode, just make it
;;; delete the test buffer.

(defun jw-test-kill-test-buffer ()
  (interactive)
  (kill-buffer jw-test-buffer-name)
  (if jw-test-last-test-buffer
      (pop-to-buffer jw-test-last-test-buffer) ))

(define-key compilation-mode-map "\C-c\C-t" 'jw-test-kill-test-buffer)
