;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-toggle
;;; Purpose: Setups for Ruby test file toggle
;;; ==================================================================

(require 'toggle)

;;; Note that we have switched the order of test/code files in these
;;; rules.  This allows the lib/*.rb rules to work properly.

(add-to-list
 'toggle-mapping-styles
 '(jw-rails . (("test/functional/\\1_test.rb"   . "app/controllers/\\1.rb")
               ("test/unit/helpers/\\1_test.rb" . "app/helpers/\\1.rb")
               ("test/unit/lib/\\1_test.rb"     . "lib/\\1.rb" )
               ("test/unit/\\1_test.rb"         . "app/models/\\1.rb")
               ) ))

(add-to-list
 'toggle-mapping-styles
 '(jw-postfix    . (("test/\\1_test.rb" . "lib/\\1.rb")
                    ("\\1_test.rb"      . "\\1.rb")
                    ) ))

(add-to-list
 'toggle-mapping-styles
 '(jw-prefix    . (("test/test_\\1.rb" . "lib/\\1.rb")
                   ("test_\\1.rb"      . "\\1.rb")
                   ) ))

(toggle-style 'jw-ruby)

;;; Simple insertion macro for .togglerc files.

(define-key text-mode-map "\C-ci"
  (lambda () 
    (interactive)
    (insert ";; (buffer-toggle-style 'jw-rails)\n")
    (insert ";; (buffer-toggle-style 'jw-postfix)\n") 
    (insert ";; (buffer-toggle-style 'jw-prefix)\n") ))

;;; Debugging ========================================================

(defun toggle-debug-loop (path rules)
  "Search the lookup rules for a toggle match displaying debug statements as you go."
  (cond ((null rules)
         (insert "END OF RULES\n")
         nil)
        ((string-match (caar rules) path)
         (insert (concat "Matching Rule: " (caar rules) "\n"))
         (let ((result (replace-match (cdar rules) nil nil path)))
           (insert "RESULT: " result)
           result) )
        (t
         (insert (concat "Failing  Rule: " (caar rules) "\n"))
         (toggle-debug-loop path (rest rules)))))

(defun toggle-debug (path)
  "Same as the standard toggle-filename, but prints debug messages."
  (interactive "fFile Name: ")
  (pop-to-buffer (set-buffer (get-buffer-create "*dbg*")))
  (goto-char (point-max))
  (insert "\n*************************************\n\n")
  (insert (format "LOOKING FOR %s\n" path))
  (insert (format "IN RULES %s\n\n" path toggle-mappings))
  (toggle-debug-loop path toggle-mappings))

(global-set-key "\C-ct\C-s" 'toggle-debug)
