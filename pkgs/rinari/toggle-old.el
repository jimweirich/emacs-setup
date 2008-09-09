;; toggle.el
;; by Ryan Davis
;
; blah.rb <-> test_blah.rb
; app/model/blah.rb <-> test/unit/blah_test.rb
; lib/blah.rb <-> test/test_blah.rb

(defcustom toggle-mappings
  '(("app/models/\\(.*\\).rb" . "test/unit/\\1_test.rb")
    ("test/unit/\\(.*\\)_test.rb" . "app/models/\\1.rb")
    ("app/controllers/\\(.*\\).rb" . "test/functional/\\1_test.rb")
    ("test/functional/\\(.*\\)_test.rb" . "app/controllers/\\1.rb")
    ("lib/\\(.*\\).rb" . "test/test_\\1.rb")
    ("test/test_\\(.*\\).rb" . "lib/\\1.rb")
    ("test_\\(.*\\).rb" . "\\1.rb")
    ("\\([^/]+\\).rb" . "test_\\1.rb"))
  "A list of (RE . TRANS) rules used by toggle-filename."
  :group 'toggle
  :type '(repeat (cons string string)))

(defun toggle-filename (path rules)
  "Transform a matching subpath in PATH as given by RULES.
Each element in RULES is a pair (RE . TRANS). If the regular
expression RE matches PATH, then replace-match is invoked with
TRANS. After the first successful match, this returns. If no rule
matches, it returns nil"
  (cond ((null rules) nil)
	((string-match (caar rules) path)
	 (replace-match (cdar rules) nil nil path))
	(t (toggle-filename path (rest rules)))))

; bpalmer suggests:
;(loop for (re . trans) in toggle-mappings
;      if (string-match re path) do (return (replace-match trans nil nil path)))

(defun toggle-buffer ()
  "Opens a related file to the current buffer using matching rules.
Matches the current buffer against rules in toggle-mappings. If a
match is found, switches to that buffer."
  (interactive)
  (let ((new-name (toggle-filename (buffer-file-name) toggle-mappings)))
    (if new-name
	(find-file new-name)
      (message (concat "Match not found for " (buffer-file-name))))))

(provide 'toggle)
