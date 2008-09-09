(require 'rinari)
(require 'behave)

(setq rinari-behave-root (expand-file-name (concat (file-name-directory load-file-name) "/../sample/")))

(context "rhtml syntax highlighting on bar.rhtml should highlight"
	 (tag rinari rhtml font-lock)
	 
	 (lexical-let ((file (concat rinari-behave-root "app/views/foo/bar.rhtml")))
	   (specify "HTML tags"
		    (expect (face-at "<div" file) equals font-lock-function-name-face))
	   (specify "HTML attributes"
		    (expect (face-at "style" file) equals font-lock-variable-name-face))
	   (specify "HTML strings"
;		    (expect (face-at "save" file) equals font-lock-string-face) ; single-quoted
		    (expect (face-at "meeting_length" file) equals font-lock-string-face)) ; double-quoted

	   (specify "HTML comments"
		    (expect (face-at "sample code" file) equals font-lock-comment-face))

	   (specify "ERB delimiters"
		    (expect '(erb-out-delim-face) equals (face-at "<%" file)))
	   (specify "strings in ERB"
;		    (expect 'font-lock-string-face member (face-at "button" file)) ; single-quoted string
		    (expect 'font-lock-string-face member (face-at "fizzbuzz" file))
		    (expect 'erb-exec-face member (face-at "fizzbuzz" file)))
	   (specify "ruby keywords in ERB"
		    (expect 'font-lock-keyword-face member (face-at "do" file)))
;; TODO:
;;	   (specify "constants in ERB"
;;		    (expect (face-at "Const" file) equals font-lock-constant-face))
	   (specify "instance variables in ERB"
		    (expect 'font-lock-variable-name-face member (face-at "@meeting" file)))
	   (specify "symbols in ERB"
		    (expect 'font-lock-constant-face member (face-at ":url" file)))))


;; (context "rhtml syntax highlighting on bar.rhtml should not highlight"
;; 	 (tag rinari rhtml font-lock)

;; 	 (specify "HTML tags in strings")
;; 	 (specify "HTML tags in ERB")

;; 	 (specify "ruby keywords in strings")
;;       (specify "module dereferencing as symbol")
;; 	 (specify "instance variables outside of ERB"))

;; (context "rhtml indentation should"
;; 	 (tag rinari rhtml indentation)

;; 	 (specify "indent deeper in nested HTML tags")
;; 	 (specify "indent deeper in an ERB block"))

(context "rhtml navigation should"
	 (tag rinari rhtml navigation)

	 (lexical-let ((file (concat rinari-behave-root "app/views/foo/bar.rhtml")))
	   (specify "find by context"
		    (find-file file)
		    (search-forward "partial")
		    (rinari-find-by-context)
		    (expect (file-name-nondirectory (buffer-file-name (current-buffer))) equals "_biz.rhtml")
		    (kill-buffer (current-buffer))
		    (search-forward "@choos")
		    (rinari-find-by-context)
;; TODO: absolute partials
;;		    (expect (file-name-nondirectory (buffer-file-name (current-buffer))) equals "_choo.rhtml")
		    (kill-buffer (current-buffer)))

	   (specify "toggle to action"
		    (find-file file)
		    (rhtml-find-action)
		    (expect (file-name-nondirectory (buffer-file-name (current-buffer))) equals "foo_controller.rb")
		    (kill-buffer (current-buffer)))

	   (specify "extract a partial"
		    (find-file file)
		    (search-forward "already-invited")
		    (beginning-of-line)
		    
		    (let ((partial-start (point)))
		      (search-forward "end ")
		      (beginning-of-line)
		      (extract-partial partial-start (point) "invited"))

 		    (switch-to-buffer "_invited.rhtml")
		    (beginning-of-buffer)
 		    (search-forward "These people have already ")
		    (set-buffer-modified-p nil)
 		    (kill-buffer (current-buffer))
		    (beginning-of-buffer)
 		    (search-forward "render :partial => 'invited'")
		    (revert-buffer nil t)
 		    (kill-buffer (current-buffer)))))

; (find-file "../sample/app/views/foo/bar.rhtml")
(provide 'rinari-spec)