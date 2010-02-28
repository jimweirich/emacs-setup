;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-rails
;;; Purpose: Setups for rails specific functionality
;;; ==================================================================


;;; Setup for dark fonts in rinari/rhtml mode

(defface erb-face
  `((t (:background "grey10")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "grey15")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)

(defface erb-out-delim-face
  `((t (:inherit erb-delim-face :weight bold :foreground "yellow")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-exec-delim-face
  `((t (:inherit erb-delim-face :weight bold :foreground "yellow")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

;;; I don't like the rinari abbrevs, so ignore them

(provide 'rinari-abbrevs)
(require 'rinari)

;;; The rinari find view function doesn't recognize .erb files.  Here's a replacement.

(defun jw-find-view ()
  (interactive)
  (let* ((funname (which-function))
 	 (cls (rinari-make-dirname (rinari-name-components funname)))
	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
 	 (appdir (file-name-directory (directory-file-name (file-name-directory (buffer-file-name))))))
    (find-file (jw-choose-file
                (list
                 (concat appdir "views/" cls "/" fn ".html.erb")
                 (concat appdir "views/" cls "/" fn ".rhtml"))))))

(define-key ruby-mode-map "\C-c\C-v" 'jw-find-view)

(defun jw-file-name-sans-extension (filename)
  (if (string-match "\.html\.erb$" filename)
      (substring filename 0 (- (length filename) 9))
    (file-name-sans-extension filename)))

(defun jw-find-action ()
  (interactive)
  (let ((action (jw-file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (find-file (rhtml-controller-name-from-view))
    (beginning-of-buffer)
    (search-forward-regexp (concat "def *" action))
    (recenter)))

(define-key rhtml-mode-map "\C-c\C-v" 'jw-find-action)

(defun routes ()
  (interactive)
  (find-file (concat (rails-root) "/config/routes.rb")))

(defun schema ()
  (interactive)
  (find-file (concat (rails-root) "/db/schema.rb")))
