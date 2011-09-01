;;; project-local-variables.el --- Set project-local variables from a file.

;; Copyright (C) 2011 Doug Alcorn

;; Author: Doug Alcorn
;; URL:
;; Version: 0.1
;; Created: 2011-03-29
;; Keywords: project, convenience, grep

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file allows you to quick grep a project tree. I uses all the
;; infrastrucutre of the built in grep and also
;; project-local-variables

(require 'project-local-variables)

(defun project-grep(regexp &optional files dir confirm)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR. By default, DIR is the project's root directory."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
                (dir (file-name-directory (plv-find-project-file default-directory "")))
;		(dir (read-directory-name "Base directory: " project-directory nil t))
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (rgrep regexp files dir confirm))

(provide 'project-grep)

;;; project-grep.el ends here
