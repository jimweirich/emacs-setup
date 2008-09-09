;; tcl-mode - A major-mode for editing tcl/tk scripts
;;
;; Copyright (C) 1993 Gregor Schmid 
;; Version 0.1
;; $Id: tcl-mode.el,v 1.1 2000/08/18 11:01:11 jim Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software

;; Please send bug-reports, suggestions etc. to
;;
;; 		schmid@fb3-s7.math.tu-berlin.de
;;
;; Any feedback is very welcome !

;; Credits
;; I took a good look at the c-mode that comes with GNU emacs 19.
;; Thank's to whoever wrote it.

;; This file was written with emacs using Jamie Lokier's folding mode
;; That's what the funny ;;{{{ marks are there for

;; Tcl-mode supports c-mode style formatting and sending of
;; lines/regions/files to a tcl interpreter. An interpreter (see
;; variable `tcl-default-application') will be started if you try to
;; send some code and none is running. You can use the process-buffer
;; (named after the application you chose) as if it were an
;; interactive shell. See the documentation for `comint.el' for
;; details.

;; Installation
;; Put tcl-mode somewhere into your load-path and add the lines
;; 	(autoload 'tcl-mode "tcl-mode" "Major mode for editing tcl-scripts." t)
;; 	(setq auto-mode-alist (cons '("\\.tcl$" . tcl-mode) auto-mode-alist))
;; to your .emacs .
;; This will put all files with the extension .tcl into tcl-mode after
;; loading.

;; Key-bindings
;; To see all the keybindings for folding mode, look at `tcl-setup-keymap'
;; or start `tcl-mode' and type `\C-h m'.
;; The keybindings may seem strange, since I prefer to use them with
;; tcl-prefix-key set to nil, but since those keybindings are already used
;; the default for `tcl-prefix-key' is `\C-x t'.
;; You can customise the keybindings either by setting `tcl-prefix-key'
;; or by putting the following in your .emacs
;; 	(setq tcl-mode-map (make-sparse-keymap))
;; and
;; 	(define-key tcl-mode-map <your-key> <function>)
;; for all the functions you need.

;; Variables
;; You may want to customize the following variables:
;; 	tcl-indent-level
;; 	tcl-always-show
;;	tcl-mode-map
;;	tcl-prefix-key
;;	tcl-mode-hook
;; 	tcl-default-application
;; 	tcl-default-command-switches

;; We need that !
(require 'comint)

;;{{{ variables

(defvar tcl-default-application "wish"
  "Default tcl/tk application to run in tcl-start-process")

(defvar tcl-default-command-switches nil
  "Command switches sfor tcl-default-application.
Should be a list of strings.")

(defvar tcl-process nil
  "Holds the current active tcl-process corresponding to current buffer.")

(defvar tcl-process-buffer nil
  "Buffer that holds tcl process associated with current buffer.")

(defvar tcl-always-show t
  "Make sure tcl-process-buffer is displayed after sending somethimg.")

(defvar tcl-mode-map nil
  "Keymap used with tcl-mode.")

(defvar tcl-prefix-key "\C-xt"
  "Prefix for all tcl-commands.")

(defvar tcl-mode-hook nil
  "Hooks called when tcl-mode fires up.")

(defvar tcl-region-start (make-marker)
  "Start of special region for communication.")

(defvar tcl-region-end (make-marker)
  "End of special region for communication.")

(defvar tcl-indent-level 4
  "Amount by which tcl subexpressions are indented.")

;;}}}
;;{{{ tcl-mode

(defun tcl-mode ()
  "Major mode for editing tcl-scripts.
The following keys are bound:
\\{tcl-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'tcl-mode)
  (setq mode-name "TCL")
  (set (make-local-variable 'tcl-process) nil)
  (set (make-local-variable 'tcl-process-buffer) nil)
  (make-local-variable 'tcl-default-command-switches)
  (set (make-local-variable 'indent-line-function) 'tcl-indent-line)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|;\\)[ \t]*\\)#")
  (or tcl-mode-map
      (tcl-setup-keymap))
  (use-local-map tcl-mode-map)
  (modify-syntax-entry ?# "<")
  (modify-syntax-entry ?\n ">")
  ;; look for a #!.../wish -f line at bob
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "#![ \t]*\\([^ \t]*\\)[ \t]+-f")
	(set (make-local-variable 'tcl-default-application)
	     (buffer-substring (match-beginning 1)
			       (match-end 1)))))
  (run-hooks 'tcl-mode-hook))

;;}}}
;;{{{ tcl-setup-keymap

(defun tcl-setup-keymap ()
  "Setup keymap for `tcl-mode'.
If the variable `tcl-prefix-key' is nil, the bindings go directly
to `tcl-mode-map', otherwise they are prefixed with `tcl-prefix-key'."
  (setq tcl-mode-map (make-sparse-keymap))
  (let ((map (if tcl-prefix-key
		 (make-sparse-keymap)
	       tcl-mode-map)))
  ;; indentation
  (define-key tcl-mode-map [delete] 'backward-delete-char-untabify)
  (define-key tcl-mode-map [?}] 'tcl-electric-brace)
  ;; communication
  (define-key map "\M-e" 'tcl-send-current-line)
  (define-key map "\M-r" 'tcl-send-region)
  (define-key map "\M-w" 'tcl-send-proc)
  (define-key map "\M-a" 'tcl-send-buffer)
  (define-key map "\M-q" 'tcl-kill-process)
  (define-key map "\M-u" 'tcl-restart-with-whole-file)
  (define-key map "\M-s" 'tcl-show-process-buffer)
  (define-key map "\M-h" 'tcl-hide-process-buffer)
  (define-key map "\M-i" 'tcl-get-error-info)
  (define-key map "\M-[" 'tcl-beginning-of-proc)
  (define-key map "\M-]" 'tcl-end-of-proc)
  (define-key map "\C-\M-s" 'tcl-set-tcl-region-start)
  (define-key map "\C-\M-e" 'tcl-set-tcl-region-end)
  (define-key map "\C-\M-r" 'tcl-send-tcl-region)
  (if tcl-prefix-key
      (define-key tcl-mode-map tcl-prefix-key map))
  ))

;;}}}
;;{{{ indentation

;;{{{ tcl-indent-line

(defun tcl-indent-line ()
  "Indent current line as tcl code.
Return the amount the indentation changed by."
  (let ((indent (tcl-calculate-indentation nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (save-excursion
      (while (eq (following-char) ?})
	(setq indent (max (- indent tcl-indent-level) 0))
	(forward-char 1)
	(if (looking-at "\\([ \t]*\\)}")
	    (progn
	      (delete-region (match-beginning 1) (match-end 1))
	      (insert-char ?  (1- tcl-indent-level))))))
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

;;}}}
;;{{{ tcl-calculate-indentation

(defun tcl-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as tcl code.
In usual case returns an integer: the column to indent to."
  (let ((pos (point)))
    (save-excursion
      (if parse-start
	  (setq pos (goto-char parse-start)))
      (beginning-of-line)
      (if (bobp)
	  (current-indentation)
	(forward-char -1)
	(if (eq (preceding-char) ?\\)
	    (+ (current-indentation)
	       (progn
		 (beginning-of-line)
		 (if (bobp)
		     (* 2 tcl-indent-level)
		   (forward-char -1)
		   (if (not (eq (preceding-char) ?\\))
		       (* 2 tcl-indent-level)
		     0))))
	  (forward-char 1)
	  (if (re-search-backward
	       "\\(^[^ \t\n\r]\\)\\|\\({\\s *\n\\)\\|\\(}\\s *\n\\)"
	       nil  t)
	      (+ (- (current-indentation)
		    (if (save-excursion
			  (beginning-of-line)
			  (and (not (bobp))
			       (progn
				 (forward-char -1)
				 (eq (preceding-char) ?\\))))
			(* 2 tcl-indent-level)
		      0))
		 (if (eq (following-char) ?{)
		     tcl-indent-level
		   0))
	    (goto-char pos)
	    (beginning-of-line)
	    (forward-line -1)
	    (current-indentation)))))))

;;}}}
;;{{{ tcl-electric-brace

(defun tcl-electric-brace (arg)
  "Insert `}' and indent line."
  (interactive "P")
  (insert-char ?} (prefix-numeric-value arg))
  (tcl-indent-line)
  (blink-matching-open))

;;}}}

;;}}}
;;{{{ searching

;;{{{ tcl-beginning-of-proc

(defun tcl-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a proc (or similar).
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
	(ret t))
    (if (and (< arg 0)
	     (looking-at "^[^ \t\n#][^\n]*{[ \t]*$"))
	(forward-char 1))
    (while (< arg 0)
      (if (re-search-forward "^[^ \t\n#][^\n]*{[ \t]*$" nil t)
	  (setq arg (1+ arg)
		found t)
	(setq ret nil
	      arg 0)))
    (if found
	(beginning-of-line))
    (while (> arg 0)
      (if (re-search-backward "^[^ \t\n#][^\n]*{[ \t]*$" nil t)
	  (setq arg (1- arg))
	(setq ret nil
	      arg 0)))
    ret))

;;}}}
;;{{{ tcl-end-of-proc

(defun tcl-end-of-proc (&optional arg)
  "Move forward to next end of proc (or similar).  With argument,
do it that many times. Negative argument -N means move back to Nth
preceding end of proc.

This function just searches for a `}' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
	(ret t))
    (if (and (< arg 0)
	     (not (bolp))
	     (save-excursion
	       (beginning-of-line)
	       (eq (following-char) ?})))
	(forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^}" nil t)
	  (setq arg (1- arg)
		found t)
	(setq ret nil
	      arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^}" nil t)
	  (setq arg (1+ arg)
		found t)
	(setq ret nil
	      arg 0)))
    (if found
	(end-of-line))
    ret))

;;}}}

;;}}}
;;{{{ communication with a inferior process via comint

;;{{{ tcl-start-process

(defun tcl-start-process (name program &optional startfile &rest switches)
  "Start a tcl process named NAME, running PROGRAM."
  (or switches
      (setq switches tcl-default-command-switches))
  (setq tcl-process-buffer (apply 'make-comint name program startfile switches))
  (setq tcl-process (get-buffer-process tcl-process-buffer))
  (save-excursion
    (set-buffer tcl-process-buffer)
    (setq comint-prompt-regexp "^[^% ]*%\\( %\\)* *"))
  )

;;}}}
;;{{{ tcl-kill-process

(defun tcl-kill-process ()
  "Kill tcl-process and tcl-process-buffer."
  (interactive)
  (if tcl-process-buffer
      (kill-buffer tcl-process-buffer)))

;;}}}
;;{{{ tcl-set-tcl-region-start

(defun tcl-set-tcl-region-start (&optional arg)
"Set start of region for use with `tcl-send-tcl-region'."
  (interactive)
  (set-marker tcl-region-start (or arg (point))))

;;}}}
;;{{{ tcl-set-tcl-region-end

(defun tcl-set-tcl-region-end (&optional arg)
"Set end of region for use with `tcl-send-tcl-region'."
  (interactive)
  (set-marker tcl-region-end (or arg (point))))

;;}}}
;;{{{ send line/region/buffer to tcl-process

;;{{{ tcl-send-current-line

(defun tcl-send-current-line ()
  "Send current line to `tcl-process'.
If `tcl-process' is nil or dead, start a new process first."
  (interactive)
  (let ((start (save-excursion (beginning-of-line) (point)))
	(end (save-excursion (end-of-line) (point))))
    (or (and tcl-process
	     (eq (process-status tcl-process) 'run))
	(tcl-start-process tcl-default-application tcl-default-application))
    (comint-simple-send tcl-process (buffer-substring start end))
    (forward-line 1)
    (if tcl-always-show
	(display-buffer tcl-process-buffer))))

;;}}}
;;{{{ tcl-send-region

(defun tcl-send-region (start end)
  "Send region to tcl `process' wrapped in eval { <region-data> }."
  (interactive "r")
  (or (and tcl-process
	   (comint-check-proc tcl-process-buffer))
      (tcl-start-process tcl-default-application tcl-default-application))
  (comint-simple-send tcl-process
		      (concat "eval {\n"(buffer-substring start end) "\n}"))
  (if tcl-always-show
      (display-buffer tcl-process-buffer)))

;;}}}
;;{{{ tcl-send-tcl-region

(defun tcl-send-tcl-region ()
  "Send tcl-region to tcl `process' wrapped in eval { <region-data> }."
  (interactive)
  (or (and tcl-region-start tcl-region-end)
      (error "tcl-region not set"))
  (or (and tcl-process
	   (comint-check-proc tcl-process-buffer))
      (tcl-start-process tcl-default-application tcl-default-application))
  (comint-simple-send tcl-process
		      (concat "eval {\n"
			      (buffer-substring tcl-region-start tcl-region-end)
			      "\n}"))
  (if tcl-always-show
      (display-buffer tcl-process-buffer)))

;;}}}
;;{{{ tcl-send-proc

(defun tcl-send-proc ()
  "Send proc around point to tcl `process' wrapped in
`eval { <proc> }'."
  (interactive)
  (let (beg end)
    (save-excursion
      (tcl-beginning-of-proc)
      (setq beg (point))
      (tcl-end-of-proc)
      (setq end (point)))
    (or (and tcl-process
	     (comint-check-proc tcl-process-buffer))
	(tcl-start-process tcl-default-application tcl-default-application))
    (comint-simple-send tcl-process
			(concat "eval {\n"
				(buffer-substring beg end)
				"\n}"))
    (if tcl-always-show
	(display-buffer tcl-process-buffer))))

;;}}}
;;{{{ tcl-send-buffer

(defun tcl-send-buffer ()
  "Send region to tcl `process' wrapped in eval { <region-data> }."
  (interactive)
  (or (and tcl-process
	   (comint-check-proc tcl-process-buffer))
      (tcl-start-process tcl-default-application tcl-default-application))
  (if (buffer-modified-p)
      (comint-simple-send tcl-process
			  (concat
			   "eval {\n"
			   (buffer-substring (point-min) (point-max))
			   "\n}"))
    (comint-simple-send tcl-process
			(concat "source "
				(buffer-file-name)
				"\n")))
  (if tcl-always-show
      (display-buffer tcl-process-buffer)))

;;}}}
;;}}}
;;{{{ tcl-get-error-info

(defun tcl-get-error-info ()
  "Send string \"set errorInfo\" to tcl-process and display
tcl-process-buffer."
  (interactive)
  (or (and tcl-process
	   (comint-check-proc tcl-process-buffer))
      (tcl-start-process tcl-default-application tcl-default-application))
  (comint-simple-send tcl-process "set errorInfo\n")
  (display-buffer tcl-process-buffer))

;;}}}
;;{{{ tcl-restart-with-whole-file

(defun tcl-restart-with-whole-file ()
  "Restart tcl-process and send whole file as input."
  (interactive)
  (tcl-kill-process)
  (tcl-start-process tcl-default-application tcl-default-application)
  (tcl-send-buffer))
  
;;}}}  
;;{{{ tcl-show-process-buffer

(defun tcl-show-process-buffer ()
  "Make sure tcl-process-buffer is being displayed."
  (interactive)
  (display-buffer tcl-process-buffer))

;;}}}
;;{{{ tcl-hide-process-buffer

(defun tcl-hide-process-buffer ()
  "Delete all windows that display tcl-process-buffer."
  (interactive)
  (delete-windows-on tcl-process-buffer))

;;}}}

;;}}}

;;{{{ Emacs local variables

;; Local Variables:
;; folded-file: t
;; End:

;;}}}
