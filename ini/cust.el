;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-cust
;;; Purpose: Custom Setups (Shouldn't this go somewhere else)
;;; ==================================================================

;;; Disable some of the modes ========================================

;;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; Common aliases ===================================================

(defalias 'qrr 'query-replace-regexp)

;;; Tabbing Helpers ==================================================

(defun jwt2 ()
  "Set the physical tab width to 4."
  (interactive)
  (setq tab-width 2))

(defun jwt4 ()
  "Set the physical tab width to 4."
  (interactive)
  (setq tab-width 4))

(defun jwt8 ()
  "Set the physical tab width to 8."
  (interactive)
  (setq tab-width 8))

(defun jw-untab ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)) )

(defun jw-retab ()
  "Retab the current file from 8 to 4 tabs."
  (interactive)
  (jwt4)
  (jw-untab)
  (jwt8) )

;;; Select the highlighting colors ===================================

(if (and window-system (not (is-xemacs)))
    (set-face-foreground 'highlight "white"))

(if window-system
    (cond (t
	   (set-face-foreground 'highlight "white")
	   (set-face-background 'highlight "black")) ))

(defun fa ()
  "Enable filladapt mode"
  (interactive)
  (load-library "filladapt")
  (filladapt-mode))

;;;emacs*cursorColor:	Yellow
;;;emacs*pointerColor:	Cyan
;;;emacs*foreground:	Beige
;;;emacs*background:	DarkSlateBlue
;;;emacs*geometry:	80x50

;;; ... Or use the following code to do much the same thing
;;;     (emacs 18 users should prefix the following commands with "x-"
;;;      e.g. (x-set-mouse-color "cyan"))

;;;(if window-system
;;;    (progn
;;;      (set-mouse-color "cyan")
;;;      (set-cursor-color "yellow")
;;;      (set-background-color "DarkSlateBlue")
;;;      (set-foreground-color "Beige")))


(defun ss()
  "Start the Emacs Client Server."
  (interactive)
  (server-start) )

;;; Never uses physical tabs for spacing =============================

(setq-default indent-tabs-mode nil)

;;; Shell Buffer Scrubbing ===========================================
 
(defun scrub-buffer () (interactive)
  (kill-region (point-min) (point-max)) )

(if (is-aquamacs)
    (defun jw-next-physical-line ()
      (interactive)
      (aquamacs-next-nonvisual-line))
  (defun jw-next-physical-line ()
    (interactive)
    (next-line)))

(defun jw-indent-line ()
  (interactive)
  (cond
   (mark-active (indent-region (point) (mark)))
   (t (beginning-of-line)
      (indent-for-tab-command)
      (beginning-of-line)
      (jw-next-physical-line))))

;;; Notes 

(defun notes ()
  (interactive)
  (find-file "~/.notes"))
