;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-font-lock
;;; Purpose: Setups for font lock
;;; ==================================================================

;;; Define the fonts of interest and their colors.

(require 'cperl-mode)

;;; Font Lock Mode Stuff ============================================

(defun flm ()
  "Enter Font Lock Mode."
  (interactive)
  (font-lock-mode 1)
  (font-lock-fontify-buffer) )

(defun nfl ()
  "No Font Lock Mode."
  (interactive)
  (font-lock-mode 0))

(defun jw-show-font-name ()
  "Show the font name at the current point"
  (interactive)
  (message (symbol-name (get-text-property (point) 'face))))

(defun jw-set-face (face properties)
  (set-face-foreground face (car properties))
  (custom-set-face-bold face (cadr properties))
  (custom-set-face-italic face (caddr properties)))

(defun jw-safe-set-face (face properties)
  (condition-case nil
      (jw-set-face face properties)
    (error nil)))
      
;;; Select *my* custom fonts and do it now!

(defun jw-cons2 (a b tail)
  (cons a (cons b tail)) )

(defun jw-color (args)
  (jw-cons2 :foreground (car args) (jw-color2 (cdr args))) )

(defun jw-color2 (args)
  (cond ((null args) ())
        ((eq (car args) :bold)
         (jw-cons2 :bold t (jw-color2 (cdr args))))
        ((eq (car args) :italic)
         (jw-cons2 :italic t (jw-color2 (cdr args))))
        (t (jw-cons2 :background (car args) (jw-color2 (cdr args)))) ))

(defun jw-font (name &rest args)
  "Font Definition Function."
  (list name
        (list
          (list '((class color))
                (jw-color args))
          (list  '((type tty))
                (jw-color args) )
          )
        't ))


(defun jw-light ()
  "Set the font-lock fonts to custom colors"
  (interactive)
  (custom-set-faces
   (jw-font 'font-lock-keyword-face          "RoyalBlue" :bold)
   (jw-font 'font-lock-string-face           "DarkGreen")
   (jw-font 'font-lock-comment-face          "grey40" :italic)
   (jw-font 'font-lock-type-face             "red")
   (jw-font 'font-lock-variable-name-face    "blue")
   (jw-font 'font-lock-function-name-face    "blue")
   (jw-font 'font-lock-constant-face         "pink")
   (jw-font 'font-lock-prompt-face           "white" "darkgreen")
   (jw-font 'minibuffer-prompt               "firebrick")
   (jw-font 'comint-highlight-prompt         "firebrick")
   (jw-font 'info-xref                       "firebrick")
   (jw-font 'info-node                       "red")
   (jw-font 'info-menu-5                     "green")
   (jw-font 'Info-title-1-face               "green")
   (jw-font 'Info-title-2-face               "red")
   ))


(defun jw-dark ()
  "Set the font-lock fonts to custom colors"
  (interactive)
  (custom-set-faces
   (jw-font 'font-lock-keyword-face            "#d07070" :bold)
   (jw-font 'font-lock-string-face             "LightGreen") ; Strings and Regex
   (jw-font 'font-lock-comment-face            "grey60")
   (jw-font 'font-lock-comment-delimiter-face  "grey60")
   (jw-font 'font-lock-type-face               "#c0c0ff" :bold)
   (jw-font 'font-lock-variable-name-face      "#90b0ff") ;
   (jw-font 'font-lock-function-name-face      "cyan")
   (jw-font 'font-lock-constant-face           "pink")
   (jw-font 'font-lock-prompt-face             "white" "darkgreen")
   (jw-font 'minibuffer-prompt                 "cyan")
   (jw-font 'comint-highlight-prompt           "cyan")
   (jw-font 'comint-highlight-input            "yellow")
   (jw-font 'info-xref                         "firebrick")
   (jw-font 'info-node                         "red")
   (jw-font 'info-menu-5                       "green")
   (jw-font 'Info-title-1-face                 "green")
   (jw-font 'Info-title-2-face                 "red")
   (jw-font 'nxml-tag-delimiter-face           "lightblue")
   (jw-font 'nxml-attribute-value-face         "yellow")
   (jw-font 'nxml-attribute-value-delimiter-face "orange")
   (jw-font 'nxml-comment-delimiter-face "orange")
   (jw-font 'nxml-comment-content-face "orange")
   (jw-font 'nxml-entity-ref-name-face "yellow")
   (jw-font 'nxml-entity-ref-delimiter-face "red")
   ))

;; Abbreviations

(defun jwfd ()  (interactive) (jw-dark))
(defun jwfl ()  (interactive) (jw-light))
(defun jwf () (interactive) (jwfd))
(jwf)

;;; The following functions are used for exploring possible color
;;; options under emacs.

(defun jw-color-example (name color) 
  "Insert a colored string example with the named color."
  (let (n f start end e)
    (setq n (intern (concat "jw-" color)))
    (setq f (make-face n))
    (set-face-foreground f color)
    (setq start (point))
    (insert-string name)
    (setq end (point))
    (insert-string " ")
    (setq e (make-extent start end))
    (set-extent-property e 'face f)
    ()
    )
  )

(defun jw-show-font-colors ()
  "Show the current font settings"
  (interactive)
  (let ((fonts jw-font-list))
    (while fonts
      (jw-color-example (symbol-name (caar fonts)) (cadar fonts))
      (insert-string "\n")
      (setq fonts (cddr fonts))
      )
    )
  )
      

(defun jw-insert-colors () 
  "Insert the set of color names defined in jw-rgbcolors."
  (interactive)
  (let ((clist jw-rgbcolors))
    (while clist 
      (jw-color-example (car clist) (car clist))
      (setq clist (cdr clist))
      )
    )
  )

;;; The color list.  Use the perl command below to generate a full
;;; color list with the colors defined on your system.
;;; To create the 'real' list of colors for your system, execute the
;;; following system command and insert the results into the rgbcolors
;;; list.  You may have to adjust the path of rgb.txt for your system.

;;; perl -ne 'print qq!"$2"\n! if /([0-9]+\s+){3}([A-Za-z0-9]+)$/;' </usr/lib/X11/rgb.txt

;;; Key Bindings =====================================================

(global-set-key "\C-C?" 'jw-show-font-name)

(setq default-frame-alist    '((cursor-color . "red")
             			       (background-color . "black")
		        	       (foreground-color . "white")))
