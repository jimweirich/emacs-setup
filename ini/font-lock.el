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

(defun jw-prepend-pair (a b tail)
  (cons a (cons b tail)) )

(defun jw-color (args)
  "Construct a color descriptor from the argument list."
  (jw-prepend-pair :foreground (car args) (jw-color-tail (cdr args))) )

(defun jw-color-tail (args)
  (cond ((null args) ())
        ((eq (car args) :bold)
         (jw-prepend-pair :bold t (jw-color-tail (cdr args))))
        ((eq (car args) :italic)
         (jw-prepend-pair :italic t (jw-color-tail (cdr args))))
        ((eq (car args) :background)
         (jw-prepend-pair :background (cadr args) (jw-color-tail (cddr args))))
        (t (jw-prepend-pair :background (car args) (jw-color-tail (cdr args)))) ))

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
   (jw-font 'font-lock-comment-face            "grey70" :italic)
   (jw-font 'font-lock-comment-delimiter-face  "grey70")
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
   (jw-font 'nxml-comment-delimiter-face       "orange")
   (jw-font 'nxml-comment-content-face         "orange")
   (jw-font 'nxml-entity-ref-name-face         "yellow")
   (jw-font 'nxml-entity-ref-delimiter-face    "red")
   (jw-font 'region                            "white" "SkyBlue4")
   ))

;; Abbreviations

(defun jwfd ()  (interactive) (jw-dark))
(defun jwfl ()  (interactive) (jw-light))
(defun jwf () (interactive) (jwfd))

;;; Color Editing Hints:

;;; Load the face-list.el library and use the (list-faces-display)
;;; function to get an interactive font color editor.

;;; The color list.  Use the perl command below to generate a full
;;; color list with the colors defined on your system.
;;; To create the 'real' list of colors for your system, execute the
;;; following system command and insert the results into the rgbcolors
;;; list.  You may have to adjust the path of rgb.txt for your system.

;;; perl -ne 'print qq!"$2"\n! if /([0-9]+\s+){3}([A-Za-z0-9]+)$/;' </usr/lib/X11/rgb.txt

(setq default-frame-alist '((cursor-color . "red")
                            (background-color . "black")
                            (foreground-color . "white")))
