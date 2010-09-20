;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-aqua
;;; Purpose: Custom Setups for aqua emacs
;;; ==================================================================

;;; Disable some of the modes ========================================

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defun jw-fancy-font-setup ()
  (interactive)

  (setq *jw-font-size* 18)

  (setq mac-option-modifier 'meta)

  (cond ((is-aquamacs)
         (setq jw-font-chooser 'inconsolata)
         (defun jw-set-font () (interactive) (aquamacs-set-face-as-default)))
        (t
         (setq jw-font-chooser 'vera-sans-mono)
         (defun jw-set-font () (interactive) )))

  (defun jw-choose-font (pts &optional no-set-font)
    (setq *jw-font-size* pts)
    (apply jw-font-chooser (list pts no-set-font)))

  (defun jw-bigger-font ()
    (interactive)
    (jw-choose-font (+ 4 *jw-font-size*)))

  (defun jw-smaller-font ()
    (interactive)
    (jw-choose-font (- *jw-font-size* 4)))

  (defun monaco (points &optional no-set-font)
    (interactive "NPoints: ")
    (set-frame-font
     (concat "-*-monaco-medium-r-normal--"
             (number-to-string points)
             "-0-72-72-m-0-iso10646-1")
     t)
    (if (not no-set-font) (jw-set-font)))

  (defun inconsolata (points &optional no-set-font)
    (interactive "NPoints: ")
    (set-frame-font
     (concat "-*-inconsolata-*-r-normal--"
             (number-to-string points)
             "-0-72-72-m-0-iso10646-1")
     t)
    (if (not no-set-font) (jw-set-font)))

  (defun vera-sans-mono (points &optional no-set-font)
    (interactive "NPoints: ")
    (set-frame-font
     (concat "-*-bitstreamverasansmono-*-r-normal--"
             (number-to-string points)
             "-0-72-72-m-0-iso10646-1")
     t))

  (defun micro()      (interactive) (jw-choose-font 8))
  (defun tiny()       (interactive) (jw-choose-font 12))
  (defun small()      (interactive) (jw-choose-font 14))
  (defun subnormal()  (interactive) (jw-choose-font 16))
  (defun screencast() (interactive) (jw-choose-font 18))
  (defun medium()     (interactive) (jw-choose-font 20))
  (defun big()        (interactive) (jw-choose-font 24))
  (defun huge()       (interactive) (jw-choose-font 36))
  (defun humongous()  (interactive) (jw-choose-font 48))
  (defun largest()    (interactive) (jw-choose-font 96))
)

(defun normal (&optional no-set-font)
  (interactive)
  (jw-choose-font 18 no-set-font))

(cond ((is-aquamacs)
      (jw-fancy-font-setup)
       (defun jw-default-font-setup ()
         (interactive)
         (normal nil)
         (jwf)))

      ((is-emacs-23)
       (jw-fancy-font-setup)
       (defun jw-default-font-setup ()
         (interactive)
         (normal nil)
         (jwf)))

      (t
       (defun jw-default-font-setup ()
         (interactive)
         (jwf))))

(defun sf ()
  (interactive)
  (jw-set-font))

(defun jw-interactive-choose-font ()
  (interactive)
  (mac-font-panel-mode nil)
  (jw-set-font))

(normal)
