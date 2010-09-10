;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-aqua
;;; Purpose: Custom Setups for aqua emacs
;;; ==================================================================

;;; Disable some of the modes ========================================

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defun jw-fancy-font-setup ()
  (interactive)

  (setq mac-option-modifier 'meta)

  (if (is-aquamacs)
      (setq jw-font-chooser 'inconsolata)
    (setq jw-font-chooser 'vera-sans-mono))

  (if (is-aquamacs)
      (defun jw-set-font () (interactive) (aquamacs-set-face-as-default))
    (defun jw-set-font () (interactive) ))

  (defun jw-choose-font (pts)
    (apply jw-font-chooser (list pts)))

  (defun monaco (points)
    (interactive "NPoints: ")
    (set-frame-font
     (concat "-*-monaco-medium-r-normal--"
             (number-to-string points)
             "-0-72-72-m-0-iso10646-1")
     t)
    (jw-set-font))

  (defun inconsolata (points)
    (interactive "NPoints: ")
    (set-frame-font
     (concat "-*-inconsolata-*-r-normal--"
             (number-to-string points)
             "-0-72-72-m-0-iso10646-1")
     t)
    (jw-set-font))

  (defun vera-sans-mono (points)
    (interactive "NPoints: ")
    (set-frame-font
     (concat "-*-bitstreamverasansmono-*-r-normal--"
             (number-to-string points)
             "-0-72-72-m-0-iso10646-1")
     t))

  (defun micro()      (interactive) (jw-choose-font 8))
  (defun tiny()       (interactive) (jw-choose-font 12))
  (defun small()      (interactive) (jw-choose-font 14))
  (defun normal()     (interactive) (jw-choose-font 16))
  (defun screencast() (interactive) (jw-choose-font 18))
  (defun medium()     (interactive) (jw-choose-font 20))
  (defun big()        (interactive) (jw-choose-font 24))
  (defun huge()       (interactive) (jw-choose-font 36))
  (defun humongous()  (interactive) (jw-choose-font 48))
  (defun largest()    (interactive) (jw-choose-font 96)))

(cond ((is-aquamacs)
       (jw-fancy-font-setup)
       (defun jw-default-font-setup ()
         (interactive)
         (screencast)
         (jwf)))

      ((is-emacs-23)
       (jw-fancy-font-setup)
       (defun jw-default-font-setup ()
         (interactive)
         (normal)
         (jwf)))

      (t
       (defun jw-default-font-setup ()
         (interactive)
         (jwf))))

(defun sf ()
  (interactive)
  (jw-set-font))

(defun jw-choose-font ()
  (interactive)
  (mac-font-panel-mode)
  (jw-set-font))
