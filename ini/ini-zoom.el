;;; ==================================================================
;;; Author:  Mark Slagel (modifications by Jim Weirich)
;;; File:    ini-zoom
;;; Purpose: Define zoom-in/zoom-out keys
;;; ==================================================================

(setq zoom-step 4)

(defun zoom-way-out() 
  (interactive)
  (set-selective-display 0))

(defun zoom-way-in() 
  (interactive)
  (set-selective-display zoom-step))

(defun zoom-out() (interactive)
 (set-selective-display
  (if selective-display
      (if (or (= selective-display 0) (= selective-display (* zoom-step 5)))
	  0
	(+ selective-display zoom-step))
    0)))

(defun zoom-in()
  (interactive)
  (set-selective-display
   (if selective-display
       (if (= selective-display 0)
	   (* zoom-step 5)
	 (if (= selective-display zoom-step)
	     zoom-step
	   (- selective-display zoom-step)))
     (* zoom-step) ) ) )

(global-set-key [f9] 'zoom-in)
(global-set-key (if (is-xemacs) [(shift f9)] [S-f9]) 'zoom-way-in)
(global-set-key [f10] 'zoom-out)
(global-set-key (if (is-xemacs) [(shift f10)] [S-f10]) 'zoom-way-out)
