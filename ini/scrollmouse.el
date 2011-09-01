;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-scroll-mouse
;;; Purpose: Enabling the scroll button on the mouse
;;; ==================================================================


(cond
 ((is-xemacs)
  (defun jw-mouse-scroll-amount ()
    (if (is-aquamacs)
        (max 1 (window-displayed-height) 3)
      (max 1 (/ (window-displayed-height) 3)) ))

  (defun jw-blip-down ()
    (interactive)
    (scroll-down-command (jw-mouse-scroll-amount)))

  (defun jw-blip-up ()
    (interactive)
    (scroll-up-command (jw-mouse-scroll-amount)))

  (define-key global-map [button4] 'jw-blip-down)
  (define-key global-map [button5] 'jw-blip-up) )
 (t
  (defun jw-mouse-scroll-amount ()
    (max 1 (/ (window-height) 5)))

  (defun jw-blip-down ()
    (interactive)
    (scroll-down (jw-mouse-scroll-amount)))

  (defun jw-blip-up ()
    (interactive)
    (scroll-up (jw-mouse-scroll-amount)))

  (define-key global-map [mouse-4] 'jw-blip-down)
  (define-key global-map [mouse-5] 'jw-blip-up) ))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . 10)))
