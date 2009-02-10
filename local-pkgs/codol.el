;;; Really Simple Code Outliner

(provide 'codol)

(defconst codol-re " *\\(def \\|class \\|module \\|include \\|extend \\|attr_\\|private\\|public\\|context \\|should\\|.* FLOW: \\|flow_step\\|# == [A-Za-z0-9]\\)")

(defvar codol-state 'full)

(make-variable-buffer-local 'codol-state)

(defun codol-hide (a b)
  (overlay-put (make-overlay a b) 'invisible 'hide))

(defun codol-full-view ()
  (interactive)
  (dolist (over (overlays-in (point-min) (point-max) ))
    (when
        (overlay-get over 'invisible)
      (delete-overlay over) ) ))

(defun codol-outline ()
  (interactive)
  (add-to-invisibility-spec 'hide)
  (save-excursion
    (beginning-of-buffer)
    (let (pt)
      (while (not (eq (point) (point-max)))
        (beginning-of-line)
        (cond ((looking-at codol-re)
               (cond (pt 
                      (codol-hide pt (point))
                      (setq pt nil)) ))
              (t (if (null pt) (setq pt (point)))))
        (next-line) )
      (if pt (codol-hide pt (point))) )))

(defun codol-toggle ()
  (interactive)
  (cond ((eq codol-state 'full)
         (codol-outline)
         (setq codol-state 'outline))
        (t
         (codol-full-view)
         (setq codol-state 'full))))

(global-set-key "\C-C\\" 'codol-toggle)
