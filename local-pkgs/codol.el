;;; Really Simple Code Outliner

(provide 'codol)

(defconst codol-ruby-words
  '(
    "def "
    "class "
    "module "
    "include "
    "extend "
    "attr_"
    "private"
    "public"
    "context "
    "Scenario "
    "should"
    "describe "
    "its? "
    "it_should_s"
    "specify "
    "# == [A-Za-z0-9]"
    ))

(defconst codol-ruby-re (concat " *\\(" (mapconcat (lambda (x) x) codol-ruby-words "\\|") "\\)"))
;;; (defconst codol-ruby-re " *\\(def \\|class \\|module \\|include \\|extend \\|attr_\\|private\\|public\\|context \\|Scenario\\|should\\|.* FLOW: \\|flow_step\\|describex \\|its? \\|it_should_s\\|specify\\|# == [A-Za-z0-9]\\)")

(defvar codol-re-patterns
  (list (cons "Ruby"  codol-ruby-re)
        (cons "Emacs-Lisp" "(defun")
        (cons "Text" "==><==")))

(defvar codol-state 'full)

(make-variable-buffer-local 'codol-state)

(defun codol-pattern-recurse (patterns)
  (cond ((null patterns) nil)
        ((equal (caar patterns) mode-name)
         (cdar patterns))
        (t (codol-pattern-recurse (cdr patterns))) ))

(defun codol-pattern ()
  (codol-pattern-recurse codol-re-patterns))

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
        (cond ((looking-at (codol-pattern))
               (cond (pt 
                      (codol-hide pt (point))
                      (setq pt nil)) ))
              (t (if (null pt) (setq pt (point)))))
        (forward-line) )
      (if pt (codol-hide pt (point))) )))

(defun codol-toggle ()
  (interactive)
  (cond ((eq codol-state 'full)
         (codol-outline)
         (setq codol-state 'outline))
        (t
         (codol-full-view)
         (setq codol-state 'full))))
