;; Load the patched version of max-frame for non-aquamacs versions.
(cond ((not (is-aquamacs))
       (load (concat elisp-directory "/pkgs/patched-maxframe.el"))

       (defun jw-maximized-frame-p ()
         mf-restore-width)

       (defun jw-toggle-maxframe ()
         "Toggle between full-sized and partial-sized window frame."
         (interactive)
         (if (jw-maximized-frame-p)
             (restore-frame)
           (maximize-frame)))))
