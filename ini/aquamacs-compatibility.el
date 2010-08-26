;; The following definitions are provided to allow some compatibility
;; with aquamacs.

(cond ((is-aquamacs)

       ;; From vc-hooks.el
       ;; The aquamacs version of this file doesn't check for a nil
       ;; return on file-name-directory, hence causing problems.

       (defun vc-registered (file)
         "Return non-nil if FILE is registered in a version control system.

This function performs the check each time it is called.  To rely
on the result of a previous call, use `vc-backend' instead.  If the
file was previously registered under a certain backend, then that
backend is tried first."
         (let (handler)
           (cond
            ((and (file-name-directory file)
                  (string-match vc-ignore-dir-regexp (file-name-directory file)))
             nil)
            ((and (boundp 'file-name-handler-alist)
                  (setq handler (find-file-name-handler file 'vc-registered)))
             ;; handler should set vc-backend and return t if registered
             (funcall handler 'vc-registered file))
            (t
             ;; There is no file name handler.
             ;; Try vc-BACKEND-registered for each handled BACKEND.
             (catch 'found
               (let ((backend (vc-file-getprop file 'vc-backend)))
                 (mapc
                  (lambda (b)
                    (and (vc-call-backend b 'registered file)
                         (vc-file-setprop file 'vc-backend b)
                         (throw 'found t)))
                  (if (or (not backend) (eq backend 'none))
                      vc-handled-backends
                    (cons backend vc-handled-backends))))
               ;; File is not registered.
               (vc-file-setprop file 'vc-backend 'none)
               nil)))))


       ;; From simple.el
       ;; This didn't exist in the aquamacs definitions.

       (defun start-file-process (name buffer program &rest program-args)
         "Start a program in a subprocess.  Return the process object for it.

Similar to `start-process', but may invoke a file handler based on
`default-directory'.  See Info node `(elisp)Magic File Names'.

This handler ought to run PROGRAM, perhaps on the local host,
perhaps on a remote host that corresponds to `default-directory'.
In the latter case, the local part of `default-directory' becomes
the working directory of the process.

PROGRAM and PROGRAM-ARGS might be file names.  They are not
objects of file handler invocation."
         (let ((fh (find-file-name-handler default-directory 'start-file-process)))
           (if fh (apply fh 'start-file-process name buffer program program-args)
             (apply 'start-process name buffer program program-args))))


       ;; From ??
       ;; This didn't exist in the aquamacs definitions.

       (defun string-match-p (a b) (string-match a b))
       ))
