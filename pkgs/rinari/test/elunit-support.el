;;; elunit-support.el --- Misc. test helpers for elunit

;; Copyright (C) 2006 Paul Stickney

;; Inspired by regress.el by Wayne Mesard and Tom Breton, Test::Unit
;; by Nathaniel Talbott, and xUnit by Kent Beck

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; 27 MAR 2007 - PST
;;   Cleanup; `elunit-symbol-to-string' is no longer a macro.

;;; PST - move this somewhere else??
(defun elunit-symbol-to-string (sym-or-str)
  "Like `symbol-name' but string arguments are passed through unaffected."
  (if (symbolp sym-or-str)
      (symbol-name sym-or-str)
    sym-or-str))

(defmacro with-test-buffer (config &rest body)
  "Simlar to `with-temp-buffer' but allows one to supply initial contents
and position.  CONFIG is either a string, in which case the contents are
inserted into the start of the buffer and the point is set to start of the
buffer or a list in the form (INITIAL-DATA [START [MATCH-MODIFIER]])
INITIAL-DATA is the text inserted into the buffer.  If START is an integer
or marker it is used to set the point.  Otherwise, if START is a string it
is used as a regex to seek the point to the beginning of the match unless
MATCH-MODIFIER is `end' in which case the point will be moved to the end of
a match.  It is an error if any of the options are not of the correct type
or if the match fails."
  (declare (indent 1))
  ;; setup and pre-checks
  (unless (listp config) ;normalize when config is a plain string
    (setq config (list config)))
  (let ((initial-contents (pop config))
        (start (pop config))
        (match-modifier (pop config)))
    (when (and start
               (not (or (stringp start)
                        (integer-or-marker-p start))))
      (error "Expecting a string, integer, or marker."))
    (when (and match-modifier
               (or (not (stringp start))
                   (not (memq match-modifier '(end)))))
      (error "Invalid match-modifier or context"))
    (when config
      (error "Too many configuration arguments"))
    ;; macro
    `(with-temp-buffer
       (insert ,initial-contents)
       (goto-char (point-min))
       ,(cond ((integer-or-marker-p start)
               `(goto-char ,start))
              ((stringp start)
               `(save-match-data
                  (re-search-forward ,start) ;; error on failure
                  (goto-char ,(if (eq match-modifier 'end)
                                  `(match-end 0)
                                `(match-beginning 0))))))
       ,@body)))

(defun error-to-pretty-string (some-error)
  "Quick way to format an error is a slightly nicer format."
  (format "%S [%S]" (cadr some-error) (car some-error)))


(provide 'elunit-support)