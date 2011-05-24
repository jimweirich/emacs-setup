;;; Speedbar setup

(require 'speedbar)

(speedbar-add-supported-extension ".rb")
(speedbar-add-supported-extension ".js")
(speedbar-add-supported-extension ".coffee")

(defun sb ()
  (interactive)
  (speedbar))
