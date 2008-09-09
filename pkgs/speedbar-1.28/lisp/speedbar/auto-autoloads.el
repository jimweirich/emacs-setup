;;; DO NOT MODIFY THIS FILE
(if (featurep 'speedbar-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "speedbar/_pkg.el")

(package-provide 'speedbar :version 1.28 :author-version "0.14beta4" :type 'regular)

;;;***

;;;### (autoloads (rpm) "rpm" "speedbar/rpm.el")

(autoload 'rpm "rpm" "\
Red Hat Package Management in Emacs." t nil)

;;;***

;;;### (autoloads (gud-speedbar-buttons) "sb-gud" "speedbar/sb-gud.el")

(autoload 'gud-speedbar-buttons "sb-gud" "\
Create a speedbar display based on the current state of GUD.
If the GUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode." nil nil)

;;;***

;;;### (autoloads (Info-speedbar-buttons Info-speedbar-browser) "sb-info" "speedbar/sb-info.el")

(autoload 'Info-speedbar-browser "sb-info" "\
Initialize speedbar to display an info node browser.
This will add a speedbar major display mode." t nil)

(autoload 'Info-speedbar-buttons "sb-info" "\
Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for." nil nil)

;;;***

;;;### (autoloads (rmail-speedbar-buttons) "sb-rmail" "speedbar/sb-rmail.el")

(autoload 'rmail-speedbar-buttons "sb-rmail" "\
Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder." nil nil)

;;;***

;;;### (autoloads (w3-speedbar-buttons) "sb-w3" "speedbar/sb-w3.el")

(autoload 'w3-speedbar-buttons "sb-w3" "\
Create speedbar buttons for the current web BUFFER displayed in w3 mode." nil nil)

;;;***

;;;### (autoloads (speedbar-get-focus speedbar-frame-mode) "speedbar" "speedbar/speedbar.el")

(defalias 'speedbar 'speedbar-frame-mode)

(autoload 'speedbar-frame-mode "speedbar" "\
Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
nil means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted." t nil)

(autoload 'speedbar-get-focus "speedbar" "\
Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame." t nil)

;;;***

(provide 'speedbar-autoloads)
