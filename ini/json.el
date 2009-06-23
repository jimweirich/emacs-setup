;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    json
;;; Purpose: Handle JSON reformatting
;;; ==================================================================

(defun rejs ()
  (interactive)
  (save-excursion 
    (let ((start (point-min))
          (end (point-max))
          (shell-file-name "pp_json.rb")
          (delete-p t)
          (buffer (current-buffer))
          (redisplay-p t))
      (call-process-region start end shell-file-name delete-p buffer redisplay-p) )))
