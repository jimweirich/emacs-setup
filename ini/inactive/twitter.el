;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-twitter
;;; Purpose: Setups twittering-mode
;;; ==================================================================

(require 'twittering-mode)

(let ((fn (expand-file-name "~/.twitter-model.el")))
  (if (file-exists-p fn)
      (load-file (expand-file-name "~/.twitter-model.el"))))

(setq twittering-status-format "%i %s (%S),  %@:\n -- %t\n")

(defun twittering-buffer-find-or-activate ()
  (if (not (get-buffer "*twittering*"))
      (twittering-mode)))

(defun tweet ()
  (interactive)
  (twittering-buffer-find-or-activate)
  (twittering-update-status-interactive))

(defun switch-to-buffer-twittering ()
  (interactive)
  (twittering-buffer-find-or-activate)
  (switch-to-buffer "*twittering*"))

(defun reply-to-tweet ()
  (interactive)
  (beginning-of-line)
  (forward-char 2)
  (twittering-enter))

(define-key twittering-mode-map "r" 'reply-to-tweet)

(global-set-key "\C-cctt" 'tweet)
(global-set-key "\C-cctb" 'switch-to-buffer-twittering)
