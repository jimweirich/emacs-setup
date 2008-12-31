;;; Load twittering mode.

(require 'twittering-mode)

(let ((fn (expand-file-name "~/.pw/twittering-pw.el")))
  (when (file-readable-p fn)
    (load fn) ))
