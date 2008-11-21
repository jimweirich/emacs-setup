;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-rails
;;; Purpose: Setups for rails specific functionality
;;; ==================================================================


;;; Setup for dark fonts in rinari/rhtml mode

(defface erb-face
  `((t (:background "grey10")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "grey15")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)

(defface erb-out-delim-face
  `((t (:inherit erb-delim-face :weight bold :foreground "yellow")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-exec-delim-face
  `((t (:inherit erb-delim-face :weight bold :foreground "yellow")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

;;; I don't like the rinari abbrevs, so ignore them
(provide 'rinari-abbrevs)
(require 'rinari)
