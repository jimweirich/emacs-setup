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

;;; Load rinari

(add-to-list 'load-path "~/.elisp/packages/thirdparty/rinari")
(add-to-list 'load-path "~/.elisp/packages/thirdparty/rinari/rhtml")

;;; I don't like the rinari abbrevs, so ignore them
(provide 'rinari-abbrevs)
(require 'rinari)
