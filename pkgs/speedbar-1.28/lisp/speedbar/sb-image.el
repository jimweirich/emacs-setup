;;; sb-image --- Image management for speedbar

;;; Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: file, tags, tools
;; X-RCS: $Id: sb-image.el,v 1.7 2002/03/17 11:44:13 zappo Exp $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Supporting Image display for Emacs 20 and less, Emacs 21, and XEmacs,
;; is a challenging task, which doesn't take kindly to being byte compiled.
;; When sharing speedbar.elc between these three applications, the Image
;; support can get lost.
;;
;; By splitting out that hard part into this file, and avoiding byte
;; compilation, one copy speedbar can support all these platforms together.
;;
;; This file requires the `image' package if it is available.

(condition-case nil
    (require 'image)
  (error nil))

;;; Code:
(defcustom speedbar-use-images (and (or (fboundp 'defimage)
					(fboundp 'make-image-specifier))
				    (if (fboundp 'display-graphic-p)
					(display-graphic-p)
				      window-system))
  "*Non nil if speedbar should display icons."
  :group 'speedbar
  :type 'boolean)

;;; Some images if defimage is available:
(eval-and-compile

(if (fboundp 'defimage)
    (defalias 'defimage-speedbar 'defimage)

  (if (not (fboundp 'make-glyph))
      
(defmacro defimage-speedbar (variable imagespec docstring)
  "Don't bother loading up an image...
Argument VARIABLE is the varible to define.
Argument IMAGESPEC is the list defining the image to create.
Argument DOCSTRING is the documentation for VARIABLE."
  `(defvar ,variable nil ,docstring))

;; ELSE
(defun speedbar-find-image-on-load-path (image)
  "Find the image file IMAGE on the load path."
  (let ((l (cons
	    ;; In XEmacs, try the data directory first (for an
	    ;; install in XEmacs property.  Then search the load
	    ;; path (for user installs)
	    (locate-data-directory "speedbar")
	    load-path))
	(r nil))
    (while (and l (not r))
      (if (file-exists-p (concat (car l) "/" image))
	  (setq r (concat (car l) "/" image)))
      (setq l (cdr l)))
    r))

(defun speedbar-convert-emacs21-imagespec-to-xemacs (spec)
  "Convert the Emacs21 Image SPEC into an XEmacs image spec."
  (let* ((sl (car spec))
	 (itype (nth 1 sl))
	 (ifile (nth 3 sl)))
    (vector itype ':file (speedbar-find-image-on-load-path ifile))))

(defmacro defimage-speedbar (variable imagespec docstring)
  "Devine VARIABLE as an image if `defimage' is not available..
IMAGESPEC is the image data, and DOCSTRING is documentation for the image."
  `(defvar ,variable
     ;; The Emacs21 version of defimage looks just like the XEmacs image
     ;; specifier, except that it needs a :type keyword.  If we line
     ;; stuff up right, we can use this cheat to support XEmacs specifiers.
     (condition-case nil
	 (make-glyph
	  (make-image-specifier
	   (speedbar-convert-emacs21-imagespec-to-xemacs (quote ,imagespec)))
	  'buffer)
       (error nil))
     ,docstring))

)))

(defimage-speedbar speedbar-directory
  ((:type xpm :file "sb-dir.xpm" :ascent center))
  "Image used for emptly closed directories.")

(defimage-speedbar speedbar-directory-plus
  ((:type xpm :file "sb-dir-plus.xpm" :ascent center))
  "Image used for closed directories with stuff in them.")

(defimage-speedbar speedbar-directory-minus
  ((:type xpm :file "sb-dir-minus.xpm" :ascent center))
  "Image used for open directories with stuff in them.")

(defimage-speedbar speedbar-page-plus
  ((:type xpm :file "sb-pg-plus.xpm" :ascent center))
  "Image used for closed files with stuff in them.")

(defimage-speedbar speedbar-page-minus
  ((:type xpm :file "sb-pg-minus.xpm" :ascent center))
  "Image used for open files with stuff in them.")

(defimage-speedbar speedbar-page
  ((:type xpm :file "sb-pg.xpm" :ascent center))
  "Image used for files that can't be opened.")

(defimage-speedbar speedbar-tag
  ((:type xpm :file "sb-tag.xpm" :ascent center))
  "Image used for tags.")

(defimage-speedbar speedbar-tag-plus
  ((:type xpm :file "sb-tag-plus.xpm" :ascent center))
  "Image used for closed tag groups.")

(defimage-speedbar speedbar-tag-minus
  ((:type xpm :file "sb-tag-minus.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-tag-gt
  ((:type xpm :file "sb-tag-gt.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-tag-v
  ((:type xpm :file "sb-tag-v.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-tag-type
  ((:type xpm :file "sb-tag-type.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-box-plus
  ((:type xpm :file "sb-box-plus.xpm" :ascent center))
  "Image used for closed groups of tags.")

(defimage-speedbar speedbar-box-minus
  ((:type xpm :file "sb-box-minus.xpm" :ascent center))
  "Image used for open groups of tags.")

(defimage-speedbar speedbar-mail
  ((:type xpm :file "sb-mail.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-checkout
  ((:type xpm :file "sb-chk.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-object
  ((:type xpm :file "sb-obj.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-object-out-of-date
  ((:type xpm :file "sb-objod.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-label
  ((:type xpm :file "sb-label.xpm" :ascent center))
  "Image used for label prefix.")

(defimage-speedbar speedbar-read-only
  ((:type xpm :file "sb-ro.xpm" :ascent center))
  "Image used to mark an item Read Only, Locked, or some such.")

(defimage-speedbar speedbar-document-tag
  ((:type xpm :file "sb-doc.xpm" :ascent center))
  "Image used to indicate documentation available.")

(defimage-speedbar speedbar-document-plus
  ((:type xpm :file "sb-doc-plus.xpm" :ascent center))
  "Image used to indicate documentation available.")

(defimage-speedbar speedbar-document-minus
  ((:type xpm :file "sb-doc-minus.xpm" :ascent center))
  "Image used to indicate documentation available.")

(defimage-speedbar speedbar-info-tag
  ((:type xpm :file "sb-info.xpm" :ascent center))
  "Image used to indicate more information available.")

(defvar speedbar-expand-image-button-alist
  '(("<+>" . speedbar-directory-plus)
    ("<->" . speedbar-directory-minus)
    ("< >" . speedbar-directory)
    ("[+]" . speedbar-page-plus)
    ("[-]" . speedbar-page-minus)
    ("[?]" . speedbar-page)
    ("[ ]" . speedbar-page)
    ("{+}" . speedbar-box-plus)
    ("{-}" . speedbar-box-minus)
    ("<M>" . speedbar-mail)
    ("<d>" . speedbar-document-tag)
    ("<i>" . speedbar-info-tag)
    (" =>" . speedbar-tag)
    (" +>" . speedbar-tag-gt)
    (" ->" . speedbar-tag-v)
    (">" . speedbar-tag)
    ("@" . speedbar-tag-type)
    ("  @" . speedbar-tag-type)
    ("*" . speedbar-checkout)
    ("#" . speedbar-object)
    ("!" . speedbar-object-out-of-date)
    ("//" . speedbar-label)
    ("%" . speedbar-read-only)
    ;; Not used in this form, but kept here so we don't forget
    ("<d+>" . speedbar-document-plus)
    ("<d->" . speedbar-document-minus)
    )
  "List of text and image associations.")

(defun speedbar-insert-image-button-maybe (start length)
  "Insert an image button based on text starting at START for LENGTH chars.
If buttontext is unknown, just insert that text.
If we have an image associated with it, use that image."
  (if speedbar-use-images
      (let* ((bt (buffer-substring start (+ length start)))
	     (a (assoc bt speedbar-expand-image-button-alist)))
	;; Regular images (created with `insert-image' are intangible
	;; which (I suppose) make them more compatible with XEmacs 21.
	;; Unfortunatly, there is a giant pile o code dependent on the
	;; underlying text.  This means if we leave it tangible, then I
	;; don't have to change said giant piles o code.
	(if (and a (symbol-value (cdr a)))
	    (if (featurep 'xemacs)
		(add-text-properties (+ start (length bt)) start
				     (list 'end-glyph (symbol-value (cdr a))
					   'rear-nonsticky (list 'display)
					   'invisible t
					   'detachable t))
	      (add-text-properties start (+ start (length bt))
				   (list 'display (symbol-value (cdr a))
					 'rear-nonsticky (list 'display))))
	  ;(message "Bad text [%s]" (buffer-substring start (+ start length)))
	  ))))

(defun speedbar-image-dump ()
  "Dump out the current state of the Speedbar image alist.
See `speedbar-expand-image-button-alist' for details."
  (interactive)
  (with-output-to-temp-buffer "*Speedbar Images*"
    (save-excursion
      (set-buffer "*Speedbar Images*")
      (goto-char (point-max))
      (insert "Speedbar image cache.\n\n")
      (let ((start (point)) (end nil))
	(insert "Image\tText\tImage Name")
	(setq end (point))
	(insert "\n")
	(put-text-property start end 'face 'underline))
      (let ((ia speedbar-expand-image-button-alist))
	(while ia
	  (let ((start (point)))
	    (insert (car (car ia)))
	    (insert "\t")
	    (speedbar-insert-image-button-maybe start
						(length (car (car ia))))
	    (insert (car (car ia)) "\t" (format "%s" (cdr (car ia))) "\n"))
	  (setq ia (cdr ia)))))))

(provide 'sb-image)

;;; sb-image.el ends here
