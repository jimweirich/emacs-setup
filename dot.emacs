;;; Jim Weirich's GNU-Emacs Initialization File

(setq elisp-directory "~jim/.elisp")
(load (concat elisp-directory "/load-ini.el"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-source-path (quote (("/home/jim/working/rubyforge/rubygems" "RubyGems") ("/home/jim/working/rubyforge/rake" "Rake"))))
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(init-face-from-resources nil)
 '(ispell-program-name "/opt/local/bin/ispell")
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(mm-inline-media-tests (quote (("image/jpeg" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote jpeg) handle))) ("image/png" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote png) handle))) ("image/gif" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote gif) handle))) ("image/tiff" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote tiff) handle))) ("image/xbm" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote xbm) handle))) ("image/x-xbitmap" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote xbm) handle))) ("image/xpm" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote xpm) handle))) ("image/x-pixmap" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote xpm) handle))) ("image/bmp" mm-inline-image (lambda (handle) (mm-valid-and-fit-image-p (quote bmp) handle))) ("text/plain" mm-inline-text identity) ("text/enriched" mm-inline-text identity) ("text/richtext" mm-inline-text identity) ("text/x-patch" mm-display-patch-inline (lambda (handle) (locate-library "diff-mode"))) ("application/emacs-lisp" mm-display-elisp-inline identity) ("text/x-vcard" mm-inline-text (lambda (handle) (or (featurep (quote vcard)) (locate-library "vcard")))) ("message/delivery-status" mm-inline-text identity) ("message/rfc822" mm-inline-message identity) ("message/partial" mm-inline-partial identity) ("text/.*" mm-inline-text identity) ("audio/wav" mm-inline-audio (lambda (handle) (and (or (featurep (quote nas-sound)) (featurep (quote native-sound))) (device-sound-enabled-p)))) ("audio/au" mm-inline-audio (lambda (handle) (and (or (featurep (quote nas-sound)) (featurep (quote native-sound))) (device-sound-enabled-p)))) ("application/pgp-signature" ignore identity) ("multipart/alternative" ignore identity) ("multipart/mixed" ignore identity) ("multipart/related" ignore identity))))
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(toolbar-mail-reader (quote gnus))
 '(toolbar-news-reader (quote gnus)))

(setq minibuffer-max-depth nil)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:size "14pt" :family "Fixed"))))
 '(Info-title-1-face ((((class color)) (:foreground "green")) (((type tty)) (:foreground "green"))) t)
 '(Info-title-2-face ((((class color)) (:foreground "red")) (((type tty)) (:foreground "red"))) t)
 '(comint-highlight-input ((((class color)) (:foreground "yellow")) (((type tty)) (:foreground "yellow"))))
 '(comint-highlight-prompt ((((class color)) (:foreground "cyan")) (((type tty)) (:foreground "cyan"))))
 '(font-lock-comment-delimiter-face ((((class color)) (:foreground "grey70")) (((type tty)) (:foreground "grey70"))))
 '(font-lock-comment-face ((((class color)) (:foreground "grey70" :italic t)) (((type tty)) (:foreground "grey70" :italic t))))
 '(font-lock-constant-face ((((class color)) (:foreground "pink")) (((type tty)) (:foreground "pink"))))
 '(font-lock-function-name-face ((((class color)) (:foreground "cyan")) (((type tty)) (:foreground "cyan"))))
 '(font-lock-keyword-face ((((class color)) (:foreground "#d07070" :bold t)) (((type tty)) (:foreground "#d07070" :bold t))))
 '(font-lock-prompt-face ((((class color)) (:foreground "white" :background "darkgreen")) (((type tty)) (:foreground "white" :background "darkgreen"))) t)
 '(font-lock-string-face ((((class color)) (:foreground "LightGreen")) (((type tty)) (:foreground "LightGreen"))))
 '(font-lock-type-face ((((class color)) (:foreground "#c0c0ff" :bold t)) (((type tty)) (:foreground "#c0c0ff" :bold t))))
 '(font-lock-variable-name-face ((((class color)) (:foreground "#90b0ff")) (((type tty)) (:foreground "#90b0ff"))))
 '(info-menu-5 ((((class color)) (:foreground "green")) (((type tty)) (:foreground "green"))) t)
 '(info-node ((((class color)) (:foreground "red")) (((type tty)) (:foreground "red"))) t)
 '(info-xref ((((class color)) (:foreground "firebrick")) (((type tty)) (:foreground "firebrick"))) t)
 '(minibuffer-prompt ((((class color)) (:foreground "cyan")) (((type tty)) (:foreground "cyan"))))
 '(nxml-attribute-value-delimiter-face ((((class color)) (:foreground "orange")) (((type tty)) (:foreground "orange"))) t)
 '(nxml-attribute-value-face ((((class color)) (:foreground "yellow")) (((type tty)) (:foreground "yellow"))) t)
 '(nxml-comment-content-face ((((class color)) (:foreground "orange")) (((type tty)) (:foreground "orange"))) t)
 '(nxml-comment-delimiter-face ((((class color)) (:foreground "orange")) (((type tty)) (:foreground "orange"))) t)
 '(nxml-entity-ref-delimiter-face ((((class color)) (:foreground "red")) (((type tty)) (:foreground "red"))) t)
 '(nxml-entity-ref-name-face ((((class color)) (:foreground "yellow")) (((type tty)) (:foreground "yellow"))) t)
 '(nxml-tag-delimiter-face ((((class color)) (:foreground "lightblue")) (((type tty)) (:foreground "lightblue"))) t)
 '(region ((((class color)) (:foreground "white" :background "SkyBlue4")) (((type tty)) (:foreground "white" :background "SkyBlue4"))))
 '(smerge-refined-change ((t (:background "orange")))))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(let ((fn (expand-file-name "~/.emacs.d/elpa/package.el")))
  (when (file-readable-p fn)
    (load fn)
    (package-initialize)))
