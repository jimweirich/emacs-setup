;; Top Level Mappings

;; Use (kbd ...) to simplify key descriptions.
(defun jw-define-key (key-description function)
  (global-set-key (kbd key-description) function))

(jw-define-key "C-<tab>" 'jw-indent-line)
(jw-define-key "C-z" 'scroll-down)	; I *hate* suspend bound on this key
(jw-define-key "M-g" 'goto-line)	; goto a line position
(jw-define-key "M-o" 'fm)
(jw-define-key "M-#" 'comment-region)

;; C-x mappings

(global-set-key "\C-x4h" 'toggle-bs-mode)
(global-set-key "\C-x\C-m" 'compile)	; do the compile command
(global-set-key "\C-x\C-n" 'next-error)	; goto next compile error
(global-set-key "\C-x " 'big-shell)	; select a full screen shell

;; C-c mappings

(global-set-key "\C-c^" 'top-level)
(global-set-key "\C-ch" 'jw-show-key-binding) ; is this the same as describe-key (C-h k)
(global-set-key "\C-ce" 'find-errors)
(global-set-key "\C-c=" 'cmt-insert-bar-heavy)
(global-set-key "\C-c#" 'cmt-insert-bar-hash)
(global-set-key "\C-c-" 'cmt-insert-bar-light)
(global-set-key "\C-c*" 'cmt-insert-bar-star) 
(global-set-key "\C-c#" 'cmt-insert-bar-hash)

(global-set-key "\C-c " 'multi-shell)	; override the default binding here
(global-set-key "\C-c " 'shell)		; select a shell
(global-set-key "\C-c\C-r" 'shell-resync-dirs) ; resync shell with current dir
(global-set-key "\C-cf" 'auto-fill-mode) ; toggle fill mode
(global-set-key "\C-c>" 'tags-reset-tags-tables)
(global-set-key "\C-cnm" 'ccaid-add-mode-comment)
(global-set-key "\C-cs" 'jw-toggle-process-echo-flag)
(global-set-key "\C-C\\" 'codol-toggle)

(global-set-key "\C-C?" 'jw-show-font-name) ;hint: this could be done with built-in describe-face

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(global-set-key "\C-cpe" 'p4-edit)
(global-set-key "\C-cpr" 'p4-revert)
(global-set-key "\C-cps" 'p4-sync-code-buffers)
(global-set-key "\C-cpv" 'p4-env)

(global-set-key "\C-cr" 'jw-run-rake)

(global-set-key "\C-c\C-f" 'ido-find-file-in-tag-files)
(global-set-key "\C-cctt" 'tweet)
(global-set-key "\C-cctb" 'switch-to-buffer-twittering)
(global-set-key "\C-ccc" 'jw-clear-overlays-to-top-level)


;;; Map the various run test commands
(global-set-key "\C-Ctr"    'jw-run-test-rake)
(global-set-key "\C-Ctu"    'jw-run-test-units)
(global-set-key "\C-Ctl"    'jw-run-test-functionals)
(global-set-key "\C-Cti"    'jw-run-test-integration)
(global-set-key "\C-Ctc"    'jw-run-test-cruise)
(global-set-key "\C-Ctf"    'jw-run-test-or-spec-file)
(global-set-key "\C-CtF"    'jw-run-last-test-or-spec-file)
(global-set-key "\C-Ct\C-f" 'jw-run-last-test-or-spec-file)
(global-set-key "\C-Ctm"    'jw-run-test-or-spec-method)
(global-set-key "\C-CtM"    'jw-run-last-test-or-spec-method)
(global-set-key "\C-Ct\C-m" 'jw-run-last-test-or-spec-method)
(global-set-key "\C-ctt"    'jw-mark-for-testing)
(global-set-key "\C-ctw"    'jw-test-toggle-warnings)

(global-set-key "\C-Ct1" (lambda () (interactive)(setq jw-test-single-window t)))
(global-set-key "\C-Ct2" (lambda () (interactive)(setq jw-test-single-window nil)))
(global-set-key "\C-Cts" 'toggle-style)
(global-set-key "\C-Ct\C-t" 'jw-toggle-clear-buffer-styles)
(global-set-key "\C-ct\C-s" 'toggle-debug)

;; Function Keys

(global-set-key [f3] 'jw-cite-filled)
(global-set-key [M-f1] (lambda () (interactive) (mshell 1)))
(global-set-key [M-f2] (lambda () (interactive) (mshell 2)))
(global-set-key [M-f3] (lambda () (interactive) (mshell 3)))
(global-set-key [M-f4] (lambda () (interactive) (mshell 4)))
(global-set-key [M-f5] (lambda () (interactive) (mshell 5)))
(global-set-key [M-f6] (lambda () (interactive) (mshell 6)))
(global-set-key [M-f7] (lambda () (interactive) (mshell 7)))
(global-set-key [M-f12] 'jw-select-gud-buffer)

(global-set-key [f2] 'jw-visit-source)

(global-set-key [f9] 'zoom-in)
(global-set-key (if (is-xemacs) [(shift f9)] [S-f9]) 'zoom-way-in)
(global-set-key [f10] 'zoom-out)
(global-set-key (if (is-xemacs) [(shift f10)] [S-f10]) 'zoom-way-out)

(global-set-key (if (is-xemacs) [(shift f2)] [S-f2]) "\C-?")
(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [C-f6] 'unansi)
(global-set-key [f6] 'refresh)
(global-set-key [f10] 'call-last-kbd-macro)
(global-set-key [f12] 'jw-zap-all-ansi)
(global-set-key [f7] 'tempo-complete-tag)

(if (is-xemacs)
    (progn (global-set-key [f4] 'server-start)
           (global-set-key [(shift f4)] 'gnuserv-start) ))

(if (is-emacs-19)
    (progn
      (global-set-key (if (is-xemacs) [(shift backspace)] [S-backspace]) "\C-?")
      (global-set-key [f1] "\C-h")
      (global-set-key [f4] 'call-last-kbd-macro)
      (global-set-key (if (is-xemacs) [(shift f4)] [S-f4]) 'name-last-kbd-macrob)
      ))

;; reverse migration
(global-set-key "\C-cl" (lambda () (interactive) (setq xxx (parse-up))))

(global-set-key [(meta f10)] 'ruby-xmp-region)
(global-set-key "\C-cm" 'jw-mark-for-rdebug)
(global-set-key "\C-cd" 'jw-select-gud-buffer)


(if (boundp 'osx-key-mode-map)
    (progn
      (define-key osx-key-mode-map (kbd "A-p") 'jw-noop)
      (define-key osx-key-mode-map (kbd "A-t") 'jw-noop)
      (define-key osx-key-mode-map (kbd "C-A-p") 'aquamacs-print) ))

