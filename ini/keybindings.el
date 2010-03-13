;; Top Level Mappings

(global-set-key (kbd "C-<tab>") 'jw-indent-line)
(global-set-key (kbd "C-z") 'scroll-down) ; I *hate* suspend bound on this key

(if (is-aquamacs)                       ; Why do I have to do this for aquamacs?
    (define-key osx-key-mode-map (kbd "C-z") 'scroll-down))
(global-set-key (kbd "M-g") 'goto-line)	; goto a line position
(global-set-key (kbd "M-#") 'comment-region)

;; C-x mappings
(global-set-key (kbd "C-x 4 h") 'toggle-bs-mode) ; pbk:utils
(global-set-key (kbd "C-x C-m") 'compile)        ;
(global-set-key (kbd "C-x C-n") 'next-error) ;

;; C-C mappings
(global-set-key (kbd "C-C .")   'cmt-insert-bar-dots)
(global-set-key (kbd "C-C #")   'cmt-insert-bar-hash)
(global-set-key (kbd "C-C *")   'cmt-insert-bar-star) 
(global-set-key (kbd "C-C -")   'cmt-insert-bar-light)
(global-set-key (kbd "C-C =")   'cmt-insert-bar-heavy)
(global-set-key (kbd "C-C >")   'tags-reset-tags-tables)
(global-set-key (kbd "C-C ?")   'describe-face) ;hint: this could be done with built-in describe-face
(global-set-key (kbd "C-C SPC") 'multi-shell) ; override the default binding here
(global-set-key (kbd "C-C \\")  'codol-toggle)
(global-set-key (kbd "C-C ^")   'top-level)
(global-set-key (kbd "C-C a")   'org-agenda)
(global-set-key (kbd "C-C d")   'jw-select-gud-buffer)
(global-set-key (kbd "C-C e")   'find-errors)
(global-set-key (kbd "C-C f")   'auto-fill-mode) ; toggle fill mode
(global-set-key (kbd "C-C h")   'jw-show-key-binding) ; is this the same as describe-key (C-h k)
(global-set-key (kbd "C-C l")   'org-store-link)
(global-set-key (kbd "C-C m")   'jw-mark-for-rdebug)
(global-set-key (kbd "C-C r")   'jw-run-rake)
(global-set-key (kbd "C-C s")   'jw-toggle-process-echo-flag)

(global-set-key (kbd "C-C C-f") 'ido-find-file-in-tag-files)
(global-set-key (kbd "C-C C-v") 'jw-find-view)
(global-set-key (kbd "C-C C-t") 'jw-split-or-toggle) ; pkg:testing

;; C-C c
(global-set-key (kbd "C-C c c") 'jw-clear-overlays-to-top-level)
(global-set-key (kbd "C-C c s") 'jw-swap-windows)

;; C-C p -- Perforce bindings
(global-set-key (kbd "C-C p e") 'p4-edit) ; pkg:perforce
(global-set-key (kbd "C-C p r") 'p4-revert) ; pkg:perforce
(global-set-key (kbd "C-C p s") 'p4-sync-code-buffers) ; pkg:perforce
(global-set-key (kbd "C-C p v") 'p4-env) ; pkg:perforce

;;; C-C t -- Testing Commands
(global-set-key (kbd "C-C t F")    'jw-run-last-test-or-spec-file) ; pkg:testing
(global-set-key (kbd "C-C t M")    'jw-run-last-test-or-spec-method) ; pkg:testing
(global-set-key (kbd "C-C t c")    'jw-run-test-cruise) ; pkg:testing
(global-set-key (kbd "C-C t f")    'jw-run-test-or-spec-file) ; pkg:testing
(global-set-key (kbd "C-C t i")    'jw-run-test-integration) ; pkg:testing
(global-set-key (kbd "C-C t l")    'jw-run-test-functionals) ; pkg:testing
(global-set-key (kbd "C-C t m")    'jw-run-test-or-spec-method) ; pkg:testing
(global-set-key (kbd "C-C t r")    'jw-run-test-rake) ; pkg:testing
(global-set-key (kbd "C-C t s")    'toggle-style) ; pkg:testing
(global-set-key (kbd "C-C t t")    'jw-mark-for-testing) ; pkg:testing
(global-set-key (kbd "C-C t u")    'jw-run-test-units) ; pkg:testing
(global-set-key (kbd "C-C t w")    'jw-test-toggle-warnings) ; pkg:testing

(global-set-key (kbd "C-C t C-f")  'jw-run-last-test-or-spec-file) ; pkg:testing
(global-set-key (kbd "C-C t C-m")  'jw-run-last-test-or-spec-method) ; pkg:testing
(global-set-key (kbd "C-C t C-t") 'jw-toggle-clear-buffer-styles) ; pkg:testing
(global-set-key (kbd "C-C t C-s") 'toggle-debug) ; pkg:testing

(global-set-key (kbd "C-C t y") 'jw-testing-use-ruby)
(global-set-key (kbd "C-C t j") 'jw-testing-use-jruby)
(global-set-key (kbd "C-C t 9") 'jw-testing-use-ruby19)
(global-set-key (kbd "C-C t x") 'jw-testing-use-ruby19x)

(global-set-key (kbd "C-C t 1") (lambda () (interactive) (setq jw-test-single-window t))) ; pkg:testing
(global-set-key (kbd "C-C t 2") (lambda () (interactive) (setq jw-test-single-window nil))) ; pkg:testing

;; Function Keys

(global-set-key (kbd "<f2>") 'jw-visit-source) ; pkg:visit-source

(global-set-key (kbd "<f3>") 'jw-cite-filled) ; pkg:gnues

(global-set-key (kbd "<f5>") 'call-last-kbd-macro)

(global-set-key (kbd "<f6>") 'refresh)  ; pkg:utils

(global-set-key (kbd "<f7>") 'unansi) ; pkg:utils

(global-set-key (kbd "<f9>") 'zoom-in)  ; pkg:zoom
(global-set-key (kbd "S-<f9>") 'zoom-way-in) ; pkg:zoom

(global-set-key (kbd "<f10>") 'zoom-out) ; pkg:zoom
(global-set-key (kbd "S-<f10>") 'zoom-way-out) ; pkg:zoom

(global-set-key (kbd "<f10>") 'call-last-kbd-macro)
(global-set-key (kbd "M-<f10>") 'ruby-xmp-region)

(global-set-key (kbd "<f12>") 'jw-zap-all-ansi)
(global-set-key (kbd "M-<f12>") 'jw-select-gud-buffer) ; pkg:ruby

;; Shell runners


(global-set-key (kbd "M-<f1>") (lambda () (interactive) (mshell 1))) ; pkg:utils
(global-set-key (kbd "M-<f2>") (lambda () (interactive) (mshell 2))) ; pkg:utils
(global-set-key (kbd "M-<f3>") (lambda () (interactive) (mshell 3))) ; pkg:utils
(global-set-key (kbd "M-<f4>") (lambda () (interactive) (mshell 4))) ; pkg:utils
(global-set-key (kbd "M-<f5>") (lambda () (interactive) (mshell 5))) ; pkg:utils
(global-set-key (kbd "M-<f6>") (lambda () (interactive) (mshell 6))) ; pkg:utils
(global-set-key (kbd "M-<f7>") (lambda () (interactive) (mshell 7))) ; pkg:utils

(if (is-emacs-19)
    (progn
      (global-set-key (if (is-xemacs) [(shift backspace)] [S-backspace]) "\C-?")
      (global-set-key (kbd "<f1>") "\C-h")
      (global-set-key (kbd "<f4>") 'call-last-kbd-macro)
      (global-set-key (kbd "S-<f4>") 'name-last-kbd-macrob)))

;; Remap some annoying keys in Aquamacs

(if (boundp 'osx-key-mode-map)
    (progn
      (define-key osx-key-mode-map (kbd "A-p") 'jw-noop)
      (define-key osx-key-mode-map (kbd "A-t") 'jw-noop)
      (define-key osx-key-mode-map (kbd "C-A-p") 'aquamacs-print) ))
