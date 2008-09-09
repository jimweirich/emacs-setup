;;; icicles.el --- Minibuffer input completion and cycling.
;;
;; Filename: icicles.el
;; Description: Minibuffer completion and cycling.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Tue Aug  1 14:21:16 1995
;; Version: 22.0
;; Last-Updated: Sat Oct 13 13:29:28 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 21912
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `color-theme',
;;   `cus-edit', `cus-face', `cus-load', `cus-start', `custom',
;;   `dired', `dired+', `dired-aux', `dired-x', `easymenu',
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind', `ffap', `ffap-',
;;   `fit-frame', `frame-cmds', `frame-fns', `help-mode', `hexrgb',
;;   `icicles-cmd', `icicles-face', `icicles-fn', `icicles-mac',
;;   `icicles-mcmd', `icicles-mode', `icicles-opt', `icicles-var',
;;   `info', `info+', `kmacro', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `pp', `pp+', `strings', `thingatpt',
;;   `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Minibuffer input completion and cycling of completion candidates.
;;
;;  Input completion takes as input a string and returns a name that
;;  contains the input string.  This library enables minibuffer
;;  cycling of completion candidates, and provides additional support
;;  for input completion.
;;
;;  Two kinds of completion are offered here, which are distinguished
;;  by how the input string is matched against the completed name:
;;
;;   - Prefix completion - The input string is a prefix of the
;;                         completed name.  This is the usual Emacs
;;                         completion.
;;
;;   - Apropos completion - The input string is a regular expression
;;                          that matches somewhere (anywhere) within
;;                          the completed name.  You can think of the
;;                          name as having been returned by `apropos'
;;                          (except it also works for file and buffer
;;                          names).
;;
;;  Files `icicles-doc1.el' and `icicles-doc2.el' contain the doc for
;;  Icicles, including how to install and use Icicles.  You can also
;;  read the Icicles doc, in formatted form, on the Emacs-Wiki Web
;;  site: http://www.emacswiki.org/cgi-bin/wiki/Icicles.  Emacs Wiki
;;  also has a few addtional pages about Icicles.  In particular, if
;;  you are new to Emacs, as well as Icicles, see this page:
;;  http://www.emacswiki.org/cgi-bin/wiki/EmacsNewbieWithIcicles.
;;
;;  See also: Library `icicles-menu.el', which lets you execute menu
;;  commands, cycling and completing them.
 
;;(@> "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Things Defined in Icicles")
;;  (@> "Change log")
 
;;(@* "Things Defined in Icicles")
;;
;;  Things Defined in Icicles
;;  -------------------------
;;
;;  Key bindings defined in Icicles: see (@> "Key Bindings"), below.
;;
;;  Macros defined in Icicles:
;;
;;    `icicle-define-add-to-alist-command', `icicle-define-command',
;;    `icicle-define-file-command', `icicle-define-sort-command'.
;; 
;;  Commands defined in Icicles -
;;
;;   Commands to be used mainly at top level:
;;
;;    `a', `any', `buffer', `clear-option', `file',
;;    `icicle-add-buffer-candidate', `icicle-add-buffer-config',
;;    `icicle-add-candidate-to-saved-completion-set',
;;    `icicle-add-region', `icicle-add/update-saved-completion-set',
;;    `icicle-anything', `icicle-apropos', `icicle-apropos-command',
;;    `icicle-apropos-function', `icicle-apropos-option',
;;    `icicle-apropos-variable', `icicle-apropos-zippy',
;;    `icicle-bookmark', `icicle-buffer', `icicle-buffer-config',
;;    `icicle-buffer-list', `icicle-buffer-other-window',
;;    `icicle-change-alternative-sort-order',
;;    `icicle-change-sort-order', `icicle-color-theme',
;;    `icicle-comint-command', `icicle-comint-search',
;;    `icicle-compilation-search', `icicle-complete-keys',
;;    `icicle-complete-thesaurus-entry', `icicle-customize-apropos',
;;    `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options', `icicle-customize-face',
;;    `icicle-customize-icicles-group', `icicle-dabbrev-completion',
;;    `icicle-delete-file', `icicle-delete-window',
;;    `icicle-delete-windows', `icicle-delete-windows-on',
;;    `icicle-describe-file', `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window', `icicle-doc',
;;    `icicle-exchange-point-and-mark',
;;    `icicle-execute-extended-command',
;;    `icicle-execute-named-keyboard-macro', `icicle-face-list',
;;    `icicle-file-list', `icicle-find-file',
;;    `icicle-find-file-other-window', `icicle-font',
;;    `icicle-frame-bg', `icicle-frame-fg', `icicle-fundoc',
;;    `icicle-generic-S-tab', `icicle-goto-global-marker',
;;    `icicle-goto-marker', `icicle-imenu', `icicle-imenu-command',
;;    `icicle-imenu-non-interactive-function',
;;    `icicle-Info-goto-node', `icicle-Info-goto-node-cmd',
;;    `icicle-Info-index', `icicle-Info-index-20',
;;    `icicle-Info-index-cmd', `icicle-insert-char',
;;    `icicle-insert-kill', `icicle-insert-thesaurus-entry',
;;    `icicle-keyword-list', `icicle-kill-buffer', `icicle-kmacro',
;;    `icicle-lisp-complete-symbol', `icicle-locate-file',
;;    `icicle-locate-file-other-window', `icicle-map' `icicle-mode',
;;    `icy-mode', `icicle-object-action', `icicle-occur',
;;    `icicle-other-window-or-frame', `icicle-plist',
;;    `icicle-read-color', `icicle-read-kbd-macro',
;;    `icicle-recent-file', `icicle-recent-file-other-window',
;;    `icicle-regexp-list', `icicle-region-open-all-files',
;;    `icicle-remove-all-regions-in-buffer',
;;    `icicle-remove-buffer-candidate', `icicle-remove-buffer-config',
;;    `icicle-remove-candidate-from-saved-completion-set',
;;    `icicle-remove-region', `icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command', `icicle-reset-option-to-nil',
;;    `icicle-save-string-to-variable', `icicle-search',
;;    `icicle-search-all-regions', `icicle-search-buffer',
;;    `icicle-search-char-property', `icicle-search-file',
;;    `icicle-search-generic', `icicle-search-highlight-cleanup',
;;    `icicle-search-keywords', `icicle-search-overlay-property',
;;    `icicle-search-region', `icicle-search-text-property',
;;    `icicle-search-word', `icicle-select-frame',
;;    `icicle-select-region', `icicle-select-window',
;;    `icicle-send-bug-report', `icicle-set-option-to-t',
;;    `icicle-sort-alphabetical', `icicle-sort-by-directories-last',
;;    `icicle-sort-by-last-file-modification-time',
;;    `icicle-sort-by-last-use',
;;    `icicle-sort-by-previous-use-alphabetically',
;;    `icicle-sort-case-insensitive', `icicle-sort-turned-OFF',
;;    `icicle-toggle-~-for-home-dir',
;;    `icicle-toggle-alternative-sorting',
;;    `icicle-toggle-angle-brackets',
;;    `icicle-toggle-case-sensitivity',
;;    `icicle-toggle-fuzzy-completion',
;;    `icicle-toggle-highlight-all-current',
;;    `icicle-toggle-highlight-historical-candidates',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-incremental-completion',
;;    `icicle-toggle-literal-replacement', `icicle-toggle-option',
;;    `icicle-toggle-regexp-quote', `icicle-toggle-search-cleanup',
;;    `icicle-toggle-search-replace-whole',
;;    `icicle-toggle-search-whole-word', `icicle-toggle-sorting',
;;    `icicle-toggle-transforming',
;;    `icicle-toggle-WYSIWYG-Completions', `icicle-vardoc',
;;    `icicle-yank-insert', `toggle',
;;    `toggle-icicle-alternative-sorting',
;;    `toggle-icicle-angle-brackets',
;;    `toggle-icicle-case-sensitivity',
;;    `toggle-icicle-fuzzy-completion',
;;    `toggle-icicle-highlight-all-current',
;;    `toggle-icicle-highlight-historical-candidates',
;;    `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-literal-replacement',
;;    `toggle-icicle-regexp-quote', `toggle-icicle-search-cleanup',
;;    `toggle-icicle-search-replace-whole',
;;    `toggle-icicle-search-whole-word', `toggle-icicle-sorting',
;;    `toggle-icicle-transforming',
;;    `toggle-icicle-WYSIWYG-Completions', `what-which-how'.
;;
;;   Commands to be used mainly in the minibuffer or *Completions*:
;; 
;;    `icicle-abort-minibuffer-input', `icicle-all-candidates-action',
;;    `icicle-all-candidates-alt-action', `icicle-apropos-complete',
;;    `icicle-apropos-complete-and-exit',
;;    `icicle-apropos-complete-and-narrow',
;;    `icicle-apropos-complete-no-display',
;;    `icicle-backward-delete-char-untabify',
;;    `icicle-backward-kill-paragraph',
;;    `icicle-backward-kill-sentence', `icicle-backward-kill-sexp',
;;    `icicle-backward-kill-word', `icicle-candidate-action',
;;    `icicle-candidate-alt-action',
;;    `icicle-candidate-read-fn-invoke',
;;    `icicle-candidate-set-complement',
;;    `icicle-candidate-set-define',
;;    `icicle-candidate-set-difference',
;;    `icicle-candidate-set-intersection',
;;    `icicle-candidate-set-retrieve',
;;    `icicle-candidate-set-retrieve-from-cache-file',
;;    `icicle-candidate-set-retrieve-from-variable',
;;    `icicle-candidate-set-save', `icicle-candidate-set-save-more',
;;    `icicle-candidate-set-save-more-selected',
;;    `icicle-candidate-set-save-selected',
;;    `icicle-candidate-set-save-to-cache-file',
;;    `icicle-candidate-set-save-to-variable',
;;    `icicle-candidate-set-swap', `icicle-candidate-set-truncate',
;;    `icicle-candidate-set-union',
;;    `icicle-change-alternative-sort-order',
;;    `icicle-change-sort-order', `icicle-completion-help',
;;    `icicle-Completions-mouse-3-menu',
;;    `icicle-search-define-replacement',
;;    `icicle-delete-backward-char', `icicle-delete-candidate-object',
;;    `icicle-delete-char', `icicle-digit-argument',
;;    `icicle-dispatch-C-^', `icicle-dispatch-C-.',
;;    `icicle-dispatch-C-comma', `icicle-dispatch-M-comma',
;;    `icicle-dispatch-M-q', `icicle-erase-minibuffer',
;;    `icicle-erase-minibuffer-or-history-element',
;;    `icicle-exit-minibuffer', `icicle-help-on-candidate',
;;    `icicle-help-on-next-apropos-candidate',
;;    `icicle-help-on-previous-apropos-candidate',
;;    `icicle-help-on-next-prefix-candidate',
;;    `icicle-help-on-previous-prefix-candidate', `icicle-history',
;;    `icicle-insert-completion', `icicle-insert-history-element',
;;    `icicle-insert-key-description',
;;    `icicle-insert-string-at-point',
;;    `icicle-insert-string-from-variable', `icicle-isearch-complete',
;;    `icicle-keep-only-past-inputs', `icicle-kill-failed-input',
;;    `icicle-kill-line', `icicle-kill-paragraph',
;;    `icicle-kill-region', `icicle-kill-region-wimpy',
;;    `icicle-kill-sentence', `icicle-kill-sexp', `icicle-kill-word',
;;    `icicle-minibuffer-complete-and-exit',
;;    `icicle-mouse-candidate-action',
;;    `icicle-mouse-candidate-alt-action',
;;    `icicle-mouse-candidate-read-fn-invoke',
;;    `icicle-mouse-candidate-set-save',
;;    `icicle-mouse-candidate-set-save-more',
;;    `icicle-mouse-choose-completion',
;;    `icicle-mouse-help-on-candidate',
;;    `icicle-mouse-remove-candidate', `icicle-mouse-save-candidate',
;;    `icicle-mouse-save-then-kill', `icicle-move-to-next-completion',
;;    `icicle-move-to-previous-completion',
;;    `icicle-narrow-candidates',
;;    `icicle-narrow-candidates-with-predicate',
;;    `icicle-negative-argument', `icicle-next-apropos-candidate',
;;    `icicle-next-apropos-candidate-action',
;;    `icicle-next-apropos-candidate-alt-action',
;;    `icicle-next-candidate-per-mode', `icicle-next-line',
;;    `icicle-next-prefix-candidate',
;;    `icicle-next-prefix-candidate-action',
;;    `icicle-next-prefix-candidate-alt-action',
;;    `icicle-pp-eval-expression', `icicle-prefix-complete',
;;    `icicle-prefix-complete-no-display',
;;    `icicle-prefix-word-complete',
;;    `icicle-previous-apropos-candidate',
;;    `icicle-previous-apropos-candidate-action',
;;    `icicle-previous-apropos-candidate-alt-action',
;;    `icicle-previous-candidate-per-mode', `icicle-previous-line',
;;    `icicle-previous-prefix-candidate',
;;    `icicle-previous-prefix-candidate-action',
;;    `icicle-previous-prefix-candidate-alt-action',
;;    `icicle-remove-candidate', `icicle-remove-Completions-window',
;;    `icicle-retrieve-last-input', `icicle-retrieve-next-input',
;;    `icicle-retrieve-previous-input', `icicle-reverse-sort-order',
;;    `icicle-save-candidate', `icicle-save-predicate-to-variable',
;;    `icicle-scroll-Completions', `icicle-search-define-replacement',
;;    `icicle-self-insert', `icicle-sort-alphabetical',
;;    `icicle-sort-by-directories-last',
;;    `icicle-sort-by-last-file-modification-time',
;;    `icicle-sort-by-last-use',
;;    `icicle-sort-by-previous-use-alphabetically',
;;    `icicle-sort-case-insensitive', `icicle-sort-turned-OFF',
;;    `icicle-switch-to-Completions-buf',
;;    `icicle-switch-to-completions',
;;    `icicle-switch-to/from-minibuffer',
;;    `icicle-toggle-~-for-home-dir',
;;    `icicle-toggle-alternative-sorting',
;;    `icicle-toggle-angle-brackets',
;;    `icicle-toggle-case-sensitivity',
;;    `icicle-toggle-fuzzy-completion',
;;    `icicle-toggle-highlight-all-current',
;;    `icicle-toggle-highlight-historical-candidates',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-incremental-completion',
;;    `icicle-toggle-literal-replacement',
;;    `icicle-toggle-regexp-quote', `icicle-toggle-search-cleanup',
;;    `icicle-toggle-search-replace-whole',
;;    `icicle-toggle-search-whole-word', `icicle-toggle-sorting',
;;    `icicle-toggle-transforming', `icicle-transpose-chars',
;;    `icicle-transpose-sexps', `icicle-transpose-words',
;;    `icicle-universal-argument', `icicle-universal-argument-minus',
;;    `icicle-universal-argument-more',
;;    `icicle-universal-argument-other-key', `icicle-yank',
;;    `icicle-yank-pop', `old-exit-minibuffer',
;;    `old-minibuffer-complete-and-exit', `old-switch-to-completions',
;;    `toggle-icicle-~-for-home-dir',
;;    `toggle-icicle-alternative-sorting',
;;    `toggle-icicle-angle-brackets',
;;    `toggle-icicle-case-sensitivity',
;;    `toggle-icicle-fuzzy-completion',
;;    `toggle-icicle-highlight-all-current',
;;    `toggle-icicle-highlight-historical-candidates',
;;    `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-literal-replacement',
;;    `toggle-icicle-regexp-quote', `toggle-icicle-search-cleanup',
;;    `toggle-icicle-search-replace-whole',
;;    `toggle-icicle-search-whole-word', `toggle-icicle-sorting',
;;    `toggle-icicle-transforming'.
;;
;;  Faces defined in Icicles (in Custom group `icicles'):
;;
;;    `icicle-candidate-part',
;;    `icicle-common-match-highlight-Completions',
;;    `icicle-complete-input',
;;    `icicle-completing-mustmatch-prompt-prefix',
;;    `icicle-completing-prompt-prefix',
;;    `icicle-Completions-instruction-1',
;;    `icicle-Completions-instruction-2',
;;    `icicle-current-candidate-highlight',
;;    `icicle-historical-candidate', `icicle-input-completion-fail',
;;    `icicle-match-highlight-Completions',
;;    `icicle-match-highlight-minibuffer', `icicle-prompt-suffix',
;;    `icicle-saved-candidate', `icicle-search-context-level-1',
;;    `icicle-search-context-level-2',
;;    `icicle-search-context-level-3',
;;    `icicle-search-context-level-4',
;;    `icicle-search-context-level-5',
;;    `icicle-search-context-level-6',
;;    `icicle-search-context-level-7',
;;    `icicle-search-context-level-8', `icicle-search-current-input',
;;    `icicle-search-main-regexp-current',
;;    `icicle-search-main-regexp-others', `icicle-special-candidate',
;;    `icicle-whitespace-highlight', `minibuffer-prompt'.
;;
;;  User options defined in Icicles:
;;
;;    `icicle-act-before-cycle-flag', `icicle-add-buffer-name-flag',
;;    `icicle-alternative-sort-function',
;;    `icicle-anything-transform-candidates-flag',
;;    `icicle-bind-top-level-commands-flag', `icicle-buffer-configs',
;;    `icicle-buffer-extras',
;;    `icicle-buffer-ignore-space-prefix-flag',
;;    `icicle-buffer-match-regexp', `icicle-buffer-no-match-regexp',
;;    `icicle-buffer-predicate', `icicle-buffer-require-match-flag'
;;    `icicle-buffer-sort', `icicle-candidate-width-factor',
;;    `icicle-change-region-background-flag',
;;    `icicle-change-sort-order-completion-flag',
;;    `icicle-C-l-uses-completion-flag', `icicle-color-themes',
;;    `icicle-complete-keys-self-insert-flag',
;;    `icicle-completing-mustmatch-prompt-prefix',
;;    `icicle-completing-prompt-prefix',
;;    `icicle-completion-history-max-length',
;;    `icicle-Completions-display-min-input-chars',
;;    `icicle-Completions-frame-at-right-flag',
;;    `icicle-Completions-window-default-width',
;;    `icicle-Completions-window-max-height',
;;    `icicle-cycle-into-subdirs-flag',
;;    `icicle-cycling-respects-completion-mode-flag',
;;    `icicle-default-thing-insertion',
;;    `icicle-define-alias-commands-flag',
;;    `icicle-deletion-action-flag',
;;    `icicle-expand-input-to-common-match-flag',
;;    `icicle-fuzzy-completion-flag',
;;    `icicle-highlight-historical-candidates-flag',
;;    `icicle-highlight-input-completion-failure-flag',
;;    `icicle-highlight-input-initial-whitespace-flag',
;;    `icicle-ignore-space-prefix-flag',
;;    `icicle-incremental-completion-delay',
;;    `icicle-incremental-completion-flag',
;;    `icicle-incremental-completion-threshold',
;;    `icicle-init-value-flag', `icicle-input-string',
;;    `icicle-inter-candidates-min-spaces',
;;    `icicle-key-descriptions-use-<>-flag',
;;    `icicle-key-descriptions-use-angle-brackets-flag',
;;    `icicle-keymaps-for-key-completion', `icicle-kmacro-ring-max',
;;    `icicle-list-end-string', `icicle-list-join-string',
;;    `icicle-list-nth-parts-join-string',
;;    `icicle-mark-position-in-candidate',
;;    `icicle-minibuffer-setup-hook', `icicle-modal-cycle-down-key',
;;    `icicle-modal-cycle-up-key', `icicle-mode', `icicle-mode-hook',
;;    `icicle-point-position-in-candidate',
;;    `icicle-redefine-standard-commands-flag',
;;    `icicle-regexp-quote-flag', `icicle-regexp-search-ring-max',
;;    `icicle-region-alist', `icicle-region-auto-open-files-flag',
;;    `icicle-region-background', `icicle-regions-name-length-max',
;;    `icicle-reminder-prompt-flag', `icicle-require-match-flag',
;;    `icicle-saved-completion-sets', `icicle-search-cleanup-flag',
;;    `icicle-search-context-match-predicate',
;;    `icicle-search-highlight-all-current-flag',
;;    `icicle-search-highlight-context-levels-flag',
;;    `icicle-search-highlight-threshold', `icicle-search-hook',
;;    `icicle-search-replace-literally-flag',
;;    `icicle-search-replace-whole-candidate-flag',
;;    `icicle-search-ring-max', `icicle-search-whole-word-flag',
;;    `icicle-show-Completions-help-flag',
;;    `icicle-show-Completions-initially-flag',
;;    `icicle-sort-function', `icicle-sort-functions-alist',
;;    `icicle-special-candidate-regexp',
;;    `icicle-TAB-shows-candidates-flag',
;;    `icicle-thing-at-point-functions',
;;    `icicle-top-level-when-sole-completion-flag',
;;    `icicle-touche-pas-aux-menus-flag', `icicle-transform-function',
;;    `icicle-update-input-hook', `icicle-use-~-for-home-dir-flag',
;;    `icicle-use-candidates-only-once-flag',
;;    `icicle-word-completion-key', `icicle-WYSIWYG-Completions-flag',
;;    `icicle-yank-function'.
;;
;;  Non-interactive functions in Icicles:
;;
;;    `custom-variable-p', `icicle-abbreviate-or-expand-file-name',
;;    `icicle-activate-mark', `icicle-add-key+cmd',
;;    `icicle-all-candidates-action-1',
;;    `icicle-anything-candidate-value',
;;    `icicle-apply-to-saved-candidate', `icicle-apropos-candidates',
;;    `icicle-apropos-complete-1',
;;    `icicle-barf-if-outside-Completions',
;;    `icicle-barf-if-outside-Completions-and-minibuffer',
;;    `icicle-barf-if-outside-minibuffer', `icicle-binary-option-p',
;;    `icicle-bind-completion-keys', `icicle-bind-isearch-keys',
;;    `icicle-bind-S-TAB-for-map-variable',
;;    `icicle-bind-S-TAB-in-keymaps-from',
;;    `icicle-buffer-sort-*...*-last',
;;    `icicle-cancel-Help-redirection', `icicle-candidate-action-1',
;;    `icicle-candidate-set-1',
;;    `icicle-case-insensitive-string-less-p',
;;    `icicle-char-properties-in-buffer',
;;    `icicle-char-properties-in-buffers',
;;    `icicle-choose-anything-candidate',
;;    `icicle-choose-candidate-of-type', `icicle-clear-minibuffer',
;;    `icicle-color-blue-lessp', `icicle-color-green-lessp',
;;    `icicle-color-hue-lessp', `icicle-color-red-lessp',
;;    `icicle-color-saturation-lessp', `icicle-color-value-lessp',
;;    `icicle-comint-get-final-choice',
;;    `icicle-comint-get-minibuffer-input', `icicle-comint-hook-fn',
;;    `icicle-comint-send-input', `icicle-compilation-hook-fn',
;;    `icicle-command-names-alphabetic-p',
;;    `icicle-compilation-search-in-context-fn',
;;    `icicle-complete-keys-1', `icicle-complete-keys-action',
;;    `icicle-completing-p', `icicle-completing-read',
;;    `icicle-completing-read-history',
;;    `icicle-completion-setup-function',
;;    `icicle-current-completion-in-Completions',
;;    `icicle-customize-faces', `icicle-define-icicle-mode-map',
;;    `icicle-delete-current-candidate-object',
;;    `icicle-delete-file-or-directory', `icicle-delete-if',
;;    `icicle-delete-if-not', `icicle-delete-region-from-alist',
;;    `icicle-delete-whitespace-from-string', `icicle-dirs-last-p',
;;    `icicle-display-completion-list', `icicle-display-Completions',
;;    `icicle-display-candidates-in-Completions',
;;    `icicle-edmacro-parse-keys',
;;    `icicle-ensure-overriding-map-is-bound',
;;    `icicle-execute-extended-command-1', `icicle-expand-file-name',
;;    `icicle-face-valid-attribute-values', `icicle-file-directory-p',
;;    `icicle-file-name-apropos-candidates',
;;    `icicle-file-name-directory-w-default',
;;    `icicle-file-name-input-p', `icicle-file-name-nondirectory',
;;    `icicle-file-name-prefix-candidates', `icicle-file-readable-p',
;;    `icicle-file-writable-p', `icicle-files-within',
;;    `icicle-filter-alist', `icicle-filter-wo-input',
;;    `icicle-find-file-other-window-w-wildcards',
;;    `icicle-find-file-w-wildcards',
;;    `icicle-first-matching-candidate',
;;    `icicle-fit-Completions-window', `icicle-fix-default-directory',
;;    `icicle-flat-list', `icicle-font-name-history',
;;    `icicle-frame-name-history', `icicle-frames-on',
;;    `icicle-function-name-history', `icicle-fuzzy-candidates',
;;    `icicle-get-alist-candidate',
;;    `icicle-get-anything-actions-for-type',
;;    `icicle-get-anything-cached-candidates',
;;    `icicle-get-anything-candidates',
;;    `icicle-get-anything-candidates-of-type',
;;    `icicle-get-anything-default-actions-for-type',
;;    `icicle-get-anything-input-delay',
;;    `icicle-get-anything-req-pat-chars',
;;    `icicle-get-anything-types', `icicle-group-regexp',
;;    `icicle-help-on-candidate-symbol',
;;    `icicle-highlight-candidate-in-Completions',
;;    `icicle-highlight-complete-input',
;;    `icicle-highlight-initial-whitespace',
;;    `icicle-highlight-input-noncompletion',
;;    `icicle-historical-alphabetic-p', `icicle-imenu-command-p',
;;    `icicle-imenu-in-buffer-p',
;;    `icicle-imenu-non-interactive-function-p',
;;    `icicle-increment-cand-nb+signal-end',
;;    `icicle-increment-color-hue', `icicle-increment-color-value',
;;    `icicle-Info-goto-node-action', `icicle-Info-index-action',
;;    `icicle-insert-candidates',
;;    `icicle-insert-Completions-help-string',
;;    `icicle-insert-for-yank', `icicle-insert-input',
;;    `icicle-insert-thesaurus-entry-cand-fn', `icicle-insert-thing',
;;    `icicle-isearch-resume', `icicle-lisp-vanilla-completing-read',
;;    `icicle-key-description', `icicle-keys+cmds-w-prefix',
;;    `icicle-kill-a-buffer',
;;    `icicle-kill-a-buffer-and-update-completions',
;;    `icicle-kmacro-action', `icicle-last-modified-first-p',
;;    `icicle-longest-common-match', `icicle-make-color-candidate',
;;    `icicle-make-face-candidate', `icicle-map-action',
;;    `icicle-marker+text', `icicle-markers',
;;    `icicle-maybe-sort-and-strip-candidates',
;;    `icicle-minibuffer-contents',
;;    `icicle-minibuffer-contents-from-minibuffer',
;;    `icicle-minibuffer-prompt-end', `icicle-minibuffer-setup',
;;    `icicle-most-recent-first-p', `icicle-mouse-candidate-action-1',
;;    `icicle-msg-maybe-in-minibuffer',
;;    `icicle-nb-of-candidate-in-Completions',
;;    `icicle-next-candidate',
;;    `icicle-next-single-char-property-change',
;;    `icicle-non-whitespace-string-p', `icicle-part-1-lessp',
;;    `icicle-part-2-lessp', `icicle-part-3-lessp',
;;    `icicle-part-4-lessp', `icicle-part-N-lessp',
;;    `icicle-place-cursor', `icicle-place-overlay',
;;    `icicle-prefix-candidates', `icicle-prefix-complete-1',
;;    `icicle-prefix-keys-first-p', `icicle-put-at-head',
;;    `icicle-raise-Completions-frame', `icicle-read-face-name',
;;    `icicle-read-file-name', `icicle-read-from-minibuffer',
;;    `icicle-read-from-minibuf-nil-default',
;;    `icicle-read-single-key-description', `icicle-read-string',
;;    `icicle-read-var-value-satisfying',
;;    `icicle-rebind-completion-maps', `icicle-rebind-global',
;;    `icicle-recompute-candidates',
;;    `icicle-redefine-standard-commands',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns',
;;    `icicle-region-add-buffers', `icicle-remap',
;;    `icicle-region-help', `icicle-region-or-buffer-limits',
;;    `icicle-region-sorted', `icicle-remove-buffer-candidate-action',
;;    `icicle-remove-buffer-config-action',
;;    `icicle-remove-candidate-display-others',
;;    `icicle-remove-color-duplicates', `icicle-remove-dots',
;;    `icicle-remove-duplicates', `icicle-remove-property',
;;    `icicle-restore-completion-keys', `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-restore-standard-options',
;;    `icicle-retrieve-candidates-from-set', `icicle-reversible-sort',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-save-or-restore-input',
;;    `icicle-scroll-or-update-Completions', `icicle-search-action',
;;    `icicle-search-char-property-scan',
;;    `icicle-search-choose-buffers', `icicle-search-help',
;;    `icicle-search-highlight-all-input-matches',
;;    `icicle-search-highlight-and-maybe-replace',
;;    `icicle-search-highlight-input-matches-here',
;;    `icicle-search-in-context-default-fn',
;;    `icicle-search-property-args',
;;    `icicle-search-read-context-regexp', `icicle-search-read-word',
;;    `icicle-search-regexp-scan', `icicle-search-region-action',
;;    `icicle-search-replace-candidate',
;;    `icicle-search-replace-fixed-case-p',
;;    `icicle-search-replace-match', `icicle-search-where-arg',
;;    `icicle-select-minibuffer-contents'
;;    `icicle-select-region-action', `icicle-set-calling-cmd',
;;    `icicle-set-difference', `icicle-set-intersection',
;;    `icicle-set-union', `icicle-signum',
;;    `icicle-start-of-candidates-in-Completions',
;;    `icicle-strip-ignored-files-and-sort',
;;    `icicle-successive-action', `icicle-this-command-keys-prefix',
;;    `icicle-top-level-prep', `icicle-transform-candidates',
;;    `icicle-transform-multi-completion',
;;    `icicle-transform-sole-candidate', `icicle-try-switch-buffer',
;;    `icicle-unbind-S-TAB-for-map-variable',
;;    `icicle-unbind-S-TAB-in-keymaps-from',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-unsorted-apropos-candidates',
;;    `icicle-unsorted-file-name-apropos-candidates',
;;    `icicle-unsorted-file-name-prefix-candidates',
;;    `icicle-unsorted-prefix-candidates',
;;    `icicle-update-completions', `icicle-update-help-string',
;;    `icicle-update-ignored-extensions-regexp',
;;    `old-completing-read', `old-choose-completion-string',
;;    `old-completion-setup-function', `old-read-file-name'.
;;
;;  Internal variables defined in Icicles:
;;
;;    `icicle-all-candidates-action-fn',
;;    `icicle-all-candidates-action-p',
;;    `icicle-all-candidates-alternative-action-fn',
;;    `icicle-bookmark-history', `icicle-buffer-config-history',
;;    `icicle-candidate-action-fn',
;;    `icicle-candidate-alternative-action-fn',
;;    `icicle-candidate-entry-fn', `icicle-candidate-help-fn',
;;    `icicle-candidate-nb', `icicle-candidate-properties-alist',
;;    `icicle-candidates-alist', `icicle-char-property-value-history',
;;    `icicle-cmd-calling-for-completion', `icicle-color-history',
;;    `icicle-color-theme-history', `icicle-common-match-string',
;;    `icicle-complete-input-overlay', `icicle-complete-keys-alist',
;;    `icicle-completing-p', `icicle-completion-candidates'
;;    `icicle-completion-help-string',
;;    `icicle-completion-set-history',
;;    `icicle-current-completion-candidate-overlay',
;;    `icicle-current-completion-mode', `icicle-current-input',
;;    `icicle-current-raw-input', `icicle-default-directory',
;;    `icicle-default-thing-insertion-flipped-p',
;;    `icicle-delete-candidate-object', `icicle-dictionary-history',
;;    `icicle-edit-update-p', `icicle-extra-candidates',
;;    `icicle-face-name-history', `icicle-font-name-history',
;;    `icicle-frame-name-history', `icicle-function-name-history',
;;    `icicle-ignored-extensions', `icicle-ignored-extensions-regexp',
;;    `icicle-incremental-completion-p', `icicle-inhibit-sort-p',
;;    `icicle-initial-value', `icicle-input-fail-pos',
;;    `icicle-insert-string-at-pt-end',
;;    `icicle-insert-string-at-pt-start', `icicle-kill-history',
;;    `icicle-kmacro-alist', `icicle-kmacro-history',
;;    `icicle-last-completion-candidate',
;;    `icicle-last-completion-command', `icicle-last-input',
;;    `icicle-last-sort-function', `icicle-last-top-level-command',
;;    `icicle-last-transform-function', `icicle-list-use-nth-parts',
;;    `icicle-menu-items-alist', `icicle-menu-map',
;;    `icicle-minor-mode-map-entry', `icicle-mode-map',
;;    `icicle-must-match-regexp', `icicle-must-not-match-regexp',
;;    `icicle-must-pass-predicate',
;;    `icicle-nb-of-other-cycle-candidates',
;;    `icicle-object-named-types', `icicle-object-predicate-types',
;;    `icicle-pre-minibuffer-buffer', `icicle-post-command-hook',
;;    `icicle-pre-command-hook',
;;    `icicle-previous-raw-file-name-inputs',
;;    `icicle-previous-raw-non-file-name-inputs', `icicle-prompt',
;;    `icicle-prompt-suffix', `icicle-re-no-dot',
;;    `icicle-require-match-p', `icicle-reverse-sort-p',
;;    `icicle-saved-candidate-overlays',
;;    `icicle-saved-candidates-variables-obarray',
;;    `icicle-saved-completion-candidate',
;;    `icicle-saved-completion-candidates',
;;    `icicle-saved-completion-candidates-internal',
;;    `icicle-saved-ignored-extensions',
;;    `icicle-saved-kmacro-ring-max',
;;    `icicle-saved-regexp-search-ring-max',
;;    `icicle-saved-region-background',
;;    `icicle-saved-search-ring-max', `icicle-search-command',
;;    `icicle-search-context-level', `icicle-search-context-regexp',
;;    `icicle-search-current-overlay', `icicle-search-final-choice',
;;    `icicle-search-history', `icicle-search-in-context-fn',
;;    `icicle-searching-p', `icicle-search-level-overlays',
;;    `icicle-search-overlays', `icicle-search-refined-overlays',
;;    `icicle-search-replacement',
;;    `icicle-search-replacement-history',
;;    `icicle-successive-grab-count',
;;    `icicle-text-property-value-history',
;;    `icicle-thing-at-pt-fns-pointer',
;;    `icicle-universal-argument-map', `icicle-variable-name-history',
;;    `icicle-whole-candidate-as-text-prop-p'.
;;
;;  Emacs functions defined in Icicles for older Emacs versions.
;;
;;    `select-frame-set-input-focus'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED in Icicles:
;;
;;  `completing-read'              - (See below and doc string.)
;;  `display-completion-list'      - (See below and doc string.)
;;  `exit-minibuffer'              - Remove *Completion* window.
;;  `minibuffer-complete-and-exit' - Remove *Completion* window.
;;  `read-file-name'               - (See below and doc string.)
;;  `read-from-minibuffer'         - (See below and doc string.)
;;  `read-string'                  - (See below and doc string.)
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED in Icicles:
;;
;;  `dabbrev-completion' - Use Icicles completion when you repeat
;;                         (`C-M-/').
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' have
;;              been REDEFINED in Icicles:
;;
;;  `lisp-complete-symbol' - Selects *Completions* window even if on
;;                           another frame.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el' have
;;              been REDEFINED in Icicles:
;;
;;  `mouse-choose-completion' - Return the number of the completion.
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED in Icicles:
;;
;;  `choose-completion-string' -
;;     Don't exit minibuffer after `lisp-complete-symbol' completion.
;;  `completion-setup-function' - 1. Put faces on inserted string(s).
;;                                2. Help on help.
;;  `switch-to-completions' - Always selects *Completions* window.
;;
;;  `next-history-element' (advised only) -
;;     Depending on `icicle-init-value-flag', select minibuffer
;;     contents.
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2007/07/22 dadams
;;     Require icicles-cmd.el before icicles-mode.el.
;; 2007/06/07 dadams
;;     Moved all doc to new files icicles-doc1.el and icicles-doc2.el.
;; 2007/05/12 dadams
;;     Moved Search Enhancements subsections to top level: Isearch
;;       Completion, Icicles Search Commands, Overview, Search and Replace.
;; 2007/05/06 dadams
;;     Changed S-C- to C-S- and M-C- to C-M- in doc.
;; 2007/04/20 dadams
;;     Require icicles-face.el after icicles-opt.el.
;; 2007/03/09 dadams
;;     Renamed sections .*Removal of Duplicates to .*Removing Duplicates and
;;                      More on Multi-Commands to More About Multi-Commands.
;; 2007/02/24 dadams
;;     Added section More on Multi-Commands.  Added subsection Chipping Away
;;           the Non-Elephant to Nutshell View.
;; 2007/02/03 dadams
;;     Updated section Sorting Candidates and Removal of Duplicates.
;; 2007/01/28 dadams
;;     Added: subsection Using Progressive ... Process of Elimination.
;; 2007/01/21 dadams
;;     Added: section Text Properties in *Completions*.
;; 2007/01/19 dadams
;;     Added: section Programming Multi-Completions.
;; 2007/01/16 dadams
;;     Added linkd links.  Cleanup.
;; 2007/01/15 dadams
;;     Added: section Sorting Candidates and Removal of Duplicates.
;;     Renamed:
;;      icicle-sort-and-strip-ignored to icicle-strip-ignored-files-and-sort,
;;      icicle-dirs-last-p to icicle-dirs-last-p,
;;      icicle-sort-case-insensitively to *-case-insensitive-string-lessp.
;; 2007/01/12 dadams
;;     Updated section Multi-Completions for icicle-list-use-nth-parts.
;; 2007/01/06 dadams
;;     File-Name and Directory-Name Completion Tips:
;;       Mention icicle-use-~-for-home-dir-flag.
;; 2006/11/23 dadams
;;     Added icicle-TAB-shows-candidates-flag.
;; 2006/11/10 dadams
;;     Multi-Commands: Mention prompt prefix +.
;; 2006/11/05 dadams
;;     icicle-occur is bound to C-c '.  Search commands use multiple buffers.
;;     Added Nutshell subsection Perform Multiple Operations In One Command.
;; 2006/10/19 dadams
;;     Added Goggle Matching section.
;; 2006/10/16 dadams
;;     Added key completion to Nutshell View.
;; 2006/10/14 dadams
;;     Renamed: icicle-cancel-*Help*-* to icicle-cancel-Help-*.
;;     Moved conditional eval-when-compile to top level.
;; 2006/10/01 dadams
;;     Updated for new alternative-sort toggle:
;;       History Enhancements, Key Completion, Customization *, Key Bindings.
;; 2006/09/30 dadams
;;     Changed bindings of icicle-candidate-set-(save|retrieve) from C-<, C->
;;       to C-M-<, C-M->.
;;     Added icicle-key-descriptions-use-<>-flag in Customization section.
;; 2006/09/17 dadams
;;     Added section Key Completion.
;; 2006/09/12 dadams
;;     Added section Moving Between the Minibuffer and Other Buffers.
;; 2006/08/23 dadams
;;     Added sections Icicles Mult M-x and Defining Icicles Multi M-x.
;; 2006/08/18 dadams
;;     Added section Icicles Info Enhancements.
;; 2006/08/13 dadams
;;     Documented icicle-completing(-mustmatch)-prompt-prefix.
;; 2006/06/17 dadams
;;     Rewrote Multi-Commands, Defining Icicles Commands (Including
;;       Multi-Commands), and Defining Multi-Commands the Hard Way.
;;     Renamed: Defining Icicles Commands: + (Including Multi-Commands).
;;              Defining Multi-Commands: + the Hard Way.
;;     Added: Defining Multiple-Choice Menus.
;; 2006/06/08 dadams
;;     Removed require of icicle-keys.el (obsolete).
;; 2006/05/26 dadams
;;     Mention M-k as icicle-erase-minibuffer-or-history-element.
;;     Don't mention M-S-backspace and M-S-delete any more.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;       Updated its doc to reflect new functionality.
;; 2006/05/18 dadams
;;     Change install instructions to include turning on Icicle mode.
;; 2006/05/16 dadams
;;     Require icicles-keys.el when icicle-bind-top-level-commands-flag.
;;     Updated doc to reflect new library icicles-keys.el.
;; 2006/05/15 dadams
;;     Renamed: ici*-nospace-flag to icicle-ignore-space-prefix-flag.
;;     Updated doc of icicle-ignore-space-prefix-flag.
;;     Added doc of icicle-buffer-ignore-space-prefix-flag.
;; 2006/04/14 dadams
;;     Added section Inserting a Regexp from a Variable.
;; 2006/04/09 dadams
;;     Added descriptions of icicle-arrows-respect-completion-type-flag.
;; 2006/03/19 dadams
;;     Added description of icicle-expand-input-to-common-match-flag.
;; 2006/03/07 dadams
;;     Correct the description of icicle-doc - match against only the
;;       doc, not the symbol name.
;; 2006/03/06 dadams
;;     Reordered Commentary sections, putting Emacs-Lisp stuff later.
;; 2006/03/05 dadams
;;     Mention icicle-touche-pas-aux-menus-flag.
;; 2006/03/03 dadams
;;     Clarified Multi-Completions description.
;; 2006/03/01 dadams
;;     Added: icicle-(complete|insert)-thesaurus-entry.
;;            Updated Commentary.
;; 2006/02/27 dadams
;;     Split into multiple libraries:
;;       *-cmd, *-face, *-fn, *-mac, *-mode, *-opt, *-var.
;; 2006/02/25 dadams
;;     Added: icicle-narrow-candidates (bound to M-*),
;;            icicle-icicle-completing-p, icicle-set-calling-cmd,
;;            icicle-reset-icicle-completing-p,
;;            icicle-run-icicle-(pre|post)-command-hook.
;;     Add all hooks in icicle-mode only, except for minibuffer-local
;;       hooks (pre- and post-command).
;;     Remove all hooks when exit Icicle mode.
;;     icicle-completing-read, icicle-read-file-name:
;;       Add catch icicle-read-top.  Set icicle-icicle-completing-p.
;;       Separate case of not Icicle mode from other no-prompt cases.
;;     Reordered some groups of functions.
;; 2006/02/24 dadams
;;     icicle-candidate-set-1: Treat empty set.
;; 2006/02/21 dadams
;;     icicle-prefix-complete:
;;       Implemented icompletion here, like icicle-apropos-complete-1.
;;     icicle-call-then-update-Completions:
;;       Use icicle-last-completion-command, not
;;           icicle-apropos-complete.
;;     Renamed icicle-apropos-icompleting-p to icicle-icompleting-p.
;;     Added: icicle-(kill|delete)(-backward)-*, icicle-yank etc.
;;            Bound them.
;;     Added: icicle-call-then-update-Completions.
;;     Added: icicle-incremental-completion-p.
;;       Use instead of icicle-incremental-completion-flag everywhere.
;;       Upgrade from t in icicle-display-candidates-in-Completions.
;;       Reset in icicle-minibuffer-setup.
;;     icicle-isearch-complete:
;;       Use search-ring symbol as history arg to completing-read.
;;     icicle-display-candidates-in-Completions,
;;     icicle-keep-only-past-inputs, icicle-history:
;;       Ensure that minibuffer-history-variable is a list.
;;     Fixed typos:
;;       icicle-keep-past-inputs -> icicle-keep-only-past-inputs.
;; 2006/02/20 dadams
;;     icicle-insert-string-at-point: Treat negative prefix arg.
;;     Added: icicle-signum.
;;     icicle-insert-thing:
;;       Remove text properties of string to insert.
;; 2006/02/19 dadams
;;     icicle-thing-at-point-functions:
;;       Added function to grab successive text.
;;     icicle-insert-string-at-point:
;;       Treat successive-grab fn and prefix arg.
;;     Added: icicle-default-thing-insertion,
;;            icicle-default-thing-insertion-flipped-p,
;;            icicle-insert-string-at-pt-(start|end),
;;            icicle-successive-grab-count, icicle-insert-thing.
;;     Renamed: icicle-insert-string-near-point to
;;              icicle-insert-string-at-point.
;; 2006/02/18 dadams
;;     icicle-retrieve-last-input:
;;       Don't reset icicle-last-completion-command if not interactive
;;     icicle-candidate-set-complement, icicle-keep-only-past-inputs:
;;       Use icicle-retrieve-last-input.
;;     icicle-keep-only-past-inputs:
;;       Rewrote modeled on icicle-apropos-complete:
;;        Take into account singleton and empty candidate set.
;;        Provide input to icicle-display-ca*.
;;        Set icicle-last-completion-command.
;;     icicle-history: Force redisplay of *Completions*.
;;                     Don't set this-command.
;;     icicle-completing-read: Ensure icicle-initial-value is not nil.
;;     icicle-save-or-restore-input: Don't restore empty input.
;;     icicle-recompute-candidates:
;;       Don't recompute if last completion cmd was
;;       icicle-keep-only-past-inputs.
;;     Added: icicle-historical-candidate,
;;            icicle-keep-only-past-inputs.
;;     icicle-display-candidates-in-Completions:
;;       Use icicle-historical-candidate.
;;     Bind icicle-keep-only-past-inputs to M-pause in minibuffer
;;       completion maps.
;; 2006/02/17 dadams
;;     Added: icicle-complete-input-overlay,
;;            icicle-highlight-complete-input, icicle-complete-input.
;;     icicle-(prefix|apropos)-complete(-1):
;;       Use icicle-highlight-complete-input.
;;     Added icicle-inhibit-reminder-prompt-flag.
;;           Thx to Jonathan Simms for the suggestion.
;;     icicle-completing-read, icicle-read-file-name:
;;       Use icicle-inhibit-reminder-prompt-flag.
;; 2006/02/12 dadams
;;     icicle-read-string: Finished bug fix of 2/11.
;;                         More thx to Andrey Zhdanov.
;; 2006/02/11 dadams
;;     icicle-insert-string-near-point:
;;       Always start with first function.
;;     read-from-minibuffer: Bug fix: don't use def if init is consp.
;;                           Thx to Andrey Zhdanov.
;; 2006/02/09 dadams
;;     Added: icicle-insert-string-near-point,
;;            icicle-thing-at-point-functions,
;;            icicle-thing-at-pt-fns-pointer.
;;            Bound icicle-insert-string-near-point.
;;     Added Commentary section "Inserting Text Found Near the Cursor"
;;     Require: thingatpt+.el, thingatpt.el.
;;     Bug fix: icicle-execute-extended-command(-1):
;;              Take care of last-command and this-command.
;; 2006/02/08 dadams
;;     icicle-completing-read: Treat consp case of initial-input.
;;     icicle-read-file-name: Fixed bug introduced 02/02:
;;       Don't ensure initial-input is not null.
;; 2006/02/07 dadams
;;     Bug fix: Files menu find-file stuff was bound to *recent-file*.
;; 2006/02/03 dadams
;;     icicle-init-value-flag: Use nil as the default value.
;;     Added: icicle-read-from-minibuffer, icicle-read-string.
;;              Use in icicle-(redefine|restore)-standard-commands.
;; 2006/02/02 dadams
;;     icicle-completing-read, read-file-name:
;;       Respect icicle-init-value-flag only if default value not nil.
;;     read-file-name: Ensure initial-value is not null.
;;                     Initialize icicle-initial-value.
;;                     Respect icicle-init-value-flag.
;; 2006/01/29 dadams
;;     icicle-completing-read, icicle-read-file-name:
;;       Remove binding of ESC-TAB.
;;     icicle-lisp-complete-symbol:
;;       Enable recursive minibuffers if in minibuffer.
;;     Commentary: Combine lisp-complete-symbol with dabbrev.
;;     Updated bindings listed in icicle-completion-help-string.
;; 2006/01/28 dadams
;;     New feature: icicle-lisp-complete-symbol (added).
;;                  Added to Commentary and moved section.
;;     Corrected fix of 2005/12/14:
;;       icicle-minibuffer-setup:
;;         Save region background at recursion level 1.
;;       icicle-saved-region-background: defvar to nil.
;;     Added: icicle-increment-color-hue.
;;            Use in icicle-region-background.
;;     Added: icicle-(re)set-option-to-(nil|t), icicle-clear-option,
;;            icicle-toggle-option, icicle-binary-option-p.
;; 2006/01/26 dadams
;;     Added: icicle(-saved)(-regexp)-search-ring-max,
;;            icicle-(redefine|restore)-standard-options.
;;     icicle-mode: Use icicle-(redefine|restore)-standard-options.
;;                  Use icicle-(redefine|restore)-standard-commands
;;                    for Emacs 21+ also (forgot?).
;;     icicle-(redefine|restore)-*: Use defalias, not fset.
;; 2006/01/24 dadams
;;     New feature: icicle-isearch-complete.
;;       Added: icicle-isearch-complete, icicle-isearch-resume,
;;              icicle-bind-isearch-keys.
;;       icicle-mode: add/remove isearch-mode-hook.
;;     Minor bug fix: initial value was treated as
;;                    icicle-last-completion-candidate.
;;       Added: icicle-initial-value.
;;       icicle-completing-read, icicle-read-file-name:
;;         Set icicle-initial-value,
;;             not icicle-last-completion-candidate.
;;       icicle-next-candidate:
;;         Initialize icicle-last-completion-candidate to
;;           icicle-initial-value.
;;       icicle-save-or-restore-input:
;;         Don't change icicle-current-input if = icicle-initial-value
;;       Renamed: icicle-init-value to icicle-init-value-flag.
;; 2006/01/23 dadams
;;     Use command remapping for self-insert-command in Emacs 22.
;;     Changed icicle-(re|un)map to defsubst.
;;     Removed Commentary section on icicle-execute-extended-command.
;;     icicle-apropos-complete-and-exit, icicle-apropos-complete-1:
;;       Use flag icicle-apropos-complete-and-exit-p to suppress
;;         minibuffer-message.
;; 2006/01/22 dadams
;;     Added: icicle-execute-extended-command*.
;;     completing-read, icicle-read-file-name:
;;       Corrected nil case for icicle-require-match-flag (bug fix).
;;       Hard-code bindings, instead of using \\[...], so the simpler
;;         bindings are shown.
;;     Changed C-o to C-RET for consistency (C-o still works too).
;;       icicle-(bind|restore)-completion-keys: Added C-RET binding.
;; 2006/01/21 dadams
;;     icicle-mouse-choose-completion:
;;       Don't save selected window if it's *Completions*.
;;     Added more Commentary about icicle-retrieve-last-input.
;; 2006/01/20 dadams
;;     icicle-sort-and-strip-ignored:
;;       Don't ignore names if only ignored extensions match.
;;     Added: icicle-apropos-complete-and-exit.
;;            Bound it in icicle-rebind-completion-maps.
;;     icicle-minibuffer-setup: Don't reset icicle-require-match-flag.
;;     icicle-apropos-complete: Return the list of candidates.
;; 2006/01/19 dadams
;;     Added: icicle(-buffer)-require-match-flag.
;;            Thanks to Mathias Dahl for feedback.
;;            Use in completing-read, read-file-name, and
;;              icicle-minibuffer-setup.
;;     Re-alphabetized defcustoms.
;; 2006/01/07 dadams
;;     Added :link.
;; 2005/12/31 dadams
;;     Added: icicle-fix-default-directory.
;;     icicle-read-file-name:
;;       Use icicle-fix-default-directory hack to fix bug.
;; 2005/12/26 dadams
;;     Added icicle-sort-case-insensitively.
;;     Added more parent groups for icicles group.
;; 2005/12/14 dadams
;;     icicle-minibuffer-setup:
;;       Only save region background when at top level.
;;     Added: icicle-Completions-frame-at-right-flag.
;;            Use in icicle-candidate-action.
;;     Added: defvars for font-lock-keyword-face,
;;            font-lock-function-name-face.
;; 2005/12/09 dadams
;;     Fontify icicle-define* in emacs-lisp-mode.
;; 2005/12/02 dadams
;;     Added: icicle-customize-apropos*.
;;            Use in icicle-(redefine|restore)-standard-commands.
;; 2005/12/01 dadams
;;     Added: icicle-repeat-complex-command,
;;            icicle-redefine-standard-commands-flag,
;;            icicle-(redefine|restore)-standard-commands.
;; 2005/11/30 dadams
;;     Added: icicle-apropos-zippy.
;;     icicle-apropos-command, icicle-apropos-variable:
;;       Corrected completing-read for do-all arg.
;;     icicle-apropos-command, icicle-apropos-option:
;;       My version must not respect apropos-do-all.
;; 2005/11/29 dadams
;;     Added: icicle-apropos*.
;;     icicle-help-on-candidate: Treat plists.
;;                               Message "No help" is the default.
;; 2005/11/25 dadams
;;     Added: icicle-dabbrev-completion.
;;     Renamed all names with "Completions" to use "Completions", for
;;       coherence with XEmacs port.
;; 2005/11/24 dadams
;;     icicle-mouse-choose-completion:
;;       Delete *Completions* window systematically.
;; 2005/11/21 dadams
;;     icicle-delete-windows-on:
;;       Avoid error Attempt to delete minibuffer or sole ... window.
;;     icicle-prefix-complete, icicle-apropos-complete-1,
;;     icicle-next-candidate:
;;       Use icicle-delete-windows-on, not delete-window.
;;     icicle-candidate-set-save: Use map in doc string.
;;     icicle-compilation-search: Tidied up doc string.
;;     Use #' for clarity.
;; 2005/11/20 dadams
;;     icicle-completing-read:
;;       Added treatment of completions that are lists of strings.
;;     Updated Commentary: new section on completions that are lists.
;;     Added: icicle-list-join-string, icicle-doc, icicle-fundoc,
;;            icicle-vardoc.
;; 2005/11/15 dadams
;;     Temporarily removed defadvice of next-history-element for
;;       Emacs 22.  Bug reported.
;;     icicle-minibuffer-prompt-end: Changed from defsubst to defun.
;; 2005/11/13 dadams
;;     icicle-mouse-candidate-action:
;;       buffer-substring -> buffer-substring-no-properties.
;;     icicle-completing-read:
;;       Bind, don't set, minibuffer-completion-table.
;;     icicle-buffer*: Use other buffer for DEF, not INIT-VALUE.
;;     Added: icicle-preselect-init-value-flag,
;;            icicle-(add|remove)-buffer-*,
;;            icicle-read-from-minibuf-nil-default,
;;            icicle-buffer-list, icicle-select-minibuffer-contents,
;;            icicle-completing-p.
;;     icicle-minibuffer-setup:
;;       Select minibuf contents if icicle-preselect-init-value-flag.
;;       Only display *Completions* if icicle-completing-p.
;;     Advised next-history-element.
;; 2005/11/11 dadams
;;     Added: icicle-show-*Completions*-initially-flag,
;;            icicle-display-*Completions*.
;;     icicle-minibuffer-setup:
;;       If icicle-show-*Completions*-initially-flag, display it.
;; 2005/11/09 dadams
;;     Added: icicle-mouse-candidate-action.
;;            Bind in icicle-rebind-completion-maps.
;;     icicle-buffer(-other-window):
;;       Use buffer-name-history as HIST arg to completing-read.
;; 2005/11/08 dadams
;;     Add/remove hook icicle-cancel-*Help*-redirection in
;;       icicle-mode, not at top level.
;;     Removed icicle-reset-icicle-menu-items-alist.
;;     Reset icicle-menu-items-alist in icicle-execute-menu-command
;;       of icicles-menu.el.
;; 2005/11/06 dadams
;;     Include minibuffer-local-filename-completion-map.
;; 2005/11/05 dadams
;;     icicle-display-candidates-in-*Completions*:
;;       Don't try to highlight root if it is "".
;;     icicle-help-on-candidate:
;;       Test null, not boundp icicle-menu-items-alist.
;;       If menu item's command is a lambda, set cand-symb to nil.
;;     icicle-mode: Use icicle-reset-icicle-menu-items-alist on
;;                  minibuffer-exit-hook.
;;     Added: icicle-reset-icicle-menu-items-alist.
;;     Added defvar for icicle-menu-items-alist.
;;     Added byte-compiler comments and defvars to quiet byte-compile.
;; 2005/11/04 dadams
;;     icicle-display-candidates-in-*Completions:
;;       Bug fix - use (functionp minibuffer-completion-table), not
;;                 (icicle-file-name-input-p).
;; 2005/11/03 dadams
;;     Added: icicle-filter-wo-input and vars icicle-must-*,
;;            icicle-extra*, icicle-buffer-*, icicle-buffer-config*,
;;            icicle-buffer-sort*.
;;     icicle-unsorted-*:
;;       Use icicle-filter-wo-input and icicle-extra-candidates.
;;     Added Commentary section Global Filters.
;;     icicle-buffer* commands: Added filter bindings.
;;     icicle-define(-file)-command: Minor bug fix:
;;       Ensure buffer is live before switching back.
;; 2005/11/01 dadams
;;     Added: icicle-must(-not)-match-regexp.
;;            Use in icicle-unsorted-*-candidates.
;; 2005/10/31 dadams
;;     Added: icicle-use-default-as-init-value-flag.
;;            Use in completing-read.
;;     icicle-find-file*: Minor bug fix - REQUIRE-MATCH should be nil.
;; 2005/10/29 dadams
;;     icicle-display-candidates-in-*Completions:
;;       Minor bug fix - wrap in save-window-excursion.
;;     icicle-minibuffer-contents-from-minibuffer:
;;       Minor bug fix - do nothing if file & user erased minibuffer.
;;     Menu-bar menus:
;;       Enable Icicles menu items only in Icicle mode.  Put search
;;       stuff on Search menu, if available.   Use "[Icy]" prefix for
;;       Icicles items in menus other than "Icicles".
;; 2005/10/28 dadams
;;     Added: icicle-define-file-command.
;;            Use it to define icicle-delete-file, icicle-find-file*.
;;     icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Do action before moving to next|prev.
;;     icicle-candidate-action:
;;       Raise *Completions* frame, to keep it on top.
;; 2005/10/27 dadams
;;     Added: icicle-define-command, icicle-find-file*,
;;            select-frame-set-input-focus.
;;     Redefined using icicle-define-command:
;;       icicle-bookmark, icicle-buffer*, icicle-color-theme,
;;       icicle-delete-file, icicle-find-file*, icicle-font,
;;       icicle-frame-*, icicle-recent-file*.
;;     icicle-all-candidates-action:
;;       Report failures, not successes.  Use error msg.
;;     Added Commentary sections: Special-Character Conflicts,
;;                                Defining Icicles Commands.
;;     Commentary section Act on All Candidates:
;;       Added delete-one-or-more-files example.
;;     Added icicle-find-file* to menu-bar menus.
;;     Inactivated top-level menu items when minibuffer is active.
;;     Renamed:
;;       icicle-delete-file-1 to icicle-delete-file-or-directory.
;; 2005/10/25 dadams
;;     Thx to Lennart Borgman for suggestion about
;;       select-frame-set-input-focus.
;; 2005/10/24 dadams
;;     icicle-search:
;;       1) Bug fix - need to have mouse-choose-completion set
;;          icicle-candidate-nb.
;;       2) Show error message.
;;     Default value of icicle-candidate-nb is now nil, not -1.
;;     Added: icicle-mouse-choose-completion,
;;            icicle-nb-of-candidate-in-*Completions*.
;;     icicle-move-to-(next|previous)-completion,
;;     icicle-increment-cand-nb+signal-end:
;;       Reset candidate number to 0 if nil.
;;     icicle-(redefine|restore)-std-completion-fns:
;;       Use icicle-mouse-choose-completion.
;; 2005/10/23 dadams
;;     Added: icicle-mode-map.
;;     icicle-(bind|restore)-completion-keys: Updated menu-bar menu.
;;     icicle-compilation-search:
;;       Error if not in a compilation buffer.
;; 2005/10/21 dadams
;;     icicle-remove-duplicates: redefined.
;; 2005/10/18 dadams
;;     icicle-file-name-input-p doc string:
;;       Mention why don't use minibuffer-completing-file-name.
;; 2005/10/16 dadams
;;     Added: icicle-compilation-search, icicle-search-hook.
;;     icicle-search: Run icicle-search-hook.
;;                    Added optional sit-for-period arg.
;;     icicle-mode: Added list of top-level commands to doc string.
;;     icicle-scroll-or-update-*Completions*:
;;       Added msg arg - only display msg if don't scroll.
;; 2005/10/14 dadams
;;     Allow for multisets of candidates.
;;     Added: icicle-search, icicle-completion-nospace-flag,
;;            icicle-candidate-nb, icicle-filter-alist,
;;            icicle-increment-cand-nb+signal-end.
;;     Commentary: Updated for icicle-search.
;;     icicle-next-candidate: Major rewrite.
;;       Uses icicle-candidate-nb,
;;         icicle-increment-cand-nb+signal-end,
;;         icicle-move-to-next-completion.
;;     Use icicle-completion-nospace-flag in calls to all-completions.
;;     icicle-previous-(apropos|prefix)-candidate,
;;     icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Added optional arg.
;;     icicle-apropos-complete-1, icicle-next-candidate,
;;     icicle-recompute-candidates:
;;       Added *-action commands to memq test.
;;     icicle-move-to-next-completion:
;;       Added optional no-minibuffer-follow-p arg.
;;     icicle-scroll-or-update-*Completions*:
;;       Update display even if handle-switch-frame.
;; 2005/10/12 dadams
;;     Added: icicle-redefine-std-completion-fns,
;;            icicle-restore-std-completion-fns,
;;            icicle-delete-windows-on, icicle-frames-on.
;;     icicle-mode: Use icicle-redefine-std-completion-fns,
;;                  icicle-restore-std-completion-fns.
;;     Renamed to use icicle- prefix: choose-completion-string,
;;       completing-read, completion-setup-function, exit-minibuffer,
;;       minibuffer-complete-and-exit, read-file-name,
;;       switch-to-completions.  Added these and also old- versions.
;;     icicle-history: Treat file names also.
;;     remove-windows-on -> icicle-delete-windows-on.
;; 2005/10/11 dadams
;;     Added: icicle-history, icicle-scroll-or-update-*Completions*,
;;            icicle-undo-std-completion-faces.
;;     Minor bug fixes:
;;       icicle-remove-dots: Also match against "." and ".."
;;         (lack of slash in Emacs 21+).
;;       icicle-save-or-*: Don't reset to last input if
;;                         icicle-last-completion-candidate is "".
;;                         Update icicle-last-completion-candidate
;;                         also to use current input.
;;       Reset icicle-last-input in icicle-minibuffer-setup, not in
;;         completing-read and read-file-name.
;;       icicle-display-candidates-in-*Completions*,
;;       icicle-next-candidate:
;;         Put candidate in consp before applying predicate.
;;       icicle-recompute-candidates:
;;         Don't recompute unless icicle-last-completion-command.
;;       icicle-retrieve-last-input:
;;         Use icicle-current-input, not icicle-last-input.
;;       icicle-self-insert:
;;         Update icicle-current-input and set this-command to
;;         icicle-apropos-complete.
;;       icicle-apropos-complete: Use error-message-string.
;;       icicle-apropos-complete-1:
;;         Protect icicle-file-directory-p with
;;         icicle-file-name-input-p.  Unconditionally update
;;         icicle-last-completion-command.
;;     Moved undoing of std completion faces to icicle-mode.
;;     Use icicle-scroll-or-update-*Completions* in
;;         icicle-candidate-set-1.
;; 2005/10/06 dadams
;;     icicle-prefix-complete, icicle-apropos-complete-1:
;;       Removed vestigial slash cruft - should have been removed in
;;         2005/09/01 fix.
;;     Added: icicle-remove-dots.
;;            Use in icicle-save-or-restore-input.
;; 2005/10/05 dadams
;;     icicle-msg-maybe-in-minibuffer: use format-string arg.
;; 2005/10/04 dadams
;;     Replace use of minibuffer-completion-help by
;;       icicle-apropos-complete.
;;     Added: icicle-recent-file*, icicle-toggle-ignored-extensions,
;;            icicle-update-completions,
;;            icicle-msg-maybe-in-minibuffer,
;;            icicle-saved-ignored-extensions.
;;     Bound icicle-toggle-*.
;;     icicle-toggle-sorting:
;;       Use icicle-update-completions, icicle-msg-maybe-in-minibuffer
;;     icicle-sort-and-strip-ignored:
;;       icicle-ignored-extensions-regexp nil => nothing is ignored.
;;     Reorder key bindings, so prompt shows S-tab, not S-iso-lefttab.
;;     icicle-next-candidate: Fixed code to highlight candidate in
;;       *Completions*: restriction.
;; 2005/10/03 dadams
;;     Regexps can now use backslash (it is no longer a directory
;;       separator on MS Windows).
;;       icicle-minibuffer-contents-from-minibuffer,
;;       icicle-file-name-directory-w-default:
;;         Escape backslash, so not used as directory separator on
;;         MS Windows.
;;       Added: icicle-apropos-complete-1,
;;              icicle-file-name-nondirectory.
;;       icicle-apropos-complete: Use icicle-apropos-complete-1.
;;                                Treat regexp error via message.
;;       Use icicle-file-name-nondirectory everywhere, instead of
;;         file-name-nondirectory.
;;     Can now use "?" for regexps; it no longer shows completion list
;;     Do icicle-update-ignored-extensions-regexp inside
;;       icicle-minibuffer-setup.
;;     Added and bound: icicle-retrieve-last-input.
;;     Updated icicle-completion-help-string with recent bindings.
;;     Renamed: icicle-last-command to icicle-last-completion-command.
;;              icicle-candidate-set-restore to
;;              icicle-candidate-set-retrieve.
;; 2005/10/01 dadams
;;     Added: icicle-candidate-set-(define|restore|swap).
;;     Changed binding of icicle-candidate-set-save to C->.
;;     Bound new commands.
;; 2005/10/01 dadams
;;     Major rewrite to add set operations: complement, difference,
;;                                          union, intersection.
;;       Added: icicle-completion-candidates, icicle-current-input,
;;              icicle-candidate-set-*, icicle-set-*,
;;              icicle-save-or-restore-input,
;;              icicle-recompute-candidates.
;;       Bound icicle-candidate-set*.
;;       Added Commentary for Sets of Completion Candidates.
;;       icicle-(apropos|prefix)-complete:
;;         Update icicle-completion-candidates, only as needed.
;;       icicle-next-candidate:
;;         Reverse candidates only if switched to opposite-direction
;;           command of same type.
;;         Likewise, for refresh of *Completions*.
;;         Protect put-text-property for root (e.g. no match for
;;           complement).
;;       icicle-(apropos|prefix)-complete,
;;       icicle-prefix-word-complete, icicle-next-candidate:
;;         use icicle-completion-candidates.
;;       icicle-all-candidates-action:
;;         Use icicle-completion-candidates, not
;;         icicle-apropos-complete.
;;       icicle-display-candidates-in-*Completions*:
;;         Removed first arg (candidates).
;;         Update icicle-completion-candidates.
;;    icicle-all-candidates-action:
;;      Use icicle-completion-candidates, so act on completions of
;;      either kind.
;; 2005/09/30 dadams
;;     Commented out resetting of minibuffer-completion-table to nil
;;     for icompletion.
;;     Thx to Andrey for bug report on M-x M-r problem.
;; 2005/09/27 dadams
;;     icicle-(bind|restore)-completion-keys:
;;       Bound [S-iso-lefttab] like [S-tab].
;; 2005/09/26 dadams
;;     Bug fix: Changed "\C-!"  to [(control ?!)] (others similarly).
;;     Bound [S-iso-lefttab] like [S-tab].
;; 2005/09/16 dadams
;;     Added: icicle-all-candidates-action, icicle-delete-file*,
;;     icicle-rebind-completion-maps:
;;       Bound icicle-all-candidates-action to C-!.
;;     icicle-(apropos|prefix)-complete: Return candidates list.
;;     icicle-bookmark, icicle-buffer*, icicle-color-theme,
;;     icicle-font, icicle-frame*:
;;       Return t for success, nil for failure.
;;     Commentary: Added section Choose All Completion Candidates.
;; 2005/09/14 dadams
;;     icicle-rebind-completion-maps:
;;       Bound TAB and S-TAB for navigation.
;;     icicle-move-to-(next|previous)-completion,
;;     icicle-(next|previous)-line: Wrap around.
;; 2005/09/13 dadams
;;     Major rewrite of file treatment, to treat directory candidates
;;       similarly to files.
;;     Added: icicle-default-directory, icicle-file-directory-p,
;;            icicle-sort-function, icicle-toggle-sorting,
;;            toggle-icicle-sorting.
;;     Use icicle-file-directory-p everywhere, except
;;       choose-completion-string.
;;     Removed: icicle-nondirectory-*.
;;     icicle-next-candidate:
;;       If not icicle-cycle-into-subdirs-flag, then use relative
;;       file/dir name, not nondirectory part.
;;     icicle-(apropos|prefix)-complete:
;;       Set icicle-default-directory if sole completion is a
;;       subdirectory.
;;     icicle-sort-and-strip-ignored:
;;       Removed optional arg and treatment of subdirs.
;;     icicle-next-(apropos|prefix)-candidate,
;;     icicle-(apropos|prefix)-complete:
;;       Don't treat icicle-cycle-into-subdirs-flag here.
;;     icicle-(apropos|prefix)-complete, icicle-next-candidate:
;;       Set icicle-default-directory, if directory candidate.
;;     icicle-minibuffer-setup: Set icicle-default-directory.
;;     icicle-apropos-complete:
;;       Different message if icicle-apropos-icompleting-p.
;;     icicle-sort-dirs-last:
;;       Treat other kinds of candidates, besides files and dirs.
;;     Commentary and doc strings: Updated for icicle-sort-function,
;;                                 icicle-cycle-into-subdirs.
;;     Let delete-selection mode work with icicle-self-insert.
;;     icicle-display-candidates-in-*Completions*:
;;       Make *Completions* read-only.
;; 2005/09/09 dadams
;;     choose-completion-string:
;;       bug fix for Emacs 21.3.1 - use Emacs 20 version for 21.3.1.
;; 2005/09/08 dadams
;;     completion-setup-function:
;;       bug fix for Emacs 21.3.1 - use Emacs 20 version for 21.3.1.
;;     Added: icicle-remap, icicle-unmap,
;;            icicle-(bind|restore)-completion-keys.
;;     completing-read: Do not append suffix if not in Icicle mode.
;;     icicle-rebind-completion-maps:
;;       Clean-up.  Use icicle-(bind|restore)-completion-keys.
;;       Don't (suppress-keymap completion-list-mode-map).
;; 2005/09/06 dadams
;;     Provided apropos icompletion.
;;     Added: icicle-self-insert, icicle-incremental-completion-flag,
;;            icicle-apropos-icompleting-p.
;;     icicle-apropos-complete: Respect icicle-apropos-icompleting-p.
;;     Commentary: Updated Icompletion and Customization sections.
;;                 Added Apropos Icompletion.
;;     Changed default value of icicle-word-completion-key to M-SPC.
;;     icicle-rebind-completion-maps:
;;       Bind icicle-self-insert. Use self-insert for SPC.
;;       Updated icicle-completion-help-string.
;;       Treat menu-bar menu for the minibuffer.
;;     completion-setup-function:
;;       Only add instruction2 when icicle-mode.
;;     icicle-display-candidates-in-*Completions*:
;;       Use save-restriction.
;;     icicle-minibuffer-contents-from-minibuffer:
;;       Allow for mixing $ of environment vars with $ of regexps.
;; 2005/09/02 dadams
;;     Added: icicle-bookmark, icicle-buffer(-other-window),
;;            icicle-candidate-action, icicle-candidate-action-fn,
;;            icicle-color-theme(s), icicle-font, icicle-frame-(b|f)g.
;;     Renamed: icicle-(next|previous)-(apropos|prefix)-*-help to
;;              icicle-(next|previous)-(apropos|prefix)-*-action.
;;     icicle-(apropos|prefix)-complete:
;;       Set icicle-last-completion-candidate.
;;     In renamed functions:
;;       Use icicle-candidate-action, not icicle-help-on-candidate.
;;     icicle-rebind-completion-maps:
;;       Bound C-o to icicle-candidate-action.
;;     Added Commentary section on actions on candidates.
;;     icicle-move-to-next-completion:
;;       Test line num, not char position (fix).
;;     icicle-previous-line: 3 or 4, not 4 or 5 (fix).
;; 2005/09/01 dadams
;;     Fixed major bug: file-name completion did not work at all in
;;       non-MS Windows!
;;       icicle-file-name-(apropos|prefix)-candidates:
;;         Removed code for case where input starts with "/".
;;       icicle-nondirectory-file-name-(apropos|prefix)-candidates:
;;         Removed code for case where input starts with "/".
;;         Bind default-directory.
;;       icicle-(apropos|prefix)-complete:
;;         Treat case when icicle-cycle-into-subdirs-flag = nil.
;;     icicle-next-candidate:
;;       Took out code that moved point when line is too long.
;;     icicle-minibuffer-setup: Reset icicle-prompt.
;; 2005/08/23 dadams
;;     Added: icicle-help-on-candidate,
;;            icicle-cancel-*Help*-redirection,
;;            icicle-(previous|next)-(prefix|apropos)-candidate-help.
;;            Bound them all.
;;     icicle-rebind-completion-maps:
;;       Bound icicle-help-on-candidate,
;;       icicle-(previous|next)-(prefix|apropos)-candidate-help.
;; 2005/08/22 dadams
;;     Unconditionally require cl.el when compile (because of case).
;; 2005/08/19 dadams
;;     Renamed icicle-cursor-position-in-candidate to
;;             icicle-point-position-in-candidate.
;;     Added: icicle-mark-position-in-candidate,
;;            icicle-minibuffer-prompt-end.
;;     icicle-place-cursor: Position both point and mark.
;;     icicle-point-position-in-candidate:
;;       Change values from bob, eob to input-start/end.
;;     Removed: icicle-select-rest-of-completion-flag.
;;              Use inequality test on point and mark.
;;     Updated commentary.
;; 2005/08/16 dadams
;;     Minbuffer messages:
;;       Differentiate prefix from apropos completion.
;;     completing-read, read-file-name:
;;       Append icicle-prompt-suffix for Emacs 20 (oversight).
;; 2005/08/15 dadams
;;     Bug fix: Only use face-spec-reset-face if target faces defined.
;;     read-file-name: bug fix:
;;       Use condition-case to get the correct number of args for
;;       old-read-file-name. Thx to Mathias Dahl for both bug reports.
;; 2005/08/14 dadams
;;     icicle-place-cursor: Narrow region to exclude minibuffer-prompt
;; 2005/08/13 dadams
;;     Add regexp support (removed it when introduced highlighting).
;;       icicle-next-candidate:
;;         Added regexp-p arg.  Use in icicle-next-apropos-candidate.
;;       icicle-place-cursor:
;;         Use regexp search.  For root-start, go to match-beginning.
;;       icicle-unsorted-file-name-apropos-candidates:
;;         Don't use regexp-quote.
;;     icicle-switch-to-*Completions*:
;;       Search in restriction of mouse-face zone; repeat.
;;       Treat file case (use nondirectory part).
;;       Bind case-fold-search.
;;     Protect (aref <input> 0) against empty string.
;;     member -> memq, for symbols.
;; 2005/08/12 dadams
;;     Added: icicle-word-completion-key, icy-mode,
;;            icicle-insert-a-space.
;;     icicle-rebind-completion-maps:
;;       Use icicle-word-completion-key and icicle-insert-a-space.
;;     completing-read, icicle-rebind-completion-maps:
;;       Corrected bindings in doc string.
;; 2005/07/29 dadams
;;     Added: icicle-change-region-background-flag,
;;            icicle-increment-color-value, icicle-region-background,
;;            icicle-saved-region-background,
;;            icicle-restore-region-face.
;;     Added icicle-restore-region-face to minibuffer-exit-hook.
;;     Require hexrgb.el.
;;     Removed: icicle-select-rest-of-completion.
;;     icicle-minibuffer-setup: Save icicle-saved-region-background
;;                              and use icicle-region-background.
;; 2005/07/28 dadams
;;     Added: icicle-*Completions*-instruction-*.
;;     completion-setup-function:
;;       Use icicle-*Completions*-instruction-*.
;;       Remove ? from instruction2.  Put both instr on same line.
;;       Use put-text-property, not *-w-face*.
;;     ------
;;     Move all completion stuff here, from simple+.el:
;;       choose-completion-string, completion-setup-function,
;;       switch-to-completions.
;;     Wrap *Completions* window deletion in save-selected-window.
;;     Added icicle-prefix-word-complete, and bound it to SPC.
;;     completion-setup-function: Renamed
;;       icicle-completing-read-prompt-suffix to icicle-prompt-suffix.
;; 2005/07/27 dadams
;;     Renamed: icicle-completing-read-prompt* to icicle-prompt*.
;;     Added: read-file-name, face
;;            icicle-completing-read-prompt-suffix,
;;            icicle-remove-property,
;;            icicle-select-rest-of-completion (simple, for now).
;;     completing-read: Apply faces to prompt.
;;     icicle-place-cursor: Use icicle-select-rest-of-completion.
;;     Added (if icicle-mode (icicle-mode 1)) at end.
;;     Reworded Commentary in terms of "input completion", not just
;;       completing-read.
;; 2005/07/26 dadams
;;     rebind-minibuffer-completion-maps: Minor bug fix.
;;     icicle-mode: Added " Icy" to mode line.
;;     Wrapped Emacs 21 version of icicle-mode (with
;;       define-minor-mode) in (eval (quote...)), so byte-compiling
;;       in Emacs 20 will produce a *.elc that works in Emacs 21.
;; 2005/07/25 dadams
;;     Added: icicle-mode, icicle-*-hook, icicle-minibuffer-setup,
;;            icicle-activate-mark.
;;     rebind-minibuffer-completion-maps:
;;       Restore bindings when exit Icicle mode.
;;       Added argument.  Pick up everything bound to help-command.
;;                        Updated doc string.
;;       Message only when mode is turned on.
;; 2005/07/24 dadams
;;     Now leave region from end of root to end of completion, so you
;;       can easily replace it, especially if you use
;;       delete-selection mode.  (Suggestion by Lennart Borgman.)
;;     Added: icicle-select-rest-of-completion-flag.
;;     icicle-place-cursor:
;;       Create active region if icicle-select-rest-of-completion-flag
;;     icicle-completion-help: Removed icicle-abort-minibuffer-input.
;;     icicle-abort-minibuffer-input:
;;       Removed obsolete code & comment on icomplete-inhibit.
;; 2005/07/22 dadams
;;     Major fixup: Treat file and directory names well, respect std
;;                  user options, more.
;;     Renamed:
;;       icicle-(next|previous)?-completion-candidate to
;;         icicle-*-prefix-candidate(s),
;;       icicle*filename* to icicle*file-name*,
;;       icicle-descend-into-subdirs to
;;         icicle-cycle-into-subdirs-flag.
;;     Added: icicle-file-name-apropos-candidates,
;;            icicle-file-name-directory-w-default,
;;            icicle-file-name-input-p,
;;            icicle-file-name-prefix-candidates,
;;            icicle-nondirectory-file-name-apropos-candidates,
;;            icicle-nondirectory-file-name-prefix-candidates,
;;            icicle-sort-dirs-last,
;;            icicle-unsorted-apropos-candidates,
;;            icicle-unsorted-file-name-apropos-candidates,
;;            icicle-unsorted-file-name-prefix-candidates,
;;            icicle-unsorted-prefix-candidates, icicle-last-command.
;;     Respect insert-default-directory and completion-auto-help.
;;     Use minibuffer-message instead of message.
;;     Commentary: Added Customization & Tips section.
;;     completing-read: Updated doc string.  Save icicle-last-input.
;;                      Reset icicle-nb-of-other-cycle-candidates.
;;     icicle-next-*-candidate: Branch to file-specific functions.
;;     icicle-*-candidates: Use icicle-unsorted-*-candidates.
;;     icicle-next-candidate:
;;       Delete *Completions* window if no candidates.
;;       Use icicle-file-name-directory, not file-name-directory.
;;     icicle-minibuffer-contents-from-minibuffer:
;;       Use substitute-in-file-name.
;;     icicle-*-complete:
;;       Treat slashed file names (e.g. "/foo").
;;       Use icicle-file-name-*-candidates,
;;         icicle-file-name-directory-w-default for files.
;;       Added messages [No completion], [Sole completion],
;;         [Complete, but not unique].
;;       Use icicle-last-command for repetition test. And set it.
;;     icicle-rebind-completion-maps:
;;       Updated icicle-completion-help-string and message.
;; 2005/07/21 dadams
;;     icicle-apropos-candidates:
;;       Use, not apropos, but delete-if-not on string-match.
;;       Treat files too.
;;     Removed icicle-intersection.
;;     Added: icicle-descend-into-subdirs.
;;     icicle-sort-and-strip-ignored: Use icicle-descend-into-subdirs.
;;                                    Don't use "." and "..".
;;     icicle-next-candidate:
;;       File names w/o dir.
;;       Use regexp-quote on root for underlining file-name root.
;;       Insert directory name for file.
;;     icicle-place-cursor:
;;       Search past dir, then search for file-name w/o dir.
;;     icicle-prefix-complete, icicle-apropos-complete,
;;     icicle-switch-to-*Completions*:
;;       Use icicle-minibuffer-contents-from-minibuffer.
;;     icicle-prefix-complete, icicle-apropos-complete:
;;       Insert dir when single candidate.
;;     icicle-display-candidates-in-*Completions*:
;;       Underline file-name w/o dir.
;; 2005/07/20 dadams
;;     icicle-next-candidate,
;;     icicle-display-candidates-in-*Completions*:
;;       Use set-buffer-modified-p.
;;     icicle-next-candidate: Use ding when hit end of cycle.
;;     Added: icicle-cursor-position-in-candidate,
;;            icicle-place-cursor.
;;            Use in icicle-next-candidate to position cursor.
;;     Added: defgroup icicles.
;; 2005/07/19 dadams
;;     Initialize icicle-ignored-*.
;;     Added: icicle-nb-of-other-cycle-candidates,
;;            icicle-minibuffer-contents-from-minibuffer.
;;     completing-read: Reset icicle-last-completion-candidate to nil.
;;     icicle-next-candidate:
;;       Use icicle-minibuffer-contents-from-minibuffer.
;;       Save icicle-nb-of-other-cycle-candidates for
;;         icomplete-completions (icomplete+).
;;       Use copy of "next" string since change its text properties.
;;       Use regexp-quote for underlined root.
;;       Use put-text-property, so works in Emacs 20.
;;       Update *Completions*, even if last command is repeated.
;;     icicle-*-complete: Complete rewrite.
;;     icicle-display-candidates-in-*Completions*:
;;       Do even if last command is repeated.
;; 2005/07/18 dadams
;;     icicle-display-*:
;;       Highlight only first occurrence in each candidate.
;;     icicle-next-candidate: Use completion-ignore-case.
;; 2005/07/17 dadams
;;     Treat file names also.
;;     Added: icicle-delete-if*, and use instead of delete-if-*.
;;            Removed require cl.el.
;;     Added: icicle-ignored-extensions*,
;;            icicle-sort-and-strip-ignored,
;;            icicle-filename-input-p,
;;            icicle-update-ignored-extensions-regexp,
;;            icicle-prefix-complete.  Bound icicle-prefix-complete.
;;     Use icicle-update-ignored-extensions-regexp as
;;       minibuffer-setup-hook.
;;     icicle-*-candidates: Use icicle-sort-and-strip-ignored.
;;     icicle-next-candidate,
;;     icicle-display-candidates-in-*Completions*:
;;       Don't use predicate on file-name candidates
;;       (icicle-filename-input-p).
;;     icicle-next-candidate:
;;       Use read-file-name-completion-ignore-case (Emacs 22) and
;;       file-name-nondirectory.
;;     icicle-apropos-complete:
;;       Return t/nil. Treat single candidate as no-op.
;;     Reset std completions-* faces, so they don't interfere with
;;       apropos highlighting.
;; 2005/07/16 dadams
;;     Added: icicle-display-*, icicle-apropos-complete.
;;     Use icicle-display-* in icicle-next-candidate and
;;       icicle-apropos-complete.
;;     Bound icicle-apropos-complete to S-tab in completion maps.
;;     icicle-switch-to-*Completions*:
;;       Move to start of candidate.  Highlight candidate, not regexp.
;;     icicle-next-candidate: Underline the root that was completed.
;;     Added: faces icicle-root-highlight-*.
;;     Removed: faces: icicle-completion-help*.
;;     Removed (not used): require of strings.el.
;;     Commentary: Added Nutshell View.
;; 2005/07/15 dadams
;;     Renamed: icicle-completion-help+ to icicle-completion-help.
;;     Replaced: icicle-delete-lines by icicle-erase-minibuffer.
;;     icicle-next-candidate:
;;       Wrapped display-* and re-search-forward in condition-case.
;;       Use icicle-place-overlay.
;;     Changed icicle-completion-help bindings to [f1].
;;     Added: icicle-*-line, icicle-switch-to-*,
;;            icicle-move-to-*-completion,
;;            icicle-current-completion-in-*Completions*,
;;            icicle-place-overlay.
;;     Added bindings for icicle-*-line, icicle-switch-to-*,
;;                        icicle-move-to-*.
;;     Bound q to icicle-abort-minibuffer-input in
;;       completion-list-mode-map.
;;     icicle-completing-read-prompt-suffix: Mention both [f1] and ?.
;;     Removed: icicle-fit-frame.
;;     Commentary: Added How...Improves...(5).  Updated Key Bindings.
;; 2005/07/14 dadams
;;     icicle-next-candidate:
;;       Update *Completions*, if displayed, to reflect current
;;       candidates, but don't do it if this-command = last-command.
;;       Reverse list as needed, to keep same order.   Ensure current
;;       *Completions* choice shows in window (recenter as needed).
;;       For highlighting: Search with re-search-forward to be sure
;;                         to get the right one.
;;       Took test for presence of predicate out of loop.
;;     Commentary: Added Note on pop-up-frames = t.
;; 2005/07/13 dadams
;;     Rewrote icicle-apropos-candidates.
;;     Added: icicle-intersection.
;; 2005/07/12 dadams
;;     Added: icicle-(next|previous)-apropos-candidate,
;;            icicle-next-candidate, icicle-apropos-candidates,
;;            icicle-completion-candidates.
;;     Bound: icicle-(next|previous)-apropos-candidate.
;;     Renamed: icicle-completion-help-(title-)face: Removed "-face".
;;     icicle-next-completion-candidate:
;;       Redefined to use icicle-next-candidate.
;;     icicle-rebind-completion-maps:
;;       Updated text to mention apropos completion.
;;     icicle-completion-help+:
;;       Use icicle-abort-minibuffer-input, not abort-recursive-edit.
;; 2005/07/10 dadams
;;     First version of icicles.el (previously called elect-mbuf.el).
;;     Renamed: minibuffer-completion-help-string to
;;              icicle-completion-help-string,
;;       completing-read-prompt to icicle-completing-read-prompt,
;;       completing-read-prompt-suffix to
;;        icicle-completing-read-prompt-suffix,
;;       mbuf-completion-help-face to icicle-completion-help-face,
;;       mbuf-completion-help-title-face to
;;         icicle-completion-help-title-face,
;;       minibuffer-last-default to icicle-last-completion-candidate,
;;       command-calling-for-completion to
;;         icicle-cmd-calling-for-completion,
;;       minibuffer-completion-help+ to icicle-completion-help+,
;;       abort-minibuffer-input to icicle-abort-minibuffer-input,
;;       next-default-input to icicle-next-completion-candidate,
;;       previous-default-input to
;;         icicle-previous-completion-candidate,
;;       rebind-minibuffer-completion-maps to
;;         icicle-rebind-completion-maps,
;;     Added: minibuffer-complete-and-exit, icicle-fit-frame,
;;            icicle-last-input.
;;     Moved delete-lines here from and renamed to
;;       icicle-delete-lines.
;;     Removed: mod+ (unused).
;;     icicle-completion-help+:
;;       Use *Help*, not *Completions*.  Don't show completions.
;;     icicle-next-completion-candidate:
;;       Use insert, not insert-string.
;;     icicle-rebind-completion-maps: Made it interactive.
;; 2005/07/09 dadams
;;     Removed: buffer-alist (not used).
;; 2005/05/15 dadams
;;     Renamed: flash-ding-minibuffer-frame to
;;              1on1-flash-ding-minibuffer-frame.
;; 2005/05/10 dadams
;;     Hacked completing-read to remove *Completions* window at end
;;       if require-match is non-nil.  (Don't know why/when this
;;       became a problem.)
;; 2004/09/21 dadams
;;     Updated to work in Emacs 21 (and 20):
;;       next-default-input uses delete-minibuffer-contents for 21,
;;       but erase-buffer for 20.
;;       minibuffer-completion-help+:
;;         bind inhibit-read-only to t around erase-buffer.
;; 2001/01/10 dadams
;;     Protected remove-windows-on via fboundp.
;; 1999/09/03 dadams
;;     Added: mbuf-completion-help-face,
;;            mbuf-completion-help-title-face.
;;     minibuffer-completion-help+:
;;       Use mbuf-*-face's instead of hard-coding.
;;     minibuffer-completion-help-string,
;;     completing-read-prompt-suffix: defconst -> defvar.
;; 1999/08/26 dadams
;;     Protected faces via boundp.
;; 1999/04/13 dadams
;;     Bound delete-lines to M-S-DEL and M-S-backspace.
;; 1999/03/17 dadams
;;     protect calls with test fboundp.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/03/26 dadams
;;     minibuffer-completion-help+: concat -> concat-w-faces (color).
;; 1995/12/20 dadams
;;     exit-minibuffer: Iconify *Completion* frame.
;; 1995/12/15 dadams
;;     abort-minibuffer-input:
;;       Reset minibuffer-completion-table to avoid icompletion.
;;     Defined replacement exit-minibuffer to do the same as #1.
;; 1995/12/01 dadams
;;     abort-minibuffer-input: Incorporated delete-selection-mode code
;;     rebind-minibuffer-completion-maps: Added C-g bindings for
;;       minibuffer-local-map, minibuffer-local-ns-map,
;;       minibuffer-local-isearch-map.
;; 1995/10/25 dadams
;;     Put defvar of minibuffer-completion-help-string after do
;;       rebind-minibuffer-completion-maps, so its doc string gives
;;       bindings.
;; 1995/10/24 dadams
;;     Mention ESC-TAB completion in completing-read.
;; 1995/10/17 dadams
;;     Let minibuffer use ESC-TAB for completion (Lisp symbols etc.)
;;     completing-read:
;;       Minibuffer adopts current buffer's ESC-TAB binding.
;;     Added command-calling-for-completion to memorize current
;;       command (done in completion-setup-hook).
;; 1995/09/12 dadams
;;     Added abort-minibuffer-input.
;;     Define C-g as abort-minibuffer-input in
;;       completion-list-mode-map and minibuffer-local-* maps.
;;     No self-insertion for completion-list-mode-map.
;; 1995/08/16 dadams
;;     next-default-input: Fixed bug - skip repeated alist entries.
;; 1995/08/10 dadams
;;     Rewrote minibuffer-completion-help+:
;;       Provide help even if no completions.
;;     So, added minibuffer-completion-help-string.
;;     `?' defined correctly for minibuffer-local-must-match-map.
;; 1995/08/08 dadams
;;     next-default-input: error msg: no hard coding of key seq.
;; 1995/08/02 dadams
;;     Major rewrite.
;;       No reminders in prompts.  Added minibuffer-completion-help+
;;       to provide help info for *Completions*.
;;     Log for functions that were previously in simple+.el:
;;       choose-completion-string, completion-setup-function,
;;       switch-to-completions.
;; 2005/07/28 dadams
;;     completion-setup-function:
;;       Renamed icicle-completing-read-prompt-suffix to
;;         icicle-prompt-suffix.
;; 2005/07/15 dadams
;;     choose-completion-string, completion-setup-function:
;;       Updated for Emacs 21+.
;; 2005/07/10 dadams
;;     Renamed: command-calling-for-completion to
;;              icicle-cmd-calling-for-completion.
;; 2004/09/21 dadams
;;     Only redefine choose-completion-string if prior to Emacs 21.
;; 1999/03/17 dadams
;;     choose-completion-string:
;;       Added doc string.  Updated to correspond to Emacs 34.1.
;;     completion-setup-function: diff prompt setups.
;;       face1 & face2 tests.
;;     Added: switch-to-completions.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Warn that Icicles is designed for use with a windowing system.
(unless window-system
  (with-output-to-temp-buffer "*WARNING*"
    (princ "You probably do NOT want to use Icicles without a windowing ")
    (princ "system (manager).\n\n")
    (princ "Consider using `emacsclient' (or `gnuclient').  ")
    (princ "Icicles makes use of many\n")
    (princ "keys that are unavailable when running Emacs in console mode.  ")
    (princ "If you do\n")
    (princ "use Icicles in this mode, you will want to rebind those keys - ")
    (princ "see file\n`icicles-mode.el'.\n"))
  (message "*WARNING* - Icicles uses keys that require a windowing system")
  (sit-for 5))

(eval-when-compile
 (when (< emacs-major-version 20) (require 'cl))) ;; when, unless

;;;;;;;;;;;;;


;;; Load other Icicles files (except documentation) ------------------

(require 'icicles-opt)
(require 'icicles-var)
(require 'icicles-face) ;; Requires opt

(require 'icicles-fn) ;; Requires opt, var
(require 'icicles-mac) ;; Requires var
(require 'icicles-mcmd) ;; Requires opt, var, fn, mac
(require 'icicles-cmd) ;; Requires mac, opt, var, fn, mcmd
(require 'icicles-mode) ;; Requires opt, cmd

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles.el ends here
