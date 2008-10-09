;;; icicles.el --- Minibuffer input completion and cycling.
;;
;; Filename: icicles.el
;; Description: Minibuffer completion and cycling.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2008, Drew Adams, all rights reserved.
;; Created: Tue Aug  1 14:21:16 1995
;; Version: 22.0
;; Last-Updated: Sat Oct  4 10:26:15 2008 (-0700)
;;           By: dradams
;;     Update #: 22247
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `apropos-fn+var', `avoid', `cl',
;;   `color-theme', `cus-edit', `cus-face', `cus-load', `cus-start',
;;   `custom', `dired', `dired+', `dired-aux', `dired-x', `doremi',
;;   `easymenu', `ediff-diff', `ediff-help', `ediff-init',
;;   `ediff-merg', `ediff-mult', `ediff-util', `ediff-wind', `ffap',
;;   `ffap-', `fit-frame', `frame-cmds', `frame-fns', `help+20',
;;   `hexrgb', `icicles-cmd', `icicles-face', `icicles-fn',
;;   `icicles-mac', `icicles-mcmd', `icicles-mode', `icicles-opt',
;;   `icicles-var', `info', `info+', `kmacro', `menu-bar',
;;   `menu-bar+', `misc-cmds', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `mwheel', `pp', `pp+', `ring', `ring+',
;;   `second-sel', `strings', `subr-21', `thingatpt', `thingatpt+',
;;   `unaccent', `w32-browser', `w32browser-dlgopen', `wid-edit',
;;   `wid-edit+', `widget'.
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
;;  See also: Library `lacarte.el', which lets you execute menu
;;  commands, cycling and completing them.  It is not part of Icicles,
;;  but it is greatly enhanced by Icicles.
 
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
;;    `icicle-add-entry-to-saved-completion-set',
;;    `icicle-add-file-to-fileset', `icicle-add-region',
;;    `icicle-add/update-saved-completion-set', `icicle-anything',
;;    `icicle-apply' `icicle-apropos', `icicle-apropos-command',
;;    `icicle-apropos-function', `icicle-apropos-option',
;;    `icicle-apropos-variable', `icicle-apropos-zippy',
;;    `icicle-bbdb-complete-name', `icicle-bookmark',
;;    `icicle-bookmark-jump', `icicle-bookmark-jump-other-window',
;;    `icicle-bookmark-other-window', `icicle-buffer',
;;    `icicle-buffer-config', `icicle-buffer-list',
;;    `icicle-buffer-other-window',
;;    `icicle-change-alternative-sort-order',
;;    `icicle-change-sort-order', `icicle-clear-history',
;;    `icicle-clear-current-history', `icicle-color-theme',
;;    `icicle-comint-command', `icicle-comint-search',
;;    `icicle-command-abbrev', `icicle-compilation-search',
;;    `icicle-complete-keys', `icicle-complete-thesaurus-entry',
;;    `icicle-customize-apropos', `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options',
;;    `icicle-customize-apropos-options-of-type',
;;    `icicle-customize-face', `icicle-customize-icicles-group',
;;    `icicle-dabbrev-completion', `icicle-delete-file',
;;    `icicle-delete-window', `icicle-delete-windows',
;;    `icicle-delete-windows-on', `icicle-describe-file',
;;    `icicle-describe-option-of-type', `icicle-directory-list',
;;    `icicle-dired-project', `icicle-dired-project-other-window',
;;    `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window',
;;    `icicle-dired-save-marked',
;;    `icicle-dired-save-marked-as-project',
;;    `icicle-dired-save-marked-more',
;;    `icicle-dired-save-marked-persistently',
;;    `icicle-dired-save-marked-to-variable', `icicle-doc',
;;    `icicle-exchange-point-and-mark',
;;    `icicle-execute-extended-command',
;;    `icicle-execute-named-keyboard-macro', `icicle-face-list',
;;    `icicle-file', `icicle-file-list', `icicle-file-other-window',
;;    `icicle-find-file', `icicle-find-file-absolute',
;;    `icicle-find-file-absolute-other-window',
;;    `icicle-find-file-other-window', `icicle-find-tag',
;;    `icicle-font', `icicle-frame-bg', `icicle-frame-fg',
;;    `icicle-fundoc', `icicle-generic-S-tab',
;;    `icicle-goto-global-marker',
;;    `icicle-goto-global-marker-or-pop-global-mark',
;;    `icicle-goto-marker', `icicle-goto-marker-or-set-mark-command',
;;    `icicle-grep-saved-file-candidates',
;;    `icicle-handle-switch-frame', `icicle-imenu',
;;    `icicle-imenu-command', `icicle-imenu-non-interactive-function',
;;    `icicle-Info-goto-node', `icicle-Info-goto-node-cmd',
;;    `icicle-Info-index', `icicle-Info-index-20',
;;    `icicle-Info-index-cmd', `icicle-Info-menu',
;;    `icicle-Info-menu-cmd', `icicle-insert-char',
;;    `icicle-insert-kill', `icicle-insert-thesaurus-entry',
;;    `icicle-keyword-list', `icicle-kill-buffer', `icicle-kmacro',
;;    `icicle-lisp-complete-symbol', `icicle-locate-file',
;;    `icicle-locate-file-other-window', `icicle-mode', `icy-mode',
;;    `icicle-object-action', `icicle-occur',
;;    `icicle-other-window-or-frame', `icicle-plist',
;;    `icicle-pop-tag-mark', `icicle-pp-eval-expression',
;;    `icicle-read-color', `icicle-read-kbd-macro',
;;    `icicle-recent-file', `icicle-recent-file-other-window',
;;    `icicle-regexp-list', `icicle-region-open-all-files',
;;    `icicle-remove-all-regions-in-buffer',
;;    `icicle-remove-buffer-candidate', `icicle-remove-buffer-config',
;;    `icicle-remove-entry-from-saved-completion-set',
;;    `icicle-remove-region', `icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command', `icicle-reset-option-to-nil',
;;    `icicle-save-string-to-variable', `icicle-search',
;;    `icicle-search-all-regions', `icicle-search-buffer',
;;    `icicle-search-char-property', `icicle-search-dired-marked',
;;    `icicle-search-file', `icicle-search-generic',
;;    `icicle-search-highlight-cleanup', `icicle-search-keywords',
;;    `icicle-search-overlay-property', `icicle-search-region',
;;    `icicle-search-text-property', `icicle-search-word',
;;    `icicle-select-frame', `icicle-select-frame-by-name',
;;    `icicle-select-region', `icicle-select-window',
;;    `icicle-select-window-by-name', `icicle-send-bug-report',
;;    `icicle-set-option-to-t', `icicle-skip-this-command',
;;    `icicle-sort-alphabetical', `icicle-sort-by-abbrev-frequency',
;;    `icicle-sort-by-directories-last',
;;    `icicle-sort-by-last-file-modification-time',
;;    `icicle-sort-by-last-use',
;;    `icicle-sort-by-previous-use-alphabetically',
;;    `icicle-sort-by-2nd-parts-alphabetically',
;;    `icicle-sort-case-insensitive',
;;    `icicle-sort-proxy-candidates-first',
;;    `icicle-sort-special-candidates-first',
;;    `icicle-sort-special-candidates-first',
;;    `icicle-sort-turned-OFF', `icicle-tags-search',
;;    `icicle-toggle-~-for-home-dir',
;;    `icicle-toggle-alternative-sorting',
;;    `icicle-toggle-angle-brackets',
;;    `icicle-toggle-apropos-match-function',
;;    `icicle-toggle-case-sensitivity', `icicle-toggle-C-for-actions',
;;    `icicle-toggle-fuzzy-completion',
;;    `icicle-toggle-highlight-all-current',
;;    `icicle-toggle-highlight-historical-candidates',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-incremental-completion',
;;    `icicle-toggle-literal-replacement', `icicle-toggle-option',
;;    `icicle-toggle-proxy-candidates', `icicle-toggle-regexp-quote',
;;    `icicle-toggle-remote-file-testing',
;;    `icicle-toggle-search-cleanup',
;;    `icicle-toggle-search-replace-whole',
;;    `icicle-toggle-search-whole-word', `icicle-toggle-sorting',
;;    `icicle-toggle-transforming',
;;    `icicle-toggle-WYSIWYG-Completions', `icicle-vardoc',
;;    `icicle-where-is', `icicle-yank-insert', `toggle',
;;    `toggle-icicle-~-for-home-dir',
;;    `toggle-icicle-alternative-sorting',
;;    `toggle-icicle-angle-brackets',
;;    `toggle-icicle-apropos-match-function',
;;    `toggle-icicle-case-sensitivity', `toggle-icicle-C-for-actions',
;;    `toggle-icicle-fuzzy-completion',
;;    `toggle-icicle-highlight-all-current',
;;    `toggle-icicle-highlight-historical-candidates',
;;    `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-literal-replacement',
;;    `toggle-icicle-proxy-candidates', `toggle-icicle-regexp-quote',
;;    `toggle-icicle-remote-file-testing',
;;    `toggle-icicle-search-cleanup',
;;    `toggle-icicle-search-replace-whole',
;;    `toggle-icicle-search-whole-word', `toggle-icicle-sorting',
;;    `toggle-icicle-transforming',
;;    `toggle-icicle-WYSIWYG-Completions', `what-which-how'.
;;
;;   Commands to be used mainly in the minibuffer or *Completions*:
;;
;;    `icicle-abort-recursive-edit', `icicle-all-candidates-action',
;;    `icicle-all-candidates-alt-action',
;;    `icicle-all-candidates-list-action',
;;    `icicle-all-candidates-list-alt-action',
;;    `icicle-apropos-complete', `icicle-apropos-complete-and-exit',
;;    `icicle-apropos-complete-and-narrow',
;;    `icicle-apropos-complete-no-display',
;;    `icicle-backward-delete-char-untabify',
;;    `icicle-backward-kill-paragraph',
;;    `icicle-backward-kill-sentence', `icicle-backward-kill-sexp',
;;    `icicle-backward-kill-word', `icicle-beginning-of-line+',
;;    `icicle-candidate-action', `icicle-candidate-alt-action',
;;    `icicle-candidate-read-fn-invoke',
;;    `icicle-candidate-set-complement',
;;    `icicle-candidate-set-define',
;;    `icicle-candidate-set-difference',
;;    `icicle-candidate-set-intersection',
;;    `icicle-candidate-set-retrieve',
;;    `icicle-candidate-set-retrieve-from-variable',
;;    `icicle-candidate-set-retrieve-persistent',
;;    `icicle-candidate-set-save', `icicle-candidate-set-save-more',
;;    `icicle-candidate-set-save-more-selected',
;;    `icicle-candidate-set-save-persistently',
;;    `icicle-candidate-set-save-selected',
;;    `icicle-candidate-set-save-to-variable',
;;    `icicle-candidate-set-swap', `icicle-candidate-set-truncate',
;;    `icicle-candidate-set-union',
;;    `icicle-change-alternative-sort-order',
;;    `icicle-change-sort-order', `icicle-choose-completion',
;;    `icicle-clear-current-history', `icicle-completion-help',
;;    `icicle-Completions-mouse-3-menu',
;;    `icicle-search-define-replacement',
;;    `icicle-delete-backward-char', `icicle-delete-candidate-object',
;;    `icicle-delete-char', `icicle-digit-argument',
;;    `icicle-dispatch-C-^', `icicle-dispatch-C-.',
;;    `icicle-dispatch-C-comma', `icicle-dispatch-M-comma',
;;    `icicle-dispatch-M-q', `icicle-doremi-candidate-width-factor',
;;    `icicle-doremi-inter-candidates-min-spaces',
;;    `icicle-end-of-line+', `icicle-erase-minibuffer',
;;    `icicle-erase-minibuffer-or-history-element',
;;    `icicle-exit-minibuffer', `icicle-help-on-candidate',
;;    `icicle-help-on-next-apropos-candidate',
;;    `icicle-help-on-next-prefix-candidate',
;;    `icicle-help-on-previous-apropos-candidate',
;;    `icicle-help-on-previous-prefix-candidate', `icicle-history',
;;    `icicle-insert-completion', `icicle-insert-history-element',
;;    `icicle-insert-key-description',
;;    `icicle-insert-list-join-string',
;;    `icicle-insert-newline-in-minibuffer',
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
;;    `icicle-mouse-remove-candidate',
;;    `icicle-mouse-save/unsave-candidate',
;;    `icicle-mouse-save-then-kill', `icicle-mouse-yank-secondary',
;;    `icicle-move-to-next-completion',
;;    `icicle-move-to-previous-completion',
;;    `icicle-narrow-candidates',
;;    `icicle-narrow-candidates-with-predicate',
;;    `icicle-negative-argument', `icicle-next-apropos-candidate',
;;    `icicle-next-apropos-candidate-action',
;;    `icicle-next-apropos-candidate-alt-action',
;;    `icicle-next-apropos-match-function',
;;    `icicle-next-candidate-per-mode', `icicle-next-line',
;;    `icicle-next-prefix-candidate',
;;    `icicle-next-prefix-candidate-action',
;;    `icicle-next-prefix-candidate-alt-action',
;;    `icicle-pp-eval-expression-in-minibuffer',
;;    `icicle-prefix-complete', `icicle-prefix-complete-no-display',
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
;;    `icicle-save-predicate-to-variable',
;;    `icicle-save/unsave-candidate', `icicle-scroll-Completions',
;;    `icicle-scroll-Completions-up',
;;    `icicle-search-define-replacement', `icicle-self-insert',
;;    `icicle-sort-alphabetical', `icicle-sort-by-abbrev-frequency',
;;    `icicle-sort-by-directories-last',
;;    `icicle-sort-by-last-file-modification-time',
;;    `icicle-sort-by-last-use',
;;    `icicle-sort-by-previous-use-alphabetically',
;;    `icicle-sort-case-insensitive',
;;    `icicle-sort-proxy-candidates-first', `icicle-sort-turned-OFF',
;;    `icicle-switch-to-Completions-buf',
;;    `icicle-switch-to-completions',
;;    `icicle-switch-to/from-minibuffer',
;;    `icicle-toggle-~-for-home-dir', `icicle-toggle-C-for-actions',
;;    `icicle-toggle-alternative-sorting',
;;    `icicle-toggle-angle-brackets',
;;    `icicle-toggle-case-sensitivity',
;;    `icicle-toggle-expand-to-common-match',
;;    `icicle-toggle-fuzzy-completion',
;;    `icicle-toggle-highlight-all-current',
;;    `icicle-toggle-highlight-historical-candidates',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-incremental-completion',
;;    `icicle-toggle-literal-replacement',
;;    `icicle-toggle-proxy-candidates', `icicle-toggle-regexp-quote',
;;    `icicle-toggle-search-cleanup',
;;    `icicle-toggle-search-replace-common-match',
;;    `icicle-toggle-search-replace-whole',
;;    `icicle-toggle-search-whole-word', `icicle-toggle-sorting',
;;    `icicle-toggle-transforming', `icicle-transpose-chars',
;;    `icicle-transpose-sexps', `icicle-transpose-words',
;;    `icicle-universal-argument', `icicle-universal-argument-minus',
;;    `icicle-universal-argument-more',
;;    `icicle-universal-argument-other-key', `icicle-yank',
;;    `icicle-yank-pop', `icicle-yank-secondary',
;;    `old-choose-completion', `old-exit-minibuffer',
;;    `old-minibuffer-complete-and-exit', `old-switch-to-completions',
;;    `toggle-icicle-~-for-home-dir', `toggle-icicle-C-for-actions',
;;    `toggle-icicle-alternative-sorting',
;;    `toggle-icicle-angle-brackets',
;;    `toggle-icicle-case-sensitivity',
;;    `toggle-icicle-expand-to-common-match',
;;    `toggle-icicle-fuzzy-completion',
;;    `toggle-icicle-highlight-all-current',
;;    `toggle-icicle-highlight-historical-candidates',
;;    `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-literal-replacement',
;;    `toggle-icicle-proxy-candidates', `toggle-icicle-regexp-quote',
;;    `toggle-icicle-search-cleanup',
;;    `toggle-icicle-search-replace-common-match',
;;    `toggle-icicle-search-replace-whole',
;;    `toggle-icicle-search-whole-word', `toggle-icicle-sorting',
;;    `toggle-icicle-transforming'.
;;
;;  Faces defined in Icicles (in Custom group `icicles'):
;;
;;    `icicle-candidate-part',
;;    `icicle-common-match-highlight-Completions',
;;    `icicle-complete-input', `icicle-completion',
;;    `icicle-Completions-instruction-1',
;;    `icicle-Completions-instruction-2',
;;    `icicle-current-candidate-highlight',
;;    `icicle-historical-candidate', `icicle-input-completion-fail',
;;    `icicle-input-completion-fail-lax',
;;    `icicle-match-highlight-Completions',
;;    `icicle-match-highlight-minibuffer',
;;    `icicle-mustmatch-completion', `icicle-proxy-candidate',
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
;;    `icicle-add-proxy-candidates-flag',
;;    `icicle-alternative-sort-function',
;;    `icicle-anything-transform-candidates-flag',
;;    `icicle-apropos-complete-no-display-keys',
;;    `icicle-buffer-configs', `icicle-buffer-extras',
;;    `icicle-buffer-ignore-space-prefix-flag',
;;    `icicle-buffer-match-regexp', `icicle-buffer-no-match-regexp',
;;    `icicle-buffer-predicate', `icicle-buffer-require-match-flag'
;;    `icicle-buffer-sort', `icicle-candidate-width-factor',
;;    `icicle-change-region-background-flag',
;;    `icicle-change-sort-order-completion-flag',
;;    `icicle-C-l-uses-completion-flag', `icicle-color-themes',
;;    `icicle-command-abbrev-alist',
;;    `icicle-command-abbrev-match-all-parts-flag',
;;    `icicle-command-abbrev-priority-flag',
;;    `icicle-complete-keys-self-insert-flag',
;;    `icicle-completion-history-max-length',
;;    `icicle-Completions-display-min-input-chars',
;;    `icicle-Completions-frame-at-right-flag',
;;    `icicle-Completions-window-max-height',
;;    `icicle-customize-save-flag', `icicle-cycle-into-subdirs-flag',
;;    `icicle-cycling-respects-completion-mode-flag',
;;    `icicle-default-thing-insertion', `icicle-default-value',
;;    `icicle-define-alias-commands-flag',
;;    `icicle-deletion-action-flag',
;;    `icicle-expand-input-to-common-match-flag',
;;    `icicle-filesets-as-saved-completion-sets-flag',
;;    `icicle-fuzzy-completion-flag', `icicle-generic-S-tab-keys',
;;    `icicle-highlight-historical-candidates-flag',
;;    `icicle-highlight-input-completion-failure',
;;    `icicle-highlight-input-completion-failure-delay',
;;    `icicle-highlight-input-completion-failure-threshold',
;;    `icicle-highlight-input-initial-whitespace-flag',
;;    `icicle-highlight-lighter-flag',
;;    `icicle-ignore-space-prefix-flag',
;;    `icicle-incremental-completion-delay',
;;    `icicle-incremental-completion-flag',
;;    `icicle-incremental-completion-threshold',
;;    `icicle-inhibit-ding-flag', `icicle-input-string',
;;    `icicle-inter-candidates-min-spaces',
;;    `icicle-isearch-complete-keys',
;;    `icicle-key-descriptions-use-<>-flag',
;;    `icicle-key-descriptions-use-angle-brackets-flag',
;;    `icicle-keymaps-for-key-completion', `icicle-kmacro-ring-max',
;;    `icicle-list-end-string', `icicle-list-join-string',
;;    `icicle-list-nth-parts-join-string',
;;    `icicle-mark-position-in-candidate',
;;    `icicle-minibuffer-setup-hook', `icicle-modal-cycle-down-keys',
;;    `icicle-modal-cycle-up-keys', `icicle-mode', `icicle-mode-hook',
;;    `icicle-option-type-prefix-arg-list',
;;    `icicle-point-position-in-candidate',
;;    `icicle-pp-eval-expression-print-length',
;;    `icicle-pp-eval-expression-print-level',
;;    `icicle-prefix-complete-keys',
;;    `icicle-prefix-complete-no-display-keys',
;;    `icicle-redefine-standard-commands-flag',
;;    `icicle-regexp-quote-flag', `icicle-regexp-search-ring-max',
;;    `icicle-region-alist', `icicle-region-auto-open-files-flag',
;;    `icicle-region-background', `icicle-regions-name-length-max',
;;    `icicle-require-match-flag', `icicle-saved-completion-sets',
;;    `icicle-search-cleanup-flag',
;;    `icicle-search-context-match-predicate',
;;    `icicle-search-highlight-all-current-flag',
;;    `icicle-search-highlight-context-levels-flag',
;;    `icicle-search-highlight-threshold', `icicle-search-hook',
;;    `icicle-search-replace-common-match-flag',
;;    `icicle-search-replace-literally-flag',
;;    `icicle-search-replace-whole-candidate-flag',
;;    `icicle-search-ring-max', `icicle-search-whole-word-flag',
;;    `icicle-show-Completions-help-flag',
;;    `icicle-show-Completions-initially-flag',
;;    `icicle-sort-function', `icicle-sort-functions-alist',
;;    `icicle-special-candidate-regexp',
;;    `icicle-TAB-shows-candidates-flag',
;;    `icicle-test-for-remote-files-flag',
;;    `icicle-thing-at-point-functions',
;;    `icicle-top-level-key-bindings',
;;    `icicle-top-level-when-sole-completion-flag',
;;    `icicle-touche-pas-aux-menus-flag', `icicle-transform-function',
;;    `icicle-unpropertize-completion-result-flag',
;;    `icicle-update-input-hook', `icicle-use-~-for-home-dir-flag',
;;    `icicle-use-C-for-actions-flag',
;;    `icicle-use-candidates-only-once-flag',
;;    `icicle-word-completion-keys',
;;    `icicle-WYSIWYG-Completions-flag', `icicle-yank-function'.
;;
;;  Non-interactive functions in Icicles:
;;
;;    `custom-variable-p', `icicle-2nd-part-string-less-p',
;;    `icicle-abbreviate-or-expand-file-name', `icicle-activate-mark',
;;    `icicle-add-key+cmd', `icicle-all-candidates-action-1',
;;    `icicle-any-candidates-p', `icicle-anything-candidate-value',
;;    `icicle-apply-action', `icicle-apply-list-action',
;;    `icicle-apply-to-saved-candidate',
;;    `icicle-apropos-any-candidates-p',
;;    `icicle-apropos-any-file-name-candidates-p',
;;    `icicle-apropos-candidates', `icicle-apropos-complete-1',
;;    `icicle-barf-if-outside-Completions',
;;    `icicle-barf-if-outside-Completions-and-minibuffer',
;;    `icicle-barf-if-outside-minibuffer', `icicle-binary-option-p',
;;    `icicle-bind-completion-keys', `icicle-bind-isearch-keys',
;;    `icicle-bind-S-TAB-for-map-variable',
;;    `icicle-bind-S-TAB-in-keymaps-from',
;;    `icicle-bind-top-level-commands', `icicle-bookmark-jump-1',
;;    `icicle-buffer-sort-*...*-last',
;;    `icicle-call-then-update-Completions',
;;    `icicle-cancel-Help-redirection', `icicle-candidate-action-1',
;;    `icicle-candidate-set-1',
;;    `icicle-case-insensitive-string-less-p',
;;    `icicle-case-string-less-p', `icicle-cdr-lessp',
;;    `icicle-char-properties-in-buffer',
;;    `icicle-char-properties-in-buffers',
;;    `icicle-choose-anything-candidate',
;;    `icicle-choose-candidate-of-type',
;;    `icicle-choose-completion-string', `icicle-clear-history-1',
;;    `icicle-clear-history-entry', `icicle-clear-minibuffer',
;;    `icicle-color-blue-lessp', `icicle-color-green-lessp',
;;    `icicle-color-help', `icicle-color-hue-lessp',
;;    `icicle-color-red-lessp', `icicle-color-saturation-lessp',
;;    `icicle-color-value-lessp', `icicle-comint-get-final-choice',
;;    `icicle-comint-get-minibuffer-input', `icicle-comint-hook-fn',
;;    `icicle-comint-send-input', `icicle-compilation-hook-fn',
;;    `icicle-command-abbrev-action', `icicle-command-abbrev-command',
;;    `icicle-command-abbrev-matching-commands',
;;    `icicle-command-abbrev-record', `icicle-command-abbrev-regexp',
;;    `icicle-command-abbrev-save',
;;    `icicle-command-abbrev-used-more-p',
;;    `icicle-command-names-alphabetic-p',
;;    `icicle-compilation-search-in-context-fn',
;;    `icicle-complete-again-update', `icicle-complete-keys-1',
;;    `icicle-complete-keys-action', `icicle-completing-p',
;;    `icicle-completing-read', , `icicle-completing-read-multiple',
;;    `icicle-completing-read-history',
;;    `icicle-completion-setup-function',
;;    `icicle-current-completion-in-Completions',
;;    `icicle-current-sort-functions', `icicle-current-sort-order',
;;    `icicle-customize-faces', `icicle-custom-type',
;;    `icicle-dabbrev--abbrev-at-point',
;;    `icicle-define-crm-completion-map', `icicle-define-icicle-maps',
;;    `icicle-delete-candidate-object-1', `icicle-delete-count',
;;    `icicle-delete-current-candidate-object',
;;    `icicle-delete-file-or-directory',
;;    `icicle-delete-region-from-alist',
;;    `icicle-delete-whitespace-from-string',
;;    `icicle-describe-opt-action',
;;    `icicle-describe-opt-of-type-complete', `icicle-ding',
;;    `icicle-dirs-last-p', `icicle-displayable-cand-from-saved-set',
;;    `icicle-display-completion-list', `icicle-display-Completions',
;;    `icicle-display-candidates-in-Completions', `icicle-doc-action',
;;    `icicle-edmacro-parse-keys',
;;    `icicle-ensure-overriding-map-is-bound',
;;    `icicle-execute-extended-command-1',
;;    `icicle-expanded-common-match',
;;    `icicle-expanded-common-match-1', `icicle-expand-file-name',
;;    `icicle-explicit-saved-completion-candidates', `icicle-explore',
;;    `icicle-face-valid-attribute-values', `icicle-file-directory-p',
;;    `icicle-file-name-apropos-candidates',
;;    `icicle-file-name-directory-w-default',
;;    `icicle-file-name-input-p', `icicle-file-name-nondirectory',
;;    `icicle-file-name-prefix-candidates', `icicle-file-readable-p',
;;    `icicle-file-remote-p', `icicle-file-writable-p',
;;    `icicle-filesets-files-under', `icicle-files-within',
;;    `icicle-filter-alist', `icicle-filter-wo-input',
;;    `icicle-find-file-other-window-w-wildcards',
;;    `icicle-find-file-w-wildcards', `icicle-find-tag-action',
;;    `icicle-find-tag-define-candidates',
;;    `icicle-find-tag-define-candidates-1',
;;    `icicle-find-tag-final-act', `icicle-find-tag-help',
;;    `icicle-find-tag-quit-or-error',
;;    `icicle-first-matching-candidate',
;;    `icicle-fit-completions-window', `icicle-fix-default-directory',
;;    `icicle-flat-list', `icicle-font-name-history',
;;    `icicle-fn-doc-minus-sig', `icicle-frame-name-history',
;;    `icicle-frames-on', `icicle-function-name-history',
;;    `icicle-funvardoc-action', `icicle-fuzzy-candidates',
;;    `icicle-get-alist-candidate',
;;    `icicle-get-anything-actions-for-type',
;;    `icicle-get-anything-cached-candidates',
;;    `icicle-get-anything-candidates',
;;    `icicle-get-anything-candidates-of-type',
;;    `icicle-get-anything-default-actions-for-type',
;;    `icicle-get-anything-input-delay',
;;    `icicle-get-anything-req-pat-chars',
;;    `icicle-get-anything-types',
;;    `icicle-get-candidates-from-saved-set', `icicle-goto-marker-1',
;;    `icicle-goto-marker-1-action', `icicle-group-regexp',
;;    `icicle-help-on-candidate-symbol',
;;    `icicle-highlight-candidate-in-Completions',
;;    `icicle-highlight-complete-input',
;;    `icicle-highlight-initial-whitespace',
;;    `icicle-highlight-input-noncompletion',
;;    `icicle-highlight-input-noncompletion-rest',
;;    `icicle-highlight-lighter', `icicle-historical-alphabetic-p',
;;    `icicle-imenu-command-p', `icicle-imenu-in-buffer-p',
;;    `icicle-imenu-non-interactive-function-p',
;;    `icicle-increment-cand-nb+signal-end',
;;    `icicle-increment-color-hue', `icicle-increment-color-value',
;;    `icicle-Info-book-order-p',
;;    `icicle-Info-build-node-completions',
;;    `icicle-Info-build-node-completions-fix-*',
;;    `icicle-Info-goto-node-1', `icicle-Info-goto-node-action',
;;    `icicle-Info-index-action', `icicle-Info-read-node-name',
;;    `icicle-input-from-minibuffer',
;;    `icicle-isearch-complete-past-string',
;;    `icicle-insert-candidates',
;;    `icicle-insert-Completions-help-string',
;;    `icicle-insert-for-yank', `icicle-insert-input',
;;    `icicle-insert-thesaurus-entry-cand-fn', `icicle-insert-thing',
;;    `icicle-lisp-vanilla-completing-read', `icicle-key-description',
;;    `icicle-keys+cmds-w-prefix', `icicle-kill-a-buffer',
;;    `icicle-kill-a-buffer-and-update-completions',
;;    `icicle-kmacro-action', `icicle-last-modified-first-p',
;;    `icicle-make-color-candidate', `icicle-make-face-candidate',
;;    `icicle-make-frame-alist', `icicle-make-window-alist',
;;    `icicle-markers', `icicle-markers-to-readable',
;;    `icicle-marker+text',
;;    `icicle-maybe-multi-completion-completing-p',
;;    `icicle-maybe-sort-and-strip-candidates',`icicle-mctize-all',
;;    `icicle-mctized-display-candidate',
;;    `icicle-mctized-full-candidate', `icicle-minibuf-input',
;;    `icicle-minibuf-input-sans-dir', `icicle-minibuffer-prompt-end',
;;    `icicle-minibuffer-setup', `icicle-most-recent-first-p',
;;    `icicle-mouse-candidate-action-1',
;;    `icicle-msg-maybe-in-minibuffer', `icicle-ms-windows-NET-USE',
;;    `icicle-nb-of-candidate-in-Completions',
;;    `icicle-next-candidate',
;;    `icicle-next-single-char-property-change',
;;    `icicle-non-whitespace-string-p', `icicle-part-1-cdr-lessp',
;;    `icicle-part-1-lessp', `icicle-part-2-lessp',
;;    `icicle-part-3-lessp', `icicle-part-4-lessp',
;;    `icicle-part-N-lessp', `icicle-place-cursor',
;;    `icicle-place-overlay', `icicle-prefix-any-candidates-p',
;;    `icicle-prefix-any-file-name-candidates-p',
;;    `icicle-prefix-candidates', `icicle-prefix-complete-1',
;;    `icicle-prefix-keys-first-p', `icicle-proxy-candidate-first-p',
;;    `icicle-put-at-head', `icicle-put-whole-cand-prop',
;;    `icicle-raise-Completions-frame', `icicle-readable-to-markers',
;;    `icicle-read-char-exclusive', `icicle-read-face-name',
;;    `icicle-read-file-name', `icicle-read-from-minibuffer',
;;    `icicle-read-from-minibuf-nil-default',
;;    `icicle-read-single-key-description', `icicle-read-number',
;;    `icicle-read-string', `icicle-read-string-completing',
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
;;    `icicle-remove-cand-from-lists',
;;    `icicle-remove-candidate-display-others',
;;    `icicle-remove-color-duplicates', `icicle-remove-dots',
;;    `icicle-remove-duplicates', `icicle-remove-if',
;;    `icicle-remove-if-not', `icicle-remove-property',
;;    `icicle-remove-saved-set-action',
;;    `icicle-restore-completion-keys', `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-restore-standard-options',
;;    `icicle-retrieve-candidates-from-set', `icicle-reversible-sort',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook', `icicle-saved-fileset-p',
;;    `icicle-save-or-restore-input', `icicle-scatter',
;;    `icicle-scatter-match', `icicle-scroll-or-update-Completions',
;;    `icicle-search-action', `icicle-search-char-property-scan',
;;    `icicle-search-choose-buffers', `icicle-search-cleanup',
;;    `icicle-search-define-candidates',
;;    `icicle-search-define-candidates-1', `icicle-search-final-act',
;;    `icicle-search-help',
;;    `icicle-search-highlight-all-input-matches',
;;    `icicle-search-highlight-and-maybe-replace',
;;    `icicle-search-highlight-input-matches-here',
;;    `icicle-search-in-context-default-fn',
;;    `icicle-search-property-args', `icicle-search-quit-or-error',
;;    `icicle-search-read-context-regexp', `icicle-search-read-word',
;;    `icicle-search-regexp-scan', `icicle-search-region-action',
;;    `icicle-search-replace-candidate',
;;    `icicle-search-replace-fixed-case-p',
;;    `icicle-search-replace-match', `icicle-search-where-arg',
;;    `icicle-select-minibuffer-contents'
;;    `icicle-select-region-action', `icicle-set-calling-cmd',
;;    `icicle-set-difference', `icicle-set-intersection',
;;    `icicle-set-union', `icicle-signum',
;;    `icicle-S-iso-lefttab-to-S-TAB',
;;    `icicle-start-of-candidates-in-Completions',
;;    `icicle-strip-ignored-files-and-sort',
;;    `icicle-subst-envvar-in-file-name',
;;    `icicle-substring-no-properties', `icicle-successive-action',
;;    `icicle-this-command-keys-prefix', `icicle-top-level-prep',
;;    `icicle-transform-candidates',
;;    `icicle-transform-multi-completion',
;;    `icicle-transform-sole-candidate', `icicle-try-switch-buffer',
;;    `icicle-unbind-isearch-keys',
;;    `icicle-unbind-S-TAB-for-map-variable',
;;    `icicle-unbind-S-TAB-in-keymaps-from',
;;    `icicle-undo-std-completion-faces',
;;    `icicle-unhighlight-lighter', `icicle-unmap',
;;    `icicle-unpropertize', `icicle-unsorted-apropos-candidates',
;;    `icicle-unsorted-file-name-apropos-candidates',
;;    `icicle-unsorted-file-name-prefix-candidates',
;;    `icicle-unsorted-prefix-candidates', `icicle-update-and-next',
;;    `icicle-update-help-string',
;;    `icicle-update-ignored-extensions-regexp',
;;    `icicle-value-satisfies-type-p', `icicle-var-inherits-type-p',
;;    `icicle-var-is-of-type-p', `icicle-var-matches-type-p',
;;    `icicle-var-val-satisfies-type-p',
;;    `old-choose-completion-string', `old-completing-read',
;;    `old-completing-read-multiple', `old-completion-setup-function',
;;    `old-read-file-name'.
;;
;;  Internal variables defined in Icicles:
;;
;;    `icicle-abs-file-candidates', `icicle-all-candidates-action-p',
;;    `icicle-all-candidates-list-action-fn',
;;    `icicle-all-candidates-list-alt-action-fn',
;;    `icicle-apply-nomsg', `icicle-apropos-complete-match-fn',
;;    `icicle-apropos-match-fns-alist', `icicle-bookmark-history',
;;    `icicle-bookmark-menu-map', `icicle-buffer-config-history',
;;    `icicle-candidate-action-fn', `icicle-candidate-alt-action-fn',
;;    `icicle-candidate-entry-fn', `icicle-candidate-help-fn',
;;    `icicle-candidate-nb', `icicle-candidate-properties-alist',
;;    `icicle-candidates-alist', `icicle-char-property-value-history',
;;    `icicle-cmd-calling-for-completion', `icicle-color-history',
;;    `icicle-color-theme-history', `icicle-commands-for-abbrev',
;;    `icicle-common-match-string',
;;    `icicle-comp-base-is-default-dir-p',
;;    `icicle-complete-input-overlay', `icicle-complete-keys-alist',
;;    `icicle-completing-p', `icicle-completion-candidates'
;;    `icicle-completion-help-string',
;;    `icicle-completion-set-history',
;;    `icicle-crm-local-completion-map',
;;    `icicle-crm-local-must-match-map',
;;    `icicle-current-completion-candidate-overlay',
;;    `icicle-current-completion-mode', `icicle-current-input',
;;    `icicle-current-raw-input', `icicle-custom-menu-map',
;;    `icicle-default-directory',
;;    `icicle-default-thing-insertion-flipped-p',
;;    `icicle-delete-candidate-object', `icicle-describe-menu-map',
;;    `icicle-dictionary-history', `icicle-dir-candidate-can-exit-p',
;;    `icicle-edit-menu-map', `icicle-edit-update-p',
;;    `icicle-explore-final-choice',
;;    `icicle-explore-final-choice-full', `icicle-extra-candidates',
;;    `icicle-face-name-history', `icicle-file-menu-map',
;;    `icicle-font-name-history', `icicle-frame-alist',
;;    `icicle-frame-name-history', `icicle-frames-menu-map',
;;    `icicle-function-name-history', `icicle-ignored-extensions',
;;    `icicle-ignored-extensions-regexp',
;;    `icicle-incremental-completion-p', `icicle-info-menu-map',
;;    `icicle-Info-only-rest-of-book-p', `icicle-inhibit-sort-p',
;;    `icicle-inhibit-try-switch-buffer', `icicle-initial-value',
;;    `icicle-input-fail-pos', `icicle-insert-string-at-pt-end',
;;    `icicle-insert-string-at-pt-start', `icicle-kill-history',
;;    `icicle-kmacro-alist', `icicle-kmacro-history',
;;    `icicle-last-completion-candidate',
;;    `icicle-last-completion-command', `icicle-last-input',
;;    `icicle-last-sort-function', `icicle-last-top-level-command',
;;    `icicle-last-transform-function', `icicle-list-use-nth-parts',
;;    `icicle-menu-map', `icicle-minor-mode-map-entry',
;;    `icicle-mode-map', `icicle-ms-windows-drive-hash',
;;    `icicle-must-match-regexp', `icicle-must-not-match-regexp',
;;    `icicle-must-pass-predicate',
;;    `icicle-nb-of-other-cycle-candidates',
;;    `icicle-object-named-types', `icicle-object-predicate-types',
;;    `icicle-old-read-file-name-fn', `icicle-options-menu-map',
;;    `icicle-pre-minibuffer-buffer', `icicle-post-command-hook',
;;    `icicle-pre-command-hook',
;;    `icicle-previous-raw-file-name-inputs',
;;    `icicle-previous-raw-non-file-name-inputs', `icicle-prompt',
;;    `icicle-proxy-candidate-regexp', `icicle-proxy-candidates',
;;    `icicle-read-expression-map', `icicle-re-no-dot',
;;    `icicle-require-match-p', `icicle-reverse-sort-p',
;;    `icicle-saved-candidate-overlays',
;;    `icicle-saved-candidates-variables-obarray',
;;    `icicle-saved-completion-candidate',
;;    `icicle-saved-completion-candidates',
;;    `icicle-saved-completion-candidates-internal',
;;    `icicle-saved-ignored-extensions',
;;    `icicle-saved-kmacro-ring-max', `icicle-saved-proxy-candidates',
;;    `icicle-saved-regexp-search-ring-max',
;;    `icicle-saved-region-background',
;;    `icicle-saved-search-ring-max',
;;    `icicle-scroll-Completions-backward-p', `icicle-search-command',
;;    `icicle-search-context-level', `icicle-search-context-regexp',
;;    `icicle-search-current-overlay', `icicle-search-final-choice',
;;    `icicle-search-history', `icicle-search-in-context-fn',
;;    `icicle-searching-p', `icicle-search-level-overlays',
;;    `icicle-search-menu-map', `icicle-search-tags-menu-map',
;;    `icicle-search-overlays', `icicle-search-refined-overlays',
;;    `icicle-search-replacement',
;;    `icicle-search-replacement-history',
;;    `icicle-successive-grab-count',
;;    `icicle-text-property-value-history',
;;    `icicle-thing-at-pt-fns-pointer',
;;    `icicle-universal-argument-map', `icicle-variable-name-history',
;;    `icicle-whole-candidate-as-text-prop-p',
;;    `lacarte-menu-items-alist', `old-crm-local-completion-map',
;;    `old-crm-local-must-match-map'.
;;
;;  Emacs functions defined in Icicles for older Emacs versions:
;;
;;    `select-frame-set-input-focus'.
;;
;;  Widgets (customization types) defined in Icicles:
;;
;;    `icicle-key-definition'.
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
;;     Depending on `icicle-default-value', select minibuffer
;;     contents.
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
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
