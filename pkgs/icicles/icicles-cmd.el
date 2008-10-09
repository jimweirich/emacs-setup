;;; icicles-cmd.el --- Top-level commands for Icicles
;;
;; Filename: icicles-cmd.el
;; Description: Top-level commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2008, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Sun Oct  5 18:51:13 2008 (-0700)
;;           By: dradams
;;     Update #: 16558
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-cmd.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `color-theme',
;;   `cus-edit', `cus-face', `cus-load', `cus-start', `custom',
;;   `dired', `dired+', `dired-aux', `dired-x', `doremi', `easymenu',
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind', `ffap', `ffap-',
;;   `fit-frame', `frame-cmds', `frame-fns', `hexrgb', `icicles-fn',
;;   `icicles-mac', `icicles-mcmd', `icicles-opt', `icicles-var',
;;   `info', `info+', `kmacro', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `mwheel', `pp', `pp+', `ring', `ring+',
;;   `strings', `subr-21', `thingatpt', `thingatpt+', `w32-browser',
;;   `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  top-level commands (and a few non-interactive functions used in
;;  those commands).  For commands to be used mainly in the minibuffer
;;  or buffer *Completions*, see `icicles-mcmd.el'.  For Icicles
;;  documentation, see `icicles.el'.
;;
;;  If you use the byte-compiled version of this library,
;;  `icicles-cmd.elc', in Emacs 23, then it must be byte-compiled
;;  using Emacs 23.
;;
;;  Commands defined here - (+) means a multi-command:
;;
;;    (+) `a', (+) `any', (+)`buffer', (+)`clear-option', (+)`file',
;;    (+)`icicle-add-buffer-candidate', (+)`icicle-add-buffer-config',
;;    `icicle-add-entry-to-saved-completion-set', `icicle-add-region',
;;    (+)`icicle-anything', (+)`icicle-apply', `icicle-apropos',
;;    `icicle-apropos-command', `icicle-apropos-function',
;;    `icicle-apropos-option', `icicle-apropos-variable',
;;    `icicle-apropos-zippy', `icicle-bbdb-complete-name',
;;    (+)`icicle-bookmark', `icicle-bookmark-jump',
;;    `icicle-bookmark-jump-other-window',
;;    (+)`icicle-bookmark-other-window', (+)`icicle-buffer',
;;    (+)`icicle-buffer-config', (+)`icicle-buffer-list',
;;    (+)`icicle-buffer-other-window', (+)`icicle-clear-history',
;;    (+)`icicle-clear-current-history', (+)`icicle-color-theme',
;;    (+)`icicle-comint-command', (+)`icicle-comint-search',
;;    (+)`icicle-command-abbrev', (+)`icicle-compilation-search',
;;    (+)`icicle-complete-keys', `icicle-complete-thesaurus-entry',
;;    `icicle-customize-apropos', `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options',
;;    `icicle-customize-apropos-options-of-type',
;;    (+)`icicle-customize-face', `icicle-customize-icicles-group',
;;    `icicle-dabbrev-completion', (+)`icicle-delete-file',
;;    (+)`icicle-delete-window', (+)`icicle-delete-windows',
;;    (+)`icicle-describe-option-of-type', (+)`icicle-directory-list',
;;    `icicle-dired-project', `icicle-dired-project-other-window',
;;    `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window',
;;    `icicle-dired-save-marked',
;;    `icicle-dired-save-marked-as-project',
;;    `icicle-dired-save-marked-more',
;;    `icicle-dired-save-marked-persistently',
;;    `icicle-dired-save-marked-to-variable', (+)`icicle-doc',
;;    (+)`icicle-exchange-point-and-mark',
;;    (+)`icicle-execute-extended-command',
;;    (+)`icicle-execute-named-keyboard-macro', (+)`icicle-face-list',
;;    (+)`icicle-file', (+)`icicle-file-list',
;;    (+)`icicle-file-other-window', (+)`icicle-find-file',
;;    (+)`icicle-find-file-absolute',
;;    (+)`icicle-find-file-absolute-other-window',
;;    (+)`icicle-find-file-other-window', (+)`icicle-find-first-tag',
;;    (+)`icicle-find-first-tag-other-window', (+)`icicle-find-tag',
;;    (+)`icicle-font', (+)`icicle-frame-bg', (+)`icicle-frame-fg',
;;    (+)`icicle-fundoc', (+)`icicle-generic-S-tab',
;;    (+)`icicle-goto-global-marker',
;;    (+)`icicle-goto-global-marker-or-pop-global-mark',
;;    (+)`icicle-goto-marker',
;;    (+)`icicle-goto-marker-or-set-mark-command',
;;    `icicle-grep-saved-file-candidates', (+)`icicle-imenu',
;;    (+)`icicle-imenu-command',
;;    (+)`icicle-imenu-non-interactive-function',
;;    (+)`icicle-Info-goto-node', (+)`icicle-Info-goto-node-cmd',
;;    (+)`icicle-Info-index', (+)`icicle-Info-index-20',
;;    (+)`icicle-Info-index-cmd', (+)`icicle-Info-menu',
;;    `icicle-Info-menu-cmd', `icicle-insert-char',
;;    (+)`icicle-insert-kill', (+)`icicle-insert-thesaurus-entry',
;;    (+)`icicle-keyword-list', (+)`icicle-kill-buffer',
;;    (+)`icicle-kmacro', `icicle-lisp-complete-symbol',
;;    (+)`icicle-locate-file', (+)`icicle-locate-file-other-window',
;;    `icicle-non-whitespace-string-p', (+)`icicle-object-action',
;;    (+)`icicle-occur', (+)`icicle-other-window-or-frame',
;;    (+)`icicle-plist', `icicle-pop-tag-mark',
;;    `icicle-pp-eval-expression', `icicle-read-color',
;;    `icicle-read-kbd-macro', (+)`icicle-recent-file',
;;    (+)`icicle-recent-file-other-window', (+)`icicle-regexp-list',
;;    `icicle-region-open-all-files',
;;    (+)`icicle-remove-all-regions-in-buffer',
;;    (+)`icicle-remove-buffer-candidate',
;;    (+)`icicle-remove-buffer-config',
;;    `icicle-remove-entry-from-saved-completion-set',
;;    (+)`icicle-remove-region',
;;    (+)`icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command',
;;    (+)`icicle-reset-option-to-nil',
;;    `icicle-save-string-to-variable', (+)`icicle-search',
;;    (+)`icicle-search-all-regions', (+)`icicle-search-buffer',
;;    (+)`icicle-search-char-property', `icicle-search-dired-marked',
;;    (+)`icicle-search-file', (+)`icicle-search-generic',
;;    `icicle-search-highlight-cleanup', (+)`icicle-search-keywords',
;;    (+)`icicle-search-overlay-property', (+)`icicle-search-region',
;;    (+)`icicle-search-text-property', (+)`icicle-search-word',
;;    (+)`icicle-select-frame', `icicle-select-frame-by-name',
;;    (+)`icicle-select-region', (+)`icicle-select-window',
;;    `icicle-select-window-by-name', `icicle-send-bug-report',
;;    (+)`icicle-set-option-to-t', (+)`icicle-tags-search',
;;    (+)`icicle-toggle-option', (+)`icicle-vardoc',
;;    (+)`icicle-where-is', (+)`icicle-yank-insert', (+)`toggle',
;;    (+)`what-which-how'.
;;
;;  Non-interactive functions defined here:
;;
;;    `custom-variable-p', `icicle-add-key+cmd',
;;    `icicle-anything-candidate-value', `icicle-apply-action',
;;    `icicle-apply-list-action', `icicle-binary-option-p',
;;    `icicle-bookmark-jump-1', `icicle-char-properties-in-buffer',
;;    `icicle-char-properties-in-buffers',
;;    `icicle-choose-anything-candidate',
;;    `icicle-choose-candidate-of-type', `icicle-clear-history-1',
;;    `icicle-clear-history-entry', `icicle-comint-get-final-choice',
;;    `icicle-color-help', `icicle-comint-get-minibuffer-input',
;;    `icicle-comint-hook-fn', `icicle-comint-send-input',
;;    `icicle-command-abbrev-action',
;;    (+)`icicle-command-abbrev-command',
;;    `icicle-command-abbrev-matching-commands',
;;    `icicle-command-abbrev-record', `icicle-command-abbrev-regexp',
;;    `icicle-compilation-hook-fn',
;;    `icicle-compilation-search-in-context-fn',
;;    `icicle-complete-keys-1', `icicle-complete-keys-action',
;;    `icicle-customize-faces', `icicle-dabbrev--abbrev-at-point',
;;    `icicle-delete-file-or-directory',
;;    `icicle-delete-region-from-alist', `icicle-describe-opt-action',
;;    `icicle-describe-opt-of-type-complete', `icicle-doc-action',
;;    `icicle-edmacro-parse-keys',
;;    `icicle-execute-extended-command-1', `icicle-explore',
;;    `icicle-find-file-other-window-w-wildcards',
;;    `icicle-find-file-w-wildcards', `icicle-find-tag-action',
;;    `icicle-find-tag-define-candidates',
;;    `icicle-find-tag-define-candidates-1',
;;    `icicle-find-tag-final-act', `icicle-find-tag-help',
;;    `icicle-find-tag-quit-or-error', `icicle-flat-list',
;;    `icicle-fn-doc-minus-sig', `icicle-funvardoc-action',
;;    `icicle-get-anything-actions-for-type',
;;    `icicle-get-anything-cached-candidates',
;;    `icicle-get-anything-candidates',
;;    `icicle-get-anything-candidates-of-type',
;;    `icicle-get-anything-default-actions-for-type',
;;    `icicle-get-anything-input-delay',
;;    `icicle-get-anything-req-pat-chars',
;;    `icicle-get-anything-types', `icicle-goto-marker-1',
;;    `icicle-goto-marker-1-action', `icicle-group-regexp',
;;    `icicle-imenu-command-p', `icicle-imenu-in-buffer-p',
;;    `icicle-imenu-non-interactive-function-p',
;;    `icicle-Info-book-order-p',
;;    `icicle-Info-build-node-completions',
;;    `icicle-Info-build-node-completions-fix-*',
;;    `icicle-Info-goto-node-1', `icicle-Info-goto-node-action',
;;    `icicle-Info-index-action', `icicle-Info-read-node-name',
;;    `icicle-insert-for-yank',
;;    `icicle-insert-thesaurus-entry-cand-fn',
;;    `icicle-keys+cmds-w-prefix',
;;    `icicle-kill-a-buffer-and-update-completions',
;;    `icicle-kmacro-action',
;;    `icicle-make-color-candidate',`icicle-make-file+date-candidate',
;;    `icicle-make-frame-alist', `icicle-make-window-alist',
;;    `icicle-marker+text', `icicle-markers',
;;    `icicle-next-single-char-property-change',
;;    `icicle-read-single-key-description',
;;    `icicle-read-var-value-satisfying', `icicle-region-add-buffers',
;;    `icicle-region-help', `icicle-region-or-buffer-limits',
;;    `icicle-region-sorted', `icicle-remove-all-regions-action',
;;    `icicle-remove-buffer-candidate-action',
;;    `icicle-remove-buffer-config-action',
;;    `icicle-remove-color-duplicates',
;;    `icicle-remove-saved-set-action', `icicle-search-action',
;;    `icicle-search-char-property-scan',
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
;;    `icicle-select-region-action',
;;    `icicle-search-replace-candidate',
;;    `icicle-search-replace-fixed-case-p',
;;    `icicle-search-replace-match',
;;    `icicle-search-replace-search-hit', `icicle-search-where-arg',
;;    `icicle-this-command-keys-prefix'.
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED HERE:
;;
;;  `dabbrev-completion' - Use Icicles minibuffer completion when there
;;                         are multiple candidates.
;;
;;
;;  ***** NOTE: The following functions defined in `bbdb-com.el' have
;;              been REDEFINED HERE.
;;              (BBDB is available here: http://bbdb.sourceforge.net/.)
;;
;;  `bbdb-complete-name' - Use Icicles minibuffer completion when there
;;                         are multiple candidates.
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' have
;;              been REDEFINED in Icicles:
;;
;;  `lisp-complete-symbol' - Selects *Completions* window even if on
;;                           another frame.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;
;;  ***** NOTE: The following functions defined in `cus-edit.el' have
;;              been REDEFINED HERE:
;;
;;  `customize-apropos', `customize-apropos-faces',
;;  `customize-apropos-groups', `customize-apropos-options' -
;;     Use `completing-read' to read the regexp.
;;  `customize-face', `customize-face-other-window' - Multi-commands.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in `icicles.el'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Icicles multi-commands")
;;  (@> "Other top-level Icicles commands")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; loop, pushnew
                                  ;; plus, for Emacs < 21: dolist, push
                                  ;; plus, for Emacs < 20: when, unless
(eval-when-compile (when (>= emacs-major-version 22) (require 'edmacro))) ;; edmacro-subseq
(eval-when-compile (when (>= emacs-major-version 21) (require 'recentf))) ;; recentf-mode

(eval-when-compile (require 'dabbrev))
  ;; dabbrev-case-fold-search, dabbrev-upcase-means-case-search, dabbrev--last-obarray,
  ;; dabbrev--last-completion-buffer, dabbrev--last-abbreviation, dabbrev--check-other-buffers,
  ;; dabbrev-case-replace, dabbrev--reset-global-variables, dabbrev--minibuffer-origin,
  ;; dabbrev--find-all-expansions, dabbrev--substitute-expansion
(eval-when-compile (require 'bookmark))
  ;; bookmark-all-names, bookmark-buffer-name, bookmark-current-bookmark
(eval-when-compile (require 'comint))
  ;; comint-prompt-regexp, comint-input-ring, comint-mode-map, comint-check-proc,
  ;; comint-copy-old-input, comint-send-input
(eval-when-compile (require 'imenu)) ;; imenu-syntax-alist
(eval-when-compile (require 'compile)) ;; compilation-find-buffer
(eval-when-compile (require 'info)) ;; Info-goto-node
(eval-when-compile (require 'etags))
  ;; file-of-tag, find-tag, find-tag-default, find-tag-history, find-tag-marker-ring,
  ;; find-tag-other-window, goto-tag-location-function, snarf-tag-function,
  ;; tag-find-file-of-tag-noselect, tags-case-fold-search, tags-complete-tag,
  ;; tags-lazy-completion-table, tags-table-files, visit-tags-table-buffer

;; Commented out because `synonyms.el' soft-requires Icicles.
;; (eval-when-compile (require 'synonyms nil t)) ;; (no error if not found):
  ;; synonyms-ensure-synonyms-read-from-cache, synonyms-obarray
(eval-when-compile (require 'misc-cmds nil t)) ;; (no error if not found):
  ;; kill-buffer-and-its-windows
(eval-when-compile (if (> emacs-major-version 21)
                       (require 'palette nil t)
                     (require 'eyedropper nil t))) ;; (no error if not found): 
  ;; eyedrop-background-at-mouse, eyedrop-background-at-point, eyedrop-foreground-at-mouse,
  ;; eyedrop-foreground-at-point, eyedrop-picked-background, eyedrop-picked-foreground
(eval-when-compile (when (> emacs-major-version 21)
                     (require 'anything nil t))) ;; (no error if not found):
  ;; anything-candidate-cache, anything-get-sources, anything-idle-delay, anything-pattern,
  ;; anything-sources, anything-transform-candidates
(eval-when-compile (require 'bbdb nil t) (require 'bbdb-com nil t)) ;; (no error if not found):
  ;; bbdb-auto-fill-function, bbdb-complete-name, bbdb-complete-name-allow-cycling,
  ;; bbdb-complete-name-cleanup, bbdb-complete-name-hooks, bbdb-completion-display-record,
  ;; bbdb-completion-predicate, bbdb-completion-type, bbdb-display-records-1,
  ;; bbdb-dwim-net-address, bbdb-expand-mail-aliases, bbdb-extract-address-components-func,
  ;; bbdb-gag-messages, bbdb-hashtable, bbdb-mapc, bbdb-pop-up-bbdb-buffer, bbdb-record-aka,
  ;; bbdb-record-name, bbdb-record-net, bbdb-search-intertwingle, bbdb-string-trim
(eval-when-compile (require 'yow nil t)) ;; (no error if not found):
  ;; apropos-zippy, yow-after-load-message, yow-file, yow-load-message
(eval-when-compile (require 'cookie1 nil t)) ;; (no error if not found): cookie-cache
(require 'cus-edit)
  ;; customize-apropos, customize-apropos-faces, customize-apropos-groups,
  ;; customize-apropos-options, custom-buffer-create, custom-buffer-order-groups, customize-face,
  ;; customize-face-other-window, custom-sort-items
(require 'misc-fns nil t)   ;; (no error if not found): another-buffer
(require 'apropos-fn+var nil t) ;; (no error if not found):
  ;; apropos-command, apropos-function, apropos-option, apropos-variable
(require 'dired+ nil t) ;; (no error if not found):
                        ;; diredp-find-a-file, diredp-find-a-file-other-window
(require 'frame-cmds nil t) ;; (no error if not found): delete-windows-on (my version)
(when window-system (require 'hexrgb nil t))
                         ;; (no error if not found): hexrgb-read-color, hexrgb-color-name-to-hex
(require 'strings nil t) ;; (no error if not found): read-number (my version)

(eval-when-compile (require 'icicles-mac))
  ;; icicle-define-command, icicle-define-file-command, icicle-define-add-to-alist-command
(require 'icicles-mcmd) ;; icicle-search-define-replacement, icicle-yank
(require 'icicles-var)
  ;; icicle-bookmark-history, icicle-buffer-config-history, icicle-candidate-action-fn,
  ;; icicle-candidate-entry-fn, icicle-candidate-nb, icicle-candidates-alist,
  ;; icicle-char-property-value-history, icicle-color-history, icicle-color-theme-history,
  ;; icicle-complete-keys-alist, icicle-completion-candidates, icicle-completion-set-history,
  ;; icicle-current-input, icicle-current-raw-input, icicle-dictionary-history,
  ;; icicle-extra-candidates, icicle-font-name-history, icicle-frame-name-history,
  ;; icicle-function-name-history, icicle-incremental-completion-p, icicle-kill-history,
  ;; icicle-kmacro-alist, icicle-kmacro-history, icicle-must-match-regexp,
  ;; icicle-must-not-match-regexp, icicle-must-pass-predicate, icicle-re-no-dot,
  ;; icicle-saved-completion-candidates, icicle-search-command, icicle-search-current-overlay,
  ;; icicle-search-final-choice, icicle-search-history, icicle-search-overlays,
  ;; icicle-search-refined-overlays, icicle-variable-name-history
(require 'icicles-opt)
  ;; icicle-add-proxy-candidates-flag, icicle-alternative-sort-function, icicle-buffer-configs,
  ;; icicle-buffer-extras, icicle-buffer-ignore-space-prefix-flag, icicle-buffer-match-regexp,
  ;; icicle-buffer-no-match-regexp, icicle-buffer-predicate, icicle-buffer-require-match-flag,
  ;; icicle-buffer-sort, icicle-color-themes, icicle-complete-keys-self-insert-flag,
  ;; icicle-ignore-space-prefix-flag, icicle-incremental-completion-flag, icicle-input-string,
  ;; icicle-key-descriptions-use-<>-flag, icicle-region-alist, icicle-regions-name-length-max,
  ;; icicle-require-match-flag, icicle-saved-completion-sets, icicle-search-cleanup-flag,
  ;; icicle-search-highlight-all-current-flag, icicle-search-highlight-threshold,
  ;; icicle-search-hook, icicle-show-Completions-initially-flag, icicle-sort-function,
  ;; icicle-transform-function, icicle-update-input-hook
(require 'icicles-fn) ;; icicle-assoc-delete-all, icicle-completing-read-history,
                      ;; icicle-get-alist-candidate, icicle-highlight-lighter, icicle-kill-a-buffer,
                      ;; icicle-read-from-minibuf-nil-default.



;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(when (< emacs-major-version 21)
  (defvar eval-expression-debug-on-error))

(when (< emacs-major-version 22)
  (defvar compilation-current-error)
  (defvar compilation-highlight-overlay)
  (defvar cookie-cache)
  (defvar kmacro-ring)
  (defvar next-error-hook)
  (defvar recentf-list)
  (defvar tags-case-fold-search)
  (defvar yow-after-load-message)
  (defvar yow-file)
  (defvar yow-load-message))

(defvar replace-count)
(defvar icicle-kmacro-alist) ;; Defined in icicles-var.el for Emacs 22
(defvar icicle-track-pt) ;; Defined in icicle-insert-thesaurus-entry
(defvar anything-sources)
(defvar anything-candidate-cache)
(defvar anything-idle-delay)
(defvar find-tag-marker-ring) ;; Defined in etags.el
(defvar goto-tag-location-function) ;; Defined in etags.el
(defvar snarf-tag-function) ;; Defined in etags.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Commands -----------------------------------------------


;;; Redefined standard commands.............................


;; REPLACE ORIGINAL `pp-eval-expression' defined in `pp.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; This is essentially the same as `pp-eval-expression' defined in `pp+.el'.
;;
;; 1. Use no `emacs-lisp-mode-hook'.
;; 2. Read with completion, using `icicle-read-expression-map'.
;; 3. Call font-lock-fontify-buffer.
;; 4. Progress message added.
;; 5. Added optional arg and insertion behavior.
;; 6. Respect `icicle-pp-eval-expression-print-length', `icicle-pp-eval-expression-print-level',
;;    and `eval-expression-debug-on-error'.
;; 7. Adjusted to work in different Emacs releases.
;;
;;;###autoload
(defun icicle-pp-eval-expression (expression ; Bound to `M-:' in Icicle mode.
                                  &optional insert-value)
  "Evaluate an Emacs-Lisp expression and pretty-print its value.
Add the value to the front of the variable `values'.
With a prefix arg, insert the value into the current buffer at point.
With no prefix arg:
 If the value fits on one line (frame width) show it in the echo area.
 Otherwise, show the value in buffer *Pp Eval Output*.

With a negative prefix arg, if the value is a string, then insert it
into the buffer without double-quotes (`\"').

This command respects options
`icicle-pp-eval-expression-print-length',
`icicle-pp-eval-expression-print-level', and
`eval-expression-debug-on-error'.

Emacs-Lisp mode completion and indentation bindings are in effect."
  (interactive
   (list (read-from-minibuffer "Eval: " nil icicle-read-expression-map t 'read-expression-history)
         current-prefix-arg))
  (message "Evaluating...")
  (if (or (not (boundp 'eval-expression-debug-on-error))
          (null eval-expression-debug-on-error))
      (setq values  (cons (eval expression) values))
    (let ((old-value  (make-symbol "t"))
          new-value)
      ;; Bind debug-on-error to something unique so that we can
      ;; detect when evaled code changes it.
      (let ((debug-on-error  old-value))
	(setq values     (cons (eval expression) values)
              new-value  debug-on-error))
      ;; If evaled code has changed the value of debug-on-error,
      ;; propagate that change to the global binding.
      (unless (eq old-value new-value)
	(setq debug-on-error  new-value))))
  (let ((print-length  icicle-pp-eval-expression-print-length)
	(print-level   icicle-pp-eval-expression-print-level))
    (cond (insert-value
           (message "Evaluating...done. Value inserted.")
           (setq insert-value  (prefix-numeric-value insert-value))
           (if (or (not (stringp (car values))) (wholenump insert-value))
               (pp (car values) (current-buffer))
             (princ (car values) (current-buffer))))
          (t
           (let* ((old-show-function  temp-buffer-show-function)
                  ;; Use this function to display the buffer.
                  ;; This function either decides not to display it at all
                  ;; or displays it in the usual way.
                  (temp-buffer-show-function
                   (function
                    (lambda (buf)
                     (save-excursion
                       (set-buffer buf)
                       (goto-char (point-min))
                       (end-of-line 1)
                       (if (or (< (1+ (point)) (point-max))
                               (>= (- (point) (point-min)) (frame-width)))
                           (let ((temp-buffer-show-function  old-show-function)
                                 (old-selected               (selected-window))
                                 (window                     (display-buffer buf)))
                             (goto-char (point-min)) ; expected by some hooks ...
                             (make-frame-visible (window-frame window))
                             (unwind-protect
                                  (progn
                                    (select-window window)
                                    (run-hooks 'temp-buffer-show-hook))
                               (select-window old-selected)
                               (message "Evaluating...done.  See buffer *Pp Eval Output*.")))
                         (message "%s" (buffer-substring (point-min) (point)))))))))
             (with-output-to-temp-buffer "*Pp Eval Output*"
               (pp (car values))
               (with-current-buffer standard-output
                 (setq buffer-read-only  nil)
                 (let ((emacs-lisp-mode-hook    nil)
                       (change-major-mode-hook  nil))
                   (emacs-lisp-mode))
                 (set (make-local-variable 'font-lock-verbose) nil)
                 (font-lock-fontify-buffer))))))))



;; REPLACE ORIGINAL `dabbrev-completion' defined in `dabbrev.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; You can complete from an empty abbrev also.
;; Uses Icicles completion when there are multiple candidates.
;;
(or (fboundp 'old-dabbrev-completion)
(fset 'old-dabbrev-completion (symbol-function 'dabbrev-completion)))

;;;###autoload
(defun icicle-dabbrev-completion (&optional arg) ; Bound to `C-M-/' globally.
  "Completion on current word.
Like \\[dabbrev-expand], but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by
`dabbrev-friend-buffer-function', to find the completions.

If the prefix argument is 16 (which comes from `C-u C-u'), then it
searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."
  (interactive "*P")
  (unless (featurep 'dabbrev)
    (unless (require 'dabbrev nil t) (error "Library `dabbrev' not found"))
    (icicle-mode 1))                    ; Redefine `dabbrev-completion' to Icicles version.
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers  (and arg t))
         (dabbrev-check-all-buffers    (and arg (= (prefix-numeric-value arg) 16)))
         (abbrev                       (icicle-dabbrev--abbrev-at-point))
         (ignore-case-p                (and (if (eq dabbrev-case-fold-search 'case-fold-search)
                                                case-fold-search
                                              dabbrev-case-fold-search)
                                            (or (not dabbrev-upcase-means-case-search)
                                                (string= abbrev (downcase abbrev)))))
         (my-obarray                   dabbrev--last-obarray)
         init)
    ;; If new abbreviation to expand, then expand it.
    (save-excursion
      (unless (and (null arg)
                   my-obarray
                   (or (eq dabbrev--last-completion-buffer (current-buffer))
                       (and (window-minibuffer-p (selected-window))
                            (eq dabbrev--last-completion-buffer
                                (dabbrev--minibuffer-origin))))
                   dabbrev--last-abbreviation
                   (>= (length abbrev) (length dabbrev--last-abbreviation))
                   (string= dabbrev--last-abbreviation
                            (substring abbrev 0 (length dabbrev--last-abbreviation)))
                   (setq init  (try-completion abbrev my-obarray)))
        (setq dabbrev--last-abbreviation  abbrev)
        (let ((completion-list         (dabbrev--find-all-expansions abbrev ignore-case-p))
              (completion-ignore-case  ignore-case-p))
          ;; Make an obarray with all expansions
          (setq my-obarray  (make-vector (length completion-list) 0))
          (unless (> (length my-obarray) 0)
            (error "No dynamic expansion for \"%s\" found%s" abbrev
                   (if dabbrev--check-other-buffers "" " in this-buffer")))
          (dolist (string completion-list)
            (cond ((or (not ignore-case-p) (not dabbrev-case-replace))
                   (intern string my-obarray))
                  ((string= abbrev (upcase abbrev))
                   (intern (upcase string) my-obarray))
                  ((string= (substring abbrev 0 1) (upcase (substring abbrev 0 1)))
                   (intern (capitalize string) my-obarray))
                  (t (intern (downcase string) my-obarray))))
          (setq dabbrev--last-obarray            my-obarray
                dabbrev--last-completion-buffer  (current-buffer)
                ;; Find the expanded common string.
                init                             (try-completion abbrev my-obarray)))))
    ;; Let the user choose between the expansions
    (unless (stringp init) (setq init  abbrev))
    (cond
      ((and (not (string-equal init ""))
            (not (string-equal (downcase init) (downcase abbrev)))
            (<= (length (all-completions init my-obarray)) 1))
       (message "Completed (no other completions)")
       (if (< emacs-major-version 21)
           (dabbrev--substitute-expansion nil abbrev init)
         (dabbrev--substitute-expansion nil abbrev init nil))
       (when (window-minibuffer-p (selected-window)) (message nil)))
;;$$       ;; Complete text only up through the common root. NOT USED.
;;       ((and icicle-dabbrev-stop-at-common-root-p
;;             (not (string-equal init ""))
;;             (not (string-equal (downcase init) (downcase abbrev))))
;;        (message "Use `%s' again to complete further"
;;                 (icicle-key-description (this-command-keys)
;;                                         (not icicle-key-descriptions-use-<>-flag)))
;;        (if (< emacs-major-version 21)
;;            (dabbrev--substitute-expansion nil abbrev init)
;;          (dabbrev--substitute-expansion nil abbrev init nil))
;;        (when (window-minibuffer-p (selected-window)) (message nil))) ; $$ NEEDED?
      (t
       ;; String is a common root already.  Use Icicles completion.
       (icicle-highlight-lighter)
       (message "Making completion list...")
       (search-backward abbrev)
       (replace-match "")
       (condition-case nil
           (let* ((icicle-show-Completions-initially-flag  t)
                  (icicle-incremental-completion-p         'display)
                  (minibuffer-completion-table             my-obarray)
                  (choice
                   (completing-read "Complete: " my-obarray nil nil init nil init)))
             (when choice (insert choice)))
         (quit (insert abbrev)))))))

(defun icicle-dabbrev--abbrev-at-point ()
  "Like `dabbrev--abbrev-at-point', but returns \"\" if there is no match.
Vanilla `dabbrev--abbrev-at-point' raises an error if no match."
  (let ((abv ""))
    (setq dabbrev--last-abbrev-location (point)) ; Record the end of the abbreviation.
    (unless (bobp)
      (save-excursion                   ; Return abbrev at point
        ;; If we aren't right after an abbreviation, move point back to just after one.
        ;; This is so the user can get successive words by typing the punctuation followed by M-/.
        (save-match-data
          (when (and (save-excursion
                       (forward-char -1)
                       (not (looking-at
                             (concat "\\(" (or dabbrev-abbrev-char-regexp "\\sw\\|\\s_") "\\)+"))))
                     (re-search-backward (or dabbrev-abbrev-char-regexp "\\sw\\|\\s_") nil t))
            (forward-char 1)))
        (dabbrev--goto-start-of-abbrev) ; Now find the beginning of that one.
        (setq abv (buffer-substring-no-properties dabbrev--last-abbrev-location (point)))))
    abv))


;; REPLACE ORIGINAL `bbdb-complete-name' defined in `bbdb-com.el',
;; saving it for restoration when you toggle `icicle-mode'.
;; Note: BBDB, the Insidious Big Brother Database, is available here:
;;       http://bbdb.sourceforge.net/.
;;
;; Uses Icicles completion when there are multiple candidates.
;;
(when (fboundp 'bbdb-complete-name)
(or (fboundp 'old-bbdb-complete-name)
(fset 'old-bbdb-complete-name (symbol-function 'bbdb-complete-name))))

;; Free vars here: `bbdb-*' are bound in `bbdb-com.el'.
;;;###autoload
(defun icicle-bbdb-complete-name (&optional start-pos)
  "Complete the user full-name or net-address before point.
Completes only up to the preceding newline, colon, or comma, or the
value of START-POS.

If what has been typed is unique, insert an entry of the form \"User
Name <net-addr>\" (but see `bbdb-dwim-net-address-allow-redundancy').
If it is a valid completion but not unique, you can choose from the
list of completions using Icicles completion.

If your input is completed and `bbdb-complete-name-allow-cycling' is
true, then you can repeat to cycle through the nets for the matching
record.

When called with a prefix arg, display a list of all nets.  Completion
behaviour can be controlled with `bbdb-completion-type'."
  (interactive)
  (unless (and (require 'bbdb nil t) (require 'bbdb-com nil t))
    (error "`icicle-bbdb-complete-name' requires BBDB"))
  (let* ((end                  (point))
         (beg                  (or start-pos (save-excursion (re-search-backward
                                                              "\\(\\`\\|[\n:,]\\)[ \t]*")
                                                             (goto-char (match-end 0)) (point))))
         (orig                 (buffer-substring beg end))
         (typed                (downcase orig))
         (pattern              (bbdb-string-trim typed))
         (ht                   (bbdb-hashtable))
         ;; Make a list of possible completion strings (all-the-completions), and a flag to
         ;; indicate if there's a single matching record or not (only-one-p)
         (only-one-p           t)
         (all-the-completions  nil)
         (pred
          (lambda (sym)
            (when (bbdb-completion-predicate sym)
              (when (and only-one-p all-the-completions
                         (or
                          ;; Not sure about this. more than one record
                          ;; attached to the symbol? Does that happen?
                          (> (length (symbol-value sym)) 1)
                          ;; This is the doozy, though. Multiple syms
                          ;; which all match the same record
                          (delete t (mapcar (lambda(x)
                                              (equal (symbol-value x) (symbol-value sym)))
                                            all-the-completions))))
                (setq only-one-p  nil))
              (if (memq sym all-the-completions)
                  nil
                (setq all-the-completions  (cons sym all-the-completions))))))
         (completion           (progn (all-completions pattern ht pred)
                                      (try-completion pattern ht)))
         (exact-match          (eq completion t)))
    (cond
      ;; No matches found OR you're trying completion on an
      ;; already-completed record. In the latter case, we might have to
      ;; cycle through the nets for that record.
      ((or (null completion)
           (and bbdb-complete-name-allow-cycling
                exact-match             ; Which is a net of the record
                (member orig (bbdb-record-net (car (symbol-value (intern-soft pattern ht)))))))
       (bbdb-complete-name-cleanup)     ; Clean up the completion buffer, if it exists
       ;; Check for cycling
       (unless (catch 'bbdb-cycling-exit
                 ;; Jump straight out if we're not cycling
                 (unless bbdb-complete-name-allow-cycling (throw 'bbdb-cycling-exit nil))
                 ;; Find the record we're working on.
                 (let* ((addr  (funcall bbdb-extract-address-components-func orig))
                        (rec  (and (listp addr)
                                   ;; For now, we ignore the case where this returns more than one
                                   ;; record. Ideally, the last expansion would be stored in a
                                   ;; buffer-local variable, perhaps.
                                   (car (bbdb-search-intertwingle (caar addr) (car (cdar addr)))))))
                   (unless rec (throw 'bbdb-cycling-exit nil))
                   (if current-prefix-arg
                       ;; Use completion buffer
                       (let ((standard-output  (get-buffer-create "*Completions*")))
                         ;; A previously existing buffer has to be cleaned first
                         (save-excursion (set-buffer standard-output)
                                         (setq buffer-read-only  nil)
                                         (erase-buffer))
                         (display-completion-list
                          (mapcar (lambda (n) (bbdb-dwim-net-address rec n))
                                  (bbdb-record-net rec)))
                         (delete-region beg end)
                         (switch-to-buffer standard-output))
                     ;; Use next address
                     (let* ((addrs      (bbdb-record-net rec))
                            (this-addr  (or (cadr (member (car (cdar addr)) addrs))
                                            (nth 0 addrs))))
                       (if (= (length addrs) 1)
                           (throw 'bbdb-cycling-exit t) ; No alternatives. don't signal an error.
                         ;; Replace with new mail address
                         (delete-region beg end)
                         (insert (bbdb-dwim-net-address rec this-addr))
                         (run-hooks 'bbdb-complete-name-hooks)
                         (throw 'bbdb-cycling-exit t))))))
         ;; FALL THROUGH.  Check mail aliases
         (when (and (or (not bbdb-expand-mail-aliases) (not (expand-abbrev)))
                    bbdb-complete-name-hooks)
           (message "No completion for `%s'" pattern) (icicle-ding)))) ; no matches
      ;; Match for a single record. If cycling is enabled then we don't
      ;; care too much about the exact-match part.
      ((and only-one-p (or exact-match bbdb-complete-name-allow-cycling))
       (let* ((sym   (if exact-match (intern-soft pattern ht) (car all-the-completions)))
              (recs  (symbol-value sym))
              the-net match-recs lst primary matched)
         (while recs
           (when (bbdb-record-net (car recs))
             ;; Did we match on name?
             (let ((b-r-name  (or (bbdb-record-name (car recs)) "")))
               (if (string= pattern (substring (downcase b-r-name) 0
                                               (min (length b-r-name) (length pattern))))
                   (setq match-recs  (cons (car recs) match-recs)
                         matched     t)))
             ;; Did we match on aka?
             (unless matched
               (setq lst  (bbdb-record-aka (car recs)))
               (while lst
                 (if (string= pattern (substring (downcase (car lst)) 0
                                                 (min (length (downcase (car lst)))
                                                      (length pattern))))
                     (setq match-recs  (append match-recs (list (car recs)))
                           matched     t
                           lst         ())
                   (setq lst  (cdr lst)))))
             ;; Name didn't match name so check net matching
             (unless matched
               (setq lst      (bbdb-record-net (car recs))
                     primary  t)        ; primary wins over secondary...
               (while lst
                 (if (string= pattern (substring (downcase (car lst)) 0
                                                 (min (length (downcase (car lst)))
                                                      (length pattern))))
                     (setq the-net     (car lst)
                           lst         nil
                           match-recs  (if primary
                                           (cons (car recs) match-recs)
                                         (append match-recs (list (car recs))))))
                 (setq lst      (cdr lst)
                       primary  nil))))
           ;; loop to next rec
           (setq recs     (cdr recs)
                 matched  nil))
         (unless match-recs (error "Only exact matching record has net field"))
         ;; now replace the text with the expansion
         (delete-region beg end)
         (insert (bbdb-dwim-net-address (car match-recs) the-net))
         ;; if we're past fill-column, wrap at the previous comma.
         (when (and (bbdb-auto-fill-function) (>= (current-column) fill-column))
           (let ((p  (point))
                 bol)
             (save-excursion
               (beginning-of-line)
               (setq bol  (point))
               (goto-char p)
               (when (search-backward "," bol t) (forward-char 1) (insert "\n   ")))))
         ;; Update the *BBDB* buffer if desired.
         (when bbdb-completion-display-record
           (let ((bbdb-gag-messages  t))
             (bbdb-pop-up-bbdb-buffer)
             (bbdb-display-records-1 match-recs t)))
         (bbdb-complete-name-cleanup)
         ;; Call the exact-completion hook
         (run-hooks 'bbdb-complete-name-hooks)))
      ;; Partial match
      ;; note, we can't use the trimmed version of the pattern here or
      ;; we'll recurse infinitely on e.g. common first names
      ((and (stringp completion) (not (string= typed completion)))
       (delete-region beg end)
       (insert completion)
       (setq end  (point))
       (let ((last                              "")
             (bbdb-complete-name-allow-cycling  nil))
         (while (and (stringp completion) (not (string= completion last))
                     (setq last        completion
                           pattern     (downcase orig)
                           completion  (progn (all-completions pattern ht pred)
                                              (try-completion pattern ht))))
           (when (stringp completion) (delete-region beg end) (insert completion)))
         (bbdb-complete-name beg)))     ; RECURSE <================
      ;; Exact match, but more than one record
      (t
       (unless (eq (selected-window) (minibuffer-window)) (message "Making completion list..."))
       (let (dwim-completions uniq nets net name akas)
         ;; Collect all the dwim-addresses for each completion, but only once for each record.
         ;; Add if the net is part of the completions.
         (bbdb-mapc (lambda (sym)
                      (bbdb-mapc (lambda (rec)
                                   (unless (member rec uniq)
                                     (setq uniq  (cons rec uniq)
                                           nets  (bbdb-record-net rec)
                                           name  (downcase (or (bbdb-record-name rec) ""))
                                           akas  (mapcar 'downcase (bbdb-record-aka rec)))
                                     (while nets
                                       (setq net  (car nets))
                                       (when (cond
                                               ((and (member bbdb-completion-type ; Primary
                                                             '(primary primary-or-name))
                                                     (member (intern-soft (downcase net) ht)
                                                             all-the-completions))
                                                (setq nets  nil)
                                                t)
                                               ((and name (member bbdb-completion-type ; Name
                                                                  '(nil name primary-or-name))
                                                     (let ((cname  (symbol-name sym)))
                                                       (or (string= cname name)
                                                           (member cname akas))))
                                                (setq name  nil)
                                                t)
                                               ((and (member bbdb-completion-type '(nil net)) ; Net
                                                     (member (intern-soft (downcase net) ht)
                                                             all-the-completions)))
                                               ;; (name-or-)primary
                                               ((and (member bbdb-completion-type
                                                             '(name-or-primary))
                                                     (let ((cname  (symbol-name sym)))
                                                       (or (string= cname name)
                                                           (member cname akas))))
                                                (setq nets  nil)
                                                t))
                                         (setq dwim-completions
                                               (cons (bbdb-dwim-net-address rec net)
                                                     dwim-completions))
                                         (when exact-match (setq nets  nil)))
                                       (setq nets  (cdr nets)))))
                                 (symbol-value sym)))
                    all-the-completions)
         (cond ((and dwim-completions (null (cdr dwim-completions))) ; Insert the unique match.
                (delete-region beg end) (insert (car dwim-completions)) (message ""))
               (t                       ; More than one match.  Use Icicles minibuffer completion.
                (condition-case nil
                    (let* ((icicle-show-Completions-initially-flag      t)
                           (icicle-incremental-completion-p             'display)
                           (icicle-top-level-when-sole-completion-flag  t)
                           (choice (save-excursion
                                     (completing-read "Complete: " (mapcar #'list dwim-completions)
                                                      nil t pattern nil pattern))))
                      (when choice
                        (delete-region beg end)
                        (insert choice)))
                  (error nil))
                (unless (eq (selected-window) (minibuffer-window))
                  (message "Making completion list...done")))))))))


;; REPLACE ORIGINAL `lisp-complete-symbol' defined in `lisp.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Selects *Completions* window even if on another frame.
;;
(or (fboundp 'old-lisp-complete-symbol)
(fset 'old-lisp-complete-symbol (symbol-function 'lisp-complete-symbol)))

;;;###autoload
(defun icicle-lisp-complete-symbol ()   ; Bound to `ESC TAB' globally.
  "Complete the Lisp symbol preceding point against known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

The context determines which symbols are considered.
If the symbol starts just after an open-parenthesis, only symbols
with function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (interactive)
  (let* ((end            (point))
         (buffer-syntax  (syntax-table))
         (beg
          (unwind-protect
               (save-excursion
                 (set-syntax-table emacs-lisp-mode-syntax-table)
                 (backward-sexp 1)
                 (while (= (char-syntax (following-char)) ?\') (forward-char 1))
                 (point))
            (set-syntax-table buffer-syntax)))
         (pattern        (buffer-substring beg end))
         (predicate
          (if (eq (char-after (1- beg)) ?\()
              'fboundp
            (function (lambda (sym) (or (boundp sym) (fboundp sym) (symbol-plist sym))))))
         (enable-recursive-minibuffers  (active-minibuffer-window))
         (icicle-top-level-when-sole-completion-flag  t)
         (completion
          (save-excursion (completing-read "Complete Lisp symbol: " obarray predicate t pattern))))
    (delete-region beg end)
    (insert completion)))


;; REPLACE ORIGINAL `customize-face' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Multi-command version.
;;
(or (fboundp 'old-customize-face)
(fset 'old-customize-face (symbol-function 'customize-face)))

;;;###autoload
(defun icicle-customize-face (face)
  "Customize face FACE.
Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

`C-mouse-2', `C-RET' - Act on current completion candidate only
`C-down'  - Move to next prefix-completion candidate and act
`C-up'    - Move to previous prefix-completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`M-!'     - Act on *all* candidates (or all that are saved):
            Customize all in the same buffer.
`C-!'     - Act on *all* candidates (or all that are saved):
            Customize each in a separate buffer.

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate,
or `C-g' to quit.

With a prefix argument, you can enter multiple faces at the same time
with `RET' (in Emacs 22 or later).  This gives you the completion
behavior of `customize-face' in vanilla Emacs.  The advantage is that
the default value is the list of all faces under the cursor.  The
disadvantage is that face candidates are not WYSIWYG in buffer
*Completions*.

This is an Icicles command - see `icicle-mode'."
  (interactive
   (list (let* ((icicle-list-use-nth-parts  '(1))
                (icicle-candidate-action-fn
                 (lambda (x)
                   (old-customize-face (intern (icicle-transform-multi-completion x)))
                   (select-window (minibuffer-window))
                   (select-frame-set-input-focus (selected-frame))))
                (icicle-all-candidates-list-action-fn 'icicle-customize-faces))
           (if (and (> emacs-major-version 21) current-prefix-arg)
               (read-face-name "Customize face: " "all faces" t)
             (read-face-name "Customize face: ")))))
  (old-customize-face face))


;; REPLACE ORIGINAL `customize-face-other-window' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Multi-command version.
;;
(or (fboundp 'old-customize-face-other-window)
(fset 'old-customize-face-other-window (symbol-function 'customize-face-other-window)))

;;;###autoload
(defun icicle-customize-face-other-window (face)
  "Customize face FACE in another window.
Same as `icicle-customize-face', but uses another window."
  (interactive
   (list (let* ((icicle-list-use-nth-parts  '(1))
                (icicle-candidate-action-fn
                 (lambda (x)
                   (old-customize-face-other-window (intern (icicle-transform-multi-completion x)))
                   (select-window (minibuffer-window))
                   (select-frame-set-input-focus (selected-frame))))
                (icicle-all-candidates-list-action-fn 'icicle-customize-faces))
           (if (and (> emacs-major-version 21) current-prefix-arg)
               (read-face-name "Customize face: " "all faces" t)
             (read-face-name "Customize face: ")))))
  (old-customize-face-other-window face))

(defun icicle-customize-faces (faces)
  "Open Customize buffer on all faces in list FACES."
  (let ((icicle-list-nth-parts-join-string  ": ")
        (icicle-list-join-string            ": ")
        (icicle-list-end-string             "")
        (icicle-list-use-nth-parts          '(1)))
    (custom-buffer-create
     (custom-sort-items
      (mapcar (lambda (f) (list (intern (icicle-transform-multi-completion f)) 'custom-face))
              faces)
      t custom-buffer-order-groups)
     "*Customize Apropos*")))


;; REPLACE ORIGINAL `customize-apropos' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(or (fboundp 'old-customize-apropos)
(fset 'old-customize-apropos (symbol-function 'customize-apropos)))

;;;###autoload
(defun icicle-customize-apropos (regexp &optional all)
  "Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which
  are not user-settable, as well as faces and groups.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg  current-prefix-arg))
     (list (completing-read "Customize (regexp): " obarray
                            (lambda (symbol)
                              (or (get symbol 'custom-group)
                                  (custom-facep symbol)
                                  (and (boundp symbol)
                                       (or (get symbol 'saved-value)
                                           (custom-variable-p symbol)
                                           (if (null pref-arg)
                                               (user-variable-p symbol)
                                             (get symbol 'variable-documentation))))))
                            nil nil 'regexp-history)
           pref-arg)))
  (let ((found  nil))
    (mapatoms (lambda (symbol)
                (when (string-match regexp (symbol-name symbol))
                  (when (and (not (memq all '(faces options))) ; groups or t
                             (get symbol 'custom-group))
                    (push (list symbol 'custom-group) found))
                  (when (and (not (memq all '(options groups))) ; faces or t
                             (custom-facep symbol))
                    (push (list symbol 'custom-face) found))
                  (when (and (not (memq all '(groups faces))) ; options or t
                             (boundp symbol)
                             (or (get symbol 'saved-value)
                                 (custom-variable-p symbol)
                                 (if (memq all '(nil options))
                                     (user-variable-p symbol)
                                   (get symbol 'variable-documentation))))
                    (push (list symbol 'custom-variable) found)))))
    (if (not found)
        (error "No matches")
      (custom-buffer-create (custom-sort-items found t custom-buffer-order-groups)
                            "*Customize Apropos*"))))

;; Define this for Emacs 20 and 21
(unless (fboundp 'custom-variable-p)
  (defun custom-variable-p (variable)
    "Return non-nil if VARIABLE is a custom variable."
    (or (get variable 'standard-value) (get variable 'custom-autoload))))


;; REPLACE ORIGINAL `customize-apropos-faces' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(or (fboundp 'old-customize-apropos-faces)
(fset 'old-customize-apropos-faces (symbol-function 'customize-apropos-faces)))

;;;###autoload
(defun icicle-customize-apropos-faces (regexp)
  "Customize all user faces matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize faces (regexp): " obarray 'custom-facep nil nil
                          'regexp-history)))
  (customize-apropos regexp 'faces))


;; REPLACE ORIGINAL `customize-apropos-groups' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(or (fboundp 'old-customize-apropos-groups)
(fset 'old-customize-apropos-groups (symbol-function 'customize-apropos-groups)))

;;;###autoload
(defun icicle-customize-apropos-groups (regexp)
  "Customize all user groups matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize groups (regexp): " obarray
                          (lambda (symbol) (get symbol 'custom-group)) nil nil 'regexp-history)))
  (customize-apropos regexp 'groups))


;; REPLACE ORIGINAL `customize-apropos-options' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(or (fboundp 'old-customize-apropos-options)
(fset 'old-customize-apropos-options (symbol-function 'customize-apropos-options)))

;;;###autoload
(defun icicle-customize-apropos-options (regexp &optional arg)
  "Customize all user options matching REGEXP.
With prefix arg, include options which are not user-settable.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg  current-prefix-arg))
     (list (completing-read "Customize options (regexp): " obarray
                            (lambda (symbol)
                              (and (boundp symbol)
                                   (or (get symbol 'saved-value)
                                       (custom-variable-p symbol)
                                       (if (null pref-arg)
                                           (user-variable-p symbol)
                                         (get symbol 'variable-documentation)))))
                            nil nil 'regexp-history)
           pref-arg)))
  (customize-apropos regexp (or arg 'options)))


;; REPLACE ORIGINAL `customize-apropos-options-of-type' defined in `cus-edit+.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(or (fboundp 'old-customize-apropos-options-of-type)
    (not (fboundp 'customize-apropos-options-of-type)) ; Doesn't exist.
    (fset 'old-customize-apropos-options (symbol-function 'customize-apropos-options)))

;;;###autoload
(defun icicle-customize-apropos-options-of-type (type regexp)
  "Customize all loaded customizable options of type TYPE that match REGEXP.
With no prefix arg, each option is defined with `defcustom' type TYPE.
With a prefix arg, either each option is defined with `defcustom' type
 TYPE or its current value is compatible with TYPE.

If TYPE is nil (the default value) then all `defcustom' variables are
potential candidates.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see which options will be available in the customize buffer."
  (interactive
   (let ((typ       (car (condition-case err
                             (read-from-string
                              (let ((types ()))
                                (mapatoms
                                 (lambda (cand)
                                   (when (custom-variable-p cand)
                                     (push (list (format "%s"
                                                         (format "%S" (get cand 'custom-type))))
                                           types))))
                                (completing-read "Customize all options of type: "
                                                 (icicle-remove-duplicates types)
                                                 nil nil nil nil "nil")))
                           (end-of-file (error "No such custom type")))))
         (pref-arg  current-prefix-arg))
     (list typ
           (completing-read
            "Customize options matching (regexp): " obarray
            (lambda (symb)
              (and (boundp symb)
                   (or (not (fboundp 'indirect-variable)) (eq (indirect-variable symb) symb))
                   (or (get symb 'saved-value) (custom-variable-p symb))
                   (or (not typ)        ; `typ' = nil means use all types.
                       (if pref-arg
                           (icicle-var-is-of-type-p symb (list typ))
                         (equal (get symb 'custom-type) typ)))))
            nil nil 'regexp-history))))
  (custom-buffer-create (custom-sort-items
                         (mapcar (lambda (s) (list (intern s) 'custom-variable))
                                 icicle-completion-candidates)
                         t "*Customize Apropos*")))


;; REPLACE ORIGINAL `repeat-complex-command' defined in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the command to repeat, letting you use `S-TAB' and
;; `TAB' to see the history list and `C-,' to toggle sorting that display.
;;
(or (fboundp 'old-repeat-complex-command)
(fset 'old-repeat-complex-command (symbol-function 'repeat-complex-command)))

;;;###autoload
(defun icicle-repeat-complex-command (arg) ; Bound to `C-x ESC ESC', `C-x M-:' in Icicle mode.
  "Edit and re-evaluate the last complex command, or ARGth from last.
A complex command is one that used the minibuffer.
ARG is the prefix argument numeric value.

You can edit the past command you choose before executing it.  The
Lisp form of the command is used.  If the command you enter differs
from the previous complex command, then it is added to the front of
the command history.

Icicles completion is available for choosing a past command.  You can
still use the vanilla Emacs bindings `\\<minibuffer-local-map>\\[next-history-element]' and \
`\\[previous-history-element]' to cycle inputs,
and `\\[repeat-matching-complex-command]' to match regexp input, but Icicles input cycling (`up',
`down',`next', `prior') and apropos completion (`S-TAB') are superior
and more convenient."
  (interactive "p")
  (let ((elt  (nth (1- arg) command-history))
        newcmd)
    (if elt
        (progn
          (setq newcmd
                (let ((print-level                   nil)
                      (minibuffer-history-position   arg)
                      (minibuffer-history-sexp-flag  (1+ (minibuffer-depth))))
                  (unwind-protect
                       (let ((icicle-transform-function  'icicle-remove-duplicates))
                         (read (completing-read
                                "Redo: " (mapcar (lambda (entry) (list (prin1-to-string entry)))
                                                 command-history)
                                nil nil (prin1-to-string elt) (cons 'command-history arg)
                                (prin1-to-string elt))))
                    ;; If command was added to command-history as a string, get rid of that.
                    ;; We want only evaluable expressions there.
                    (if (stringp (car command-history))
                        (setq command-history  (cdr command-history))))))
          ;; If command to be redone does not match front of history, add it to the history.
          (or (equal newcmd (car command-history))
              (setq command-history  (cons newcmd command-history)))
          (eval newcmd))
      (if command-history
          (error "Argument %d is beyond length of command history" arg)
        (error "There are no previous complex commands to repeat")))))

;;;###autoload
(defun icicle-add-entry-to-saved-completion-set (set-name entry type)
  "Add ENTRY to saved completion-candidates set SET-NAME.
ENTRY is normally a single candidate (a string).
 With a prefix arg, however, and if option
 `icicle-filesets-as-saved-completion-sets-flag' is non-nil, then
 ENTRY is the name of an  Emacs fileset (Emacs 22 or later).
TYPE is the type of entry to add: `Fileset' or `Candidate'."
  (interactive
   (let ((typ (if (and current-prefix-arg icicle-filesets-as-saved-completion-sets-flag
                       (prog1 (or (require 'filesets nil t)
                                  (error "Feature `filesets' not provided"))
                         (filesets-init))
                       filesets-data)
                  'Fileset
                'Candidate)))
     (list
      (save-selected-window
        (completing-read "Saved completion set: " icicle-saved-completion-sets nil t nil
                         'icicle-completion-set-history))
      (if (eq typ 'Fileset)
          (list ':fileset               ; Just save the fileset name, not the data.
                (car (assoc (completing-read "Fileset to add: " filesets-data nil t)
                            filesets-data)))
        (completing-read "Candidate to add: " (mapcar #'list icicle-saved-completion-candidates)))
      typ)))
  (let ((file-name  (cdr (assoc set-name icicle-saved-completion-sets))))
    (unless (icicle-file-readable-p file-name) (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf  (find-file-noselect file-name 'nowarn 'raw))
          candidates newcands entry-type)
      (unwind-protect
           (condition-case icicle-add-entry-to-saved-completion-set
               (when (listp (setq newcands  (setq candidates  (read list-buf))))
                 (message "Set `%s' read from file `%s'" set-name file-name))
             (error (error "Bad cache file. %s"
                           (error-message-string icicle-add-entry-to-saved-completion-set))))
        (kill-buffer list-buf))
      (unless (consp newcands) (error "Bad data in cache file `%s'" file-name))
      (pushnew entry newcands :test #'equal)
      (setq entry (if (eq type 'Fileset) (caar entry) entry))
      (if (= (length candidates) (length newcands))
          (message "%s `%s' is already in saved set `%s', file `%s'" type entry set-name file-name)
        (with-temp-message (format "Writing entry to cache file `%s'..." file-name)
          (with-temp-file file-name (prin1 newcands (current-buffer))))
        (message "%s `%s' added to saved set `%s', file `%s'" type entry set-name file-name)))))

;;;###autoload
(defun icicle-remove-entry-from-saved-completion-set (set-name)
  "Remove an entry from saved completion-candidates set SET-NAME.
SET-NAME can be an Icicles saved completions set (cache file) or the
name of an Emacs fileset.

The entry to remove can be a single completion candidate (a string) or
an Emacs fileset.  You can thus remove a file name from a fileset or
remove a fileset from an Icicles saved completion set.  (You can also
remove a file name from a saved completion set.)  If a saved set has
both a file and a fileset of the same name, then both are removed.

To use filesets here, use Emacs 22 or later, load library `filesets',
use `(filesets-init)', and ensure that option
`icicle-filesets-as-saved-completion-sets-flag' is non-nil."
  (interactive
   (list (completing-read "Saved completion set: "
                          (if (and icicle-filesets-as-saved-completion-sets-flag
                                   (featurep 'filesets) filesets-data)
                              (append filesets-data icicle-saved-completion-sets)
                            icicle-saved-completion-sets)
                          nil t nil 'icicle-completion-set-history)))
  (let* ((file-name   (cdr (assoc set-name icicle-saved-completion-sets)))
         (candidates  (icicle-get-candidates-from-saved-set set-name 'dont-expand))
         (icicle-whole-candidate-as-text-prop-p  t)
         (entry
          (icicle-get-alist-candidate
           (completing-read
            "Candidate to remove: "
            (mapcar (lambda (e)
                      (cond ((icicle-saved-fileset-p e) ; Swap `:fileset' with fileset name.
                             `(,(cadr e) ,(car e) ,@(cddr e)))
                            ((consp e) e)
                            (t (list e)))) ; Listify naked string.
                    candidates)
            nil t))))
    (when (and (consp entry) (eq (cadr entry) ':fileset)) ; Swap back again: `:fileset' and name.
      (setq entry `(,(cadr entry) ,(car entry) ,@(cddr entry))))
    (when (and (consp entry) (null (cdr entry))) (setq entry  (car entry))) ; Use just the string.
    ;; Delete any such candidate, then remove text properties used for completion.
    (setq candidates  (mapcar #'icicle-unpropertize (delete entry candidates)))
    (cond (file-name
           (with-temp-message           ; Remove from cache file.
               (format "Writing remaining candidates to cache file `%s'..." file-name)
             (with-temp-file file-name (prin1 candidates (current-buffer)))))
          ((icicle-saved-fileset-p (list ':fileset set-name)) ; Remove from fileset.
           (unless (require 'filesets nil t) (error "Feature `filesets' not provided"))
           (filesets-init)
           (let ((fst  (and filesets-data (assoc set-name filesets-data)))) ; The fileset itself.
             (unless fst (error "No such fileset: `%s'" set-name))
             (let ((fst-files  (filesets-entry-get-files fst)))
               (if (car (filesets-member entry fst-files :test 'filesets-files-equalp))
                   (if fst-files        ; Similar to code in `filesets-remove-buffer'.
                       (let ((new-fst  (list (cons ':files (delete entry fst-files)))))
                         (setcdr fst new-fst)
                         (filesets-set-config set-name 'filesets-data filesets-data))
                     (message "Cannot remove `%s' from fileset `%s'" entry set-name))
                 (message "`%s' not in fileset `%s'" entry set-name))))))
    (icicle-msg-maybe-in-minibuffer
     (concat (format "`%s' removed from %s `%s'"
                     (if (icicle-saved-fileset-p entry) (cadr entry) entry)
                     (if (icicle-saved-fileset-p entry) "fileset" "saved set")
                     set-name)
             (and file-name (format ", file `%s'" file-name))))))

;;;###autoload
(icicle-define-command icicle-remove-saved-completion-set ; Command name
  "Remove an entry from `icicle-saved-completion-sets'.
You are prompted to also delete the associated cache file.
You can add entries to `icicle-saved-completion-sets' using command
`icicle-add/update-saved-completion-set'." ; Doc string
  icicle-remove-saved-set-action
  "Remove set of completion candidates named: " ; `completing-read' args
  icicle-saved-completion-sets nil t nil 'icicle-completion-set-history nil nil
  ((icicle-whole-candidate-as-text-prop-p  t) ; Additional bindings
   (icicle-use-candidates-only-once-flag   t))
  nil nil (icicle-remove-Completions-window)) ; First code, undo code, last code

(defun icicle-remove-saved-set-action (set-name)
  "Remove saved set SET-NAME from `icicle-saved-completion-sets'."
  (let ((enable-recursive-minibuffers  t)
        (sets                          icicle-saved-completion-sets)
        set cache)
    (save-selected-window
      (select-window (minibuffer-window))
      (while (setq set    (assoc set-name sets)
                   cache  (cdr set))
        (when (file-exists-p cache)
          (if (y-or-n-p (format "Delete cache file `%s'? " cache))
              (when (condition-case err
                        (progn (delete-file cache) t)
                      (error (progn (message (error-message-string err)) nil)))
                (message "DELETED `%s'" cache) (sit-for 1))
            (message "OK, file NOT deleted") (sit-for 1)))
        (setq sets (delete set sets)))))
  (setq icicle-saved-completion-sets
        (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
  (customize-save-variable 'icicle-saved-completion-sets icicle-saved-completion-sets)
  (message "Candidate set `%s' removed" set-name))

;;;###autoload
(defun icicle-dired-save-marked (&optional arg) ; Bound to `C-M->' in Dired.
  "Save the marked file names in Dired as a set of completion candidates.
Saves file names in variable `icicle-saved-completion-candidates', by
default.
With a plain prefix arg (`C-u'), saves names in a cache file.
With a plain prefix arg (`C-u'), save candidates in a cache file.
With a non-zero numeric prefix arg (`C-u N'), save candidates in a
 variable for which you are prompted.
With a zero prefix arg (`C-0'), save candidates in a fileset (Emacs 22
 or later).  Use this only for file-name candidates, obviously.  To
 subsequently use a fileset for candidate retrieval, option
 `icicle-filesets-as-saved-completion-sets-flag' must be non-nil.

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a Dired buffer."
  (interactive "P")
  (unless (eq major-mode 'dired-mode)
    (error "Command `icicle-dired-save-marked' must be called from a Dired buffer"))
  (icicle-candidate-set-save-1 (dired-get-marked-files) arg))

;;;###autoload
(defun icicle-dired-save-marked-more (&optional arg) ; Bound to `C->' in Dired.
  "Add the marked file names in Dired to the saved candidates set.
Adds candidates to `icicle-saved-completion-candidates', by default.
A prefix argument acts the same as for `icicle-candidate-set-save'.

The existing saved candidates are still saved.  The current candidates
are added to those already saved.

You can retrieve the saved set of candidates with `C-M-<'.
You can use the saved set of candidates for operations such as"
  (interactive "P")
  (unless (eq major-mode 'dired-mode)
    (error "`icicle-dired-save-marked-more' must be called from a Dired buffer"))
  (icicle-candidate-set-save-1 (dired-get-marked-files) arg t))

;;;###autoload
(defun icicle-dired-save-marked-to-variable () ; Bound to `C-M-}' in Dired.
  "Save the marked file names in Dired as a candidate set to a variable.
You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a Dired buffer."
  (interactive)
  (icicle-candidate-set-save-1 (dired-get-marked-files) 99))

(defalias 'icicle-dired-save-marked-as-project ; Bound to `C-}' in Dired.
    'icicle-dired-save-marked-persistently)
;;;###autoload
(defun icicle-dired-save-marked-persistently (filesetp)
  "Save the marked file names in Dired as a persistent set.
With no prefix arg, save in a cache file.
With a prefix arg, save in an Emacs fileset (Emacs 22 or later).

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a Dired buffer."
  (interactive "P")
  (icicle-candidate-set-save-1 (dired-get-marked-files) (if filesetp 0 '(1))))


(put 'icicle-dired-saved-file-candidates 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-dired-saved-file-candidates (prompt-for-dir-p)
  "Open Dired on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
With a prefix argument, you are prompted for the directory.
Otherwise, the default directory is assumed.
Saved names that don't correspond to existing files are ignored.
Existence of files with relative names is checked in the directory."
  (interactive "P")
  ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets? 
  (unless icicle-saved-completion-candidates
    (error (substitute-command-keys "No saved completion candidates.  \
Use \\<minibuffer-local-completion-map>`\\[icicle-candidate-set-save]' to save candidates")))
  (let* ((default-directory  (if prompt-for-dir-p
                                 (read-file-name "Directory: " nil default-directory nil)
                               default-directory))
         (file-names         ()))
    (dolist (f icicle-saved-completion-candidates) (when (file-exists-p f) (push f file-names)))
    (unless file-names (error "No saved candidates are existing files"))
    (dired (cons (generate-new-buffer-name "Icy File Set") (nreverse file-names)))))

;;;###autoload
(defun icicle-dired-saved-file-candidates-other-window (prompt-for-dir-p) ; Bound `C-M-<' in Dired.
  "Open Dired on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
Open in another window.
With a prefix argument, you are prompted for the directory.
Otherwise, the default directory is assumed.
Saved names that don't correspond to existing files are ignored.
Existence of files with relative names is checked in the directory."
  (interactive "P")
  ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets? 
  (unless icicle-saved-completion-candidates
    (error (substitute-command-keys "No saved completion candidates.  \
Use \\<minibuffer-local-completion-map>`\\[icicle-candidate-set-save]' to save candidates")))
  (let* ((default-directory  (if prompt-for-dir-p
                                 (read-file-name "Directory: " nil default-directory nil)
                               default-directory))
         (file-names         ()))
    (dolist (f icicle-saved-completion-candidates) (when (file-exists-p f) (push f file-names)))
    (unless file-names (error "No saved candidates are existing files"))
    (dired-other-window (cons (generate-new-buffer-name "Icy File Set") (nreverse file-names)))))

(put 'icicle-dired-project 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-dired-project (prompt-for-dir-p)
  "Open Dired on a saved project.
A project is either a persistent completion set or an Emacs fileset.
With a prefix argument, you are prompted for the directory.
Otherwise, the default directory is assumed.
Project file names that don't correspond to existing files are ignored.
Existence of files with relative names is checked in the directory."
  (interactive "P")
  ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets? 
  (let ((set-name  (completing-read "Project (saved file names): "
                                    (if (and icicle-filesets-as-saved-completion-sets-flag
                                             (featurep 'filesets) filesets-data)
                                        (append filesets-data icicle-saved-completion-sets)
                                      icicle-saved-completion-sets)
                                    nil nil nil 'icicle-completion-set-history)))
    (icicle-retrieve-candidates-from-set set-name)
    (let* ((default-directory  (if prompt-for-dir-p
                                   (read-file-name "Dired directory: " nil default-directory nil)
                                 default-directory))
           (file-names         ()))
      (dolist (f icicle-saved-completion-candidates) (when (file-exists-p f) (push f file-names)))
      (unless file-names (error "No files in project `%s' actually exist" set-name))
      (dired (cons (generate-new-buffer-name set-name)
                   (nreverse (mapcar (lambda (file)
                                       (if (file-name-absolute-p file)
                                           (expand-file-name file)
                                         file))
                                     file-names)))))))

;;;###autoload
(defun icicle-dired-project-other-window (prompt-for-dir-p) ; Bound to `C-{' in Dired.
  "Open Dired on a saved project in another window.
A project is either a persistent completion set or an Emacs fileset.
With a prefix argument, you are prompted for the directory.
Otherwise, the default directory is assumed.
Project file names that don't correspond to existing files are ignored.
Existence of files with relative names is checked in the directory."
  (interactive "P")
  ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets? 
  (let ((set-name  (completing-read "Project (saved file names): "
                                    (if (and icicle-filesets-as-saved-completion-sets-flag
                                             (featurep 'filesets) filesets-data)
                                        (append filesets-data icicle-saved-completion-sets)
                                      icicle-saved-completion-sets)
                                    nil nil nil 'icicle-completion-set-history)))
    (icicle-retrieve-candidates-from-set set-name)
    (let* ((default-directory  (if prompt-for-dir-p
                                   (read-file-name "Dired directory: " nil default-directory nil)
                                 default-directory))
           (file-names         ()))
      (dolist (f icicle-saved-completion-candidates) (when (file-exists-p f) (push f file-names)))
      (unless file-names (error "No files in project `%s' actually exist" set-name))
      (dired-other-window (cons (generate-new-buffer-name set-name)
                                (nreverse (mapcar (lambda (file)
                                                    (if (file-name-absolute-p file)
                                                        (expand-file-name file)
                                                      file))
                                                  file-names)))))))

;;;###autoload
(defun icicle-grep-saved-file-candidates (command-args)
  "Run `grep' on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
Saved names that do not correspond to existing files are ignored.
Existence of files with relative names is checked in the default
directory."
  (interactive
   (list
    (let ((file-names  ()))
      (unless icicle-saved-completion-candidates
        (error (substitute-command-keys "No saved completion candidates.  \
Use \\<minibuffer-local-completion-map>`\\[icicle-candidate-set-save]' to save candidates")))
      (unless grep-command (grep-compute-defaults))
      (dolist (f icicle-saved-completion-candidates) (when (file-exists-p f) (push f file-names)))
      (let ((default (and (fboundp 'grep-default-command) (grep-default-command))))
        (read-from-minibuffer
         "grep <pattern> <files> :  "
         (let ((up-to-files (concat grep-command "   ")))
           (cons (concat up-to-files (mapconcat #'identity icicle-saved-completion-candidates " "))
                 (- (length up-to-files) 2)))
         nil nil 'grep-history default)))))
  (grep command-args))
 
;;(@* "Icicles multi-commands")
;;; Icicles multi-commands .   .   .   .   .   .   .   .   .

;; Utility function.  Use it to define multi-commands that navigate.
(defun icicle-explore (define-candidates-fn final-action-fn quit-fn error-fn cleanup-fn prompt
                                            &rest compl-read-args)
  "Icicle explorer: explore complex completion candidates.
Navigate among locations or other entities represented by a set of
completion candidates.  See `icicle-search' for a typical example.

Arguments:
 DEFINE-CANDIDATES-FN:
   Function of no args called to fill `icicle-candidates-alist' with
   the candidates.
 FINAL-ACTION-FN:
   Function of no args called after the final choice of candidate
   (after both `icicle-explore-final-choice' and
   `icicle-explore-final-choice-full' have been set).  Typically uses
   `icicle-explore-final-choice-full', the full candidate.
 QUIT-FN: Function of no args called if user uses `C-g'.
 ERROR-FN: Function of no args called if an error is raised.
 CLEANUP-FN: Function of no args called after exploring.
 PROMPT: Prompt string for `completing-read'.
 COMPL-READ-ARGS: `completing-read' args other than PROMPT and TABLE.

Returns:
 The result of executing FINAL-ACTION-FN, if that arg is non-nil.
 Otherwise, `icicle-explore-final-choice-full'."
  (let ((icicle-incremental-completion-flag     'always)
        (icicle-whole-candidate-as-text-prop-p  t)
        (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
        (icicle-act-before-cycle-flag           icicle-act-before-cycle-flag)
        (orig-pt-explore                        (point-marker))
        (orig-win-explore                       (selected-window))
        result)
    (setq icicle-act-before-cycle-flag      nil
          icicle-candidates-alist           nil
          icicle-explore-final-choice       ""
          icicle-explore-final-choice-full  nil)
    (icicle-highlight-lighter)
    (message "Finding candidates...")
    (when define-candidates-fn (funcall define-candidates-fn))
    (unless icicle-candidates-alist (error "No candidates defined"))
    (unwind-protect
         (condition-case failure
             (progn
               (setq icicle-explore-final-choice
                     (apply #'completing-read prompt icicle-candidates-alist compl-read-args))
               (setq icicle-explore-final-choice-full
                     (icicle-get-alist-candidate icicle-explore-final-choice))
               (unless icicle-explore-final-choice-full (error "No such occurrence"))
               (setq result  (if final-action-fn
                                 (funcall final-action-fn)
                               icicle-explore-final-choice-full)))
           (quit (if quit-fn (funcall quit-fn) (keyboard-quit)))
           (error (when error-fn (funcall error-fn))
                  (error "%s" (error-message-string failure))))
      (when cleanup-fn (funcall cleanup-fn)))
    result))

;;;###autoload
(icicle-define-command icicle-execute-extended-command ; Bound to `M-x' in Icicle mode.
  "Read command name, then read its arguments and call it.
This is `execute-extended-command', turned into a multi-command." ; Doc string
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute command%s: "         ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray 'commandp t nil 'extended-command-history nil nil
  (;; Bindings
   (use-file-dialog  nil)               ; `mouse-2' in *Completions* shouldn't use file dialog box.
   (last-command     last-command)      ; Save and restore the last command.
   new-last-cmd)                        ; Set in `icicle-execute-extended-command-1'.
  nil  nil
  (setq this-command  new-last-cmd))    ; Final code: this will update `last-command'.

;; Free vars here: `orig-buff' and `orig-window' are bound by `icicle-define-command'.
;;                 `new-last-cmd' is bound in `icicle-execute-extended-command'.
(defun icicle-execute-extended-command-1 (cmd-name)
  "Action function to execute a command or a named keyboard macro."
  (when (get-buffer orig-buff) (set-buffer orig-buff))
  (when (window-live-p orig-window) (select-window orig-window))
  (when (string= "" cmd-name) (error "No command name"))

  ;; Rebind `icicle-candidate-action-fn' to a function that calls the
  ;; candidate CMD-NAME on a single argument that it reads.  This is
  ;; used only if CMD-NAME is a command that, itself, reads an input
  ;; argument with completion.  When that is the case, you can use
  ;; completion on that input, and if you do that, you can use `C-RET'
  ;; to use command CMD-NAME as a multi-command.  In other words, this
  ;; binding allows for two levels of multi-commands.
  (let* ((cmd    (intern cmd-name))
         (icicle-candidate-action-fn
          (and icicle-candidate-action-fn ; This is nil after the command name is read.
               (lambda (arg)
                 (condition-case nil
                     (funcall cmd arg)  ; Try to use string candidate `arg'.
                   ;; If that didn't work, use a symbol or number candidate.
                   (wrong-type-argument (funcall cmd (car (read-from-string arg))))
                   (wrong-number-of-arguments ; Punt - show help.
                    (funcall #'icicle-help-on-candidate)))
                 (select-window (minibuffer-window))
                 (select-frame-set-input-focus (selected-frame)))))
         (fn     (symbol-function cmd))
         (count  (prefix-numeric-value current-prefix-arg)))
    ;; Message showing what `cmd' is bound to.  This is pretty much a transcription of C code in
    ;; `keyboard.c'.  Not sure it DTRT when there is already a msg in the echo area.
    (when (and suggest-key-bindings (not executing-kbd-macro))
      (let* ((bindings   (where-is-internal cmd overriding-local-map t))
             (curr-msg   (current-message))
             (wait-time  (if curr-msg
                             (or (and (numberp suggest-key-bindings) suggest-key-bindings) 2)
                           0)))
        (when (and bindings (not (and (vectorp bindings) (eq (aref bindings 0) 'mouse-movement))))
          (when (and (sit-for wait-time) (atom unread-command-events))
            (let ((message-log-max  nil))      ; Don't log this message.
              (message "You can run the command `%s' with `%s'"
                       (symbol-name cmd)
                       (key-description bindings)))
            (when (and (sit-for wait-time) curr-msg) (message curr-msg))))))
    (cond ((arrayp fn)
           (let ((this-command  cmd)) (execute-kbd-macro fn count))
           (when (> count 1) (message "(%d times)" count)))
          (t
           (run-hooks 'post-command-hook)
           (run-hooks 'pre-command-hook)
           (let ((enable-recursive-minibuffers  t)
                 ;; Bind, don't set `this-command'.  When you use `C-next', `this-command' needs
                 ;; to be `cmd' during the `C-RET' part, but `last-command' must not be `cmd'
                 ;; during the `next' part.
                 (this-command cmd))
             (call-interactively cmd 'record-it))))
    ;; After `M-x' `last-command' must be the command finally entered with `RET' or, if you end
    ;; with `C-g', the last command entered with `C-RET'.
    (setq new-last-cmd  cmd)))

;; Inspired by Emacs partial completion and by library `exec-abbrev-cmd.el' (Tassilo Horn
;; <tassilo@member.fsf.org>).  The idea of command abbreviation is combined here with normal
;; command invocation, in an Icicles multi-command.
;;;###autoload
(icicle-define-command icicle-command-abbrev ; Bound to `C-x SPC' in Icicle mode.
  "Read command name or its abbreviation, read command args, call command.
Read input, then call `icicle-command-abbrev-action' to act on it.

If `icicle-add-proxy-candidates-flag' is non-nil, then command
abbreviations, as well as commands, are available as completion
candidates.  Otherwise, only commands are available.  You can toggle
this user option using `\\<minibuffer-local-completion-map>\\[icicle-toggle-proxy-candidates]'\
in the minibuffer.

When an abbreviation is available, you can treat it just like a
command.  The rest of this description covers the behavior of choosing
an abbreviation.

If an abbreviation matches a single command name, then that command is
invoked.  If it matches more than one, then you can use completion to
choose one.

Hyphens (`-') in command names divide them into parts.  For example,
`find-file' has two parts: `find' and `file'.  Each character of a
command abbreviation corresponds to one part of each of the commands
that match the abbreviation.  For example, abbreviation `ff' matches
commands `find-file' and `focus-frame', and abbreviation `fg' matches
`find-grep'.

User option `icicle-command-abbrev-match-all-parts-flag' = nil means
that an abbreviation need not match all parts of a command name; it
need match only a prefix.  For example, nil means that abbreviation
`ff' also matches `find-file-other-window' and `fg' also matches
`find-grep-dired'."                     ; Doc string
  icicle-command-abbrev-action          ; Function to perform the action
  "Command or abbrev: " obarray  'commandp ; `completing-read' args
  nil nil 'icicle-command-abbrev-history nil nil
  ((icicle-sort-function     'icicle-command-abbrev-used-more-p) ; Bindings.
   (icicle-proxy-candidates  (let ((ipc  ())
                                   abv)
                               (dolist (entry icicle-command-abbrev-alist ipc)
                                 (setq abv  (symbol-name (cadr entry)))
                                 (unless (member abv ipc) (push abv ipc)))))
   (use-file-dialog          nil)       ; `mouse-2' in *Completions* shouldn't use file dialog box.
   (last-command             last-command)) ; Save and restore the last command.
  nil nil
  (setq icicle-proxy-candidates  nil))

(defun icicle-command-abbrev-action (abbrev-or-cmd)
  "Action function for `icicle-command-abbrev'.
If ABBREV-OR-CMD is a command, call it.
If ABBREV-OR-CMD is an abbreviation for a single command, invoke it.
If ABBREV-OR-CMD is an abbreviation for multiple commands, call
`icicle-command-abbrev-command', to let user choose commands.
If ABBREV-OR-CMD is not an abbreviation or a command, raise an error."
  (setq abbrev-or-cmd  (intern abbrev-or-cmd))
  (let* ((not-cmdp                          (not (commandp abbrev-or-cmd)))
         (regexp                            (and (or not-cmdp icicle-command-abbrev-priority-flag)
                                                 (icicle-command-abbrev-regexp abbrev-or-cmd)))
         (icicle-commands-for-abbrev        (and (or not-cmdp icicle-command-abbrev-priority-flag)
                                                 (icicle-command-abbrev-matching-commands regexp)))
         (icicle-add-proxy-candidates-flag  icicle-add-proxy-candidates-flag)
         (icicle-proxy-candidates           icicle-proxy-candidates))
    (cond ((and not-cmdp (null icicle-commands-for-abbrev))
           (error "No such command or abbreviation `%s'" abbrev-or-cmd))
          (icicle-commands-for-abbrev
           (let* ((icicle-sort-function  'icicle-command-abbrev-used-more-p)
                  (cmd
                   (if (null (cdr icicle-commands-for-abbrev))
                       (prog1 (intern (caar icicle-commands-for-abbrev))
                         (push (caar icicle-commands-for-abbrev) extended-command-history)
                         (call-interactively (intern (caar icicle-commands-for-abbrev)) t))
                     (let ((enable-recursive-minibuffers  t))
                       (icicle-remove-Completions-window)
                       (icicle-command-abbrev-command)))))
             (icicle-command-abbrev-record abbrev-or-cmd cmd)))
          ((not not-cmdp) (call-interactively abbrev-or-cmd)))))

(defun icicle-command-abbrev-regexp (abbrev)
  "Return the command-matching regexp for ABBREV."
  (let ((char-list  (append (symbol-name abbrev) nil))
        (str        "^"))
    (dolist (c char-list) (setq str  (concat str (list c) "[^-]*-")))
    (concat (substring str 0 (1- (length str)))
            (if icicle-command-abbrev-match-all-parts-flag "$" ".*$"))))

(defun icicle-command-abbrev-matching-commands (regexp)
  "Return a completion alist of commands that match REGEXP."
  (mapcar #'list (icicle-remove-if-not
                  (lambda (strg) (string-match regexp strg))
                  (let ((cmds  nil))
                    (mapatoms (lambda (symb) (when (commandp symb) (push (symbol-name symb) cmds))))
                    cmds))))

;;;###autoload
(icicle-define-command icicle-command-abbrev-command
  "Read command name, then read its arguments and call command." ; Doc string
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Command abbreviated%s%s: "   ; `completing-read' arguments
          (cond ((and icicle-current-input (not (string= "" icicle-current-input)))
                 (format " `%s'" icicle-current-input))
                (icicle-candidate-nb
                 (format " `%s'" (elt icicle-completion-candidates icicle-candidate-nb)))
                (t ""))
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  icicle-commands-for-abbrev nil t nil 'extended-command-history nil nil
  (;; Bindings
   (use-file-dialog                   nil) ; `mouse-2' in *Completions* shouldn't use file dialog.
   (last-command                      last-command) ; Save and restore the last command.
   (icicle-add-proxy-candidates-flag  nil) ; No abbrevs - just commands here.
   new-last-cmd)                        ; Set in `icicle-execute-extended-command-1'.
  nil nil
  (setq this-command  new-last-cmd)     ; Final code: this will update `last-command'.
  t)                                    ; Do not make this function interactive.

(defun icicle-command-abbrev-record (abbrev command)
  "Record ABBREV and COMMAND in `icicle-command-abbrev-alist'."
  (let ((entry  (assq command icicle-command-abbrev-alist)))
    (when (and abbrev command)
      (if entry
          (setq icicle-command-abbrev-alist  (cons (list command abbrev (1+ (car (cddr entry))))
                                                   (delete entry icicle-command-abbrev-alist)))
        (push (list command abbrev 1) icicle-command-abbrev-alist)))))

;;;###autoload
(icicle-define-command icicle-execute-named-keyboard-macro ; Bound to `C-x M-e' in Icicle mode.
  "Read the name of a keyboard macro, then execute it."
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute keyboard macro%s: "  ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray (lambda (fn) (and (commandp fn) (arrayp (symbol-function fn))))
  t nil 'icicle-kmacro-history nil nil
  ((last-command  last-command)))       ; Bindings: save and restore the last command.

;;;###autoload
(when (require 'kmacro nil t)
  (icicle-define-command icicle-kmacro  ; Bound to `S-f4' in Icicle mode (Emacs 22+).
    "Execute a keyboard macro according to its position in `kmacro-ring'.
Macros in the keyboard macro ring are given names `1', `2', and so on,
for use as completion candidates.

With prefix argument, repeat macro that many times (but see below).
If a macro is still being defined, end it first, then execute it.

Since this is a multi-command, you can execute any number of macros
any number of times in a single invocation.  In particular, you can
execute a given macro repeatedly using `C-RET' (be sure you use `TAB'
first, to make it the current candidate).

If you use a prefix arg for `icicle-kmacro', then each multi-command
action (e.g. `C-RET') repeats the macro that number of times.  However
if you use a prefix arg for an individual action, then the action
repeats the macro that number of times.  Without its own prefix arg,
an action uses the base prefix arg you used for `icicle-kmacro'."
    icicle-kmacro-action                ; Function to perform the action
    (format "Execute keyboard macro%s: " ; `completing-read' args
            (if current-prefix-arg
                (format " (prefix %s)" (prefix-numeric-value current-prefix-arg))
              ""))
    (let ((count  0))
      (setq icicle-kmacro-alist
            (mapcar (lambda (x) (cons (format "%d" (setq count  (1+ count))) x))
                    (reverse (if nil kmacro-ring (cons (kmacro-ring-head) kmacro-ring))))))
    nil 'no-exit-wo-match nil 'icicle-kmacro-history
    (and (kmacro-ring-head) (null kmacro-ring) "1") nil
    ((pref-arg  current-prefix-arg))    ; Additional bindings
    (progn                              ; First code
      (when defining-kbd-macro (kmacro-end-or-call-macro current-prefix-arg) (error "Done"))
      (unless (or (kmacro-ring-head) kmacro-ring) (error "No keyboard macro defined"))))

  ;; Free vars here: `orig-buff' and `orig-window' are bound by `icicle-define-command'.
  (defun icicle-kmacro-action (cand)
    "Action function for `icicle-kmacro'."
    (when (get-buffer orig-buff) (set-buffer orig-buff))
    (when (window-live-p orig-window) (select-window orig-window))
    (let* ((count  (if current-prefix-arg (prefix-numeric-value current-prefix-arg) pref-arg))
           (macro  (cadr (assoc cand icicle-kmacro-alist))))
      (unless macro (error "No such macro: `%s'" cand))
      (execute-kbd-macro macro count #'kmacro-loop-setup-function)
      (when (> count 1) (message "(%d times)" count)))))

;;;###autoload
(icicle-define-command icicle-set-option-to-t ; Command name
  "Set option to t.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt) (set (intern opt) t) (message "`%s' is now t" opt)) ; Action function
  "Set option to t: " obarray           ; `completing-read' args
  (cond ((and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
         (lambda (x) (and (boundp x) (user-variable-p x) (eq nil (symbol-value x)))))
        (current-prefix-arg (lambda (x) (and (boundp x) (eq nil (symbol-value x)))))
        (t (lambda (x) (and (boundp x) (icicle-binary-option-p x) (eq nil (symbol-value x))))))
  'must-confirm nil
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers          t) ; Additional bindings
   (icicle-use-candidates-only-once-flag  t)))

;;;###autoload
(icicle-define-command icicle-clear-history
  "Clear a minibuffer history of selected entries.
You are prompted for the history to clear, then you are prompted for
the entries to delete from it.  You can use multi-command completion
for both inputs.  That is, you can act on multiple histories and
delete multiple entries from each.

For convenience, you can use `S-delete' the same way as `C-RET': Each
of them deletes the current entry candidate from the history.

With a prefix argument, empty the chosen history completely
\(you are not prompted to choose history entries to delete).

`icicle-act-before-cycle-flag' is bound to t here during completion of
history entries, so `C-next' and so on act on the current candidate."
  icicle-clear-history-1                ; Function to perform the action
  "History to clear: " icicle-clear-history-hist-vars ; `completing-read' args
  nil t nil nil (symbol-name minibuffer-history-variable) nil
  ((pref-arg                        current-prefix-arg) ; Additional bindings
   (enable-recursive-minibuffers    t)
   (icicle-transform-function       'icicle-remove-duplicates)
   (icicle-clear-history-hist-vars  `((,(symbol-name minibuffer-history-variable))
                                      (,(symbol-name 'icicle-previous-raw-file-name-inputs))
                                      (,(symbol-name 'icicle-previous-raw-non-file-name-inputs))))
   (icicle-delete-candidate-object  'icicle-clear-history-entry))
  (mapatoms (lambda (x) (when (and (boundp x) (consp (symbol-value x)) ; First code.
                                   (stringp (car (symbol-value x)))
                                   (string-match "-\\(history\\|ring\\)\\'" (symbol-name x)))
                          (push (list (symbol-name x)) icicle-clear-history-hist-vars)))))

;; Free vars here: `pref-arg' is bound in `icicle-clear-history'.
(defun icicle-clear-history-1 (hist)
  "Action function for `icicle-clear-history' history-variable candidates."
  (setq hist  (intern hist))
  (if (not pref-arg)
      (let* ((icicle-candidate-action-fn              'icicle-clear-history-entry)
             (icicle-transform-function               'icicle-remove-duplicates)
             (icicle-clear-history-hist               hist)
             (icicle-use-candidates-only-once-flag    t)
             (icicle-show-Completions-initially-flag  t)
             (icicle-act-before-cycle-flag            t))
        (when hist                      ; Maybe there are no more, due to deletion actions.
          (funcall icicle-candidate-action-fn
                   (completing-read "Clear input: " (mapcar #'list (symbol-value hist)) nil t))))
    (set hist nil))
  (unless hist (message "History `%s' is now empty" hist))
  nil)

;; Free vars here: `icicle-clear-history-hist' is bound in `icicle-clear-history-1'.
(defun icicle-clear-history-entry (cand)
  "Action function for history entry candidates in `icicle-clear-history'."
  (unless (string= "" cand)
    (set icicle-clear-history-hist
         (icicle-remove-if
          (lambda (x)
            (string= (icicle-substring-no-properties cand) (icicle-substring-no-properties x)))
          (symbol-value icicle-clear-history-hist)))
    ;; We assume here that CAND was in fact present in the history originally.
    (message "`%s' deleted from history `%s'" cand icicle-clear-history-hist))
  nil)

;;;###autoload
(icicle-define-command icicle-clear-current-history ; Bound to `M-i' in minibuffer.
  "Clear current minibuffer history of selected entries.
You are prompted for the history entries to delete.

With a prefix argument, however, empty the history completely
\(you are not prompted to choose history entries to delete).

`icicle-act-before-cycle-flag' is bound to t here during completion of
history entries, so `C-next' and so on act on the current candidate."
  icicle-clear-history-entry            ; Function to perform the action
  "Clear input: " (mapcar #'list (symbol-value icicle-clear-history-hist)) ; `completing-read' args
  nil t nil nil nil nil
  ((pref-arg                                current-prefix-arg) ; Additional bindings
   (enable-recursive-minibuffers            t)
   (icicle-transform-function               'icicle-remove-duplicates)
   (icicle-use-candidates-only-once-flag    t)
   (icicle-show-Completions-initially-flag  t)
   (icicle-clear-history-hist               minibuffer-history-variable))
  (when pref-arg                        ; Use `error' just to exit and make sure message is seen.
    (icicle-ding)
    (if (not (yes-or-no-p (format "Are you sure you want to empty `%s' completely? "
                                  minibuffer-history-variable)))
        (error "")
      (set minibuffer-history-variable nil)
      (error "History `%s' is now empty" minibuffer-history-variable))))

(when (and icicle-define-alias-commands-flag (not (fboundp 'clear-option)))
  (defalias 'clear-option 'icicle-reset-option-to-nil))
;;;###autoload
(icicle-define-command icicle-reset-option-to-nil ; Command name
  "Set option to nil.  This makes sense for binary and list options.
By default, the set of completion candidates is limited to user
options.  Note: it is *not* limited to binary and list options.
With a prefix arg, all variables are candidates." ; Doc string
  (lambda (opt) (set (intern opt) nil) (message "`%s' is now nil" opt)) ; Action function
  "Clear option (set it to nil): " obarray ; `completing-read' args
  (if current-prefix-arg
      (lambda (x) (and (boundp x) (symbol-value x)))
    (lambda (x) (and (boundp x) (user-variable-p x) (symbol-value x))))
  t nil (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history)
  nil nil
  ((enable-recursive-minibuffers          t) ; Additional bindings
   (icicle-use-candidates-only-once-flag  t)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'toggle)))
  (defalias 'toggle 'icicle-toggle-option))
;;;###autoload
(icicle-define-command icicle-toggle-option ; Command name
  "Toggle option's value.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt)                         ; Function to perform the action
    (let ((sym  (intern opt)))
      (set sym (not (eval sym))) (message "`%s' is now %s" opt (eval sym))))
  "Toggle value of option: " obarray    ; `completing-read' args
  (cond ((and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
         'user-variable-p)
        (current-prefix-arg 'boundp)
        (t 'icicle-binary-option-p))
  'must-confirm nil
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers  t)))   ; Additional bindings

(defun icicle-binary-option-p (symbol)
  "Non-nil if SYMBOl is a user option that has custom-type `boolean'."
  (eq (get symbol 'custom-type) 'boolean))

;;;###autoload
(icicle-define-command icicle-bookmark  ; Command name
  "Jump to a bookmark.
You can use `S-delete' on any bookmark during completion to delete it." ; Doc string
  icicle-bookmark-jump                  ; Function to perform the action
  "Bookmark: " (mapcar #'list (bookmark-all-names)) ; `completing-read' args
  nil t nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (or (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark) (bookmark-buffer-name))
  nil
  ((completion-ignore-case          bookmark-completion-ignore-case) ; Additional bindings
   (icicle-delete-candidate-object  'bookmark-delete))
  nil                                   ; First code, undo code, last code
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch)))

;;;###autoload
(icicle-define-command icicle-bookmark-other-window ; Command name
  "Jump to a bookmark in another window..
You can use `S-delete' on any bookmark during completion to delete it." ; Doc string
  icicle-bookmark-jump-other-window     ; Function to perform the action
  "Bookmark: " (mapcar #'list (bookmark-all-names)) ; `completing-read' args
  nil t nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (or (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark) (bookmark-buffer-name))
  nil
  ((completion-ignore-case          bookmark-completion-ignore-case) ; Additional bindings
   (icicle-delete-candidate-object  'bookmark-delete))
  nil                                   ; First code, undo code, last code
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch)))

(defun icicle-bookmark-jump (bookmark)
  "Jump to BOOKMARK.
You probably don't want to use this.  Use `icicle-bookmark' instead."
  (interactive (list (bookmark-completing-read "Jump to bookmark" bookmark-current-bookmark)))
  (icicle-bookmark-jump-1 bookmark))

(defun icicle-bookmark-jump-other-window (bookmark)
  "Jump to BOOKMARK in another window.
You probably don't want to use this.  Use
`icicle-bookmark-other-window' instead."
  (interactive (list (bookmark-completing-read "Jump to bookmark (in another window)"
                                               bookmark-current-bookmark)))
  (icicle-bookmark-jump-1 bookmark 'other-window))

(defun icicle-bookmark-jump-1 (bookmark &optional other-window-p)
  "Helper function for `icicle-bookmark-jump(-other-window)'."
  (unless bookmark (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark)
  (let ((cell  (bookmark-jump-noselect bookmark)))
    (when cell
      (if other-window-p
          (pop-to-buffer (car cell) 'other-window)
        (switch-to-buffer (car cell)))
      (goto-char (cdr cell))
      (progn (run-hooks 'bookmark-after-jump-hook) t)
      ;; If there is an annotation for this bookmark, show it in a buffer.
      (when bookmark-automatically-show-annotations (bookmark-show-annotation bookmark))
      (when (fboundp 'crosshairs-highlight) (crosshairs-highlight))))
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload
(icicle-define-command icicle-find-first-tag ; Command name
  "Find first tag in current tags table whose name matches your input.
This is similar to standard command `find-tag', with these
differences:

* This is a multi-command, so you can visit any number of tags.

* Only the first tag of several identical tags is a candidate, so you
  cannot visit the others.  That is, there is no equivalent to using
  `M-,' (`tags-loop-continue') after `find-tag' to find additional,
  identical tags.

To browse all tags (including duplicates) in all tags tables, use the
more powerful Icicles multi-command `icicle-find-tag'." ; Doc string
  find-tag                              ; Function to perform the action
  "Find tag: "                          ; `completing-read' args
  (if (fboundp 'tags-lazy-completion-table) (tags-lazy-completion-table) 'tags-complete-tag)
  nil nil nil nil (funcall (or find-tag-default-function (get major-mode 'find-tag-default-function)
                               'find-tag-default))
  nil
  ((completion-ignore-case  (if (and (boundp 'tags-case-fold-search) ; Additional bindings
                                     (memq tags-case-fold-search '(t nil)))
                                tags-case-fold-search
                              case-fold-search)))
  (require 'etags))                     ; First code.

;;;###autoload
(icicle-define-command icicle-find-first-tag-other-window ; Command name
  "Find first tag in current tags table whose name matches your input.
This is similar to standard command `find-tag-other-window', with
these differences:

* This is a multi-command, so you can visit any number of tags.

* Only the first tag of several identical tags is a candidate, so you
  cannot visit the others.  That is, there is no equivalent to using
  `M-,' (`tags-loop-continue') after `find-tag-other-window' to find
  additional, identical tags.

To browse all tags (including duplicates) in all tags tables, use the
more powerful Icicles multi-command `icicle-find-tag'." ; Doc string
  find-tag-other-window                 ; Function to perform the action
  "Find tag other window: "             ; `completing-read' args
  (if (fboundp 'tags-lazy-completion-table) (tags-lazy-completion-table) 'tags-complete-tag)
  nil nil nil nil (funcall (or find-tag-default-function (get major-mode 'find-tag-default-function)
                               'find-tag-default))
  nil
  ((completion-ignore-case  (if (and (boundp 'tags-case-fold-search) ; Additional bindings
                                     (memq tags-case-fold-search '(t nil)))
                                tags-case-fold-search
                              case-fold-search)))
  (require 'etags))                     ; First code.

;; Free vars here: `orig-pt-explore' is bound in `icicle-explore'.
;;;###autoload
(defun icicle-find-tag (regexp &optional arg)
  "Navigate among all tags that match REGEXP.
You are prompted for the REGEXP to match.  Enter REGEXP with `RET'.
You can use completion to choose a tag in the current tags table as
REGEXP.  You can use `\\[icicle-pop-tag-mark]' to return to your
starting point.

All matching tags are shown, including duplicate tags from the same or
different source files.  This means that you do not need `M-,' - you
see all tags as candidates to visit.

By default:

* Tags from all tags files are candidates.
* The source file name is shown after a tag, in buffer *Completions*.

A prefix argument changes this default behavior, as follows:

* arg = 0 or arg > 0: only the current tag table is used
* arg = 0 or arg < 0: source file names are not shown"
  (interactive
   (let ((completion-ignore-case  (if (and (boundp 'tags-case-fold-search)
                                           (memq tags-case-fold-search '(t nil)))
                                      tags-case-fold-search
                                    case-fold-search)))
     (require 'etags)
     (list (completing-read "Find tag matching regexp: "
                            (if (fboundp 'tags-lazy-completion-table)
                                (tags-lazy-completion-table) ; Emacs 23+
                              'tags-complete-tag) ; Emacs < 23
                            nil nil nil 'find-tag-history
                            (funcall (or find-tag-default-function
                                         (get major-mode 'find-tag-default-function)
                                         'find-tag-default)))
           current-prefix-arg)))

  (let ((icicle-whole-candidate-as-text-prop-p  t)
        (icicle-sort-function                   nil)
        (icicle-inhibit-sort-p                  t)
        (icicle-candidate-action-fn             'icicle-find-tag-action)
        (icicle-candidate-help-fn               'icicle-find-tag-help)
        (completion-ignore-case                 case-fold-search)
        (orig-pt-find-tag                       (point-marker)))

    (ring-insert find-tag-marker-ring orig-pt-find-tag) ; Record starting point.
    (icicle-explore (lambda () (icicle-find-tag-define-candidates regexp arg))
                    #'icicle-find-tag-final-act #'icicle-find-tag-quit-or-error
                    #'icicle-find-tag-quit-or-error nil
                    "Choose a tag: " nil t nil 'find-tag-history)))

;;;###autoload
(defun icicle-pop-tag-mark ()
  "Like `pop-tag-mark', but uses `pop-to-buffer', not `switch-to-buffer'."
  (interactive)
  (require 'etags)
  (when (ring-empty-p find-tag-marker-ring) (error "No previous locations for find-tag invocation"))
  (let ((marker  (ring-remove find-tag-marker-ring 0)))
    (pop-to-buffer (or (marker-buffer marker) (error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    (set-marker marker nil nil)))

(defun icicle-find-tag-define-candidates (regexp arg)
  "Define candidates for `icicle-find-tag'."
  (save-excursion
    (let ((first-time  t)
          (morep       t))
      (setq icicle-candidates-alist  ())
      (while (and morep (visit-tags-table-buffer (not first-time)))
        (when (and arg (wholenump (prefix-numeric-value arg))) (setq morep  nil))
        (setq first-time               nil
              icicle-candidates-alist  (append icicle-candidates-alist
                                               (nreverse
                                                (icicle-find-tag-define-candidates-1
                                                 regexp (> (prefix-numeric-value arg)
                                                           0)))))))))

(defun icicle-find-tag-define-candidates-1 (regexp show-file-p)
  "Helper function for `icicle-find-tag-define-candidates'.
Returns completion alist of tag information for tags matching REGEXP.
Include file name (label) if SHOW-FILE-P is non-nil.

If SHOW-FILE-P is nil, then alist items look like this:

 (TAG TAG-INFO FILE-PATH GOTO-FUNC)

If SHOW-FILE-P is non-nil, then alist items look like this:

 ((TAG FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC) or

 (FILE-LABEL TAG-INFO FILE-PATH GOTO-FUNC) if no matching TAG.

TAG-INFO is what `snarf-tag-function' (e.g. `etags-snarf-tag')
returns.  It is a cons (TEXT LINE . POSITION).

TEXT is the initial part of a line containing the tag.
LINE is the line number.
POSITION is the (one-based) char position of TEXT within the file.

If TEXT is t, it means the tag refers to exactly LINE or POSITION,
whichever is present, LINE having preference, no searching.
Either LINE or POSITION can be nil.  POSITION is used if present."
  (icicle-highlight-lighter)
  (message "Gathering tags...")
  (goto-char (point-min))
  (let ((temp-list  ()))
    (while (re-search-forward regexp nil t)
      (let* ((goto-func  goto-tag-location-function) ; e.g. `etags-goto-tag-location'.
             ;; TAG-INFO: If no specific tag, (t nil (point-min)). Else, (TEXT LINE . STARTPOS).
             ;; e.g. TEXT = "(defun foo ()" or just "foo" (if explicit),
             ;;      LINE = "148", STARTPOS = "1723"
             (tag-info (save-excursion (funcall snarf-tag-function))) ; e.g. `etags-snarf-tag'.
             (tag (if (eq t (car tag-info)) nil (car tag-info)))
             ;; FILE-PATH is absolute. FILE-LABEL is relative to `default-directory'.
             (file-path (save-excursion
                          (if tag (file-of-tag) (save-excursion (next-line 1) (file-of-tag)))))
             (file-label (expand-file-name file-path (file-truename default-directory))))
        (when (and tag (not (string= "" tag)) (= (aref tag 0) ?\( ))
          (setq tag  (concat tag " ...)")))
        (when (file-readable-p file-path)
          ;; Add item to alist.
          ;;   Item looks like this:    ((TAG FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC)
          ;;   or like this, if no tag: ((FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC)
          (cond (tag
                 (push `(,(if show-file-p
                              (list tag
                                    (progn (put-text-property 0 (length file-label) 'face
                                                              'icicle-candidate-part file-label)
                                           file-label))
                              tag)
                         ,tag-info ,file-path ,goto-func)
                       temp-list))
                (show-file-p            ; No tag.  Use only the FILE-LABEL.
                 (push `((,(progn (put-text-property 0 (length file-label) 'face
                                                     'icicle-candidate-part file-label)
                                  file-label))
                         ,tag-info ,file-path ,goto-func)
                       temp-list))))))
    temp-list))                         ; Return the alist for this TAGS file.

(defun icicle-find-tag-action (ignored-string)
  "Action function for `icicle-find-tag'."
  ;; Ignore (TAG FILE-LABEL) part.  Use only (TAG-INFO FILE-PATH GOTO-FUNC) part.
  (let* ((cand       (cdr (elt (icicle-filter-alist icicle-candidates-alist
                                                    icicle-completion-candidates)
                               icicle-candidate-nb)))
         (tag-info   (nth 0 cand))
         (goto-func  (nth 2 cand)))
    (switch-to-buffer-other-window      ; Go to source file at FILE-PATH.
     (if (fboundp 'tag-find-file-of-tag-noselect)
         (tag-find-file-of-tag-noselect (nth 1 cand))
       (find-file-noselect (nth 1 cand))))
    (widen)
    (condition-case err
        (funcall goto-func tag-info)    ; Go to text at TAG-INFO.
      (error (message (error-message-string err)) nil)))
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame)))

(defun icicle-find-tag-help (cand)
  "Use as `icicle-candidate-help-fn' for `icicle-find-first-tag'."
  (let* ((cand      (cdr (elt (icicle-filter-alist icicle-candidates-alist
                                                   icicle-completion-candidates)
                              icicle-candidate-nb)))
         (tag-info  (nth 0 cand)))
    (message (if (eq t (car tag-info))
                 "No tag - file name itself matches"
               (format "Line: %d, Position: %d, File: %s"
                       (cadr tag-info) (cddr tag-info) (nth 1 cand))))
    (sit-for 4)))

(defun icicle-find-tag-final-act ()
  "Go to the final tag choice."
  (let ((cand  (cdr icicle-explore-final-choice-full)))
    (unless cand (error "No such occurrence: %s" cand))
    (switch-to-buffer-other-window ; Go to source file at FILE-PATH.
     (if (fboundp 'tag-find-file-of-tag-noselect)
         (tag-find-file-of-tag-noselect (nth 1 cand))
       (find-file-noselect (nth 1 cand))))
    (widen)
    (funcall (nth 2 cand) (nth 0 cand)))) ; Go to text at TAG-INFO.
  
(defun icicle-find-tag-quit-or-error ()
  "Pop back to the last tag visited."
  (icicle-pop-tag-mark)
  (raise-frame))

;;;###autoload
(defun icicle-other-window-or-frame (arg) ; Bound to `C-x o' in Icicle mode.
  "Select a window or frame, by name or by order.
This command combines Emacs commands `other-window' and `other-frame',
together with Icicles multi-commands `icicle-select-window', and
`icicle-select-frame'.  Use the prefix argument to choose, as follows:

 With no prefix arg or a non-zero numeric prefix arg:
  If the selected frame has multiple windows, then this is
  `other-window'.  Otherwise, it is `other-frame'.

 With a zero prefix arg (e.g. `C-0'):
  If the selected frame has multiple windows, then this is
  `icicle-select-window' with windows in the frame as candidates.
  Otherwise (single-window frame), this is `icicle-select-frame'.

 With plain `C-u':
  If the selected frame has multiple windows, then this is
  `icicle-select-window' with windows from all visible frames as
  candidates.  Otherwise, this is `icicle-select-frame'."
  (interactive "P")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((consp arg)
           (if (one-window-p) (icicle-select-frame) (icicle-select-window)))
          ((zerop numarg)
           (if (one-window-p)
               (icicle-select-frame)
             (let ((current-prefix-arg  nil)) (icicle-select-window))))
          (t
           (if (one-window-p) (other-frame numarg) (other-window numarg))))))

;;;###autoload
(icicle-define-command icicle-select-frame ; Bound to `C-x 5 o' in Icicle mode.
  "Select frame by its name and raise it.
A frame name in this context is suffixed as needed by [NUMBER], to
make it unique.  For example, in a context where frames are named for
their buffers and you have two frames showing buffer *Help*, one of
the frames will be called `*Help*[2]' for use with this command." ; Doc string
  icicle-select-frame-by-name           ; Function to perform the action
  "Select frame: "                      ; `completing-read' args
  icicle-frame-alist nil t nil
  (if (boundp 'frame-name-history) 'frame-name-history 'icicle-frame-name-history)
  (cdr (assq 'name (frame-parameters (next-frame (selected-frame))))) nil
  ((icicle-frame-alist  (icicle-make-frame-alist)))) ; Additional bindings

;;;###autoload
(defun icicle-select-frame-by-name (name &optional frame-alist)
  "Select the frame named NAME, and raise it.
Optional argument FRAME-ALIST is an alist of frames to choose from.
Each element has the form (FNAME . FRAME), where FNAME names FRAME.
See `icicle-make-frame-alist' for more about FNAME."
  (interactive (let* ((alist    (icicle-make-frame-alist))
                      (default  (car (rassoc (selected-frame) alist)))
                      (input    (completing-read "Select Frame: " alist nil t nil
                                                 'frame-name-history default)))
                 (list (if (= (length input) 0) default input)
                       alist)))
  (unless frame-alist (setq frame-alist  (or (and (boundp 'icicle-frame-alist) icicle-frame-alist)
                                             (icicle-make-frame-alist))))
  (let ((frame  (cdr (assoc name frame-alist))))
    (unless frame (error "No such frame: `%s'" name))
    (make-frame-visible frame)
    (select-frame-set-input-focus frame)))

(defun icicle-make-frame-alist ()
  "Return an alist of entries (FNAME . FRAME), where FNAME names FRAME.
Frame parameter `name' is used as FNAME, unless there is more than one
frame with the same name.  In that case, FNAME includes a suffix
\[NUMBER], to make it a unique name.  The NUMBER order among frame
names that differ only by their [NUMBER] is arbitrary."
  (let ((fr-alist  ())
        (count     2)
        fname new-name)
    (dolist (fr (frame-list))
      (setq fname  (frame-parameter fr 'name))
      (if (not (assoc fname fr-alist))
          (push (cons fname fr) fr-alist)
        (setq new-name  fname)
        (while (assoc new-name fr-alist)
          (setq new-name  (format "%s[%d]" fname count)
                count     (1+ count)))
        (push (cons new-name fr) fr-alist))
      (setq count  2))
    fr-alist))

;; Free vars here: `icicle-window-alist' is bound in Bindings form.
;;;###autoload
(icicle-define-command icicle-select-window ; Command name
  "Select window by its name.
With no prefix arg, candidate windows are those of the selected frame.
With a prefix arg, windows of all visible frames are candidates.

A window name is the name of its displayed buffer, but suffixed as
needed by [NUMBER], to make the name unique.  For example, if you have
two windows showing buffer *Help*, one of the windows will be called
`*Help*[2]' for use with this command." ; Doc string
  icicle-select-window-by-name          ; Function to perform the action
  "Select window: "                     ; `completing-read' args
  icicle-window-alist nil t nil nil
  (buffer-name (window-buffer (other-window 1))) nil
  ((icicle-window-alist  (icicle-make-window-alist current-prefix-arg)))) ; Additional bindings

;; Free vars here: `icicle-window-alist' is bound in `icicle-select-window'.
(defun icicle-select-window-by-name (name &optional window-alist)
  "Select the window named NAME.
Optional argument WINDOW-ALIST is an alist of windows to choose from.

Interactively:
 A prefix arg means windows from all visible frames are candidates.
 No prefix arg means windows from the selected frame are candidates.

Each alist element has the form (WNAME . WINDOW), where WNAME names
WINDOW.  See `icicle-make-window-alist' for more about WNAME."
  (interactive (let* ((alist    (icicle-make-window-alist current-prefix-arg))
                      (default  (car (rassoc (selected-window) alist)))
                      (input    (completing-read "Select Window: " alist nil t nil nil default)))
                 (list (if (= (length input) 0) default input) alist)))
  (unless window-alist
    (setq window-alist  (or (and (boundp 'icicle-window-alist) icicle-window-alist)
                            (icicle-make-window-alist))))
  (let ((window  (cdr (assoc name window-alist))))
    (unless window (error "No such window: `%s'" name))
    (select-window window)
    (when (fboundp 'crosshairs-highlight) (crosshairs-highlight))
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-make-window-alist (&optional all-p)
  "Return an alist of entries (WNAME . WINDOW), where WNAME names WINDOW.
The name of the buffer in a window is used as its name, unless there
is more than one window displaying the same buffer.  In that case,
WNAME includes a suffix [NUMBER], to make it a unique name.  The
NUMBER order among window names that differ only by their [NUMBER] is
arbitrary.

Non-nil argument ALL-P means use windows from all visible frames.
Otherwise, use only windows from the selected frame."
  (let ((win-alist  ())
        (count      2)
        wname new-name)
    (walk-windows (lambda (w)
                    (setq wname  (buffer-name (window-buffer w)))
                    (if (not (assoc wname win-alist))
                        (push (cons wname w) win-alist)
                      (setq new-name  wname)
                      (while (assoc new-name win-alist)
                        (setq new-name  (format "%s[%d]" wname count)
                              count     (1+ count)))
                      (push (cons new-name w) win-alist))
                    (setq count  2))
                  'no-mini
                  (if all-p 'visible 'this-frame))
    win-alist))

;;;###autoload
(icicle-define-command icicle-delete-windows ; Command name
  "Delete windows showing a buffer, anywhere." ; Doc string
  delete-windows-on                     ; Function to perform the action
  "Delete windows on buffer: "          ; `completing-read' args
  (let ((cand-bufs  nil))
    (dolist (buf (buffer-list))
      (when (get-buffer-window buf 0) (push (list (buffer-name buf)) cand-bufs)))
    cand-bufs)
  nil t nil 'buffer-name-history (buffer-name (current-buffer)) nil
  ((icicle-use-candidates-only-once-flag  t) ; Additional bindings
   (icicle-inhibit-try-switch-buffer      t)))

;;;###autoload
(defun icicle-delete-window (bufferp)   ; Bound to `C-x 0' in Icicle mode.
  "`delete-window' or prompt for buffer and delete all its windows.
When called from the minibuffer, remove the *Completions* window.

Otherwise:
 With no prefix arg, delete the selected window.
 With a prefix arg, prompt for a buffer and delete all windows, on
   any frame, that show that buffer.

 With a prefix arg, this is an Icicles multi-command - see
 `icicle-mode'.  Input-candidate completion and cycling are
 available.  While cycling, these keys with prefix `C-' are active:

 `C-RET'   - Act on current completion candidate only
 `C-down'  - Move to next prefix-completion candidate and act
 `C-up'    - Move to previous prefix-completion candidate and act
 `C-next'  - Move to next apropos-completion candidate and act
 `C-prior' - Move to previous apropos-completion candidate and act
 `C-!'     - Act on *all* candidates (or all that are saved),
             successively (careful!)

 With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
 `C-M-RET', `C-M-down', and so on) provide help about candidates.

 Use `mouse-2', `RET', or `S-RET' to finally choose a candidate,
 or `C-g' to quit."
  (interactive "P")
  (if (window-minibuffer-p (selected-window))
      (icicle-remove-Completions-window)
    (if bufferp (icicle-delete-windows) (delete-window))))

;;;###autoload
(icicle-define-command icicle-kill-buffer ; Bound to `C-x k' in Icicle mode.
  "Kill a buffer."                      ; Doc string
  icicle-kill-a-buffer-and-update-completions ; Function to perform the action
  "Kill buffer: "                       ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list)) nil t nil 'buffer-name-history
  (buffer-name (current-buffer)) nil
  ((icicle-must-match-regexp         icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp     icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate       icicle-buffer-predicate)
   (icicle-extra-candidates          icicle-buffer-extras)
   (icicle-sort-function             icicle-buffer-sort)
   (icicle-require-match-flag        icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag  icicle-buffer-ignore-space-prefix-flag)))

(defun icicle-kill-a-buffer-and-update-completions (buf)
  "Kill buffer BUF and update the set of completions."
  (setq buf  (get-buffer buf))
  (if buf
      (condition-case err
          (if (not (buffer-live-p buf))
              (message "Buffer already deleted: `%s'" buf)
            (if (fboundp 'kill-buffer-and-its-windows)
                (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
              (kill-buffer buf))
            ;; Update the set of completions, then update *Completions*.
            (setq minibuffer-completion-table  (mapcar (lambda (buf) (list (buffer-name buf)))
                                                       (buffer-list)))
            (icicle-complete-again-update))
        (error nil))
    (message "No such live buffer: `%s'" buf)))

(put 'icicle-buffer 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-buffer    ; Bound to `C-x b' in Icicle mode.
  "Switch to a different buffer.
You can use `S-delete' during completion to kill a candidate buffer.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'." ; Doc string
  switch-to-buffer                      ; Function to perform the action
  "Switch to buffer: "                  ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer) ; In `misc-fns.el'.
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-must-match-regexp         icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp     icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate       icicle-buffer-predicate)
   (icicle-extra-candidates          icicle-buffer-extras)
   (icicle-sort-function             (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object   'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag        icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag  icicle-buffer-ignore-space-prefix-flag)))

;;;###autoload
(icicle-define-command icicle-buffer-other-window ; Bound to `C-x 4 b' in Icicle mode.
  "Switch to a different buffer in another window.
You can use `S-delete' during completion to kill a candidate buffer.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'" ; Doc string
  switch-to-buffer-other-window         ; Function to perform the action
  "Switch to buffer: "                  ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer)
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-must-match-regexp         icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp     icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate       icicle-buffer-predicate)
   (icicle-extra-candidates          icicle-buffer-extras)
   (icicle-sort-function             (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object   'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag        icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag  icicle-buffer-ignore-space-prefix-flag)))

;;;###autoload
(icicle-define-command icicle-add-buffer-candidate ; Command name
  "Add buffer as an always-show completion candidate.
This just adds the buffer to `icicle-buffer-extras'.
You can use `S-delete' on any completion candidate to remove it from
`icicle-buffer-extras'."                ; Doc string
  (lambda (buf)
    (add-to-list 'icicle-buffer-extras buf) ; Action function
    (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
    (message "Buffer `%s' added to always-show buffers" buf))
  "Buffer candidate to show always: "   ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer)
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-delete-candidate-object        'icicle-remove-buffer-candidate-action) ; Bindings
   (icicle-use-candidates-only-once-flag  t)))

;;;###autoload
(icicle-define-command icicle-remove-buffer-candidate ; Command name
  "Remove buffer as an always-show completion candidate.
This just removes the buffer from `icicle-buffer-extras'." ; Doc string
  icicle-remove-buffer-candidate-action ; Action function
  "Remove buffer from always-show list: " ; `completing-read' args
  (mapcar #'list icicle-buffer-extras) nil t nil 'buffer-name-history (car icicle-buffer-extras) nil
  ((icicle-use-candidates-only-once-flag  t))) ;  ; Additional bindings

(defun icicle-remove-buffer-candidate-action (buf)
  "Action function for command `icicle-remove-buffer-candidate'."
  (setq icicle-buffer-extras  (delete buf icicle-buffer-extras))
  (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
  (message "Buffer `%s' removed from always-show buffers" buf))

;;;###autoload
(icicle-define-command icicle-buffer-config ; Command name
  "Choose a configuration of user options for `icicle-buffer'.
You can use `S-delete' on any configuration during completion to
remove it.  See user option `icicle-buffer-configs'.
See also commands `icicle-add-buffer-config' and
`icicle-remove-buffer-config'."         ; Doc string
  (lambda (config-name)                 ; Function to perform the action
    (let ((config  (assoc config-name icicle-buffer-configs)))
      (setq icicle-buffer-match-regexp     (elt config 1)
            icicle-buffer-no-match-regexp  (elt config 2)
            icicle-buffer-predicate        (elt config 3)
            icicle-buffer-extras           (elt config 4)
            icicle-buffer-sort             (elt config 5))))
  "Configuration: " icicle-buffer-configs nil t nil ; `completing-read' args
  'icicle-buffer-config-history nil nil
  ((icicle-delete-candidate-object  'icicle-remove-buffer-config-action))) ; Additional bindings

;;;###autoload
(icicle-define-add-to-alist-command icicle-add-buffer-config ; Command name
  "Add buffer configuration to `icicle-buffer-configs'.
You are prompted for the buffer configuration components.
For the list of extra buffers to always display, you can choose them
using `C-mouse-2', `C-RET', and so on, just as you would make any
Icicles multiple choice."
  (lambda ()
    (let ((name            (read-from-minibuffer "Add buffer configuration.  Name: "))
          (match-regexp    (icicle-read-from-minibuf-nil-default
                            "Regexp to match: " nil nil nil 'regexp-history
                            icicle-buffer-match-regexp))
          (nomatch-regexp  (icicle-read-from-minibuf-nil-default
                            "Regexp not to match: " nil nil nil 'regexp-history
                            icicle-buffer-no-match-regexp))
          (pred            (icicle-read-from-minibuf-nil-default
                            "Predicate to satify: " nil nil nil
                            (if (boundp 'function-name-history)
                                'function-name-history
                              'icicle-function-name-history)
                            icicle-buffer-predicate))
          (sort-fn         (icicle-read-from-minibuf-nil-default
                            "Sort function: " nil nil t
                            (if (boundp 'function-name-history)
                                'function-name-history
                              'icicle-function-name-history)
                            (and icicle-buffer-sort (symbol-name icicle-buffer-sort))))
          (extras          (progn (message "Choose extra buffers to show...") (sit-for 1)
                                  (icicle-buffer-list)))) ; Do last, for convenience.
      (list name match-regexp nomatch-regexp pred extras sort-fn)))
  icicle-buffer-configs)

;;;###autoload
(icicle-define-command icicle-buffer-list ; Command name
  "Choose a list of buffer names.
You can use `S-delete' during completion to kill a candidate buffer.
The list of names (strings) is returned.

With a prefix argument, only buffers visiting files are candidates.
Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior."                          ; Doc string
  (lambda (name) (push name buf-names)) ; Function to perform the action
  "Choose buffer (`RET' when done): "   ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf)))
          (if current-prefix-arg
              (icicle-remove-if-not (lambda (bf) (buffer-file-name bf)) (buffer-list))
            (buffer-list)))
  nil nil nil 'buffer-name-history nil nil
  ((buf-names ())                       ; Additional bindings
   (icicle-must-match-regexp              icicle-buffer-match-regexp)
   (icicle-must-not-match-regexp          icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate            icicle-buffer-predicate)
   (icicle-extra-candidates               icicle-buffer-extras)
   (icicle-sort-function                  (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object        'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag             icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag       icicle-buffer-ignore-space-prefix-flag)
   (icicle-use-candidates-only-once-flag  t))
  nil nil                               ; First code, undo code
  (prog1 (setq buf-names  (nreverse (delete "" buf-names))) ; Last code - return the list of buffers
    (when (interactive-p) (message "Buffers: %S" buf-names))))

;;;###autoload
(icicle-define-command icicle-remove-buffer-config ; Command name
  "Remove buffer configuration from `icicle-buffer-configs'." ; Doc string
  icicle-remove-buffer-config-action    ; Action function
  "Remove buffer configuration: "       ; `completing-read' args
  (mapcar (lambda (config) (list (car config))) icicle-buffer-configs)
  nil t nil 'icicle-buffer-config-history (caar icicle-buffer-configs) nil
  ((icicle-use-candidates-only-once-flag  t))) ; Additional bindings

(defun icicle-remove-buffer-config-action (config-name)
  "Action function for command `icicle-remove-buffer-config'."
  (setq icicle-buffer-configs  (icicle-assoc-delete-all config-name icicle-buffer-configs))
  (customize-save-variable 'icicle-buffer-configs icicle-buffer-configs)
  (message "Buffer configuration `%s' removed" config-name))

;;;###autoload
(icicle-define-command icicle-face-list ; Command name
  "Choose a list of face names.  The list of names (strings) is returned." ; Doc string
  (lambda (name) (push (icicle-transform-multi-completion name) face-names)) ; Action function
  "Choose face (`RET' when done): "     ; `completing-read' args
  (mapcar #'icicle-make-face-candidate (face-list))
  nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
  (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)
  nil nil
  ((icicle-list-nth-parts-join-string     ": ") ; Additional bindings
   (icicle-list-join-string               ": ")
   (icicle-list-end-string                "")
   (icicle-list-use-nth-parts             '(1))
   (icicle-use-candidates-only-once-flag  t)
   (face-names nil))
  nil nil                               ; First code, undo code
  (prog1 (setq face-names  (nreverse (delete "" face-names))) ; Last code - return list of faces.
    (when (interactive-p) (message "Faces: %S" face-names))))

;;;###autoload
(icicle-define-command icicle-color-theme ; Command name
  "Change color theme.
You can use `S-delete' during completion to remove the current
candidate from the list of color themes.

To use this command, you must have loaded library `color-theme.el',
available from http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme." ; Doc string
  (lambda (theme) (funcall (intern theme))) ; Action - just call the theme.
  "Theme: " icicle-color-themes nil t nil ; `completing-read' args
  (if (boundp 'color-theme-history) 'color-theme-history 'icicle-color-theme-history)
  nil nil
  ((icicle-delete-candidate-object  'icicle-color-themes))) ; Additional bindings

;;;###autoload
(icicle-define-command icicle-insert-kill ; Bound to `C-- C-y' via `icicle-yank-insert'.
  "Insert previously killed text from the `kill-ring'.
This is like `yank', but it does not rotate the `kill-ring'.
You can sort the candidates to yank - use `C-,'.
You can use `S-delete' during completion to remove a candidate entry
from the `kill-ring'."                  ; Doc string
  icicle-insert-for-yank                ; Function to perform the action
  "Insert: " (mapcar #'list kill-ring) nil t nil 'icicle-kill-history ; `completing-read' args
  (car kill-ring) nil
  ((icicle-transform-function       'icicle-remove-duplicates) ; Additional bindings
   (icicle-sort-function            nil)
   (icicle-delete-candidate-object  'kill-ring)))

(defun icicle-insert-for-yank (string)
  "`insert-for-yank', if defined; else, `insert' with `read-only' removed."
  (if (fboundp 'insert-for-yank)        ; Defined in `subr.el' (not required).
      (insert-for-yank string)
    (let ((opoint  (point)))
      (insert string)
      (let ((inhibit-read-only  t)) (remove-text-properties opoint (point) '(read-only nil))))))

;;;###autoload
(defun icicle-yank-insert (&optional arg) ;  Bound to `C-y' (or whatever `yank' was bound to).
  "`icicle-insert-kill' if prefix arg < 0; else `icicle-yank-function'.
However, when called from the minibuffer, this calls `icicle-yank':
The prefix arg is then ignored and *Completions* is updated with
regexp input matches.
By default, user option `icicle-yank-function' is bound to `yank'."
  (interactive "*P")
  (if (window-minibuffer-p (selected-window))
      (icicle-yank arg)
    (if (wholenump (prefix-numeric-value arg))
        (funcall icicle-yank-function arg)
      (icicle-insert-kill))))

;;;###autoload
(icicle-define-file-command icicle-delete-file ; Command name
  "Delete a file or directory."         ; Doc string
  icicle-delete-file-or-directory       ; Function to perform the action
  "Delete file or directory: " default-directory nil t nil nil ; `read-file-name' args
  ((icicle-use-candidates-only-once-flag  t))) ; Additional bindings

(defun icicle-delete-file-or-directory (file)
  "Delete file or (empty) directory FILE."
  (condition-case i-delete-file
      (if (eq t (car (file-attributes file)))
          (delete-directory file)
        (delete-file file))
    (error (message (error-message-string i-delete-file))
           (error (error-message-string i-delete-file)))))

;; $$$$$ (icicle-define-command icicle-file-list ; Command name
;;   "Choose a list of file names.
;; You can use `S-delete' during completion to delete a candidate file.
;; The list of names (strings) is returned." ; Doc string
;;   (lambda (name) (push name file-names)) ; Function to perform the action
;;   "Choose file (`RET' when done): "     ; `completing-read' args
;;   (mapcar #'list (directory-files default-directory nil icicle-re-no-dot))
;;   nil nil nil 'file-name-history nil nil
;;   ((file-names nil)                     ; Additional bindings
;;    (icicle-delete-candidate-object  'icicle-delete-file-or-directory) ; `S-delete' deletes file.
;;    (icicle-use-candidates-only-once-flag  t))
;;   nil nil                               ; First code, undo code
;;   (prog1 (setq file-names  (nreverse (delete "" file-names))) ; Last code - return files list
;;     (when (interactive-p) (message "Files: %S" file-names))))

;;;###autoload
(icicle-define-file-command icicle-file-list ; Command name
  "Choose a list of file and directory names.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.
You can navigate the directory tree, picking files and directories
anywhere in the tree.

You can use `S-delete' during completion to delete a candidate file.
  Careful: This deletes the file, it does not just remove it as a
  candidate.
The list of file names (strings) is returned." ; Doc string
  (lambda (name) (push name file-names)) ; Function to perform the action
  "Choose file (`RET' when done): "     ; `read-file-name' args
  nil nil t nil nil
  ((file-names                         nil) ; Additional bindings
   (icicle-delete-candidate-object     'icicle-delete-file-or-directory)
   (icicle-comp-base-is-default-dir-p  t)
   ;; $$$$$ (icicle-dir-candidate-can-exit-p (not current-prefix-arg))
   )
  nil nil                               ; First code, undo code
  (prog1 (setq file-names  (nreverse (delete "" file-names))) ; Last code - return list of files.
    (when (interactive-p) (message "Files: %S" file-names))))

;;;###autoload
(when (> emacs-major-version 21)
  (icicle-define-file-command icicle-directory-list ; Command name
    "Choose a list of directory names.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.
You can navigate the directory tree, picking directories anywhere in
the tree.

You can use `S-delete' during completion to delete a candidate (empty)
directory.
  Careful: This deletes the directory, it does not just remove it as a
  candidate.
The list of directory names (strings) is returned." ; Doc string
    (lambda (name) (push name dir-names)) ; Function to perform the action
    "Choose file (`RET' when done): "   ; `read-file-name' args
    ;; $$$$$$ nil nil t nil (lambda (file) (eq t (car (file-attributes file)))) ; PREDICATE
    nil nil t nil #'file-directory-p    ; PREDICATE
    ((dir-names                          nil) ; Additional bindings
     (icicle-delete-candidate-object     'icicle-delete-file-or-directory)
     (icicle-comp-base-is-default-dir-p  t)
     ;; $$$$$ (icicle-dir-candidate-can-exit-p (not current-prefix-arg))
     )
    nil nil                             ; First code, undo code
    (prog1 (setq dir-names  (nreverse (delete "" dir-names))) ; Last code - return the list of dirs.
      (when (interactive-p) (message "Directories: %S" dir-names)))))


(put 'icicle-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-file (arg)                ; Bound to `C-x C-f' in Icicle mode.
  "Visit a file or directory.
With no prefix argument, use relative file names
 (`icicle-find-file').
With a prefix argument, use absolute file names
 (`icicle-find-file-absolute').
With a negative prefix argument, you can choose also by date:
 Completion candidates include the the last modification date.

Note that when you use a prefix argument completion matches candidates
as ordinary strings.  It knows nothing of file names per se.  In
particular, you cannot use remote file-name syntax if you use a prefix
argument.

You can use `S-delete' during completion to delete a candidate file."
  (interactive "P")
  (if arg
      (if (wholenump (prefix-numeric-value arg))
          (let ((current-prefix-arg  nil)) (icicle-find-file-absolute))
        (icicle-find-file-absolute))
    (icicle-find-file)))


(put 'icicle-file-other-window 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-file-other-window (arg)   ; Bound to `C-x 4 f' in Icicle mode.
  "Visit a file or directory in another window.
With no prefix argument, use relative file names
 (`icicle-find-file-other-window').
With a prefix argument, use absolute file names
 (`icicle-find-file-absolute-other-window').
With a negative prefix argument, you can choose also by date:
 Completion candidates include the the last modification date.

Note that when you use a prefix argument completion matches candidates
as ordinary strings.  It knows nothing of file names per se.  In
particular, you cannot use remote file-name syntax if you use a prefix
argument.

You can use `S-delete' during completion to delete a candidate file."
  (interactive "P")
  (if arg
      (if (wholenump (prefix-numeric-value arg))
          (let ((current-prefix-arg  nil)) (icicle-find-file-absolute-other-window))
        (icicle-find-file-absolute-other-window))
    (icicle-find-file-other-window)))


(put 'icicle-find-file-absolute 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-find-file-absolute ; Command name
  "Visit a file or directory, given its absolute name.
Unlike `icicle-find-file', the completion candidates are absolute, not
relative, file names.  By default, the completion candidates are files
in the current directory, but you can substitute other candidates by
retrieving a saved candidate set.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

With a prefix argument, you can choose also by date: Completion
candidates include the the last modification date.

You can use `S-delete' during completion to delete a candidate file." ; Doc string
  (lambda (f) (find-file (icicle-transform-multi-completion f) 'wildcards)) ; Action function
  "File or directory: "                 ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          icicle-abs-file-candidates)
  nil nil default-directory 'file-name-history default-directory nil
  ((icicle-abs-file-candidates         (directory-files default-directory 'full nil 'nosort))
   (icicle-delete-candidate-object     'icicle-delete-file-or-directory) ; Additional bindings
   (icicle-candidate-properties-alist  (and current-prefix-arg
                                            '((1 (face 'icicle-candidate-part)))))
   (icicle-list-use-nth-parts          (and current-prefix-arg '(1)))))


(put 'icicle-find-file-absolute-other-window 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-find-file-absolute-other-window ; Command name
  "Visit a file or directory in another window, given its absolute name.
Unlike `icicle-find-file-other-window', the completion candidates are
absolute, not relative, file names.  By default, the completion
candidates are files in the current directory, but you can substitute
other candidates by retrieving a saved candidate set.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

With a prefix argument, you can choose also by date: Completion
candidates include the the last modification date.

You can use `S-delete' during completion to delete a candidate file." ; Doc string
  (lambda (f) (find-file-other-window (icicle-transform-multi-completion f) 'wildcards)) ; Action
  "File or directory: "                 ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          icicle-abs-file-candidates)
  nil nil default-directory 'file-name-history default-directory nil
  ((icicle-abs-file-candidates         (directory-files default-directory 'full nil 'nosort))
   (icicle-delete-candidate-object     'icicle-delete-file-or-directory) ; Additional bindings
   (icicle-candidate-properties-alist  (and current-prefix-arg
                                            '((1 (face 'icicle-candidate-part)))))
   (icicle-list-use-nth-parts          (and current-prefix-arg '(1)))))


(put 'icicle-find-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-file-command icicle-find-file
  "Visit a file or directory.
You can use `S-delete' during completion to delete a candidate file." ; Doc string
  icicle-find-file-w-wildcards          ; Function to perform the action
  "File or directory: " nil             ; `read-file-name' args
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file)) ; Defined in `dired+.el'.
      (condition-case nil               ; E.g. error because not on file line (ignore)
          (abbreviate-file-name (dired-get-file-for-visit))
        (error default-directory))
    default-directory)
  nil nil nil
  ((icicle-delete-candidate-object  'icicle-delete-file-or-directory))) ; Additional bindings

(defun icicle-find-file-w-wildcards (filename)
  "Find file FILENAME, where the name possibly includes shell wildcards."
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file))
      (diredp-find-a-file filename t)
    (find-file filename t)))

;;;###autoload
(icicle-define-file-command icicle-find-file-other-window
  "Visit a file or directory in another window.
You can use `S-delete' during completion to delete a candidate file." ; Doc string
  icicle-find-file-other-window-w-wildcards ; Function to perform the action
  "File or directory: " nil             ; `read-file-name' args
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file-other-window)) ; In `dired+.el'
      (condition-case nil               ; E.g. error because not on file line (ignore)
          (abbreviate-file-name (dired-get-file-for-visit))
        (error default-directory))
    default-directory)
  nil nil nil
  ((icicle-delete-candidate-object  'icicle-delete-file-or-directory))) ; Additional bindings

(defun icicle-find-file-other-window-w-wildcards (filename)
  "Find file FILENAME, where the name possibly includes shell wildcards."
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file-other-window))
      (diredp-find-a-file-other-window filename t)
    (find-file-other-window filename t)))

(put 'icicle-recent-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-recent-file ; Command name
  "Open a recently used file.
With a prefix argument, you can choose also by date: Completion
candidates include the the last modification date.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

You can use `S-delete' during completion to delete a candidate file." ; Doc string
  (lambda (f) (find-file (icicle-transform-multi-completion f) 'wildcards)) ; Action function
  "Recent file: "                       ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          recentf-list)
  nil nil nil 'file-name-history (car recentf-list) nil
  ((icicle-delete-candidate-object     'icicle-delete-file-or-directory) ; Additional bindings
   (icicle-candidate-properties-alist  (and current-prefix-arg
                                            '((1 (face 'icicle-candidate-part)))))
   (icicle-list-use-nth-parts          (and current-prefix-arg '(1))))
  (progn (unless (boundp 'recentf-list) (require 'recentf)) ;  First code
         (when (fboundp 'recentf-mode) (recentf-mode 99))
         (unless (consp recentf-list) (error "No recently accessed files"))))

;;;###autoload
(icicle-define-command icicle-recent-file-other-window ; Command name
  "Open a recently used file in another window.
With a prefix argument, you can choose also by date: Completion
candidates include the the last modification date.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

You can use `S-delete' during completion to delete a candidate file." ; Doc string
  (lambda (f) (find-file-other-window (icicle-transform-multi-completion f) 'wildcards)) ; Action
  "Recent file: "                       ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          recentf-list)
  nil nil nil 'file-name-history (car recentf-list) nil
  ((icicle-delete-candidate-object     'icicle-delete-file-or-directory) ; Additional bindings
   (icicle-candidate-properties-alist  (and current-prefix-arg
                                            '((1 (face 'icicle-candidate-part)))))
   (icicle-list-use-nth-parts          (and current-prefix-arg '(1))))
  (progn (unless (boundp 'recentf-list) (require 'recentf)) ;  First code
         (when (fboundp 'recentf-mode) (recentf-mode 99))
         (unless (consp recentf-list) (error "No recently accessed files"))))

(put 'icicle-locate-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-locate-file ; Command name
  "Visit a file within a directory or its subdirectories.
With a non-negative (>= 0) prefix argument, you are prompted for the
directory.  Otherwise, the current directory is used.

With a non-positive (<= 0) prefix argument, you can choose also by
date: Completion candidates include the last modification date.

The absolute names of all files within the directory and all of its
subdirectories are targets for completion.  Regexp input is matched
against all parts of the absolute name, not just the file-name part.

You can use this to find all files within your file system that match
a regexp, but be aware that gathering and matching the file names will
take some time.

Remember that you can save the set of files matching your input using
`\\<minibuffer-local-completion-map>\\[icicle-candidate-set-save]' or \
`\\[icicle-candidate-set-save-persistently]'.  You can then retrieve quickly them later using
`\\[icicle-candidate-set-retrieve]' or \
`\\[icicle-candidate-set-retrieve-persistent]'.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax." ; Doc string
  (lambda (f) (find-file (icicle-transform-multi-completion f) 'wildcards)) ; Action function
  "File: "                              ; `completing-read' args
  (mapcar (if (<= (prefix-numeric-value current-prefix-arg) 0)
              #'icicle-make-file+date-candidate
            #'list)
          (icicle-files-within (directory-files dir 'full icicle-re-no-dot) nil))
  nil nil nil 'file-name-history nil nil
  ((use-dialog-box                     nil) ; Additional bindings
   (dir  (if (and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
             (read-file-name "Locate under which directory: " nil default-directory nil)
           default-directory))
   (icicle-candidate-properties-alist  '((1 (face 'icicle-candidate-part))))
   (icicle-list-use-nth-parts          '(1)))
  (progn
    (icicle-highlight-lighter)
    (message "Gathering files within `%s' (this could take a while)..." dir))) ;  First code

;;;###autoload
(icicle-define-command icicle-locate-file-other-window ; Command name
  "Like `icicle-locate-file' but the file is visited in another window." ; Doc string
  (lambda (f) (find-file-other-window (icicle-transform-multi-completion f) 'wildcards)) ; Action
  "File: "                              ; `completing-read' args
  (mapcar (if (<= (prefix-numeric-value current-prefix-arg) 0)
              #'icicle-make-file+date-candidate
            #'list)
          (icicle-files-within (directory-files dir 'full icicle-re-no-dot) nil))
  nil nil nil 'file-name-history nil nil
  ((use-dialog-box                     nil) ; Additional bindings
   (dir  (if (and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
             (read-file-name "Locate under which directory: " nil default-directory nil)
           default-directory))
   (icicle-candidate-properties-alist  '((1 (face 'icicle-candidate-part))))
   (icicle-list-use-nth-parts          '(1)))
  (progn
    (icicle-highlight-lighter)
    (message "Gathering files within `%s' (this could take a while)..." dir))) ;  First code

(defun icicle-make-file+date-candidate (file)
  "Returns a multi-completion candidate: FILE + last modification date."
  (list (list file (format-time-string "%Y %m %d %T " (nth 5 (file-attributes file))))))

;;;###autoload
(icicle-define-command icicle-font      ; Command name
  "Change font of current frame."       ; Doc string
  (lambda (font) (modify-frame-parameters orig-frame (list (cons 'font font)))) ; Action function
  "Font: " (mapcar #'list (x-list-fonts "*")) ; `completing-read' args
  nil t nil (if (boundp 'font-name-history) 'font-name-history 'icicle-font-name-history) nil nil
  ((orig-frame  (selected-frame))       ; Additional bindings
   (orig-font   (frame-parameter nil 'font)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'font orig-font))) ; Undo code
  nil)                                  ; Last code

;;;###autoload
(icicle-define-command icicle-frame-bg  ; Command name
  "Change background of current frame." ; Doc string
  (lambda (color)                       ; Function to perform the action
    (modify-frame-parameters orig-frame (list (cons 'background-color color))))
  "Background color:: " (mapcar #'list (x-defined-colors)) ; `completing-read' args
  nil t nil (if (boundp 'color-history) 'color-history 'icicle-color-history) nil nil
  ((orig-frame  (selected-frame))       ; Additional bindings
   (orig-bg     (frame-parameter nil 'background-color)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'background-color orig-bg))) ; Undo code
  nil)                                  ; Last code

;;;###autoload
(icicle-define-command icicle-frame-fg  ; Command name
  "Change foreground of current frame." ; Doc string
  (lambda (color)                       ; Function to perform the action
    (modify-frame-parameters orig-frame (list (cons 'foreground-color color))))
  "Foreground color:: " (mapcar #'list (x-defined-colors)) ; `completing-read' args
  nil t nil (if (boundp 'color-history) 'color-history 'icicle-color-history) nil nil
  ((orig-frame  (selected-frame))       ; Additional bindings
   (orig-bg     (frame-parameter nil 'foreground-color)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'foreground-color orig-bg))) ; Undo code
  nil)                                  ; Last code

;; Bind this, not `icicle-Info-index', to `i' in Info mode,
;; so plain `Info-index' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-index-cmd ()         ; Bound to `i' in Info mode.
  "If in Icicle mode, run `icicle-Info-index'; else, run `Info-index'.
Note: In Emacs versions prior to version 22, this runs `Info-index'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-index 'Info-index)))

;;;###autoload
(defun icicle-Info-index ()
  "Like `Info-index', but you can use Icicles keys `C-RET', `C-up' etc."
  (interactive)
  (let ((info-buf                    (current-buffer))
        (info-window                 (selected-window))
        (icicle-candidate-action-fn  'icicle-Info-index-action))
    (call-interactively (if (> emacs-major-version 21) 'Info-index 'icicle-Info-index-20))))

;; Thx to Tamas Patrovics for this Emacs 20 version.
;;;###autoload
(defun icicle-Info-index-20 ()
  "Like `Info-index', but you can use completion for the index topic."
  (interactive)
  (Info-index "")
  (let ((pattern     "\\* +\\([^:]*\\):.")
        (candidates  nil))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t) (push (list (match-string 1)) candidates))
    (Info-index (completing-read "Index topic: " candidates nil t))))

;; Free vars here: `info-buf' and `info-window' are bound in `icicle-Info-index'.
(defun icicle-Info-index-action (topic)
  "Completion action function for `icicle-Info-index'."
  (let ((minibuf-win  (selected-window)))
    (set-buffer info-buf)
    (select-window info-window)
    (Info-index topic)
    (select-window minibuf-win)))

;; Bind this, not `icicle-Info-menu', to `m' in Info mode,
;; so plain `Info-menu' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-menu-cmd ()    ; Bound to `m' in Info mode.
  "In Icicle mode, run `icicle-Info-menu'; else, `Info-menu'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-menu 'Info-menu)))

;; Free vars here: `Info-menu-entry-name-re' is bound in `info.el'.
(icicle-define-command icicle-Info-menu
  "Go to a menu node."                  ; Doc string
  (lambda (m) (icicle-Info-goto-node (cdr (icicle-get-alist-candidate m)))) ; Action function
  "Menu item: " icicle-candidates-alist ; `completing-read' args
  nil t nil nil (save-excursion
                  (goto-char (point-min))
                  (unless (search-forward "\n* menu:" nil t) (error "No menu in this node"))
                  (setq menu-eol  (point))
                  (and (< menu-eol opoint)
                       (save-excursion
                         (goto-char opoint) (end-of-line)
                         (and (re-search-backward (concat "\n\\* +\\("
                                                          (if (boundp 'Info-menu-entry-name-re)
                                                              Info-menu-entry-name-re
                                                            "[^:\t\n]*")
                                                          "\\):")
                                                  menu-eol t)
                              (match-string-no-properties 1)))))
  nil
  ((opoint                                 (point)) ; Additional bindings
   (completion-ignore-case                 t)
   (case-fold-search                       t)
   (icicle-sort-function                   nil)
   (icicle-whole-candidate-as-text-prop-p  t)
   (Info-complete-menu-buffer              (current-buffer))
   (icicle-candidates-alist                (mapcar (lambda (m) (cons m (Info-extract-menu-item m)))
                                                   (reverse
                                                    (all-completions "" 'Info-complete-menu-item))))
   menu-eol))

;; Bind this, not `icicle-Info-goto-node', to `g' in Info mode,
;; so plain `Info-goto-node' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-goto-node-cmd ()     ; Bound to `g' in Info mode.
  "In Icicle mode, run `icicle-Info-goto-node'; else, `Info-goto-node'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-goto-node 'Info-goto-node)))

;;;###autoload
(defun icicle-Info-goto-node (nodename &optional arg)
  "Go to Info node named NODENAME.
NODENAME has the form NODE or (FILE)NODE-IN-FILE, where:
 NODE names a node in the current Info file or one of its subfiles.
 FILE names an Info file containing node NODE-IN-FILE.
Completion is available for node names in the current Info file.

With a prefix argument ARG, treated as a numeric value:

 * If ARG < 0, then completion candidates are presented in book order,
   and are limited to the current node and the rest of the book.
   In this case, the first candidate is `..', which goes up.
   
 * If ARG >= 0, then show node in a new Info buffer (Emacs 21+).

In Lisp code, if ARG is a string, then show the node in a new Info
buffer named ARG.

With no prefix argument, or with a non-negative prefix arg, you can
use `C-,' to choose how to sort completion candidates.  By default,
they are sorted alphabetically.

Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

`C-mouse-2', `C-RET' - Go to current completion candidate (node)
`C-down'  - Go to next prefix-completion candidate
`C-up'    - Go to previous prefix-completion candidate
`C-next'  - Go to next apropos-completion candidate
`C-prior' - Go to previous apropos-completion candidate

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.

This is an Icicles command - see `icicle-mode'."
  (interactive
   (let* ((info-buf                         (current-buffer))
          (info-window                      (selected-window))
          (icicle-candidate-action-fn       'icicle-Info-goto-node-action)
          (icicle-Info-only-rest-of-book-p  (< (prefix-numeric-value current-prefix-arg) 0))
          (icicle-sort-functions-alist      (cons '("in book order" .  icicle-Info-book-order-p)
                                                  icicle-sort-functions-alist))
          (icicle-sort-function             (if icicle-Info-only-rest-of-book-p
                                                #'icicle-Info-book-order-p
                                              icicle-sort-function)))
     (list (icicle-Info-read-node-name "Go to node: ") current-prefix-arg)))
  (icicle-Info-goto-node-1 nodename arg))

(defun icicle-Info-goto-node-1 (nodename &optional arg)
  "Same as `Info-goto-node', but this also goes up for `..' pseudo-node."
  (if (and (string= nodename "..") (Info-check-pointer "up"))
      (Info-up)
    (if (> emacs-major-version 20)
        (Info-goto-node nodename (and arg (not icicle-Info-only-rest-of-book-p)))
      (Info-goto-node nodename))))

(defun icicle-Info-read-node-name (prompt)
  (let* ((completion-ignore-case           t)
         (Info-read-node-completion-table  (icicle-Info-build-node-completions))
         (nodename                         (completing-read prompt 'Info-read-node-name-1 nil nil)))
    (if (equal nodename "") (icicle-Info-read-node-name prompt) nodename)))

(defun icicle-Info-build-node-completions ()
  "Build completions list for Info nodes.
This takes `icicle-Info-only-rest-of-book-p' into account."
  (icicle-highlight-lighter)
  (if (or (not icicle-Info-only-rest-of-book-p) (string= Info-current-node "Top"))
      (icicle-Info-build-node-completions-fix-*)
    (reverse (cons '("..") (member (list Info-current-node)
                                   (reverse (icicle-Info-build-node-completions-fix-*)))))))

(defun icicle-Info-build-node-completions-fix-* ()
  "`Info-build-node-completions', but get rid of `*' pseudo-node.
This just fixes a bug in Emacs 21 and 22.1."
  (let ((comps  (Info-build-node-completions)))
    (when (equal (car comps) '("*")) (setq comps  (cdr comps)))
    comps))

;; Free vars here:
;; `info-buf' and `info-window' are bound in `icicle-Info-goto-node'.
;; `Info-read-node-completion-table' is bound in `info.el'.
(defun icicle-Info-goto-node-action (node)
  "Completion action function for `icicle-Info-goto-node'."
  (set-buffer info-buf)
  (select-window info-window)
  (icicle-Info-goto-node-1 node)
  (when icicle-Info-only-rest-of-book-p
    (setq Info-read-node-completion-table  (icicle-Info-build-node-completions)
          icicle-current-input             "")
    (icicle-complete-again-update)
    (if (and (string= Info-current-node "Top") Info-history)
        (let* ((hist  Info-history)
               (last  (car (cdr (car hist)))))
          (while (string= "Top" (car (cdr (car hist)))) (pop hist))
          (setq icicle-candidate-nb
                (1- (length (reverse (member (list (car (cdr (car hist))))
                                             (icicle-Info-build-node-completions-fix-*)))))))
      (setq icicle-candidate-nb  1))     ; Skip `..'.

    ;; $$$$$$ Maybe factor this out. Same thing in several places.  However, here we don't do
    ;; `icicle-maybe-sort-and-strip-candidates' at beginning of first clause.
    (cond ((and icicle-completion-candidates (cdr icicle-completion-candidates)) ; > 1 left.
           (message "Displaying completion candidates...")
           (save-selected-window (icicle-display-candidates-in-Completions))
           (with-current-buffer "*Completions*"
             (goto-char (icicle-start-of-candidates-in-Completions))
             (icicle-move-to-next-completion
              (mod icicle-candidate-nb (length icicle-completion-candidates)))
             (set-window-point (get-buffer-window "*Completions*" 0) (point))
             (setq icicle-last-completion-candidate  (icicle-current-completion-in-Completions))
             (set-buffer-modified-p nil)))
          (icicle-completion-candidates ; Single candidate left
           (save-selected-window (icicle-remove-Completions-window))
           (let ((completion  (icicle-transform-multi-completion
                               (car icicle-completion-candidates))))
             (select-window (active-minibuffer-window))
             (with-current-buffer (window-buffer) ; Need if *Completions* redirected to minibuffer.
               (goto-char (icicle-minibuffer-prompt-end))
               (icicle-clear-minibuffer)
               (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                           (icicle-file-name-directory-w-default icicle-current-input)
                         "")
                       completion))))
          (t                            ; No candidates left
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if *Completions* redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)))))
  (select-window (active-minibuffer-window)))

(defun icicle-Info-book-order-p (s1 s2)
  "Non-nil if Info node S1 comes before node S2 in the book."
  t)        ; This just reverses the default order, which is reversed.

;;;###autoload
(icicle-define-command icicle-insert-thesaurus-entry ; Command name
  "Insert an entry from a thesaurus.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-insert-thesaurus-entry-cand-fn ; Function to perform the action
  "Thesaurus entry to match: " synonyms-obarray ; `completing-read' args
  nil nil nil 'icicle-dictionary-history nil nil
  ((icicle-track-pt  (point)))          ; Additional bindings
  (progn                                ; First code
    (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
      (error "You must first load library `synonyms.el'"))
    (synonyms-ensure-synonyms-read-from-cache))
  (when (window-live-p orig-window)     ; Undo code
    (select-window orig-window)
    (select-frame-set-input-focus (selected-frame))
    (goto-char icicle-track-pt))
  (when (window-live-p orig-window)     ; Last code
    (select-window orig-window)
    (select-frame-set-input-focus (selected-frame))
    (goto-char icicle-track-pt)))

;; Free vars here: `orig-buff' is bound in `icicle-insert-thesaurus-entry'.
(defun icicle-insert-thesaurus-entry-cand-fn (string)
  "Action function for `icicle-insert-thesaurus-entry'.
Insert STRING, followed by a space, at position TRACK-PT of buffer
ORIG-BUFF."
  (set-buffer orig-buff)
  (goto-char icicle-track-pt)
  (insert string " ")
  (setq icicle-track-pt  (point))
  (save-excursion (set-buffer (window-buffer (minibuffer-window))) (icicle-clear-minibuffer))
  (save-selected-window (icicle-remove-Completions-window)))

;;;###autoload
(defun icicle-complete-thesaurus-entry (word) ; Bound to `C-c /' in Icicle mode.
  "Complete WORD to an entry from a thesaurus.
The default value of WORD is the word at the cursor.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'."
  (interactive (list (word-at-point)))
  (unless word (error "No word at point to complete"))
  (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
    (error "You must first load library `synonyms.el'"))
  (synonyms-ensure-synonyms-read-from-cache)
  (when (and (looking-at "\\b") (not (looking-at "\\s-"))) (forward-word 1))
  (delete-region (progn (forward-word -1) (point)) (progn (forward-word 1) (point)))
  (insert (completing-read "Thesaurus entry to match: " synonyms-obarray
                           nil nil word 'icicle-dictionary-history word))
  (unless (looking-at "\\s-") (insert " ")))

;;;###autoload
(icicle-define-command icicle-plist     ; Command name
  "Choose a symbol and its property list.
Each candidate for completion is a symbol name plus its property list
\(as a string).  They are separated by `icicle-list-join-string'
\(^G^J, by default).  You can match an input regexp against the symbol
name or the property list or both.  Use `C-M-j' (equivalent here to
`C-q C-g C-j') to input the default separator.

Remember that you can use `[^^G]' to match any character except ^G,
which includes newline. Remember also that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  (lambda (entry) (with-output-to-temp-buffer "*Help*" (princ entry))) ; Action function
  "SYMB `C-M-j' PLIST (`RET' when done): " ; `completing-read' args
  (let ((result  nil))                  ; TABLE arg is an alist with items ((symb plist-string))
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (condition-case nil
                    (let ((plist  (symbol-plist symb)))
                      (when plist
                        (push (list (list (symbol-name symb) (format "%s" plist))) result)))
                  (error nil))))        ; Ignore symbols that produce errors.
    result)
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist  '((1 (face 'icicle-candidate-part))))) ; Additional bindings
  (progn
    (icicle-highlight-lighter)
    (message "Gathering property lists..."))) ; First code

;;;###autoload
(icicle-define-command icicle-where-is  ; Command name
  "Show keyboard/menu/mouse sequences that invoke specified command.
This is a multi-command version of `where-is'.

With no prefix argument, only commands actually bound to keys are
completion candidates.  With a prefix argument, all commands are
candidates.

With a plain (non-numeric) prefix argument, `C-u', insert the message
in the current buffer."                 ; Doc string
  (lambda (x) (let ((symb  (intern-soft x))) ; Action function
                (where-is symb (and pref-arg (consp pref-arg)))))
  "Where is command: " obarray          ; `completing-read' args
  (if pref-arg
      'commandp
    (lambda (c)
      (with-current-buffer orig-buff
        (and (commandp c) (where-is-internal c overriding-local-map 'non-ascii)))))
  t nil nil (let ((fn  (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                           (function-called-at-point))))
              (and fn (symbol-name fn)))
  t
  ((pref-arg  current-prefix-arg)       ; Additional bindings
   (icicle-candidate-help-fn
    (lambda (c) (let* ((keys   (where-is-internal (intern-soft c) overriding-local-map))
                       (keys1  (mapconcat 'key-description keys ", ")))
                  (message (format "`%s' is on `%s'" c keys1))
                  (sit-for 3))))))

;;;###autoload
(icicle-define-command icicle-describe-option-of-type ; Command name
  "Describe a user option that was defined with a given `defcustom' type.
Enter patterns for the OPTION name and TYPE definition in the
minibuffer, separated by `icicle-list-join-string', which is \"^G^J\",
by default.  (`^G' here means the Control-g character, input using
`C-h C-g'.  Likewise, for `^J'.)

OPTION is a regexp that is matched against option names.

Depending on the prefix arg, TYPE is interpreted as either of these:

 - a regexp to match against the option type

 - a definition acceptable for `defcustom' :type, or its first symbol,
   for example, (choice (integer) (regexp)) or `choice'

In the second case, depending on the prefix arg, TYPE can be matched
against the option type, or it can be matched against either the
option type or one of its subtypes.

In the second case also, depending on the prefix arg, if TYPE does not
match some option's type, that option might still be a candidate, if
its current value satisfies TYPE.

In sum, the prefix arg determines the type-matching behavior, as
follows:

 - None:      OPTION is defined with TYPE or a subtype of TYPE.
              TYPE is a regexp.

 - `C-u':     OPTION is defined with TYPE or a subtype of TYPE,
                or its current value is compatible with TYPE.
              TYPE is a type definition or its first symbol.

 - Negative:  OPTION is defined with TYPE (exact match).
              TYPE is a regexp.

 - Positive:  OPTION is defined with TYPE,
                or its current value is compatible with TYPE.
              TYPE is a type definition or its first symbol.

 - Zero:      OPTION is defined with TYPE or a subtype of TYPE.
              TYPE is a type definition or its first symbol.

 - `C-u C-u': OPTION is defined with TYPE (exact match).
              TYPE is a type definition or its first symbol.

You can change these prefix-arg key sequences by customizing option
`icicle-option-type-prefix-arg-list'.  For example, if you tend to use
the matching defined here for `C-u', you might want to make that the
default behavior (no prefix arg).  You can assign any of the six
behaviors to any of the prefix-arg keys.

If TYPE is nil, then *all* options that match OPTION are candidates.

Note that options defined in libraries that have not been loaded can
be candidates, but their type will appear as nil, since it is not
known before loading the option definition.

You can match your input against the option name or the type
definition or both.  Use `C-M-j' (equivalent here to `C-q C-g C-j') to
input the default separator.

For example, to match all Icicles options whose type matches `string'
\(according to the prefix arg), use `S-TAB' with this input:

icicle.*^G
string$

If you instead want all Icicles options whose type definition contains
`string', as in (repeat string), then use this:

icicle.*^G
\[^^G]*string

Here, `[^^G]' matches any character except ^G, which includes newline.
If you use `.'  here instead of `[^^G]', then only the first lines of
type definitions are searched for `string', because `.' matches any
character except a newline.  (The first `^' in `[^^G]' is a circumflex
character.  The second `^' is part of `^G', the printed representation
of a Control-g character.)

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-describe-opt-action            ; Action function
  "OPTION `C-M-j' TYPE (`RET' when done): " ; `completing-read' args
  'icicle-describe-opt-of-type-complete nil nil nil nil nil nil
  ((icicle-candidate-properties-alist  '((1 (face 'icicle-candidate-part)))) ; Additional bindings
   ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching
   ;; in `icicle-unsorted-apropos-candidates' etc., because `icicle-describe-opt-of-type-complete'
   ;; does everything.
   (icicle-apropos-complete-match-fn   nil)
   (icicle-candidate-help-fn           'icicle-describe-opt-action)
   ;; $$$ (icicle-highlight-input-completion-failure nil)
   (pref-arg                           current-prefix-arg))
  (progn                                ; First code
    (icicle-highlight-lighter)
    (message "Gathering user options and their types...")))

(defun icicle-describe-opt-action (opt+type)
  "Action function for `icicle-describe-option-of-type'."
  (let ((icicle-list-use-nth-parts  '(1)))
    (describe-variable (intern (icicle-transform-multi-completion opt+type)))))

;; Free var here: `pref-arg' - it is bound in `icicle-describe-option-of-type'.
(defun icicle-describe-opt-of-type-complete (strg pred completion-mode)
  "Completion function for `icicle-describe-option-of-type'.
This is used as the value of `minibuffer-completion-table'."
  (setq strg  icicle-current-input)
  ;; Parse strg into its option part and its type part: OPS  and TPS.
  ;; Make raw alist of all options and their types: ((a . ta) (b . tb)...).
  (let* ((num-prefix  (prefix-numeric-value pref-arg))
         (mode        (cond ((not pref-arg) ; No prefix arg
                             (nth 4 icicle-option-type-prefix-arg-list))
                            ((and (consp pref-arg) (= 16 num-prefix)) ; C-u C-u
                             (nth 0 icicle-option-type-prefix-arg-list))
                            ((consp pref-arg) (nth 2 icicle-option-type-prefix-arg-list)) ; C-u
                            ((zerop num-prefix) (nth 1 icicle-option-type-prefix-arg-list)) ; C-0
                            ((wholenump num-prefix) ; C-9
                             (nth 3 icicle-option-type-prefix-arg-list))
                            (t (nth 5 icicle-option-type-prefix-arg-list)))) ; C--
         (ops         (let ((icicle-list-use-nth-parts  '(1)))
                        (icicle-transform-multi-completion strg)))
         (tps         (let ((icicle-list-use-nth-parts  '(2)))
                        (icicle-transform-multi-completion strg)))
         (tp          (and (not (string= "" tps))
                           ;; Use regexp if no prefix arg or negative; else use sexp.
                           (if (memq mode '(inherit-or-regexp direct-or-regexp)) tps (read tps))))
         (result      nil))
    (mapatoms
     (lambda (symb)
       (when (if (fboundp 'custom-variable-p) (custom-variable-p symb) (user-variable-p symb))
         (condition-case nil
             (push (list symb (get symb 'custom-type)) result)
           (error nil)))))
    ;; Keep only candidates that correspond to input.
    (setq result
          (let ((ops-re  (if (memq icicle-current-completion-mode '(nil apropos))
                             ops
                           (concat "^" ops))))
            (icicle-remove-if-not
             (lambda (opt+typ)
               (and (string-match ops-re (symbol-name (car opt+typ)))
                    (or (null tp)
                        (icicle-var-is-of-type-p (car opt+typ) (list tp)
                                                 (case mode
                                                   ((inherit inherit-or-regexp) 'inherit)
                                                   ((direct  direct-or-regexp)  'direct)
                                                   (inherit-or-value     'inherit-or-value)
                                                   (direct-or-value      'direct-or-value))))))
             result)))
    ;; Change alist entries to multi-completions: "op^G^Jtp".
    (setq result  (mapcar (lambda (entry)
                            (concat (mapconcat (lambda (e) (pp-to-string e))
                                               entry icicle-list-join-string)
                                    icicle-list-end-string))
                          result))
    (if completion-mode
        result                          ; `all-completions', `test-completion'
      (try-completion strg (mapcar #'list result) pred)))) ; `try-completion'

;;;###autoload
(icicle-define-command icicle-vardoc    ; Command name
  "Choose a variable description.
Each candidate for completion is a variable name plus its
documentation.  They are separated by `icicle-list-join-string'
\(\"^G^J\", by default).  You can match an input regexp against the
variable name or the documentation or both.  Use `C-M-j' (equivalent
here to `C-q C-g C-j') to input the default separator.

For example, use input

\"dired.*^G
\[^^G]*list\"

with `S-TAB' to match all variables whose names contain \"dired\" and
whose documentation contains \"list\".  Here, `[^^G]' matches any
character except ^G, which includes newline.  If you use `.*' here,
instead, then only the first lines of doc strings are searched.

With a prefix argument, only user variables (options) are candidates.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-funvardoc-action               ; Action function
  "VAR `C-M-j' DOC (`RET' when done): " ; `completing-read' args
  (let ((result  nil))                  ; TABLE arg is an alist whose items are ((symb doc)).
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (condition-case nil
                    (when (and (boundp symb) (or (not current-prefix-arg) (user-variable-p symb)))
                      (let ((doc  (documentation-property symb 'variable-documentation)))
                        (when (icicle-non-whitespace-string-p doc)
                          (push (list (list (symbol-name symb) doc)) result))))
                  (error nil))))        ; Ignore symbols that produce errors.
    result)
  nil nil nil 'icicle-doc-history nil nil
  ((icicle-candidate-properties-alist  '((1 (face 'icicle-candidate-part)))) ; Additional bindings
   (icicle-candidate-help-fn           'icicle-funvardoc-action))
  (progn
    (icicle-highlight-lighter)
    (message "Gathering variable descriptions..."))) ; First code

(defun icicle-funvardoc-action (entry)
  "Completion action function for `icicle-vardoc' and `icicle-fundoc'."
  (with-output-to-temp-buffer "*Help*" (princ entry)))

;;;###autoload
(icicle-define-command icicle-fundoc    ; Command name
  "Choose a function description.
Each candidate for completion is a function name plus its
documentation.  They are separated by `icicle-list-join-string'
\(\"^G^J\", by default).  You can match an input regexp against the
function name or the documentation or both.  Use `C-M-j' (equivalent
here to `C-q C-g C-j') to input the default separator.

For example, use input

\"dired.*^G
\[^^G]*file\"

with `S-TAB' to match all functions whose names contain \"dired\" and
whose documentation contains \"file\".  Here, `[^^G]' matches any
character except ^G, which includes newline.  If you use `.*' here,
instead, then only the first lines of doc strings are searched.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-funvardoc-action               ; Action function
  "FUNC `C-M-j' DOC (`RET' when done): " ; `completing-read' args
  (let ((result  nil))                  ; TABLE arg is an alist whose items are ((symb doc)).
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (when (fboundp symb)
                  (condition-case nil
                      (let ((doc  (icicle-fn-doc-minus-sig (documentation symb))))
                        (when (icicle-non-whitespace-string-p doc)
                          (push (list (list (symbol-name symb) doc)) result)))
                    (error nil)))))     ; Ignore symbols that produce errors.
    result)
  nil nil nil 'icicle-doc-history nil nil
  ((icicle-candidate-properties-alist  '((1 (face 'icicle-candidate-part)))) ; Additional bindings
   (icicle-candidate-help-fn           'icicle-funvardoc-action))
  (progn
    (icicle-highlight-lighter)
    (message "Gathering function descriptions..."))) ; First code

(defun icicle-fn-doc-minus-sig (docstring)
  "Return DOCSTRING minus the function signature (usage info)."
  (let ((sig-p  (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring)))
    (if sig-p (substring docstring 0 (match-beginning 0)) docstring)))

;;;###autoload
(icicle-define-command icicle-doc       ; Command name
  "Choose documentation for a symbol.
Each candidate for completion is the description of a function,
variable, or face.  Displays the documentation and returns the symbol.

Remember that you can use `S-TAB' to toggle incremental completion." ; Doc string
  icicle-doc-action                     ; Action function: display the doc.
  "Find doc with regexp: "              ; `completing-read' args
  (let ((result  nil)                   ; TABLE arg is an alist whose items are (doc . symb).
        doc)                            ; Each completion candidate is a list of strings.
    (mapatoms (lambda (symb)
                (condition-case nil
                    (progn
                      (cond ((fboundp symb)
                             (setq doc  (concat (icicle-fn-doc-minus-sig (documentation symb))
                                                "\n\n"))
                             (when (icicle-non-whitespace-string-p doc)
                               (push (cons doc symb) result)))
                            ;; $$$ This works fine, but it slows things down:
                            ;; ((and (fboundp 'describe-keymap) (boundp symb)
                            ;;       (keymapp (symbol-value symb)))
                            ;;  (setq doc (concat
                            ;;             (symbol-name symb) ":\n"
                            ;;             (documentation-property symb 'variable-documentation)
                            ;;             "\n\n"
                            ;;             (substitute-command-keys
                            ;;              (concat "\\{" (symbol-name symb) "}")) "\n\n"))
                            ;;  (when (icicle-non-whitespace-string-p doc)
                            ;;    (push (cons doc symb) result)))
                            ((boundp symb)
                             (setq doc
                                   (concat (documentation-property symb 'variable-documentation)
                                           "\n\n"))
                             (when (icicle-non-whitespace-string-p doc)
                               (push (cons doc symb) result)))
                            ((facep symb)
                             (setq doc  (concat (documentation-property symb 'face-documentation)
                                                "\n\n"))
                             (when (icicle-non-whitespace-string-p doc)
                               (push (cons doc symb) result)))))
                  (error nil))))        ; Ignore symbols that produce errors.
    ;; `icicle-candidate-action-fn' is used in the main body of command
    ;;`icicle-doc' and is also bound to `C-RET'.  We need to refer to the
    ;; TABLE arg to `completing-read' within the body of the function.
    ;; So, we cheat and pre-assign `minibuffer-completion-table' to it here.
    (setq minibuffer-completion-table  result))
  nil nil nil 'icicle-doc-history nil nil
  ((icicle-transform-function  'icicle-remove-duplicates) ; Bindings. Dups are due to `fset's.
   (icicle-candidate-help-fn   'icicle-doc-action))
  (progn
    (icicle-highlight-lighter)
    (message "Gathering documentation..."))) ; First code

(defun icicle-doc-action (entry)
  "Completion action function for `icicle-doc': Display the doc."
  (let ((symb  (cdr (assoc entry minibuffer-completion-table))))
    (cond ((fboundp symb) (describe-function symb))
          ;; $$$ This works fine, but it slows things down:
          ;; ((and (fboundp 'describe-keymap) (boundp symb) (keymapp (symbol-value symb)))
          ;;  (describe-keymap symb))
          ((and symb (boundp symb)) (describe-variable symb))
          ((facep symb) (describe-face symb)))
    symb))

;;;###autoload
(defun icicle-non-whitespace-string-p (string)
  "Return non-nil if STRING contains a non-whitespace character.
The `standard-syntax-table' definition of whitespace is used."
  (interactive "s")
  (let ((orig-syntable  (syntax-table)))
    (unwind-protect
       (progn
         (set-syntax-table (standard-syntax-table))
         (and (stringp string) (> (length string) 0) (string-match "\\S-" string)))
      (set-syntax-table orig-syntable))))

;;;###autoload
(defun icicle-apropos (apropos-regexp &optional do-all)
  "Like `apropos', but lets you see the list of matches (with `S-TAB')."
  (interactive (list (completing-read "Apropos symbol (regexp or words): " obarray
                                      nil nil nil 'regexp-history)
                     current-prefix-arg))
  (apropos apropos-regexp do-all))

;;;###autoload
(cond
  ;; Use my versions of the `apropos*' commands, defined in `apropos-fn+var.el'.
  ;; Note that unlike my versions of `apropos-option' and `apropos-command', the `icicle-'
  ;; versions here do not respect `apropos-do-all': they always work with options and commands.
  ((fboundp 'apropos-option)
   (defun icicle-apropos-variable (pattern)
     "Show variables that match PATTERN.
This includes variables that are not user options.
You can see the list of matches with `S-TAB'.
See `apropos-variable' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos variable (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray
             #'(lambda (symbol) (and (boundp symbol) (get symbol 'variable-documentation)))
             nil nil 'regexp-history)))
     (apropos-variable pattern))

   (defun icicle-apropos-option (pattern)
     "Show user options (variables) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-option' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos user option (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'user-variable-p nil nil 'regexp-history)))
     (let ((apropos-do-all  nil)) (apropos-option pattern)))

   (defun icicle-apropos-function (pattern)
     "Show functions that match PATTERN.
This includes functions that are not commands.
You can see the list of matches with `S-TAB'.
See `apropos-function' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos function (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'fboundp nil nil 'regexp-history)))
     (apropos-function pattern))

   (defun icicle-apropos-command (pattern)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-command' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos command (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'commandp nil nil 'regexp-history)))
     (let ((apropos-do-all  nil)) (apropos-command pattern))))

  ;; My versions are not available.  Use the vanilla Emacs versions of the `apropos...' commands.
  (t
   (defun icicle-apropos-variable (pattern &optional do-all)
     "Show variables that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-variable' for a description of PATTERN.

With optional prefix DO-ALL or if `apropos-do-all' is non-nil, also
show normal variables."
     (interactive
      (list (progn
              (unless (or (boundp 'apropos-do-all) (require 'apropos nil t))
                (error "Library `apropos' not found"))
              (completing-read
               (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                      "variable" "user option")
                       " (regexp" (and (>= emacs-major-version 22) " or words") "): ")
               obarray (if (or current-prefix-arg apropos-do-all)
                           #'(lambda (symbol) (and (boundp symbol)
                                                   (get symbol 'variable-documentation)))
                         'user-variable-p)
               nil nil 'regexp-history))
            current-prefix-arg))
     (apropos-variable pattern do-all))

   (defun icicle-apropos-command (pattern &optional do-all var-predicate)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-command' for a description of PATTERN.

With \\[universal-argument] prefix, or if `apropos-do-all' is non-nil,
also show noninteractive functions.

If VAR-PREDICATE is non-nil, show only variables, and only those that
satisfy the predicate VAR-PREDICATE."
     (interactive
      (list (progn
              (unless (boundp 'apropos-do-all)
                (unless (require 'apropos nil t) (error "Library `apropos' not found")))
              (completing-read
               (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                      "command or function" "command")
                       "(regexp" (and (>= emacs-major-version 22) " or words") "): ")
               obarray (if current-prefix-arg 'fboundp 'commandp) nil nil 'regexp-history))
            current-prefix-arg))
     (apropos-command pattern do-all var-predicate))))

;;;###autoload
(defun icicle-apropos-zippy (regexp)
  "Show all Zippy quotes matching the regular-expression input.
Return the list of matches."
  (interactive (progn (unless (boundp 'yow-file)
                        (unless (require 'yow nil t) (error "Library `yow' not found")))
                      (cookie yow-file yow-load-message yow-after-load-message)
                      (let* ((case-fold-search     t)
                             (cookie-table-symbol  (intern yow-file cookie-cache))
                             (string-table         (symbol-value cookie-table-symbol))
                             (table                (nreverse (mapcar #'list string-table))))
                        (list (completing-read "Apropos Zippy (regexp): " table
                                               nil nil nil 'regexp-history)))))
  (let ((matches  (apropos-zippy icicle-current-input)))
    (when (interactive-p)
      (with-output-to-temp-buffer "*Zippy Apropos*"
        (while matches
          (princ (car matches))
          (setq matches  (cdr matches))
          (and matches (princ "\n\n")))))
    matches))                           ; Return matching Zippyisms.

(defalias 'icicle-map 'icicle-apply)
;;;###autoload
(defun icicle-apply (alist fn &optional nomsg)
  "Selectively apply a function to elements in an alist.
FN is a function.  ALIST is an alist such as can be used as the

COLLECTION argument for Icicles `completing-read'.  Its elements can
represent multi-completions, for example.  Interactively, COLLECTION
is a variable (a symbol) whose value is an alist.

Optional argument NOMSG non-nil means do not display an informative
message each time FN is applied.  If nil, then a message shows the key
of the alist element that FN is applied to and the result of the
application.

Interactively, you are prompted for both arguments.  Completion is
available for each.  The completion list for ALIST candidates is the
set of variables whose value is a cons.  With no prefix argument, the
names of these variables must end with \"alist\".  With a prefix
argument, the first car of each variable value must itself be a cons.

After choosing the ALIST and FN, you are prompted to choose one or
more keys of the alist elements, and FN is applied to each element
that has a key that you choose.  Multi-command completion is available
for choosing these candidates: you can apply FN to any number of
elements, any number of times.

Examples: If ALIST is `auto-mode-alist' and FN is `cdr', then the
completion candidates are the keys of the alist, and the result of
applying FN to an alist element is simply the value of that key.  If
you choose, for example, candidate \"\\.el\\'\", then the result is
`cdr' applied to the alist element (\"\\.el\\'\" . emacs-lisp-mode),
which is the symbol `emacs-lisp-mode'.  In this case, the function
performs simple lookup.

If FN were instead (lambda (x) (describe-function (cdr x))), then the
result of choosing candidate \"\\.el\\'\" would be to display the help
for function `emacs-lisp-mode'.

NOTE: `icicle-apply' does not, by itself, impose any particular sort
order.  Neither does it inhibit sorting.  If you call this function
from Lisp code and you want it to use a certain sort order or you want
no sorting, then bind `icicle-sort-function' accordingly.

During completion you can use multi-command keys.  Each displays the
value of applying FN to an alist element whose key is a completion
candidate.

`C-RET'   - Act on current completion candidate only
`C-down'  - Move to next prefix-completion candidate and act
`C-up'    - Move to previous prefix-completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`C-!'     - Act on *each* candidate (or each that is saved), in turn.
`M-!'     - Act on the list of *all* candidates (or all saved).

Note that `M-!' applies FN to the *list* of chosen alist elements,
whereas `C-!' applies FN to each chosen element, in turn.  For
example, if FN is `length' and your input is `\.el', then `M-!' displays
the result of applying `length' to the list of chosen elements:

 ((\"\\.el\\'\" . emacs-lisp-mode) (\"\\.elc'\" . emacs-lisp-mode))

which is 2.

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.  This is an Icicles command - see `icicle-mode'.

`icicle-apply' overrides `icicle-ignore-space-prefix-flag', binding it
to nil so that candidates with initial spaces can be matched."
  (interactive
   (list (symbol-value
          (intern (completing-read "Alist (variable): " obarray
                                   `(lambda (symb)
                                     (and
                                      (boundp symb) (consp (symbol-value symb))
                                      ,(if current-prefix-arg
                                           '(consp (car (symbol-value symb)))
                                           '(string-match "alist$" (symbol-name symb)))))
                                   t nil (if (boundp 'variable-name-history)
                                             'variable-name-history
                                           'icicle-variable-name-history))))
         (read (completing-read "Function: " obarray 'functionp nil nil
                                (if (boundp 'function-name-history)
                                    'function-name-history
                                  'icicle-function-name-history)))))

  (setq icicle-candidate-entry-fn  fn)  ; Save in global variable - used by `icicle-apply-action'.
  (let ((icicle-candidate-action-fn            'icicle-apply-action)
        (icicle-all-candidates-list-action-fn  'icicle-apply-list-action)
        (icicle-ignore-space-prefix-flag       nil)
        (icicle-apply-nomsg                    nomsg)
        (enable-recursive-minibuffers          t))
    (icicle-explore
     (lambda ()
       (setq icicle-candidates-alist    ; Ensure that keys of ALIST are strings or conses.
             (mapcar (lambda (key+val)
                       (if (consp (car key+val))
                           key+val      ; Multi-completion candidate: (("aaa" "bbb") . ccc)
                         (cons (format "%s" (car key+val)) (cdr key+val))))
                     alist)))
     (lambda ()
       (let ((result  (funcall icicle-candidate-entry-fn icicle-explore-final-choice-full)))
         (unless nomsg
           (message "Key: %s,  Result: %s" (car icicle-explore-final-choice-full) result))
         result))                       ; Return result.
     nil nil nil "Choose an occurrence: " nil t)))

(defun icicle-apply-action (string)
  "Completion action function for `icicle-apply'."
  (unwind-protect
      (condition-case icicle-apply-action
	  (progn
	    (icicle-highlight-candidate-in-Completions)
	    ;; Apply function to candidate element and display it.
	    (let* ((key+value  (icicle-get-alist-candidate string))
		   (result     (funcall icicle-candidate-entry-fn key+value)))
	      (unless icicle-apply-nomsg
		(message "Key: %s,  Result: %s" (car key+value) result)))
	    nil)			; Return nil for success.
	(error "%s" (error-message-string icicle-apply-action))) ; Return error msg.
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-apply-list-action (strings)
  "Completion action list function for `icicle-apply'."
  (unwind-protect
       (condition-case icicle-apply-list-action
           (progn                       ; Apply function to candidate element and display it.
             (message "Result: %s" (funcall icicle-candidate-entry-fn
                                            (mapcar #'icicle-get-alist-candidate strings)))
             nil)                       ; Return nil for success.
         (error "%s" (error-message-string icicle-apply-list-action))) ; Return error msg.
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

;;;###autoload
(defun icicle-goto-marker-or-set-mark-command (arg) ; Bound to `C-@', `C-SPC'.
  "With prefix arg < 0, `icicle-goto-marker'; else `set-mark-command'."
  (interactive "P")
  (if (wholenump (prefix-numeric-value arg))
      (set-mark-command arg)
    (icicle-goto-marker)))

;;;###autoload
(defun icicle-goto-global-marker-or-pop-global-mark (arg) ; Bound to `C-x C-@', `C-x C-SPC'.
  "With prefix arg < 0, `icicle-goto-global-marker'; else `pop-global-mark'."
  (interactive "P")
  (if (wholenump (prefix-numeric-value arg))
      (pop-global-mark)
    (icicle-goto-global-marker)))

;;;###autoload
(defun icicle-goto-marker ()
  "Go to a marker in this buffer, choosing it by the line that includes it.
The target line is highlighted temporarily (Emacs 22 or later).

By default, candidates are sorted in marker order, that is, with
respect to their buffer positions.  Use `C-M-,' or `C-,' to change the
sort order.

During completion you can use these keys:

`C-RET'   - Goto marker named by current completion candidate
`C-down'  - Goto marker named by next prefix-completion candidate
`C-up'    - Goto marker named by previous prefix-completion candidate
`C-next'  - Goto marker named by next apropos-completion candidate
`C-prior' - Goto marker named by previous apropos-completion candidate
`S-delete' - Delete marker named by current completion candidate

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to choose a candidate as the final
destination, or `C-g' to quit.  This is an Icicles command - see
`icicle-mode'."
  (interactive)
  (let ((icicle-sort-functions-alist  (cons '("by position" .  icicle-cdr-lessp)
                                            icicle-sort-functions-alist))
        (icicle-sort-function         'icicle-cdr-lessp))
    (icicle-goto-marker-1 mark-ring)))

;;;###autoload
(defun icicle-goto-global-marker ()
  "Like `icicle-goto-marker', but visits global, not local, markers.
If user option `icicle-add-buffer-name-flag' is non-nil, then each
completion candidate is annotated (prefixed) with the name of the
marker's buffer, to facilitate orientation."
  (interactive)
  (let ((icicle-list-nth-parts-join-string  "\t")
        (icicle-list-join-string            "\t")
        (icicle-list-end-string             "")
        (icicle-sort-functions-alist
         (cons '("by buffer, then by position" . icicle-part-1-cdr-lessp)
               icicle-sort-functions-alist))
        (icicle-sort-function               'icicle-part-1-cdr-lessp)
        (icicle-candidate-properties-alist  (and icicle-add-buffer-name-flag
                                                 '((1 (face 'icicle-candidate-part))))))
    (icicle-goto-marker-1 global-mark-ring)))

(defun icicle-goto-marker-1 (ring)
  "Helper function for `icicle-goto-marker', `icicle-goto-global-marker'.
RING is the marker ring to use."
  (unwind-protect
       (let* ((global-ring-p  (memq this-command '(icicle-goto-global-marker
                                                   icicle-goto-global-marker-or-pop-global-mark)))
              (markers        (if (and (not global-ring-p) (marker-buffer (mark-marker)))
                                  (cons (mark-marker) (icicle-markers ring))
                                (icicle-markers ring)))
              (icicle-delete-candidate-object
               (lambda (cand)
                 (let ((mrkr+txt  (icicle-get-alist-candidate cand)))
                   (move-marker (cdr mrkr+txt) nil))))
              (icicle-alternative-sort-function  nil)
              (icicle-last-sort-function         nil)
              (orig-buff                         (current-buffer)))
         (unless (consp markers)
           (error (if global-ring-p "No global markers" "No markers in this buffer")))
         (cond ((cdr markers)
                (icicle-apply (mapcar (lambda (mrkr) (icicle-marker+text mrkr global-ring-p))
                                      markers)
                              #'icicle-goto-marker-1-action
                              'nomsg))
               ((= (point) (car markers)) (message "Already at marker: %d" (point)))
               (t
                (icicle-goto-marker-1-action (icicle-marker+text (car markers) global-ring-p)))))
    (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))))

(defun icicle-goto-marker-1-action (cand)
  "Action function for `icicle-goto-marker-1'."
  (pop-to-buffer (marker-buffer (cdr cand)))
  (select-frame-set-input-focus (selected-frame))
  (goto-char (cdr cand))
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))

(defun icicle-marker+text (marker &optional globalp)
  "Cons of text line that includes MARKER with MARKER itself.
If the marker is on an empty line, then text \"<EMPTY LINE>\" is used.
If both optional argument GLOBALP and option
`icicle-add-buffer-name-flag' are non-nil, then the text is prefixed
by MARKER's buffer name."
  (save-excursion
    (set-buffer (marker-buffer marker))
    (goto-char marker)
    (let ((line  (buffer-substring-no-properties (save-excursion (beginning-of-line) (point))
                                                 (save-excursion (end-of-line) (point)))))
      (when (string= "" line) (setq line  "<EMPTY LINE>"))
      (if (and globalp icicle-add-buffer-name-flag)
          (cons (list (buffer-name) line) marker)
        (cons line marker)))))

(defun icicle-markers (ring)
  "Marks in mark RING that are in live buffers other than a minibuffer."
  (let ((markers  nil))
    (dolist (mkr ring)
      (when (and (buffer-live-p (marker-buffer mkr))
                 (not (string-match "\\` \\*Minibuf-[0-9]+\\*\\'"
                                    (buffer-name (marker-buffer mkr)))))
        (push mkr markers)))
    markers))

;;;###autoload
(defun icicle-region-open-all-files ()
  "Visit all files that contain regions in `icicle-region-alist'.
The files are visited, but not displayed.
If a file listed in `icicle-region-alist' does not exist or is not
readable, then it is ignored."
  (interactive)
  (let ((alist  (icicle-remove-if-not (lambda (entry) (car (cddr entry))) icicle-region-alist)))
    (unless alist (error "There are no file buffers in `icicle-region-alist'"))
    (dolist (entry alist)
      (let ((file  (car (cddr entry)))) (when (file-readable-p file) (find-file-noselect file))))))

;;;###autoload
(defun icicle-exchange-point-and-mark (&optional arg) ; Bound to `C-x C-x'.
  "`exchange-point-and-mark', `icicle-add-region', or `icicle-select-region'.
With no prefix arg: `exchange-point-and-mark'.
With a numeric prefix arg:`icicle-add-region'.
With a plain `C-u' prefix arg: `icicle-select-region'."
  (interactive "P")
  (if arg
      (if (atom current-prefix-arg)
          (call-interactively #'icicle-add-region)
        (unless (consp icicle-region-alist)
          (error "`icicle-region-alist' is empty; try again, with a numeric prefix arg"))
        (call-interactively #'icicle-select-region))
    (call-interactively #'exchange-point-and-mark)))

;;;###autoload
(defun icicle-add-region (start end &optional tag) ; Bound to `C-N C-x C-x', N = whole number.
  "Add current region to list of regions, `icicle-region-alist'.
This saves (the limits of) the current region.
Updates the persistent value of user option `icicle-region-alist'.

With a prefix argument, you are prompted for a tag to name the region.
Otherwise, the first `icicle-regions-name-length-max' characters of
the region itself serve as the name.

To remove a region from `icicle-region-alist', use command
`icicle-remove-region' or customize `icicle-region-alist'."
  (interactive "r\nP")
  (when (= start end) (error "Cannot add region; it is empty"))
  (when (> start end) (setq end  (prog1 start (setq start  end))))
  (let ((region-prefix
         (buffer-substring-no-properties start (+ start (min icicle-regions-name-length-max
                                                             (- end start))))))
    (add-to-list 'icicle-region-alist
                 (list (setq tag  (if tag
                                      (icicle-completing-read-history "Region name (tag): " nil nil
                                                                      nil region-prefix)
                                    region-prefix))
                       (buffer-name)
                       (buffer-file-name)
                       start
                       end))
    (customize-save-variable 'icicle-region-alist icicle-region-alist)
    (message "Region added to `icicle-region-alist' with tag `%s'"
             (if (> (length tag) 20) (concat (substring tag 0 17) "...") tag))))

;;;###autoload
(icicle-define-command icicle-select-region ; Bound to `C-u C-x C-x'
  "Choose a region from the list of Icicles regions, and activate it.
Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

Regions in `icicle-region-alist' that are in buffers that do not
currently exist are ignored.

Note that each region is defined by its limits, so that if the
region's buffer has changed, then the text used to identify the region
might no longer correspond to the text at the beginning of the
region.

If user option `icicle-add-buffer-name-flag' is non-nil, then each
completion candidate is annotated with the name of the region's
buffer, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the start and end points of the region and its length.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict.

You can use `S-delete' during completion to delete a region from
`icicle-region-alist'.

You can use `icicle-add-region' to define the list of regions,
`icicle-region-alist'."                 ; Doc string
  icicle-select-region-action           ; Function to perform the action
  "Select region: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((IGNORED--FOR-SIDE-EFFECT               (when icicle-region-auto-open-files-flag ; Bindings
                                             (icicle-region-open-all-files)))
   (icicle-candidate-help-fn               'icicle-region-help)
   (icicle-delete-candidate-object         'icicle-delete-region-from-alist)
   (icicle-list-nth-parts-join-string      "\t")
   (icicle-list-join-string                "\t")
   (icicle-list-end-string                 "")
   (icicle-list-use-nth-parts              '(1))
   (icicle-sort-function                   nil)
   (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p                  t)
   (icicle-whole-candidate-as-text-prop-p  t)
   (icicle-candidates-alist (icicle-region-add-buffers (icicle-region-sorted)))))

(defun icicle-select-region-action (reg-name)
  "Action function for `icicle-select-region'."
  (let* ((reg   (icicle-get-alist-candidate reg-name))
         (buf   (cadr reg))
         (file  (car (cddr reg))))
    (when (and (not (get-buffer buf)) file) ; If no buffer, try to open the file.  If no file, msg.
      (if (file-readable-p file) (find-file-noselect file) (message "No such file: `%s'" file)))
    (when (get-buffer buf)
      (pop-to-buffer buf) (raise-frame) (goto-char (cadr (cddr reg)))
      (push-mark (car (cddr (cddr reg))) 'nomsg 'activate)))
  (setq deactivate-mark  nil))

(defun icicle-region-sorted ()
  "`icicle-region-alist', sorted first by buffer name and then by tag."
  (sort (icicle-regions) (lambda (x1 x2)
                           (let ((buf1  (cadr x1))
                                 (buf2  (cadr x2)))
                             (or (string-lessp buf1 buf2)
                                 (and (string= buf1 buf2) (string-lessp (car x1) (car x2))))))))

(defun icicle-region-add-buffers (region-list)
  "Add buffer names to REGION-LIST, if `icicle-add-buffer-name-flag'."
  (if icicle-add-buffer-name-flag
      (mapcar (lambda (entry)
                (let ((buf  (copy-sequence (cadr entry))))
                  (put-text-property 0 (length buf) 'face 'icicle-candidate-part buf)
                  (cons (list (car entry) buf) (cdr entry))))
              region-list)
    region-list))

;;;###autoload
(icicle-define-command icicle-remove-region ; Command name
  "Remove a region from the list of regions, `icicle-region-alist'.
Update the persistent value of user option `icicle-region-alist'.

Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

To add a region to `icicle-region-alist', do one of the following:

* Use `\\[icicle-exchange-point-and-mark]' with a numeric prefix arg.
* Use command `icicle-add-region'.
* Customize `icicle-region-alist'."     ; Doc string
  icicle-delete-region-from-alist       ; Function to perform the action
  "Remove region from saved regions: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((icicle-candidate-help-fn              'icicle-region-help) ; Additional bindings
   (icicle-list-nth-parts-join-string     "\t")
   (icicle-list-join-string               "\t")
   (icicle-list-end-string                "")
   (icicle-list-use-nth-parts             '(1))
   (icicle-sort-function                  nil)
   (icicle-transform-function             (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p                 t)
   (icicle-whole-candidate-as-text-prop-p t)
   (icicle-candidates-alist               (icicle-region-add-buffers (icicle-region-sorted)))))

(defun icicle-delete-region-from-alist (reg-name)
  "Delete the region named REG-NAME from `icicle-region-alist'."
  (let ((alist-cand  (icicle-get-alist-candidate reg-name)))
    (setq icicle-region-alist
          (delete (cons (caar alist-cand) (cdr alist-cand)) icicle-region-alist)))
  (customize-save-variable 'icicle-region-alist icicle-region-alist))

;;;###autoload
(icicle-define-command icicle-remove-all-regions-in-buffer ; Command name
  "Remove all regions in a buffer from `icicle-region-alist'.
To remove individual regions, use command `icicle-remove-region'.
To add a region to `icicle-region-alist', use `icicle-add-region'.
Alternatively, you can customize `icicle-region-alist'." ; Doc string
  icicle-remove-all-regions-action      ; Function to perform the action
  "Buffer: "                            ; `completing-read' args
  (icicle-remove-duplicates (mapcar (lambda (reg) (list (cadr reg))) icicle-region-alist))
  nil t nil nil (buffer-name) nil
  ((icicle-use-candidates-only-once-flag  t))) ; Additional bindings

(defun icicle-remove-all-regions-action (buffer)
  "Action function for `icicle-remove-all-regions-in-buffer'.
Remove all regions in BUFFER from `icicle-region-alist'.
BUFFER is the name of a buffer."
  (dolist (reg icicle-region-alist)
    (when (string= buffer (cadr reg)) (setq icicle-region-alist  (delete reg icicle-region-alist))))
  (customize-save-variable 'icicle-region-alist icicle-region-alist)
  (message "Removed all regions in buffer `%s'" buffer))

;;;###autoload
(icicle-define-command icicle-search-region ; Command name
  "Search a region from the list of regions, `icicle-region-alist'.
Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

You can use `S-delete' during completion to delete a region from
`icicle-region-alist'.

You can use `icicle-add-region' to define the list of regions.
Regions in `icicles-regions' that are in buffers that do not currently
exist are ignored.

Note that each region is defined by its limits, so that if the
region's buffer has changed, then the text used to identify the region
might no longer correspond to the text at the beginning of the
region.

If user option `icicle-add-buffer-name-flag' is non-nil, then each
completion candidate is annotated with the name of the region's
buffer, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the start and end points of the region and its length.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict."               ; Doc string
  icicle-search-region-action           ; Function to perform the action
  "Search region: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((IGNORED--FOR-SIDE-EFFECT               (when icicle-region-auto-open-files-flag ; Bindings
                                             (icicle-region-open-all-files)))
   (icicle-candidate-help-fn               'icicle-region-help)
   (icicle-delete-candidate-object         'icicle-delete-region-from-alist)
   (enable-recursive-minibuffers           t)
   (regexp                                 (icicle-search-read-context-regexp))
   (icicle-list-nth-parts-join-string      "\t")
   (icicle-list-join-string                "\t")
   (icicle-list-end-string                 "")
   (icicle-list-use-nth-parts              '(1))
   (icicle-sort-function                   nil)
   (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p                  t)
   (icicle-whole-candidate-as-text-prop-p  t)
   (icicle-candidates-alist                (icicle-region-add-buffers (icicle-region-sorted)))))

;; Free var here: `regexp' is bound in `icicle-search-region'.
(defun icicle-search-region-action (reg-name)
  "Action function for `icicle-search-region'."
  (let* ((reg   (icicle-get-alist-candidate reg-name))
         (buf   (cadr reg))
         (file  (car (cddr reg))))
    (when (and (not (get-buffer buf)) file (file-readable-p file)) (find-file-noselect file))
    (pop-to-buffer buf) (raise-frame)
    (let ((icicle-show-Completions-initially-flag  t)
          (icicle-candidate-action-fn              'icicle-search-action)
          (icicle-candidates-alist                 icicle-candidates-alist))
      (icicle-search (cadr (cddr reg)) (car (cddr (cddr reg))) regexp t))
    (save-excursion (set-buffer (window-buffer (minibuffer-window))) (icicle-erase-minibuffer))))

(defun icicle-region-help (reg-name)
  "Use as `icicle-candidate-help-fn' for `icicle-region-alist' commands."
  (icicle-msg-maybe-in-minibuffer
   (let* ((reg    (icicle-get-alist-candidate reg-name))
          (file   (car (cddr reg)))
          (start  (cadr (cddr reg)))
          (end    (car (cddr (cddr reg)))))
     (or (concat (format "%d to %d" end start) " in buffer `" (cadr reg)
                 (and file (format "', file `%s" file)) (format "', %d chars" (- end start)))
         "No help"))))

(defun icicle-regions ()
  "Variable `icicle-region-alist', but without non-existent non-file buffers."
  (let ((unsorted-regions  (icicle-remove-if (lambda (reg) (and (not (car (cddr reg))) ; No file.
                                                                (not (get-buffer (cadr reg)))))
                                             icicle-region-alist)))
    (if icicle-sort-function
        (sort unsorted-regions (lambda (a b) (funcall icicle-sort-function (car a) (car b))))
      unsorted-regions)))

;;;###autoload
(defun icicle-search-generic ()         ; Bound to `C-x `'.
  "Run `icicle-search-command'.  By default, this is `icicle-search'.
In Compilation and Grep modes, this is `icicle-compilation-search'.
In Comint, Shell, GUD, and Inferior Lisp modes, this is
   `icicle-comint-search'."
  (interactive)
  (call-interactively icicle-search-command))

;;;###autoload
(defun icicle-search (beg end scan-fn-or-regexp require-match ; Bound to `C-c `'.
                      &optional where &rest args)
  "Search for matches, with completion, cycling, and hit replacement.
Interactively, search for regexp matches.  You are prompted for a
regexp, which you enter using `RET'.  The search hits (matches) are
available as completion candidates.  You can then use apropos
completion to filter the candidates using a different regexp, which
you can change dynamically (as always).  You can replace individual
matches with another string, as in `query-replace' or
`query-replace-regexp'.  Candidates appear in order of buffer
occurrence; you cannot sort them.

Non-interactively, search can be for regexp matches or any other kind
of matches.  Argument SCAN-FN-OR-REGEXP is the regexp to match, or it
is a function that defines an alist of buffer zones to search.  You
can navigate among the matching buffer zones (defined either as regexp
matches or via function), called search \"contexts\", and you can
match another regexp against the text in a search context.  See the
end of this description for information about the other arguments.

If the search-context regexp contains regexp subgroups, that is,
subexpressions of the form `\(...\)', then you are prompted for the
subgroup to use to define the search contexts.  Subgroup 0 means the
context is whatever matches the whole regexp.  Subgroup 1 means the
context is whatever matches the first subgroup, and so on.  The
subgroup number is the number of occurrences of `\(', starting at the
beginning of the regexp.

You can further limit the set of search contexts by setting user
option `icicle-search-context-match-predicate' to a predicate that
takes a search-context (string) argument.  Only contexts that satisfy
the predicate are used.  For example, if the predicate is (lambda (x)
\(commandp (intern-soft x))), then only contexts that name commands
are kept.

Search respects `icicle-regexp-quote-flag' and
`icicle-search-whole-word-flag'.  You can toggle these during search,
by using `C-`' and `M-q', respectively.  If `icicle-regexp-quote-flag'
is non-nil, then regexp special characters are quoted, so that they
become non-special.  If `icicle-search-whole-word-flag' is non-nil,
then whole-word searching is done.  During word search, all characters
in the search string you type are treated as if they were word
constituents: the search string is matched literally, but only at word
boundaries.  (You can also use `\\[icicle-search-word]' to perform
word search.)

Optional Behaviors: Prefix Argument
-----------------------------------

By default, search only the current buffer.  Search the active region,
or, if there is none, then search the entire buffer.

With a prefix argument, you can search multiple buffers, files, or
regions, as follows:

- With a simple prefix arg (`C-u'), search all of the regions in
`icicle-region-alist'.  Those regions can be in any buffers.  If a
region is in a buffer that does not exist, it is skipped.  You can
always re-create the buffer (e.g. visit the file), and try again.

Note: To search selected regions in `icicle-region-alist'
individually, use multi-command `icicle-search-region'.

- With a non-negative numeric prefix arg, search multiple buffers
completely.  You are prompted for the buffers to search - all of each
buffer is searched.  Any existing buffers can be chosen.  If the
prefix arg is 99, then only buffers visiting files are candidates.

- With a negative numeric prefix arg, search multiple files
completely.  You are prompted for the files to search - all of each
file is searched.  Any existing files in the current directory can be
chosen.

Navigation and Help
-------------------

The use of completion for this command is special.  It is not unusual
in this context to have multiple completion candidates that are
identical - only the positions of their occurrences in the search
buffer(s) differ.  In that case, you cannot choose one simply by
completing it in the minibuffer, because the destination would be
ambiguous.  That is, simply completing your input and entering the
completion with `RET' will not take you to its occurrence in the
search buffer, unless it is unique.

Instead, choose search hits to visit using any of the candidate-action
keys: `C-RET', `C-mouse-2', `C-next', `C-prior', `C-down', and `C-up'.
The last four of these cycle among the search hits.  The current
candidate in *Completions* corresponds to the current location
visited (it is not off by one, as is usually the case in Icicles).

As always, the `C-M-' keys provide help on individual candidates:
`C-M-RET', `C-M-mouse-2', `C-M-next', `C-M-prior', `C-M-down', and
`C-M-up'.  For `icicle-search', they indicate the buffer and position
of the search hit.

You can cycle among candidates without moving to their occurrences in
the search buffer, using `next', `prior', `down', and `up' (no `C-').

Highlighting
------------

In the search buffer (that is, where the hits are), `icicle-search'
does the following:

- Highlights the current match (buffer zone) for the initial (context)
  regexp, using face `icicle-search-main-regexp-current'.

- Highlights the first `icicle-search-highlight-threshold' context
  matches, using face `icicle-search-main-regexp-others'.

- Highlights 1-8 context levels, within each search context.  This
  happens only if your initial (context) regexp has \\(...\\) groups
  and option `icicle-search-highlight-context-levels-flag' is non-nil.

- Highlights the match for your current input, using face
  `icicle-search-current-input'.  Highlights all such matches if
  option `icicle-search-highlight-all-current-flag' is non-nil;
  otherwise, highlights just the currently visited match.
  You can toggle this option using `C-^'.

If user option `icicle-search-cleanup-flag' is non-nil (the default),
then all search highlighting is removed from the search buffer when
you are finished searching.  If it is nil, then you can remove this
highlighting later using command `icicle-search-highlight-cleanup'.
You can toggle `icicle-search-cleanup-flag' during Icicles search
using `C-.'  in the minibuffer.

Search and Replace
------------------

You can replace the current search match by using any of the
alternative action keys: `C-S-RET', `C-S-mouse-2' (in *Completions*),
`C-S-next', `C-S-prior', `C-S-down', and `C-S-up'.  You can use `C-|'
to replace all matches at once.

At the first use of any of these, you are prompted for the replacement
string; it is used thereafter, or until you use `M-,'
\(`icicle-search-define-replacement').  The replacement string can be
anything that is allowed as a replacement by `query-replace-regexp'.
In Emacs 22 or later, this includes Lisp sexp evaluation via `\,'.

Unlike `query-replace', you need not visit each search match - you can
visit and replace selected matches in any order.

What is meant here by a \"search match\"?  It can be either an entire
search context or just whatever your current minibuffer input matches.

Key `C-,' toggles option `icicle-search-replace-whole-candidate-flag'.
By default, the entire current search context is replaced, that is,
whatever matches the context regexp that you entered initially, using
`RET'.  However, you can use `C-,' at any time during searching to
toggle between this default behavior and replacement of whatever your
current minibuffer input matches.

Remember this:

 - If `icicle-search-replace-whole-candidate-flag' is non-nil, then
   the granularity of replacement is a complete search context.  In
   this case, replacement behaves similarly to `query-replace-regexp'.
   You can still use minibuffer input to filter the set of search
   contexts, but replacement is on a whole-context basis.

 - If `icicle-search-replace-whole-candidate-flag' is nil, then you
   can replace multiple input matches separately within a search
   context.  This behavior is unique to Icicles.

If `icicle-search-replace-whole-candidate-flag' is non-nil, then you
can use the navigational alternative action keys, `C-S-next',
`C-S-prior', `C-S-down', and `C-S-up', to replace successive search
contexts.

Search traversal using these keys is always by search context, not by
input match.  This means that you cannot use these keys to replace
individual input matches within a search context, except for the first
such match.  That is, if `icicle-search-replace-whole-candidate-flag'
is nil and you use these keys, then only the first match of your input
in each search context is replaced.

If your input matches multiple parts of the search context, and you
want to replace them in order, then use `C-S-RET' repeatedly.  This
replaces successive input matches within a search context, then moves
on to the next context, and so on.  You can traverse all matches of
your input in the order they appear in the buffer by repeating
`C-S-RET'.

Repeated use of `C-S-RET' is generally for the case where you are
replacing input matches, not whole search contexts.  If you repeat
`C-S-RET' when `icicle-search-replace-whole-candidate-flag' is
non-nil, then you will, in effect, just replace the same context over
and over - unless, that is, your current input does not match the
replacement text.  In that case, the replacement is no longer a
matching search context (candidate), and `C-S-RET' moves on to the
next context.

What your input matches depends on a few Icicles options:

 - `icicle-regexp-quote-flag' determines whether to use regexp
   matching or literal matching.

 - `icicle-search-highlight-all-current-flag',
   `icicle-expand-input-to-common-match-flag' and
   `icicle-search-replace-common-match-flag' together determine
   whether to replace exactly what your input matches in the current
   search hit or the expanded common match (ECM) of your input among
   all search hits.  If any of these options is nil, then your exact
   input match is replaced; otherwise, the ECM is replaced.

Using Regexps
-------------

At any time, you can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' (command
`icicle-insert-string-from-variable') to insert text (e.g. a regexp)
from a variable into the minibuffer.  For example, you can search for
ends of sentences by using `C-u \\[icicle-insert-string-from-variable]' and choosing variable
`sentence-end' as the variable.  And you can use
`\\[icicle-save-string-to-variable]' to save a string to a variable
for later use by `\\[icicle-insert-string-from-variable]'.

When employed with useful regexps, `C-=' can turn `icicle-search' into
a general navigator or browser of code, mail messages, and many other
types of buffer.  Imenu regexps work fine, for example - command
`icicle-imenu' simply uses `icicle-search' this way.  See
`icicle-insert-string-from-variable' for more tips on inserting
regexps from variables.

Additional Information
----------------------

If user option `icicle-add-buffer-name-flag' is non-nil, then each
candidate is annotated with the name of the buffer where the hit
occurs, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the position of the hit in the buffer.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict.

After you visit a completion candidate, the hooks in variable
`icicle-search-hook' are run.

`icicle-search' overrides `icicle-ignore-space-prefix-flag', binding
it to nil, so that candidates with initial spaces can be matched.

`icicle-search' sets `icicle-search-final-choice' to the final user
choice, which might not be one of the search candidates if
REQUIRE-MATCH is nil.

Non-Interactive Use
-------------------

When called non-interactively, these are the `icicle-search'
arguments:

BEG is the beginning of the region to search; END is the end.
SCAN-FN-OR-REGEXP: Regexp or function that determines the set of
  initial candidates (match zones).  If a function, it is passed, as
  arguments, the buffer to search, the beginning and end of the search
  region in that buffer, and ARGS.
REQUIRE-MATCH is passed to `completing-read'.
Optional arg WHERE is either a list of buffers or a list of region
  entries that have the same form as `icicle-region-alist'.  If nil,
  then only the current buffer is used.
ARGS are arguments that are passed to function SCAN-FN-OR-REGEXP.

Note that if SCAN-FN-OR-REGEXP is a regexp string, then function
`icicle-search-regexp-scan' is used to determine the set of match
zones.  You can limit hits to regexp matches that also satisfy a
predicate, by using `(PREDICATE)' as ARGS: PREDICATE is then passed to
`icicle-search-regexp-scan' as its PREDICATE argument.

This command is intended for use only in Icicle mode."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (setq icicle-search-context-regexp  (and (stringp scan-fn-or-regexp) scan-fn-or-regexp))
  (let ((icicle-candidate-action-fn         (or icicle-candidate-action-fn 'icicle-search-action))
        (icicle-candidate-help-fn           'icicle-search-help)
        (icicle-candidate-alt-action-fn     (or icicle-candidate-alt-action-fn
                                                'icicle-search-replace-search-hit))
        (icicle-update-input-hook           (list 'icicle-search-highlight-all-input-matches))
        (icicle-search-ecm                  nil)
        (icicle-list-nth-parts-join-string  "\t")
        (icicle-list-join-string            "\t")
        (icicle-list-end-string             "")
        (icicle-list-use-nth-parts          '(1))
        (icicle-sort-function               nil)
        (icicle-inhibit-sort-p              t)
        (completion-ignore-case             case-fold-search))
    (icicle-explore (lambda () (icicle-search-define-candidates beg end scan-fn-or-regexp
                                                                require-match where args))
                    #'icicle-search-final-act #'icicle-search-quit-or-error
                    #'icicle-search-quit-or-error #'icicle-search-cleanup
                    "Choose an occurrence: " nil require-match nil 'icicle-search-history)))

;; This is the same as `region-or-buffer-limits' in `misc-fns.el'.
(defun icicle-region-or-buffer-limits ()
    "Return the start and end of the region as a list, smallest first.
If the region is not active or is empty, then use bob and eob."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

(defun icicle-search-read-context-regexp (&optional prompt pred init hist def i-i-m)
  "Read context regexp and determine `icicle-search-context-level'.
The regexp is read with completion against previous regexp input.
The arguments are for use by `completing-read' to read the regexp.
 HIST (or `regexp-history' if HIST is nil) is used for the
 `completing-read' TABLE argument.  The REQUIRE-MATCH arg is nil.
 A default prompt is used if PROMPT is nil."
  (setq hist    (or hist 'regexp-history)
        prompt  (or prompt "Find (regexp): "))
  (let ((regexp  (icicle-completing-read-history prompt 'regexp-history pred init def i-i-m)))
    (while (string= "" regexp)
      (message "Regexp cannot be empty.  Try again...") (sit-for 2)
      (setq regexp  (icicle-completing-read-history prompt 'regexp-history pred init def i-i-m)))
    (setq prompt                       "Subgroup to use as search context [0, 1, 2,...]: "
          icicle-search-context-level  (if (string-match "\\\\(" regexp)
                                           (truncate (if (fboundp 'read-number)
                                                         (read-number prompt 0)
                                                       (read-from-minibuffer ; Hope for a number.
                                                        prompt nil nil nil nil 0)))
                                         0))
    regexp))

(defun icicle-search-where-arg ()
  "Return WHERE arg for `icicle-search*' commands, based on prefix arg."
  (cond ((consp current-prefix-arg)
         (message "Searching saved regions") (sit-for 1) icicle-region-alist)
        ((wholenump current-prefix-arg)
         (icicle-search-choose-buffers (= 99 (prefix-numeric-value current-prefix-arg))))
        (current-prefix-arg
         (message "Searching multiple files") (sit-for 1)
         (let ((icicle-show-Completions-initially-flag  t))
           (save-selected-window (icicle-file-list))))
        (t nil)))

(defun icicle-search-choose-buffers (files-only-p)
  "Choose multiple buffers to search.
FILES-ONLY-P non-nil means that only buffers visiting files are
candidates."
  (message "Searching multiple buffers") (sit-for 1)
  (let ((icicle-show-Completions-initially-flag  t))
    (mapcar #'get-buffer (let ((icicle-buffer-require-match-flag  'partial-match-ok)
                               (current-prefix-arg                files-only-p))
                           (save-selected-window (icicle-buffer-list))))))

(defun icicle-search-read-word ()
  "Read a word to search for (whole-word search).
Regexp special characters within the word are escaped (quoted)."
  (setq icicle-search-context-level  0)
  (concat "\\b"
          (regexp-quote (icicle-completing-read-history "Search for whole word: "
                                                        'icicle-search-history))
          "\\b"))

(defun icicle-search-final-act ()
  "Go to the final search hit choice, then run `icicle-search-hook'.
The hit's frame is raised and selected."
  (let* ((marker  (cdr icicle-explore-final-choice-full))
         (buf     (marker-buffer marker)))
    (unless (bufferp buf) (error "No such buffer: %s" buf))
    (pop-to-buffer buf)
    (raise-frame)
    (goto-char (marker-position marker))
    (select-frame-set-input-focus (selected-frame))
    (run-hooks 'icicle-search-hook)))

;; Free vars here: `orig-pt-explore', `orig-win-explore' are bound in `icicle-explore'.
(defun icicle-search-quit-or-error ()
  "Return to the starting point."
  (when (window-live-p orig-win-explore)
    (select-window orig-win-explore)
    (goto-char orig-pt-explore)))

;; Free vars here: `orig-win-explore' is bound in `icicle-explore'.
(defun icicle-search-cleanup ()
  "Clean up search highlighting, if `icicle-search-highlight-cleanup'.
Select original window."
  (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
  (when (window-live-p orig-win-explore)
    (select-window orig-win-explore)
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-search-define-candidates (beg end scan-fn-or-regexp require-match where args)
  "Define completion candidates for `icicle-search'.
The arguments are the same as for `icicle-search'."
  (when (and icicle-regexp-quote-flag
             (not icicle-search-whole-word-flag)
             (stringp scan-fn-or-regexp))
    (setq scan-fn-or-regexp  (regexp-quote scan-fn-or-regexp)))
  (cond ((and (consp where) (bufferp (car where))) ; List of buffers - search buffers.
         (dolist (buf where)
           (icicle-search-define-candidates-1 buf nil nil scan-fn-or-regexp args)))
        ((and (consp where)           ; List of files - search files.
              (stringp (car where))
              (file-exists-p (car where)))
         (dolist (file where)
           (icicle-search-define-candidates-1 (find-file-noselect file 'nowarn) nil nil
                                              scan-fn-or-regexp args)))
        ((consp where)                ; Search all regions in `icicle-region-alist'.
         (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))
         (let ((non-existent-buffers  ()))
           (dolist (reg where)
             (if (bufferp (get-buffer (cadr reg)))
                 (icicle-search-define-candidates-1
                  (get-buffer (cadr reg)) (cadr (cddr reg))
                  (car (cddr (cddr reg))) scan-fn-or-regexp args)
               (push (cadr reg) non-existent-buffers)))
           (when non-existent-buffers
             (message "Skipping regions in non-existent buffers: `%s'"
                      (mapconcat #'identity (icicle-remove-duplicates non-existent-buffers)
                                 "', `"))
             (sit-for 3))))
        (t                            ; Search this buffer only.
         (icicle-search-define-candidates-1 nil beg end scan-fn-or-regexp args))))

(defun icicle-search-define-candidates-1 (buffer beg end scan-fn-or-regexp args)
  "Helper function for `icicle-search-define-candidates'.
BUFFER is a buffer to scan for candidates.
The other arguments are the same as for `icicle-search'."
  (if (functionp scan-fn-or-regexp)
      (apply scan-fn-or-regexp buffer beg end args)
    (apply 'icicle-search-regexp-scan buffer beg end scan-fn-or-regexp args)))

(defun icicle-search-regexp-scan (buffer beg end regexp &optional predicate)
  "Scan BUFFER for REGEXP, pushing hits onto `icicle-candidates-alist'.
PREDICATE is nil or a boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context
If PREDICATE is non-nil, then push only the hits for which it holds.
If REGEXP has subgroups, then what the Nth subgroup matches is used as
the search context (hit), where N = `icicle-search-context-level'.  If
N=0, then the overall match of REGEXP is used as the search context.

If BUFFER is nil, scan the current buffer.
Highlight the matches in face `icicle-search-main-regexp-others'.
If BEG and END are non-nil, scan only between positions BEG and END."
  (setq regexp  (or regexp (icicle-search-read-context-regexp)))
  (let ((add-bufname-p  (and buffer icicle-add-buffer-name-flag))
        (temp-list      ())
        (last-beg       nil))
    (unless buffer (setq buffer  (current-buffer)))
    (when (bufferp buffer)
      (set-buffer buffer)
      (unless (and beg end)
        (setq beg  (point-min)
              end  (point-max)))
      (condition-case icicle-search-regexp-scan
          (save-excursion
            (goto-char (setq last-beg  beg))
            (while (and beg (< beg end) (not (eobp)))
              (while (and (setq beg  (re-search-forward regexp end t))
                          (eq last-beg beg)
                          (not (eobp)))
                (forward-char) (setq beg  (1+ beg))) ; Matched again, same place.  Advance 1 char.
              (when (and beg (not (eobp)))
                (unless (match-beginning icicle-search-context-level)
                  (error "Search context has no subgroup of level %d - try a lower number"
                         icicle-search-context-level))
                (let* ((hit-string  (buffer-substring-no-properties
                                     (match-beginning icicle-search-context-level)
                                     (match-end icicle-search-context-level)))
                       (end-marker  (copy-marker (match-end icicle-search-context-level))))
                  (when (and (not (string= "" hit-string))
                             (or (not predicate)
                                 (save-match-data (funcall predicate hit-string end-marker))))
                    ;; Add whole candidate to `temp-list'.  Whole candidate is
                    ;; (`hit-string' . `end-marker') or ((`hit-string' BUFNAME) . `end-marker').
                    (push (cons (if add-bufname-p
                                    (list hit-string
                                          (let ((string  (copy-sequence (buffer-name))))
                                            (put-text-property 0 (length string) 'face
                                                               'icicle-candidate-part string)
                                            string))
                                  hit-string)
                                end-marker)
                          temp-list)
                    ;; Highlight search context in buffer.
                    (when (<= (+ (length temp-list) (length icicle-candidates-alist))
                              icicle-search-highlight-threshold)
                      (let ((ov  (make-overlay (match-beginning icicle-search-context-level)
                                               (match-end icicle-search-context-level))))
                        (push ov icicle-search-overlays)
                        (overlay-put ov 'priority 200) ; > ediff's 100+, < isearch-overlay's 1001.
                        (overlay-put ov 'face 'icicle-search-main-regexp-others))))))
              (setq last-beg  beg))
            (setq icicle-candidates-alist  (append icicle-candidates-alist (nreverse temp-list))))
        (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
        (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
               (error (error-message-string icicle-search-regexp-scan)))))))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-all-input-matches (&optional input)
  "Highlight, inside each search context, what the current input matches."
  (save-excursion
    ;; Update by deleting (if it exists) and then creating.
    ;; If a single overlay exists, it means that the user just changed
    ;; `icicle-search-highlight-threshold' to non-zero.
    ;; Otherwise, it's nil or a list of overlays.
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays))))
  (when icicle-search-highlight-all-current-flag
    (setq input  (or input icicle-current-input))
    (unless (or (string= "" input) (null icicle-search-overlays))
      (let ((hits  ()))
        (save-excursion
          (dolist (ov icicle-search-overlays)
            (set-buffer (overlay-buffer ov))
            (save-restriction             ; Search within the current search context.
              (narrow-to-region (overlay-start ov) (overlay-end ov))
              (goto-char (point-min))
              (when (condition-case nil (re-search-forward input nil 'move-to-end) (error nil))
                (push (buffer-substring-no-properties (point-min) (point-max)) hits))))
          (when (and icicle-expand-input-to-common-match-flag  hits)
            (setq icicle-search-ecm  (icicle-expanded-common-match input hits)))
          (dolist (ov icicle-search-overlays)
            (set-buffer (overlay-buffer ov))
            (save-restriction             ; Search within the current search context.
              (narrow-to-region (overlay-start ov) (overlay-end ov))
              (when (member (buffer-substring-no-properties (point-min) (point-max)) hits)
                (goto-char (point-min))
                (save-match-data
                  (while (condition-case nil
                             (re-search-forward (or icicle-search-ecm input) nil 'move-to-end)
                           (error nil))
                    (setq ov  (make-overlay (match-beginning 0) (match-end 0)))
                    (push ov icicle-search-refined-overlays)
                    (overlay-put ov 'priority 220)
                    (overlay-put ov 'face 'icicle-search-current-input)))))))))))

(defun icicle-search-replace-search-hit (candidate)
  "Replace search hit CANDIDATE with `icicle-search-replacement'."
  (let ((icicle-candidate-nb             icicle-candidate-nb)
        (icicle-completion-candidates    icicle-completion-candidates)
        (icicle-last-completion-command  icicle-last-completion-command)
        (icicle-last-input               icicle-last-input)
        (compl-win                       (get-buffer-window "*Completions*" 0)))
    (unless icicle-search-replacement
      (icicle-search-define-replacement)
      (when (and compl-win icicle-completion-candidates)
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list icicle-completion-candidates)))))
  (setq icicle-candidate-nb  (or icicle-candidate-nb 0)) ; Replace-all has nil, so use 0.
  (funcall icicle-candidate-action-fn candidate icicle-search-replacement))

;; Free vars here: `orig-win-explore' is bound in `icicle-explore'.
(defun icicle-search-action (string &optional replace-string)
  "Completion action function for command `icicle-search'.
1. Move to the regexp match in the original buffer and highlight it.
2. If `icicle-search-highlight-threshold' is zero, highlight what the
   current input matches, inside the match of the initial regexp.
3. If REPLACE-STRING is non-nil, replace current candidate with it.
   If `icicle-search-replace-whole-candidate-flag' is nil, replace
   only the candidate part that matches the current input.
4. Highlight the current candidate in *Completions*.

   Note: The replacement can be anything allowed as a replacement by
   `query-replace-regexp', including Lisp-evaluation
   constructs (`\,...')."
  (prog1
      (condition-case icicle-search-action
          (progn
            ;; Move cursor to the match in the original buffer and highlight it.
            (let* ((cand+mrker  (icicle-get-alist-candidate string))
                   ;; $$$$$$$$
                   ;; (cand+mrker (elt (icicle-filter-alist icicle-candidates-alist
                   ;;                                       icicle-completion-candidates)
                   ;;                  icicle-candidate-nb))
                   (candidate   (if (consp (car-safe cand+mrker))
                                    (car-safe (car-safe cand+mrker))
                                  (car-safe cand+mrker)))
                   (marker      (cdr-safe cand+mrker))
                   (icicle-search-in-context-fn
                    (or icicle-search-in-context-fn 'icicle-search-in-context-default-fn)))
              (unless marker (error "No such occurrence"))
              (save-selected-window
                (when (window-live-p orig-win-explore) (select-window orig-win-explore))
                (let ((completion-ignore-case  case-fold-search)
                      (buf                     (marker-buffer marker))
                      (icicle-candidate-nb     icicle-candidate-nb)) ; Save and restore this.
                  (unless (bufferp buf) (error "No such buffer: %s" buf))
                  (pop-to-buffer buf)
                  (raise-frame)
                  (goto-char marker)
                  ;; Highlight current search context using `icicle-search-main-regexp-current'.
                  (icicle-place-overlay (- marker (length candidate)) marker
                                        'icicle-search-current-overlay
                                        'icicle-search-main-regexp-current
                                        202 buf)
                  (funcall icicle-search-in-context-fn cand+mrker replace-string)
                  (icicle-highlight-candidate-in-Completions)
                  (run-hooks 'icicle-search-hook)))
              nil))                     ; Return nil for success.
        (error (message (error-message-string icicle-search-action))
               (error-message-string icicle-search-action))) ; Return error message.
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-search-in-context-default-fn (cand+mrker replace-string)
  "Default value of `icicle-search-in-context-fn'."
  (let ((candidate  (if (consp (car-safe cand+mrker))
                        (car-safe (car-safe cand+mrker))
                      (car-safe cand+mrker)))
        (marker     (cdr-safe cand+mrker)))
    (save-excursion (save-restriction   ; Search within the current search context.
                      (narrow-to-region (- marker (length candidate)) marker)
                      (icicle-search-highlight-and-maybe-replace cand+mrker replace-string))))
  (let ((icicle-candidate-nb  icicle-candidate-nb)) (icicle-complete-again-update)))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-and-maybe-replace (cand+mrker replace-string)
  "Highlight within search context and replace using REPLACE-STRING.
If REPLACE-STRING is nil, no replacement occurs.
Arguments are the same as for `icicle-search-in-context-fn'."
  (icicle-search-highlight-context-levels)
  (icicle-search-highlight-input-matches-here)
  (when replace-string
    (goto-char (point-min))
    (cond (icicle-search-replace-whole-candidate-flag
           (let ((candidate  (if (consp (car-safe cand+mrker))
                                 (car-safe (car-safe cand+mrker))
                               (car-safe cand+mrker))))
             (cond ((string= candidate replace-string) ; Sanity check only.
                    (save-restriction (widen) (message
                                               "Replacement = candidate, and \
current input matches candidate") (sit-for 2)))
                   (t
                    (set-match-data (list (point-min) (point-max)))
                    (icicle-search-replace-match replace-string
                                                 (icicle-search-replace-fixed-case-p
                                                  icicle-search-context-regexp))))))
          (t
           (save-match-data
             (let ((first-p  t)
                   (ecm      (and icicle-search-replace-common-match-flag icicle-search-ecm)))
               (while (and (re-search-forward (or ecm icicle-current-input) nil 'move-to-end)
                           (or first-p icicle-all-candidates-action-p))
                 (setq first-p  nil)
                 (icicle-search-replace-match replace-string
                                              (icicle-search-replace-fixed-case-p
                                               icicle-current-input)))))))
    (icicle-search-replace-candidate cand+mrker (buffer-substring (point-min) (point-max)))
    (save-selected-window (select-window (minibuffer-window)) (icicle-retrieve-last-input))
    (when (or (not icicle-candidate-nb) ; Just in case - should never be nil.
              (>= icicle-candidate-nb (length icicle-completion-candidates)))
      (setq icicle-candidate-nb  0))
    (setq icicle-last-completion-candidate  (buffer-substring (point-min) (point-max)))
    (icicle-highlight-candidate-in-Completions)
    (icicle-search-highlight-context-levels)
    (icicle-search-highlight-input-matches-here)))

(defun icicle-search-replace-match (replace-string fixedcase)
  "Replace current match with REPLACE-STRING, interpreting escapes.
Treat REPLACE-STRING as it would be by `query-replace-regexp'.
FIXEDCASE is as for `replace-match'.  Non-nil means do not alter case."
  (if (fboundp 'query-replace-compile-replacement) ; Emacs 22.
      (let ((compiled
             (save-match-data
               (query-replace-compile-replacement replace-string
                                                  (not icicle-search-replace-literally-flag)))))
        (condition-case icicle-search-replace-match1
            (let ((enable-recursive-minibuffers    t) ; So we can read input from \?.
                  ;; Save and restore these, because we might read input from \?.
                  (icicle-last-completion-command  icicle-last-completion-command)
                  (icicle-last-input               icicle-last-input))
              (replace-match-maybe-edit
               (if (consp compiled)
                   (funcall (car compiled) (cdr compiled) (setq replace-count  (1+ replace-count)))
                 compiled)
               fixedcase icicle-search-replace-literally-flag nil (match-data)))
          (error (icicle-remove-Completions-window) (error "No match for %s" replace-string))))
    (condition-case icicle-search-replace-match2 ; Emacs < 22.  Try to interpret `\'.
        (replace-match replace-string fixedcase icicle-search-replace-literally-flag)
      (error (replace-match replace-string fixedcase t))))) ;   If error, replace literally.

(defun icicle-search-highlight-context-levels ()
  "Highlight context levels differently (up to 8 levels).
No such highlighting is done if any of these conditions holds:
 * `icicle-search-context-level' is not 0 (search context < regexp).
 * `icicle-search-highlight-context-levels-flag' is nil.
 * `icicle-search-context-regexp' is nil (non-regexp searching)."
  (unless (or (/= icicle-search-context-level 0)
              (not icicle-search-highlight-context-levels-flag)
              (not icicle-search-context-regexp)) ; E.g. text-property searching
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays  (cdr icicle-search-level-overlays)))
    (save-match-data
      (let ((level       1)
            (max-levels  (min (regexp-opt-depth icicle-search-context-regexp) 8)))
        (goto-char (point-min))
        (re-search-forward icicle-search-context-regexp nil t)
        (condition-case nil
            (while (<= level max-levels)
              (let ((ov  (make-overlay (match-beginning level) (match-end level))))
                (push ov icicle-search-level-overlays)
                (overlay-put ov 'priority (+ 205 level)) ; > ediff's 100+, < isearch-overlay's 1001.
                (overlay-put ov 'face (intern (concat "icicle-search-context-level-"
                                                      (number-to-string level)))))
              (setq level  (1+ level)))
          (error nil))))))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-input-matches-here ()
  "Highlight all input matches in the current search context."
  (unless (or (> 0 icicle-search-highlight-threshold) (string= "" icicle-current-input))
    (goto-char (point-min))
    (when (and (not icicle-search-highlight-all-current-flag)
               (overlayp icicle-search-refined-overlays))
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  nil))
    (unless icicle-search-highlight-all-current-flag
      (while icicle-search-refined-overlays
        (delete-overlay (car icicle-search-refined-overlays))
        (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays))))
    (let ((ov  nil))
      (save-match-data
        (while (and (not (eobp)) (re-search-forward (or icicle-search-ecm icicle-current-input)
                                                    nil 'move-to-end))
          (setq ov  (make-overlay (match-beginning 0) (match-end 0)))
          (push ov icicle-search-refined-overlays)
          (overlay-put ov 'priority 220) ; Greater than any possible context-level priority (213).
          (overlay-put ov 'face 'icicle-search-current-input))))))

(defun icicle-search-replace-fixed-case-p (from)
  "Return non-nil if FROM should be replaced without transferring case.
FROM is a string or nil.  If FROM is nil, then return nil."
  (and from (not (and case-fold-search case-replace (string= from (downcase from))))))

(defun icicle-search-replace-candidate (cand+mrker new-cand)
  "In `icicle-candidates-alist', replace car of CAND+MRKER with NEW-CAND."
  (let ((newlist  icicle-candidates-alist))
    (while newlist
      (when (equal (car newlist) cand+mrker) (setcar newlist (cons new-cand (cdr-safe cand+mrker))))
      (setq newlist  (cdr newlist)))
  icicle-candidates-alist))

(defun icicle-search-help (cand)
  "Use as `icicle-candidate-help-fn' for `icicle-search' commands."
  (icicle-msg-maybe-in-minibuffer
   (let ((marker  (cdr (icicle-get-alist-candidate cand))))
     (or (concat "Buffer: `" (buffer-name (marker-buffer marker))
                 (format "', Position: %d" (marker-position marker)))
         "No help"))))

;;;###autoload
(defun icicle-search-keywords (beg end keywords require-match ; Bound to `C-c ^'.
                               &optional where &rest args)
  "Search with one or more keywords, which can each be a regexp.
Text that matches *any* of the keywords is found.

You can use completion to choose one or more previously entered
regexps (using `C-RET', `C-mouse-2', `C-next', and so on), or you can
enter new keywords (using `C-RET').  Use `RET' or `mouse-2' to choose
the last keyword.

Keywords are interpreted as regexps.  You can change to substring
completion instead, matching regexp special characters literally, by
using `C-`' during completion to toggle `icicle-regexp-quote-flag'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation."
  (interactive
   `(,@(icicle-region-or-buffer-limits)
     ,(icicle-group-regexp (mapconcat #'icicle-group-regexp (icicle-keyword-list) "\\|"))
     ,(not icicle-add-buffer-name-flag)
     ,(icicle-search-where-arg)))
  (icicle-search beg end keywords (not icicle-add-buffer-name-flag) where))

(defalias 'icicle-regexp-list 'icicle-keyword-list)
;;;###autoload
(icicle-define-command icicle-keyword-list ; Command name
  "Choose a list of keywords. The list of keywords (strings) is returned.
You can choose from keywords entered previously or enter new keywords
using `C-RET'.  Each keyword is a regexp.  The regexps are OR'd, and
the resulting regexp is used for `icicle-search'." ; Doc string
  (lambda (name)                        ; Function to perform the action
    (push name keywords)
    (message "Added keyword `%s'" name))
  "Choose keyword (regexp) (`RET' when done): " ; `completing-read' args
  (mapcar #'list (icicle-remove-duplicates regexp-history)) nil nil nil 'regexp-history nil nil
  ((keywords                              nil) ; Additional bindings
   (icicle-use-candidates-only-once-flag  t))
  nil nil                               ; First code, undo code
  (prog1 (setq keywords  (nreverse (delete "" keywords))) ; Last code - return the list of keywords.
    (when (interactive-p) (message "Keywords (regexps): %S" keywords))))

(defun icicle-group-regexp (regexp)
  "Wrap REGEXP between regexp parens, as a regexp group."
  (concat "\\(" regexp "\\)"))

;;;###autoload
(defun icicle-search-char-property (beg end require-match ; Bound to `C-c "'.
                                    &optional where prop values predicate)
  "Search for text that has a character property with a certain value.
If the property is `face' or `font-lock-face', then you can pick
multiple faces, using completion.  Text is then searched that has a
face property that includes any of the selected faces.  If you choose
no face (empty input), then text with any face is found.

By \"character property\" is meant either an overlay property or a
text property.  If you want to search for only an overlay property or
only a text property, then use `icicle-search-overlay-property' or
`icicle-search-text-property' instead.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation.

Non-interactively, arguments BEG, END, REQUIRE-MATCH, and WHERE are as
for `icicle-search'.  Arguments PROP, VALUES, and PREDICATE are passed
to `icicle-search-char-property-scan' to define the search contexts."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values nil
                 predicate))

;;;###autoload
(defun icicle-search-overlay-property (beg end require-match &optional where prop values predicate)
  "Same as `icicle-search-char-property', except only overlay property.
That is, do not also search a text property."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values 'overlay
                 predicate))

;;;###autoload
(defun icicle-search-text-property (beg end require-match ; Bound to `C-c "'.
                                    &optional where prop values predicate)
  "Same as `icicle-search-char-property', except only text property.
That is, do not also search an overlay property."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values 'text
                 predicate))

(defun icicle-search-property-args ()
  "Read and return interactive arguments for `icicle-search-*-property'."
  (let* ((where    (icicle-search-where-arg))
         (beg+end  (icicle-region-or-buffer-limits))
         (beg1     (car beg+end))
         (end1     (cadr beg+end))
         (props    (mapcar (lambda (prop) (list (symbol-name prop)))
                           (icicle-char-properties-in-buffers where beg1 end1)))
         (prop     (intern (completing-read "Property to search: " props nil nil nil nil "face")))
         (values   (if (memq prop '(face font-lock-face))
                       (let ((faces  (icicle-face-list)))
                         (if faces (mapcar #'intern faces) (face-list))) ; Default: all faces.
                     (list (intern (icicle-completing-read-history
                                    "Property value: " 'icicle-char-property-value-history))))))
    `(,beg1 ,end1 ,(not icicle-add-buffer-name-flag) ,where ,prop ,values)))

(defun icicle-char-properties-in-buffers (where beg end &optional type)
  "List of all character properties in WHERE.
Only the character properties are included, not their values.
WHERE is a list of buffers, a list of files, or a list of region
  entries that have the same form as `icicle-region-alist'.  If nil,
  then only the current buffer is used.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (cond ((and (consp where) (bufferp (car where))) ; List of buffers - search buffers.
         (dolist (buf where) (icicle-char-properties-in-buffer buf nil nil type)))
        ((and (consp where)             ; List of files - search files.
              (stringp (car where))
              (file-exists-p (car where)))
         (dolist (file where)
           (icicle-char-properties-in-buffer (find-file-noselect file) nil nil type)))
        ((consp where)                  ; Search all regions in `icicle-region-alist'.
         (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))
         (dolist (reg where)
           (when (bufferp (get-buffer (cadr reg)))
             (icicle-char-properties-in-buffer
              (get-buffer (cadr reg)) (cadr (cddr reg)) (car (cddr (cddr reg))) type))))
        (t                              ; Search this buffer only.
         (icicle-char-properties-in-buffer (current-buffer) beg end type))))

(defun icicle-char-properties-in-buffer (&optional buffer beg end type)
  "List of all character properties in BUFFER between BEG and END.
Only the character properties are included, not their values.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (let ((props  nil)
        ovrlays curr-props)
    (save-excursion
      (unless buffer (setq buffer  (current-buffer)))
      (set-buffer buffer)
      (unless (and beg end)
        (setq beg  (point-min)
              end  (point-max)))
      (when (or (not type) (eq type 'overlay)) ; Get overlay properties.
        (setq ovrlays  (overlays-in beg end))
        (dolist (ovrly ovrlays)
          (setq curr-props  (overlay-properties ovrly))
          (while curr-props
            (unless (memq (car curr-props) props) (push (car curr-props) props))
            (setq curr-props  (cddr curr-props)))))
      (when (or (not type) (eq type 'text)) ; Get text properties.
        (while (< beg end)
          (setq beg         (or (next-property-change beg nil end) end)
                curr-props  (text-properties-at beg))
          (while curr-props
            (unless (memq (car curr-props) props) (push (car curr-props) props))
            (setq curr-props  (cddr curr-props))))))
    props))

(defun icicle-search-char-property-scan (buffer beg end prop values type predicate)
  "Scan BUFFER for character property PROP with values VALUES.
Push hits onto `icicle-candidates-alist'.
If BUFFER is nil, scan the current buffer.
Highlight the matches in face `icicle-search-main-regexp-others'.
If BEG and END are non-nil, scan only between positions BEG and END.

Find text with a PROP value that overlaps with VALUES.  That is, if
the value of PROP is an atom, then it must be a member of VALUES; if
it is a list, then at least one list element must be a member of
VALUES.

If PREDICATE is non-nil, then push only the hits for which it holds.
PREDICATE is nil or a Boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context"
  (let ((add-bufname-p  (and buffer icicle-add-buffer-name-flag))
        (temp-list      ())
        (zone-end       nil))
    (unless buffer (setq buffer  (current-buffer)))
    (set-buffer buffer)
    (unless (and beg end)
      (setq beg  (point-min)
            end  (point-max)))
    (condition-case icicle-search-char-property-scan
        (save-excursion
          (while (and (< beg end)
                      (let* ((charval  (and (or (not type) (eq type 'overlay))
                                            (get-char-property beg prop)))
                             (textval  (and (or (not type) (eq type 'text))
                                            (get-text-property beg prop)))
                             (currval  (icicle-flat-list charval textval)))
                        (not (icicle-set-intersection values currval))))
            (setq beg  (icicle-next-single-char-property-change beg prop nil end)))
          (while (and beg (< beg end))
            (setq zone-end  (or (icicle-next-single-char-property-change beg prop nil end) end))
            (let* ((hit-string  (buffer-substring-no-properties beg zone-end))
                   (end-marker  (copy-marker zone-end)))
              (when (or (not predicate) (save-match-data (funcall predicate hit-string end-marker)))
                (push (cons (if add-bufname-p
                                (list hit-string
                                      (let ((string  (copy-sequence (buffer-name))))
                                        (put-text-property 0 (length string)
                                                           'face 'icicle-candidate-part string)
                                        string))
                              hit-string)
                            end-marker)
                      temp-list)
                ;; Highlight search context in buffer.
                (when (<= (+ (length temp-list) (length icicle-candidates-alist))
                          icicle-search-highlight-threshold)
                  (let ((ov  (make-overlay beg zone-end)))
                    (push ov icicle-search-overlays)
                    (overlay-put ov 'priority 200) ; > ediff's 100+, but < isearch overlays
                    (overlay-put ov 'face 'icicle-search-main-regexp-others)))))
            (setq beg  zone-end)
            (while (and (< beg end)
                        (let* ((charval  (and (or (not type) (eq type 'overlay))
                                              (get-char-property beg prop)))
                               (textval  (and (or (not type) (eq type 'text))
                                              (get-text-property beg prop)))
                               (currval  (icicle-flat-list charval textval)))
                          (not (icicle-set-intersection values currval))))
              (setq beg  (icicle-next-single-char-property-change beg prop nil end))))
          (setq icicle-candidates-alist  (append icicle-candidates-alist (nreverse temp-list))))
      (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
      (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
             (error (error-message-string icicle-search-char-property-scan))))))

(defun icicle-flat-list (val1 val2)
  "Return a flat list with all values in VAL1 and VAL2."
  (let ((result  nil))
    (unless (listp val1) (setq val1  (list val1)))
    (unless (listp val2) (setq val2  (list val2)))
    (while val1 (add-to-list 'result (pop val1)))
    (while val2 (add-to-list 'result (pop val2)))
    result))

(if (fboundp 'next-single-char-property-change)
    (defalias 'icicle-next-single-char-property-change 'next-single-char-property-change)
  (defun icicle-next-single-char-property-change (position prop &optional object limit)
    "Position of next change of PROP for text property or overlay change.
Scans characters forward from buffer position POSITION until property
PROP changes.  Returns the position of that change.

POSITION is a buffer position (integer or marker).

Optional third arg OBJECT is ignored.  It is present for compatibility
 with Emacs 22.

If optional fourth arg LIMIT is non-nil, search stops at position
LIMIT.  LIMIT is returned if nothing is found before LIMIT.

The property values are compared with `eq'.  If the property is
constant all the way to the end of the buffer, then the last valid
buffer position is returned."
    (save-excursion
      (goto-char position)
      (let ((propval  (get-char-property (point) prop))
            (end      (min limit (point-max))))
        (while (and (< (point) end) (eq (get-char-property (point) prop) propval))
          (goto-char (min (next-overlay-change (point))
                          (next-single-property-change (point) prop nil end)))))
      (point))))

;;;###autoload
(defun icicle-search-highlight-cleanup ()
  "Remove all highlighting from the last use of `icicle-search'."
  (interactive)
  (let ((inhibit-quit  t))
    (message "Removing search highlighting...")
    (while icicle-search-overlays
      (delete-overlay (car icicle-search-overlays))
      (setq icicle-search-overlays  (cdr icicle-search-overlays)))
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays  (cdr icicle-search-level-overlays)))
    (when (overlayp icicle-search-current-overlay)
      (delete-overlay icicle-search-current-overlay))
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  ()))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays)))
    (message "Removing search highlighting...done")))

;;;###autoload
(defun icicle-search-word (beg end word-regexp require-match ; Bound to `C-c $'.
                           &optional where &rest args)
  "Search for a whole word.
Word search is literal: regexp special characters are treated as
non-special.  In fact, they are also treated as if they were
word-constituent characters.  That is, your typed input is searched
for literally, but matches must begin and end on a word boundary.
This also means that you can include whitespace within the \"word\"
being sought.

At the prompt for a word, you can use completion against previous
Icicles search inputs to choose the word, or you can enter a new word.

Non-interactively, WORD-REGEXP should be a regexp that matches a word.
The other arguments are the same as for `icicle-search'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(icicle-search-read-word)
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (icicle-search beg end word-regexp (not icicle-add-buffer-name-flag) where))

;;;###autoload
(defun icicle-search-all-regions (scan-fn-or-regexp require-match &rest args)
  "Search all saved regions.
This is the same as using a simple prefix arg, `C-u', with
`icicle-search'.  Args are the same as for `icicle-search', but
without args BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match icicle-region-alist args))

;;;###autoload
(defun icicle-search-buffer (scan-fn-or-regexp require-match &rest args)
  "Search multiple buffers completely.
This is the same as using a non-negative numeric prefix arg, such as
`C-9', with `icicle-search'.  You are prompted for the buffers to
search.  All of each buffer is searched.  Any existing buffers can be
chosen.  Args are the same as for `icicle-search', but without args
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match
         (let ((icicle-show-Completions-initially-flag  t))
           (mapcar #'get-buffer (let ((icicle-buffer-require-match-flag  'partial-match-ok))
                                  (icicle-buffer-list))))
         args))

;;;###autoload
(defun icicle-search-file (scan-fn-or-regexp require-match &rest args)
  "Search multiple files completely.
This is the same as using a negative numeric prefix arg, such as
`C--', with `icicle-search'.  You are prompted for the files to search.
All of each file is searched.  Any existing files in the current
directory can be chosen.  Args are the same as for `icicle-search',
but without args BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match
         (let ((icicle-show-Completions-initially-flag  t)) (icicle-file-list))
         args))

;;;###autoload
(defun icicle-search-dired-marked (scan-fn-or-regexp require-match &rest args)
  "Search the marked files in Dired.
Args are the same as for `icicle-search', but without args BEG, END,
and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)))
  (unless (eq major-mode 'dired-mode)
    (error "Command `icicle-search-dired-marked' must be called from a Dired buffer"))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match
         (let ((icicle-show-Completions-initially-flag  t)) (dired-get-marked-files))
         args))

;;;###autoload
(defun icicle-occur (beg end &optional buffers) ; Bound to `C-c ''.
  "`icicle-search' with a regexp of \".*\".  An `occur' with icompletion.
Type a regexp to match within each line of the buffer.  Use `S-TAB' to
show matching lines.  Use `C-RET' or `C-mouse-2' to go to the line of
the current candidate.  Use `C-next', `C-prior', `C-down', or`C-up' to
cycle among the matching lines.

By default, search only the current buffer.  Search the active region,
or, if none, the entire buffer.  With a prefix argument, you are
prompted for the buffers to search.  You can choose buffers using
completion (`C-RET' and so on).  If the prefix argument is 99, then
only buffers visiting files are candidates.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the `icicle-search'
documentation."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(and current-prefix-arg
                       (icicle-search-choose-buffers (= 99 (prefix-numeric-value
                                                            current-prefix-arg))))))
  (let ((fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end ".*" (not icicle-add-buffer-name-flag) buffers))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload
(defun icicle-comint-search (beg end)   ; Bound to `C-x `' in `comint-mode'.
  "Use `icicle-search' to pick up a previous input for reuse.
Use this in a `comint-mode' buffer, such as *shell* or
*inferior-lisp*.  This searches your interactive history in the buffer
for a match to your current input, which you can change dynamically.
When you choose a previous input, it is copied to the current prompt,
for reuse.  If the region is active, then only it is searched;
otherwise, the entire buffer is searched.

Use `C-RET' or `C-mouse-2' to choose a previous input for reuse.  Use
`C-next', `C-prior', `C-down', or `C-up' to cycle among your previous
inputs.

As for other Icicles search commands, your current input narrows the
set of possible candidates.  See `icicle-search' for more
information.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

Note that previous commands are identified by looking through the
shell buffer for a shell prompt.  This is not foolproof.  If, for
instance you use command `ls', the output includes an auto-save file
such as #foo.el#, and `#' in the first column represents a shell
prompt, then #foo.el# will be misinterpreted as a previous command.

Also, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

Being a search command, `icicle-comint-search' cannot give you access
to previous shell commands that are not visible in the current buffer.
See also \\<comint-mode-map>\\[icicle-comint-command] for another way to reuse commands,
including those from previous sessions.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the `icicle-search'
documentation."
  (interactive (icicle-region-or-buffer-limits))
  ;; Is there a better test we can use, to make sure the current mode inherits from `comint-mode'?
  (unless (where-is-internal 'comint-send-input (keymap-parent (current-local-map)))
    (error "Current mode must be derived from comint mode"))
  (let ((orig-search-hook           icicle-search-hook)
        (icicle-transform-function  'icicle-remove-duplicates))
    (add-hook 'icicle-search-hook 'icicle-comint-send-input)
    (icicle-search beg end (concat comint-prompt-regexp "\\S-.*") nil) ; Match not required (edit).
    (remove-hook 'icicle-search-hook 'icicle-comint-send-input))
  (goto-char (point-max)))

(defun icicle-comint-send-input ()
  "Grab current completion input and use that for comint input."
  (unless (comint-check-proc (current-buffer))
    (error "No live process associated with this buffer"))
  (let ((comint-get-old-input
         (if (minibuffer-window-active-p (minibuffer-window))
             'icicle-comint-get-minibuffer-input ; Use minibuffer input (e.g. for action fn).
           'icicle-comint-get-final-choice))) ; Use final choice.
    (comint-copy-old-input))
  (comint-send-input))

(defun icicle-comint-get-minibuffer-input ()
  "Return the minibuffer input, beyond the prompt."
  (let* ((cand         (icicle-minibuf-input))
         (input-start  (and (string-match comint-prompt-regexp cand) (match-end 0))))
    (if input-start (substring cand input-start) cand)))

(defun icicle-comint-get-final-choice ()
  "Return the final choice, beyond the prompt."
  (let ((input-start  (and (string-match comint-prompt-regexp icicle-explore-final-choice)
                           (match-end 0))))
    (if input-start
        (substring icicle-explore-final-choice input-start)
      icicle-explore-final-choice)))

;;;###autoload
(icicle-define-command icicle-comint-command ; Bound to `C-c TAB' in `comint-mode'.
  "Retrieve a previously used command.
Use this in a `comint-mode' buffer such as *shell* or *inferior-lisp*.

Note, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

See also \\<comint-mode-map>\\[icicle-comint-search] for another way to reuse commands."
  insert
  "Choose a previous command: "         ; `completing-read' args
  (mapcar #'list (cddr comint-input-ring)) nil nil nil 'shell-command-history
  (aref (cddr comint-input-ring) 0) nil
  ((icicle-transform-function  'icicle-remove-duplicates))) ; Additional bindings

(defun icicle-comint-hook-fn ()
  "Hook giving comint mode Icicles command bindings."
  (set (make-local-variable 'icicle-search-command) 'icicle-comint-search)
  (define-key comint-mode-map "\C-c\C-i" 'icicle-comint-command)
  (define-key comint-mode-map [(control ?c) tab] 'icicle-comint-command))

;;;###autoload
(defun icicle-compilation-search (beg end) ; Bound to `C-c `' in `compilation(-minor)-mode'.
  "Like `icicle-search', but shows the matching compilation-buffer
hit.  Use this in a compilation buffer, such as `*grep*', searching
for a regexp as with `icicle-search'.  Use `C-RET' or `C-mouse-2' to
show the target-buffer hit corresponding to the current completion
candidate.  Use `C-next', `C-prior', `C-down', or `C-up' to cycle
among the target-buffer hits.

As for `icicle-search', you can further narrow the match candidates by
typing a second regexp to search for among the first matches.  See
`icicle-search' for more information.

Altogether, using this with `grep' gives you two or three levels of
regexp searching: 1) the `grep' regexp, 2) the major `icicle-search'
regexp, and optionally 3) the refining `icicle-search' regexp.

In Emacs 22 and later, you can replace search-hit text, as in
`icicle-search'.  In earlier Emacs versions, you cannot replace text.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the `icicle-search'
documentation."
  (interactive (icicle-region-or-buffer-limits))
  (unless (condition-case nil (eq (current-buffer) (compilation-find-buffer)) (error nil))
    (error "Current buffer must be a compilation buffer"))
  (save-excursion (goto-char (point-min))
                  (compilation-next-error 1)
                  (setq beg  (if beg (max beg (point)) (point))))
  (let ((icicle-transform-function    (if (interactive-p) nil icicle-transform-function))
        (icicle-candidate-alt-action-fn
         (if (boundp 'compilation-highlight-overlay) ; Emacs 22 test.
             icicle-candidate-alt-action-fn
           (lambda (cand)
             (message "Cannot replace matching text in Emacs before version 22"))))
        ;; $$$ Bind `next-error-highlight' to t, for indefinite, after Emacs bug is fixed.
        (next-error-highlight         1000000) 
        (icicle-search-in-context-fn  'icicle-compilation-search-in-context-fn)
        (fg (face-foreground          'icicle-search-main-regexp-others))
        (bg (face-background          'icicle-search-main-regexp-others)))
    (unwind-protect
         (progn
           (set-face-foreground 'icicle-search-main-regexp-others nil)
           (set-face-background 'icicle-search-main-regexp-others nil)
           (icicle-search beg end ".*" t))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

(defun icicle-compilation-search-in-context-fn (cand+mrker replace-string)
  "`icicle-search-in-context-fn' used for `icicle-compilation-search'."
  (if (not (fboundp 'compilation-next-error-function))
      (compile-goto-error)                   ; Emacs 20, 21.
    (setq compilation-current-error  (point)) ; Emacs 22+.
    (compilation-next-error-function 0 nil))
  (save-excursion
    (save-restriction
      (narrow-to-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point)))
      (icicle-search-highlight-and-maybe-replace cand+mrker replace-string)))
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight 'line-only 'nomsg))
  (let ((icicle-candidate-nb  icicle-candidate-nb)) (icicle-complete-again-update)))

(defun icicle-compilation-hook-fn ()
  "Hook setting `icicle-search-command' for compilation modes.
Used on `compilation-mode-hook' and `compilation-minor-mode-hook'."
  (set (make-local-variable 'icicle-search-command) 'icicle-compilation-search))

;;;###autoload
(defun icicle-imenu (beg end require-match &optional where) ; Bound to `C-c ='.
  "Go to an Imenu entry using `icicle-search'.
Recommended: Use library `imenu+.el' also.
In Emacs-Lisp mode, `imenu+.el' classifies definitions using these
submenus:

 1. Keys         - keys in the global keymap
 2. Keys in Maps - keys in keymaps other than global keymap
 3. Functions    - functions, whether interactive or not
 4. Macros       - macros defined with `defmacro'
 5. User Options - user variables, from `defcustom'
 6. Variables    - other variables (non-options), from `defvar'
 7. Faces        - faces, from `defface'
 8. Other        - other definitions

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation.

Note: If you use this command with a prefix argument, then the Imenu
mode (and `imenu-generic-expression') of the current buffer determines
what kinds of definitions are found.  So, if you want to search for
definitions in a certain language, then invoke this command from a
buffer in that language."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (unless imenu-generic-expression (error "No Imenu for this buffer"))
  (let ((case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist))
    (dolist (syn slist)                 ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus    (icicle-remove-if-not
                             #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                             (mapcar (lambda (menu) ; Name an unlabeled menu `Others'.
                                       (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                     imenu-generic-expression)))
                  (submenu  (let ((icicle-show-Completions-initially-flag  t))
                              (completing-read "Choose: " menus nil t)))
                  (regexp   (cadr (assoc submenu menus)))
                  (icicle-transform-function
                   (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No match"))
             (icicle-search beg end regexp require-match where)))
      (set-syntax-table old-table))))

(defun icicle-imenu-in-buffer-p (menu)
  "Return non-nil if the regexp in MENU has a match in the buffer."
  (save-excursion (goto-char (point-min)) (re-search-forward (cadr menu) nil t)))

(defun icicle-imenu-command (beg end require-match &optional where)
  "Go to an Emacs command definition using `icicle-search'.
This uses `commandp', so it finds only currently defined commands.
That is, if the buffer has not been evaluated, then its function
definitions are not considered commands by `icicle-imenu-command'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode."))
  (let ((case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist))
    (dolist (syn slist)                 ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus    (icicle-remove-if-not
                             #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                             (mapcar (lambda (menu) ; Name an unlabeled menu `Others'.
                                       (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                     (if (boundp 'emacs-lisp-imenu-generic-expression)
                                         emacs-lisp-imenu-generic-expression
                                       lisp-imenu-generic-expression))))
                  (submenu  (or (assoc "Functions" menus) (assoc "Others" menus)
                                (error "No command definitions in buffer")))
                  (regexp   (cadr (assoc (car submenu) menus)))
                  (icicle-transform-function
                   (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No command definitions in buffer"))
             (icicle-search beg end regexp require-match where 'icicle-imenu-command-p)))
      (set-syntax-table old-table))))

(defun icicle-imenu-non-interactive-function (beg end require-match &optional where)
  "Go to an Emacs non-interactive function definition with `icicle-search'.
This uses `commandp' to distinguish currently defined commands from
other functions.  This means that if the buffer has not yet been
evaluated, then all of its function definitions are considered
non-interactive by `icicle-imenu-non-interactive-function'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode."))
  (let ((case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist))
    (dolist (syn slist)                 ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus    (icicle-remove-if-not
                             #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                             (mapcar (lambda (menu) ; Name an unlabeled menu `Others'.
                                       (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                     (if (boundp 'emacs-lisp-imenu-generic-expression)
                                         emacs-lisp-imenu-generic-expression
                                       lisp-imenu-generic-expression))))
                  (submenu  (or (assoc "Functions" menus) (assoc "Others" menus)
                                (error "No command definitions in buffer")))
                  (regexp   (cadr (assoc (car submenu) menus)))
                  (icicle-transform-function
                   (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No command definitions in buffer"))
             (icicle-search beg end regexp require-match where
                            'icicle-imenu-non-interactive-function-p)))
      (set-syntax-table old-table))))

(defun icicle-imenu-command-p (ignored-hit-string ignored-marker)
  "Return non-nil for a command definition.
Predicate for `icicle-search'."
  (let ((indx  (if (< emacs-major-version 21) 6 2)))
    (commandp (intern-soft
               (buffer-substring-no-properties (match-beginning indx) (match-end indx))))))

(defun icicle-imenu-non-interactive-function-p (ignored-hit-string ignored-marker)
  "Return non-nil for a non-interactive function definition.
Predicate for `icicle-search'."
  (let* ((indx  (if (< emacs-major-version 21) 6 2))
         (fn    (intern-soft
                 (buffer-substring-no-properties (match-beginning indx) (match-end indx)))))
    (and (fboundp fn) (not (commandp fn)))))

;;;###autoload
(defun icicle-tags-search (regexp &optional arg)
  "Search all source files listed in tags tables for matches for REGEXP.
You are prompted for the REGEXP to match.  Enter REGEXP with `RET'.
You do not need `M-,' - you see all matches as search hits to visit.

All tags in a tags file are used, including duplicate tags from the
same or different source files.

By default, all tags files are used, but if you provide a prefix
argument then only the current tag table is used.

If your TAGS file references source files that no longer exist, those
files are listed.  In that case, you might want to update your TAGS
file."
  (interactive
   (let ((completion-ignore-case  (if (and (boundp 'tags-case-fold-search)
                                           (memq tags-case-fold-search '(t nil)))
                                      tags-case-fold-search
                                    case-fold-search)))
     (require 'etags)
     (list (icicle-search-read-context-regexp "Search files with tags for regexp: ")
           current-prefix-arg)))
  (let ((files  ()))
    (save-excursion
      (let ((first-time  t)
            (morep       t))
        (while (and morep (visit-tags-table-buffer (not first-time)))
          (when arg (setq morep  nil))
          (setq first-time  nil)
          (let ((tail  (last files)))
            (if tail
                (setcdr tail (mapcar 'expand-file-name (tags-table-files)))
              (setq files  (mapcar 'expand-file-name (tags-table-files))))))))
    (let ((tail              files)     ; Remove names of non-existent or unreadable files.
          (unreadable-files  ()))
      (while tail
        (if (file-readable-p (car tail))
            (setq tail  (cdr tail))
          (push (car tail) unreadable-files)
          (setcar tail (cadr tail))
          (setcdr tail (cddr tail))))
      (when unreadable-files
        (with-output-to-temp-buffer "*Unreadable Files*"
          (princ "These missing or unreadable files were ignored:") (terpri) (terpri)
          (dolist (file unreadable-files) (princ file) (terpri)))))
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))
    (icicle-search nil nil regexp nil files)))

;;;###autoload
(defun icicle-save-string-to-variable (askp)
  "Save a string (text) to a variable.
You are prompted for the string to save.  Typically, you store a
regexp or part of a regexp in the variable.

By default, the variable is user option `icicle-input-string'.
To save to a different variable, use a prefix argument; you are then
prompted for the variable to use.

You can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' to insert a string from a
variable."
  (interactive "P")
  (let* ((enable-recursive-minibuffers  t)
         (var   (if askp
                    (intern (completing-read "Variable: " obarray 'boundp nil nil
                                             (if (boundp 'variable-name-history)
                                                 'variable-name-history
                                               'icicle-variable-name-history)
                                             (symbol-name 'icicle-input-string)))
                  'icicle-input-string))
         (text  (icicle-completing-read-history (format "Text to save in `%s': " var))))
    (set var text)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'any)))
  (defalias 'any 'icicle-anything))
(when (> emacs-major-version 21)
  (defun icicle-anything (type)
    "Act on an object of type TYPE.
You are prompted for the type, then for an object of that type.  The
type is either the declared `type' of an Anything source, or its
`name' if it has no `type'.

This command is available only if you use library `anything.el'.

This is an Icicles multi-command: You can act on multiple objects in
multiple ways during a single command invocation.  When you choose an
object using `RET' or `mouse-2', the default action is applied to it.
The default action is also applied to the current completion candidate
when you use `C-RET', `C-mouse-2', and so on.

You can apply a different action by using an alternative action key:
`C-S-RET', `C-S-mouse-2', and so on.  This lets you choose the action
to apply using completion. You can use `C-RET', `C-mouse-2', and so
on, to perform multiple actions.

This command is intended for use only in Icicle mode."
    (interactive
     (let ((icicle-show-Completions-initially-flag  t)
           (icicle-whole-candidate-as-text-prop-p   icicle-anything-transform-candidates-flag))
       (unless (require 'anything nil t) (error "You must load library `anything.el' first"))
       (list (intern (completing-read "What (type): " (icicle-remove-duplicates
                                                       (mapcar #'list (icicle-get-anything-types)))
                                      nil t)))))
    (icicle-object-action type)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'file)))
  (defun file ()
    "Act on a file.  You are prompted for the file and the action.
During file-name completion, you can delete the file named by the
current candidate, using `S-delete'.

This is just `icicle-object-action' with type `file'."
    (interactive) (icicle-object-action 'file)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'buffer)))
  (defun buffer ()
    "Act on a buffer.  You are prompted for the buffer and the action.
During buffer-name completion, you can kill the buffer named by the
current candidate, using `S-delete'.

This is just `icicle-object-action' with type `buffer'."
    (interactive) (icicle-object-action 'buffer)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'a)))
  (defalias 'a 'icicle-object-action))
(when (and icicle-define-alias-commands-flag (not (fboundp 'what-which-how)))
  (defalias 'what-which-how 'icicle-object-action))
;;;###autoload
(defun icicle-object-action (&optional type)
  "Act on an object of type TYPE.
You are prompted for the type (\"What\"), then for an object of that
type (\"Which\"), then for the action function to apply to the
object (\"How\").  For Anything types (see below), you are not
prompted for the action function.

The \"type\" of an object is one of these:

a. A member of the list `icicle-object-predicate-types'.  These are
   type predicates, such as `bufferp', `keywordp', or `atom'.

b. The `type' of an Anything source, or its `name' if it has no
   `type'.  This is available only if you use library `anything.el'.

c. A member of the list `icicle-object-named-types'.

In the case of Anything types (only), this is a multi-command:
* `C-RET', `C-mouse-2', and so on perform the default action.
* `C-S-RET', `C-S-mouse-2', and so on let you choose the action using
  completion.

Though this is not a multi-command for non-Anything types `buffer' and
`file', you can use `S-delete' during completion to delete the object
\(buffer or file) named by the current completion candidate.

The objects of types (b) and (c) are easily associated with names.
Their names are the completion candidates.  So, for instance, if you
choose type `buffer', then you can act on a buffer by choosing its
name.

The objects of predicate type (type a) are not necessarily named.  The
completion candidates for these objects are symbols whose values are
the objects acted upon.  So, for instance, if you choose type
`bufferp', then you can choose a symbol whose value is a buffer, in
order to act on that buffer.  While a buffer is always named, an
object of type `stringp' is not.  The value of `emacs-version' is one
such string that you can act on.

Be aware that the action function you choose must accomodate the
object you choose as its only an argument.  Also, completion of the
function candidate itself is not strict, so you can enter a lambda
form.

With a prefix argument, the result of applying the function to the
object is pretty-printed using `pp-eval-expression'.  Otherwise, the
function is called for its effect only, and its value is not
displayed.

This command is intended for use only in Icicle mode."
  (interactive)
  (let* ((anything-loaded-p         (and (> emacs-major-version 21) (require 'anything nil t)))
         (anything-types            (and (not type) anything-loaded-p (icicle-get-anything-types)))
         (typ
          (or type
              (let ((icicle-show-Completions-initially-flag  t))
                (intern
                 (completing-read
                  "What (type): "
                  (icicle-remove-duplicates (mapcar #'list (append anything-types
                                                                   icicle-object-named-types
                                                                   icicle-object-predicate-types)))
                  nil t)))))
         (predicate-type-p          (and (member (symbol-name typ) icicle-object-predicate-types)
                                         (not (memq typ anything-types))))
         (anything-candidates       (and anything-loaded-p (not predicate-type-p)
                                         (icicle-get-anything-candidates-of-type typ)))
         (anything-default-actions  (and anything-candidates
                                         (icicle-get-anything-default-actions-for-type typ)))
         (anything-actions          (and anything-candidates
                                         (icicle-get-anything-actions-for-type typ)))
         (icicle-saved-completion-candidate
          (cond (predicate-type-p (icicle-read-var-value-satisfying typ))
                (anything-candidates
                 (icicle-choose-anything-candidate typ anything-candidates
                                                   anything-default-actions anything-actions))
                ((member (symbol-name typ) (and anything-loaded-p (icicle-get-anything-types)))
                 (error "No candidates for type `%s'" (symbol-name typ)))
                (t
                 (icicle-choose-candidate-of-type typ))))
         (icicle-candidate-action-fn      (if anything-candidates
                                              (lambda (fn) (icicle-apply-to-saved-candidate fn t))
                                            'icicle-apply-to-saved-candidate))
         (icicle-candidate-alt-action-fn  (and anything-candidates
                                               (lambda (fn)
                                                 (icicle-apply-to-saved-candidate fn t)))))
    (if anything-candidates
        (if (null (cdr anything-default-actions))
            (funcall (cdar anything-default-actions) icicle-saved-completion-candidate)
          (funcall (let ((icicle-show-Completions-initially-flag  t))
                     (completing-read "How (action): " anything-default-actions nil t))
                   icicle-saved-completion-candidate))
      (icicle-apply-to-saved-candidate (completing-read "How (action): " obarray 'functionp)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-types ()
    "Return list of types defined in `anything-sources'.  See `anything.el'."
    (and (boundp 'anything-sources) (consp anything-sources)
         (let ((types  ())
               type)
           (dolist (source (anything-get-sources))
             (if (setq type  (assoc-default 'type source))
                 (push (symbol-name type) types)
               (when (setq type  (assoc-default 'name source)) (push type types))))
           (setq types
                 (mapcar (lambda (typ)
                           (setq typ  (copy-sequence typ))
                           (put-text-property 0 (length typ) 'face 'icicle-special-candidate typ)
                           typ)
                         (icicle-remove-duplicates types)))))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-candidates-of-type (type)
    "Return list of Anything candidates for type TYPE.
Used only when `anything-sources' is non-nil - see `anything.el'."
    (and (boundp 'anything-sources) (consp anything-sources)
         (let ((anything-candidate-cache  ())
               (candidates                nil))
           (dolist (source (anything-get-sources))
             (let ((init-fn  (assoc-default 'init source))) (when init-fn (funcall init-fn)))
             (when (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
               (setq candidates  (icicle-get-anything-cached-candidates source))))
           (when (and (not (functionp candidates)) (consp candidates))
             (mapcar (lambda (cand) (if (consp cand) cand (list cand))) candidates))
           candidates))))

;; Similar to `anything-get-cached-candidates' in `anything.el', but ignores processes.
;; Free var here: `anything-candidate-cache'.
(when (> emacs-major-version 21)
  (defun icicle-get-anything-cached-candidates (source)
    "Return cached value of candidates for Anything SOURCE.
Cache the candidates if there is not yet a cached value."
    (let* ((source-name      (assoc-default 'name source))
           (candidate-cache  (assoc source-name anything-candidate-cache))
           candidates)
      (if candidate-cache
          (setq candidates  (cdr candidate-cache))
        (setq candidates  (icicle-get-anything-candidates source))
        (when (processp candidates) (setq candidates  ()))
        (setq candidate-cache  (cons source-name candidates))
        (push candidate-cache anything-candidate-cache))
      candidates)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-candidates (source)
    "Return the list of candidates from Anything SOURCE."
    (let* ((candidate-source  (assoc-default 'candidates source))
           (candidates
            (cond ((functionp candidate-source)
                   `(lambda (string pred mode)
                     (let ((anything-pattern  icicle-current-input))
                       (setq string  anything-pattern)
                       (let ((all-cands  (funcall ,candidate-source)))
                         (setq all-cands
                               (icicle-remove-if-not
                                (lambda (cand)
                                  (string-match (if (eq 'prefix icicle-current-completion-mode)
                                                    (concat "^" (regexp-quote string))
                                                  string)
                                                cand))
                                all-cands))
                         (cond ((eq mode t) all-cands)
                               ((eq mode nil)
                                (icicle-expanded-common-match icicle-current-input all-cands))
                               ((eq mode 'lambda) t))))))
                  ((listp candidate-source) candidate-source)
                  ((and (symbolp candidate-source) (boundp candidate-source))
                   (symbol-value candidate-source))
                  (t
                   (error
                    (concat "Source `candidates' value is not a function, variable or list: %s")
                    candidate-source)))))
      (if (or (not icicle-anything-transform-candidates-flag) (processp candidates))
          candidates
        (anything-transform-candidates candidates source)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-actions-for-type (type)
    "Set and return `icicle-candidates-alist' of actions for type TYPE."
    (setq icicle-candidates-alist  ())
    (let ((all-sources-actions  ())
          this-source-actions)
      (dolist (source (anything-get-sources))
        (when (or (eq type (assoc-default 'type source))
                  (string= (symbol-name type) (assoc-default 'name source)))
          (setq this-source-actions  (assoc-default 'action source))
          (dolist (action this-source-actions)
            (unless (memq action all-sources-actions)
              (push action all-sources-actions)))))
      (setq icicle-candidates-alist  (sort all-sources-actions
                                           (lambda (a1 a2)
                                             (funcall icicle-sort-function (car a1) (car a2))))))))
(when (> emacs-major-version 21)
  (defun icicle-choose-anything-candidate (type candidates default-actions actions)
    "Read an Anything object of type TYPE with completion, and return it.
During completion, you can act on selected completion candidates, in
turn, using the action keys.
CANDIDATES is the list of candidates of type TYPE.
DEFAULT-ACTIONS is the list of default actions for type TYPE.
ACTIONS is the list of all actions for type TYPE."
    (let* ((win                                         (selected-window))
           (icicle-sort-function                        nil)
           (icicle-transform-function                   nil)
           (icicle-Completions-display-min-input-chars  (icicle-get-anything-req-pat-chars type))
           (icicle-incremental-completion-delay         (icicle-get-anything-input-delay type))
           (icicle-whole-candidate-as-text-prop-p       icicle-anything-transform-candidates-flag)
           (icicle-candidates-alist
            (if (or (functionp candidates) icicle-whole-candidate-as-text-prop-p)
                candidates
              icicle-candidates-alist))
           (icicle-candidate-action-fn
            (lambda (obj)
              (when icicle-whole-candidate-as-text-prop-p
                (setq obj  (icicle-anything-candidate-value obj)))
              (let ((enable-recursive-minibuffers  t))
                (with-selected-window win
                  (if (null (cdr default-actions))
                      (funcall (cdar default-actions) obj)
                    (funcall (completing-read "How (action): " default-actions nil t) obj))))
              (select-window (minibuffer-window))
              (select-frame-set-input-focus (selected-frame))
              (icicle-raise-Completions-frame)))
           (icicle-candidate-alt-action-fn
            (lambda (obj)
              (when icicle-whole-candidate-as-text-prop-p
                (setq obj  (icicle-anything-candidate-value obj)))
              (let ((icicle-show-Completions-initially-flag  t)
                    (icicle-saved-completion-candidate       obj)
                    (icicle-candidates-alist                 actions)
                    (enable-recursive-minibuffers            t))
                (with-selected-window win
                  (icicle-apply-to-saved-candidate
                   (let ((enable-recursive-minibuffers      t)
                         (icicle-last-completion-candidate  icicle-last-completion-candidate)
                         (icicle-candidate-action-fn
                          (lambda (actn) (with-selected-window win
                                           (let ((enable-recursive-minibuffers t)
                                                 (icicle-candidates-alist actions))
                                             (icicle-apply-to-saved-candidate actn t)))))
                         (icicle-candidate-alt-action-fn    nil))
                     (completing-read "How (action): " actions nil t))
                   t)))))
           (orig-action-fn  icicle-candidate-action-fn)
           (icicle-candidate-help-fn
            (if icicle-whole-candidate-as-text-prop-p
                (lambda (obj)
                  (let ((icicle-candidate-help-fn  nil))
                    (icicle-help-on-candidate-symbol
                     (intern (icicle-anything-candidate-value obj)))))
              icicle-candidate-help-fn))
           (icicle-candidate-action-fn
            (if icicle-whole-candidate-as-text-prop-p
                (lambda (obj)
                  (let ((icicle-last-input  (icicle-anything-candidate-value obj)))
                    (funcall orig-action-fn obj)))
              icicle-candidate-action-fn)))
      (if icicle-whole-candidate-as-text-prop-p
          (icicle-anything-candidate-value
           (completing-read (concat "Which (" (symbol-name type) "): ") candidates nil t))
        (completing-read (concat "Which (" (symbol-name type) "): ") candidates nil t)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-req-pat-chars (type)
    "Return max `required-pattern' value for sources of type TYPE.
The value returned is also always at least as big as
`icicle-Completions-display-min-input-chars'."
    (let ((req-pat              icicle-Completions-display-min-input-chars)
          (req-pat-this-source  nil))
      (dolist (source (anything-get-sources))
        (when (and (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
                   (setq req-pat-this-source  (assoc-default 'requires-pattern source)))
          (setq req-pat  (max req-pat req-pat-this-source))))
      req-pat)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-input-delay (type)
    "Return max `delay' value for sources of type TYPE.
The value returned is also always at least as big as
`icicle-incremental-completion-delay'."
    (let ((delay              icicle-incremental-completion-delay)
          (delay-this-source  nil))
      (dolist (source (anything-get-sources))
        (when (and (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
                   (setq delay-this-source  (and (assoc 'delayed source) anything-idle-delay)))
          (setq delay  (max delay delay-this-source))))
      delay)))

(when (> emacs-major-version 21)
  (defun icicle-anything-candidate-value (candidate)
    "Return the real value associated with string CANDIDATE."
    (or (cdr-safe (icicle-get-alist-candidate candidate)) candidate)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-default-actions-for-type (type)
    "Set and return `icicle-candidates-alist' of default actions for type TYPE."
    (setq icicle-candidates-alist  ())
    (let ((all-sources-actions  ())
          this-source-actions)
      (dolist (source (anything-get-sources))
        (when (or (eq type (assoc-default 'type source))
                  (string= (symbol-name type) (assoc-default 'name source)))
          (setq this-source-actions  (assoc-default 'action source))
          (unless (memq (car this-source-actions) all-sources-actions)
            (push (car this-source-actions) all-sources-actions))))
      (setq icicle-candidates-alist  (sort all-sources-actions
                                           (lambda (a1 a2)
                                             (funcall icicle-sort-function (car a1) (car a2))))))))

(defun icicle-choose-candidate-of-type (type)
  "Read an object of type TYPE with completion, and return it."
  (case type
    (buffer
     (let ((icicle-must-match-regexp        icicle-buffer-match-regexp)
           (icicle-must-not-match-regexp    icicle-buffer-no-match-regexp)
           (icicle-must-pass-predicate      icicle-buffer-predicate)
           (icicle-extra-candidates         icicle-buffer-extras)
           (icicle-sort-function            (or icicle-buffer-sort icicle-sort-function))
           (icicle-delete-candidate-object  'icicle-kill-a-buffer) ; `S-delete' kills buffer.
           (icicle-require-match-flag        icicle-buffer-require-match-flag)
           (icicle-ignore-space-prefix-flag  icicle-buffer-ignore-space-prefix-flag))
       (get-buffer-create
        (completing-read "Which (buffer): " (mapcar (lambda (buf) (list (buffer-name buf)))
                                                    (buffer-list))
                         nil nil nil 'buffer-name-history nil nil))))
    (file (let ((icicle-delete-candidate-object  'icicle-delete-file-or-directory)) ; `S-delete'
            (read-file-name "Which (file): " nil
                            (if (and (eq major-mode 'dired-mode)
                                     (fboundp 'diredp-find-a-file)) ; Defined in `dired+.el'.
                                (condition-case nil ; E.g. error because not on file line (ignore)
                                    (abbreviate-file-name (dired-get-file-for-visit))
                                  (error default-directory))
                              default-directory) nil)))
    (command (intern (completing-read "Which (command): " obarray 'commandp)))
    (face (intern (completing-read "Which (face): " (mapcar (lambda (x) (list (format "%s" x)))
                                                            (face-list)))))
    (frame (let ((frame-alist  (icicle-make-frame-alist)))
             (cdr (assoc (completing-read "Which (frame): " frame-alist)
                         frame-alist))))
    (function (intern (completing-read "Which (function): " obarray 'fboundp)))
    (option (intern (completing-read "Which (user option): " obarray 'user-variable-p)))
    (process (get-process (completing-read
                           "Which (process): " (mapcar (lambda (proc) (list (process-name proc)))
                                                       (process-list)))))
    (symbol (intern (completing-read "Which (symbol): " obarray)))
    (variable (intern (completing-read "Which (variable): " obarray 'boundp)))
    (window (let ((buffers  ()))
              (walk-windows (lambda (win)
                              (push (list (format "%s" (window-buffer win))) buffers))
                            nil t)
              (get-buffer-window (completing-read "Window showing buffer: " buffers) 0)))
    (otherwise (error "Bad object type: %S" type))))

(defun icicle-read-var-value-satisfying (pred)
  "Reads a variable that satisfies PRED and returns its value."
  (symbol-value (intern (completing-read (format "Which (%s value of symbol): " pred) obarray
                                         `(lambda (symb)
                                            (and (boundp symb)
                                                 (funcall #',pred (symbol-value symb))))))))

;;;###autoload
(defun icicle-customize-icicles-group ()
  "Customize Icicles options and faces.  View their documentation."
  (interactive)
  (customize-group-other-window 'Icicles))

;;;###autoload
(defun icicle-send-bug-report ()
  "Send a bug report about an Icicles problem."
  (interactive)
  (browse-url (format (concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Icicles bug: \
&body=Describe bug below, using a precise recipe that starts with `emacs -Q' or `emacs -q'.  \
Each Icicles file has a header `Update #' that you can use to identify it.  %s.")
                      (emacs-version))))
 
;;(@* "Other top-level Icicles commands")
;;; Other top-level Icicles commands   .   .   .   .   .   .

;;;###autoload
(when (fboundp 'map-keymap)             ; Emacs 22.

  (defun icicle-generic-S-tab ()
    "Treat `S-TAB' in minibuffer, *Completions*, and elsewhere.
Bound to `S-TAB' by default, but you can change the binding using
option `icicle-generic-S-tab-keys'.  Whatever it is bound to, it does
this:
 - Call `icicle-apropos-complete' if in minibuffer during completion.
 - Call `icicle-move-to-previous-completion' if in *Completions*.
 - Call `icicle-complete-keys', otherwise."
    (interactive)
    (cond ((icicle-completing-p)        ; Cannot use the var here, since not sure to be in minibuf.
           (setq this-command  'icicle-apropos-complete)
           (icicle-apropos-complete))   ; Defined in `icicles-mcmd.el'
          ((eq (get-buffer "*Completions*") (current-buffer))
           (setq this-command  'icicle-move-to-previous-completion)
           (icicle-move-to-previous-completion (prefix-numeric-value current-prefix-arg)))
          (t
           (setq this-command  'icicle-complete-keys)
           (icicle-complete-keys))))

  ;; This is a quick-and-dirty definition, not an efficient one.
  ;; It gathers all key bindings and then throws most of them away!  Good enough.
  (defun icicle-insert-char ()
    "Insert a character, using key completion.
Keys bound to `self-insert-command' are completion candidates."
    (interactive)
    (barf-if-buffer-read-only)
    (let ((icicle-complete-keys-self-insert-flag  t)
          (icicle-must-match-regexp               "^.+  =  self-insert-command"))
      (icicle-complete-keys)))

  (defun icicle-complete-keys ()        ; Bound to prefix keys followed by `S-TAB' (unless defined).
    "Complete a key sequence for the currently invoked prefix key.
Input-candidate completion and cycling are available.

You can navigate the key-binding hierarchy (prefix-key hierarchy),
just as would navigate a file-system hierarchy (to complete directory
and file names) or a menu hierarchy (to complete submenu and menu-item
names).

Completion candidates generally have the form `KEY  =  COMMAND'.

If COMMAND is `...', then KEY is a prefix key; choosing it updates the
completion candidates list to the keys under that prefix.  For
example, choosing `C-x = ...' changes the candidates to those with
prefix `C-x'.

The special candidate `..' means to go up one level of the key-binding
hierarchy and complete candidates there.  For example, if you are
currently completing prefix key `C-x 5', and you choose candidate
`..', then you will be completing prefix `C-x', the parent of `C-x 5'.

Except at the top level, the default value for completion is `..'.

If option `icicle-complete-keys-self-insert-flag' is non-nil, then
keys bound to `self-insert-command' are included as possible
completion candidates; otherwise (the default), they are not.  Command
`icicle-insert-char' works like `icicle-complete-keys', but in
includes only keys bound to `self-insert-command' - use it to insert a
character that is difficult or impossible to type with your keyboard.

You can use `C-M-,' at any time to switch between sorting with local
bindings first and sorting with prefix keys first.  You can use `C-,'
at any time to change the sort order among these two and sorting by
command name.

While cycling, these keys describe candidates:

`C-RET'   - Describe command of current completion candidate only
`C-down'  - Move to next prefix-completion candidate and describe
`C-up'    - Move to previous prefix-completion candidate and describe
`C-next'  - Move to next apropos-completion candidate and describe
`C-prior' - Move to previous apropos-completion candidate and describe
`C-!'     - Describe *all* candidates (or all that are saved),
            successively - use the [back] button in buffer *Help* to
            visit the descriptions

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.  This is an Icicles command - see `icicle-mode'."
    (interactive)
    (let* ((icicle-transform-function               'icicle-remove-duplicates)
           (icicle-show-Completions-initially-flag  t)
           (icicle-candidate-action-fn              'icicle-complete-keys-action)
           (enable-recursive-minibuffers            t)
           ;; `orig-(buff|win)-key-complete' are used free in `icicle-complete-keys-action'.
           (orig-buff-key-complete                  (current-buffer))
           (orig-win-key-complete                   (selected-window))
           (icicle-completing-keys-p                t) ; Provide a condition to test key completion.
           (icicle-sort-function                    'icicle-special-candidates-first-p)
           (icicle-alternative-sort-function        'icicle-prefix-keys-first-p)
           (icicle-sort-functions-alist
            ;; The first entry just gives `icicle-special-candidates-first-p' a different name.
            '(("by key name, local bindings first" . icicle-special-candidates-first-p)
              ("by key name, prefix keys first" . icicle-prefix-keys-first-p)
              ("by command name" . icicle-command-names-alphabetic-p)
              ("turned OFF"))))
      (icicle-complete-keys-1 (icicle-this-command-keys-prefix))))

  (defun icicle-this-command-keys-prefix ()
    "Return the prefix of the currently invoked key sequence."
    (let ((this-key  (this-command-keys))) (substring this-key 0 (1- (length this-key)))))

  ;; Free vars here: `icicle-complete-keys-alist' is bound in `icicles-var.el'.
  (defun icicle-complete-keys-1 (prefix) ; PREFIX is a free var in `icicle-complete-keys-action'.
    "Complete a key sequence for prefix key PREFIX (a vector)."
    (let ((orig-extra-cands  icicle-extra-candidates)) ; Free in `icicle-complete-keys-action'.
      (unwind-protect
           (progn
             (icicle-keys+cmds-w-prefix prefix)
             (unless icicle-complete-keys-alist (error "No keys for prefix `%s'" prefix))
             (let* ((this-cmd-keys      ; For error report - e.g. mouse cmd.
                     (this-command-keys-vector)) ; Free var in `icicle-complete-keys-action'.
                    (prefix-description
                     (icicle-key-description prefix (not icicle-key-descriptions-use-<>-flag)))
                    (candidate
                     (completing-read
                      (concat "Complete keys" (and (not (string= "" prefix-description))
                                                   (concat " " prefix-description))
                              ": ")
                      icicle-complete-keys-alist nil t nil nil
                      ;;$$ (if (equal [] prefix) nil "\\.\\.")
                      )))
               (icicle-complete-keys-action  candidate)))
        (mapc (lambda (cand) (put (car cand) 'icicle-special-candidate nil)) ; Reset the property.
              icicle-complete-keys-alist))))

  ;; Free vars here:
  ;; `orig-buff-key-complete' and `orig-win-key-complete' are bound in `icicle-complete-keys'.
  ;; `prefix', `orig-extra-cands', and `this-cmd-keys' are bound in `icicle-complete-keys-1'.
  (defun icicle-complete-keys-action (candidate)
    "Completion action function for `icicle-complete-keys'."
    (let* ((key+binding    (cdr-safe (assq (intern candidate) icicle-complete-keys-alist)))
           (key            (car-safe key+binding))
           (binding        (cdr-safe key+binding))
           (cmd-name       nil)
           (action-window  (selected-window)))
      (unwind-protect
           (progn
             (set-buffer orig-buff-key-complete)
             (select-window orig-win-key-complete)
             (if (string= ".." candidate)
                 (setq cmd-name  "..")
               (unless (and (string-match "\\(.+\\)  =  \\(.+\\)" candidate) (match-beginning 2))
                 (error "No match"))
               (setq cmd-name  (substring candidate (match-beginning 2) (match-end 2))))
             (cond ((string= ".." cmd-name) ; Go back up to parent prefix.
                    (setq last-command  'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat (nbutlast (append prefix nil)))))
                   ((and key (string= "..." cmd-name)) ; Go down to prefix.
                    (setq last-command  'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat prefix key)))
                   (t
                    (setq this-command             binding
                          last-command             binding
                          icicle-extra-candidates  orig-extra-cands) ; Restore it.
                    (when (eq 'self-insert-command binding)
                      (unless key (error "Cannot insert `%s'" key))
                      (setq last-command-char  (aref key 0)))
                    (when (eq 'digit-argument binding)
                      (setq last-command-char  (aref key 0))
                      (icicle-msg-maybe-in-minibuffer "Numeric argument"))
                    (when (eq 'negative-argument binding)
                      (icicle-msg-maybe-in-minibuffer "Negative argument"))
                    (setq last-nonmenu-event  1) ; So *Completions* mouse-click info is ignored.
                    (condition-case try-command
                        (call-interactively binding nil this-cmd-keys)
                      (error (error (error-message-string try-command)))))))
        (select-window action-window))))

  (defun icicle-keys+cmds-w-prefix (prefix)
    "Fill `icicle-complete-keys-alist' for prefix key PREFIX (a vector)."
    (let ((prefix-map  nil))
      (setq icicle-complete-keys-alist  ())
      (dolist (map (current-active-maps t))
        (setq prefix-map  (lookup-key map prefix))
        ;; NOTE: `icicle-add-key+cmd' Uses `prefix' and `map' as free vars.
        (when (keymapp prefix-map) (map-keymap #'icicle-add-key+cmd prefix-map)))
      (unless (equal [] prefix) (push (list '\.\.) icicle-complete-keys-alist))
      icicle-complete-keys-alist))

  ;; Free vars here: `prefix' and `map' are bound in `icicle-keys+cmds-w-prefix'.
  (defun icicle-add-key+cmd (event binding)
    "Add completion for EVENT and BINDING to `icicle-complete-keys-alist'."
    (cond
      ;; (menu-item ITEM-STRING): non-selectable item - skip it.
      ((and (eq 'menu-item (car-safe binding))
            (null (cdr-safe (cdr-safe binding))))
       (setq binding  nil))             ; So `keymapp' test, below, fails.

      ;; (ITEM-STRING): non-selectable item - skip it.
      ((and (stringp (car-safe binding)) (null (cdr-safe binding)))
       (setq binding  nil))             ; So `keymapp' test, below, fails.

      ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
      ((eq 'menu-item (car-safe binding))
       (let ((enable-condition  (memq ':enable (cdr-safe (cdr-safe (cdr-safe binding))))))
         (if (or (not enable-condition)
                 (condition-case nil    ; Don't enable if we can't check the condition.
                     (eval (cadr enable-condition))
                   (error nil)))
             (setq binding  (car-safe (cdr-safe (cdr-safe binding))))
           (setq binding  nil))))

      ;; (ITEM-STRING . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
      ((stringp (car-safe binding))
       (setq binding  (cdr binding))
       ;; Skip HELP-STRING
       (when (stringp (car-safe binding)) (setq binding  (cdr binding)))
       ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
       (when (and (consp binding) (consp (car binding))) (setq binding  (cdr binding)))))

    ;; Follow indirections to ultimate symbol naming a command.
    (while (and (symbolp binding) (fboundp binding) (keymapp (symbol-function binding)))
      (setq binding  (symbol-function binding)))

    ;; `prefix' and `map' are free here, bound in `icicle-keys+cmds-w-prefix'.
    (cond ((and (or (keymapp binding)
                    (and (commandp binding)
                         (equal binding (key-binding (vconcat prefix (vector event))))
                         (not (eq binding 'icicle-generic-S-tab))
                         (not (eq binding 'icicle-complete-keys))))
                (or (not (eq binding 'self-insert-command)) ; Command, keymap.
                    (and icicle-complete-keys-self-insert-flag ; Insert normal char.
                         (char-valid-p event))))
           (let* ((key-desc   (propertize (single-key-description
                                           event
                                           (not icicle-key-descriptions-use-<>-flag))
                                          'face 'icicle-candidate-part))
                  (candidate  (intern (concat key-desc "  =  " (if (keymapp binding)
                                                                   "..."
                                                                 (prin1-to-string binding))))))
             ;; Skip keys bound to `undefined'.
             (unless (string= "undefined" (prin1-to-string binding))
               (push (cons candidate (cons (vector event) binding)) icicle-complete-keys-alist))
             (when (eq map (current-local-map)) (put candidate 'icicle-special-candidate t))))
          ((and (integerp event) (generic-char-p event) ; Insert generic char.
                (eq 'self-insert-command  binding))
           (ignore))))                  ; Placeholder for future use.

  ;; $$ No longer used.  Was used in `icicle-complete-keys-1'.
  (defun icicle-read-single-key-description (string need-vector &optional no-angles)
    "If STRING contains a space, then the vector containing the symbol named STRING.
Otherwise, call `icicle-read-kbd-macro'.
Other args are as for `icicle-read-kbd-macro'."
    (cond ((and no-angles (string-match " " string)) (vector (intern string)))
          ((string-match "^<\\([^>]* [^>]*\\)>" string)
           (vector (intern (substring string (match-beginning 1) (match-end 1)))))
          (t (icicle-read-kbd-macro string need-vector no-angles))))

  ;; $$ No longer used.  Was used as `icicle-candidate-action-fn' in `icicle-complete-keys'.
  (defun icicle-complete-keys-help (candidate)
    "Describe the command associated with the current completion candidate."
    (interactive)                       ; Interactively, just describes itself.
    (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
    (string-match "\\(.+\\)  =  \\(.+\\)" candidate)
    (let ((frame-with-focus  (selected-frame))
          (cmd               (intern-soft (substring candidate (match-beginning 2) (match-end 2)))))
      (if (not (fboundp cmd))
          (icicle-msg-maybe-in-minibuffer "No help")
        (describe-function cmd))
      (icicle-raise-Completions-frame)
      ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
      ;; because the *Help* frame takes the focus away from the minibuffer frame.
      ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
      (let* ((help-window  (get-buffer-window "*Help*" 0))
             (help-frame   (and help-window (window-frame help-window))))
        (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
    (message nil))                      ; Let minibuffer contents show immmediately.

  (defun icicle-read-kbd-macro (start &optional end no-angles)
    "Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always.

Optional argument NO-ANGLES non-nil means to expect key
descriptions not to use angle brackets (<...>).  For example:

 (icicle-read-kbd-macro \"<mode-line>\" t)   returns [mode-line]
 (icicle-read-kbd-macro  \"mode-line\"  t t) returns [mode-line]"
    (interactive "r")
    (if (stringp start)
        (icicle-edmacro-parse-keys start end no-angles)
      (setq last-kbd-macro
            (icicle-edmacro-parse-keys (buffer-substring start end) nil no-angles))))

  (defun icicle-edmacro-parse-keys (string &optional need-vector no-angles)
    "Same as `edmacro-parse-keys', but with added NO-ANGLES argument.
NO-ANGLES is the same as for `icicle-read-kbd-macro'."
    (let ((case-fold-search  nil)
          (pos               0)
          (res               []))
      (while (and (< pos (length string))
                  (string-match "[^ \t\n\f]+" string pos))
        (let ((word   (substring string (match-beginning 0) (match-end 0)))
              (key    nil)
              (times  1))
          (setq pos  (match-end 0))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times  (string-to-number (substring word 0 (match-end 1)))
                  word   (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key  (vconcat (if (eq (key-binding [?\M-x])
                                             'execute-extended-command)
                                         [?\M-x]
                                       (or (car (where-is-internal
                                                 'execute-extended-command))
                                           [?\M-x]))
                                     (substring word 2 -2) "\r")))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos  (string-match "$" string pos)))
                ((and (string-match (if no-angles
                                        "^\\(\\([ACHMsS]-\\)*\\)\\(..+\\)$"
                                      "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$")
                                    word)
                      (or (not no-angles)
                          (save-match-data (not (string-match "^\\([ACHMsS]-.\\)+$" word))))
                      (progn
                        (setq word  (concat (substring word (match-beginning 1)
                                                       (match-end 1))
                                            (substring word (match-beginning 3)
                                                       (match-end 3))))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key  (list (intern word))))
                (t
                 (let ((orig-word  word)
                       (prefix     0)
                       (bits       0))
                   (while (string-match "^[ACHMsS]-." word)
                     (incf bits (cdr (assq (aref word 0)
                                           '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                             (?H . ?\H-\^@) (?M . ?\M-\^@)
                                             (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                     (incf prefix 2)
                     (callf substring word 2))
                   (when (string-match "^\\^.$" word)
                     (incf bits ?\C-\^@)
                     (incf prefix)
                     (callf substring word 1))
                   (let ((found  (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                               ("LFD" . "\n") ("TAB" . "\t")
                                               ("ESC" . "\e") ("SPC" . " ")
                                               ("DEL" . "\177")))))
                     (when found (setq word  (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (loop for ch across word
                           for n = 0 then (+ (* n 8) ch -48)
                           finally do (setq word  (vector n))))
                   (cond ((= bits 0)
                          (setq key  word))
                         ((and (= bits ?\M-\^@) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key  (loop for x across word collect (+ x bits))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key  (list (+ bits (- ?\C-\^@) (logand (aref word 0) 31)))))
                         (t
                          (setq key  (list (+ bits (aref word 0)))))))))
          (when key
            (loop repeat times do (callf vconcat res key)))))
      (when (and (>= (length res) 4)
                 (eq (aref res 0) ?\C-x)
                 (eq (aref res 1) ?\()
                 (eq (aref res (- (length res) 2)) ?\C-x)
                 (eq (aref res (- (length res) 1)) ?\)))
        (setq res  (edmacro-subseq res 2 -2)))
      (if (and (not need-vector)
               (loop for ch across res
                     always (and (char-valid-p ch)
                                 (let ((ch2  (logand ch (lognot ?\M-\^@))))
                                   (and (>= ch2 0) (<= ch2 127))))))
          (concat (loop for ch across res
                        collect (if (= (logand ch ?\M-\^@) 0)
                                    ch (+ ch 128))))
        res))))

;; See also `hexrgb-read-color' in `hexrgb.el'.
;;;###autoload
(defun icicle-read-color (&optional arg prompt)
  "Read a color name or hex RGB color value #RRRRGGGGBBBB.
A string value is returned.
Interactively, optional argument ARG is the prefix arg.
Optional argument PROMPT is the prompt to use (default \"Color: \").

In addition to standard color names and RGB (red, green, blue) hex
values, the following are also available as proxy color candidates,
provided `icicle-add-proxy-candidates-flag' is non-nil and library
`palette.el' or `eyedropper.el' is used.  In each case, the
corresponding color is used.

* `*copied foreground*'  - last copied foreground, if available
* `*copied background*'  - last copied background, if available
* `*mouse-2 foreground*' - foreground where you click `mouse-2'
* `*mouse-2 background*' - background where you click `mouse-2'
* `*point foreground*'   - foreground under the text cursor
* `*point background*'   - background under the text cursor

\(You can copy a color using eyedropper commands such as
`eyedrop-pick-foreground-at-mouse'.)

In addition, the names of user options (variables) whose custom type
is `color' are also proxy candidates, but with `'' as a prefix and
suffix.  So, for example, option `icicle-region-background' appears as
proxy color candidate `'icicle-region-background''.

As always, you can toggle the use of proxy candidates using `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-proxy-candidates]' in
the minibuffer.

With plain `C-u', use `hexrgb-read-color', which lets you complete a
color name or input any valid RGB hex value (without completion).

With no prefix arg, return a string with both the color name and the
RGB value, separated by `icicle-list-nth-parts-join-string'.

With a numeric prefix arg of 0 or 1, return the color name.  With any
other numeric prefix arg, return the RGB value.

In the plain `C-u' case, your input is checked to ensure that it
represents a valid color.

In all other cases:

- You can complete your input against the color name, the RGB value,
  or both.

- If you enter input without completing or cycling, the input is not
  checked: whatever is entered is returned as the string value.

From Emacs Lisp, ARG controls what is returned. If ARG is nil,
`icicle-list-use-nth-parts' can also be used to control the behavior.

Note: Duplicate color names are removed by downcasing and removing
whitespace.  For example, \"AliceBlue\" and \"alice blue\" are both
treated as \"aliceblue\".  Otherwise, candidates with different names
but the same RGB values are not considered duplicates, so, for
example, input can match either \"darkred\" or \"red4\", which both
have RGB #8b8b00000000.  You can toggle duplicate removal at any time
using `C-$'.

During completion, candidate help (e.g. `C-M-RET') shows you the RGB
and HSV (hue, saturation, value) color components.

This command is intended only for use in Icicle mode (but it can be
used with `C-u', with Icicle mode turned off)."
  (interactive "P")
  (unless (featurep 'hexrgb) (error "`icicle-read-color' requires library `hexrgb.el'"))
  (icicle-highlight-lighter)
  (let ((icicle-candidate-help-fn  'icicle-color-help)
        color)
    (if (consp arg)                     ; Plain `C-u': complete against color name only,
        (let ((icicle-transform-function  'icicle-remove-color-duplicates))
          (setq color  (hexrgb-read-color t))) ; and be able to input any valid RGB string.

      ;; Complete against name+RGB pairs, but user can enter invalid value without completing.
      (let* ((completion-ignore-case     t)
             (icicle-transform-function  'icicle-remove-color-duplicates)
             (icicle-sort-functions-alist
              '(("by color name" . icicle-part-1-lessp)
                ("by color hue"  . (lambda (s1 s2) (not (icicle-color-hue-lessp s1 s2))))
                ("by color purity (saturation)" .
                 (lambda (s1 s2) (not (icicle-color-saturation-lessp s1 s2))))
                ("by color brightness (value)" .
                 (lambda (s1 s2) (not (icicle-color-value-lessp s1 s2))))
                ("by amount of red"   . (lambda (s1 s2) (not (icicle-color-red-lessp s1 s2))))
                ("by amount of green" . (lambda (s1 s2) (not (icicle-color-green-lessp s1 s2))))
                ("by amount of blue"  . (lambda (s1 s2) (not (icicle-color-blue-lessp s1 s2))))
                ("by color rgb"       . (lambda (s1 s2) (not (icicle-part-2-lessp s1 s2))))
                ("turned OFF")))
             ;; Make the two `*-join-string' variables the same, so past inputs are recognized.
             ;; Do not use " " as the value, because color names such as "white smoke" would be
             ;; split, and "smoke" would not be recognized as a color name when trying to list
             ;; candidates in *Completions*.
             (icicle-list-nth-parts-join-string  ": ")
             (icicle-list-join-string            ": ")
             (icicle-list-end-string             "")
             (icicle-proxy-candidate-regexp      "^[*'].+[*']")
             (named-colors
              (mapcar #'icicle-make-color-candidate (x-defined-colors)))
             (icicle-proxy-candidates
              (mapcar                   ; Convert multi-completions to strings.
               (lambda (entry)
                 (concat (mapconcat #'identity (car entry) icicle-list-join-string)
                         icicle-list-end-string))
               (append
                (and (or (fboundp 'eyedrop-foreground-at-point)
                         (and (> emacs-major-version 21) (require 'palette nil t))
                         (require 'eyedropper nil t))
                     (append
                      (and eyedrop-picked-foreground ; Multi-completions.
                           `(,(icicle-make-color-candidate
                               "*copied foreground*" (downcase (hexrgb-color-name-to-hex
                                                                eyedrop-picked-foreground)))))
                      (and eyedrop-picked-background
                           `(,(icicle-make-color-candidate
                               "*copied background*" (downcase (hexrgb-color-name-to-hex
                                                                eyedrop-picked-background)))))
                      `(,(icicle-make-color-candidate
                          "*point foreground*" (downcase (hexrgb-color-name-to-hex
                                                          (eyedrop-foreground-at-point))))
                        ,(icicle-make-color-candidate
                          "*point background*" (downcase (hexrgb-color-name-to-hex
                                                          (eyedrop-background-at-point)))))
                      '((("*mouse-2 foreground*")) (("*mouse-2 background*")))))
                (let ((ipc  ()))
                  (mapatoms
                   (lambda (cand)
                     (when (and (user-variable-p cand)
                                (icicle-var-is-of-type-p cand '(color))
                                ;; This shouldn't be necessary, but type `color' isn't
                                ;; enforced - it just means `string' (so far).
                                (x-color-defined-p (symbol-value cand)))
                       (push `,(icicle-make-color-candidate
                                (concat "'" (symbol-name cand) "'")
                                (downcase (hexrgb-color-name-to-hex (symbol-value cand))))
                             ipc))))
                  ipc))))
             (icicle-list-use-nth-parts
              (or (and arg (list arg))  ; 1 or 2, by program or via `C-1' or `C-2'.
                  icicle-list-use-nth-parts ; Bound externally by program.
                  '(1 2)))              ; Both parts, by default.
             (mouse-pseudo-color-p  nil))
        (setq color  (icicle-transform-multi-completion
                      (completing-read (or prompt "Color: ") named-colors)))
        (when (fboundp 'eyedrop-foreground-at-point)
          (cond ((string-match "^\*mouse-2 foreground\*" color)
                 (setq color  (prog1 (eyedrop-foreground-at-mouse
                                      (read-event
                                       "Click `mouse-2' anywhere to choose foreground color"))
                                (read-event)) ; Discard mouse up event.
                       mouse-pseudo-color-p  t))
                ((string-match "^\*mouse-2 background\*" color)
                 (setq color  (prog1 (eyedrop-background-at-mouse
                                      (read-event
                                       "Click `mouse-2' anywhere to choose background color"))
                                (read-event)) ; Discard mouse up event.
                       mouse-pseudo-color-p  t))))
        (when mouse-pseudo-color-p
          (let ((icicle-list-nth-parts-join-string  ": ")
                (icicle-list-join-string            ": ")
                (icicle-list-end-string             "")
                (icicle-list-use-nth-parts
                 (or (and arg (list arg)) ; 1 or 2, by program or via `C-1' or `C-2'.
                     icicle-list-use-nth-parts ; Bound externally by program.
                     '(1 2))))          ; Both parts, by default.
            (setq color  (icicle-transform-multi-completion
                          (concat color ": " (hexrgb-color-name-to-hex color))))))))
    (when (interactive-p) (message "Color: `%s'" color))
    color))

(defun icicle-make-color-candidate (color-name &optional hex-rgb)
  "Return multi-completion candidate of COLOR-NAME and its hex RGB string.
If `icicle-WYSIWYG-Completions-flag' is non-nil, then the hex RGB
string has the color as its background text property.
Optional arg HEX-RGB is the hex RGB string.
If nil, then COLOR-NAME is used to determine the hex RGB string."
  (unless (featurep 'hexrgb) (error "`icicle-make-color-candidate' requires library `hexrgb.el'"))
  (let ((rgb-string  (or hex-rgb (hexrgb-color-name-to-hex color-name))))
    (when icicle-WYSIWYG-Completions-flag
      (put-text-property 0 (length rgb-string) 'face (cons 'background-color rgb-string)
                         rgb-string))
    (list (list color-name rgb-string))))

(defun icicle-remove-color-duplicates (list)
  "Copy of LIST with duplicate color candidates removed.
Candidates are considered duplicates if they have the same color name,
abstracting from whitespace and letter case."
  (let ((tail  list)
        new)
    (save-match-data
      (while tail
        (let* ((this            (car tail))
               (pseudo-color-p  (string-match "^\*" this)))
          (string-match ": " this)
          (unless pseudo-color-p
            (setq this  (icicle-delete-whitespace-from-string
                         (downcase this) 0 (match-beginning 0))))
          (unless (member this new) (push this new)))
        (pop tail)))
    (nreverse new)))

(defun icicle-color-help (color)
  "Display help on COLOR."
  (let ((icicle-list-use-nth-parts  '(1 2)))
    (with-output-to-temp-buffer "*Help*"
      (princ (format "Color: %s" (icicle-transform-multi-completion color))) (terpri) (terpri)
      (setq icicle-list-use-nth-parts  '(2)
            color                      (icicle-transform-multi-completion color))
      (let* ((rgb  (hexrgb-hex-to-rgb color))
             (hsv  (apply #'hexrgb-rgb-to-hsv rgb)))
        (princ "RGB:") (mapcar (lambda (component) (princ (format "  %.18f" component))) rgb)
        (terpri) (terpri)
        (princ "HSV:") (mapcar (lambda (component) (princ (format "  %.18f" component))) hsv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-cmd.el ends here
