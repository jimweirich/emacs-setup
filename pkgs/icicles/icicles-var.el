;;; icicles-var.el --- Internal variables for Icicles
;;
;; Filename: icicles-var.el
;; Description: Internal variables for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2008, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:23:26 2006
;; Version: 22.0
;; Last-Updated: Mon Oct  6 14:36:59 2008 (-0700)
;;           By: dradams
;;     Update #: 741
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-var.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `color-theme', `cus-face',
;;   `easymenu', `ffap', `ffap-', `hexrgb', `icicles-opt', `kmacro',
;;   `thingatpt', `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  internal variables (not to be modified by users.  See `icicles.el'
;;  for documentation.
;;
;;  Internal variables defined here:
;;
;;    `icicle-abs-file-candidates', `icicle-all-candidates-action-p',
;;    `icicle-all-candidates-list-action-fn',
;;    `icicle-all-candidates-list-alt-action-fn',
;;    `icicle-apply-nomsg', `icicle-apropos-complete-match-fn',
;;    `icicle-apropos-match-fns-alist', `icicle-bookmark-history',
;;    `icicle-buffer-config-history', `icicle-candidate-action-fn',
;;    `icicle-candidate-alt-action-fn', `icicle-candidate-entry-fn',
;;    `icicle-candidate-help-fn', `icicle-candidate-nb',
;;    `icicle-candidate-properties-alist', `icicle-candidates-alist',
;;    `icicle-char-property-value-history',
;;    `icicle-cmd-calling-for-completion', `icicle-color-history',
;;    `icicle-color-theme-history', `icicle-commands-for-abbrev',
;;    `icicle-common-match-string',
;;    `icicle-comp-base-is-default-dir-p',
;;    `icicle-complete-input-overlay', `icicle-complete-keys-alist',
;;    `icicle-completing-p', `icicle-completion-candidates'
;;    `icicle-completion-help-string',
;;    `icicle-completion-set-history',
;;    `icicle-current-completion-candidate-overlay',
;;    `icicle-current-completion-mode', `icicle-current-input',
;;    `icicle-current-raw-input', `icicle-default-directory',
;;    `icicle-default-thing-insertion-flipped-p',
;;    `icicle-delete-candidate-object', `icicle-dictionary-history',
;;    `icicle-dir-candidate-can-exit-p', `icicle-edit-update-p',
;;    `icicle-explore-final-choice',
;;    `icicle-explore-final-choice-full', `icicle-extra-candidates',
;;    `icicle-face-name-history', `icicle-font-name-history',
;;    `icicle-frame-alist', `icicle-frame-name-history',
;;    `icicle-function-name-history', `icicle-ignored-extensions',
;;    `icicle-ignored-extensions-regexp',
;;    `icicle-incremental-completion-p',
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
;;    `icicle-ms-windows-drive-hash', `icicle-must-match-regexp',
;;    `icicle-must-not-match-regexp', `icicle-must-pass-predicate',
;;    `icicle-nb-of-other-cycle-candidates',
;;    `icicle-object-named-types', `icicle-object-predicate-types',
;;    `icicle-old-read-file-name-fn', `icicle-pre-minibuffer-buffer',
;;    `icicle-post-command-hook', `icicle-pre-command-hook',
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
;;    `icicle-search-overlays', `icicle-search-refined-overlays',
;;    `icicle-search-replacement',
;;    `icicle-search-replacement-history',
;;    `icicle-successive-grab-count',
;;    `icicle-text-property-value-history',
;;    `icicle-thing-at-pt-fns-pointer',
;;    `icicle-universal-argument-map', `icicle-variable-name-history',
;;    `icicle-whole-candidate-as-text-prop-p',
;;    `lacarte-menu-items-alist'.
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
;;  (@> "Internal variables (alphabetical)")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile                               ;; for Emacs < 21: push
 (when (< emacs-major-version 21) (require 'cl)));; for Emacs < 20: when, unless

(require 'apropos-fn+var nil t) ;; (no error if not found): apropos-command,
                                ;; apropos-function, apropos-option, apropos-variable
(require 'icicles-opt) ;; icicle-sort-function

;;; Defvars to quiet byte-compiler:
(when (< emacs-major-version 22) (defvar kmacro-ring-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Internal variables (alphabetical)")

;;; Internal variables (alphabetical) --------------------------------

(defvar lacarte-menu-items-alist nil)    ; Defined in `lacarte.el'.

;; These two are defined here so they won't raise an error in `font-lock-add-keywords'.
(defvar font-lock-function-name-face 'font-lock-function-name-face ; In `font-lock.el'.
  "Face name to use for function names.")

(defvar font-lock-keyword-face 'font-lock-keyword-face ; Defined in `font-lock.el'.
  "Face name to use for keywords.")

(defvar icicle-abs-file-candidates nil
  "Current list of absolute file-name candidates.")

(defvar icicle-all-candidates-action-p nil
  "Non-nil means that we are acting on all candidates.
That is, `icicle-all-candidates-action-1' is in progress.")

(defvar icicle-all-candidates-list-alt-action-fn nil
  "Alternative action function to apply to a list of matching completions.")

(defvar icicle-all-candidates-list-action-fn nil
  "Action function to apply to a list of all matching completions.")

(defvar icicle-apply-nomsg nil
  "Non-nil means `icicle-apply' does not show status of applying function.")

(defvar icicle-apropos-complete-match-fn 'string-match
  "Function to filter apropos completion candidates.
Takes as arguments an input string and a completion candidate, and
returns non-nil if the string is considered to match the candidate.

A value of nil instead of a function means completion candidates are
not filtered by the input, except possibly by a function-valued
`minibuffer-completion-table'.")

(defvar icicle-apropos-match-fns-alist '(("apropos" . string-match)
                                         ("scatter" . icicle-scatter-match))
  "Alist of string matching functions and their names.
Each element has the form (NAME . FUNCTION), where NAME is a string
name and FUNCTION is the function.")

(defvar icicle-bookmark-history nil "History for bookmark names.")

(defvar icicle-buffer-config-history nil "History for buffer configuration names.")

(defvar icicle-candidate-action-fn nil
  "Action function to apply to current completion candidate.
For `icicle-all-candidates-action' to be able to report successes,
this should return nil for \"success\" and non-nil for \"failure\".")

(defvar icicle-candidate-alt-action-fn nil
  "Alternative action function to apply to current completion candidate.
For `icicle-all-candidates-alt-action' to be able to report successes,
this should return nil for \"success\" and non-nil for \"failure\".")

(defvar icicle-candidate-entry-fn nil
  "Function to apply to selected entries in `icicle-candidates-alist'.")

(defvar icicle-candidate-help-fn nil
  "Help function to be applied to current completion candidate.
If non-nil, it must be a function that accepts a completion candidate
  (a string or a symbol) as argument.
If nil, default help function `icicle-help-on-candidate' is used.")

(defvar icicle-candidate-nb nil
  "Current completion candidate number, or nil if not cycling candidates.
Numbering starts at zero.")

(defvar icicle-candidate-properties-alist nil
  "Alist of multi-completion indexes and associated text properties.
The text properties apply to candidates in *Completions*.  Each alist
entry has the form (NTH PROPERTIES) or (NTH PROPERTIES JOIN-TOO).

NTH is a whole-number index identifying the multi-completion part.

PROPERTIES is a list of text properties to apply to the part.

JOIN-TOO non-nil means to also apply PROPERTIES to the join string
that follows the part.

Example alist:

 ((3 (face 'underline))
  (2 (invisible t) t))

The first entry underlines the third multi-completion part.
The second entry makes both the second part and the join string that
follows it invisible.")

(defvar icicle-candidates-alist nil
  "Alist of candidate entries.
The car (key) of each entry is treated as a completion candidate.
The cdr is some other data to be used when the candidate is chosen.

This is reset to nil at the beginning of each top-level command.  It
is used only by commands that use completion without allowing sorting
of completion candidates.")

(defvar icicle-char-property-value-history nil "History for character property values.")

(defvar icicle-cmd-calling-for-completion 'ignore
  "Last command causing display of list of possible completions.")

(defvar icicle-color-history nil "History for color names.")

(defvar icicle-color-theme-history nil "History for color-theme names.")

(defvar icicle-commands-for-abbrev nil
  "List of commands that match the current abbreviation.")

(defvar icicle-common-match-string nil
  "Longest common match among all completion candidates.
nil means no such common match is available.")

(defvar icicle-comp-base-is-default-dir-p nil
  "Non-nil means to use `default-directory' as the completion base.
This means use its length as `completion-base-size'.")

(defvar icicle-complete-input-overlay nil
  "Overlay used to highlight minibuffer input when it is complete.")

(when (fboundp 'map-keymap)             ; Emacs 22+.
  (defvar icicle-complete-keys-alist nil "Alist of keys and their bindings.
Each alist element is of the form (NAME KEY . BINDING), where:
 NAME is a symbol naming the key and its binding, whose name has form:
   KEYNAME  =  BINDING-NAME
 KEY is the actual key sequence
 BINDING is the actual binding of KEY."))

(defvar icicle-completing-p nil "Cached value of function `icicle-completing-p'.")

(defvar icicle-completion-candidates nil "Current list of completion candidates.")

(defvar icicle-completion-help-string "" "Description of minibuffer bindings.")

(defvar icicle-completion-set-history nil "History for completion-set names.")

(defvar icicle-current-completion-candidate-overlay nil
  "Overlay used to highlight current completion candidate.")

(defvar icicle-current-completion-mode nil
  "Symbol `prefix' or `apropos', specifying the current completion mode.")

(defvar icicle-current-input "" "Current minibuffer input.")

(defvar icicle-current-raw-input "" "Current minibuffer raw (unexpanded) input.
This can be different from `icicle-current-input' only when
`icicle-expand-input-to-common-match-flag' is non-nil.")

(defvar icicle-default-directory default-directory
  "Local copy of `default-directory'.
Set whenever minibuffer is entered or input is completed.")

(defvar icicle-default-thing-insertion-flipped-p nil
  "Non-nil means a previous `M-.' in this succession was used with `C-u'.
This means that the meaning of `icicle-default-thing-insertion' has
been reversed.")

(defvar icicle-delete-candidate-object nil
  "Defines deletion action for command `icicle-delete-candidate-object'.
The value can be a function or a symbol bound to an alist.

If the value is a function, then the function is called on the current
completion candidate (a string) to delete some corresponding object.

If the value is a symbol (variable) bound to an alist, then
`icicle-delete-current-candidate-object' is called to delete the
corresponding object from that alist.  If the variable is also a user
option, then the option is saved after the candidate is deleted.

Note that if the value is a variable and you use multi-completion
candidates during completion, then the alist value of the variable
must itself contain multi-completions.  Otherwise, no candidate will
be deleted, because `icicle-delete-current-candidate-object' deletes
the full candidate object.")

(defvar icicle-dictionary-history nil "History for dictionary entries.")

(defvar icicle-dir-candidate-can-exit-p nil
  "Non-nil means you can exit the minibuffer when you choose a directory.")

(defvar icicle-doc-history nil "History for documentation entries.")

(defvar icicle-edit-update-p nil
  "Internal flag: non-nil when editing text in minibuffer.
More precisely, non-nil when updating the completions list inside
simple character-editing commands such as `icicle-self-insert' and
`icicle-delete-backward-char'.")

(defvar icicle-explore-final-choice ""
  "Final `icicle-explore' completion choice (a string).")

(defvar icicle-explore-final-choice-full nil
  "Full alist element that corresponds to `icicle-explore-final-choice'.
This is an element of `icicle-candidates-alist'.
The element's car is a completion-candidate string.")

(defvar icicle-extra-candidates nil "A list of extra completion candidates (strings).")

(defvar icicle-face-name-history nil "History for font names.")

(defvar icicle-font-name-history nil "History for font names.")

(defvar icicle-frame-alist nil "Alist of frames, returned by `icicle-make-frame-alist'.")

(defvar icicle-frame-name-history nil "History for frame names.")

(defvar icicle-function-name-history nil "History for function names.
Each name is a symbol name or a lambda form, as a string.")

(defvar icicle-ignored-extensions completion-ignored-extensions
  "Copy of `completion-ignored-extensions', serving as a control flag.
When `completion-ignored-extensions' changes, we remake
`icicle-ignored-extensions-regexp'.")

(defvar icicle-ignored-extensions-regexp
  (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "$\\|")
          "$\\)\\'")
  "Regular expression matching ignored file extensions.
If this is nil, then no file extensions are ignored.
The ignored file extensions come from `completion-ignored-extensions'.")

(defvar icicle-incremental-completion-p nil
  "Takes the place of `icicle-incremental-completion-flag' during input.
The program updates this to `always' from `t' after *Completions* has
been displayed.")

(defvar icicle-Info-only-rest-of-book-p nil
  "Non-nil means complete only Info nodes from the rest of the book.")

(defvar icicle-inhibit-sort-p nil
  "Non-nil means that users cannot sort completion candidates.
They also cannot remove duplicates.")

(defvar icicle-inhibit-try-switch-buffer nil
  "Non-nil means `icicle-try-switch-buffer' does not switch to `orig-buff'.")

(defvar icicle-initial-value ""
  "Initial value used in minibuffer completion.
Any function that reads from the minibuffer and accepts a default
value or initial value should, before reading, put that value in
`icicle-initial-value'.  For example, `completing-read' does that.

In addition, `completing-read' and `read-file-name' will respect this
value, using it as the initial value if none is provided explicitly.
This means that you can bind `icicle-initial-value' around an
expression that calls `completing-read' or `read-file-name', and the
bound value will be used as the initial value.")

(defvar icicle-input-fail-pos nil
  "Position in minibuffer of start of completion match failure.
Nil means no match failure is known.")

(defvar icicle-insert-string-at-pt-end nil
  "Position of end of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-insert-string-at-pt-start nil
  "Position of start of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-kill-history nil "History of kill-ring entries.")

(when (boundp 'kmacro-ring)             ; Emacs 22+
  (defvar icicle-kmacro-alist nil
    "Alist with elements (CANDIDATE-NAME . RING-ITEM).
CANDIDATE-NAME is 1, 2, 3....

RING-ITEM is an item in `kmacro-ring' or `(kmacro-ring-head)'.")
  (defvar icicle-kmacro-history nil "History for keyboard-macro names."))

(defvar icicle-last-completion-candidate ""
  "Last completion candidate used in minibuffer completion.")

(defvar icicle-last-completion-command nil "Last completion command used.")

(defvar icicle-last-input "" "Last minibuffer input typed (not from cycling).")

(defvar icicle-last-sort-function (or icicle-sort-function 'icicle-case-string-less-p)
  "Local copy of `icicle-sort-function', so we can restore it.")

(defvar icicle-last-top-level-command nil "Last top-level command used.")

(defvar icicle-last-transform-function (or icicle-transform-function
                                           'icicle-remove-duplicates)
  "Local copy of `icicle-transform-function', so we can restore it.")

(defvar icicle-list-use-nth-parts nil
  "List of indexes of multi-completion pieces to use.
This is not an internal variable.  You can bind this in your own Lisp
code to affect completion behavior.

An empty list means use the entire multi-completion.  Otherwise,
concatenate, in order, the Nth parts of the multi-completion, where N
is each of the (one-based) indexes, in turn.  Any index larger than
the actual number of parts in the multi-completion means use the last
part.

For example: If the value is (1), then use only the first part of the
multi-completion as the completion candidate. If the value is (2 1),
then use as candidate the second part followed by the first part, the
two parts being joined by option `icicle-list-nth-parts-join-string'.
If the value is (1 99) and the multi-completion has fewer than 99
parts, then use the first and last parts, joined by
`icicle-list-nth-parts-join-string'.  If the value is (2 1 2), then
use the second part, first part, and second part again - you can use a
given part any number of times.")

(defvar icicle-menu-map nil "Icicles menu-bar menu keymap.")

(defvar icicle-minor-mode-map-entry nil "Icicles mode entry in `minor-mode-map-alist'.")

(defvar icicle-ms-windows-drive-hash (and (fboundp 'make-hash-table)
                                          (make-hash-table :test 'equal))
  "Hash table for caching result of MS Windows `NET USE' system calls.
For Emacs 20 and 21, this is not used unless you load library `cl.el'
at runtime.")

(defvar icicle-must-match-regexp nil
  "A regexp that completion candidates must match, or nil.
If nil, then this does nothing.  If a regexp (string), then show only
candidates whose display form matches it (and matches the user input).
The display form is the string shown in `*Completions*'.
See also `icicle-must-not-match-regexp'.")

(defvar icicle-must-not-match-regexp nil
  "A regexp that completion candidates must not match, or nil.
If nil, then this does nothing.  If a regexp (string), then show only
candidates whose display form does not match it.
The display form is the string shown in `*Completions*'.
See also `icicle-must-match-regexp'.")

(defvar icicle-must-pass-predicate nil
  "A predicate that completion candidates must satisfy, or nil.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates whose display form
satisfies the predicate are displayed.  The display form is the string
shown in `*Completions*'.")

(defvar icicle-nb-of-other-cycle-candidates 0
  "Number of other candidates available for cycling.
This is for use by other libraries, in particular, `icomplete+.el'.")

(defvar icicle-object-named-types '("buffer" "command" "face" "file" "frame" "function"
                                    "option" "process" "symbol" "variable" "window")
  "Type names whose objects can easily be associated with names.")

(defvar icicle-object-predicate-types
  (append '("atom" "arrayp" "bool-vector-p" "bufferp" "byte-code-function-p"
            "case-table-p" "char-or-string-p" "char-table-p" "commandp" "consp" "facep"
            "floatp" "frame-configuration-p" "frame-live-p" "framep" "functionp"
            "hash-table-p" "integer-or-marker-p" "integerp" "keymapp" "keywordp" "listp"
            "markerp" "wholenump" "nlistp" "numberp" "number-or-marker-p" "overlayp"
            "processp" "sequencep" "stringp" "subrp" "symbolp" "syntax-table-p"
            "user-variable-p" "vectorp" "window-configuration-p" "window-live-p"
            "windowp")
          ;; These are conditional because they are not defined for some Emacs versions.
          (and (fboundp 'display-table-p) '("display-table-p"))
          (and (fboundp 'string-or-null-p) '("string-or-null-p"))
          (and (fboundp 'booleanp) '("booleanp")))
  "Type names that are predicate names.")

(defvar icicle-post-command-hook nil
  "Functions added to `post-command-hook' when in Icicle mode.
Use command `icy-mode' (aka `icicle-mode') to set this up properly.")

(defvar icicle-pre-command-hook nil
  "Functions added to `pre-command-hook' when in Icicle mode.
Use command `icy-mode' (aka `icicle-mode') to set this up properly.")

(defvar icicle-old-read-file-name-fn (and (not (boundp 'read-file-name-function)) ; Em 22+
                                          'orig-read-file-name) ; Emacs 20, 21
  "Value of `read-file-name-function' outside of Icicle mode.
For versions of Emacs before 22, this is `read-file-name'.")

(defvar icicle-pre-minibuffer-buffer nil
  "Buffer that was current before the minibuffer became active.")

(defvar icicle-previous-raw-file-name-inputs nil
  "Previous inputs user has typed during file-name completion.
These are inputs typed but not necessarily entered with `RET'.")

(defvar icicle-previous-raw-non-file-name-inputs nil
  "Previous inputs user has typed during non-file-name completion.
These are inputs typed but not necessarily entered with `RET'.")

(defvar icicle-prompt "" "Prompt used for completion.")

(defvar icicle-proxy-candidate-regexp nil
  "Regexp to match proxy candidates, or nil to do nothing.
The candidates are highlighted in buffer *Completions* using face
`icicle-proxy-candidate'.")

(defvar icicle-proxy-candidates nil "List of proxy completion candidates (strings).")

(defvar icicle-read-expression-map nil
  "Icicle mode version of `read-expression-map'.
Several Emacs-Lisp mode key bindings are used.")
(unless icicle-read-expression-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\t" 'lisp-complete-symbol)
    (define-key map "\t" 'lisp-indent-line)
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\e\t" 'lisp-complete-symbol)
    (define-key map "\e\C-x" 'eval-defun)
    (define-key map "\e\C-q" 'indent-pp-sexp)
    ;;(define-key map "\177" 'backward-delete-char-untabify)
    (set-keymap-parent map minibuffer-local-map)
    (setq icicle-read-expression-map map)))

(defvar icicle-re-no-dot "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regexp that matches anything except `.' and `..'.")

(defvar icicle-require-match-p nil
  "Current REQUIRE-MATCH arg to `completing-read' or `read-file-name'.")

(defvar icicle-reverse-sort-p nil
  "Non-nil means that candidates are being sorted in the reverse order.")

(defvar icicle-saved-candidate-overlays nil
  "Overlays used to highlight saved completion candidates.")

(defvar icicle-saved-candidates-variables-obarray (make-vector 100 0)
  "Obarray of variables you have saved sets of completion candidates in.
Used for completion in `icicle-candidate-set-retrieve-from-variable'.")

(defvar icicle-saved-completion-candidate nil
  "Completion candidate to be restored after recursive `completing-read'.")

(defvar icicle-saved-completion-candidates nil
  "Completion candidates saved using `icicle-candidate-set-save'.")

(defvar icicle-saved-completion-candidates-internal nil
  "Completion candidates saved temporarily by program.")

(defvar icicle-saved-ignored-extensions nil
  "Local copy of `icicle-ignored-extensions', so we can restore it.")

(when (boundp 'kmacro-ring)             ; Emacs 22+
  (defvar icicle-saved-kmacro-ring-max kmacro-ring-max
    "Saved value of `kmacro-ring-max', so it can be restored."))

(defvar icicle-saved-regexp-search-ring-max regexp-search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-saved-proxy-candidates nil "Saved value of `icicle-proxy-candidates'.")

(defvar icicle-saved-region-background nil
  "Background of `region' face.  Saved so it can be restored.")

(defvar icicle-saved-search-ring-max search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-scroll-Completions-backward-p nil
  "Non-nil means repeated `TAB' or `S-TAB' scrolls *Completions* backward.")

(defvar icicle-search-command 'icicle-search
  "Command to use for Icicles searches.
You can set a buffer-local value of this variable, to use a specific
search command in a particular mode.")

(defvar icicle-search-context-level 0
  "Match level for `icicle-search' context regexp.
0 means use whatever matches the whole context regexp as the search
context.  1 means use whatever matches the first subgroup of the
regexp as the search context, and so on.")

(defvar icicle-search-context-regexp ""
  "Current search-context regexp used in `icicle-search'.")

(defvar icicle-search-current-overlay nil
  "Overlay used to highlight current match of `icicle-search' regexp arg.")

(defvar icicle-search-final-choice nil
  "Final user input from `icicle-search'.
This might or might not be one of the possible search candidates.")

(defvar icicle-search-history nil "History for `icicle-search' final choices.")

(defvar icicle-search-in-context-fn 'icicle-search-in-context-default-fn
  "Function used by `icicle-search-action' to act on search context.
The default value is `icicle-search-in-context-default-fn'.
The function must take two arguments:
 - A full search candidate object, which is a cons of the candidate
   name and its source-file marker.
 - A replacement string, or nil, if no replacement is to be made.

When the function is called, the region is narrowed to the current
search context.")

(defvar icicle-searching-p nil "Non-nil means an Icicles search command is in progress.")

(defvar icicle-search-level-overlays nil
  "Overlays used to highlight context levels other than the top level.")

(defvar icicle-search-overlays nil
  "Overlays used to highlight match of `icicle-search' regexp argument.")

(defvar icicle-search-refined-overlays nil
  "Overlay(s) used to highlight match of current input for `icicle-search'.
If `icicle-search-highlight-threshold' is less than one, then this is
a single overlay (or nil).  Otherwise, this is a list of overlays.")

(defvar icicle-search-replacement nil
  "Replacement string for use during `icicle-search'.")

(defvar icicle-search-replacement-history nil
  "History variable for reading replacement string for `icicle-search'.")

(defvar icicle-successive-grab-count 0
  "Number of text things to be grabbed by next `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]'.")

(defvar icicle-text-property-value-history nil
  "History variable for reading text properties.")

;; (defvar icicle-text-properties-alist
;;   '(;; Properties listed in Elisp manual node `Special Properties':
;;     ("category") ("face") ("font-lock-face") ("mouse-face") ("fontified") ("display")
;;     ("help-echo") ("keymap") ("local-map") ("syntax-table") ("read-only") ("invisible")
;;     ("intangible") ("field") ("cursor") ("pointer") ("line-spacing") ("line-height")
;;     ("modification-hooks") ("insert-in-front-hooks") ("insert-behind-hooks")
;;     ("point-entered") ("point-left")
;;     ;; Properties listed in Elisp manual node `Format Properties':
;;     ("hard") ("right-margin") ("left-margin") ("justification")
;;     ;; Properties listed in Elisp manual node `Links and Mouse-1':
;;     ("follow-link")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp':
;;     ("allout-was-hidden") ("ansi-color") ("buffer") ("buffer-name") ("column")
;;     ("button") ("skip") ("literal") ("front-sticky") ("rear-nonsticky") ("composition")
;;     ("untranslated-utf-8") ("yank-handler") ("dired-filename") ("read-face-name")
;;     ("directory") ("message") ("debug") ("font-lock-multiline") ("unknown")
;;     ("insert-in-front-hooks") ("kbd-help") ("hilit-chg") ("ibuffer-filter-group-name")
;;     ("ibuffer-properties") ("ibuffer-title") ("ibuffer-summary")
;;     ("ibuffer-title-header") ("inhibit-line-move-field-capture") ("image-counter")
;;     ("header-line") ("cvs-goal-column") ("occur-target") ("occur-match")
;;     ("foreign-selection") ("before-string") ("after-string") ("ses")
;;     ("smerge-force-highlighting") ("speedbar-function") ("speedbar-token")
;;     ("speedbar-text") ("type") ("stroke-glyph") ("data") ("thumb-image-file")
;;     ("original-file-name") ("associated-dired-buffer") ("tags") ("comment")
;;     ("tumme-thumbnail") ("tutorial-remark") ("vc-cvs-annotate-time") ("end-name")
;;     ("old-name") ("end-link") ("old-link") ("end-perm") ("old-perm") ("perm-changed")
;;     ("widget-doc") ("secret") ("real-field")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/emacs-lisp':
;;     ("elp-symname") ("printed-value") ("duplicable")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/emulation':
;;     ("cursor")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/erc':
;;     ("erc-callback") ("erc-data") ("erc-identified") ("erc-parsed") ("erc-parsed")
;;     ("timestamp") ("erc-prompt")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/eshell':
;;     ("comment") ("arg-begin") ("arg-end") ("escaped") ("history") ("number")
;;     ("test-func")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/gnus':
;;     ("earcon-data") ("earcon-callback") ("gnus-category") ("gnus-part")
;;     ("article-type") ("gnus-decoration") ("dummy-invisible") ("original-date")
;;     ("gnus-data") ("gnus-callback") ("gnus-prev") ("gnus-next") ("gnus-mime-details")
;;     ("gnus-line-format") ("gnus-backlog") ("gnus-image-category")
;;     ("gnus-image-text-deletable") ("gnus-group") ("gnus-level") ("gnus-indentation")
;;     ("gnus-unread") ("gnus-number") ("articles") ("gnus-server") ("gnus-named-server")
;;     ("gnus-intangible") ("gnus-topic") ("gnus-topic-level") ("gnus-topic-unread")
;;     ("gnus-topic-visible") ("gnus-active") ("gnus-position") ("gnus-time")
;;     ("gnus-face") ("gnus-undeletable") ("message-rank") ("egg-end") ("egg-lang")
;;     ("egg-start") ("message-hidden") ("message-deletable") ("buffer") ("from") ("mm")
;;     ("script-name")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/international':
;;     ("kkc-conversion-index") ("advice") ("untranslated-utf-8") ("composition")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/mail':
;;     ("footnote-number") ("rmail-fontified")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/mh-e':
;;     ("mh-data") ("mh-mime-inserted") ("mh-part") ("mh-region") ("mh-callback")
;;     ("mh-button-pressed") ("mh-line-format") ("mh-folder") ("mh-children-p")
;;     ("mh-expanded") ("mh-level") ("mh-count")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/net':
;;     ("feed") ("w3m-image") ("nt-age") ("nt-title") ("nt-guid") ("nt-desc")
;;     ("org-invisible") ("nt-link") ("nt-type") ("nt-face")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/progmodes':
;;     ("c-type") ("c-awk-NL-prop") ("c-is-sws") ("c-decl-arg-start") ("c-decl-end")
;;     ("c-decl-id-start") ("c-decl-type-start") ("message") ("REx-interpolated")
;;     ("in-pod") ("here-doc-group") ("syntax-type") ("indentable") ("REx-part2")
;;     ("first-format-line") ("attrib-group") ("cperl-postpone") ("cpp-data")
;;     ("cpp-callback") ("token") ("ebrowse-tree") ("ebrowse-member") ("ebrowse-what")
;;     ("gdb-enabled") ("gdb-bptno") ("gdb-max-frames") ("link") ("fetch") ("begin-glyph")
;;     ("begin-glyph-layout") ("idlwave-class") ("data") ("source") ("keyword")
;;     ("find-args")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/term':
;;     ("mac-ts-active-input-string")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/textmodes':
;;     ("fill-space") ("priority") ("test") ("end-glyph") ("begin-glyph") ("org-cwidth")
;;     ("org-dwidth") ("org-dwidth-n") ("org-linked-text") (":org-clock-minutes")
;;     ("org-protected") ("org-date-line") ("org-today") ("day") ("org-agenda-type")
;;     ("time-of-day") ("org-not-done-regexp") ("prefix-length") ("tags") ("org-marker")
;;     ("org-agenda-diary-link") ("org-hd-marker") ("dotime") ("org-category")
;;     ("undone-face") ("done-face") ("xr-alist") ("table-cell") ("text-clones")
;;     ;; Others in my own libraries:
;;     ("font-lock-ignore") ("highlight") ("back-link") ("forward-link"))
;;   "Alist of text properties known to Emacs.
;; Each element is of form (PROP), where PROP is the name of a text
;; property (a string).")

(defvar icicle-thing-at-pt-fns-pointer 0
  "Current index into the car of `icicle-thing-at-point-functions'.
This points to the current function in the list.")

(defvar icicle-universal-argument-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'icicle-universal-argument-other-key)
    (define-key map (vector meta-prefix-char t) 'icicle-universal-argument-other-key)
    (define-key map [switch-frame] nil)
    (define-key map [?\C-u] 'icicle-universal-argument-more)
    (define-key map [?-] 'icicle-universal-argument-minus)
    (define-key map [?0] 'icicle-digit-argument)
    (define-key map [?1] 'icicle-digit-argument)
    (define-key map [?2] 'icicle-digit-argument)
    (define-key map [?3] 'icicle-digit-argument)
    (define-key map [?4] 'icicle-digit-argument)
    (define-key map [?5] 'icicle-digit-argument)
    (define-key map [?6] 'icicle-digit-argument)
    (define-key map [?7] 'icicle-digit-argument)
    (define-key map [?8] 'icicle-digit-argument)
    (define-key map [?9] 'icicle-digit-argument)
    (define-key map [kp-0] 'icicle-digit-argument)
    (define-key map [kp-1] 'icicle-digit-argument)
    (define-key map [kp-2] 'icicle-digit-argument)
    (define-key map [kp-3] 'icicle-digit-argument)
    (define-key map [kp-4] 'icicle-digit-argument)
    (define-key map [kp-5] 'icicle-digit-argument)
    (define-key map [kp-6] 'icicle-digit-argument)
    (define-key map [kp-7] 'icicle-digit-argument)
    (define-key map [kp-8] 'icicle-digit-argument)
    (define-key map [kp-9] 'icicle-digit-argument)
    (define-key map [kp-subtract] 'icicle-universal-argument-minus)
    map)
  "Keymap used while processing `C-u' during Icicles completion.")

(defvar icicle-whole-candidate-as-text-prop-p nil
  "Non-nil means string candidate has candidate data as text property.
If non-nil, then the value of text property `icicle-whole-candidate'
for a string completion candidate (e.g. what is displayed) is the cdr
of the full completion-candidate alist element.  The car of that
element is the string.")

(defvar icicle-variable-name-history nil "History for variable names.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-var)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-var.el ends here
