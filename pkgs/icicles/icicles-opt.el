;;; icicles-opt.el --- User options (variables) for Icicles
;;
;; Filename: icicles-opt.el
;; Description: User options (variables) for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:22:14 2006
;; Version: 22.0
;; Last-Updated: Sun Oct 14 15:35:31 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 1196
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-opt.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `color-theme', `cus-face', `easymenu', `ffap', `ffap-',
;;   `hexrgb', `thingatpt', `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  user options (variables).  See `icicles.el' for documentation.
;;
;;  User options defined here (in Custom group `Icicles'):
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
;;    `icicle-modal-cycle-up-key',
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
;;  Functions defined here:
;;
;;    `icicle-buffer-sort-*...*-last', `icicle-increment-color-hue',
;;    `icicle-increment-color-saturation',
;;    `icicle-increment-color-value'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Change log")
;;  (@> "User options, organized alphabetically, except for dependencies")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2007/10/14 dadams
;;     icicle-act-before-cycle-flag: Default value is now nil.
;; 2007/10/07 dadams
;;     Added: icicle-deletion-action-flag.
;; 2007/09/28 dadams
;;     Added: icicle-fuzzy-completion-flag.
;; 2007/08/25 dadams
;;     Added: icicle-anything-transform-candidates-flag, icicle-define-alias-commands-flag.
;; 2007/08/19 dadams
;;     Added: icicle-highlight-input-completion-failure-flag.
;; 2007/08/16 dadams
;;     icicle-region-alist, icicle-sort-functions-alist: Use alist as :type for recent Emacs.
;; 2007/07/27 dadams
;;     Added: Moved icicle-act-first-then-navigate-p here from icicles-var.el, as option *-flag.
;;            Thx to Juri Linkov for suggestion.
;; 2007/07/03 dadams
;;     Added: icicle-completion-history-max-length, icicle-C-l-uses-completion-flag.
;;     icicle-expand-input-to-common-match-flag: Updated doc string accordingly.
;; 2007/06/20 dadams
;;     icicle-WYSIWYG-Completions-flag: Use string value, not whole number, for sample text.
;; 2007/06/19 dadams
;;     icicle-WYSIWYG-Completions-flag: Allow also a whole-number value, for separate swatch.
;; 2007/06/17 dadams
;;     Added: icicle-WYSIWYG-Completions-flag.
;; 2007/06/12 dadams
;;     icicle-region-background: Use different value for dark-background frames.
;; 2007/06/09 dadams
;;     Added: icicle-use-candidates-only-once-flag.
;; 2007/06/05 dadams
;;     Don't require hexrgb.el if no window system.
;;     icicle-increment-color-*: Protected with featurep hexrgb and error message.
;; 2007/05/06 dadams
;;     Added: icicle-search-context-match-predicate.
;; 2007/05/02 dadams
;;     Added: icicle-search-whole-word-flag, icicle-yank-function.
;;     icicle-regexp-quote-flag: Updated doc string to mention search.
;; 2007/04/20 dadams
;;     Added: icicle-search-highlight-context-levels-flag, icicle-increment-color-saturation.
;; 2007/04/17 dadams
;;     Added: icicle-search-replace-literally-flag.
;; 2007/04/15 dadams
;;     icicle-search-replace-whole-candidate-flag: Changed default value to t.
;; 2007/04/07 dadams
;;     Added: icicle-search-replace-whole-candidate-flag.
;; 2007/03/31 dadams
;;     Added: icicle-top-level-when-sole-completion-flag.
;;     icicle(-regexp)-search-ring-max: Default is 1/10th what it was.
;; 2007/03/30 dadams
;;     Added: icicle-candidate-width-factor, icicle-inter-candidates-min-spaces.
;; 2007/03/12 dadams
;;     Added: icicle-Completions-window-max-height.
;;     icicle-show-Completions-help-flag: defvaralias it to completion-show-help (if bound).
;; 2007/03/10 dadams
;;     Added: icicle-Completions-window-default-width.
;; 2007/02/27 dadams
;;     Changed default value of icicle-incremental-completion-delay to 0.7.
;; 2007/02/22 dadams
;;     icicle-buffer-sort: Use icicle-buffer-sort-*...*-last, not nil, as default value.
;; 2007/02/20 dadams
;;     Changed icicle-region-auto-open-files-flag default value to nil.
;; 2007/02/19 dadams
;;     icicle-region-alist: Added buffer's file to alist entry.
;;     Added: icicle-region-auto-open-files-flag.
;; 2007/02/17 dadams
;;     Added: icicle-keymaps-for-key-completion.
;; 2007/02/06 dadams
;;     Added: icicle-add-buffer-name-flag.
;; 2007/01/29 dadams
;;     icicle-sort-function: Use icicle-case-string-less-p as value, not string-lessp.
;; 2007/01/23 dadams
;;     Added: icicle-highlight-historical-candidates-flag.
;;     Updated doc strings of *-flag to mention toggles.
;; 2007/01/18 dadams
;;     Renamed: icicle-regions to icicle-region-alist.
;; 2007/01/15 dadams
;;     Added: icicle-change-sort-order-completion-flag, icicle-sort-functions-alist.
;;     icicle-cycle-into-subdirs-flag, icicle-sort-function: Updated doc string.
;; 2007/01/14 dadams
;;     Added: icicle-list-nth-parts-join-string.
;; 2007/01/08 dadams
;;     icicle-reminder-prompt-flag: Reduced default value from 20 to 7 Emacs sessions.
;; 2007/01/06 dadams
;;     Added: icicle-use-~-for-home-dir-flag.  Thanks to Timothy Stotts for the suggestion.
;; 2006/12/29 dadams
;;     icicle-thing-at-point-functions: Added ffap-guesser as first alternative text grabber.
;;     icicle-default-thing-insertion: Changed default value to icicle-default-thing-insertion.
;; 2006/12/25 dadams
;;     Moved to icicles-fn.el: icicle-historical-alphabetic-p.
;; 2006/12/22 dadams
;;     Assigned Icicles subgroups, instead of group Icicles.
;; 2006/12/10 dadams
;;     icicle-regions: Corrected (forgot repeat).
;; 2006/11/26 dadams
;;     Added: icicle-regions(-name-length-max).
;; 2006/11/24 dadams
;;     Added: icicle-kmacro-ring-max.
;; 2006/11/23 dadams
;;     Added: icicle-TAB-shows-candidates-flag.  Thx to Tamas Patrovics for the suggestion.
;; 2006/11/09 dadams
;;     icicle-search-highlight-all-flag -> icicle-search-highlight-threshold.
;;     Added: icicle-search-highlight-all-current-flag.
;; 2006/10/28 dadams
;;     icicle-region-background: Changed :type to 'color for Emacs 21+.
;;     icicle(-alternative)-sort-function, icicle-buffer-sort, icicle-transform-function:
;;       function -> choice of nil or function.
;;     icicle-buffer-configs: Added :tag's.
;;     icicle-saved-completion-sets: Corrected doc string.
;; 2006/10/21 dadams
;;     Added: icicle-complete-keys-self-insert-flag.
;; 2006/10/14 dadams
;;     icicle-list-end-string: Added :type and :group.
;;     Moved conditional eval-when-compile to top level.
;; 2006/10/04 dadams
;;     Added: icicle-special-candidate-regexp.
;; 2006/10/03 dadams
;;     icicle-list-join-string: Replaced ^G^J by \007\012, to be able to upload to Emacs Wiki.
;; 2006/10/01 dadams
;;     icicle-alternative-sort-function: Updated doc string - it's now a general toggle.
;; 2006/09/30 dadams
;;     Added: icicle-key-descriptions-use-*-flag.
;; 2006/09/16 dadams
;;     Added: icicle-list-end-string.
;; 2006/09/03 dadams
;;     Renamed icicle-show-Completions-help to icicle-show-Completions-help-flag.
;; 2006/08/13 dadams
;;     Added: icicle-completing(-mustmatch)-prompt-prefix.
;; 2006/07/28 dadams
;;     icicle-change-region-background-flag:
;;       Default value takes no account of delete-selection mode.  Improved doc string.
;;     icicle-region-background:
;;       Don't make region invisible if hexrgb.el was not loaded.
;;       Change value, not hue, if grayscale frame background.  Improved doc string.
;; 2006/07/23 dadams
;;     Added: icicle-transform-function.
;;     icicle-sort-function: Added Note to doc string.
;; 2006/07/20 dadams
;;     Added: icicle-modal-cycle-(up|down)-key.
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added: icicle-show-completions-help.  Renamed it to icicle-show-Completions-help.
;; 2006/07/18 dadams
;;     Added: icicle-Completions-display-min-input-chars.  Thx to Damien Elmes.
;; 2006/07/10 dadams
;;     icicle-historical-alphabetic-p: Fallback directory if no previous input.
;; 2006/07/07 dadams
;;     Added: icicle-alternative-sort-function, icicle-historical-alphabetic-p.
;; 2006/07/04 dadams
;;     icicle-expand-input-to-common-match-flag: Updated doc string.
;; 2006/06/09 dadams
;;     icicle-region-background: Use nil in defcustom.  Initialize separately.
;; 2006/06/08 dadams
;;     icicle-bind-top-level-commands-flag: Updated doc string.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;       Changed its functionality to use a countdown.
;; 2006/05/16 dadams
;;     Added: icicle-bind-top-level-commands-flag.
;; 2006/05/15 dadams
;;     Renamed: icicle-completion-nospace-flag to icicle-ignore-space-prefix-flag.
;;     Added: icicle-buffer-ignore-space-prefix-flag.
;;     icicle-ignore-space-prefix-flag: Changed default value to nil.
;; 2006/05/09 dadams
;;     icicle-incremental-completion-threshold: Updated doc string (msg "Displaying...").
;; 2006/04/28 dadams
;;     Added: icicle-highlight-input-initial-whitespace-flag.
;; 2006/04/14 dadams
;;     Added: icicle-input-string, icicle-search-cleanup-flag, icicle-update-input-hook.
;;     icicle-list-join-string: Added :type and :group.
;; 2006/04/09 dadams
;;     Added: icicle-arrows-respect-completion-type-flag.
;; 2006/04/07 dadams
;;     Added: icicle-search-highlight-all-flag.
;; 2006/04/02 dadams
;;     Added: icicle-regexp-quote-flag.
;; 2006/03/24 dadams
;;     Added: icicle-incremental-completion-(delay|threshold).
;; 2006/03/20 dadams
;;     icicle-expand-input-to-common-match-flag: Changed default value to t.
;; 2006/03/19 dadams
;;     Added: icicle-expand-input-to-common-match-flag.
;; 2006/03/17 dadams
;;     Removed: icicle-cache-file.
;;     Added: icicle-saved-completion-sets.
;; 2006/03/13 dadams
;;     Added: icicle-cache-file.
;; 2006/03/08 dadams
;;     icicle-default-thing-insertion: Use substitute-command-keys in :tag.
;; 2006/03/05 dadams
;;     Moved from here to icicle-mode.el: icicle-mode, icicle-mode-hook.
;;     Added: icicle-touche-pas-aux-menus-flag.
;; 2006/03/03 dadams
;;     icicle-list-join-string: Changed value to ^G^J.  Clarified doc string.
;;
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

(eval-when-compile (when (< emacs-major-version 20) (require 'cl))) ;; when, unless

(when window-system (require 'hexrgb nil t))
                            ;; (no error if not found): hexrgb-color-values-to-hex,
                            ;; hexrgb-increment-(red|green|blue), hexrgb-rgb-to-hsv,
                            ;; hexrgb-color-values-to-hex, hexrgb-hsv-to-rgb
(require 'thingatpt)        ;; symbol-at-point, thing-at-point, thing-at-point-url-at-point,
(require 'thingatpt+ nil t) ;; (no error if not found): symbol-name-nearest-point,
                            ;; word-nearest-point

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "User options, organized alphabetically, except for dependencies")

;;; User options, organized alphabetically, except for dependencies --

;;;###autoload
(defcustom icicle-act-before-cycle-flag nil
  "*Nil means cycle to the next or previous candidate, and then act on it.
Non-nil means act on current candidate, then cycle to next or previous.

This affects keys such as the following\\<minibuffer-local-completion-map>:
`C-down', `C-up', `C-next', `C-prior', \
`\\[icicle-help-on-next-prefix-candidate]', `\\[icicle-help-on-previous-prefix-candidate]',
`\\[icicle-help-on-next-apropos-candidate]', `\\[icicle-help-on-previous-apropos-candidate]', \
`\\[icicle-next-prefix-candidate-alt-action]', \
`\\[icicle-previous-prefix-candidate-alt-action]', \
`\\[icicle-next-apropos-candidate-alt-action]', and
`\\[icicle-previous-apropos-candidate-alt-action]'.

Note: A few Icicles commands ignore this setting, in order to \"do the
right thing\"."
  :type 'boolean :group 'Icicles-Key-Bindings :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-add-buffer-name-flag t
  "*Non-nil means to add the buffer name to completion candidates.
This means that for some Icicles commands, such as `icicle-search' and
`icicle-select-region', the normal completion candidate is treated as
a multi-completion: the name of the buffer associated with it is added
to the candidate and highlighted.

The main advantage is that you can easily see which buffer a candidate
applies to.  Also, the buffer name is part of the candidate, so you
can match against it.

Note that even when the value is nil, you can use `C-M-mouse-2' and so
on to see information about a candidate, and this information includes
its buffer name whenever a non-nil value would have shown the buffer
name."
  :type 'boolean :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-alternative-sort-function 'icicle-historical-alphabetic-p
  "*An alternative sort function, in place of `icicle-sort-function'.
You can swap this with `icicle-sort-function' at any time by using
`icicle-toggle-alternative-sorting' (`\\<minibuffer-local-completion-map>\
\\[icicle-toggle-alternative-sorting]' in the minibuffer)."
  :type '(choice (const :tag "None" nil) function) :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-anything-transform-candidates-flag nil
  "*Non-nil means `icicle-anything' transforms completion candidates.
Function `anything-transform-candidates' is used for the transforming.

The advantage of a nil value is that `icicle-anything' then acts as a
multi-command: you can act on multiple candidates, or apply multiple
actions for the same candidate, within a single invocation of
`icicle-anything' (or related commands).

The advantage of a non-nil value is that the displayed candidates
might be more readable."
  :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-bind-top-level-commands-flag t
  "*Non-nil means to bind top-level Icicles commands.
That is done in `icicle-define-icicle-mode-map'.
If you change this from t to nil, then you must restart Emacs for it
to take effect.  Going from nil to t requires no restart.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-buffer-extras nil
  "*List of additional buffer-name candidates added to the normal list.
List elements are strings."
  :type '(repeat string) :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-ignore-space-prefix-flag t
  "*Override `icicle-ignore-space-prefix-flag' for `icicle-buffer*'.
Note: This option is provided mainly for use (binding) in
      `icicle-define-command' and `icicle-define-file-command'.
      You probably do not want to set this globally, but you can."
  :type 'boolean :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-match-regexp nil
  "*Nil or a regexp that buffer-name completion candidates must match.
If nil, then this does nothing.  If a regexp, then show only
candidates that match it (and match the user input).
See also `icicle-buffer-no-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-no-match-regexp nil
  "*Nil or a regexp that buffer-name completion candidates must not match.
If nil, then this does nothing.  If a regexp, then show only
candidates that do not match it.
See also `icicle-buffer-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-predicate nil
  "*Nil or a predicate that buffer-name candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.  For example, this value will show only buffers that
are associated with files:

  (lambda (bufname) (buffer-file-name (get-buffer bufname)))."
  :type '(choice (const :tag "None" nil) function)
  :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-require-match-flag nil
  "*Override `icicle-require-match-flag' for `icicle-buffer*' commands.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
      `icicle-define-command' and `icicle-define-file-command'.
      You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"  nil)
          (const :tag "Do not require a match"            no-match-required)
          (const :tag "Require a partial match, with RET" partial-match-ok)
          (const :tag "Require a full match"              full-match-required))
  :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-sort 'icicle-buffer-sort-*...*-last
  "*Nil or a sort function for buffer names.
Examples of sort functions are `icicle-buffer-sort-*...*-last' and
`string<'.  If nil, then buffer names are not sorted.  Option
`icicle-sort-function' is bound to `icicle-buffer-sort' by command
`icicle-buffer'."
  :type '(choice (const :tag "None" nil) function)
  :group 'Icicles-Buffers :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-candidate-width-factor 70
  "*Percentage of widest candidate width to use for calculating columns.
The number of columns of candidates displayed in *Completions* is no
more than the window width divided by this percentage of the maximum
candidate width.

Increasing this toward 100 spreads columns out. Decreasing it
compresses columns together.  The higher the value, the more
candidates will form well-defined columns, but the likelier that
horizontal space will be wasted between them.  The lower the value,
the more candidates will not line up in columns, but the less
horizontal space will be wasted between them.

When most candidates are almost as wide as the widest candidate, a
high value works well.  When most candidates are much shorter than the
widest candidate, a low value works well."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-change-sort-order-completion-flag nil
  "*Non-nil means `icicle-change-sort-order' uses completion, by default.
Otherwise, it cycles among the possible sort orders.  You can override
the behavior by using `C-u' with `icicle-change-sort-order'."
  :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-C-l-uses-completion-flag nil
  "*Non-nil means \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]' uses completion for choosing completion history
entries, by default.  Otherwise, it cycles among the possible previous
inputs.  You can override the behavior by using `C-u' with `\\[icicle-retrieve-previous-input]'."
  :type 'boolean :group 'Icicles-Minibuffer-Display :group 'Icicles-Matching)

;; Replace this list by your favorite color themes. Each must be the name of a defined function.
;; By default, this includes all color themes defined globally (variable `color-themes').
;;;###autoload
(defcustom icicle-color-themes
  (and (require 'color-theme nil t)
       (delq 'bury-buffer
             (mapcar (lambda (entry) (list (symbol-name (car entry)))) color-themes)))
  "*List of color themes to cycle through using `M-x icicle-color-theme'."
  :type 'hook :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-complete-keys-self-insert-flag nil
  "*Non-nil means `icicle-complete-keys' includes self-inserting keys.
That means keys bound to `self-insert-command'."
  :type 'boolean :group 'Icicles-Key-Completion)

;;;###autoload
(defcustom icicle-completing-mustmatch-prompt-prefix "="
  "*Prompt prefix to use to indicate must-match completing is possible.
A space is automatically appended to this string, if non-empty.
If you don't want this indicator, set this option to \"\".
Not used for versions of Emacs before Emacs 21."
  :type 'string :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-completing-prompt-prefix " "
  "*Prompt prefix to use to indicate completing is possible.
A space is automatically appended to this string, if non-empty.
If you don't want this indicator, set this option to \"\".
Not used for versions of Emacs before Emacs 21."
  :type 'string :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-completion-history-max-length (if icicle-C-l-uses-completion-flag 1000 100)
  "Maximum number of inputs to save in the completion history.
This is the history that you access using \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]' and `\\[icicle-retrieve-next-input]'."
  :type 'integer :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-Completions-display-min-input-chars 0
  "**Completions* window is removed if fewer chars than this are input.
You might want to set this to, say 1 or 2, to avoid display of a large
set of candidates during incremental completion.  The default value of
0 causes this option to have no effect: *Completions* is never removed
based only on the number of input characters."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-Completions-frame-at-right-flag t
  "*Non-nil means move *Completions* frame to right edge of display.
This is done by `icicle-candidate-action'.
It only happens if *Completions* is alone in its frame.
This can be useful to make *Completions* more visible."
  :type 'boolean :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-Completions-window-default-width 80
  "*Default width of *Completions* window.
This is not used if any of these conditions is true:
- *Completions* is already displayed in some window.
- *Completions will be displayed in some existing window.
- *Completions* is a special buffer with its own frame.
If you use a standalone frame, then its specified width is used.
If *Completions* is already displayed, or it will be displayed in an
existing window, then its width is not changed."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-Completions-window-max-height 30
  "*Maximum height of *Completions* window, in lines.
The window is fit to the buffer size, with this as maximum height.
Not used if *Completions* is a special buffer with its own frame.
Not used in Emacs releases prior to 21."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-cycle-into-subdirs-flag nil
  "*Non-nil means minibuffer-input cycling explores subdirectories.
If this is non-nil, then you might want to use a function such as
`icicle-dirs-last-p' for option `icicle-sort-function', to prevent
cycling into subdirectories depth first.  Command
`icicle-sort-by-directories-last' does that."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-cycling-respects-completion-mode-flag nil
  "*Non-nil means `TAB' and `S-TAB' change the behavior of cycling keys.
Nil means that `up' and `down' always cycle prefix completions.
Non-nil means that `up' and `down':
 - Traverse the input history, by default.
 - Cycle prefix completions, if preceded by `TAB'.
 - Cycle apropos completions, if preceded by `S-TAB'.

If this option is non-nil you can still use `M-p' and `M-n' to
traverse the input history, `C-p' and `C-n' to cycle prefix
completions, and `prior' and `next' to cycle apropos completions.  If
you do that, you need not use `TAB' and `S-TAB' to switch between the
two completion types.  Once you have used `TAB' or `S-TAB', the only
way to traverse the history is via `M-p' and `M-n'."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-default-thing-insertion 'alternatives
  "*Behavior of successive `\\<minibuffer-local-map>\\[icicle-insert-string-at-point]'.
If `alternatives', then the next function in the `car' of
`icicle-thing-at-point-functions' is used to retrieve the text to be
inserted.
If `more-of-the-same', then the function that is the `cdr' of
`icicle-thing-at-point-functions' is used to retrieve the text to be
inserted."
  :type `(choice
          (const :tag ,(substitute-command-keys
                        "Successive calls to `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]' use different text-grabbing functions.")
           alternatives)
          (const :tag ,(substitute-command-keys
                        "Successive calls to `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]' grab more text at point.")
           more-of-the-same))
  :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-define-alias-commands-flag t
  "*Non-nil means define some commands that do not begin with `icicle-'.
For convenience, a few top-level commands are defined, typically as
aliases for commands with longer names.  For example, command `toggle'
is defined as an alias for command `icicle-toggle-option'.  In any
case, no such command is ever defined by Icicles if a function with
the same name is already defined.")

;;;###autoload
(defcustom icicle-deletion-action-flag t
  "*Non-nil means `S-delete' during completion deletes the current object.
More precisely, it deletes the object named by the current completion
candidate, if a deletion action is defined for the current command.
If no deletion action is defined, then the value of this option has no
effect.

If you are worried about inadvertently deleting an object by
accidentally hitting `S-delete', you can customize this to nil to
inhibit `S-delete' object deletion during completion.")

;;;###autoload
(defcustom icicle-expand-input-to-common-match-flag t
  "*Non-nil means that `\\<minibuffer-local-completion-map>\\[icicle-apropos-complete]' \
expands your minibuffer input to the
longest common match among all completion candidates.  This replaces
your input, completing it as far as possible.

If you want to edit your original input, use \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]'.

For apropos completion, your input is, in general, a regexp.  Setting
this option to nil will let you always work with a regexp in the
minibuffer for apropos completion - your regexp is then never replaced
by the longest common match."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-fuzzy-completion-flag nil
  "*Non-nil means use fuzzy prefix completion for \
`\\<minibuffer-local-completion-map>\\[icicle-prefix-complete]'.
This has no effect if library `fuzzy-match.el' is not used (loaded).
If non-nil, then `TAB' completes non-filename input using fuzzy
prefix matching as defined in `fuzzy-match.el'.  See `fuzzy-match.el'
for details.

This option has no effect on file-name completion.  Fuzzy prefix
completion is always case-sensitive, and leading spaces are taken into
account.  Completion candidates are always sorted by decreasing fuzzy
match strength.  That is, fuzzy completion is not affected by
`\\[icicle-toggle-case-sensitivity]', `C-^', or `C-,'.

You can toggle this option from the minibuffer at any time with \
`\\[icicle-toggle-fuzzy-completion]'."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-highlight-historical-candidates-flag t
  "*Non-nil means highlight  *Completions* candidates that have been used.
This is done using face `icicle-historical-candidate'.
Historical candidates are those that you have entered (using `RET' or
`S-RET') previously.  You can toggle this option from the minibuffer
at any time using `C-pause'."
  :type 'boolean :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-highlight-input-completion-failure-flag 'lax-and-strict
  "*Non-nil means highlight the part of your input that does not complete.
This is done using face `icicle-input-completion-fail'.
You can use `\\<minibuffer-local-completion-map>\\[icicle-kill-failed-input]' to kill the \
highlighted part.

Note: For this highlighting to occur, option
`icicle-incremental-completion-flag' must also be non-nil.  You can
toggle `icicle-incremental-completion-flag' using `C-#' in the
minibuffer.  One reason you might want to turn this off is that when
it is non-nil Icicles might recompute completion candidates multiple
times, in order to determine the longest part that completes.  Also,
incremental completion is effectively turned off when a remote file
name is read, so no highlighting occurs then either."
  :type '(choice
          (const :tag "Never"                            nil)
          (const :tag "Only during strict completion"    strict-only)
          (const :tag "During lax and strict completion" lax-and-strict))
  :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-highlight-input-initial-whitespace-flag t
  "*Non-nil means highlight initial whitespace in your input.
This is done using face `icicle-whitespace-highlight'.
Purpose: Otherwise, you might not notice that you accidentally typed
some whitespace at the beginning of your input, so you might not
understand the set of matching candidates (or lack thereof)."
  :type 'boolean :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-ignore-space-prefix-flag nil
  "*Non-nil means ignore completion candidates that start with a space.
However, such candidates are not ignored for prefix completion when
the input also starts with a space.  You can toggle this option from
the minibuffer (except during searching with `C-c `') using `C-^'.
Note: Some Icicles functionalities ignore the value of this option."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-incremental-completion-delay 0.7
  "*Number of seconds to wait before updating *Completions* incrementally.
There is no wait if the number of completion candidates is less than
or equal to `icicle-incremental-completion-threshold'.
See also `icicle-incremental-completion-flag'."
  :type 'number :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-incremental-completion-flag t
  "*Non-nil means update *Completions* buffer incrementally, as you type.
nil means do not update *Completions* buffer incrementally, as you type.
t means do nothing if *Completions* is not already displayed.
Non-nil and non-t means display *Completions* and update it.
You can toggle this between t and nil from the minibuffer at any time
using `C-#'.

Note: Incremental completion is effectively turned off when a remote
file name is read, that is, whenever your file-name input matches a
remote-file syntax.

See also `icicle-incremental-completion-delay' and
`icicle-incremental-completion-threshold'."
  :type 'boolean :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-incremental-completion-threshold 1000
  "*More candidates means apply `icicle-incremental-completion-delay'.
See also `icicle-incremental-completion-flag' and
`icicle-incremental-completion-delay'.
This threshold is also used to decide when to display the message
 \"Displaying completion candidates...\"."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-init-value-flag nil
  "*Non-nil means to use default value as init value when reading input.
This is used by `completing-read', `read-file-name', `read-string',
and `read-from-minibuffer'.  When the default-value argument to one of
these functions is non-nil and the initial-input argument is nil or
\"\", the default value is inserted in the minibuffer as the initial
input.

This has the advantage of not requiring you to use `M-n' to
retrieve the default value.  It has the disadvantage of making
you use `M-p' (or do something else) to get rid of the default
value in the minibuffer if you do not want to use or edit it.  If
you often want to use or edit the default value, then set
`icicle-init-value-flag' to non-nil; if you rarely do so, then
set it to nil.

The particular non-nil value determines whether or not the value is
preselected and, if preselected, where the cursor is left: at the
beginning or end of the value.  Possible values:

  nil               - Do not insert default value.
  `insert'          - Insert default value (leave cursor at end).
  `preselect-start' - Insert and preselect default value;
                      leave cursor at beginning.
  `preselect-end'   - Insert and preselect default value;
                      leave cursor at end.

My own preference is `insert'.  This is not the value by default only
because people are not used to it.  I recommend that you try `insert'
for a while, before giving up on it.

Preselection can be useful in Delete Selection mode or PC Selection
mode.  It makes it easy to replace the value by typing characters, or
delete it by hitting `C-d' or `DEL' (backspace).  However, all of the
initial input is lost if you type or hit `C-d' or `DEL'.  That is
inconvenient if you want to keep most of it and edit it only slightly."
  :type '(choice
          (const :tag "Do not insert default value as initial value"     nil)
          (const :tag "Insert (and leave cursor at end)"                 insert)
          (const :tag "Insert, preselect, and leave cursor at beginning" preselect-start)
          (const :tag "Insert, preselect, and leave cursor at end"       preselect-end))
  :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-input-string ".*"
  "*String to insert in minibuffer via `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]'.
Typically, this is a regexp or a portion of a regexp."
  :type 'string :group 'Icicles-Miscellaneous)

(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'icicle-key-descriptions-use-angle-brackets-flag
      'icicle-key-descriptions-use-<>-flag))

;;;###autoload
(defcustom icicle-inter-candidates-min-spaces 1
  "*Minimum number of spaces between candidates displayed in *Completions*."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-key-descriptions-use-<>-flag nil
  "*Non-nil means Icicles key descriptions should use angle brackets (<>).
For example, non-nil gives `<mode-line>'; nil gives `mode-line'.

This does not affect Emacs key descriptions outside of
Icicles (e.g. `C-h k' or `C-h w').

This has no effect for versions of Emacs prior to 21, because
they never use angle brackets."
  :type 'boolean :group 'Icicles-Key-Completion :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-keymaps-for-key-completion
  '(dired-mode-map vc-dired-mode-map calendar-mode-map synonyms-mode-map)
  "*List of keymaps in which to bind `S-TAB' to `icicle-complete-keys'.
List elements are symbols that are bound to keymaps.

Each keymap should have at least one prefix key.  `S-TAB' is bound in
each keymap, so that you can use it to complete the prefix keys.

If one of the keymaps is not defined when Icicle mode is entered, then
it is ignored.  If you later define it, then just exit and reenter
Icicle mode, to bind `S-TAB' in the newly defined map.  For example,
use `M-x icy-mode' twice after entering Calendar mode, to be able to
complete `calendar-mode' prefix keys such as `A'.

Do not add `global-map' or any keymaps, such as `ctl-x-map', that are
accessible from the global keymap to the list - they are already
treated, by default.  Do not add `key-translation-map' to the list -
it won't work."
  :type '(repeat symbol) :group 'Icicles-Key-Bindings)

;;;###autoload
(when (boundp 'kmacro-ring)             ; Emacs 22
  (defcustom icicle-kmacro-ring-max (if (boundp 'most-positive-fixnum)
                                        most-positive-fixnum
                                      67108863) ; 1/2 of `most-positive-fixnum' on Windows.
    "*Icicles version of `kmacro-ring-max'."
    :type 'integer :group 'Icicles-Miscellaneous))

;;;###autoload
(defcustom icicle-list-end-string "

"
  "*String appended to a completion candidate that is a list of strings.
When a completion candidate is a list of strings, they are joined
pairwise using `icicle-list-join-string', and `icicle-list-end-string'
is appended to the joined strings.  The result is what is displayed as
a completion candidate in buffer *Completions*, and that is what is
matched by your minibuffer input.

The purpose of `icicle-list-end-string' is to allow some separation
between the displayed completion candidates.  Candidates that are
provided to input-reading functions such as `completing-read' as lists
of strings are often displayed using multiple lines of text.  If
`icicle-list-end-string' is \"\", then the candidates appear run
together, with no visual separation.

It is important to remember that `icicle-list-end-string' is part of
each completion candidate in such circumstances.  This matters if you
use a regexp that ends in `$', matching the end of the candidate."
  :type 'string :group 'Icicles-Completions-Display)

;; Note: If your copy of this file does not have the two-character string "^G^J"
;; (Control-G, Control-J) or, equivalently, \007\012, as the default value, you will want
;; to change the file to have that.  To insert these control characters in the file, use
;; `C-q'.  Emacs Wiki loses the ^G from the file, so I use \007, which works OK.
;;
;;;###autoload
(defcustom icicle-list-join-string "\007\012"
  "*String joining items in a completion that is a list of strings.
When a completion candidate is a list of strings, this string is used
to join the strings in the list, for display and matching purposes.
When completing input, you type regexps that match the strings,
separating them pairwise by the value of `icicle-list-join-string'.
Actually, what you enter is interpreted as a single regexp to be
matched against the joined strings.  Typically, the candidate list
contains two strings: a name and its doc string.

A good value for this option is a string that:
 1) does not normally occur in doc strings,
 2) visually separates the two strings it joins, and
 3) is not too difficult or too long to type.

The default value is \"^G\^J\", that is, control-g followed by
control-j (newline):
 1) ^G does not normally occur in doc strings
 2) a newline visually separates the multiple component strings, which
    helps readability in buffer *Completions*
 3) you can type the value using `C-q C-g C-q C-j'."
  :type 'string :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-list-nth-parts-join-string " "
  "*String joining candidate parts split by `icicle-list-use-nth-parts'.
This has an effect on multi-completion candidates only, and only if
the current command uses `icicle-list-use-nth-parts'.")

;;;###autoload
(defcustom icicle-mark-position-in-candidate 'input-end
  "*Position of mark when you cycle through completion candidates.
This is the mark position in the minibuffer.
Possible values are those for `icicle-point-position-in-candidate'."
  :type '(choice
          (const :tag "Leave mark at the beginning of the minibuffer input" input-start)
          (const :tag "Leave mark at the end of the minibuffer input" input-end)
          (const :tag "Leave mark at the beginning of the completion root" root-start)
          (const :tag "Leave mark at the end of the completion root" root-end))
  :group 'Icicles-Minibuffer-Display)

;; Inspired from `icomplete-minibuffer-setup-hook'.
;;;###autoload
(defcustom icicle-minibuffer-setup-hook nil
  "*Functions run at the end of minibuffer setup for Icicle mode."
  :type 'hook :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-modal-cycle-down-key [down]
  "*Key sequence to use for modal cycling to the next candidate.
The value has the same form as a key-sequence arg to `define-key'.
This is only used if `icicle-cycling-respects-completion-mode-flag' is
non-nil."
  :type 'sexp :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-modal-cycle-up-key [up]
  "*Key sequence to use for modal cycling to the previous candidate.
The value has the same form as a key-sequence arg to `define-key'.
This is only used if `icicle-cycling-respects-completion-mode-flag' is
non-nil."
  :type 'sexp :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-point-position-in-candidate 'root-end
  "*Position of cursor when you cycle through completion candidates.
This is the cursor position in the minibuffer.
Possible values are:
 `input-start': beginning of the minibuffer input
 `input-end':   end of the minibuffer input
 `root-start':  beginning of the completion root
 `root-end':    end of the completion root
When input is expected to be a file name, `input-start' is just after
the directory, which is added automatically during completion cycling.
See also `icicle-mark-position-in-candidate'."
  :type '(choice
          (const :tag "Leave cursor at the beginning of the minibuffer input" input-start)
          (const :tag "Leave cursor at the end of the minibuffer input" input-end)
          (const :tag "Leave cursor at the beginning of the completion root" root-start)
          (const :tag "Leave cursor at the end of the completion root" root-end))
  :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-change-region-background-flag
  (not (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate))
  "*Non-nil means use color `icicle-region-background' during input.
See `icicle-region-background'.  If you load library `hexrgb.el'
before Icicles, then `icicle-region-background' will be a slightly
different hue from your normal background color.  This makes
minibuffer input easier to read than if your normal `region' face were
used.  This has an effect only during minibuffer input.  A non-nil
value for this option is particularly useful if you use
delete-selection mode."
  :type 'boolean :group 'Icicles-Minibuffer-Display)

;; This is essentially a version of `doremi-increment-color-component' for hue only.
(defun icicle-increment-color-hue (color increment)
  "Increase hue component of COLOR by INCREMENT."
  (unless (featurep 'hexrgb) (error "`icicle-increment-color-hue' requires library `hexrgb.el'"))
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb (x-color-values color))
         (red   (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green (/ (float (nth 1 rgb)) 65535.0))
         (blue  (/ (float (nth 2 rgb)) 65535.0))
         (hsv (hexrgb-rgb-to-hsv red green blue))
         (hue        (nth 0 hsv))
         (saturation (nth 1 hsv))
         (value      (nth 2 hsv)))
    (setq hue (+ hue (/ increment 100.0)))
    (when (> hue 1.0) (setq hue (1- hue)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;; This is essentially a version of `doremi-increment-color-component' for saturation only.
(defun icicle-increment-color-saturation (color increment)
  "Increase saturation component of COLOR by INCREMENT."
  (unless (featurep 'hexrgb)
    (error "`icicle-increment-color-saturation' requires library `hexrgb.el'"))
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb (x-color-values color))
         (red   (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green (/ (float (nth 1 rgb)) 65535.0))
         (blue  (/ (float (nth 2 rgb)) 65535.0))
         (hsv (hexrgb-rgb-to-hsv red green blue))
         (hue        (nth 0 hsv))
         (saturation (nth 1 hsv))
         (value      (nth 2 hsv)))
    (setq saturation (+ saturation (/ increment 100.0)))
    (when (> saturation 1.0) (setq saturation (1- saturation)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;; This is essentially a version of `doremi-increment-color-component' for value only.
(defun icicle-increment-color-value (color increment)
  "Increase value component (brightness) of COLOR by INCREMENT."
  (unless (featurep 'hexrgb)
    (error "`icicle-increment-color-value' requires library `hexrgb.el'"))
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb (x-color-values color))
         (red   (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green (/ (float (nth 1 rgb)) 65535.0))
         (blue  (/ (float (nth 2 rgb)) 65535.0))
         (hsv (hexrgb-rgb-to-hsv red green blue))
         (hue        (nth 0 hsv))
         (saturation (nth 1 hsv))
         (value      (nth 2 hsv)))
    (setq value (+ value (/ increment 100.0)))
    (when (> value 1.0) (setq value (1- value)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;;;###autoload
(defcustom icicle-redefine-standard-commands-flag t
  "*Non-nil means Icicle mode redefines some standard Emacs commands."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-regexp-quote-flag nil
  "*Non-nil means special characters in regexps are escaped.
This means that no characters are recognized as special: they match
themselves.  This turns apropos completion into simple substring
completion.  It also turns Icicles searching into literal searching.
You can toggle this option from the minibuffer at any
time using `C-`'."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-regexp-search-ring-max (if (boundp 'most-positive-fixnum)
                                             (/ most-positive-fixnum 10)
                                           13421772) ; 1/10 of `most-positive-fixnum' on Windows.
  "*Icicles version of `regexp-search-ring-max'."
  :type 'integer :group 'Icicles-Searching)

;;;###autoload
(if (< emacs-major-version 21)
    (defcustom icicle-region-alist nil  ; Emacs 20
      "*Alist of regions (in any buffers).
Use commands `icicle-add-region' and `icicle-remove-region' to define
this list.

List elements have the form (STRING BUFFER FILE START END), where:
STRING is the first `icicle-regions-name-length-max' characters in the
  region.
BUFFER is the name of the buffer containing the region.
FILE is the buffer's `buffer-file-name', or nil if not a file buffer.
START and END are character positions that delimit the region."
      :type '(repeat (list
                      (string  :tag "Tag")
                      (string  :tag "Buffer name")
                      (file    :tag "File name (absolute)")
                      (integer :tag "Region start")
                      (integer :tag "Region end")
                      ))
      :group 'Icicles-Miscellaneous)
  (defcustom icicle-region-alist nil    ; Emacs 21+
    "*Alist of regions (in any buffers).
Use commands `icicle-add-region' and `icicle-remove-region' to define
this list.

List elements have the form (STRING BUFFER FILE START END), where:
STRING is the first `icicle-regions-name-length-max' characters in the
  region.
BUFFER is the name of the buffer containing the region.
FILE is the buffer's `buffer-file-name', or nil if not a file buffer.
START and END are character positions that delimit the region."
    :type '(alist
            :key-type (string :tag "Tag")
            :value-type (list
                         (string  :tag "Buffer name")
                         (file    :tag "File name (absolute)")
                         (integer :tag "Region start")
                         (integer :tag "Region end")))
    :group 'Icicles-Miscellaneous))

;;;###autoload
(defcustom icicle-region-auto-open-files-flag nil
  "*Non-nil means commands accessing `icicle-region-alist' open its files.
That is, the file of each buffer in `icicle-region-alist' that is
associated with a file is visited, without displaying it.

If this is nil, you can still open all such files at any time, using
command `icicle-region-open-all-files'."
  :type 'boolean :group 'Icicles-Miscellaneous :group 'Icicles-Searching)

;; You can use `icicle-increment-color-value' in place of `icicle-increment-color-hue', if you
;; prefer highlighting background to be slightly darker instead of a slightly different hue.
;;
;;;###autoload
(defcustom icicle-region-background nil
  "*Background color to use for region during minibuffer cycling.
This has no effect if `icicle-change-region-background-flag' is nil.
If you do not define this explicitly, and if you have loaded library
`hexrgb.el' (recommended), then this color will be slightly
different from your frame background.  This still lets you notice the
region, but it makes the region less conspicuous, so you can more
easily read your minibuffer input."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'Icicles-Minibuffer-Display)

;; Do it this way, not inside `defcustom', to avoid processing by
;; `customize-update-all' run on idle timer by `cus-edit+.el'.
(setq icicle-region-background
      (or icicle-region-background      ; Don't redefine, if user has set it.
          (if (featurep 'hexrgb)
              (let ((bg (or (and (boundp '1on1-active-minibuffer-frame-background)
                                 1on1-active-minibuffer-frame-background) ; In `oneonone.el'.
                            (cdr (assq 'background-color (frame-parameters)))
                            (face-background 'region))))
                (if (hexrgb-approx-equal (hexrgb-saturation bg) 0.0)
                    (icicle-increment-color-value bg ; Grayscale - change bg value slightly.
                                                  (if (eq frame-background-mode 'dark)
                                                      20
                                                    -10))
                  (icicle-increment-color-hue bg 24))) ; Color - change bg hue slightly.
            (face-background 'region)))) ; Use normal region background.

;;;###autoload
(defcustom icicle-regions-name-length-max 80
  "Maximum number of characters used to name a region.
This many characters, maximum, from the beginning of the region, is
used to name the region."
  :type 'integer :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-reminder-prompt-flag 7
  "*Whether to use `icicle-prompt-suffix' reminder in minibuffer prompt.
Nil means never use the reminder.
Non-nil means use the reminder, if space permits:
 An integer value means use only for that many Emacs sessions.
 t means always use it."
  :type '(choice
          (const   :tag "Never use a reminder in the prompt"                  nil)
          (const   :tag "Always use a reminder in the prompt"                 t)
          (integer :tag "Use a reminder in the prompt for this many sessions" :value 20))
  :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-require-match-flag nil
  "*Control REQUIRE-MATCH arg to `completing-read' and `read-file-name'.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
      `icicle-define-command' and `icicle-define-file-command'.
      You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"  nil)
          (const :tag "Do not require a match"            no-match-required)
          (const :tag "Require a partial match, with RET" partial-match-ok)
          (const :tag "Require a full match"              full-match-required))
  :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-saved-completion-sets nil
  "*Completion sets available for `icicle-candidate-set-retrieve'.
The form is ((SET-NAME . CACHE-FILE-NAME)...), where SET-NAME is the
name of a set of completion candidates and CACHE-FILE-NAME is the
absolute name of the cache file that contains those candidates.
You normally do not customize this directly, statically.
Instead, you add or remove sets using commands
`icicle-add/update-saved-completion-set' and
`icicle-remove-saved-completion-set'."
  :type '(repeat (cons string file)) :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-search-cleanup-flag t
  "*Controls whether to remove highlighting after a search.
If this is nil, highlighting can be removed manually with
`\\[icicle-search-highlight-cleanup]'."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-context-match-predicate nil
  "*Nil or a predicate that candidate search contexts must satisfy.
If nil, then this does nothing.  Otherwise, this is a predicate of one
argument, a string, and only search contexts that satisfy it are
displayed.  Command `icicle-search' binds internal variable
`icicle-must-pass-predicate' to this value.

Note that this predicate is different from the predicate used by \
`\\<minibuffer-local-completion-map>\\[icicle-narrow-candidates-with-predicate]'.
That predicate takes as argument a full search-context candidate,
which includes the context position."
  :type '(choice (const :tag "None" nil) function) :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-search-highlight-all-current-flag nil
  "*Non-nil means highlight input match in each context search hit.
Setting this to non-nil can impact performance negatively, because the
highlighting is updated with each input change.  You can toggle this
option from the minibuffer during `C-c`' search using `C-^'."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-highlight-context-levels-flag t
  "*Non-nil means highlight 1-8 context levels, within the search context.
Level highlighting is done only when this is non-nil and a subgroup is
not used as the search context, that is, the context corresponds to
the entire search regexp."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-highlight-threshold 100000
  "*Max number of context search hits to highlight at once.
This highlighting uses face `icicle-search-main-regexp-others'."
  :type 'integer :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-hook nil
  "*List of hook functions run by `icicle-search' (see `run-hooks')."
  :type 'hook :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-replace-literally-flag nil
  "*Non-nil means to treat replacement text literally.
Nil means to interpret `\' specially in replacement text, as in the
  LITERAL argument to `replace-match'.
You can use `M-`' to toggle this at any time during Icicles search."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-replace-whole-candidate-flag t
  "*Non-nil means replacement during search replaces the entire search hit.
Nil means to replace only what matches your current input.
You can use `C-,' to toggle this at any time during Icicles search."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-ring-max (if (boundp 'most-positive-fixnum)
                                             (/ most-positive-fixnum 10)
                                           13421772) ; 1/10 of `most-positive-fixnum' on Windows.
  "*Icicles version of `search-ring-max'."
  :type 'integer :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-whole-word-flag nil
  "*Non-nil means that `icicle-search' looks for a whole word.
You can use `M-q' to toggle this at any time during Icicles search."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(if (and (fboundp 'defvaralias) (boundp 'completion-show-help))
    (defvaralias 'icicle-show-Completions-help-flag 'completion-show-help)
  (defcustom icicle-show-Completions-help-flag t
    "*Non-nil means display help lines at the top of buffer *Completions*."
    :type 'boolean :group 'Icicles-Completions-Display))

;;;###autoload
(defcustom icicle-show-Completions-initially-flag nil
  "*Non-nil means to show buffer *Completions* even without user input.
nil means that *Completions* is shown upon demand, via `TAB' or
`S-TAB'.

Alternatively, you can set option `icicle-incremental-completion-flag'
to a value that is neither nil nor t.  That will display buffer
*Completions* as soon as you type or delete input (but not
initially)."
  :type 'boolean :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-sort-function 'icicle-case-string-less-p
  "*Comparison function passed to `sort', to sort completion candidates.
This sorting determines the order of candidates when cycling and their
order in buffer *Completions*.  If the value is nil, then no sorting
is done.

When `icicle-cycle-into-subdirs-flag' is non-nil, you might want to
use a function such as `icicle-dirs-last-p' for this option, to
prevent cycling into subdirectories depth first.  Command
`icicle-sort-by-directories-last' does that.

You can toggle sorting at any time using command
`icicle-toggle-sorting', bound to `C-,' in the minibuffer.

Note: Although this is a user option, it may be changed by program
locally, for use in particular contexts.  In particular, you can bind
this to nil in an Emacs-Lisp function, to inhibit sorting in that
context."
  :type '(choice (const :tag "None" nil) function) :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-buffer-configs
  `(("All" nil nil nil nil ,icicle-sort-function)
    ("Files" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname))) nil
     ,icicle-sort-function)
    ("Files and Scratch" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname)))
     ("*scratch*") ,icicle-sort-function)
    ("All, *...* Buffers Last" nil nil nil nil icicle-buffer-sort-*...*-last))
  "*List of option configurations available for `icicle-buffer-config'.
The form is (CONFIG...), where CONFIG is a list of these items:

 - Configuration name                    (string)
 - `icicle-buffer-match-regexp' value    (regexp string)
 - `icicle-buffer-no-match-regexp' value (regexp string)
 - `icicle-buffer-predicate' value       (function)
 - `icicle-buffer-extras' value          (list of strings)
 - `icicle-buffer-sort' value            (function)

A configuration describes which buffer names are displayed during
completion and their order."
  :type '(repeat (list
                  string                ; Configuration name
                  (choice (const :tag "None" nil) (string :tag "Match regexp"))
                  (choice (const :tag "None" nil) (string :tag "No-match regexp"))
                  (choice (const :tag "None" nil) (function :tag "Predicate")) ; Predicate
                  (choice (const :tag "None" nil) (repeat (string :tag "Extra buffer")))
                  (choice (const :tag "None" nil) (function :tag "Sort function"))))
  :group 'Icicles-Buffers)

(defun icicle-buffer-sort-*...*-last (buf1 buf2)
  "Return non-nil if BUF1 is `string<' BUF2 or only BUF2 starts with \"*\"."
  (let ((b1 (if completion-ignore-case (downcase buf1) buf1))
        (b2 (if completion-ignore-case (downcase buf2) buf2)))
    (if (string-match "^\\*" b1)
        (and (string-match "^\\*" b2) (string< b1 b2))
      (or (string-match "^\\*" b2) (string< b1 b2)))))

(if (< emacs-major-version 21)
    (defcustom icicle-sort-functions-alist nil ; Emacs 20
      "Alist of sort functions.
You probably do not want to customize this option.  Instead, use macro
`icicle-define-sort-command' to define a new sort function and add it
to this alist.
Each alist element has the form (SORT-ORDER . COMPARISON-FUNCTION).
SORT-ORDER is a short string (or symbol) describing the sort order.
 Examples: \"by date\", \"alphabetically\", \"directories first\".
COMPARISON-FN is a function that compares two strings, returning
 non-nil if and only if the first string sorts before the second."
      ;; We don't use `alist' :type, because we want Emacs-20 compatibility.
      :type '(repeat
              (cons
               (choice :tag "Sort order" string symbol)
               (choice (function :tag "Comparison function") (const :tag "Do not sort" nil))))
      :group 'Icicles-Completions-Display :group 'Icicles-Matching)
  (defcustom icicle-sort-functions-alist nil ; Emacs 21+
    "Alist of sort functions.
You probably do not want to customize this option.  Instead, use macro
`icicle-define-sort-command' to define a new sort function and add it
to this alist.
Each alist element has the form (SORT-ORDER . COMPARISON-FUNCTION).
SORT-ORDER is a short string (or symbol) describing the sort order.
 Examples: \"by date\", \"alphabetically\", \"directories first\".
COMPARISON-FN is a function that compares two strings, returning
 non-nil if and only if the first string sorts before the second."
    ;; We don't use `alist' :type, because we want Emacs-20 compatibility.
    :type '(alist
            :key-type (choice :tag "Sort order" string symbol)
            :value-type
            (choice (function :tag "Comparison function") (const :tag "Do not sort" nil)))
    :group 'Icicles-Completions-Display :group 'Icicles-Matching))

;;;###autoload
(defcustom icicle-special-candidate-regexp nil
  "*Regexp to match special completion candidates, or nil to do nothing.
The candidates are highlighted in buffer *Completions* using face
`icicle-special-candidate'."
  :type '(choice (const :tag "None" nil) regexp) :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-TAB-shows-candidates-flag t
  "*Non-nil means that `TAB' always shows completion candidates.
Nil means follow the standard Emacs behavior of completing to the
longest common prefix, and only displaying the candidates after a
second `TAB'."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-thing-at-point-functions
  (progn (or (require 'ffap- nil t) (require 'ffap nil t)) ; Try `ffap-.el' first.
         (cons
          ;; 1) Ffap, 2) Lisp symbol or file name, 3) word, 4) url.
          `(,@(and (fboundp 'ffap-guesser) '(ffap-guesser))
            ,(if (fboundp 'symbol-name-nearest-point)
                 'symbol-name-nearest-point
                 (lambda () (symbol-name (symbol-at-point))))
            ,(if (fboundp 'word-nearest-point)
                 'word-nearest-point
                 (lambda () (thing-at-point 'word)))
            thing-at-point-url-at-point)
          'forward-word))
  "*Functions that return a string at or near the cursor when you use `M-.'.
This is a cons cell whose car and cdr may each be empty.

The car of the cons cell is a list of functions that grab different
kinds of strings at or near point.  By default, there are four
functions, which grab 1) whatever `ffap-guesser' finds, 2) the symbol
or file name, 3) the word, 4) the URL at point.  Any number of
functions can be used.  They are used in sequence by command
`icicle-insert-string-at-point' (bound to `M-.').

The cdr of the cons cell is nil or a function that advances point one
text thing.  Each time command `icicle-insert-string-at-point' is
called successively, this is called to grab more things of text (of
the same kind).  By default, successive words are grabbed.

If either the car or cdr is empty, then the other alone determines the
behavior of `icicle-insert-string-at-point'.  Otherwise, option
`icicle-default-thing-insertion' determines whether the car or cdr is
used by `icicle-insert-string-at-point'.  `C-u' with no number
reverses the meaning of `icicle-default-thing-insertion'."
  :type
  '(cons
    (choice
     (repeat (function :tag "Function to grab some text at point and insert it in minibuffer"))
     (const :tag "No alternative text-grabbing functions" nil))
    (choice
     (const :tag "No function to successively grab more text" nil)
     (function :tag "Function to advance point one text thing")))
  :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-top-level-when-sole-completion-flag nil
  "*Non-nil means to return to top level if only one matching completion.
The sole completion is accepted."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-touche-pas-aux-menus-flag nil
  "*Non-nil means do not add items to menus except Minibuf and Icicles.
This value is used only when Icicles mode is initially established, so
changing this has no effect after Icicles has been loaded.  However,
you can change it and save the new value so it will be used next time.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-transform-function nil
  "*Function used to transform the list of completion candidates.
This is applied to the list of initial candidates.
If this is nil, then no transformation takes place.

You can toggle this option at any time from the minibuffer using
`C-$,'.

NOTE: Although this is a user option, you probably do *NOT* want to
customize it.  Icicles commands already \"do the right thing\" when it
comes to candidate transformation.  The value of this option may be
changed by program locally, for use in particular contexts.  For
example, when you use `C-c C-`' (\ `icicle-search-generic') in a
*shell* buffer, Icicles uses this variable with a value of
`icicle-remove-duplicates', to remove duplicate shell commands from
your input history list.

Emacs-Lisp programmers can use this variable to transform the list of
candidates in any way they like.  A typical use is to remove
duplicates, by binding it to `icicle-remove-duplicates'."
  :type '(choice (const :tag "None" nil) function) :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-update-input-hook nil
  "*Functions run when minibuffer input is updated (typing or deleting)."
  :type 'hook :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-use-~-for-home-dir-flag t
  "*Non-nil means abbreviate your home directory using `~'.
You can toggle this option from the minibuffer at any time using
`M-~'."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-use-candidates-only-once-flag nil
  "*Non-nil means remove each candidate from the set after using it.
When you use a multi-command and act on a candidate (for example, with
`C-RET'), the candidate is removed from those available if this is
non-nil.  If this is nil, then the candidate is not removed, so you
can act on it again.

You can customize this option if you prefer the non-nil behavior all
of the time.  However, most users will not want to do that.

If you write Emacs-Lisp code, you can bind this to non-nil during
completion in contexts where it makes little sense for users to act on
the same candidate more than once.  That way, users cannot choose it
again, and they are not distracted seeing it as a candidate."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-word-completion-key [(meta ?\ )]
  "*Key sequence to use for minibuffer word completion.
The value has the same form as a key-sequence arg to `define-key'.

Because file names, in particular, can contain spaces, some people
prefer this to be a non-printable key sequence, such as `M-SPC'.  This
is the default value in Icicles.

But because the spacebar is such a convenient key to hit, other people
prefer to use `SPC' for word completion, and to insert a space some
other way.  The usual way to do that is via `C-q SPC', but command
`icicle-insert-a-space' is provided for convenience.  You can bind
this to `M-SPC', for instance, in `minibuffer-local-completion-map',
`minibuffer-local-completion-map', and
`minibuffer-local-must-match-map'."
  :type 'sexp :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-WYSIWYG-Completions-flag "MMMM"
  "*Non-nil means to show candidates in *Completions* using WYSIWYG.
Some commands might override a string value with different text.  This
is the case for `icicle-read-color', for instance: the swatch text is
always the color's RGB code."
  :type '(choice
          (string :tag "Show candidate plus a WYSIWYG swatch with text..." :value "MMMM")
          (const  :tag "Show candidate itself using WYSIWYG"               t)
          (const  :tag "Show candidate as is, with no text properties"     nil))
  :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-yank-function 'yank
  "*Yank function.  A function that takes a prefix argument.
This should be a command that is bound to whatever key you use to yank
text, whether in Icicle mode or not.  In Icicle mode, command
`icicle-yank-insert' calls this function, except when
`icicle-yank-insert' is called from the minibuffer or the prefix
argument is negative.  `icicle-yank-insert' passes the raw prefix
argument to `icicle-yank-function'."
  :type 'function :group 'Icicles-Miscellaneous)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-opt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-opt.el ends here
