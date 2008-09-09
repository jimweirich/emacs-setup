;;; icicles-mcmd.el --- Minibuffer commands for Icicles
;;
;; Filename: icicles-mcmd.el
;; Description: Minibuffer commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Sun Oct 14 16:02:05 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 9862
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mcmd.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `color-theme', `cus-face',
;;   `easymenu', `ffap', `ffap-', `help-mode', `hexrgb',
;;   `icicles-fn', `icicles-opt', `icicles-var', `pp', `pp+',
;;   `thingatpt', `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  commands to be used mainly in the minibuffer or buffer
;;  *Completions* (and a few non-interactive functions used in those
;;  commands).  For top-level commands, see `icicles-cmd.el'.  For
;;  Icicles documentation, see `icicles.el'.
;;
;;  Commands defined here:
;;
;;    `icicle-abort-minibuffer-input',
;;    `icicle-add/update-saved-completion-set',
;;    `icicle-all-candidates-action',
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
;;    `icicle-delete-backward-char', `icicle-delete-candidate-object',
;;    `icicle-delete-char', `icicle-delete-windows-on',
;;    `icicle-describe-file', `icicle-digit-argument',
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
;;    `icicle-next-candidate-per-mode', `icicle-next-history-element',
;;    `icicle-next-line', `icicle-next-prefix-candidate',
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
;;    `icicle-toggle-transforming',
;;    `icicle-toggle-WYSIWYG-Completions', `icicle-transpose-chars',
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
;;    `toggle-icicle-transforming',
;;    `toggle-icicle-WYSIWYG-Completions'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-all-candidates-action-1',
;;    `icicle-apply-to-saved-candidate', `icicle-apropos-complete-1',
;;    `icicle-delete-current-candidate-object',
;;    `icicle-candidate-action-1', `icicle-candidate-set-save-1',
;;    `icicle-candidate-set-save-selected-1',
;;    `icicle-current-completion-in-Completions',
;;    `icicle-current-sort-order',
;;    `icicle-ensure-overriding-map-is-bound',
;;    `icicle-help-on-candidate-symbol', `icicle-insert-input',
;;    `icicle-insert-thing', `icicle-isearch-resume',
;;    `icicle-mouse-candidate-action-1',
;;    `icicle-nb-of-candidate-in-Completions',
;;    `icicle-prefix-complete-1', `icicle-raise-Completions-frame',
;;    `icicle-remove-candidate-display-others',
;;    `icicle-retrieve-candidates-from-set', `icicle-signum',
;;    `icicle-successive-action', `icicle-transform-multi-completion',
;;    `icicle-transform-sole-candidate'.
;;
;;  Internal variables defined here:
;;
;;    `overriding-map-is-bound', `saved-overriding-map'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `exit-minibuffer'              - Remove *Completion* window.
;;  `minibuffer-complete-and-exit' - Remove *Completion* window.
;;
;;
;;  ***** NOTE: The following function defined in `mouse.el' has
;;              been REDEFINED HERE:
;;
;;  `mouse-choose-completion' - Return the number of the completion.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `switch-to-completions' - Always selects *Completions* window.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in `icicles.el'.
 
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
;;  (@> "Redefined standard commands")
;;  (@> "Icicles commands")
;;    (@> "Minibuffer editing commands")
;;    (@> "Commands to sort completion candidates")
;;    (@> "Other commands to be used mainly in the minibuffer")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2007/10/14 dadams
;;     Updated doc strings to reflect icicle-act-before-cycle-flag.
;; 2007/10/13 dadams
;;     icicle-candidate-action-1:
;;       If icicle-last-completion-candidate is a string, don't set it.  Used for: repeated C-next.
;;     icicle-remove-candidate-display-others, icicle-history:
;;       Treat also the case where cand is a string, not a consp.
;; 2007/10/07 dadams
;;     icicle-delete-candidate-object: Respect icicle-deletion-action-flag.
;; 2007/10/02 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       Apply abbreviate-file-name to file-name input.  Thx to Joonhwan Lee.
;;     icicle-toggle-fuzzy-completion: Removed soft require of fuzzy-match+.el.
;; 2007/09/29 dadams
;;     Added: icicle-toggle-fuzzy-completion.
;;     icicle-Completions-mouse-3-menu: Added icicle-toggle-fuzzy-completion.
;;     icicle-prefix-complete-1: Adjust feedback messages for fuzzy completion.
;;     icicle-(apropos|prefix)-complete-1:
;;       Only set icicle-default-directory if (icicle-file-name-input-p).
;; 2007/09/25 dadams
;;     icicle-narrow-candidates: Treat icicle-whole-candidate-as-text-prop-p case.
;;     icicle-kill-failed-input: Rewrote.
;; 2007/09/21 dadams
;;     icicle-narrow-candidates:
;;       Emacs<22, file-name completion: Append directory to each candidate.  Thx to Ian Perryman.
;; 2007/09/14 dadams
;;     icicle-(apropos|prefix)-complete-1, icicle-prefix-word-complete:
;;       Wrapped condition-case around candidates computation.
;; 2007/08/25 dadams
;;     icicle-mouse-candidate-action-1: Use buffer-substring, not buffer-substring-no-properties.
;; 2007/08/21 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       Reset icicle-input-fail-pos.  Call icicle-highlight-input-noncompletion when no match.
;; 2007/08/19 dadams
;;     Added: icicle-kill-failed-input.
;; 2007/08/18 dadams
;;     icicle-previous-apropos-candidate-alt-action: Fixed typo.  Thx to Hadron Quark.
;; 2007/07/29 dadams
;;     icicle-apply-to-saved-candidate:
;;       Added use-icicle-candidates-alist-p arg.  Use icicle-get-alist-candidate.
;;       Report original error message also.
;;     icicle-candidate-action-1: Do nothing if icicle-last-completion-candidate is not a string. (?)
;; 2007/07/27 dadams
;;     icicle-successive-action: icicle-act-first-then-navigate-p -> icicle-act-before-cycle-flag.
;; 2007/07/08 dadams
;;     icicle-all-candidates(-alt)-action:
;;       Use icicle-all-candidates(-alternative)-action-fn if defined.
;;     icicle-all-candidates-action-1: Added listp arg.
;;     icicle-mouse-save-candidate: Deactivate mark and redisplay completions, to show save highlight.
;; 2007/07/07 dadams
;;     Added: icicle-candidate-set-save(-more)-selected, icicle-candidate-set-save(-selected)-1,
;;            icicle-mouse-candidate-set-save(-more), icicle-mouse-save-then-kill.
;;     icicle-insert-completion: If no current completion, return to minibuffer anyway.
;;                               Update icicle-current-input with inserted candidate.
;;     icicle-Completions-mouse-3-menu: Added icicle-candidate-set-save-(more(-selected)|-selected).
;;     icicle-save-candidate: If no defined icicle-candidate-nb, then just display message.
;;     icicle-candidate-set-save(-more):
;;       Use icicle-candidate-set-save-1: Intern variable also in standard obarray.  Redisplay
;;       candidates and reselect minibuffer after reading file/var name.  Put eof error in minibuffer.
;;       Deactivate mark and redisplay completions.  Separate msg if reset.
;;     icicle-candidate-set-retrieve: If nothing to restore, don't restore nothing.
;;                                    If single candidate to restore, no *Completions* display.
;;                                    Else, update candidate display.
;; 2007/07/04 dadams
;;     icicle-Completions-mouse-3-menu: Added icicle-retrieve-(next|\previous)-input.
;; 2007/07/03 dadams
;;     Added: icicle-insert-history-element, icicle-retrieve-(next|previous)-input.
;;     icicle-history, icicle-keep-only-past-inputs:
;;       Don't retrieve last input unless following a cycling command.
;;     icicle-history: 
;;       Do an initial icicle-apropos-complete unless icicle-last-completion-command.
;;       If not following a cycling command, call icicle-last-completion-command (don't set it to
;;       empty string) and reset icicle-last-input to nil.
;;     icicle-Completions-mouse-3-menu:
;;       icicle-retrieve-(next|previous)-input, not icicle-retrieve-last-input.
;;     Redefined next-history-element, instead of using defadvice.
;; 2007/06/23 dadams
;;     icicle-search-define-replacement: Use icicle-completing-read-history, not read-string.
;;                                       Use icicle-search-replacement-history.
;; 2007/06/17 dadams
;;     Added: icicle-toggle-WYSIWYG-Completions.
;;     icicle-switch-to-Completions-buf, icicle-move-to-next-completion:
;;       Added priority in call to icicle-place-overlay.
;; 2007/06/13 dadams
;;     Added: icicle-candidate-set-save-more.
;;     icicle-candidate-set-save: Unify messages.
;; 2007/06/12 dadams
;;     Added: icicle(-mouse)-save-candidate.
;;     icicle-candidate-set-retrieve: Insert candidate if there is only one retrieved.
;;     icicle-insert-completion: Added optional completion arg for non-interactive insertion.
;; 2007/06/10 dadams
;;     icicle-candidate-action-1: Treat icicle-require-match-p.
;; 2007/06/09 dadams
;;     icicle-candidate-action-1, icicle-mouse-candidate-action-1:
;;       Remove candidate if icicle-use-candidates-only-once-flag.
;;     icicle-candidate-action-1:
;;       Let users act on non-candidate too (arbitrary input).
;; 2007/06/07 dadams
;;     Renamed: icicle-function-history to icicle-function-name-history,
;;              icicle-variable-history to  icicle-variable-name-history.
;;     Use standard history variable if bound, else use Icicles history variable:
;;       function-name-history, variable-name-history
;; 2007/06/01 dadams
;;     icicle-erase-minibuffer-or-history-element, icicle-history:
;;       Ensure value of minibuffer-history-variable is bound.
;;     icicle-keep-only-past-inputs: If value of minibuffer-history-variable is unbound, set to nil.
;;     icicle-keep-only-past-inputs, icicle-history:
;;       Assume value of minibuffer-history-variable is a symbol - don't test that.
;; 2007/05/29 dadams
;;     icicle-insert-thing: Added optional arg no-replace-p.  Make sure end points are defined.
;;     icicle-insert-string-from-variable: Call icicle-insert-thing with no-replace-p arg.
;;     icicle-minibuffer-complete-and-exit: Set window-point to end of minibuffer.
;; 2007/05/15 dadams
;;     icicle-completion-help and top level:
;;       (require 'help-mode nil t), not (featurep 'help-mode) and (fboundp 'define-button-type).
;; 2007/05/08 dadams
;;     Added: icicle-save-predicate-to-variable.
;;     icicle-Completions-mouse-3-menu: Added icicle-save-predicate-to-variable to menu.
;;     icicle-narrow-candidates-with-predicate: Quoted the predicate that is read.
;; 2007/05/07 dadams
;;     Added: icicle-narrow-candidates-with-predicate.
;;     icicle-Completions-mouse-3-menu: Added icicle-narrow-candidates-with-predicate (M-&).
;; 2007/05/06 dadams
;;     icicle-completion-help: Updated text at top of help buffer.
;;     icicle-customize-button: Capitalized group Icicles.
;;     Changed S-C- to C-S- and M-C- to C-M- in doc.
;; 2007/05/04 dadams
;;     icicle-candidate-read-fn-invoke, icicle-keep-only-past-inputs, icicle-retrieve-last-input,
;;     icicle-candidate-set-(retrieve|save|swap|difference|union|intersection|complement),
;;     icicle-all-candidates(-alt)-action, icicle-pp-eval-expression,
;;     icicle-insert-string-from-variable:
;;       Can now call from *Completions* too, so can choose from mouse-3 menu during multi-command.
;;     icicle-candidate-set-save, icicle-retrieve-last-input, icicle-insert-string-from-variable:
;;       Select minibuffer window.
;;     icicle-toggle-case-sensitivity: Use setq-default for case-fold-search.
;;     icicle-switch-to-Completions-buf:
;;       Use read-file-name-completion-ignore-case, if completing file name.
;;     Added empty defvars for Emacs 22 standard vars, to quiet byte compiler.
;; 2007/05/02 dadams
;;     Added: icicle-dispatch-M-q, icicle-toggle-search-whole-word, toggle-icicle-search-whole-word.
;;     Removed: icicle-dispatch-C-backquote.
;; 2007/04/29 dadams
;;     Added: icicle-sort-by-last-file-modification-time (sort order).
;; 2007/04/19 dadams
;;     icicle-successive-action: No longer interactive.  Moved barfing to calling commands.
;; 2007/04/17 dadams
;;     Added: icicle-dispatch-M-comma, icicle-search-define-replacement,
;;            icicle-dispatch-C-backquote, icicle-toggle-literal-replacement.
;; 2007/04/08 dadams
;;     Added: icicle-all-candidates-alt-action, icicle-all-candidates-action-1.
;;     icicle-candidate-action-1, icicle-delete-candidate-object, icicle-help-on-candidate,
;;     icicle-candidate-read-fn-invoke:
;;       Use negative test for prefix mode, not positive test for apropos.
;; 2007/04/07 dadams
;;     Added: icicle-successive-action, icicle-toggle-search-replace-whole, icicle-dispatch-C-comma.
;;     Defined navigating action and help functions using icicle-successive-action.
;; 2007/03/31 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       Accept sole completion if icicle-top-level-when-sole-completion-flag.
;;     icicle-narrow-candidates:
;;       Only use read-file-name for Emacs 22 or later.
;;       Accept sole completion only if icicle-top-level-when-sole-completion-flag.
;;     icicle-apropos-complete-and-narrow: Bind icicle-top-level-when-sole-completion-flag to t.
;; 2007/03/30 dadams
;;     icicle-narrow-candidates: Suppress sole-completion minibuffer-message.
;; 2007/03/23 dadams
;;     Added: icicle-apropos-complete-and-narrow.  Thx to Marian Schubert for the suggestion.
;;     icicle-narrow-candidates: Use icicle-require-match-p as REQUIRE-MATCH arg.
;; 2007/03/09 dadams
;;     Changed require to eval-when-compile require for icicles-mac.el.
;; 2007/03/08 dadams
;;     icicle-delete-current-candidate-object: Rewrote.
;;       Value of var can be an arbitrary alist, a list of strings, or a list of symbols.
;;     icicle-remove-candidate-display-others: Rewrote.
;;       Set icicle-last-completion-candidate based on icicle-candidate-nb or 0.
;;       Delete icicle-last-completion-candidate completely from icicle-completion-candidates.
;;       Update minibuffer-completion-predicate or read-file-name-predicate to remove for completion.
;;       Use with-current-buffer, not save-window-excursion, to visit *Completions*.
;;     icicle-remove-candidate:
;;       Updated doc string to mention Emacs < 22 limitation for file-name candidates.
;;     icicle-retrieve-last-input: No longer reset icicle-last-completion-command when interactive.
;; 2007/03/07 dadams
;;     icicle-switch-to-Completions-buf, icicle-remove-candidate-display-others,
;;     icicle-help-on-candidate, icicle-delete-windows-on:
;;       Use 0, not t, as frame arg to get-buffer-window.
;; 2007/03/06 dadams
;;     icicle-remove-candidate: Don't reset to first candidate matching input if no last candidate.
;;     icicle-change(alternative)-sort-order, icicle-reverse-sort-order, icicle-keep-only-past-inputs,
;;     icicle-toggle-sorting:
;;       Respect icicle-inhibit-sort-p.
;;     Renamed icicle-get-current-candidate to icicle-get-alist-candidate.
;; 2007/03/04 dadams
;;     icicle-remove-candidate-display-others:
;;       Use local var for cand-nb, because icicle-candidate-nb can change.
;;       If no last candidate, reset to first candidate matching input.
;;       Allow for icicle-candidate-nb not being defined here:
;;         Use icicle-get-current-candidate.  Move to next completion only when cand-nb is defined.
;;       Use mapconcat only when delete multi-completion.
;;       Move to next completion in *Completions* only if icicle-candidate-nb was defined.
;;       Insert default-directory too, if icicle-file-name-input-p.
;;     icicle-insert-completion: Insert default-directory too, if icicle-file-name-input-p.
;;     icicle-(apropos|prefix)-complete-1, icicle-keep-only-past-inputs:
;;       Don't include directory when set icicle-last-completion-candidate.
;;     icicle-(apropos|prefix)-complete-1:
;;       Don't include directory when testing membership of input in icicle-completion-candidates.
;; 2007/03/02 dadams
;;     icicle-delete-candidate-object:
;;       Corrected message target (object).  Added sit-for.
;;       Use local var for cand-nb, because icicle-candidate-nb can change.
;; 2007/02/27 dadams
;;     icicle-delete-candidate-object: Added message.
;;     icicle-delete-current-candidate-object: Don't erase minibuffer or update completions.
;; 2007/02/24 dadams
;;     Added: icicle(-mouse)-candidate-alt-action, icicle(-mouse)-candidate-action-1,
;;            icicle-(previous|next)-(apropos|prefix)-candidate-alt-action,
;;            icicle(-mouse)-remove-candidate, icicle-remove-candidate-display-others,
;;            icicle-delete-candidate-object, icicle-delete-current-candidate-object.
;;     icicle-insert-completion:
;;       Invoke icicle-transform-multi-completion.  Wrap in with-current-buffer (window-buffer).
;;     icicle(-mouse)-candidate-action: Use icicle(-mouse)-candidate-action-1.
;; 2007/02/06 dadams
;;     icicle-completion-help: Added extra help if completing and if multi-command.
;; 2007/02/03 dadams
;;     Renamed icicle-icompleting-p to icicle-edit-update-p.
;; 2007/02/02 dadams
;;     Updated doc strings of toggle commands to mention the minibuffer bindings.
;; 2007/01/29 dadams
;;     icicle-change-sort-order: Don't sort icicle-sort-functions-alist entries for use.
;;     Define alphabetical sort order using icicle-case-string-less-p, not string-lessp.
;; 2007/01/23 dadams
;;     Added: icicle-toggle-highlight-historical-candidates.
;;     icicle-Completions-mouse-3-menu: Updated wrt toggles.
;; 2007/01/21 dadams
;;     icicle-narrow-candidates:
;;       Use minibuffer-history-variable, not regexp-history.  Thx to Jost for bug report.
;; 2007/01/20 dadams
;;     icicle-mouse-(choose-completion|candidate-action): Use icicle-transform-multi-completion.
;; 2007/01/15 dadams
;;     Added: icicle-change(-alternative)-sort-order, icicle-reverse-sort-order, 
;;            icicle-current-sort-order, icicle-sort-*.
;;     icicle-transform-sole-candidate: Set icicle-last-completion-candidate to transformed cand.
;;     icicle-help-on-candidate: Use icicle-transform-multi-completion.
;;     icicle-Completions-mouse-3-menu: Updated with new sort-order bindings.
;;     icicle-toggle-alternative-sorting: Better message.
;;     Require icicles-mac.el.
;; 2007/01/14 dadams
;;     Added: icicle-transform-multi-completion, icicle-transform-sole-candidate.
;;     icicle-(apropos|prefix)-complete-1: Use icicle-transform-sole-candidate.  Thx to Rubikitch.
;;     icicle-help-on-candidate(-symbol): Use with-current-buffer to describe mode in Emacs20 also.
;; 2007/01/13 dadams
;;     Added: icicle-describe-file, icicle-help-on-candidate-symbol.
;;     icicle-help-on-candidate:
;;       If existing symbol, describe it.  Else if buffer or file, describe it.
;;       Otherwise, convert string to symbol and describe it.  Use icicle-help-on-candidate-symbol.
;; 2007/01/10 dadams
;;     icicle-switch-to/from-minibuffer: Error message if minibuffer is not active.
;; 2007/01/06 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       expand-file-name -> icicle-abbreviate-or-expand-file-name.
;;     Added: icicle-toggle-~-for-home-dir.
;;     icicle-prefix-complete-1: Set icicle-default-directory only if also icicle-file-name-input-p
;; 2007/01/01 dadams
;;     icicle-add/update-saved-completion-set: Use icicle-assoc-delete-all, not delete of assoc.
;;     Runtime, not compile-time, require of icicles-var.el, icicles-opt.el.
;; 2006/12/29 dadams
;;     icicle-insert-string-at-point:
;;       Treat nil return of alternative text-grabbing function.
;;       Echo the text-grabbing function when icicle-default-thing-insertion = alternatives.
;;     icicle-ensure-overriding-map-is-bound: Bug fix: Separate treatment for diff Emacs versions.
;; 2006/12/25 dadams
;;     icicle-keep-only-past-inputs:
;;       Added optional recent-first arg: Use icicle-most-recent-first-p as sort function.
;;       Update candidates list if repeat.  Do not scroll *Completions*; update it unconditionally.
;;     Added: icicle-candidate-set-truncate.
;;     Uncommented describe-mode code, since RMS fixed Emacs bug that caused infinite recursion.
;; 2006/12/24 dadams
;;     Added: icicle-Completions-mouse-3-menu.
;; 2006/12/23 dadams
;;     icicle-narrow-candidates: Bug fix: Treat file-name completion with read-file-name.
;;     icicle-help-on-candidate: Call non-nil icicle-candidate-help-fn on candidate.
;; 2006/12/18 dadams
;;     icicle-apply-to-saved-candidate: Remove print arg and use current-prefix-arg instead.
;;     icicle-ensure-overriding-map-is-bound: Protect overriding-map-is-bound with boundp.
;;     Bug fix for Emacs 21: protect help-xref with get type button-category-symbol.
;; 2006/12/17 dadams
;;     Added: icicle(-mouse)-candidate-read-fn-invoke, icicle-apply-to-saved-candidate.
;; 2006/12/10 dadams
;;     Created from minibuffer and *Completions* commands in icicles-cmd.el.
;;
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

(eval-when-compile (require 'cl)) ;; flet (plus for Emacs < 20, when, unless)

(eval-when-compile (require 'icicles-mac)) ;; icicle-define-sort-command

(require 'icicles-var)
  ;; icicle-candidate-action-fn, icicle-candidate-nb, icicle-universal-argument-map,
  ;; icicle-completion-candidates, icicle-completion-help-string,
  ;; icicle-current-completion-candidate-overlay, icicle-current-completion-mode,
  ;; icicle-current-input, icicle-current-raw-input, icicle-default-directory,
  ;; icicle-default-thing-insertion-flipped-p, icicle-edit-update-p, icicle-ignored-extensions,
  ;; icicle-ignored-extensions-regexp, icicle-incremental-completion-p,
  ;; icicle-insert-string-at-pt-end, `icicle-insert-string-at-pt-start,
  ;; icicle-last-completion-candidate, icicle-last-completion-command, icicle-last-input,
  ;; icicle-last-sort-function, icicle-last-transform-function, icicle-menu-items-alist,
  ;; icicle-nb-of-other-cycle-candidates, icicle-pre-minibuffer-buffer,
  ;; icicle-saved-candidates-variables-obarray, icicle-saved-completion-candidates,
  ;; icicle-saved-ignored-extensions, icicle-successive-grab-count, icicle-thing-at-pt-fns-pointer,
  ;; icicle-universal-argument-map, icicle-variable-name-history
(require 'icicles-opt)
  ;; icicle-alternative-sort-function, icicle-Completions-frame-at-right-flag, 
  ;; icicle-cycling-respects-completion-mode-flag, icicle-default-thing-insertion,
  ;; icicle-expand-input-to-common-match-flag, icicle-ignore-space-prefix-flag,
  ;; icicle-incremental-completion-flag, icicle-input-string, icicle-key-descriptions-use-<>-flag,
  ;; icicle-regexp-quote-flag, icicle-reminder-prompt-flag, icicle-saved-completion-sets, 
  ;; icicle-search-cleanup-flag, icicle-search-highlight-all-current-flag, icicle-sort-function,
  ;; icicle-TAB-shows-candidates-flag, icicle-thing-at-point-functions, icicle-transform-function
(require 'icicles-fn) ;; icicle-assoc-delete-all, icicle-get-alist-candidate

(require 'pp+ nil t) ;; (no error if not found): pp-eval-expression

;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(when (< emacs-major-version 22)
  (defvar overriding-map-is-bound)
  (defvar read-file-name-completion-ignore-case)
  (defvar read-file-name-predicate)
  (defvar saved-overriding-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Redefined standard commands")

;;; Redefined standard commands --------------------------------------


;;; REPLACE ORIGINAL `next-history-element' in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects minibuffer contents and leaves point at its beginning.
;;;
(or (fboundp 'old-next-history-element)
(fset 'old-next-history-element (symbol-function 'next-history-element)))

;;;###autoload
(defun icicle-next-history-element (arg) ; Bound to `M-n' in the minibuffer.
  "Insert the next element of the minibuffer history in the minibuffer.
With argument N, it uses the Nth following element."
  (interactive "p")
  (old-next-history-element (prefix-numeric-value arg))
  (when (and icicle-mode (memq icicle-init-value-flag '(preselect-start preselect-end)))
    (icicle-select-minibuffer-contents)
    (setq deactivate-mark nil)))


;;; REPLACE ORIGINAL `exit-minibuffer' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Removes *Completion* window.
;;;
(or (fboundp 'old-exit-minibuffer)
(fset 'old-exit-minibuffer (symbol-function 'exit-minibuffer)))

;;;###autoload
(defun icicle-exit-minibuffer ()        ; Bound to `C-m' (`RET') and `\n' in the minibuffer.
  "Terminate this minibuffer argument.  Removes *Completions* window."
  (interactive)
  (icicle-remove-Completions-window)
  (old-exit-minibuffer))


;;; REPLACE ORIGINAL `minibuffer-complete-and-exit' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Removes *Completion* window.
;;;
;;; Note: This calls the original, which does not use `display-completion-list', so if *Completions*
;;;       is displayed by this then it has no Icicles enhancements - it is vanilla Emacs.
;;;
(or (fboundp 'old-minibuffer-complete-and-exit)
(fset 'old-minibuffer-complete-and-exit (symbol-function 'minibuffer-complete-and-exit)))

;;;###autoload
(defun icicle-minibuffer-complete-and-exit () ; Bound to `C-m' (`RET') and `\n'
                                        ; in `minibuffer-local-must-match-map'.
  "If the minibuffer contents is a valid completion, then exit.
Otherwise try to complete it.  If completion leads to a valid completion,
a repetition of this command will exit.
Removes *Completions* window."
  (interactive)
  (save-excursion (icicle-remove-Completions-window))
  (old-minibuffer-complete-and-exit)
  (set-window-point (active-minibuffer-window) (point-max))) ; Don't see why this is needed.


;;; REPLACE ORIGINAL `mouse-choose-completion' in `mouse.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Return the number of the completion.
;;;
(or (fboundp 'old-mouse-choose-completion)
(fset 'old-mouse-choose-completion (symbol-function 'mouse-choose-completion)))

;;;###autoload
(defun icicle-mouse-choose-completion (event) ; Bound to `mouse-2' in *Completions*.
  "Click a completion candidate in buffer `*Completions*', to choose it.
Return the number of the candidate: 0 for first, 1 for second, ..."
  (interactive "e")
  (unless (active-minibuffer-window) (error "Minibuffer is not active"))
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let* ((buffer (window-buffer))
         (orig-buffer buffer)
         choice base-size)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-start event))))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (unless beg (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face) (point-max))
                choice (icicle-transform-multi-completion (buffer-substring beg end))))))
    (if (eq orig-buffer (get-buffer "*Completions*"))
        (icicle-remove-Completions-window)
      (save-selected-window (icicle-remove-Completions-window)))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions
                               (posn-point (event-start event))))
    (choose-completion-string choice buffer base-size)))

(defun icicle-nb-of-candidate-in-Completions (position)
  "Return number of completion candidate at POSITION in *Completions*.
POSITION is a buffer position."
  (let ((compl-buf (get-buffer "*Completions*")))
    (unless compl-buf (error "No *Completions* buffer"))
    (save-window-excursion
      (set-buffer compl-buf)
      (goto-char position)
      ;; If in a completion, move to its start, and set POSITION there.
      (let ((prop (get-text-property (1- (point)) 'mouse-face)))
        (when (and prop (eq prop (get-text-property (point) 'mouse-face)))
          (goto-char (previous-single-property-change
                      (point) 'mouse-face nil (icicle-start-of-candidates-in-Completions)))))
      (setq position (point))
      ;; Binary search.
      (let ((cand-nb (/ (length icicle-completion-candidates) 2))
            (last-nb 0))
        (goto-char (point-min))
        (icicle-move-to-next-completion cand-nb t)
        (while (/= (point) position)
          (let ((delta (max 1 (/ (abs (- cand-nb last-nb)) 2))))
            (cond ((< (point) position)                 
                   (icicle-move-to-next-completion delta t)
                   (setq cand-nb (+ cand-nb delta)))
                  (t
                   (icicle-move-to-next-completion (- delta) t)
                   (setq cand-nb (- cand-nb delta))))
            (setq last-nb cand-nb)))
        (set-buffer-modified-p nil)
        (1- cand-nb)))))


;;; REPLACE ORIGINAL `switch-to-completions' defined in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-switch-to-completions)
(fset 'old-switch-to-completions (symbol-function 'switch-to-completions)))

;;;###autoload
(defun icicle-switch-to-completions ()
  "Select the completion list window, *Completions*."
  (interactive)
  ;; Make sure we have a completions window.
  (or (get-buffer-window "*Completions*") (minibuffer-completion-help))
  (let ((window (get-buffer-window "*Completions*" 0))) ; Added 0 arg.
    (when window
      (select-window window)
      (goto-char (icicle-start-of-candidates-in-Completions)))))

;;; The branch that deletes a history element is based on Juri Linkov's
;;; `delete-history-element', proposed for Emacs 22 but rejected by RMS.
;;;###autoload
(defun icicle-erase-minibuffer-or-history-element () ; Bound to `M-k' in minibuffer.
  "`icicle-erase-minibuffer' or, if using history, delete history element."
  (interactive)
  (if (not (memq last-command '(previous-history-element next-history-element
                                icicle-erase-minibuffer-or-history-element
                                previous-matching-history-element next-matching-history-element)))
      (icicle-erase-minibuffer)
    (let* ((curr-pos (1- minibuffer-history-position))
           (current (nth curr-pos (and (boundp minibuffer-history-variable)
                                       (symbol-value minibuffer-history-variable)))))
      (cond ((= minibuffer-history-position 1)
             (set minibuffer-history-variable (and (boundp minibuffer-history-variable)
                                                   (cdr (symbol-value minibuffer-history-variable)))))
            ((> minibuffer-history-position 1)
             (setcdr (nthcdr (- minibuffer-history-position 2)
                             (and (boundp minibuffer-history-variable)
                                  (symbol-value minibuffer-history-variable)))
                     (nthcdr minibuffer-history-position
                             (and (boundp minibuffer-history-variable)
                                  (symbol-value minibuffer-history-variable))))))
      (condition-case nil
          (cond ((memq last-command '(next-history-element next-matching-history-element))
                 (next-history-element 1)
                 (setq this-command 'next-history-element))
                ((memq last-command '(previous-history-element previous-matching-history-element))
                 (next-history-element 1)
                 (previous-history-element 1)
                 (setq this-command 'previous-history-element)))
        (error (condition-case nil
                   (cond ((memq last-command '(next-history-element next-matching-history-element))
                          (previous-history-element 1)
                          (setq this-command 'previous-history-element))
                         ((memq last-command
                                '(previous-history-element previous-matching-history-element))
                          (next-history-element 1)
                          (setq this-command 'next-history-element)))
                 (error nil))))
      (when (and current (wholenump curr-pos))
        (icicle-msg-maybe-in-minibuffer "Deleted `%s'" current)))))
 
;;(@* "Icicles commands")

;;; Icicles commands -------------------------------------------------

;;(@* "Minibuffer editing commands")

;;; Minibuffer editing commands  . . . . . . . . . . . . . . . . . . .
;;;
;;; All except `icicle-erase-minibuffer' are bound in the minibuffer to whatever the same
;;; command without `icicle-' is bound to globally.

;;;###autoload
(defun icicle-backward-delete-char-untabify (n &optional killflag)
  "`backward-delete-char-untabify' + update *Completions* with matches.
See description of `backward-delete-char-untabify'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'backward-delete-char-untabify n killflag))

;;;###autoload
(defun icicle-delete-backward-char (n &optional killflag) ; Bound to `DEL' in minibuffer.
  "`delete-backward-char' and update *Completions* with input matches.
See description of `delete-backward-char'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'delete-backward-char n killflag))

;;;###autoload
(defun icicle-delete-char (n &optional killflag) ; Bound to `C-d' in minibuffer.
  "`delete-char' and update *Completions* with input matches.
See description of `delete-char'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'delete-char n killflag))

;;;###autoload
(defun icicle-backward-kill-word (arg)  ; Bound to `M-DEL' (`M-backspace') in minibuffer.
  "`backward-kill-word' and update *Completions* with input matches.
See description of `backward-kill-word'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-word arg))

;;;###autoload
(defun icicle-kill-word (arg)           ; Bound to `M-d' in minibuffer.
  "`kill-word' and update *Completions* with regexp input matches.
See description of `kill-word'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-word arg))

;;;###autoload
(defun icicle-backward-kill-sexp (arg)  ; Bound to `C-M-backspace' in minibuffer.
  "`backward-kill-sexp' and update *Completions* with input matches.
See description of `backward-kill-sexp'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-sexp arg))

;;;###autoload
(defun icicle-kill-sexp (arg)           ; Bound to `C-M-delete' and `C-M-k' in minibuffer.
  "`kill-sexp' and update *Completions* with regexp input matches.
See description of `kill-sexp'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-sexp arg))

;;;###autoload
(defun icicle-backward-kill-sentence (arg) ; Bound to `C-x DEL' in minibuffer.
  "`backward-kill-sentence' and update *Completions* with input matches.
See description of `backward-kill-sentence'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-sentence arg))

;;;###autoload
(defun icicle-kill-sentence (arg)
  "`kill-sentence' and update *Completions* with regexp input matches.
See description of `kill-sentence'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-sentence arg))

;;;###autoload
(defun icicle-backward-kill-paragraph (arg) ; Bound to `C-backspace' in minibuffer.
  "`backward-kill-paragraph' and update *Completions* with input matches.
See description of `backward-kill-paragraph'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-paragraph arg))

;;;###autoload
(defun icicle-kill-paragraph (arg)      ; Bound to `C-delete' in minibuffer.
  "`kill-paragraph' and update *Completions* with regexp input matches.
See description of `kill-paragraph'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-paragraph arg))

;;;###autoload
(defun icicle-kill-line (arg)           ; Bound to `C-k' and `deleteline' in minibuffer.
  "`kill-line' and update *Completions* with regexp input matches.
See description of `kill-line'."
  (interactive "P")
  (icicle-call-then-update-Completions #'kill-line arg))

;;;###autoload
(defun icicle-kill-region (beg end)     ; Bound to `C-w' in minibuffer.
;; Don't bother with Emacs 22 optional 3rd arg.
  "`kill-region' and update *Completions* with regexp input matches.
See description of `kill-region'."
  (interactive "r")
  (icicle-call-then-update-Completions #'kill-region beg end))

;;;###autoload
(when (fboundp 'kill-region-wimpy)
  (defun icicle-kill-region-wimpy (beg end) ; Bound to `C-w' in minibuffer.
    "`kill-region-wimpy' and update *Completions* with input matches.
See description of `kill-region-wimpy'."
    (interactive "r")
    (icicle-call-then-update-Completions #'kill-region-wimpy beg end)))

;;;###autoload
(defun icicle-kill-failed-input ()      ; Bound to `C-M-l' in minibuffer during completion.
  "Kill (delete) the part of the input that does not complete."
  (interactive)
  (goto-char (1- (point-max)))
  (while (and (not (bobp)) (eq 'icicle-input-completion-fail (get-text-property (point) 'face)))
    (delete-char 1)
    (backward-char 1))
  (unless (eobp) (forward-char)))

;;;###autoload
(defun icicle-transpose-chars (arg)     ; Bound to `C-t' in minibuffer.
  "`transpose-chars' and update *Completions* with regexp input matches.
See description of `transpose-chars'."
  (interactive "*P")
  (icicle-call-then-update-Completions #'transpose-chars arg))

;;;###autoload
(defun icicle-transpose-words (arg)     ; Bound to `M-t' in minibuffer.
  "`transpose-words' and update *Completions* with regexp input matches.
See description of `transpose-words'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'transpose-words arg))

;;;###autoload
(defun icicle-transpose-sexps (arg)    ; Bound to `C-M-t' in minibuffer.
  "`transpose-sexps' and update *Completions* with regexp input matches.
See description of `transpose-sexps'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'transpose-sexps arg))

;;;###autoload
(defun icicle-yank (arg)                ; Bound to `C-y' and `S-insert' in minibuffer.
  "`yank' and update *Completions* with regexp input matches.
See description of `yank'."
  (interactive "*P")
  (icicle-call-then-update-Completions #'yank arg))

;;;###autoload
(defun icicle-yank-pop (arg)            ; Bound to `M-y' and `M-insert' in minibuffer.
  "`yank-pop' and update *Completions* with regexp input matches.
See description of `yank-pop'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'yank-pop arg))

;;;###autoload
(defun icicle-self-insert (n) ;; Bound in minibuf to stuff bound globally to `self-insert-command'.
  "`self-insert' and update *Completions* with regexp input matches.
See description of `self-insert'."
  (interactive "p")
  (icicle-call-then-update-Completions #'self-insert-command n))

;; Make delete-selection mode recognize self-insertion, so it replaces region text.
(put 'icicle-self-insert 'delete-selection t)

;;;###autoload
(defun icicle-insert-a-space ()
  "Insert a space.
For convenience in the minibuffer - does the same thing as `C-q SPC'.
To use this, bind it to some key sequence in keymaps
`minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', and
`minibuffer-local-must-match-map'."
  (interactive) (insert ?\ ))

;;;###autoload
(defun icicle-erase-minibuffer ()       ; Bound to `M-S-backspace', `M-S-delete' in minibuffer.
  "Delete all user input in the minibuffer."
  (interactive)
  (icicle-call-then-update-Completions #'icicle-clear-minibuffer))
 
;;(@* "Commands to sort completion candidates")

;;; Commands to sort completion candidates . . . . . . . . . . . . . .

;; We don't bother to define a command for sort function `icicle-prefix-keys-first-p'.
;; It is bound in `icicle-complete-keys'.

;;;###autoload
(icicle-define-sort-command "alphabetical" icicle-case-string-less-p ; `icicle-sort-alphabetical'
  "Sort completion candidates alphabetically.
Letter case is ignored if `completion-ignore-case' or
`case-fold-search' is non-nil.")

;;;###autoload
(icicle-define-sort-command "by directories last" ; `icicle-sort-by-directories-last'
    icicle-dirs-last-p
  "Sort file-name completion candidates so that directories are last.
If not doing file-name completion, then sort alphabetically.")

;;;###autoload
(icicle-define-sort-command "by last file modification time"
    icicle-last-modified-first-p        ; `icicle-sort-by-last-file-modification-time'
  "Sort file-name completion candidates in order of last modification.
If not doing file-name completion, then sort alphabetically.")

;;;###autoload
(icicle-define-sort-command "by last use" ; `icicle-sort-by-last-use'
    icicle-most-recent-first-p
  "Sort completion candidates in order of last use as minibuffer input.")

;;;###autoload
(icicle-define-sort-command "by previous use alphabetically"
    icicle-historical-alphabetic-p      ; `icicle-sort-by-previous-use-alphabetically'
  "Sort completion candidates by previous use and alphabetically.
Candidates matching previous inputs are available first.  Candidates
are in two groups, each of which is sorted alphabetically separately:
those matching previous inputs, followed by those that have not yet
been used.")

;;;###autoload
(icicle-define-sort-command "case insensitive" ; `icicle-sort-case-insensitive'
    icicle-case-insensitive-string-less-p
  "Sort completion candidates alphabetically, but case-insenstively.")

;;;###autoload
(icicle-define-sort-command "turned OFF" nil ; `icicle-sort-turned-OFF'
  "Do not sort completion candidates.")

;;;###autoload
(defun icicle-dispatch-C-comma ()       ; Bound to `C-,' in the minibuffer.
  "Do the right thing for `C-,'.
When candidate sorting is possible, call `icicle-change-sort-order'.
When searching, call `icicle-toggle-search-replace-whole'.
Otherwise, do nothing.

Bound to `C-,' in the minibuffer."
  (interactive)
  (cond (icicle-searching-p (icicle-toggle-search-replace-whole))
        (icicle-inhibit-sort-p (message "Cannot sort candidates now"))
        (t (icicle-change-sort-order))))

(defalias 'toggle-icicle-search-replace-whole 'icicle-toggle-search-replace-whole)
;;;###autoload
(defun icicle-toggle-search-replace-whole () ; Bound to `C-,' in the minibuffer.
  "Toggle the value of `icicle-search-replace-whole-candidate-flag'.
Bound to `C-,' in the minibuffer when searching."
  (interactive)
  (setq icicle-search-replace-whole-candidate-flag (not icicle-search-replace-whole-candidate-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-search-replace-whole-candidate-flag
                                      "Replacing whole search context is now ON"
                                    "Replacing whole search context is now OFF")))

;;;###autoload
(defun icicle-change-sort-order (&optional arg alternativep) ; Bound to `C-,' in minibuffer.
  "Choose a sort order.
With a numeric prefix arg, reverse the current sort order.

If plain `C-u' is used or `C-u' is not used at all:

- Use completion if `icicle-change-sort-order-completion-flag' is
  non-nil and no prefix arg is used, or if it is nil and a prefix arg
  is used.

- Otherwise, just cycle to the next sort order.

This command updates `icicle-sort-function'.  Non-interactively,
optional arg ALTERNATIVEP means change the current alternative sort
order instead, updating `icicle-alternative-sort-function'."
  (interactive "P")
  (if (and (interactive-p) icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (if (and arg (not (consp arg)))
        (icicle-reverse-sort-order)
      (let (next-order)
        (cond ((or (and icicle-change-sort-order-completion-flag (not arg))
                   (and (not icicle-change-sort-order-completion-flag) arg))
               (setq next-order (let ((enable-recursive-minibuffers t))
                                  (completing-read
                                   (format "New %ssort order: " (if alternativep "alternative " ""))
                                   icicle-sort-functions-alist nil t)))
               (set (if alternativep 'icicle-alternative-sort-function 'icicle-sort-function)
                    (cdr (assoc next-order icicle-sort-functions-alist))))
              (t
               (let ((orders (mapcar 'car icicle-sort-functions-alist)))
                 (setq next-order (or (cadr (memq (icicle-current-sort-order alternativep) orders))
                                      (car orders)))
                 (set (if alternativep 'icicle-alternative-sort-function 'icicle-sort-function)
                      (cdr (assoc next-order icicle-sort-functions-alist))))))
        (icicle-update-completions)
        (icicle-msg-maybe-in-minibuffer
         "%sorting is now %s" (if alternativep "Alternative s" "S") next-order)))))

;;;###autoload
(defun icicle-dispatch-M-comma ()       ; Bound to `M-,' in the minibuffer.
  "Do the right thing for `M-,'.
If sorting is possible, call `icicle-change-alternative-sort-order'.
If searching, call `icicle-search-define-replacement'.
Otherwise, do nothing.

Bound to `M-,' in the minibuffer."
  (interactive)
  (cond (icicle-searching-p (icicle-search-define-replacement))
        (icicle-inhibit-sort-p (message "Cannot sort candidates now"))
        (t (icicle-change-alternative-sort-order))))

;;;###autoload
(defun icicle-search-define-replacement () ; Bound to `M-,' in the minibuffer.
  "Prompt user and set new value of `icicle-search-replacement'.
Bound to `M-,' in the minibuffer."
  (interactive)
  (setq icicle-search-replacement
        (let ((enable-recursive-minibuffers t))
          (icicle-completing-read-history "Replace with: " 'icicle-search-replacement-history)))
  ;; Just a sanity check.  Cannot really test equivalence of two regexps.
  (while (if icicle-search-replace-whole-candidate-flag
             (equal icicle-search-replacement scan-fn-or-regexp) ; `scan-fn-or...' is free here.
           (equal icicle-search-replacement icicle-current-input))
    (setq icicle-search-replacement (let ((enable-recursive-minibuffers t))
                                      (icicle-completing-read-history
                                       "Replacement = replaced.  Replace with: "
                                       'icicle-search-replacement-history)))))

;;;###autoload
(defun icicle-change-alternative-sort-order (&optional arg) ; Bound to `M-,' in minibuffer.
  "Choose an alternative sort order.
Similar to command `icicle-change-sort-order', but change the
alternative sort order, not the current sort order."
  (interactive "P")
  (if (and (interactive-p) icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (icicle-change-sort-order arg t)))

(defun icicle-current-sort-order (alternativep)
  "Current sort order, or nil if sorting is inactive.
If ALTERNATIVEP is non-nil, the alternative sort order is returned."
  (car (rassq (if alternativep icicle-alternative-sort-function icicle-sort-function)
              icicle-sort-functions-alist)))

;;;###autoload
(defun icicle-reverse-sort-order ()
  "Reverse the current sort order."
  (interactive)
  (if (and (interactive-p) icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (setq icicle-reverse-sort-p (not icicle-reverse-sort-p))
    (icicle-msg-maybe-in-minibuffer
     (format "Sort order is %s" (if icicle-reverse-sort-p "REVERSED" "no longer reversed")))
    (icicle-update-completions)))
 
;;(@* "Other commands to be used mainly in the minibuffer")

;;; Other commands to be used mainly in the minibuffer . . . . . . . .

;; $$ Probably need to do something to work around problem of Windows
;; selecting the new frame, when `pop-up-frames' is non-nil.  Need to
;; redirect focus back to the frame with the minibuffer.  Leave it as
;; is, for now, in hopes Emacs will eventually fix this.
;;
;;;###autoload
(defun icicle-completion-help ()        ; Bound to `C-?' in minibuffer.
  "Describe minibuffer bindings for completion."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (when (icicle-completing-p)
      (princ (concat "You are completing input" (and icicle-candidate-action-fn
                                                     " for an Icicles multi-command")
                     ".\n\n"))
      (princ "To show help on individual completion candidates:
     Current candidate                       C-M-RET, C-M-mouse-2
     Next, previous prefix-match candidate   C-M-down, C-M-up
     Next, previous apropos-match candidate  C-M-next, C-M-prior\n\n")
      (when icicle-candidate-action-fn
        (princ "To act on individual candidates:
     Current candidate                       C-RET, C-o, C-mouse-2
     Next, previous prefix-match candidate   C-down, C-up
     Next, previous apropos-match candidate  C-next, C-prior
     All candidates at once                  C-!
     Delete object named by candidate        S-delete
     Object-action: apply a fn to candidate  M-RET"))
      (when icicle-candidate-alternative-action-fn
        (princ "\nFor alternative action, use `C-S-' instead of `C-',
  except use `C-S-insert' to act on all.\n")))
    (princ (concat "\n" icicle-completion-help-string)))
  ;; Don't bother to do this for Emacs 21.3.  Its `help-insert-xref-button' signature is different.
  (when (and (require 'help-mode nil t) (fboundp 'help-insert-xref-button)) ; In `help-mode.el'.
    (save-excursion
      (with-current-buffer (get-buffer "*Help*")
        (let ((buffer-read-only nil))
          (goto-char (point-min))
          (help-insert-xref-button "[Icicles Help on the Web]" 'icicle-help-button)
          (insert "                   ")
          (help-insert-xref-button "[Icicles Options & Faces]" 'icicle-customize-button)
          (insert "\n\n\n")
          (goto-char (point-max))
          (insert "\n")
          (help-insert-xref-button "[Icicles Help on the Web]" 'icicle-help-button)
          (insert "                   ")
          (help-insert-xref-button "[Icicles Options & Faces]" 'icicle-customize-button)
          (insert "\n\n")
          (goto-char (point-min))))))
  (select-frame-set-input-focus (window-frame (minibuffer-window))))

(when (and (require 'help-mode nil t) (get 'help-xref 'button-category-symbol)) ; In `button.el'
  (define-button-type 'icicle-help-button
      :supertype 'help-xref
      'help-function #'(lambda () (browse-url "http://www.emacswiki.org/cgi-bin/wiki/Icicles"))
      'help-echo
      (purecopy
       "mouse-2, RET: Icicles documentation on the Emacs Wiki (requires Internet access)"))
  (define-button-type 'icicle-customize-button
      :supertype 'help-xref
      'help-function #'(lambda () (customize-group-other-window 'Icicles))
      'help-echo (purecopy "mouse-2, RET: Customize/Browse Icicles Options & Faces")))

;; This is just the macro expansion of the following:
;; `(def-completion-wrapper icicle-abort-minibuffer-input :minibuffer-separator)'.
;; Taken from the definition of `def-completion-wrapper' in `completion.el'.
(put 'icicle-abort-minibuffer-input 'completion-function 'use-completion-minibuffer-separator)

;;;###autoload
(defun icicle-abort-minibuffer-input () ; Bound to `C-g' in minibuffer, `C-g', `q' in *Completions*
  "Abort minibuffer input.
Remove \"*Completions*\" window, if any, before aborting minibuffer
input via `abort-recursive-edit'.
If the minibuffer is not active, then just kill buffer *Completions*."
  (interactive)
  (if (not (active-minibuffer-window))
      (when (get-buffer "*Completions*") (kill-buffer (get-buffer "*Completions*")))
    (icicle-remove-Completions-window)
    (abort-recursive-edit)))

(defun icicle-ensure-overriding-map-is-bound ()
  "Set `overriding-terminal-local-map' to `icicle-universal-argument-map'."
  (if (not (boundp 'overriding-map-is-bound)) ; Emacs 20, 21.
      (setq overriding-terminal-local-map icicle-universal-argument-map)
    (unless overriding-map-is-bound     ; Emacs 22
      (setq saved-overriding-map overriding-terminal-local-map)
      (setq overriding-terminal-local-map icicle-universal-argument-map)
      (setq overriding-map-is-bound t))))

;;;###autoload
(defun icicle-digit-argument (arg) ; Bound to `C-<0-9>', `M-<0-9>', `C-M-<0-9>' in minibuffer.
  "`digit-argument', but also echo the prefix."
  (interactive "P")
  (let* ((char (if (integerp last-command-char)
                   last-command-char
                 (get last-command-char 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (cond ((integerp arg)
           (setq prefix-arg (+ (* arg 10)
                               (if (< arg 0) (- digit) digit))))
          ((eq arg '-)
           ;; Treat -0 as just -, so that -01 will work.
           (setq prefix-arg (if (zerop digit) '- (- digit))))
          (t
           (setq prefix-arg digit))))
  (setq universal-argument-num-events (length (this-command-keys)))
  (icicle-ensure-overriding-map-is-bound)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-negative-argument (arg) ; Bound to `M--', `C-M--' in minibuffer.
  "`negative-argument', but also echo the prefix."
  (interactive "P")
  (cond ((integerp arg) (setq prefix-arg (- arg)))
        ((eq arg '-) (setq prefix-arg nil))
        (t (setq prefix-arg '-)))
  (setq universal-argument-num-events (length (this-command-keys)))
  (icicle-ensure-overriding-map-is-bound)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-universal-argument ()    ; Bound to `C-u' in minibuffer.
  "`universal-argument', but also echo the prefix."
  (interactive)
  (setq prefix-arg (list 4))
  (setq universal-argument-num-events (length (this-command-keys)))
  (icicle-ensure-overriding-map-is-bound)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-universal-argument-more (arg)
  "`universal-argument-more', but also echo the prefix."
  (interactive "P")
  (universal-argument-more arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-universal-argument-other-key (arg)
  "`universal-argument-other-key', but also echo the prefix."
  (interactive "P")
  (universal-argument-other-key arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-universal-argument-minus (arg)
  "`universal-argument-minus', but also echo the prefix."
  (interactive "P")
  (universal-argument-minus arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-apropos-complete-and-exit () ; Bound to `S-RET' in `minibuffer-local-must-match-map'.
  "If the minibuffer contents is a valid apropos completion, then exit.
Otherwise try to complete it.  If completion leads to a valid
completion, then exit.
This is to `minibuffer-complete-and-exit' as `icicle-apropos-complete'
is to `minibuffer-complete'.  That is, it is the regexp-match version."
  (interactive)
  (let* ((icicle-apropos-complete-and-exit-p t) ; Suppress "[Sole apropos completion]" msg & wait.
         (candidates (icicle-apropos-complete)))
    (when (and candidates (null (cdr candidates))) ; Single candidate.
      (old-exit-minibuffer))))

(defun icicle-retrieve-next-input (&optional arg ; Bound to `C-S-l' (`C-L') in minibuffer.
                                   dont-complete-p)
  "Retrieve next minibuffer input.  Like
`icicle-retrieve-previous-input', but traverses history toward the
present.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-retrieve-next-input]')."
  (interactive "P")
  (icicle-retrieve-previous-input arg dont-complete-p 'interactive-p))

(defun icicle-retrieve-previous-input (&optional arg ; Bound to `C-l' in minibuffer.
                                       dont-complete-p reversep)
  "Retrieve previous minibuffer input.
The possible inputs were not necessarily those entered with `RET'.
With a negative prefix arg, this just empties the completion history.
Otherwise:
 Use completion if `icicle-C-l-uses-completion-flag' is non-nil and no
   prefix arg is used, or if it is nil and a prefix arg is used, or if
   `icicle-retrieve-previous-input' is not used interactively.
 Otherwise, just cycle to the previous input.

Non-interactive arguments:
 Non-nil REVERSEP flips the direction to next from previous.
 Non-nil DONT-COMPLETE-P means don't perform completion at the end.

You can use this command only from buffer *Completions or from the
minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-retrieve-previous-input]')."
  (interactive "P")
  (let ((interactive-p (or (interactive-p) (eq reversep 'interactive-p)))
        (prev-inputs-var (if (icicle-file-name-input-p)
                             'icicle-previous-raw-file-name-inputs
                           'icicle-previous-raw-non-file-name-inputs)))
    (when interactive-p (icicle-barf-if-outside-Completions-and-minibuffer))
    (cond ((wholenump (prefix-numeric-value arg))
           (save-selected-window
             (select-window (minibuffer-window))
             (icicle-clear-minibuffer)
             (let* ((prev-inputs (symbol-value prev-inputs-var))
                    (input (if (and interactive-p
                                    (or (and icicle-C-l-uses-completion-flag (not arg))
                                        (and (not icicle-C-l-uses-completion-flag) arg)))
                               (let ((enable-recursive-minibuffers t)
                                     (icicle-show-Completions-initially-flag t))
                                 (prog1 (completing-read
                                         "Retrieve input: " (mapcar #'list prev-inputs) nil t)
                                   (setq icicle-last-input nil)))
                             (if (and (not (equal icicle-current-input icicle-current-raw-input))
                                      (or (not interactive-p)
                                          (not (memq last-command
                                                     `(icicle-retrieve-next-input
                                                       icicle-retrieve-previous-input)))))
                                 ;; $$ If add this, i.* S-TAB C-l won't retrieve i.* - get previous.
                                                       ;; ,@(and interactive-p
                                                       ;;        '(handle-switch-frame)))))))
                                 ;; $$ But if don't add it, C-l can give same twice in a row.
                              
                                 ;; Use this one, if you want to include common-match expansions
                                 ;; and save typed input even when you don't use TAB or S-TAB:
                                 ;; (or icicle-last-input icicle-current-raw-input)

                                 ;; Use this one, if you want to exclude common-match expansions from
                                 ;; completion history, and to save typed input only when you cycle or
                                 ;; complete (TAB or S-TAB):
                                 (if (and icicle-last-input (symbolp last-command)
                                          (get last-command 'icicle-cycling-command))
                                     icicle-last-input
                                   icicle-current-raw-input)
                               (let ((next (member icicle-current-raw-input prev-inputs)))
                                 (if reversep
                                     (or (let ((res nil)
                                               (inputs prev-inputs))
                                           (while (and (consp inputs) (not (eq inputs next)))
                                             (push (pop inputs) res))
                                           (car res))
                                         (car (last prev-inputs)))
                                   (or (cadr next) (car prev-inputs))))))))
               (when input
                 (setq icicle-current-raw-input input)
                 (insert input)
                 (icicle-highlight-initial-whitespace input) ; (e.g. user typo).
                 (icicle-place-cursor input))))
           (unless dont-complete-p
             (let ((icicle-edit-update-p t))
               (funcall (or icicle-last-completion-command 'icicle-apropos-complete))))
           (setq icicle-last-input nil)) ; So TAB will expand it: see `icicle-save-or-restore-input'.
          (t
           (set prev-inputs-var nil)
           (setq icicle-current-raw-input "")
           (icicle-msg-maybe-in-minibuffer "Cleared completion history")))))

;; $$ No longer bound.  Now we bind `icicle-retrieve-previous-input', instead, to `C-l'.
;;;###autoload
(defun icicle-retrieve-last-input ()
  "Put the last real input into the minibuffer.
Use this to replace a completion candidate inserted during cycling.
If `icicle-expand-input-to-common-match-flag' is non-nil, then using this
once restores the longest common match string, and using it twice in
succession restores your original regexp.

You can use this command only from buffer *Completions or from the
minibuffer."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (save-selected-window
    (select-window (minibuffer-window))
    (icicle-clear-minibuffer)
    (if (and icicle-expand-input-to-common-match-flag
             (memq last-command '(icicle-retrieve-last-input handle-switch-frame)))
        (insert icicle-current-raw-input)
      (insert icicle-current-input))
    ;;$$$ (when (interactive-p) (setq icicle-last-completion-command nil))
    (let ((input (if (and icicle-expand-input-to-common-match-flag
                          (memq last-command (list this-command 'handle-switch-frame)))
                     icicle-current-raw-input
                   icicle-current-input)))
      (icicle-highlight-initial-whitespace input) ; Highlight initial whitespace (e.g. user typo).
      (icicle-place-cursor input)))
  (deactivate-mark))

;; $$ No longer used.  It was originally used in `icicle-retrieve-last-input'.
(defun icicle-insert-input (input)
  "Insert INPUT.  Prepend the directory if appropriate."
  (insert (if (and (icicle-file-name-input-p) insert-default-directory)
              (icicle-expand-file-name input (file-name-directory input))
            input)))

;;;###autoload
(defun icicle-insert-history-element () ; Bound to `M-o' in the minibuffer.
  "Use completion to insert a previously entered input in the minibuffer.
Always available for any minibuffer input, not just during completion."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (and (boundp minibuffer-history-variable) (consp (symbol-value minibuffer-history-variable)))
    (let ((enable-recursive-minibuffers t))
      (insert (icicle-completing-read-history "Choose input: " minibuffer-history-variable))))
  (when (and icicle-mode (memq icicle-init-value-flag '(preselect-start preselect-end)))
    (icicle-select-minibuffer-contents)
    (setq deactivate-mark nil)))

;;;###autoload
(defun icicle-insert-string-at-point (&optional arg) ; Bound to `M-.' in minibuffer.
  "Insert text at the cursor into the minibuffer.
Each time this command is called, some text at or near the cursor is
inserted into the minibuffer.  One of two things happens, depending on
the value of option `icicle-default-thing-insertion' and whether or
not you use `C-u'.

`icicle-thing-at-point-functions' is a cons of two parts - call them
ALTERNATIVES and FORWARD-THING.

If ALTERNATIVES is not nil and one of the following is true:
 - FORWARD-THING is nil
 - the value of `icicle-default-thing-insertion' is `alternatives' and
   you have not used `C-u' (without #) in this series of `M-.'
 - the value of `icicle-default-thing-insertion' is `more-of-the-same'
   and you have used `C-u' (without #) in this series of `M-.'
then the next function in ALTERNATIVES is used to retrieve the text to
be inserted.

If FORWARD-THING is not nil and one of the following is true:
 - ALTERNATIVES is nil
 - the value of `icicle-default-thing-insertion' is `more-of-the-same'
   and you have not used `C-u' in this series of `M-.'
 - the value of `icicle-default-thing-insertion' is `alternatives' and
   you have used `C-u' in this series of `M-.'
then function FORWARD-THING is used to retrieve the text to be
inserted.

If `C-u' is used with a numeric argument (not just plain `C-u'), then
function FORWARD-THING is used to retrieve the text to be inserted,
and the argument determines the number of things to grab.  It also
determines the direction of thing-grabbing: A negative argument grabs
text to the left of the cursor; a positive argument grabs text to the
right.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-at-point]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (consp icicle-thing-at-point-functions) ; Option should always be a cons cell.
    (unless (eq last-command this-command) (setq icicle-default-thing-insertion-flipped-p nil))
    (let ((alt-fns (car icicle-thing-at-point-functions))
          (fwd-thing-fn (cdr icicle-thing-at-point-functions))
          (flipped (or icicle-default-thing-insertion-flipped-p ; Already flipped.
                       (setq icicle-default-thing-insertion-flipped-p
                             (if (eq 'alternatives icicle-default-thing-insertion)
                                 arg    ; Either `C-u' or `C-u 3' flips it for `alternatives'.
                               (consp arg)))))) ; Only `C-u' flips it for `more-of-the-same'.
      (cond
        ;; Use alternative text-grabbing functions successively.
        ((and alt-fns (or (if (eq 'alternatives icicle-default-thing-insertion)
                              (not flipped) ; Normal behavior for `alternatives'.
                            flipped)    ; Flipped behavior for `more-of-the-same'.
                          (not fwd-thing-fn))) ; No alternative.
         (setq icicle-successive-grab-count 1) ; In this mode, reset other mode's accumulator.
         (setq icicle-thing-at-pt-fns-pointer
               (if (eq last-command this-command) ; If repeated, get next text-grabbing function.
                   (mod (1+ icicle-thing-at-pt-fns-pointer) (length alt-fns))
                 0))
         (let ((thing "")
               (alt-fn (nth icicle-thing-at-pt-fns-pointer alt-fns)))
           (save-excursion (set-buffer (cadr (buffer-list))) (setq thing (funcall alt-fn)))
           (setq thing (or thing "nil"))
           (icicle-insert-thing thing)
           (icicle-msg-maybe-in-minibuffer (format "`%s'" alt-fn))))

        ;; Use same text-grabbing function successively.
        ((and fwd-thing-fn (or (if (eq 'alternatives icicle-default-thing-insertion)
                                   flipped ; Flipped behavior for `alternatives'.
                                 (not flipped)) ; Normal behavior for `more-of-the-same'.
                               (not alt-fns))) ; No alternative.
         (if (and arg (atom arg))

             ;; Explicit numeric arg.  If it doesn't change direction, then increment
             ;; existing count.  Otherwise, set count absolutely.
             (if (eq last-command this-command)
                 (if (= (icicle-signum icicle-successive-grab-count) ; Repeated `M-.'.
                        (icicle-signum (prefix-numeric-value arg)))
                     (setq icicle-successive-grab-count ; Same direction - increment count.
                           (* (icicle-signum icicle-successive-grab-count)
                              (+ (abs icicle-successive-grab-count)
                                 (abs (prefix-numeric-value arg)))))
                   (setq icicle-successive-grab-count (prefix-numeric-value arg))) ; New dir - set.
               (setq icicle-successive-grab-count (prefix-numeric-value arg))) ; First `M-.' - set.

           ;; No explicit numeric arg.
           ;; If first `M-.' or plain `C-u', set count. Otherwise, increment count.
           (if (eq last-command this-command)
               (setq icicle-successive-grab-count ; Repeated `M-.'.
                     (if (consp arg)
                         ;; We're here from plain `C-u' with `alternatives' - use 1, not 4.
                         (if (wholenump icicle-successive-grab-count) 1 -1)
                       (if (wholenump icicle-successive-grab-count) ; Increment count.
                           (+ icicle-successive-grab-count (abs (prefix-numeric-value arg)))
                         (- icicle-successive-grab-count (abs (prefix-numeric-value arg))))))
             (setq icicle-successive-grab-count 1))) ; First `M-.' - reset count.
         (let ((things ""))
           (save-excursion
             (set-buffer (cadr (buffer-list)))
             (setq things (buffer-substring-no-properties
                           (point)
                           (save-excursion (funcall fwd-thing-fn icicle-successive-grab-count)
                                           (point)))))
           (icicle-insert-thing things)))))))

(defun icicle-signum (num)
  "Return 1 if NUM is positive, -1 if negative, 0 if zero."
  (cond ((< num 0) -1) ((> num 0) 1) (t 0)))

(defun icicle-insert-thing (text &optional no-replace-p)
  "Insert TEXT in the minibuffer.
TEXT replaces the last text that was inserted, if this command repeats
the last and NO-REPLACE-P is nil."
  (when (and (stringp text) (not (string= "" text)))
    (remove-text-properties 0 (length text) '(face nil) text)
    (when (and (eq last-command this-command) (not no-replace-p)
               icicle-insert-string-at-pt-start) ; Ensure that we've defined the ends.
      (delete-region icicle-insert-string-at-pt-start icicle-insert-string-at-pt-end))
    (setq icicle-insert-string-at-pt-start (point))
    (insert text)
    (setq icicle-insert-string-at-pt-end (point))))

;;;###autoload
(defun icicle-insert-string-from-variable (askp) ; Bound to `C-=' in the minibuffer.
  "Insert text into the minibuffer from a variable.
By default, the variable is user option `icicle-input-string'.
To insert from a different variable, use a prefix argument; you are
then prompted for the variable to use.  You can use command
`icicle-save-string-to-variable' to save a string to a variable.
Typically, you store a regexp or part of a regexp in the variable.
This command is bound in the minibuffer to `C-=', by default.
This is especially useful when used with command `icicle-search'.

Some regexps that you might want to assign to variables:

 \"[A-Za-z0-9_.-]+@[A-Za-z0-9_.-]+\"          ; Email address
 \"\\\\([0-9]+\\\.[0-9]+\\\.[0-9]+\\\.[0-9]+\\\\)\"     ; IP address
 \"[0-9]\\\\\\\={4\\\\}-[0-9]\\\\\\\={2\\\\}-[0-9]\\\\\\\={2\\\\}\"   ; Date: 2006-04-14, Time:
 \"^[ \\\=\\t]*[0-9]?[0-9]\\\\([:.]?[0-9][0-9]\\\\)?\\\\(am\\\\|pm\\\\|AM\\\\|PM\\\\)?\"
 \"`\\\\(\\\\sw\\\\sw+\\\\)'\"                        ; Words inside `_'
 \"\\\\*.*\\\\*\"                                 ; Special buffer name: *_*

Standard Emacs Lisp libraries are full of regexps that you can assign
to variables for use with `C-='.
 See `align.el' for regexps for programming languages.
 See `url-dav.el' for regexps matching iso8601 dates.
 See `rmail.el', `sendmail.el', and `mh-show.el' for regexps matching
 mail-header fields.

Imenu regexps occurring as parts of different values of
`imenu-generic-expression' for different buffer types can be used as
variable values for `C-='.  They all work fine with `icicle-search',
turning it into a browser or navigator for the given mode.

See, for example, `generic-x.el' and `lisp-mode.el'.  Here is a regexp
for Javascript function definitions from `generic-x.el':

 \"^function\\\\s-+\\\\([A-Za-z0-9_]+\\\\)\"

And `lisp-imenu-generic-expression' (in `lisp-mode.el') provides
regexps for Lisp function, variable, and type definitions.  Here is
the variable-definition regexp:

 \"^\\\\s-*(\\\\(def\\\\(c\\\\(onst\\\\(ant\\\\)?\\\\|ustom\\\\)\\\\|ine-symbol-macro\\\\|
 parameter\\\\|var\\\\)\\\\)\\\\s-+\\\\(\\\\(\\\\sw\\\\|\\\\s_\\\\)+\\\\)\"

Command `icicle-imenu' exploits this to automatically let you browse
definitions.  It is a specialization of `icicle-search' for Imenu.

For more useful regexps, grep for `font-lock-keywords' in Emacs `lisp'
directory and subdirs.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (save-selected-window
    (select-window (minibuffer-window))
    (if askp
        (let* ((enable-recursive-minibuffers t)
               (var (intern
                     (completing-read
                      "Insert text from variable: "
                      (mapcar
                       #'list
                       (mapcar 'symbol-name
                               '(adaptive-fill-first-line-regexp adaptive-fill-regexp
                                 add-log-current-defun-header-regexp ange-ftp-gateway-prompt-pattern
                                 allout-bullets-string allout-line-boundary-regexp allout-regexp
                                 comment-start-skip comment-end comint-prompt-regexp
                                 ffap-url-regexp find-face-regexp find-function-regexp
                                 find-variable-regexp imenu-example--function-name-regexp-c
                                 org-plain-time-of-day-regexp outline-heading-end-regexp
                                 outline-line-boundary-regexp outline-plain-bullets-string
                                 outline-regexp page-delimiter paragraph-separate paragraph-start
                                 rmail-mime-charset-pattern sentence-end shell-prompt-pattern
                                 telnet-prompt-pattern temp-file-name-pattern
                                 thing-at-point-url-regexp)))
                      (lambda (cand) (boundp (intern (car cand))))
                      nil nil (if (boundp 'variable-name-history)
                                  'variable-name-history
                                'icicle-variable-name-history))))
               ;; Make sure we use the buffer-local value of the variable, if there is one.
               (text (with-current-buffer (cadr (buffer-list)) (symbol-value var))))
          (icicle-insert-thing text 'no-replace))
      (icicle-insert-thing icicle-input-string 'no-replace))))

;;;###autoload
(defun icicle-dispatch-M-q (&optional arg) ; Bound to `M-q' in the minibuffer.
  "Do the right thing for `M-q'.
If searching, call `icicle-toggle-search-whole-word'.
Otherwise, call `icicle-insert-key-description'.
Bound to `M-q' in the minibuffer."
  (interactive "P") ; Argument is ignored for `icicle-toggle-search-whole-word'.
  (cond (icicle-searching-p (icicle-toggle-search-whole-word))
        (t (icicle-insert-key-description arg))))

(defalias 'toggle-icicle-search-whole-word 'icicle-toggle-search-whole-word)
;;;###autoload
(defun icicle-toggle-search-whole-word () ; Bound to `M-q' in the minibuffer.
  "Toggle the value of `icicle-search-whole-word-flag'.
The new value takes effect for the next Icicles search command.
Bound to `M-q' in the minibuffer when searching."
  (interactive)
  (setq icicle-search-whole-word-flag (not icicle-search-whole-word-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-search-whole-word-flag
                                      "Whole-word searching is now ON, starting with next search"
                                    "Whole-word searching is now OFF, starting with next search")))

;;;###autoload
(defun icicle-insert-key-description (toggle-angle-brackets-p) ; Bound to `M-q' in minibuffer.
  "Read key and insert its description.
For example, if the key read is ^F, then \"C-f\" is inserted.

`icicle-key-descriptions-use-<>-flag' determines whether angle
brackets (`<', `>') are used for named keys, such as function
keys, but a prefix argument reverses the meaning of
`icicle-key-descriptions-use-<>-flag'.

Bound to `M-q' in the minibuffer during key completion."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((enable-recursive-minibuffers t)
         (key (progn (minibuffer-message " [Quoting key]") (read-event))))
    (insert (single-key-description key (if toggle-angle-brackets-p
                                            icicle-key-descriptions-use-<>-flag
                                          (not icicle-key-descriptions-use-<>-flag))))))
                                      
;;;###autoload
(defun icicle-pp-eval-expression ()     ; Bound to `M-:' in minibuffer.
  "Evaluate an expression and pretty-print its value.
This just calls `pp-eval-expression' from a recursive minibuffer."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let ((enable-recursive-minibuffers t)
        (icicle-reminder-prompt-flag nil))
    (call-interactively 'pp-eval-expression))
  (select-frame-set-input-focus (window-frame (minibuffer-window))))

;;;###autoload
(defun icicle-next-candidate-per-mode (&optional nth) ; Bound to `icicle-modal-cycle-down-key'
                                        ; in minibuffer.
  "Replace input by NTH next completion candidate.
Default value of NTH is 1, meaning use the next candidate.
Negative NTH means use a previous, not subsequent, candidate.

Uses the next prefix or apropos completion command, depending on
`icicle-current-completion-mode'.  If that is nil and
`icicle-cycling-respects-completion-mode-flag' is non-nil, use the
next history element instead.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-candidate-per-mode]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (cond ((eq icicle-current-completion-mode 'prefix)
         (setq this-command 'icicle-next-prefix-candidate)
         (icicle-next-prefix-candidate nth))
        ((eq icicle-current-completion-mode 'apropos)
         (setq this-command 'icicle-next-apropos-candidate)
         (icicle-next-apropos-candidate nth))
        ((and (eq icicle-current-completion-mode nil) icicle-cycling-respects-completion-mode-flag)
         (next-history-element (or nth 1)))))

;;;###autoload
(defun icicle-previous-candidate-per-mode (&optional nth) ; Bound to `icicle-modal-cycle-up-key'
                                        ; in minibuffer.
  "Replace input by NTH previous completion candidate.
Default value of NTH is 1, meaning use the previous candidate.
Negative NTH means use a subsequent, not previous, candidate.

Uses the previous prefix or apropos completion command, depending on
`icicle-current-completion-mode'. If that is nil and
`icicle-cycling-respects-completion-mode-flag' is non-nil, use the
previous history element instead.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-candidate-per-mode]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-next-candidate-per-mode (- (or nth 1))))

(put 'icicle-previous-prefix-candidate 'icicle-cycling-command t)
(put 'icicle-previous-prefix-candidate 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-previous-prefix-candidate (&optional nth) ; Bound to `C-p', `up' in minibuffer.
  "Replace input by NTH previous prefix completion for an input.
Default value of NTH is 1, meaning use the previous prefix completion.
Negative NTH means use a subsequent, not previous, prefix completion.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-prefix-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq nth (or nth 1))
  (icicle-next-prefix-candidate (- nth)))

(put 'icicle-next-prefix-candidate 'icicle-cycling-command t)
(put 'icicle-next-prefix-candidate 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-next-prefix-candidate (&optional nth) ; Bound to `down', `C-n' in minibuffer.
  "Replace input by NTH next prefix completion for an input.
Default value of NTH is 1, meaning use the next prefix completion.
Negative NTH means use a previous, not subsequent, prefix completion.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-prefix-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode 'prefix)
  (icicle-next-candidate nth (if (icicle-file-name-input-p)
                                 'icicle-file-name-prefix-candidates
                               'icicle-prefix-candidates)))

(put 'icicle-previous-apropos-candidate 'icicle-cycling-command t)
(put 'icicle-previous-apropos-candidate 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-previous-apropos-candidate (&optional nth) ; Bound to `prior', `M-v' in minibuffer.
  "Replace input by NTH previous apropos completion for an input.
Default value of NTH is 1, meaning use the previous apropos completion.
Negative NTH means use a subsequent, not previous, apropos completion.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-apropos-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq nth (or nth 1))
  (icicle-next-apropos-candidate (- nth)))

(put 'icicle-next-apropos-candidate 'icicle-cycling-command t)
(put 'icicle-next-apropos-candidate 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-next-apropos-candidate (&optional nth) ; Bound to `next', `C-v' in minibuffer.
  "Replace input by NTH next apropos completion for an input.
Default value of NTH is 1, meaning use the next apropos completion.
Negative NTH means use a previous, not subsequent, apropos completion.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-apropos-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode 'apropos)
  (icicle-next-candidate nth (if (icicle-file-name-input-p)
                                 'icicle-file-name-apropos-candidates
                               'icicle-apropos-candidates)
                         'regexp-p))

(put 'icicle-previous-prefix-candidate-action 'icicle-cycling-command t)
(put 'icicle-previous-prefix-candidate-action 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-previous-prefix-candidate-action (&optional nth) ; Bound to `C-up', `M-{' in minibuf.
  "`icicle-previous-prefix-candidate' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-previous-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-prefix-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-prefix-candidate #'icicle-candidate-action nth))

(put 'icicle-next-prefix-candidate-action 'icicle-cycling-command t)
(put 'icicle-next-prefix-candidate-action 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-next-prefix-candidate-action (&optional nth) ; Bound to `C-down', `M-}' in minibuf.
  "`icicle-next-prefix-candidate' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-next-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-prefix-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-prefix-candidate #'icicle-candidate-action nth))

(put 'icicle-previous-apropos-candidate-action 'icicle-cycling-command t)
(put 'icicle-previous-apropos-candidate-action 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-previous-apropos-candidate-action (&optional nth) ; Bound to `C-prior', `C-x >' in minib
  "`icicle-previous-apropos-candidate' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-previous-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-apropos-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-apropos-candidate #'icicle-candidate-action nth))

(put 'icicle-next-apropos-candidate-action 'icicle-cycling-command t)
(put 'icicle-next-apropos-candidate-action 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-next-apropos-candidate-action (&optional nth) ; Bound to `C-next', `C-x <' in minibuffer
  "`icicle-next-apropos-candidate' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-next-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-apropos-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-apropos-candidate #'icicle-candidate-action nth))

(put 'icicle-previous-prefix-candidate-alt-action 'icicle-cycling-command t)
(put 'icicle-previous-prefix-candidate-alt-action 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-previous-prefix-candidate-alt-action (&optional nth) ; Bound to `C-S-up' in minibuffer.
  "`icicle-previous-prefix-candidate' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-previous-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-prefix-candidate-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-prefix-candidate #'icicle-candidate-alt-action nth))

(put 'icicle-next-prefix-candidate-alt-action 'icicle-cycling-command t)
(put 'icicle-next-prefix-candidate-alt-action 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-next-prefix-candidate-alt-action (&optional nth) ; Bound to `C-S-down' in minibuffer.
  "`icicle-next-prefix-candidate' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-next-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-prefix-candidate-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-prefix-candidate #'icicle-candidate-alt-action nth))

(put 'icicle-previous-apropos-candidate-alt-action 'icicle-cycling-command t)
(put 'icicle-previous-apropos-candidate-alt-action 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-previous-apropos-candidate-alt-action (&optional nth) ; Bound to `C-S-prior' in minibuf.
  "`icicle-previous-apropos-candidate' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-previous-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-apropos-candidate-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-apropos-candidate #'icicle-candidate-alt-action nth))

(put 'icicle-next-apropos-candidate-alt-action 'icicle-cycling-command t)
(put 'icicle-next-apropos-candidate-alt-action 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-next-apropos-candidate-alt-action (&optional nth) ; Bound to `C-S-next' in minibuffer.
  "`icicle-next-apropos-candidate' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-next-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-apropos-candidate-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-apropos-candidate #'icicle-candidate-alt-action nth))

(put 'icicle-help-on-previous-prefix-candidate 'icicle-cycling-command t)
(put 'icicle-help-on-previous-prefix-candidate 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-help-on-previous-prefix-candidate (&optional nth) ; Bound to `C-M-up' in minibuf.
  "`icicle-previous-prefix-candidate' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-previous-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-previous-prefix-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-prefix-candidate #'icicle-help-on-candidate nth))

(put 'icicle-help-on-next-prefix-candidate 'icicle-cycling-command t)
(put 'icicle-help-on-next-prefix-candidate 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-help-on-next-prefix-candidate (&optional nth) ; Bound to `C-M-down' in minibuf.
  "`icicle-next-prefix-candidate' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-next-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-prefix-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-prefix-candidate #'icicle-help-on-candidate nth))

(put 'icicle-help-on-previous-apropos-candidate 'icicle-cycling-command t)
(put 'icicle-help-on-previous-apropos-candidate 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-help-on-previous-apropos-candidate (&optional nth) ; Bound to `C-M-prior' in minibuf.
  "`icicle-previous-apropos-candidate' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-previous-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-previous-apropos-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-apropos-candidate #'icicle-help-on-candidate nth))

(put 'icicle-help-on-next-apropos-candidate 'icicle-cycling-command t)
(put 'icicle-help-on-next-apropos-candidate 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-help-on-next-apropos-candidate (&optional nth) ; Bound to `C-M-next' in minibuf.
  "`icicle-next-apropos-candidate' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH is as for `icicle-next-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-apropos-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-apropos-candidate #'icicle-help-on-candidate nth))

(defun icicle-successive-action (nav-fn action-fn nth)
  "Call NAV-FN and ACTION-FN.  Pass NTH to ACTION-FN.
Set `icicle-current-completion-mode'.  The order respects the setting
of `icicle-act-before-cycle-flag'."
  (setq icicle-current-completion-mode
        (if (get nav-fn 'icicle-apropos-cycling-command) 'apropos 'prefix))
  (cond (icicle-act-before-cycle-flag
         (save-excursion (save-selected-window (funcall action-fn))) (funcall nav-fn nth))
        (t
         (funcall nav-fn nth) (save-excursion (save-selected-window (funcall action-fn))))))

(put 'icicle-prefix-complete 'icicle-completing-command t)
;;;###autoload
(defun icicle-prefix-complete ()        ; Bound to `TAB' in minibuffer.
  "Complete the minibuffer contents as far as possible, as a prefix.
If no characters can be completed, display the possible completions.
Scroll *Completions* window if this command is repeated.
Candidate completions are appropriate names whose prefix is the
minibuffer input, where appropriateness is determined by the context
\(command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-prefix-complete-1))

(put 'icicle-prefix-complete-no-display 'icicle-completing-command t)
;;;###autoload
(defun icicle-prefix-complete-no-display () ; Bound to `C-M-TAB' in minibuffer.
  "Like `icicle-prefix-complete', but without displaying *Completions*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete-no-display]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-prefix-complete-1 'no-display))

;;;###autoload
(defun icicle-prefix-complete-1 (&optional no-display-p)
  "Helper function for `icicle-prefix-complete(-no-display)'.
Optional argument NO-DISPLAY-P non-nil means do not display buffer
*Completions*.  Return the list of completion candidates."
  (setq icicle-current-completion-mode 'prefix
        icicle-current-input           (if (and icicle-last-input (symbolp last-command)
                                                (get last-command 'icicle-cycling-command))
                                           icicle-last-input ; $$ Previously didn't allow -action's.
                                         (icicle-minibuffer-contents-from-minibuffer))
        icicle-input-fail-pos          nil)
  (when (icicle-file-name-input-p)
    (setq icicle-current-input (abbreviate-file-name icicle-current-input)))
  (unless (and (stringp icicle-current-input) (stringp icicle-last-input)
               (string= icicle-current-input icicle-last-input)
               (eq last-command 'icicle-prefix-complete))
    (setq icicle-completion-candidates
          (condition-case nil
              (if (icicle-file-name-input-p)
                  (icicle-file-name-prefix-candidates icicle-current-input)
                (icicle-prefix-candidates icicle-current-input))
            (error icicle-completion-candidates)))) ; No change if completion error.
  (icicle-save-or-restore-input)
  (cond ((null icicle-completion-candidates)
         (setq icicle-nb-of-other-cycle-candidates 0)
         (icicle-highlight-input-noncompletion (icicle-minibuffer-contents))
         (save-selected-window (icicle-remove-Completions-window))
         (minibuffer-message (if (and icicle-fuzzy-completion-flag (featurep 'fuzzy-match))
                                 "  [No fuzzy completions]"
                               "  [No prefix completions]")))
        ((null (cdr icicle-completion-candidates)) ; Single candidate.  Update minibuffer.
         (setq icicle-nb-of-other-cycle-candidates 0)
         (unless icicle-edit-update-p
           (icicle-clear-minibuffer)
           (setq icicle-last-completion-candidate (car icicle-completion-candidates))
           (let ((inserted (if (and (icicle-file-name-input-p) insert-default-directory)
                               (icicle-abbreviate-or-expand-file-name
                                icicle-last-completion-candidate
                                (icicle-file-name-directory-w-default icicle-current-input))
                             icicle-last-completion-candidate)))
             (insert inserted)
             (when (and (icicle-file-name-input-p)
                        (icicle-file-directory-p (icicle-abbreviate-or-expand-file-name inserted)))
               (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                               inserted)))))
         (save-selected-window (icicle-remove-Completions-window))
         (icicle-transform-sole-candidate)
         (icicle-highlight-complete-input)
         (if icicle-top-level-when-sole-completion-flag
             (condition-case icicle-prefix-complete-1
                 (throw 'icicle-read-top (car icicle-completion-candidates))
               (no-catch (setq icicle-current-input (car icicle-completion-candidates))
                         (icicle-retrieve-last-input)
                         icicle-current-input)
               (error (message (error-message-string icicle-prefix-complete-1))))
           (if icicle-edit-update-p
               (minibuffer-message
                (format (if (and icicle-fuzzy-completion-flag (featurep 'fuzzy-match))
                            "  [One fuzzy completion: %s]"
                          "  [One prefix completion: %s]")
                        (car icicle-completion-candidates)))
             (minibuffer-message
              (if (and icicle-fuzzy-completion-flag (featurep 'fuzzy-match))
                  "  [Sole fuzzy completion]"
                "  [Sole prefix completion]")))))
        (t                              ; Multiple candidates.
         (if icicle-edit-update-p
             (icicle-display-candidates-in-Completions nil no-display-p)
           (icicle-clear-minibuffer)
           (insert icicle-current-input) ; Update minibuffer.
           (deactivate-mark)
           (icicle-highlight-initial-whitespace icicle-current-input)
           (when (and (icicle-file-name-input-p)
                      (icicle-file-directory-p icicle-last-completion-candidate))
             (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate)))
           (when (member (if (icicle-file-name-input-p)
                             (icicle-file-name-nondirectory icicle-current-input)
                           icicle-current-input)
                         icicle-completion-candidates)
             (icicle-highlight-complete-input))
           (cond ((get-buffer-window "*Completions*" 0)
                  (if (and (eq icicle-last-completion-command 'icicle-prefix-complete)
                           (memq last-command '(icicle-prefix-complete handle-switch-frame)))
                      ;; Second `TAB' in a row.  Scroll window around.
                      (icicle-scroll-Completions)
                    ;; Did something else (e.g. changed input).  Update the display.
                    (icicle-display-candidates-in-Completions nil no-display-p)))
                 ;; No window yet.  If 2nd TAB or no chars can be completed, show window.
                 (t
                  (cond (icicle-TAB-shows-candidates-flag
                         (icicle-display-candidates-in-Completions nil no-display-p))
                        ((and (eq icicle-last-completion-command 'icicle-prefix-complete)
                              (memq last-command '(icicle-prefix-complete handle-switch-frame))
                              completion-auto-help)
                         (icicle-display-candidates-in-Completions nil no-display-p))
                        ((member icicle-current-input icicle-completion-candidates)
                         (minibuffer-message "  [Complete, but not unique]"))
                        ((and (string= icicle-current-raw-input icicle-current-input)
                              completion-auto-help)
                         (icicle-display-candidates-in-Completions nil no-display-p))))))))
  (setq icicle-last-completion-command 'icicle-prefix-complete)
  icicle-completion-candidates)

(put 'icicle-prefix-word-complete 'icicle-completing-command t)
;;;###autoload
(defun icicle-prefix-word-complete ()   ; Bound to `M-SPC' in minibuffer.
  "Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion.
Return nil if there is no valid completion, else t.
Candidate completions are appropriate names whose prefix is the
minibuffer input, where appropriateness is determined by the context
\(command, variable, and so on).

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-word-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-input
        (if (and (symbolp last-command) (get last-command 'icicle-cycling-command))
            icicle-last-input           ; $$ Previously didn't allow the -action's.
          (icicle-minibuffer-contents-from-minibuffer)))
  (let ((return-value (minibuffer-complete-word)))
    (setq icicle-completion-candidates
          (condition-case nil
              (if (icicle-file-name-input-p)
                  (icicle-file-name-prefix-candidates icicle-current-input)
                (icicle-prefix-candidates icicle-current-input))
            (error icicle-completion-candidates))) ; No change if completion error.
    (when (get-buffer-window "*Completions*" 0)
      (icicle-display-candidates-in-Completions))
    (setq icicle-last-completion-command 'icicle-prefix-word-complete)
    return-value))

(put 'icicle-apropos-complete 'icicle-completing-command t)
;;;###autoload
(defun icicle-apropos-complete ()       ; Bound to `S-TAB' in minibuffer.
  "Complete the minibuffer contents as far as possible.
This uses \"apropos completion\", defined as follows:
A completion contains the minibuffer input somewhere, as a substring.
Display a list of possible completions in buffer *Completions*.
Scroll *Completions* window if this command is repeated.
Candidate completions are appropriate names that match the current
input, taken as a regular expression, where appropriateness is
determined by the context (command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((error-msg nil)
         (candidates
          (condition-case lossage
              (icicle-apropos-complete-1)
            (invalid-regexp
             (setq error-msg (car (cdr lossage)))
             ;;$$ (setq icicle-within-brackets (string-match "\\`Unmatched \\[" error-msg))
             (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
               (setq error-msg "incomplete input")))
            (error (setq error-msg (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))

(put 'icicle-apropos-complete-no-display 'icicle-completing-command t)
;;;###autoload
(defun icicle-apropos-complete-no-display () ; Bound to `C-M-S-TAB' in minibuffer.
  "Like `icicle-apropos-complete', but without displaying *Completions*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete-no-display]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((error-msg nil)
         (candidates
          (condition-case lossage
              (icicle-apropos-complete-1 'no-display)
            (invalid-regexp
             (setq error-msg (car (cdr lossage)))
             ;;$$ (setq icicle-within-brackets (string-match "\\`Unmatched \\[" error-msg))
             (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
               (setq error-msg "incomplete input")))
            (error (setq error-msg (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))

(defun icicle-apropos-complete-1 (&optional no-display-p)
  "Helper function for `icicle-apropos-complete(-no-display)'.
This does everything, except deal with regexp-match errors.
Optional argument NO-DISPLAY-P non-nil means do not display buffer
*Completions*.  Return the list of completion candidates."
  (setq icicle-current-completion-mode 'apropos
        icicle-current-input           (if (and icicle-last-input (symbolp last-command)
                                                (get last-command 'icicle-cycling-command))
                                           icicle-last-input
                                         (icicle-minibuffer-contents-from-minibuffer))
        icicle-input-fail-pos          nil)
  (when (icicle-file-name-input-p)
    (setq icicle-current-input (abbreviate-file-name icicle-current-input)))
  (unless (and (stringp icicle-current-input) (stringp icicle-last-input)
               (string= icicle-current-input icicle-last-input)
               (eq last-command 'icicle-apropos-complete))
    (setq icicle-completion-candidates
          (condition-case nil
              (if (icicle-file-name-input-p)
                  (icicle-file-name-apropos-candidates icicle-current-input)
                (icicle-apropos-candidates icicle-current-input))
            (error icicle-completion-candidates)))) ; No change if completion error.
  ;; If input matches an empty directory, then use that directory as the sole completion.
  (when (and (icicle-file-name-input-p) (null icicle-completion-candidates)
             (string-match "/$" icicle-current-input))
    (setq icicle-completion-candidates '("")))
  (icicle-save-or-restore-input)
  (cond ((null icicle-completion-candidates)
         (setq icicle-nb-of-other-cycle-candidates 0)
         (icicle-highlight-input-noncompletion (icicle-minibuffer-contents))
         (save-selected-window (icicle-remove-Completions-window))
         (minibuffer-message "  [No apropos completion]"))
        ((null (cdr icicle-completion-candidates)) ; Single candidate. Update minibuffer.
         (setq icicle-nb-of-other-cycle-candidates 0)
         (unless icicle-edit-update-p
           (icicle-clear-minibuffer)
           (setq icicle-last-completion-candidate (car icicle-completion-candidates))
           (let ((inserted (if (and (icicle-file-name-input-p) insert-default-directory)
                               (icicle-abbreviate-or-expand-file-name
                                icicle-last-completion-candidate
                                (icicle-file-name-directory-w-default icicle-current-input))
                             icicle-last-completion-candidate)))
             (insert inserted)
             (when (and (icicle-file-name-input-p)
                        (icicle-file-directory-p (icicle-abbreviate-or-expand-file-name inserted)))
               (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                               inserted)))))
         (save-selected-window (icicle-remove-Completions-window))
         (icicle-transform-sole-candidate)
         (unless (boundp 'icicle-apropos-complete-and-exit-p)
           (icicle-highlight-complete-input)
           (if icicle-top-level-when-sole-completion-flag
               (condition-case icicle-apropos-complete-1
                   (throw 'icicle-read-top (car icicle-completion-candidates))
                 (no-catch (setq icicle-current-input (car icicle-completion-candidates))
                           (icicle-retrieve-last-input)
                           icicle-current-input)
                 (error (message (error-message-string icicle-apropos-complete-1))))
             (if icicle-edit-update-p
                 (minibuffer-message (format "  [One apropos completion: %s]"
                                             (car icicle-completion-candidates)))
               (minibuffer-message "  [Sole apropos completion]")))))
        (t                              ; Multiple candidates.
         (if icicle-edit-update-p
             (icicle-display-candidates-in-Completions nil no-display-p)
           (icicle-clear-minibuffer)
           (insert icicle-current-input) ; Update minibuffer.
           (deactivate-mark)
           (icicle-highlight-initial-whitespace icicle-current-input)
           (when (and (icicle-file-name-input-p)
                      (icicle-file-directory-p icicle-last-completion-candidate))
             (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate)))
           (when (member (if (icicle-file-name-input-p)
                             (icicle-file-name-nondirectory icicle-current-input)
                           icicle-current-input)
                         icicle-completion-candidates)
             (icicle-highlight-complete-input))
           (if (get-buffer-window "*Completions*" 0)
               (if (and (eq icicle-last-completion-command 'icicle-apropos-complete)
                        (memq last-command '(icicle-apropos-complete handle-switch-frame)))
                   ;; Second `S-TAB' in a row.  Scroll window around.
                   (icicle-scroll-Completions)
                 ;; Did something else (e.g. changed input).  Update the display.
                 (icicle-display-candidates-in-Completions nil no-display-p))
             ;; No window yet.  Show window.
             (icicle-display-candidates-in-Completions nil no-display-p)))))
  (setq icicle-last-completion-command 'icicle-apropos-complete)
  icicle-completion-candidates)

(defun icicle-transform-sole-candidate ()
  "Transform matching candidate according to `icicle-list-use-nth-parts'."
  (when (and icicle-list-use-nth-parts icicle-list-join-string)
    (let ((newcand (icicle-transform-multi-completion (car icicle-completion-candidates))))
      (icicle-clear-minibuffer)
      (insert newcand)
      (setq icicle-completion-candidates      (list newcand)
            icicle-last-completion-candidate  newcand))))

(defun icicle-transform-multi-completion (candidate)
  "Transform CANDIDATE according to `icicle-list-use-nth-parts'.
If CANDIDATE is not a multi-completion, do nothing.
Return the possibly transformed candidate."
  (if (and icicle-list-use-nth-parts icicle-list-join-string)
      (let* ((parts (split-string candidate icicle-list-join-string))
             (maxpart (length parts))
             (indexes icicle-list-use-nth-parts)
             (cand "")
             (firstp t)
             partnum)
        (while indexes
          (setq partnum (car indexes))
          (when (> partnum maxpart) (setq partnum maxpart))
          (unless firstp (setq cand (concat cand icicle-list-nth-parts-join-string)))
          (setq firstp nil)
          (setq cand (concat cand (nth (1- partnum) parts)))
          (setq indexes (cdr indexes)))
        cand)
    candidate))

;;;###autoload
(defun icicle-switch-to-Completions-buf () ; Bound to `C-insert' in minibuffer.
  "Select the completion list window.
The cursor is placed on the first occurrence of the current minibuffer
content.  You can use \\<completion-list-mode-map>\
`\\[icicle-insert-completion]' to get back to the minibuffer.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-switch-to-Completions-buf]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))
  (let ((window (get-buffer-window "*Completions*" 0))
        (search-fn 'search-forward))
    (unless window                      ; Make sure we have a completions window.
      (icicle-apropos-complete)
      (setq window (get-buffer-window "*Completions*" 0)
            search-fn 're-search-forward)) ; Use regexp search: input is not yet complete.
    (when window
      (select-window window)
      (let ((case-fold-search (if (and (icicle-file-name-input-p)
                                       (boundp 'read-file-name-completion-ignore-case))
                                  read-file-name-completion-ignore-case
                                completion-ignore-case)))
        (goto-char (icicle-start-of-candidates-in-Completions))
        (when (icicle-file-name-input-p)
          (setq icicle-current-input (icicle-file-name-nondirectory icicle-current-input)))
        (when (and (eq icicle-last-completion-command 'icicle-apropos-complete)
                   ;; $$ Previously allowed the -action's.
                   (not (and (symbolp last-command) (get last-command 'icicle-cycling-command))))
          (setq search-fn 're-search-forward)) ; Use regexp search: input is not yet complete.
        (while (and (not (eobp))
                    (save-restriction
                      (narrow-to-region (point) (next-single-property-change (point) 'mouse-face
                                                                             nil (point-max)))
                      (not (funcall search-fn icicle-current-input nil 'leave-at-end)))))
        (unless (eobp)
          (goto-char (match-beginning 0))
          (let ((prop (get-text-property (1- (point)) 'mouse-face)))
            ;; If in a completion, move to the start of it.
            (when (and prop (eq prop (get-text-property (point) 'mouse-face)))
              (goto-char (previous-single-property-change (point) 'mouse-face nil (point-min)))))
          (icicle-place-overlay
           (point) (next-single-property-change (point) 'mouse-face nil (point-max))
           'icicle-current-completion-candidate-overlay 'icicle-current-candidate-highlight
           100 (current-buffer)))))))

;;;###autoload
(defun icicle-insert-completion (&optional completion) ; Bound to `C-insert' in *Completions*.
  "Select the active minibuffer window.  Insert current completion.
The current candidate in *Completions* (under the cursor) is inserted
into the minibuffer as the current input.  You can use \\<minibuffer-local-completion-map>\
`\\[icicle-switch-to-Completions-buf]'
to switch to the *Completions* window.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-insert-completion]').

Non-interactively, optional arg COMPLETION is the completion inserted."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (when (active-minibuffer-window)
    (unwind-protect                     ; If no current completion, return to minibuffer anyway.
         (progn
           (setq completion (or completion (icicle-transform-multi-completion
                                            (icicle-current-completion-in-Completions))))
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if *Completions* is redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)
             (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                         (icicle-file-name-directory-w-default icicle-current-input)
                       "")
                     completion)
             (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))))
      (select-window (active-minibuffer-window)))))

(defun icicle-current-completion-in-Completions ()
  "The completion candidate under the cursor in buffer *Completions*.
Return the name as a string."           ; This code comes from `choose-completion'.
  (let ((buffer completion-reference-buffer)
        (base-size completion-base-size)
        beg end completion)
    (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
      (setq end (point) beg (1+ (point))))
    (when (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
      (setq end (1- (point)) beg (point)))
    (when (null beg) (error "No completion here"))
    (setq beg (or (previous-single-property-change beg 'mouse-face) (point-min))
          end (or (next-single-property-change end 'mouse-face) (point-max)))
    (buffer-substring beg end)))

;;;###autoload
(defun icicle-switch-to/from-minibuffer () ; Bound to `pause' in Icicle mode.
  "Switch to minibuffer or previous buffer, in other window.
If current buffer is the minibuffer, then switch to the buffer that
was previously current.  Otherwise, switch to the minibuffer."
  (interactive)
  (unless (active-minibuffer-window) (error "Minibuffer is not active"))  
  (if (eq (selected-window) (active-minibuffer-window))
      (switch-to-buffer-other-window icicle-pre-minibuffer-buffer)
    (select-window (active-minibuffer-window))))


;; Replaces `previous-completion' (defined in `simple.el').
;;;###autoload
(defun icicle-move-to-previous-completion (n) ; Bound to `left', `S-TAB' in *Completions*.
  "Move to the previous item in the completion list.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-move-to-previous-completion]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (setq n (or n 0))
  (icicle-move-to-next-completion (- n)))


;; Replaces `next-completion' (defined in `simple.el').
;; This is the same code, except:
;; 1. This highlights the current candidate.
;; 2. This wraps around from first to last and last to first.
;;;###autoload
(defun icicle-move-to-next-completion (n &optional no-minibuffer-follow-p) ; Bound to `right', `TAB'
                                        ;  in *Completions*.
  "Move to the next item in the completion list.
With prefix argument N, move N items (negative N means move backward).
Optional second argument, if non-nil, means do not copy the completion
back to the minibuffer.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-move-to-next-completion]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (setq n (or n 0))
  (let ((beg (icicle-start-of-candidates-in-Completions))
        (end (point-max)))
    (while (and (> n 0) (not (eobp)))
      ;; If in a completion, move to the end of it.
      (when (get-text-property (point) 'mouse-face)
        (goto-char (next-single-property-change (point) 'mouse-face nil end)))
      ;; Move to start of next one.
      (unless (get-text-property (point) 'mouse-face)
        (goto-char (or (next-single-property-change (point) 'mouse-face)
                       beg)))           ; Wrap back to first candidate.
      (setq n (1- n)))
    (while (and (< n 0) (>= (count-lines 1 (point)) 3))
      (let ((prop (get-text-property (1- (point)) 'mouse-face)))
        ;; If in a completion, move to the start of it.
        (when (and prop (eq prop (get-text-property (point) 'mouse-face)))
          (goto-char (previous-single-property-change (point) 'mouse-face nil beg))))
      ;; Move to end of the previous completion.
      (unless (or (< (count-lines 1 (point)) 3)
                  (get-text-property (1- (point)) 'mouse-face))
        (goto-char (or (previous-single-property-change (point) 'mouse-face)
                       end)))           ; Wrap back to last candidate.
      ;; Move to the start of that one.
      (goto-char (previous-single-property-change (point) 'mouse-face nil beg))
      (setq n (1+ n)))
    (icicle-place-overlay
     (point) (next-single-property-change (point) 'mouse-face nil end)
     'icicle-current-completion-candidate-overlay 'icicle-current-candidate-highlight
     100 (current-buffer)))
  (unless no-minibuffer-follow-p
    (save-excursion (save-window-excursion (icicle-insert-completion)))))

;;;###autoload
(defun icicle-previous-line ()          ; Bound to `up' *Completions*.
  "Move up a line, in *Completions* buffer.  Wrap around first to last.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-previous-line]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (let ((bolp-at-start (bolp)))
    (if (> (count-lines 1 (point)) (if bolp-at-start 3 4))
        (icicle-move-to-previous-completion 2)
      (goto-char (point-max))
      (icicle-move-to-previous-completion 1)
      (if bolp-at-start
          (while (not (bolp)) (icicle-move-to-previous-completion 1))
        (while (bolp) (icicle-move-to-previous-completion 1))))))

;;;###autoload
(defun icicle-next-line ()              ; Bound to `down' in *Completions*.
  "Move down a line, in *Completions* buffer.  Wrap around last to first.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-next-line]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (let ((num-lines (- (count-lines (point-min) (point-max)) 1))
        (bolp-at-start (bolp)))
    (cond ((< (count-lines 1 (point)) (if bolp-at-start num-lines (1+ num-lines)))
           (icicle-move-to-next-completion 2)
           (when (and (bolp) (not bolp-at-start)) (icicle-move-to-next-completion 1)))
          (t
           (goto-char (point-min))
           (icicle-move-to-next-completion 1)
           (if bolp-at-start
               (while (not (bolp))
                 (icicle-move-to-next-completion 1))
             (while (bolp) (icicle-move-to-next-completion 1)))))))

;;;###autoload
(defun icicle-all-candidates-action ()  ; Bound to `C-!' in minibuffer.
  "Take action on all completion candidates.
Apply `icicle-candidate-action-fn' to each completion candidate that
matches the current input (a regular expression), successively.
The candidates that were not successfully acted upon are listed in
buffer *Help*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (unless (or icicle-all-candidates-action-fn icicle-candidate-action-fn)
    (error "No action defined."))
  (icicle-all-candidates-action-1 (or icicle-all-candidates-action-fn icicle-candidate-action-fn)
                                  icicle-all-candidates-action-fn))

;;;###autoload
(defun icicle-all-candidates-alt-action () ; Bound to `C-S-insert' in minibuffer.
  "Take alternative action on all completion candidates.
Apply `icicle-candidate-alternative-action-fn' to each completion
andidate that matches the current input (a regular expression),
successively.  The candidates that were not successfully acted upon
are listed in buffer *Help*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (unless (or icicle-all-candidates-alternative-action-fn icicle-candidate-alternative-action-fn)
    (error "No alternative action defined."))
  (icicle-all-candidates-action-1 (or icicle-all-candidates-alternative-action-fn
                                      icicle-candidate-alternative-action-fn)
                                  icicle-all-candidates-alternative-action-fn))

(defun icicle-all-candidates-action-1 (fn-var listp)
  "Helper function for `icicle-all-candidates(-alt)-action'."
  (let ((candidates icicle-completion-candidates)
        (failures nil)
        (icicle-all-candidates-action-p t))
    (if listp
        (funcall fn-var candidates)
      (while candidates
        (let ((error-msg (condition-case act-on-each
                             (funcall fn-var (car candidates))
                           (error (error-message-string act-on-each)))))
          (when error-msg (setq failures (cons (cons (car candidates) error-msg) failures)))
          (setq candidates (cdr candidates))))
      (when failures
        (with-output-to-temp-buffer "*Help*"
          (princ "Action failures:")(terpri)(terpri)
          (mapcar (lambda (entry)
                    (princ (car entry)) (princ ":") (terpri) (princ "  ")
                    (princ (cdr entry)) (terpri))
                  failures)))))
  (icicle-abort-minibuffer-input))

;;;###autoload
(defun icicle-candidate-action ()       ; Bound to `C-RET' and `C-o' in minibuffer.
  "Take action on the current minibuffer-completion candidate.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the current candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-candidate-action-1 icicle-candidate-action-fn))

;;;###autoload
(defun icicle-candidate-alt-action ()   ; Bound to `C-S-RET' in minibuffer.
  "Take alternative action on the current completion candidate.
If `icicle-candidate-alternative-action-fn' is non-nil, it is a
function to apply to the current candidate, to perform the action.

If `icicle-candidate-alternative-action-fn' is nil, the default action
is performed: display help on the candidate - see
`icicle-help-on-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-candidate-action-1 icicle-candidate-alternative-action-fn 'alternative-p))

;;$$$ (defun icicle-candidate-action-1 (fn-var &optional altp)
;;   "Helper function for `icicle-candidate(-alt)-action'."
;;   ;; If no last candidate, then reset to first candidate matching input.
;;   (unless (stringp icicle-last-completion-candidate)
;;     (setq icicle-last-completion-candidate icicle-current-input)
;;     (setq last-command (if altp 'icicle-candidate-alt-action 'icicle-candidate-action))
;;     (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
;;                                  'icicle-prefix-candidates
;;                                'icicle-apropos-candidates)
;;                            (not (eq icicle-current-completion-mode 'prefix))))
;;   (if (not fn-var)
;;       (icicle-help-on-candidate)
;;     (funcall fn-var icicle-last-completion-candidate)
;;     (icicle-raise-Completions-frame)))

(defun icicle-candidate-action-1 (fn-var &optional altp)
  "Helper function for `icicle-candidate(-alt)-action'."
  (cond ((not fn-var) (icicle-help-on-candidate))
        (icicle-require-match-p
         (unless (stringp icicle-last-completion-candidate)
           (setq icicle-last-completion-candidate icicle-current-input)
           (setq last-command (if altp 'icicle-candidate-alt-action 'icicle-candidate-action))
           (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                        'icicle-prefix-candidates
                                      'icicle-apropos-candidates)
                                  (not (eq icicle-current-completion-mode 'prefix))))
         (if (not fn-var)
             (icicle-help-on-candidate)
           (when icicle-completion-candidates (funcall fn-var icicle-last-completion-candidate))
           (icicle-raise-Completions-frame)))
        (t
         (let ((icicle-last-input (icicle-minibuffer-contents-from-minibuffer))
               (icicle-default-directory icicle-default-directory))
           (when (and (icicle-file-name-input-p) (icicle-file-directory-p icicle-last-input))
             (setq icicle-default-directory icicle-last-input))
           (funcall fn-var icicle-last-input)
           (when (and icicle-use-candidates-only-once-flag
                      (equal icicle-last-input
                             (if (icicle-file-name-input-p)
                                 (expand-file-name icicle-last-completion-candidate
                                                   (file-name-directory icicle-last-input))
                               icicle-last-completion-candidate)))
             (icicle-remove-candidate-display-others))
           (icicle-raise-Completions-frame)))))

;;;###autoload
(defun icicle-mouse-candidate-action (event) ; Bound to `C-down-mouse-2' in *Completions*.
  "Take action on the completion candidate clicked by `mouse-2'.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the clicked candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive "e")
  (icicle-mouse-candidate-action-1 event icicle-candidate-action-fn))

;;;###autoload
(defun icicle-mouse-candidate-alt-action (event) ; Bound to `C-S-down-mouse-2' in *Completions*.
  "Take alternative action on the candidate clicked by `mouse-2'.
If `icicle-candidate-alternative-action-fn' is non-nil, it is a
function to apply to the clicked candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive "e")
  (icicle-mouse-candidate-action-1 event icicle-candidate-alternative-action-fn))

(defun icicle-mouse-candidate-action-1 (event fn-var)
  "Helper function for `icicle-mouse-candidate(-alt)-action'."
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    (read-event)                        ; Swallow mouse up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (icicle-transform-multi-completion (buffer-substring beg end))))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event)))
          icicle-last-completion-candidate choice)
    (if (not fn-var)
        (icicle-help-on-candidate)
      (funcall fn-var icicle-last-completion-candidate)
      (when icicle-use-candidates-only-once-flag (icicle-remove-candidate-display-others))
      (icicle-raise-Completions-frame posn-col posn-row))))

;;;###autoload
(defun icicle-remove-candidate ()       ; Bound to `delete' in minibuffer during completion.
  "Remove current completion candidate from the set of candidates.
This has no effect on the object, if any, represented by the
candidate; in particular, that object is not deleted.

All candidates that have the same appearance in *Completions* as the
current candidate are also removed.  Thus, for purposes of this
command, candidates are distinguished only by the strings that
represent them in *Completions*, not by any additional information
that might be associated with these strings.

Note: For Emacs versions prior to 22, this does not really remove a
file-name candidate as a possible candidate.  If you use \\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete] or \\[icicle-apropos-complete],
it will reappear as a possible candidate.

You can use this command only from the minibuffer (`\\[icicle-remove-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-remove-candidate-display-others))

;;;###autoload
(defun icicle-mouse-remove-candidate (event) ; Bound to `S-mouse-2' in *Completions*.
  "Remove clicked completion candidate from the set of candidates.
This has no effect on the object, if any, represented by the
candidate; in particular, that object is not deleted.

See `icicle-remove-candidate' for more information."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    (read-event)                        ; Swallow mouse up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (icicle-transform-multi-completion
                        (buffer-substring-no-properties beg end)))))))
  (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event))))
  (icicle-remove-candidate-display-others))

(defun icicle-remove-candidate-display-others ()
  "Remove current completion candidate from list of possible candidates.
Redisplay *Completions*, unless there is only one candidate left.
Note: This actually removes all candidates that look the same."
  (setq icicle-last-completion-candidate
        (elt icicle-completion-candidates (or icicle-candidate-nb (setq icicle-candidate-nb 0))))
  (setq icicle-completion-candidates    ; Delete candidate from `icicle-completion-candidates'.
        (delete icicle-last-completion-candidate icicle-completion-candidates))
  ;;$$  (setq minibuffer-completion-table ; This would work too, but only for an alist value.
  ;;      (icicle-assoc-delete-all icicle-last-completion-candidate minibuffer-completion-table)))

  ;; Update predicate to effectively remove this candidate from those possible.
  (cond ((and (icicle-file-name-input-p)
              (boundp 'read-file-name-predicate) read-file-name-predicate) ; Emacs 22+ only.
         (setq read-file-name-predicate
               `(lambda (fname)
                 (and (if ',read-file-name-predicate (funcall ',read-file-name-predicate fname) t)
                  (not (string= ,icicle-last-completion-candidate fname))))))
        ;; Do nothing for file name if < Emacs 22. `TAB' or `S-TAB' will bring it back as a candidate.
        ((not (icicle-file-name-input-p))
         (setq minibuffer-completion-predicate
               `(lambda (cand)
                 (and (if ',minibuffer-completion-predicate
                          (funcall ',minibuffer-completion-predicate cand)
                        t)
                  (not (string=         ; We don't treat hash table `minibuffer-completion-table' yet.
                        ,icicle-last-completion-candidate
                        (cond ((symbolp cand) (symbol-name cand))
                              ((consp cand) (car cand))
                              (t cand))))))))) ; e.g. just a string.
  (when icicle-candidates-alist         ; Delete candidate from `icicle-candidates-alist'.
    (setq icicle-candidates-alist (icicle-filter-alist icicle-candidates-alist
                                                       icicle-completion-candidates)))
  (cond ((and icicle-completion-candidates (cdr icicle-completion-candidates)) ; > 1 left.
         (icicle-maybe-sort-and-strip-candidates)
         (message "Displaying completion candidates...")
         (with-output-to-temp-buffer "*Completions*" (icicle-display-candidates-in-Completions))
         (with-current-buffer "*Completions*"
           (goto-char (icicle-start-of-candidates-in-Completions))
           (icicle-move-to-next-completion
            (mod icicle-candidate-nb (length icicle-completion-candidates)))
           (set-window-point (get-buffer-window "*Completions*" 0) (point))
           (setq icicle-last-completion-candidate (icicle-current-completion-in-Completions))
           (set-buffer-modified-p nil)))
        (icicle-completion-candidates   ; Single candidate left
         (save-selected-window (icicle-remove-Completions-window))
         (let ((completion (icicle-transform-multi-completion
                            (car icicle-completion-candidates))))
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if *Completions* redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)
             (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                         (icicle-file-name-directory-w-default icicle-current-input)
                       "")
                     completion))))
        (t                              ; No candidates left
         (select-window (active-minibuffer-window))
         (with-current-buffer (window-buffer) ; Needed if *Completions* redirected to minibuffer.
           (goto-char (icicle-minibuffer-prompt-end))
           (icicle-clear-minibuffer)))))

(put 'icicle-delete-candidate-object 'icicle-cycling-command t)
(put 'icicle-delete-candidate-object 'icicle-prefix-cycling-command t)
(put 'icicle-delete-candidate-object 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-delete-candidate-object () ; Bound to `S-delete' in minibuffer.
  "Delete the object named by the current completion candidate.
Do nothing if `icicle-deletion-action-flag' is nil.

Otherwise:

* If the value of variable `icicle-delete-candidate-object' is a
  function, then apply it to the current completion candidate.  This
  should delete some object named by the completion candidate.

* If `icicle-delete-candidate-object' is not a function, then it
  should be a symbol bound to an alist.  In this case, invoke
  `icicle-delete-candidate-object' to delete the object named by the
  current completion candidate from that alist.

Note: For convenience in cycling, any other candidate objects that
have the same name as the deleted object are removed from
*Completions*, as if you had hit `delete' (`icicle-remove-candidate'),
even though they are not deleted.  To show any remaining objects with
this name, use `TAB', `S-TAB', or `M-k' to refresh the *Completions*
display.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-delete-candidate-object]')."
  (interactive)
  (when icicle-deletion-action-flag
    (when (interactive-p) (icicle-barf-if-outside-minibuffer))
    ;; If no last candidate, then reset to first candidate matching input.
    (unless (stringp icicle-last-completion-candidate)
      (setq icicle-last-completion-candidate icicle-current-input)
      (setq last-command 'icicle-delete-candidate-object)
      (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                   'icicle-prefix-candidates
                                 'icicle-apropos-candidates)
                             (not (eq icicle-current-completion-mode 'prefix))))
    (let ((cand-to-delete icicle-last-completion-candidate)) ; Use local var: value might change.
      (save-selected-window
        (if (functionp icicle-delete-candidate-object)
            (funcall icicle-delete-candidate-object icicle-last-completion-candidate)
          (icicle-delete-current-candidate-object)))
      (icicle-remove-candidate-display-others)
      (message "Deleted object named: `%s'" cand-to-delete) (sleep-for 1.5))
    (select-frame-set-input-focus (window-frame (minibuffer-window)))))

(defun icicle-delete-current-candidate-object ()
  "Delete the object(s) corresponding to the current completion candidate.
The value of `icicle-delete-candidate-object' must be a symbol
\(variable) that is bound to a list of completion-candidate objects.

The entries in the list must be completion candidates for the current
call to `completing-read', but the list itself need not be the TABLE
argument to `completing-read'.  For example, the list might be a list
of symbols, and the TABLE argument might be an obarray that contains
those symbols.

The list can be an alist, a list of strings, or a list of symbols.
Delete, from this list, the object that corresponds to the current
completion candidate.  If the variable is also a user option, then
save the option, after deleting the candidate object.

If `icicle-candidates-alist' is nil and the variable value is an
alist, then delete all entries that match the current candidate.

If `icicle-candidates-alist' is non-nil, then it means that the
current command allows multiple candidates with the same name, their
order in *Completions* is significant, and users cannot change that
order.  This order is used by `icicle-get-alist-candidate' to
determine which candidate object to delete.  Cycling or clicking
`mouse-2' specifies a unique candidate, but if completion is used
instead, and the completion matches more than one candidate, then an
error is raised.

Note that the full candidate object is what is deleted.  Therefore, do
not use this with multi-completions, unless the alist itself has
corresponding multi-completion entries."
  (let ((val (and (symbolp icicle-delete-candidate-object)
                  (symbol-value icicle-delete-candidate-object))))
    ;; The message could instead say "Value of `icicle-delete-candidate-object' must be a symbol
    ;; bound to a list", but this makes more sense.
    (unless (and val (consp val)) (error "Cannot delete candidate objects now"))
    (set icicle-delete-candidate-object ; Update the variable.
         (cond ((and icicle-candidates-alist (consp (car val)))
                (delete (icicle-get-alist-candidate icicle-last-completion-candidate) val))
               ((consp (car val))
                (icicle-assoc-delete-all icicle-last-completion-candidate val))
               ((stringp (car val)) (delete icicle-last-completion-candidate val))
               ((symbolp (car val)) (delete (intern icicle-last-completion-candidate) val))
               (t (error "Entry in list value of `icicle-delete-candidate-object' is \
not a cons, string, or symbol")))))
  (when (user-variable-p icicle-delete-candidate-object) ; Save the new user-option value.
    (customize-save-variable icicle-delete-candidate-object
                             (symbol-value icicle-delete-candidate-object))))

;;;###autoload
(defun icicle-mouse-help-on-candidate (event) ; Bound to `C-M-mouse-2' in minibuffer.
  "Display help on the minibuffer-completion candidate clicked by mouse."
  (interactive "e")
  (let ((icicle-candidate-action-fn nil)) (icicle-mouse-candidate-action event)))

;;;###autoload
(defun icicle-help-on-candidate ()      ; Bound to `C-M-RET', `C-help', and `C-f1' in minibuffer.
                                        ; Bound to `C-M-RET' in *Completions.
  "Display help on the current minibuffer-completion candidate.
The help displayed depends on the type of candidate, as follows:

 menu item - the corresponding command is described using
             `describe-function' (available only if `icicles-menu.el'
             is loaded)
 command or other function - described using `describe-function'
 user option or other variable - described using `describe-variable'
 face - described using `describe-face'
 property list - described using `apropos-describe-plist'
 buffer name - modes described using `describe-mode' (Emacs > 20)
 file name - file properties described

In the minibuffer, you can also use `\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-apropos-candidate]', `\\[icicle-help-on-previous-apropos-candidate]',
`\\[icicle-help-on-next-prefix-candidate]', and \
`\\[icicle-help-on-previous-prefix-candidate]', to display help on the candidate and then
move to the next or previous candidate.  See, for example,
`icicle-help-on-next-apropos-candidate'.
\
You can use this command only from the minibuffer or *Completions*
\(`\\[icicle-help-on-candidate]')."
  (interactive)                         ; Interactively, just describes itself.
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let ((frame-with-focus (selected-frame))
        cand-symb)
    (if (eq (current-buffer) (get-buffer "*Completions*"))
        (setq cand-symb (intern-soft (icicle-transform-multi-completion
                                      (icicle-current-completion-in-Completions))))

      ;; If no last candidate, then reset to first candidate matching input.
      (unless (stringp icicle-last-completion-candidate)
        (setq icicle-last-completion-candidate icicle-current-input)
        (setq last-command 'icicle-help-on-candidate)
        (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                     'icicle-prefix-candidates
                                   'icicle-apropos-candidates)
                               (not (eq icicle-current-completion-mode 'prefix))))
      (setq cand-symb (intern-soft (icicle-transform-multi-completion
                                    icicle-last-completion-candidate))))

    ;; If this is a call to `icicle-execute-menu-command' (defined in `icicles-menu.el'), then 
    ;; use command associated with a menu item.  `icicle-menu-items-alist' is set in
    ;; `icicles-menu.el'.  If non-nil, then `icicle-execute-menu-command' is being called.
    (when (consp icicle-menu-items-alist)
      (setq cand-symb (cdr (assoc icicle-last-completion-candidate icicle-menu-items-alist)))
      (unless (symbolp cand-symb) (setq cand-symb nil))) ; Menu item with lambda definition.

    ;; If this is a key-completion candidate, then get the true command from the candidate.
    (when (boundp 'icicle-completing-keys-p)
      (string-match "\\(.+\\)  =  \\(.+\\)" icicle-last-completion-candidate)
      (setq cand-symb (intern-soft (substring icicle-last-completion-candidate
                                              (match-beginning 2) (match-end 2)))))

    ;; Provide the help appropriate for the given type of candidate.
    (if cand-symb
        (icicle-help-on-candidate-symbol cand-symb)
      ;; Describe buffer's mode or a file's properties.  Otherwise, create a symbol and try again.
      (cond ((and (bufferp (get-buffer icicle-last-completion-candidate))
                  (with-current-buffer (get-buffer icicle-last-completion-candidate)
                    (describe-mode)
                    t)))
            ((file-exists-p icicle-last-completion-candidate)
             (icicle-describe-file icicle-last-completion-candidate))
            (t
             (setq cand-symb (intern icicle-last-completion-candidate)) ; Hard intern.
             (icicle-help-on-candidate-symbol cand-symb))))
    (icicle-raise-Completions-frame)
    ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
    ;; because the *Help* frame takes the focus away from the minibuffer frame.
    ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
    (let* ((help-window (get-buffer-window "*Help*" 0))
           (help-frame (and help-window (window-frame help-window))))
      (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
  (message nil))                        ; Let minibuffer contents show immmediately.

(defun icicle-help-on-candidate-symbol (symb)
  "Helper function for `icicle-help-on-candidate'.  The arg is a symbol."
  (cond (icicle-candidate-help-fn (funcall icicle-candidate-help-fn (symbol-name symb)))
        ((functionp symb) (describe-function symb))
        ((boundp symb) (describe-variable symb))
        ((facep symb) (describe-face symb))
        ((symbol-plist symb) (apropos-describe-plist symb))
        (t
         (setq symb (symbol-name symb)) ; Convert symbol to string, and try some more.
         (cond ((and (bufferp (get-buffer symb))
                     (with-current-buffer (get-buffer symb) (describe-mode) t)))
               ((file-exists-p symb) (icicle-describe-file symb))
               (t (icicle-msg-maybe-in-minibuffer "No help"))))))

;; This is the same as `describe-file' in `misc-cmds.el', but we avoid requiring that library.
;;;###autoload
(if (and (not (fboundp 'icicle-describe-file)) (fboundp 'describe-file))
    (fset 'icicle-describe-file (symbol-function 'describe-file))
  (defun icicle-describe-file (filename) ; Suggestion: bind to `C-h M-f'.
    "Describe the file named FILENAME."
    (interactive "FDescribe file: ")
    (help-setup-xref (list #'icicle-describe-file filename) (interactive-p))
    (let ((attrs (file-attributes filename)))
      (if (null attrs)
          (icicle-msg-maybe-in-minibuffer (format "Cannot open file `%s'" filename))
        (let ((type            (nth 0 attrs))
              (numlinks        (nth 1 attrs))
              (uid             (nth 2 attrs))
              (gid             (nth 3 attrs))
              (last-access     (nth 4 attrs))
              (last-mod        (nth 5 attrs))
              (last-status-chg (nth 6 attrs))
              (size            (nth 7 attrs))
              (permissions     (nth 8 attrs))
              ;; Skip 9: t iff file's gid would change if file were deleted and recreated.
              (inode           (nth 10 attrs))
              (device          (nth 11 attrs)))
          (save-excursion
            (with-output-to-temp-buffer "*Help*"
              (princ (format "Properties of `%s':\n\n" filename))
              (princ (format "Type:                       %s\n"
                             (cond ((eq t type) "Directory")
                                   ((stringp type) (format "Symbolic link to `%s'" type))
                                   (t "Normal file"))))
              (princ (format "Permissions:                %s\n" permissions))
              (unless (eq t type) (princ (format "Size in bytes:              %g\n" size)))
              (princ (format "Time of last access:        %s\n" last-access))
              (princ (format "Time of last modification:  %s\n" last-mod))
              (princ (format "Time of last status change: %s\n" last-status-chg))
              (princ (format "Number of links:            %d\n" numlinks))
              (princ (format "User ID (UID):              %s\n" uid))
              (princ (format "Group ID (GID):             %s\n" gid))
              (princ (format "Inode:                      %s\n" inode))
              (princ (format "Device number:              %s\n" device))
              (princ "\n\n")
              (print-help-return-message)
              (with-current-buffer standard-output (buffer-string))))))))) ; Return displayed text.

;;;###autoload
(defun icicle-candidate-read-fn-invoke () ; Bound to `M-RET' in minibuffer.
  "Read function name.  Invoke function on current completion candidate.
Set `icicle-candidate-action-fn' to the interned name.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-read-fn-invoke]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  ;; If no last candidate, then reset to first candidate matching input.
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate icicle-current-input)
    (setq last-command 'icicle-candidate-action)
    (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                 'icicle-prefix-candidates
                               'icicle-apropos-candidates)
                           (not (eq icicle-current-completion-mode 'prefix))))
  (let ((enable-recursive-minibuffers t)
        (icicle-saved-completion-candidate icicle-last-completion-candidate)
        (icicle-candidate-action-fn 'icicle-apply-to-saved-candidate))
    (icicle-apply-to-saved-candidate
     (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                      obarray 'functionp))))

;;;###autoload
(defun icicle-mouse-candidate-read-fn-invoke (event) ; Bound to `M-mouse-2' in *Completions*.
  "Read function name.  Invoke function on candidate clicked by mouse."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    ;; (read-event)                 ; Swallow mouse up event. $$ Not needed if bound to up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event)))
          icicle-last-completion-candidate choice)
    (let ((enable-recursive-minibuffers t)
          (icicle-saved-completion-candidate icicle-last-completion-candidate)
          (icicle-candidate-action-fn 'icicle-apply-to-saved-candidate))
      (icicle-apply-to-saved-candidate
       (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                        obarray 'functionp)))))

(defun icicle-apply-to-saved-candidate (function &optional use-icicle-candidates-alist-p)
  "Apply FUNCTION to `icicle-saved-completion-candidate'.
If `current-prefix-arg' is non-nil, then pretty-print the result using
`pp-eval-expression'."
  (let ((real-fn (if use-icicle-candidates-alist-p
                     (cdr (icicle-get-alist-candidate function))
                   (car (read-from-string function)))))
    ;; Actually, we should test more than `functionp', to rule out macros and special forms.
    (unless (functionp real-fn) (error "Not a function: `%S'" real-fn))
    (condition-case icicle-candidate-read-fn-invoke
        (if current-prefix-arg
            (pp-eval-expression '(funcall real-fn icicle-saved-completion-candidate))
          (funcall real-fn icicle-saved-completion-candidate))
      (error (message  (format "ERROR invoking `%S' on `%s': %s" real-fn
                               icicle-saved-completion-candidate
                               (error-message-string icicle-candidate-read-fn-invoke)))
             (sleep-for 6)))
    (select-frame-set-input-focus (window-frame (minibuffer-window)))
    (icicle-raise-Completions-frame)))

(defun icicle-raise-Completions-frame (&optional mouse-col mouse-row)
  "Raise *Completions* frame, if displayed.
This helps keep *Completions* on top.

If `icicle-Completions-frame-at-right-flag' is non-nil and
*Completions* is in its own frame, then move that frame to the right,
out of the way.

Non-nil optional args MOUSE-COL and MOUSE-ROW move the mouse pointer
to column MOUSE-COL and row MOUSE-ROW.  Do this because
`icicle-candidate-action-fn' can call `select-frame-set-input-focus',
which can position mouse pointer on a standalone minibuffer frame."
  ;; Raise *Completions* frame, if displayed.  This helps keep *Completions* on top.
  (let ((compl-win (get-buffer-window "*Completions*" 'visible)))
    (when compl-win
      (save-window-excursion
        (select-window compl-win)
        ;; Move frame to the right, out of the way.
        (when (and (one-window-p t) icicle-Completions-frame-at-right-flag)
          (modify-frame-parameters
           (selected-frame)             ; Hard-code 7 here - what does it depend on?
           `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7))))))
        (raise-frame)
        (when (and (integerp mouse-col) (integerp mouse-row))
          (set-mouse-position (selected-frame) mouse-col mouse-row))))))

;;;###autoload
(defun icicle-Completions-mouse-3-menu (event) ; Bound to `C-mouse-3' in *Completions.
  "Pop-up menu on `C-mouse-3' for the current candidate in *Completions*."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        candidate base-size menu-choice)
    ;; (read-event)                 ; Swallow mouse up event. $$ Not needed if bound to up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg       (previous-single-property-change beg 'mouse-face)
                end       (or (next-single-property-change end 'mouse-face)(point-max))
                candidate (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event)))
          icicle-last-completion-candidate candidate)
    (setq menu-choice
          (x-popup-menu
           event
           (list
            "Completion Candidate"
            (list
             "$$ NOT USED $$"
             '("Help About  (`C-M-RET')" . icicle-help-on-candidate)
             '("Act On  (`C-RET', `C-mouse-2')" . icicle-candidate-action)
             '("Apply a Function To...  (`M-RET', `M-mouse-2')" . icicle-candidate-read-fn-invoke)
             '("Insert in Minibuffer  (`insert')" .
               (lambda ()
                 (interactive)
                 (select-window (active-minibuffer-window))
                 (goto-char (icicle-minibuffer-prompt-end))
                 (icicle-clear-minibuffer)
                 (insert icicle-last-completion-candidate)))
             '("--")
             '("--")
             '("Change Sort Order  (`C-,')" . icicle-change-sort-order)
             '("Change Alternative Sort Order  (`M-,')" . icicle-change-alternative-sort-order)
             '("Swap Alternative Sort  (`C-M-,')" . icicle-toggle-alternative-sorting)
             '("--")
             '("Save All  (`C-M->')" . icicle-candidate-set-save)
             '("             to Variable...  (`C-M-})' " . icicle-candidate-set-save-to-variable)
             '("             to Cache File...  (`C-})" . icicle-candidate-set-save-to-cache-file)
             '("Add Candidates in Current Set  (`C->')" . icicle-candidate-set-save-more)
             '("Save Selected (Region) Candidates  (`C-M-)')" . icicle-candidate-set-save-selected)
             '("Add Selected (Region) Candidates  (`C-)')" . icicle-candidate-set-save-more-selected)
             '("Retrieve Saved  (`C-M-<')" . icicle-candidate-set-retrieve)
             '("--")
             '("Complement All  (`C-~')" . icicle-candidate-set-complement)
             '("Match Also Regexp...  (`M-*')" . icicle-narrow-candidates)
             '("Satisfy Also Predicate...  (`M-&')" . icicle-narrow-candidates-with-predicate)
             '("Save Predicate to Variable...  (`C-M-&')" . icicle-save-predicate-to-variable)
             '("Intersect Saved  (`C-*')" . icicle-candidate-set-intersection)
             '("Subtract Saved  (`C--')" . icicle-candidate-set-difference)
             '("Add (Union) Saved  (`C-+')" . icicle-candidate-set-union)
             '("Only Previously Entered  (`M-pause')" . icicle-keep-only-past-inputs)
             '("--")
             '("Act On All - Careful!  (`C-!')" . icicle-all-candidates-action)
             '("--")
             '("Toggle Duplicate Removal  (`C-$')" . icicle-toggle-transforming)
             '("Toggle Case Sensitivity  (`C-A')" . icicle-toggle-case-sensitivity)
             '("Toggle Highlighting Past Inputs  (`C-pause')" .
               icicle-toggle-highlight-historical-candidates)
             '("Toggle Angle Brackets  (`C-<')" . icicle-toggle-angle-brackets)
             '("Toggle Ignored File Extensions  (`C-.')" . icicle-toggle-ignored-extensions)
             '("Toggle Ignoring Space Prefix  (`C-^')" . icicle-toggle-ignored-space-prefix)
             '("Toggle Incremental Completion  (`C-#')" . icicle-toggle-incremental-completion)
             '("Toggle Fuzzy Prefix Completion  (`C-(')" . icicle-toggle-fuzzy-completion)
             '("Toggle Escaping Special Regexp Chars  (`C-`')" . icicle-toggle-regexp-quote)
             '("Toggle Removal of Search Highlighting  (`C-.')" . icicle-toggle-search-cleanup)
             '("Toggle All-Current Search Highlighting  (`C-^')" .
               icicle-toggle-highlight-all-current)
             '("Toggle Using `~' For $HOME  (`M-~')" . icicle-toggle-~-for-home-dir)
             '("+ Toggle Any Option..." . icicle-toggle-option)
             '("+ Turn Off Option..." . icicle-reset-option-to-nil)
             '("+ Turn On Option..." . icicle-set-option-to-t)
             '("--")
             '("Restore Previous Completion Input  (`C-l')" . icicle-retrieve-previous-input)
             '("Restore Next Completion Input  (`C-L')" . icicle-retrieve-next-input)
             '("Scroll  (repeated `TAB' or `S-TAB')" . icicle-scroll-Completions)
             '("One-Off Eval...  (`M-:')" . icicle-pp-eval-expression)
             '("Insert `icicle-input-string'  (`C-=')" . icicle-insert-string-from-variable)
             '("--")
             '("Icicles Help  (`C-?')" . icicle-completion-help)))))
    (and menu-choice (call-interactively menu-choice))))

;;;###autoload
(defun icicle-narrow-candidates ()      ; Bound to `M-*' in minibuffer.
  "Narrow the set of completion candidates using another input regexp.
This, in effect, performs a set intersection operation on 1) the set
of candidates in effect before the operation and 2) the set of
candidates that match the current input.  You can repeatedly use this
command to continue intersecting candidate sets, progressively
narrowing the set of matches.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-narrow-candidates]')."
  ;; We handle `no-catch' errors here because `old-completing-read' and
  ;; `old-read-file-file-name' can still be called in Icicle mode by, for instance, an
  ;; `interactive' spec (e.g. (interactive "bBuffer: ")).  In that case, we throw to a
  ;; non-existant catch.  After doing that, we just insert the result, to pass it to the
  ;; next-higher recursive minibuffer.
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((enable-recursive-minibuffers t)
        (icicle-reminder-prompt-flag nil)) ; Inhibit reminder.
    (cond ((null icicle-completion-candidates)
           (error
            (substitute-command-keys
             "No completion candidates.  Did you use `\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]' or `\\[icicle-apropos-complete]'?")))
          ((null (cdr icicle-completion-candidates))
           (if (not icicle-top-level-when-sole-completion-flag)
               (minibuffer-message "  [Sole completion]")
             (condition-case i-narrow-candidates
                 (throw 'icicle-read-top (car icicle-completion-candidates))
               (no-catch (setq icicle-current-input (car icicle-completion-candidates))
                         (icicle-retrieve-last-input)
                         icicle-current-input)
               (error (message (error-message-string i-narrow-candidates))))))
          (t
           (let* ((current-candidates icicle-completion-candidates)
                  (result
                   (if (and (> emacs-major-version 21) (icicle-file-name-input-p))
                       (read-file-name "Match also (regexp): "
                                       (icicle-file-name-directory-w-default icicle-current-input)
                                       nil icicle-require-match-p nil
                                       (lambda (fname) (member fname current-candidates)))
                     ;; In Emacs < 22, there is no PREDICATE arg to `read-file-name', so
                     ;; we use `completing-read' even for file-name completion.  In that case, we
                     ;; tack the `default-directory' onto each candidate.                     
                     (completing-read
                      "Match also (regexp): "
                      (cond ((icicle-file-name-input-p)
                             (let ((dir (icicle-file-name-directory-w-default icicle-current-input)))
                               (mapcar (lambda (file) (list (concat dir file)))
                                       icicle-completion-candidates)))
                            (icicle-whole-candidate-as-text-prop-p
                             (mapcar
                              (lambda (cand) (icicle-get-alist-candidate (car cand)))
                              (icicle-filter-alist minibuffer-completion-table
                                                       icicle-completion-candidates)))
                            (t
                             (mapcar #'list icicle-completion-candidates)))
                      nil icicle-require-match-p nil
                      minibuffer-history-variable))))
             ;; Normally, `icicle-narrow-candidates' is called from the minibuffer.
             ;; If not, just return the result read.
             (if (> (minibuffer-depth) 0)
                 (condition-case i-narrow-candidates
                     (throw 'icicle-read-top result)
                   (no-catch (setq icicle-current-input result)
                             (icicle-retrieve-last-input)
                             icicle-current-input)
                   (error (message (error-message-string i-narrow-candidates))))
               result))))))

;;;###autoload
(defun icicle-apropos-complete-and-narrow () ; Bound to `S-SPC' in minibuffer.
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((icicle-top-level-when-sole-completion-flag t))
    (icicle-apropos-complete)
    (icicle-narrow-candidates)))

;;;###autoload
(defun icicle-narrow-candidates-with-predicate () ; Bound to `M-&' in minibuffer.
  "Narrow the set of completion candidates by applying a predicate.
You can repeatedly use this command to apply additional predicates,
progressively narrowing the set of candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-narrow-candidates-with-predicate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((enable-recursive-minibuffers t)
        (icicle-reminder-prompt-flag nil) ; Inhibit reminder.
        (last-completion-cmd (or icicle-last-completion-command 'icicle-apropos-complete)))
    (cond ((null icicle-completion-candidates)
           (error
            (substitute-command-keys
             "No completion candidates.  Did you use `\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]' or `\\[icicle-apropos-complete]'?")))
          ((null (cdr icicle-completion-candidates))
           (if (not icicle-top-level-when-sole-completion-flag)
               (minibuffer-message "  [Sole completion]")
             (condition-case i-narrow-candidates
                 (throw 'icicle-read-top (car icicle-completion-candidates))
               (no-catch (setq icicle-current-input (car icicle-completion-candidates))
                         (icicle-retrieve-last-input)
                         icicle-current-input)
               (error (message (error-message-string i-narrow-candidates))))))
          (t
           ;; Read new predicate to apply.
           (let ((pred (icicle-read-from-minibuf-nil-default "Additional predicate to apply: "
                                                             nil read-expression-map t
                                                             (if (boundp 'function-name-history)
                                                                 'function-name-history
                                                               'icicle-function-name-history))))
             ;; Update predicate to effectively remove this candidate from those possible.
             (cond ((and (icicle-file-name-input-p) ; Emacs 22+ only.
                         (boundp 'read-file-name-predicate)
                         read-file-name-predicate)
                    (setq read-file-name-predicate
                          `(lambda (fname)
                            (and (if ',read-file-name-predicate
                                     (funcall ',read-file-name-predicate fname)
                                   t)
                             (funcall ',pred fname)))))
                   ;; Do nothing for file name if < Emacs 22.
                   ;; `TAB' or `S-TAB' will bring it back as a candidate.
                   ((not (icicle-file-name-input-p))
                    (setq minibuffer-completion-predicate
                          `(lambda (cand)
                            (and (if ',minibuffer-completion-predicate
                                     (funcall ',minibuffer-completion-predicate cand)
                                   t)
                             (funcall ',pred cand)))))))))
    (funcall last-completion-cmd)))

;;;###autoload
(defun icicle-save-predicate-to-variable (askp) ; Bound to `C-M-&' in minibuffer.
  "Save the current completion predicate to a variable.
By default, the variable is `icicle-input-string'.  If you use a
prefix argument, then you are prompted for the variable to use.

You can retrieve the saved predicate as a string using `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-save-predicate-to-variable]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let* ((pred minibuffer-completion-predicate)
         (enable-recursive-minibuffers t)
         (var (if askp
                  (intern (completing-read "Save candidates in variable: " obarray 'boundp
                                           nil nil
                                           (if (boundp 'variable-name-history)
                                               'variable-name-history
                                             'icicle-variable-name-history)))
                'icicle-input-string)))
    (set var (prin1-to-string pred))
    (save-selected-window (select-window (minibuffer-window))
                          (minibuffer-message (format "  [Predicate SAVED to `%s']" var)))))

;;;###autoload
(defun icicle-candidate-set-swap ()     ; Bound to `C-%' in minibuffer.
  "Swap the saved set and current sets of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-swap]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (setq icicle-saved-completion-candidates
        (prog1 icicle-completion-candidates
          (setq icicle-completion-candidates icicle-saved-completion-candidates)))
  (minibuffer-message "  [Saved set of candidates SWAPPED with current]"))

;;;###autoload
(defun icicle-candidate-set-define ()   ; Bound to `C-:' in minibuffer.
  "Define the set of current completion candidates by evaluating a sexp.
The Lisp sexp must evaluate to a list of strings, such as is returned
by `all-completions'.

You can use this command at top level or from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-define]')."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (evald-sexp (eval-minibuffer "Set the completion candidates to sexp (eval): ")))
    (if (or (null evald-sexp) (and (consp evald-sexp) (stringp (car evald-sexp))))
        (setq icicle-completion-candidates evald-sexp)
      (error "Sexp did not evaluate to a list of strings: %S" evald-sexp)))
  (icicle-maybe-sort-and-strip-candidates)
  (message "List of completion candidates DEFINED: %S" icicle-completion-candidates)
  (when (> (minibuffer-depth) 0)
    (message "Displaying completion candidates...")
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list icicle-completion-candidates))
    (icicle-narrow-candidates)))

;;;###autoload
(defun icicle-candidate-set-difference () ; Bound to `C--' in minibuffer.
  "Take the set difference between the current and saved candidates.
The new set of candidates is the set of candidates prior to executing
this command minus the saved set of candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-difference]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-1 'icicle-set-difference "  [saved set of candidates SUBTRACTED]"))

;;;###autoload
(defun icicle-candidate-set-union ()    ; Bound to `C-+' in minibuffer.
  "Take the set union between the current and saved candidates.
The new set of candidates is the union of the saved set of candidates
and the set of candidates prior to executing this command.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-union]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-1 'icicle-set-union "  [saved set of candidates ADDED]"))

;;;###autoload
(defun icicle-candidate-set-intersection () ; Bound to `C-*' in minibuffer.
  "Take the set intersection between the current and saved candidates.
The new set of candidates is the intersection of the saved set of
candidates and the set of candidates prior to executing this command.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-intersection]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-1 'icicle-set-intersection
                          "  [INTERSECTION of saved and current sets of candidates]"))

;;;###autoload
(defun icicle-candidate-set-complement () ; Bound to `C-~' in minibuffer.
  "Complement the set of current completion candidates.
The new set of candidates is the set of `all-completions' minus the
set of candidates prior to executing this command - that is, all
possible completions of the appropriate type, except for those that
are in the current set of completions.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-complement]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (setq icicle-completion-candidates
        (icicle-set-difference
         (all-completions "" minibuffer-completion-table minibuffer-completion-predicate
                          icicle-ignore-space-prefix-flag)
         icicle-completion-candidates))
  (icicle-maybe-sort-and-strip-candidates)
  (message "Displaying completion candidates...")
  (with-output-to-temp-buffer "*Completions*" (display-completion-list icicle-completion-candidates))
  (minibuffer-message "  [Set of candidates COMPLEMENTED]")
  (icicle-narrow-candidates))

(defun icicle-candidate-set-truncate (n) ; Bound to `M-$' in minibuffer.
  "Trim the set of current completion candidates at the end.
The first N candidates are kept.  N is read."
  ;; Ugly hack: `icicle-saved-completion-candidates-internal'.  No way to bind a variable
  ;; in `interactive' and have the binding be active in the function body.
  (interactive
   (list (let ((enable-recursive-minibuffers t))
           (setq icicle-saved-completion-candidates-internal icicle-completion-candidates)
           (if current-prefix-arg
               (prefix-numeric-value current-prefix-arg)
             (read-number "Number of candidates to keep: ")))))
  (setq icicle-completion-candidates icicle-saved-completion-candidates-internal)
  (setcdr (nthcdr (1- n) icicle-completion-candidates) nil)
  (icicle-maybe-sort-and-strip-candidates)
  (message "Displaying completion candidates...")
  (with-output-to-temp-buffer "*Completions*" (display-completion-list icicle-completion-candidates))
  (message (format "  [Set of candidates TRUNCATED to %d]" n))
  (icicle-narrow-candidates))
      
(defun icicle-retrieve-candidates-from-set (set-name)
  "Retrieve the saved set of completion candidates named SET-NAME.
The candidates are retrieved to `icicle-saved-completion-candidates'.
Return the name of the cache file for set SET-NAME."
  (let ((file-name (cdr (assoc set-name icicle-saved-completion-sets))))
    (unless file-name (error "Set `%s' not found in `icicle-saved-completion-sets'.  \
Use `icicle-add/update-saved-completion-set'" set-name))
    (unless (icicle-file-readable-p file-name) (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf (find-file-noselect file-name 'nowarn 'raw))
          (candidates nil))
      (message "Retrieving saved candidates from `%s'..." file-name)
      (unwind-protect
           (when (listp (setq candidates (read list-buf)))
             (message "Set `%s' read from file `%s'" set-name file-name))
        (kill-buffer list-buf))
      (unless candidates (error "No completion candidates in file `%s'" file-name))
      (setq icicle-saved-completion-candidates candidates))
    file-name))                         ; Return cache-file name.

;;;###autoload
(defun icicle-candidate-set-retrieve (&optional arg) ; Bound to `C-M-<' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-save-candidate]', `M-S-mouse-2', 
`\\<minibuffer-local-completion-map>\\[icicle-candidate-set-save]', \
`\\[icicle-candidate-set-save-to-variable]', or `\\[icicle-candidate-set-save-to-cache-file]'.
With no prefix arg, retrieve candidates from
 `icicle-saved-completion-candidates'.
With a plain prefix arg `C-u', retrieve candidates from a cache file.
With a numeric prefix arg N, retrieve candidates from a variable.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let ((name nil)
        (variablep (and arg (atom arg)))
        saved-cands)
    (if arg
        (let ((enable-recursive-minibuffers t))
          (if variablep
              (setq saved-cands
                    (symbol-value (setq name (intern
                                              (completing-read ; Variable name.
                                               "Retrieve candidates from variable: "
                                               icicle-saved-candidates-variables-obarray
                                               nil nil nil (if (boundp 'variable-name-history)
                                                               'variable-name-history
                                                             'icicle-variable-name-history))))))
            (let ((set-name (completing-read "Retrieve completion candidates from set: "
                                             icicle-saved-completion-sets nil nil nil
                                             'icicle-completion-set-history
                                             (caar icicle-saved-completion-sets))))
              (setq name (icicle-retrieve-candidates-from-set set-name))) ; File name.
            (setq saved-cands icicle-saved-completion-candidates)))
      (setq saved-cands icicle-saved-completion-candidates))
    (cond ((null saved-cands)
           (deactivate-mark)
           (icicle-display-candidates-in-Completions)
           (message "No saved candidates to restore") (sit-for 2))
          (t
           (setq icicle-completion-candidates saved-cands)
           (cond ((and (consp icicle-completion-candidates) (null (cdr icicle-completion-candidates)))
                  (icicle-remove-Completions-window)
                  (icicle-insert-completion (car icicle-completion-candidates)) ; Insert sole cand.
                  (minibuffer-message "  [Sole candidate restored]")
                  (save-selected-window (select-window (minibuffer-window))
                                        (icicle-highlight-complete-input)))
                 ((consp icicle-completion-candidates)
                  (deactivate-mark)
                  (icicle-display-candidates-in-Completions)
                  (save-selected-window
                    (select-window (minibuffer-window))
                    (minibuffer-message (if name
                                            (format "  [Saved candidates RESTORED from %s `%s']"
                                                    (if variablep "variable" "cache file") name)
                                          "  [Saved candidates RESTORED]")))
                  (icicle-narrow-candidates)))))))

;;;###autoload
(defun icicle-candidate-set-retrieve-from-variable () ; Bound to `C-M-{' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-save-candidate]', `M-S-mouse-2', or
`\\[icicle-candidate-set-save-to-variable]' (or `\\[icicle-candidate-set-save]' with a numeric \
prefix arg).

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-variable]')."
  (interactive)
  (icicle-candidate-set-retrieve 99))

;;;###autoload
(defun icicle-candidate-set-retrieve-from-cache-file () ; Bound to `C-{' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-cache-file]' or `C-u \\[icicle-candidate-set-save]'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-cache-file]')."
  (interactive)
  (icicle-candidate-set-retrieve '(1)))

;;;###autoload
(defun icicle-save-candidate ()         ; Bound to `insert' in minibuffer.
  "Add current candidate to value of `icicle-saved-completion-candidates'.
You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-save-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (if (not (wholenump icicle-candidate-nb))
      (save-selected-window (select-window (minibuffer-window))
                            (minibuffer-message "  [No current candidate]"))
    (let ((cand (elt icicle-completion-candidates icicle-candidate-nb)))
      (unless (member cand icicle-saved-completion-candidates)
        (push cand icicle-saved-completion-candidates)))
    (save-selected-window (select-window (minibuffer-window))
                          (minibuffer-message "  [Candidate SAVED]"))))

;;;###autoload
(defun icicle-mouse-save-candidate (event) ; Bound to `M-S-mouse-2' in *Completions.
  "Add clicked candidate to value of `icicle-saved-completion-candidates'."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    (read-event)                        ; Swallow mouse up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (icicle-transform-multi-completion
                        (buffer-substring-no-properties beg end))))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event)))
          icicle-last-completion-candidate choice)
    (unless (member icicle-last-completion-candidate icicle-saved-completion-candidates)
      (push icicle-last-completion-candidate icicle-saved-completion-candidates))
    (save-selected-window (select-window (minibuffer-window))
                          (minibuffer-message "  [Candidate SAVED]"))
    (deactivate-mark)
    (icicle-display-candidates-in-Completions)    
    (icicle-raise-Completions-frame posn-col posn-row)))

;;;###autoload
(defun icicle-mouse-candidate-set-save (ignore &optional arg) ; `M-S-mouse-3' in *Completions*.
  "`icicle-candidate-set-save(-selected)'.
If the region is active in *Completions*, then
`icicle-candidate-set-save-selected'.  Otherwise,
`icicle-candidate-set-save'."
  (interactive "e\nP")
  (if (and (get-buffer "*Completions*")
           (save-current-buffer
             (set-buffer (get-buffer "*Completions*"))
             (and mark-active (mark) (/= (point) (mark)))))
      (icicle-candidate-set-save-selected arg)
    (icicle-candidate-set-save arg)))

;;;###autoload
(defun icicle-mouse-candidate-set-save-more (ignore &optional arg) ; `M-mouse-3' in *Completions*.
  "`icicle-candidate-set-save-more(-selected)'.
If the region is active in *Completions*, then
`icicle-candidate-set-save-more-selected'.  Otherwise,
`icicle-candidate-set-save-more'."
  (interactive "e\nP")
  (if (and (get-buffer "*Completions*")
           (save-current-buffer
             (set-buffer (get-buffer "*Completions*"))
             (and mark-active (mark) (/= (point) (mark)))))
      (icicle-candidate-set-save-more-selected arg)
    (icicle-candidate-set-save-more arg)))

;;;###autoload
(defun icicle-mouse-save-then-kill (click &optional arg)
  "`mouse-save-then-kill', but click same place saves selected candidates."
  (interactive "e\nP")
  (flet ((mouse-save-then-kill-delete-region (beg end)
           (icicle-mouse-candidate-set-save-more nil arg)))
    (mouse-save-then-kill click))
  (setq this-command 'mouse-save-then-kill))

;;;###autoload
(defun icicle-candidate-set-save (&optional arg) ; Bound to `C-M->' in minibuffer.
  "Save the set of current completion candidates, for later recall.
Saves candidates in variable `icicle-saved-completion-candidates', by
default.
With a plain prefix arg (`C-u'), save candidates in a cache file.
With a numeric prefix arg (`C-u N'), save candidates in a variable.

You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-save-1 icicle-completion-candidates arg))

;;;###autoload
(defun icicle-candidate-set-save-more (&optional arg) ; Bound to `C->' in minibuffer.
  "Add current completion candidates to saved candidates set.
Adds candidates to `icicle-saved-completion-candidates', by default.
With a plain prefix arg (`C-u'), add candidates to a cache file.
With a numeric prefix arg (`C-u N'), add candidates to the value of
 a variable other than `icicle-saved-completion-candidates'.

The existing saved candidates are still saved.  The current candidates
are added to those already saved.

You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-save-1 icicle-completion-candidates arg t))

;;;###autoload
(defun icicle-candidate-set-save-selected (&optional arg) ; Bound to `C-M-)' in minibuffer.
  "`icicle-candidate-set-save', but only for the selected candidates.
Candidates at least partially in the region are saved."
  (interactive "P")
  (icicle-candidate-set-save-selected-1 arg))

;;;###autoload
(defun icicle-candidate-set-save-more-selected (&optional arg) ; Bound to `C-)' in minibuffer.
  "`icicle-candidate-set-save-more', but only for the selected candidates.
Candidates at least partially in the region are added to those saved."
  (interactive "P")
  (icicle-candidate-set-save-selected-1 arg t))

(defun icicle-candidate-set-save-selected-1 (arg &optional morep)
  "Helper function for `icicle-candidate-set-save(-more)-region'."
  (when (get-buffer-window "*Completions*" 0)
    (let ((beg-cand-nb 0)
          (end-cand-nb 0)
          (candidates ())
          (orig-buf (current-buffer)))
      (with-current-buffer "*Completions*"
        (when (and mark-active (mark) (/= (point) (mark)) icicle-completion-candidates)
          (let ((bob (icicle-start-of-candidates-in-Completions))
                (eob (point-max))
                (beg (region-beginning))
                (end (region-end))
                temp)
            ;; Extend region ends to include all of first and last selected candidates.
            (unless (get-text-property beg 'mouse-face)
              (if (setq temp (next-single-property-change beg 'mouse-face))
                  (setq beg temp)
                (setq beg (next-single-property-change temp 'mouse-face))))
            (unless (get-text-property end 'mouse-face)
              (if (setq temp (previous-single-property-change end 'mouse-face))
                  (setq end temp)
                (setq end (previous-single-property-change temp 'mouse-face))))
            (when (> beg end) (error "No candidates selected")) ; Active region but none selected.
            (while (and (>= beg bob) (get-text-property beg 'mouse-face)) (setq beg (1- beg)))
            (while (and (<= end eob) (get-text-property end 'mouse-face)) (setq end (1+ end)))
            (setq beg (1+ beg) end (1- end))
            (setq beg-cand-nb (icicle-nb-of-candidate-in-Completions beg)
                  end-cand-nb (icicle-nb-of-candidate-in-Completions end))
            (while (<= beg-cand-nb end-cand-nb)
              (push (elt icicle-completion-candidates beg-cand-nb) candidates)
              (setq beg-cand-nb (1+ beg-cand-nb))))))
      (setq candidates (nreverse candidates))
      (icicle-candidate-set-save-1 candidates arg morep t)
      (let ((win (get-buffer-window orig-buf 'visible)))
        (when win (select-window win))))))
            
(defun icicle-candidate-set-save-1 (new-cands arg &optional morep only-selected-p)
  "Helper function for `icicle-candidate-set-save*'."
  (let (where)
    (if arg
        (let ((enable-recursive-minibuffers t))
          (if (consp arg)
              (let* ((file-name         ; Write to cache too.
                      (prog1 (let ((icicle-completion-candidates icicle-completion-candidates))
                               (icicle-add/update-saved-completion-set))
                        (with-output-to-temp-buffer "*Completions*" ; Redisplay.
                          (display-completion-list icicle-completion-candidates))
                        (select-window (minibuffer-window))))
                     (list-buf (and morep (find-file-noselect file-name 'nowarn 'raw)))
                     (old-cands ()))
                (when morep
                  (unwind-protect
                       (condition-case nil
                           (setq old-cands (read list-buf))
                         (end-of-file
                          (save-selected-window
                            (select-window (minibuffer-window))
                            (minibuffer-message (format "  [No completion candidates in file `%s']"
                                                        file-name)))))
                    (kill-buffer list-buf)))
                (setq icicle-saved-completion-candidates (append new-cands old-cands)
                      where    (format "cache file `%s'" file-name))
                (with-temp-message (format "Writing candidates to cache file `%s'..." file-name)
                  (with-temp-file file-name
                    (prin1 icicle-saved-completion-candidates (current-buffer)))))
            (let* ((varname
                    (prog1 (let ((icicle-completion-candidates icicle-completion-candidates))
                             (completing-read (if morep
                                                  "Add candidates to variable: "
                                                "Save candidates in variable: ")
                                              icicle-saved-candidates-variables-obarray
                                              nil nil nil (if (boundp 'variable-name-history)
                                                              'variable-name-history
                                                            'icicle-variable-name-history)))
                      (with-output-to-temp-buffer "*Completions*"
                        (display-completion-list icicle-completion-candidates))
                      (select-window (minibuffer-window))))
                   (var (intern varname))) ; Intern in standard `obarray'.
              (intern varname icicle-saved-candidates-variables-obarray) ; For subsequent completion.
              (set var (if (and morep (boundp var) (listp (symbol-value var)))
                           (append new-cands (symbol-value var))
                         new-cands))
              (setq where (format "`%s'" var)))))
      (setq icicle-saved-completion-candidates
            (if (and morep (listp icicle-saved-completion-candidates))
                (append new-cands icicle-saved-completion-candidates)
              new-cands)
            where "`icicle-saved-completion-candidates'"))
    (deactivate-mark)
    (icicle-display-candidates-in-Completions)
    (save-selected-window
      (select-window (minibuffer-window))
      (minibuffer-message
       (if morep
           (if new-cands
               (format "  [%sandidates ADDED to %s]" (if only-selected-p "Selected c" "C") where)
             "  [NO candidates selected to add]")
         (if new-cands
             (format "  [%sandidates SAVED to %s]" (if only-selected-p "Selected c" "C") where)
           "  [SAVED candidates reset to NONE]"))))))

;; This is actually a top-level command, but it is in this file because it is used by
;; `icicle-retrieve-candidates-from-set' and `icicle-candidate-set-save'.
;;
;; We don't define this using `icicle-define-add-to-alist-command', because we want to
;; return the cache-file name.
;;;###autoload
(defun icicle-add/update-saved-completion-set ()
  "Add or update an entry in `icicle-saved-completion-sets'.
That is, create a new saved completion set or update an existing one.
You are prompted for the name of a set of completion candidates and
its cache file.  List `icicle-saved-completion-sets' is updated to
have an entry with these set and file names.
Return the cache-file name."
  (interactive)
  (let* ((set-name (completing-read "Saved completion set: " icicle-saved-completion-sets
                                    nil nil nil 'icicle-completion-set-history
                                    (caar icicle-saved-completion-sets)))
         (file-name ""))
    (while (not (icicle-file-writable-p file-name))
      (setq file-name (expand-file-name
                       (read-file-name "Cache file for the set: " default-directory nil nil
                                       (concat "icicles-"
                                               (icicle-delete-whitespace-from-string set-name)
                                               ".cache")))))
    (setq icicle-saved-completion-sets  ; Remove any old definition of this set.
          (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
    (push (cons set-name file-name) icicle-saved-completion-sets) ; Add new set definition.
    (customize-save-variable 'icicle-saved-completion-sets icicle-saved-completion-sets)
    (message "Added set to `icicle-saved-completion-sets': `%s'" set-name)
    file-name))                         ; Return cache-file name.

;;;###autoload
(defun icicle-candidate-set-save-to-variable () ; Bound to `C-M-}' in minibuffer.
  "Save the set of current completion candidates in a variable you choose.
You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-variable]' (or `\\[icicle-candidate-set-retrieve]'
with a numeric prefix arg).
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-variable]')."
  (interactive)
  (icicle-candidate-set-save 99))

;;;###autoload
(defun icicle-candidate-set-save-to-cache-file () ; Bound to `C-}' in minibuffer.
  "Save the set of current completion candidates persistently in a file.
You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-cache-file]' or `C-u \\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-cache-file]')."
  (interactive)
  (icicle-candidate-set-save '(1)))

;;;###autoload
(defun icicle-keep-only-past-inputs (&optional recent-first) ; Bound to`M-pause' in minibuffer.
  "Narrow completion candidates to those that have been used previously.
This filters the set of current completion candidates, keeping only
those that have been used before.  (You must first use `TAB' or
`S-TAB' to establish an explicit candidate set.)

With a prefix arg, the previous inputs are sorted chronologically,
most recent first.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (if (and recent-first (interactive-p) icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (let ((icicle-sort-function (if recent-first 'icicle-most-recent-first-p icicle-sort-function)))
      (when (or recent-first (eq icicle-last-completion-command 'icicle-keep-only-past-inputs))
        (icicle-update-completions 'no-display))
      (if (null icicle-completion-candidates)
          (minibuffer-message "  [No completion candidates to filter]")
        (unless (boundp minibuffer-history-variable) (set minibuffer-history-variable nil))
        (when (consp (symbol-value minibuffer-history-variable))
          (setq icicle-completion-candidates
                (icicle-delete-if-not
                 (lambda (candidate)
                   (when (icicle-file-name-input-p)
                     (setq candidate (expand-file-name candidate
                                                       (file-name-directory icicle-last-input))))
                   (member candidate (symbol-value minibuffer-history-variable)))
                 icicle-completion-candidates))
          (cond ((null icicle-completion-candidates)
                 (save-selected-window (icicle-remove-Completions-window))
                 (minibuffer-message "  [None of the completions have been used before]"))
                (t
                 (cond ((and (symbolp last-command) (get last-command 'icicle-cycling-command))
                        (setq icicle-current-input icicle-last-input)
                        (icicle-retrieve-last-input))
                       (t
                        (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))))
                 (cond ((null icicle-completion-candidates)
                        (setq icicle-nb-of-other-cycle-candidates 0)
                        (save-selected-window (icicle-remove-Completions-window))
                        (minibuffer-message "  [No matching history element]"))
                       ((null (cdr icicle-completion-candidates)) ; Single cand. Update minibuffer.
                        (setq icicle-nb-of-other-cycle-candidates 0)
                        (icicle-clear-minibuffer)
                        (setq icicle-last-completion-candidate (car icicle-completion-candidates))
                        (let ((inserted (if (and (icicle-file-name-input-p) insert-default-directory)
                                            (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate
                                             (icicle-file-name-directory-w-default
                                              icicle-current-input))
                                          icicle-last-completion-candidate)))
                          (insert inserted))
                        (save-selected-window (icicle-remove-Completions-window))
                        (icicle-highlight-complete-input)
                        (minibuffer-message (format "  [One matching history element]")))
                       (t
                        (when (member icicle-current-input icicle-completion-candidates)
                          (icicle-highlight-complete-input))
                        (icicle-display-candidates-in-Completions)
                        (save-window-excursion
                          (select-window (active-minibuffer-window))
                          (minibuffer-message
                           (concat "  [Filtered to (matching) historical candidates"
                                   (and recent-first ", most recent first")
                                   "]")))))
                 (setq icicle-last-completion-command 'icicle-keep-only-past-inputs)))))
      icicle-completion-candidates)))

;;;###autoload
(defun icicle-scroll-Completions ()     ; Actioned by repeated `TAB' or `S-TAB' in minubuffer.
  "Scroll the *Completions* window."
  (interactive)
  (save-selected-window
    (select-window (get-buffer-window "*Completions*" 0))
    (if (not (= (window-end) (point-max)))
        (scroll-up nil)
      (unless (= (window-start) (point-min))
        (goto-char (icicle-start-of-candidates-in-Completions))))))

;;;###autoload
(defun icicle-history ()                ; Bound to `M-h' in minibuffer.
  "Access the appropriate history list using completion or cycling.
The current minibuffer input is interpreted as a regexp and matched
against items in the history list in use for the current command.

Note:

If the required input is a file or directory name, then the entire
minibuffer input is what is matched against the history list.  The
reason for this is that file names in the history list are usually
absolute.  This is unlike the case for normal file-name completion,
which assumes the default directory.

Keep this in mind for apropos (regexp) completion; it means that to
match a file-name using a substring you must, in the minibuffer,
either not specify a directory or explicitly use \".*\" before the
file-name substring.

For example, `/foo/bar/lph' will not apropos-match the previously
input file name `/foo/bar/alphabet-soup.el'; you should use either
`/foo/bar/.*lph' or `lph' (no directory).

This also represents a difference in behavior compared to the similar
command `icicle-keep-only-past-inputs' (\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs] in the minibuffer).
That command simply filters the current set of completion candidates,
which in the case of file-name completion is a set of relative file
names.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-history]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (icicle-file-name-input-p) (setq minibuffer-completion-predicate nil))
  (when (arrayp minibuffer-completion-table)
    (setq minibuffer-completion-predicate
          `(lambda (elt) (funcall ',minibuffer-completion-predicate
                          (intern (if (consp elt) (car elt) elt))))))
  (when (and (boundp minibuffer-history-variable) (consp (symbol-value minibuffer-history-variable)))
    (setq minibuffer-completion-table
          (mapcar #'list (icicle-remove-duplicates (symbol-value minibuffer-history-variable)))))
  (save-selected-window (unless icicle-last-completion-command (icicle-apropos-complete)))
  (cond ((and (symbolp last-command) (get last-command 'icicle-cycling-command))
         (setq icicle-current-input icicle-last-input)
         (icicle-retrieve-last-input))
        (t
         (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer)
               icicle-last-input    nil ; So `icicle-save-or-restore-input' thinks input has changed.
               last-command         'icicle-history)
         (funcall icicle-last-completion-command))))

;; This is not actually a minibuffer command, since `isearch' technically uses the echo area.
;;;###autoload
(defun icicle-isearch-complete ()       ; Bound to `S-TAB' in `isearch-mode-map'.
  "Complete the search string using candidates from the search ring."
  (interactive)
  (isearch-done 'nopush)
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
         (completion (completing-read "Complete search string: "
                                      (mapcar #'list (icicle-remove-duplicates ring))
                                      nil nil isearch-string
                                      (if isearch-regexp 'regexp-search-ring 'search-ring))))
    (setq isearch-string completion)
    (icicle-isearch-resume isearch-string isearch-regexp isearch-word isearch-forward
                           (mapconcat 'isearch-text-char-description isearch-string "")
                           nil)))

(defun icicle-isearch-resume (search regexp word forward message case-fold)
  "Resume an incremental search.
SEARCH is the string or regexp searched for.
REGEXP non-nil means the resumed search was a regexp search.
WORD non-nil means resume a word search.
FORWARD non-nil means resume a forward search.
MESSAGE is the echo-area message recorded for the search resumed.
CASE-FOLD non-nil means the search was case-insensitive."
  (isearch-mode forward regexp nil nil word)
  (setq isearch-string search
        isearch-message message
        isearch-case-fold-search case-fold)
  (isearch-search-and-update))

(defalias 'toggle-icicle-WYSIWYG-Completions 'icicle-toggle-WYSIWYG-Completions)
;;;###autoload
(defun icicle-toggle-WYSIWYG-Completions ()
  "Toggle the value of option `icicle-WYSIWYG-Completions-flag'."
  (interactive)
  (setq icicle-WYSIWYG-Completions-flag (not icicle-WYSIWYG-Completions-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-WYSIWYG-Completions-flag
                                      "Using WYSIWYG for *Completions* display is now ON"
                                    "Using WYSIWYG for *Completions* display is now OFF")))

(defalias 'toggle-icicle-~-for-home-dir 'icicle-toggle-~-for-home-dir)
;;;###autoload
(defun icicle-toggle-~-for-home-dir ()  ; Bound to `M-~' in the minibuffer.
  "Toggle the value of option `icicle-use-~-for-home-dir-flag'.
Bound to `M-~' in the minibuffer."
  (interactive)
  (setq icicle-use-~-for-home-dir-flag (not icicle-use-~-for-home-dir-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-use-~-for-home-dir-flag
                                      "Using `~' for home directory is now ON"
                                    "Using `~' for home directory is now OFF")))

(defalias 'toggle-icicle-alternative-sorting 'icicle-toggle-alternative-sorting)
;;;###autoload
(defun icicle-toggle-alternative-sorting () ; Bound to `C-M-,' in the minibuffer.
  "Toggle alternative sorting of minibuffer completion candidates.
This swaps `icicle-alternative-sort-function' and `icicle-sort-function'.
Bound to `C-M-,' in the minibuffer."
  (interactive)
  (let ((alt-sort-fn icicle-alternative-sort-function))
    (setq icicle-alternative-sort-function (or icicle-sort-function icicle-last-sort-function)
          icicle-sort-function (or alt-sort-fn icicle-last-sort-function))
    (icicle-update-completions)
    (icicle-msg-maybe-in-minibuffer
     (format "Sorting: `%s', Alternative: `%s'"
             icicle-sort-function icicle-alternative-sort-function))))

(defalias 'toggle-icicle-sorting 'icicle-toggle-sorting)
;;;###autoload
(defun icicle-toggle-sorting ()         ; Not bound to a key.
  "Toggle sorting of minibuffer completion candidates.
When sorting is active, comparison is done by `icicle-sort-function'."
  (interactive)
  (if (and (interactive-p) icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (if icicle-sort-function
        (setq icicle-last-sort-function icicle-sort-function ; Save it, for restoring.
              icicle-sort-function      nil)
      (setq icicle-sort-function icicle-last-sort-function)) ; Restore it.
    (icicle-update-completions)
    (icicle-msg-maybe-in-minibuffer (if icicle-sort-function
                                        "Completion-candidate sorting is now ON"
                                      "Completion-candidate sorting is now OFF"))))

(defalias 'toggle-icicle-angle-brackets 'icicle-toggle-angle-brackets)
;;;###autoload
(defun icicle-toggle-angle-brackets () ; Bound to `C-<' in the minibuffer.
  "Toggle `icicle-key-descriptions-use-<>-flag'.
Bound to `C-<' in the minibuffer."
  (interactive)
  (setq icicle-key-descriptions-use-<>-flag (not icicle-key-descriptions-use-<>-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-key-descriptions-use-<>-flag
                                      "Displaying <...> in key descriptions is now ON"
                                    "Displaying <...> in key descriptions is now OFF")))

(defalias 'toggle-icicle-transforming 'icicle-toggle-transforming)
;;;###autoload
(defun icicle-toggle-transforming ()    ; Bound to `C-$' in the minibuffer.
  "Toggle transforming of minibuffer completion candidates.
When transforming is active, it is done by `icicle-transform-function'.

By default, transformation, if active, simply removes duplicate
candidates.  Icicles commands already \"do the right thing\" when it
comes to duplicate removal, so you might never need this command.

Bound to `C-$' in the minibuffer."
  (interactive)
  (if icicle-transform-function
      (setq icicle-last-transform-function icicle-transform-function ; Save it, for restoring.
            icicle-transform-function      nil)
    (setq icicle-transform-function icicle-last-transform-function)) ; Restore it.
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-transform-function
                                      "Completion-candidate transformation is now ON"
                                    "Completion-candidate transformation is now OFF")))

(defalias 'toggle-icicle-incremental-completion 'icicle-toggle-incremental-completion)
;;;###autoload
(defun icicle-toggle-incremental-completion () ; Bound to `C-#' in the minibuffer.
  "Toggle the value of option `icicle-incremental-completion-flag'.
If the current value is t or `always', then it is set to nil.
If the current value is nil, then it is set to t.
This command never sets the value to non-nil and non-t.

Bound to `C-#' in the minibuffer."
  (interactive)
  (setq icicle-incremental-completion-flag (not icicle-incremental-completion-flag))
  (setq icicle-incremental-completion-p icicle-incremental-completion-flag)
  (icicle-msg-maybe-in-minibuffer (if icicle-incremental-completion-flag
                                      "Incremental completion is now ON"
                                    "Incremental completion is now OFF")))

(defalias 'toggle-icicle-fuzzy-completion 'icicle-toggle-fuzzy-completion)
;;;###autoload
(defun icicle-toggle-fuzzy-completion () ; Bound to `C-(' in the minibuffer.
  "Toggle the value of option `icicle-fuzzy-completion-flag'.
Bound to `C-(' in the minibuffer."
  (interactive)
  ;; If not loaded, try to load `fuzzy-match.el'.
  (when (and (not icicle-fuzzy-completion-flag) (not (featurep 'fuzzy-match))
             (not (require 'fuzzy-match nil t)))
    (error "You must load library `fuzzy-match.el' for fuzzy completion"))
  (setq icicle-fuzzy-completion-flag  (not icicle-fuzzy-completion-flag)
        icicle-inhibit-sort-p         icicle-fuzzy-completion-flag)
  (icicle-msg-maybe-in-minibuffer (if icicle-fuzzy-completion-flag
                                      "Fuzzy completion is now ON"
                                    "Fuzzy completion is now OFF")))

;;;###autoload
(defun icicle-dispatch-C-^ ()           ; Bound to `C-^' in the minibuffer.
  "Do the right thing for `C-^'
When Icicles searching, call `icicle-toggle-highlight-all-current'.
Otherwise, call `icicle-toggle-ignored-space-prefix'.
Bound to `C-^' in the minibuffer."
  (interactive)
  (if (eq icicle-candidate-action-fn 'icicle-search-action)
      (icicle-toggle-highlight-all-current)
    (icicle-toggle-ignored-space-prefix)))

(defalias 'toggle-icicle-ignored-space-prefix 'icicle-toggle-ignored-space-prefix)
;;;###autoload
(defun icicle-toggle-ignored-space-prefix () ; Bound to `C-^' in the minibuffer.
  "Toggle `icicle-ignore-space-prefix-flag'.
Bound to `C-^' in the minibuffer, except during Icicles searching.

Note: If the current command binds `icicle-ignore-space-prefix-flag'
locally, then it is the local, not the global, value that is changed.
For example, `icicle-buffer' binds it to the value of
`icicle-buffer-ignore-space-prefix-flag'.  If that is non-nil, then
\\<minibuffer-local-completion-map>`\\[icicle-dispatch-C-^]' toggles \
`icicle-ignore-space-prefix-flag' to nil only for the
duration of `icicle-buffer'."
  (interactive)
  (setq icicle-ignore-space-prefix-flag (not icicle-ignore-space-prefix-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-ignore-space-prefix-flag
                                      "Ignoring space prefix is now ON"
                                    "Ignoring space prefix is now OFF")))

(defalias 'toggle-icicle-highlight-all-current 'icicle-toggle-highlight-all-current)
;;;###autoload
(defun icicle-toggle-highlight-all-current () ; Bound to `C-^' in the minibuffer.
  "Toggle `icicle-search-highlight-all-current-flag'.
Bound to `C-^' in the minibuffer during Icicles searching."
  (interactive)
  (setq icicle-search-highlight-all-current-flag (not icicle-search-highlight-all-current-flag))
  (icicle-erase-minibuffer)
  (icicle-retrieve-last-input)
  (icicle-msg-maybe-in-minibuffer
   (if icicle-search-highlight-all-current-flag
       "Highlighting current input match in each main search hit is now ON"
     "Highlighting current input match in each main search hit is now OFF")))

(defalias 'toggle-icicle-highlight-historical-candidates
    'icicle-toggle-highlight-historical-candidates)
;;;###autoload
(defun icicle-toggle-highlight-historical-candidates () ; Bound to `C-pause' in the minibuffer.
  "Toggle `icicle-highlight-historical-candidates-flag'.
Bound to `C-pause' in the minibuffer."
  (interactive)
  (setq icicle-highlight-historical-candidates-flag
        (not icicle-highlight-historical-candidates-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer
   (if icicle-highlight-historical-candidates-flag
       "Highlighting previously used inputs in *Completions* is now ON"
     "Highlighting previously used inputs in *Completions* is now OFF")))

;;;###autoload
(defun icicle-dispatch-C-. ()           ; Bound to `C-.' in the minibuffer.
  "Do the right thing for `C-.'.
When completing a file name, call `icicle-toggle-ignored-extensions'.
Otherwise, call `icicle-toggle-search-cleanup'.

Bound to `C-.' in the minibuffer."
  (interactive)
  (if (icicle-file-name-input-p) (icicle-toggle-ignored-extensions) (icicle-toggle-search-cleanup)))

(defalias 'toggle-icicle-ignored-extensions 'icicle-toggle-ignored-extensions)
;;;###autoload
(defun icicle-toggle-ignored-extensions () ; Bound to `C-.' in minibuffer during file-name input.
  "Toggle respect of `completion-ignored-extensions'.
Bound to `C-.' in minibuffer during file-name input."
  (interactive)
  (if (consp completion-ignored-extensions)
      (setq icicle-saved-ignored-extensions  completion-ignored-extensions ; Save it.
            completion-ignored-extensions    nil
            icicle-ignored-extensions-regexp nil)
    (setq completion-ignored-extensions icicle-saved-ignored-extensions) ; Restore it.
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'")))
  ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
  ;; `completion-ignored-extensions' changes.
  (setq icicle-ignored-extensions completion-ignored-extensions)
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if completion-ignored-extensions
                                      "Ignoring selected file extensions is now ON"
                                    "Ignoring selected file extensions is now OFF")))

(defalias 'toggle-icicle-search-cleanup 'icicle-toggle-search-cleanup)
;;;###autoload
(defun icicle-toggle-search-cleanup () ; Bound to `C-.' in minibuffer, except for file-name input.
  "Toggle removal of `icicle-search' highlighting after a search.
This toggles option `icicle-search-cleanup-flag'.
Bound to `C-.' in the minibuffer, except for file-name input."
  (interactive)
  (setq icicle-search-cleanup-flag (not icicle-search-cleanup-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-search-cleanup-flag
                                      "Removal of Icicles search highlighting is now ON"
                                    "Removal of Icicles search highlighting is now OFF")))

;;$$$ (defun icicle-dispatch-C-backquote ()   ; Bound to `C-`' in the minibuffer.
;;   "Do the right thing for `C-`'.
;; When searching, call `icicle-toggle-literal-replacement'.
;; Otherwise, call `icicle-toggle-regexp-quote'.

;; Bound to `C-`' in the minibuffer."
;;   (interactive)
;;   (if icicle-searching-p (icicle-toggle-literal-replacement) (icicle-toggle-regexp-quote)))

(defalias 'toggle-icicle-regexp-quote 'icicle-toggle-regexp-quote)
;;;###autoload
(defun icicle-toggle-regexp-quote ()    ; Bound to `C-`' in the minibuffer.
  "Toggle escaping of regexp special chars (`icicle-regexp-quote-flag').

Bound to `C-`' in the minibuffer."
  (interactive)
  (setq icicle-regexp-quote-flag (not icicle-regexp-quote-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-regexp-quote-flag
                                      "Escaping of regexp special characters is now ON"
                                    "Escaping of regexp special characters is now OFF")))

(defalias 'toggle-icicle-literal-replacement 'icicle-toggle-literal-replacement)
;;;###autoload
(defun icicle-toggle-literal-replacement () ; Bound to `C-M-`' in the minibuffer.
  "Toggle escaping of regexp special chars in replacement text.
This toggles option `icicle-search-replace-literally-flag'.

Bound to `C-M-`' in the minibuffer."
  (interactive)
  (setq icicle-search-replace-literally-flag (not icicle-search-replace-literally-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-search-replace-literally-flag
                                      "Replacement of text literally is now ON"
                                    "Replacement of text literally is now OFF")))

(defalias 'toggle-icicle-case-sensitivity 'icicle-toggle-case-sensitivity)
;;;###autoload
(defun icicle-toggle-case-sensitivity () ; Bound to `C-S-a' in the minibuffer, that is, `C-A'.
  "Toggle case sensitivity.
This toggles `case-fold-search', `completion-ignore-case', and
`read-file-name-completion-ignore-case'.  More precisely, it toggles
the default value of `case-fold-search', and then it sets the other
variables to the value of `case-fold-search'.

Note:

1. This toggles the default value of `case-fold-search'.  This means
that it does not matter which buffer is current when you call this
command, and all buffers will be affected henceforth.

2. Some commands bind one or more of these variables, so invoking this
command during command execution will not necessarily toggle the
global values of all of the variables.

Bound to `C-S-a' in the minibuffer, that is, `C-A'."
  (interactive)
  (setq-default case-fold-search (not case-fold-search))
  (setq completion-ignore-case case-fold-search)
  (when (boundp 'read-file-name-completion-ignore-case)
    (setq read-file-name-completion-ignore-case case-fold-search))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if case-fold-search
                                      "Case-sensitive comparison is now OFF, everywhere"
                                    "Case-sensitive comparison is now ON, everywhere")))

;;;###autoload
(defun icicle-remove-Completions-window () ; Bound to `C-x 0' in the minibuffer.
  "Remove the *Completions* window.
Bound to `C-x 0' in the minibuffer."
  (interactive) (icicle-delete-windows-on "*Completions*")) ; Defined in `icicles-cmd.el'.

;; This is actually a top-level command, but it is in this file because it is used by
;; `icicle-remove-Completions-window'.
;;;###autoload
(defun icicle-delete-windows-on (buffer)
  "Delete all windows showing BUFFER."
  (interactive
   (list (read-buffer "Remove all windows showing buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    ;; Avoid error message "Attempt to delete minibuffer or sole ordinary window".
    (let ((frames (icicle-frames-on buffer t)))
      (unless (and frames (null (cdr frames)) ; One frame shows buffer.
                   (cdr (assoc 'minibuffer (frame-parameters (car frames)))) ; Has a minibuffer.
                   (save-window-excursion
                     (select-frame (car frames))
                     (one-window-p t 'selected-frame))) ; Only one window.
        (dolist (fr frames)
          (delete-window (get-buffer-window buffer 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mcmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mcmd.el ends here
