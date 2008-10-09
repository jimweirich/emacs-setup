;;; icicles-mode.el --- Icicle Mode definition for Icicles
;;
;; Filename: icicles-mode.el
;; Description: Icicle Mode definition for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2008, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 10:21:10 2006
;; Version: 22.0
;; Last-Updated: Sat Oct  4 12:06:53 2008 (-0700)
;;           By: dradams
;;     Update #: 4990
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mode.el
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
;;   `hexrgb', `icicles-cmd', `icicles-fn', `icicles-mac',
;;   `icicles-mcmd', `icicles-opt', `icicles-var', `info', `info+',
;;   `kmacro', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `mkhtml', `mkhtml-htmlize', `mwheel', `pp', `pp+', `ring',
;;   `ring+', `second-sel', `strings', `subr-21', `thingatpt',
;;   `thingatpt+', `unaccent', `w32-browser', `w32browser-dlgopen',
;;   `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines the
;;  command `icicle-mode'.  See `icicles.el' for documentation.
;;
;;  Commands defined here:
;;
;;    `icicle-handle-switch-frame', `icicle-mode', `icy-mode',
;;    `icicle-skip-this-command'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-activate-mark', `icicle-bind-completion-keys',
;;    `icicle-bind-isearch-keys',
;;    `icicle-bind-S-TAB-for-map-variable',
;;    `icicle-bind-S-TAB-in-keymaps-from',
;;    `icicle-cancel-Help-redirection', `icicle-define-icicle-maps',
;;    `icicle-minibuffer-setup', `icicle-rebind-completion-maps',
;;    `icicle-rebind-global', `icicle-rebind-non-completion-keys',
;;    `icicle-restore-non-completion-keys',
;;    `icicle-redefine-standard-commands',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns', `icicle-remap',
;;    `icicle-restore-completion-keys', `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-select-minibuffer-contents', `icicle-set-calling-cmd',
;;    `icicle-S-iso-lefttab-to-S-TAB', `icicle-top-level-prep',
;;    `icicle-unbind-isearch-keys',
;;    `icicle-unbind-S-TAB-for-map-variable',
;;    `icicle-unbind-S-TAB-in-keymaps-from',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-update-help-string',
;;    `icicle-update-ignored-extensions-regexp'.
;;
;;  User options defined here (in Custom group `Icicles'):
;;
;;    `icicle-mode', `icicle-mode-hook'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-bookmark-menu-map', `icicle-custom-menu-map',
;;    `icicle-describe-menu-map', `icicle-edit-menu-map',
;;    `icicle-file-menu-map', `icicle-frames-menu-map',
;;    `icicle-info-menu-map', `icicle-mode-map',
;;    `icicle-options-menu-map', `icicle-search-menu-map',
;;    `icicle-search-tags-menu-map'.
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
;;  (@> "User Options (alphabetical)")
;;  (@> "Internal variables (alphabetical)")
;;  (@> "Icicle mode command")
;;  (@> "Other Icicles functions that define Icicle mode")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'icicles-opt)
  ;; icicle-buffer-configs, icicle-buffer-extras, icicle-change-region-background-flag,
  ;; icicle-cycling-respects-completion-mode-flag, icicle-incremental-completion-flag,
  ;; icicle-default-value, icicle-kmacro-ring-max, icicle-minibuffer-setup-hook,
  ;; icicle-modal-cycle-down-keys, icicle-modal-cycle-up-keys,
  ;; icicle-redefine-standard-commands-flag, icicle-regexp-search-ring-max,
  ;; icicle-region-background, icicle-search-ring-max, icicle-show-Completions-initially-flag,
  ;; icicle-top-level-key-bindings, icicle-touche-pas-aux-menus-flag,
  ;; icicle-word-completion-keys, icicle-yank-function
(require 'icicles-fn) ;; assq-delete-all, icicle-completing-p,
                      ;; icicle-isearch-complete-past-string, icicle-unhighlight-lighter
(require 'icicles-var)
  ;; icicle-candidate-action-fn, icicle-candidate-nb, icicle-cmd-calling-for-completion,
  ;; icicle-completing-p, icicle-completion-candidates, icicle-completion-help-string,
  ;; icicle-current-completion-mode, icicle-default-directory, icicle-ignored-extensions,
  ;; icicle-ignored-extensions-regexp, icicle-incremental-completion-p, icicle-initial-value,
  ;; icicle-last-completion-candidate, icicle-last-completion-command, icicle-last-input,
  ;; icicle-menu-map, icicle-pre-minibuffer-buffer, icicle-minor-mode-map-entry, icicle-prompt,
  ;; icicle-saved-completion-candidates, icicle-saved-kmacro-ring-max,
  ;; icicle-saved-regexp-search-ring-max, icicle-saved-region-background,
  ;; icicle-saved-search-ring-max, icicle-search-current-overlay, icicle-search-overlays,
  ;; icicle-search-refined-overlays
(require 'icicles-cmd)
  ;; icicle-add-buffer-candidate, icicle-add-buffer-config, icicle-bbdb-complete-name,
  ;; icicle-customize-face, icicle-customize-face-other-window, icicle-dabbrev-completion,
  ;; icicle-imenu, icicle-occur, icicle-search, icicle-search-buffer, icicle-search-region,
  ;; icicle-search-all-regions, icicle-search-file

;; Use `condition-case' because if `mb-depth.el' can't be found, `mb-depth+.el' is not provided.
(when (>= emacs-major-version 22) (condition-case nil (require 'mb-depth+ nil t) (error nil)))
  ;; (no error if not found): minibuffer-depth-indicate-mode

(eval-when-compile
 (when (< emacs-major-version 21) (require 'cl))) ;; push, dolist
                                                  ;; plus, for Emacs < 20: when, unless
(require 'dired+ nil t) ;; (no error if not found):
                        ;; diredp-menu-bar-operate-menu, diredp-menu-bar-subdir-menu
(require 'dired) ;; dired-mode-map
(require 'menu-bar+ nil t) ;; (no error if not found):
  ;; menu-bar-apropos-menu, menu-bar-describe-menu, menu-bar-edit-menu,
  ;; menu-bar-file-menu, menu-bar-frames-menu, menu-bar-options-menu, menu-bar-search-tags-menu

;; `icicle-apropos-complete' is used here.  It is defined in `icicles-cmd.el'.
;; `icicle-file-name-input-p' is used here.  It is defined in `icicles-fn.el'.

;;; Defvars to quiet byte-compiler:
(when (< emacs-major-version 22)
  (defvar kmacro-ring-max)
  (defvar minibuffer-local-filename-completion-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "User Options (alphabetical)")

;;; User Options (alphabetical) --------------------------------------

;; Emacs 20 only
(unless (fboundp 'define-minor-mode)
  (defcustom icicle-mode nil
    "*Toggle minibuffer input completion and cycling.
Setting this variable directly does not take effect;
use either \\[customize] or command `icy-mode' (aka `icicle-mode')."
    :set (lambda (symbol value) (icicle-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'Icicles-Miscellaneous :require 'icicles))

;;;###autoload
(defcustom icicle-mode-hook nil
  "*Functions run after entering and exiting Icicle mode."
  :type 'hook :group 'Icicles-Miscellaneous)
 
;;(@* "Internal variables (alphabetical)")

;;; Internal variables (alphabetical) --------------------------------

(defvar icicle-mode-map nil
  "Keymap for Icicle mode.  These are top-level key bindings.
See also `icicle-rebind-completion-maps' for minibuffer bindings.")
 
;;(@* "Icicle mode command")

;;; Icicle mode command ----------------------------------------------

;; Main command.  Inspired from `icomplete-mode'.
(defalias 'icy-mode 'icicle-mode)
;;;###autoload
(if (fboundp 'define-minor-mode)
    ;; Emacs 21+ ------------
    (eval '(define-minor-mode icicle-mode
            "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

The following top-level commands are also available in Icicle mode:

`clear-option' (alias)                 - Set binary option(s) to nil
`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-region'                    - Add to `icicle-region-alist'
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apply'                         - Apply function to alist items
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-zippy'                 - Show matching Zippy quotes
`icicle-bookmark'(`-other-window')'    - Jump to bookmark(s)
`icicle-buffer'(`-other-window')       - Switch to buffer(s)
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-clear-current-history'         - Clear current history entries
`icicle-clear-history'                 - Clear entries from a history
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-command-abbrev'                - Multi-command `M-x' + abbrevs
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completion-help'               - Give bindings for completion
`icicle-customize-face'                - Multi-`customize-face'
`icicle-customize-icicles-group'       - Customize Icicles
`icicle-delete-file'                   - Delete file(s)/directory(s)
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows-on'             - Delete all windows for buffer
`icicle-doremi-candidate-width-factor' - +/- candidate column width
`icicle-doremi-inter-candidates-min-spaces' - +/- candidate spacing
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-execute-extended-command'      - Multi-command `M-x'
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-file'(`-other-window')         - Visit file(s)/directory(s)
`icicle-find-file'(`-other-window')    -       same: relative only
`icicle-find-file-absolute'(`-other-window') - same: absolute only
`icicle-find-first-tag'(`-other-window')- Visit definition(s) with tag
`icicle-find-tag'                      - Visit definition(s) with tag
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description(s)
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-imenu'                         - Navigate among Imenu entries
`icicle-Info-goto-node'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-Info-menu'                     - Multi-command `Info-menu'
`icicle-insert-kill'                   - Like `yank', without rotating
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-kill-buffer'                   - Kill buffer
`icicle-kmacro'                        - Execute a keyboard macro
`icicle-locate-file'(`-other-window')  - Visit file(s) in a directory
`icy-mode' or `icicle-mode'            - Toggle Icicle mode
`icicle-next-apropos-match-function'   - Change apropos match function
`icicle-occur'                         - `occur' + apropos icompletion
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-recent-file'(`-other-window')  - Open recently used file(s)
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-region'             - Remove from `icicle-region-alist'
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-keywords'               - Search with regexp keywords
`icicle-search-region'                 - Search multiple regions
`icicle-search-word'                   - Whole-word search
`icicle-select-frame'                  - Select and raise a frame
`icicle-select-region'                 - Select from multiple regions
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-C-for-actions'          - Toggle using `C-' for actions
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-expand-to-common-match' - Toggle input expansion
`icicle-toggle-fuzzy-completion'       - Toggle fuzzy completion
`icicle-toggle-ignored-extensions'     - Toggle ignoring file suffixes
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option(s)
`icicle-toggle-proxy-candidates'       - Toggle proxy candidates
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-search-replace-common-match' - Toggle ECM replacment
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-toggle-WYSIWYG-Completions'    - Toggle WYSIWYG *Completions*
`icicle-vardoc'                        - Show variable description(s)
`icicle-where-is'                      - `where-is' multi-command
`icicle-yank-insert'                   - `yank' using completion
`toggle' (alias)                       - Toggle binary user option(s)

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' \
when the minibuffer is active."
            :global t :group 'Icicles-Miscellaneous :lighter " Icy" :init-value nil
            (cond (icicle-mode
                   (icicle-define-icicle-maps)
                   (icicle-rebind-non-completion-keys)
                   (add-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                   (add-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
                   (add-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                   (add-hook 'minibuffer-exit-hook     'icicle-unhighlight-lighter)
                   (add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)
                   (add-hook 'completion-setup-hook    'icicle-set-calling-cmd 'append)
                   (when icicle-customize-save-flag
                     (add-hook 'kill-emacs-hook        'icicle-command-abbrev-save))
                   (add-hook 'comint-mode-hook         'icicle-comint-hook-fn)
                   (add-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
                   (add-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
                   (add-hook 'temp-buffer-show-hook    'icicle-fit-completions-window)
                   (icicle-undo-std-completion-faces)
                   (icicle-redefine-std-completion-fns)
                   (icicle-redefine-standard-commands)
                   (icicle-redefine-standard-options)
                   (icicle-bind-isearch-keys)
                   (when (fboundp 'minibuffer-depth-indicate-mode) ; Defined in `mb-depth(+).el'
                     (minibuffer-depth-indicate-mode 99)))
                  (t
                   (makunbound 'icicle-mode-map)
                   (icicle-restore-non-completion-keys)
                   (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                   (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
                   (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                   (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
                   ;; The pre- and post-command hooks are local to the minibuffer,
                   ;; So they are added in `icicle-minibuffer-setup', not here.
                   ;; Nevertheless, they are removed here when Icicle mode is exited.
                   (remove-hook 'pre-command-hook         'icicle-top-level-prep)
                   (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook t)
                   (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook t)
                   (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
                   (remove-hook 'kill-emacs-hook          'icicle-command-abbrev-save)
                   (remove-hook 'comint-mode-hook         'icicle-comint-hook-fn)
                   (remove-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
                   (remove-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
                   (remove-hook 'temp-buffer-show-hook    'icicle-fit-completions-window)
                   ;; $$ Should restore standard completion faces here.
                   (icicle-restore-std-completion-fns)
                   (icicle-restore-standard-commands)
                   (icicle-restore-standard-options)
                   (icicle-unbind-isearch-keys)
                   (when (fboundp 'minibuffer-depth-indicate-mode)
                     (minibuffer-depth-indicate-mode -99))))
            (message "Turning %s Icicle mode..." (if icicle-mode "ON" "OFF"))
            (icicle-rebind-completion-maps icicle-mode)
            (icicle-update-help-string)
            (run-hooks 'icicle-mode-hook)
            (message "Turning %s Icicle mode...done" (if icicle-mode "ON" "OFF"))))

  ;; Emacs 20 ------------
  (defun icicle-mode (&optional arg)
    "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

The following top-level commands are also available in Icicle mode:

`clear-option' (alias)                 - Set binary option(s) to nil
`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-region'                    - Add to `icicle-region-alist'
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apply'                         - Apply function to alist items
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-zippy'                 - Show matching Zippy quotes
`icicle-bookmark'(`-other-window')'    - Jump to bookmark(s)
`icicle-buffer'(`-other-window')       - Switch to buffer(s)
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-clear-current-history'         - Clear current history entries
`icicle-clear-history'                 - Clear entries from a history
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-command-abbrev'                - Multi-command `M-x' + abbrevs
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completion-help'               - Give bindings for completion
`icicle-customize-face'                - Multi-`customize-face'
`icicle-customize-icicles-group'       - Customize Icicles
`icicle-delete-file'                   - Delete file(s)/directory(s)
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows-on'             - Delete all windows for buffer
`icicle-doremi-candidate-width-factor' - +/- candidate column width
`icicle-doremi-inter-candidates-min-spaces' - +/- candidate spacing
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-execute-extended-command'      - Multi-command `M-x'
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-file'(`-other-window')         - Visit file(s)/directory(s)
`icicle-find-file'(`-other-window')    -       same: relative only
`icicle-find-file-absolute'(`-other-window') - same: absolute only
`icicle-find-first-tag'(`-other-window')- Visit definition(s) with tag
`icicle-find-tag'                      - Visit definition(s) with tag
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description(s)
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-imenu'                         - Navigate among Imenu entries
`icicle-Info-goto-node'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-Info-menu'                     - Multi-command `Info-menu'
`icicle-insert-kill'                   - Like `yank', without rotating
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-kill-buffer'                   - Kill buffer
`icicle-kmacro'                        - Execute a keyboard macro
`icicle-locate-file'(`-other-window')  - Visit file(s) in a directory
`icy-mode' or `icicle-mode'            - Toggle Icicle mode
`icicle-next-apropos-match-function'   - Change apropos match function
`icicle-occur'                         - `occur' + apropos icompletion
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-recent-file'(`-other-window')  - Open recently used file(s)
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-region'             - Remove from `icicle-region-alist'
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-keywords'               - Search with regexp keywords
`icicle-search-region'                 - Search multiple regions
`icicle-search-word'                   - Whole-word search
`icicle-select-frame'                  - Select and raise a frame
`icicle-select-region'                 - Select from multiple regions
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-C-for-actions'          - Toggle using `C-' for actions
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-expand-to-common-match' - Toggle input expansion
`icicle-toggle-fuzzy-completion'       - Toggle fuzzy completion
`icicle-toggle-ignored-extensions'     - Toggle ignoring file suffixes
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option(s)
`icicle-toggle-proxy-candidates'       - Toggle proxy candidates
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-search-replace-common-match' - Toggle ECM replacment
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-toggle-WYSIWYG-Completions'    - Toggle WYSIWYG *Completions*
`icicle-vardoc'                        - Show variable description(s)
`icicle-where-is'                      - `where-is' multi-command
`icicle-yank-insert'                   - `yank' using completion
`toggle' (alias)                       - Toggle binary user option(s)

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' \
when the minibuffer is active."
    (interactive "P")
    (setq icicle-mode (if arg (> (prefix-numeric-value arg) 0) (not icicle-mode)))
    (icicle-rebind-completion-maps icicle-mode)
    (cond (icicle-mode
           (icicle-define-icicle-maps)
           (icicle-rebind-non-completion-keys)
           ;; This is not really necessary after the first time - no great loss.
           (add-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (add-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
           (add-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (add-hook 'minibuffer-exit-hook     'icicle-unhighlight-lighter)
           (add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)
           (add-hook 'completion-setup-hook    'icicle-set-calling-cmd 'append)
           (when icicle-customize-save-flag
             (add-hook 'kill-emacs-hook        'icicle-command-abbrev-save))
           (add-hook 'comint-mode-hook         'icicle-comint-hook-fn)
           (add-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
           (add-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
           (add-hook 'temp-buffer-show-hook    'icicle-fit-completions-window)
           (icicle-redefine-std-completion-fns)
           (icicle-redefine-standard-commands)
           (icicle-redefine-standard-options)
           (icicle-bind-isearch-keys)
           (icicle-update-help-string)
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now ON"))
          (t
           (makunbound 'icicle-mode-map)
           (icicle-restore-non-completion-keys)
           (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
           (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
           ;; The pre- and post-command hooks are local to the minibuffer,
           ;; So they are added in `icicle-minibuffer-setup', not here.
           ;; Nevertheless, they are removed here when Icicle mode is exited.
           (remove-hook 'pre-command-hook         'icicle-top-level-prep)
           (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook t)
           (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook t)
           (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
           (remove-hook 'kill-emacs-hook          'icicle-command-abbrev-save)
           (remove-hook 'comint-mode-hook         'icicle-comint-hook-fn)
           (remove-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
           (remove-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
           (remove-hook 'temp-buffer-show-hook    'icicle-fit-completions-window)
           (icicle-restore-std-completion-fns)
           (icicle-restore-standard-commands)
           (icicle-restore-standard-options)
           (icicle-unbind-isearch-keys)
           (icicle-update-help-string)
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now OFF"))))
  (add-to-list 'minor-mode-alist '(icicle-mode " Icy")))

(defun icicle-top-level-prep ()
  "Do top-level stuff.  Used in `pre-command-hook'."
  ;; Reset `icicle-candidates-alist' to nil; save top-level command.
  (when (= 0 (recursion-depth))
    (setq icicle-last-top-level-command  this-command
          icicle-candidates-alist        nil)))

(defun icicle-define-icicle-maps ()
  "Define `icicle-mode-map' and `icicle-menu-map'."
  (setq icicle-mode-map (make-sparse-keymap)) ; Recreate it each time, to capture latest bindings.

  ;; Define Icicles menu-bar menu.  Create it only once.  Sacrifice latest bindings for speed.
  (unless icicle-menu-map
    (setq icicle-menu-map (make-sparse-keymap "Icicles"))
    (define-key icicle-menu-map [icicle-mode] '(menu-item "Turn Off Icicle Mode" icicle-mode))
    (define-key icicle-menu-map [icicle-abort]
      '(menu-item "Cancel Minibuffer" icicle-abort-recursive-edit
        :enable (active-minibuffer-window)))
    (define-key icicle-menu-map [icicle-report-bug]
      '(menu-item "Send Bug Report" icicle-send-bug-report))
    (define-key icicle-menu-map [icicle-customize-icicles-group]
      '(menu-item "Customize Icicles" icicle-customize-icicles-group))
    (define-key icicle-menu-map [icicle-help]
      '(menu-item "Help" icicle-completion-help
        :help "Display help on minibuffer input and completion" :keys "C-?"))
    (define-key icicle-menu-map [icicle-separator-last] '("--"))

    (unless icicle-touche-pas-aux-menus-flag ; Use Dired > Multiple or Operate menu.
      (defvar icicle-dired-multiple-menu-map (make-sparse-keymap)
        "Icicles submenu for Dired > Multiple (or Operate) menu.")
      (if (boundp 'diredp-menu-bar-operate-menu) ; Defined in `dired+.el'.
          (define-key diredp-menu-bar-operate-menu [icicles]
            (cons "Icicles" icicle-dired-multiple-menu-map))
        (define-key dired-mode-map [menu-bar operate icicles]
          (cons "Icicles" icicle-dired-multiple-menu-map)))
      (define-key icicle-dired-multiple-menu-map [icicle-search-dired-marked]
        '(menu-item "Search (and Replace)..." icicle-search-dired-marked
          :visible icicle-mode :enable (eq major-mode 'dired-mode)))
      (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked-more]
        '(menu-item "Save as More Completion Candidates" icicle-dired-save-marked-more
          :visible icicle-mode :enable (eq major-mode 'dired-mode)))
      (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked]
        '(menu-item "Save as Completion Candidates" icicle-dired-save-marked
          :visible icicle-mode :enable (eq major-mode 'dired-mode)))
      (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked-as-project]
        '(menu-item "Save as Project" icicle-dired-save-marked-as-project
          :visible icicle-mode :enable (eq major-mode 'dired-mode))))

    (unless icicle-touche-pas-aux-menus-flag ; Use Dired > Multiple or Operate menu.
      (defvar icicle-dired-dir-menu-map (make-sparse-keymap)
        "Icicles submenu for Dired > Dir (or Subdir) menu.")
      (if (boundp 'diredp-menu-bar-subdir-menu) ; Defined in `dired+.el'.
          (define-key diredp-menu-bar-subdir-menu [icicles]
            (cons "Icicles" icicle-dired-dir-menu-map))
        (define-key dired-mode-map [menu-bar subdir icicles]
          (cons "Icicles" icicle-dired-dir-menu-map)))
      (define-key icicle-dired-dir-menu-map [icicle-dired-saved-file-candidates-other-window]
        '(menu-item "Open Dired for Saved Completion Candidates..."
          icicle-dired-saved-file-candidates-other-window
          :visible icicle-mode
          :enable (and icicle-saved-completion-candidates (eq major-mode 'dired-mode))))
      (define-key icicle-dired-dir-menu-map [icicle-dired-project-other-window]
        '(menu-item "Open Dired for Project..." icicle-dired-project-other-window
          :visible icicle-mode
          :enable (and icicle-saved-completion-sets (eq major-mode 'dired-mode)))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'Info-mode-menu)) ; Use Info menu, if available.
           (defvar icicle-info-menu-map (make-sparse-keymap)
             "Icicles submenu for Info menu.")
           (define-key Info-mode-menu [icicles] (cons "Icicles" icicle-info-menu-map))
           (define-key icicle-info-menu-map [icicle-Info-goto-node]
             '(menu-item "+ Go to Node..." icicle-Info-goto-node :visible icicle-mode
               :enable (eq major-mode 'Info-mode) :keys "g"))
           (define-key icicle-info-menu-map [icicle-Info-menu]
             '(menu-item "+ Go to Menu Node..." icicle-Info-menu :visible icicle-mode
               :enable (eq major-mode 'Info-mode) :keys "m"))
           (define-key icicle-info-menu-map [icicle-Info-index]
             '(menu-item "+ Look Up in Index..." icicle-Info-index :visible icicle-mode
               :enable (eq major-mode 'Info-mode) :keys "i")))
          (t
           (define-key icicle-menu-map [icicle-Info-goto-node]
             '(menu-item "+ Go to Node..." icicle-Info-goto-node
               :enable (eq major-mode 'Info-mode)))
           (define-key icicle-menu-map [icicle-Info-menu]
             '(menu-item "+ Go to Menu Node..." icicle-Info-menu
               :enable (eq major-mode 'Info-mode)))
           (define-key icicle-menu-map [icicle-Info-index]
             '(menu-item "+ Look Up in Index..." icicle-Info-index
               :enable (eq major-mode 'Info-mode)))
           (define-key icicle-menu-map [icicle-separator-Info]
             '(menu-item "--" icicle-separator-Info :visible icicle-mode
               :enable (eq major-mode 'Info-mode)))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-frames-menu)) ; Defined in `menu-bar+.el'.
           (defvar icicle-frames-menu-map (make-sparse-keymap)
             "Icicles submenu for Frames menu.")
           (define-key menu-bar-frames-menu [icicles] (cons "Icicles" icicle-frames-menu-map))
           (define-key icicle-frames-menu-map [icicle-font]
             '(menu-item "+ Change Font" icicle-font :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-frames-menu-map [icicle-frame-fg]
             '(menu-item "+ Change Foreground..." icicle-frame-fg :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-frames-menu-map [icicle-frame-bg]
             '(menu-item "+ Change Background..." icicle-frame-bg :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-font]
             '(menu-item "+ Change Font of Frame..." icicle-font
               :enable (and icicle-mode
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-frame-fg]
             '(menu-item "+ Change Foreground of Frame..." icicle-frame-fg
               :enable (and icicle-mode
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-frame-bg]
             '(menu-item "+ Change Background of Frame..." icicle-frame-bg
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-separator-frame] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-describe-menu)) ; Use Describe menu, if available.
           (defvar icicle-describe-menu-map (make-sparse-keymap)
             "Icicles submenu for Describe menu.")
           (define-key menu-bar-describe-menu [icicles] (cons "Icicles" icicle-describe-menu-map))
           (define-key icicle-describe-menu-map [icicle-plist]
             '(menu-item "+ Symbol with Property List..." icicle-plist :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-doc]
             '(menu-item "+ Doc of Fun, Var, or Face..." icicle-doc :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-fundoc]
             '(menu-item "+ Function with Name, Doc..." icicle-fundoc :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-vardoc]
             '(menu-item "+ Variable with Name, Doc..." icicle-vardoc :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-describe-option-of-type]
             '(menu-item "+ Option of Type..." icicle-describe-option-of-type
               :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-where-is]
             '(menu-item "+ Where Is..." icicle-where-is :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-plist]
             '(menu-item "+ Symbol with Property List..." icicle-plist))
           (define-key icicle-menu-map [icicle-doc]
             '(menu-item "+ Doc of Fun, Var, or Face..." icicle-doc))
           (define-key icicle-menu-map [icicle-fundoc]
             '(menu-item "+ Describe Function with Name, Doc..." icicle-fundoc))
           (define-key icicle-menu-map [icicle-vardoc]
             '(menu-item "+ Describe Variable with Name, Doc..." icicle-vardoc))
           (define-key icicle-menu-map [icicle-describe-option-of-type]
             '(menu-item "+ Option of Type..." icicle-describe-option-of-type))
           (define-key icicle-menu-map [icicle-where-is]
             '(menu-item "+ Where Is..." icicle-where-is))
           (define-key icicle-menu-map [icicle-separator-doc] '("--"))))

    (define-key icicle-menu-map [icicle-apply]
      '(menu-item "+ Apply Function to Alist Items..." icicle-apply))
    (define-key icicle-menu-map [icicle-save-string-to-variable]
      '(menu-item "Save String to Variable..." icicle-save-string-to-variable))
    (define-key icicle-menu-map [icicle-color-theme]
      '(menu-item "+ Choose Color Theme..." icicle-color-theme
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-remove-saved-completion-set]
      '(menu-item "+ Remove Saved Candidate Set..." icicle-remove-saved-completion-set
        :enable icicle-saved-completion-sets))
    (define-key icicle-menu-map [icicle-add/update-saved-completion-set]
      '(menu-item "Add/Update Saved Candidate Set..." icicle-add/update-saved-completion-set))
    (when (fboundp 'icicle-kmacro)
      (define-key icicle-menu-map [icicle-kmacro]
        '(menu-item "+ Execute Nth Keyboard Macro..." icicle-kmacro
          :enable (or (kmacro-ring-head) kmacro-ring))))
    (define-key icicle-menu-map [icicle-execute-named-keyboard-macro]
      '(menu-item "+ Execute Named Keyboard Macro..." icicle-execute-named-keyboard-macro))
    (define-key icicle-menu-map [icicle-separator-misc] '("--"))
    (define-key icicle-menu-map [icicle-imenu]
      '(menu-item "+ Imenu..." icicle-imenu
        :enable imenu-generic-expression))
    (define-key icicle-menu-map [icicle-goto-global-marker]
      '(menu-item "+ Go To Global Marker..." icicle-goto-global-marker
        :enable (consp (icicle-markers global-mark-ring)) :keys "C-- C-x C-SPC"))
    (define-key icicle-menu-map [icicle-goto-marker]
      '(menu-item "+ Go To Marker..." icicle-goto-marker
        :enable (consp (icicle-markers mark-ring)) :keys "C-- C-SPC"))
    (define-key icicle-menu-map [icicle-separator-goto] '("--"))

    (define-key icicle-menu-map [icicle-remove-region]
      '(menu-item "+ Remove Saved Region from List..." icicle-remove-region
        :enable icicle-region-alist))
    (define-key icicle-menu-map [icicle-add-region]
      '(menu-item "Save Current Region" icicle-add-region
        :enable mark-active :keys "C-9 C-x C-x"))
    (define-key icicle-menu-map [icicle-search-region]
      '(menu-item "+ Search Saved Region..." icicle-search-region
        :enable icicle-region-alist))
    (define-key icicle-menu-map [icicle-select-region]
      '(menu-item "+ Choose Saved Region..." icicle-select-region
        :enable icicle-region-alist :keys "C-u C-x C-x"))
    (define-key icicle-menu-map [icicle-separator-region] '("--"))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-apropos-menu)) ; Use Apropos menu, if available.
           (defvar icicle-apropos-menu-map (make-sparse-keymap)
             "Icicles submenu for Apropos menu.")
           (define-key menu-bar-apropos-menu [icicles] (cons "Icicles" icicle-apropos-menu-map))
           (define-key icicle-apropos-menu-map [icicle-apropos-zippy]
             '(menu-item "Zippy..." icicle-apropos-zippy :visible icicle-mode))
           (cond ((fboundp 'apropos-option)
                  (define-key icicle-apropos-menu-map [icicle-apropos]
                    '(menu-item "Symbols..." icicle-apropos :visible icicle-mode))
                  (define-key icicle-apropos-menu-map [icicle-apropos-function]
                    '(menu-item "Functions..." icicle-apropos-function :visible icicle-mode))
                  (define-key icicle-apropos-menu-map [icicle-apropos-variable]
                    '(menu-item "Variables..." icicle-apropos-variable :visible icicle-mode))
                  (define-key icicle-apropos-menu-map [icicle-apropos-option]
                    '(menu-item "Options..." icicle-apropos-option :visible icicle-mode))
                  (define-key icicle-apropos-menu-map [icicle-apropos-command]
                    '(menu-item "Commands..." icicle-apropos-command :visible icicle-mode)))
                 (t
                  (define-key icicle-apropos-menu-map [icicle-apropos-variable]
                    '(menu-item "Variables..." icicle-apropos-variable
                      :visible icicle-mode))))
           (define-key icicle-apropos-menu-map [icicle-apropos-command]
             '(menu-item "Commands..." icicle-apropos-command :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-apropos-zippy]
             '(menu-item "Apropos Zippy..." icicle-apropos-zippy))
           (cond ((fboundp 'apropos-option)
                  (define-key icicle-menu-map [icicle-apropos]
                    '(menu-item "Apropos..." icicle-apropos))
                  (define-key icicle-menu-map [icicle-apropos-function]
                    '(menu-item "Apropos Functions..." icicle-apropos-function))
                  (define-key icicle-menu-map [icicle-apropos-variable]
                    '(menu-item "Apropos Variables..." icicle-apropos-variable))
                  (define-key icicle-menu-map [icicle-apropos-option]
                    '(menu-item "Apropos Options..." icicle-apropos-option))
                  (define-key icicle-menu-map [icicle-apropos-command]
                    '(menu-item "Apropos Commands..." icicle-apropos-command)))
                 (t
                  (define-key icicle-menu-map [icicle-apropos-variable]
                    '(menu-item "Apropos Variables..." icicle-apropos-variable))
                  (define-key icicle-menu-map [icicle-apropos-command]
                    '(menu-item "Apropos Commands..." icicle-apropos-command))))
           (define-key icicle-menu-map [icicle-separator-apropos] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-options-menu)) ; Use Options menu, if available.
           (defvar icicle-options-menu-map (make-sparse-keymap)
             "Icicles submenu for Options menu.")
           (define-key menu-bar-options-menu [icicles] (cons "Icicles" icicle-options-menu-map))
           (define-key icicle-options-menu-map [icicle-set-option-to-t]
             '(menu-item "+ Turn On Any Option..." icicle-set-option-to-t
               :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-reset-option-to-nil]
             '(menu-item "+ Turn Off Any Option..." icicle-reset-option-to-nil
               :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-toggle-option]
             '(menu-item "+ Toggle Any Option..." icicle-toggle-option :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-separator-options-general] '("--"))
           (define-key icicle-options-menu-map [icicle-toggle-search-cleanup]
             '(menu-item "Toggle Removal of Search Highlighting"
               icicle-toggle-search-cleanup :visible icicle-mode :keys "C-."))
           (define-key icicle-options-menu-map [icicle-toggle-search-replace-common-match]
             '(menu-item "Toggle Replacing Longest Common Match"
               icicle-toggle-search-replace-common-match :visible icicle-mode
               :enable icicle-searching-p :keys "M-;"))
           (define-key icicle-options-menu-map [icicle-toggle-search-replace-whole]
             '(menu-item "Toggle Replacing Whole Search Hit"
               icicle-toggle-search-replace-whole :visible icicle-mode
               :enable icicle-searching-p :keys "C-,"))
           (define-key icicle-options-menu-map [icicle-toggle-highlight-all-current]
             '(menu-item "Toggle All-Current Search Highlighting"
               icicle-toggle-highlight-all-current :visible icicle-mode
               :enable icicle-searching-p :keys "C-^"))
           (define-key icicle-options-menu-map [icicle-separator-options-search] '("--"))
           (define-key icicle-options-menu-map [icicle-toggle-regexp-quote]
             '(menu-item "Toggle Escaping Special Chars" icicle-toggle-regexp-quote
               :visible icicle-mode :keys "C-`"))
           (define-key icicle-options-menu-map [icicle-toggle-fuzzy-completion]
             '(menu-item "Toggle Fuzzy Prefix Completion"
               icicle-toggle-fuzzy-completion :visible icicle-mode :keys "C-("))
           (define-key icicle-options-menu-map [icicle-toggle-incremental-completion]
             '(menu-item "Toggle Incremental Completion"
               icicle-toggle-incremental-completion :visible icicle-mode :keys "C-#"))
           (define-key icicle-options-menu-map [icicle-toggle-expand-to-common-match]
             '(menu-item "Toggle Longest Common Match"
               icicle-toggle-expand-to-common-match :visible icicle-mode :keys "C-;"))
           (define-key icicle-options-menu-map [icicle-toggle-ignored-space-prefix]
             '(menu-item "Toggle Ignoring Space Prefix" icicle-toggle-ignored-space-prefix
               :visible icicle-mode :keys "M-_"))
           (define-key icicle-options-menu-map [icicle-toggle-ignored-extensions]
             '(menu-item "Toggle Ignored File Extensions" icicle-toggle-ignored-extensions
               :visible icicle-mode :keys "C-."))
           (define-key icicle-options-menu-map [icicle-toggle-remote-file-testing]
             '(menu-item "Toggle Remote File Handling" icicle-toggle-remote-file-testing
               :visible icicle-mode :enable (not icicle-searching-p) :keys "C-^"))
           (define-key icicle-options-menu-map [icicle-toggle-angle-brackets]
             '(menu-item "Toggle Angle Brackets" icicle-toggle-angle-brackets
               :visible icicle-mode :keys "C-<"))
           (define-key icicle-options-menu-map [icicle-toggle-highlight-historical-candidates]
             '(menu-item "Toggle Highlighting Past Inputs"
               icicle-toggle-highlight-historical-candidates :visible icicle-mode :keys "C-pause"))
           (define-key icicle-options-menu-map [icicle-toggle-case-sensitivity]
             '(menu-item "Toggle Case Sensitivity" icicle-toggle-case-sensitivity
               :visible icicle-mode :keys "C-A"))
           (define-key icicle-options-menu-map [icicle-toggle-proxy-candidates]
             '(menu-item "Toggle Including Proxy Candidates" icicle-toggle-proxy-candidates
               :visible icicle-mode :keys "C-M-_"))
           (define-key icicle-options-menu-map [icicle-toggle-transforming]
             '(menu-item "Toggle Duplicate Removal" icicle-toggle-transforming
               :visible icicle-mode :keys "C-$"))
           (define-key icicle-options-menu-map [icicle-toggle-C-for-actions]
             '(menu-item "Toggle Using `C-' for Actions" icicle-toggle-C-for-actions
               :visible icicle-mode :keys "M-g"))
           (define-key icicle-options-menu-map [icicle-toggle-~-for-home-dir]
             '(menu-item "Toggle Using `~' for $HOME" icicle-toggle-~-for-home-dir
               :visible icicle-mode :keys "M-~"))
           (define-key icicle-options-menu-map [icicle-toggle-WYSIWYG-Completions]
             '(menu-item "Toggle WYSIWYG For *Completions*" icicle-toggle-WYSIWYG-Completions
               :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-next-apropos-match-function]
             '(menu-item "Change Apropos Match Function" icicle-next-apropos-match-function
               :visible icicle-mode :keys "M-("))
           (define-key icicle-options-menu-map [icicle-separator-options-sort] '("--"))
           (define-key icicle-options-menu-map [icicle-toggle-alternative-sorting]
             '(menu-item "Swap Alternative Sort" icicle-toggle-alternative-sorting
               :visible icicle-mode :keys "C-M-,"))
           (define-key icicle-options-menu-map [icicle-change-alternative-sort-order]
             '(menu-item "Change Alternative Sort Order"
               icicle-change-alternative-sort-order :visible icicle-mode :keys "M-,"))
           (define-key icicle-options-menu-map [icicle-change-sort-order]
             '(menu-item "Change Sort Order" icicle-change-sort-order :visible icicle-mode
               :enable (not icicle-inhibit-sort-p) :keys "C-,"))
           (when (fboundp 'doremi)
             (define-key icicle-options-menu-map [icicle-separator-options-doremi] '("--"))
             (define-key icicle-options-menu-map [icicle-doremi-inter-candidates-min-spaces]
               '(menu-item "Inter-Candidate Spacing - Do Re Mi"
                 icicle-doremi-inter-candidates-min-spaces :visible icicle-mode :keys "C-x |"))
             (define-key icicle-options-menu-map [icicle-doremi-candidate-width-factor]
               '(menu-item "Candidate Column Width - Do Re Mi"
                 icicle-doremi-candidate-width-factor :visible icicle-mode :keys "C-x w"))))
          (t
           (define-key icicle-menu-map [icicle-set-option-to-t]
             '(menu-item "+ Turn On Any Option..." icicle-set-option-to-t))
           (define-key icicle-menu-map [icicle-reset-option-to-nil]
             '(menu-item "+ Turn Off Any Option..." icicle-reset-option-to-nil))
           (define-key icicle-menu-map [icicle-toggle-option]
             '(menu-item "+ Toggle Any Option..." icicle-toggle-option))
           (define-key icicle-menu-map [icicle-toggle-C-for-actions]
             '(menu-item "Toggle Using `C-' for Actions" icicle-toggle-C-for-actions :keys "M-g"))
           (define-key icicle-menu-map [icicle-toggle-~-for-home-dir]
             '(menu-item "Toggle Using `~' for $HOME" icicle-toggle-~-for-home-dir :keys "M-~"))
           (define-key icicle-menu-map [icicle-next-apropos-match-function]
             '(menu-item "Change Apropos Match Function" icicle-next-apropos-match-function
               :keys "M-("))
           (define-key icicle-menu-map [icicle-toggle-WYSIWYG-Completions]
             '(menu-item "Toggle WYSIWYG For *Completions*" icicle-toggle-WYSIWYG-Completions))
           (define-key icicle-menu-map [icicle-toggle-search-cleanup]
             '(menu-item "Toggle Removal of Search Highlighting" icicle-toggle-search-cleanup
               :keys "C-."))
           (define-key icicle-menu-map [icicle-toggle-search-replace-common-match]
             '(menu-item "Toggle Replacing Longest Common Match"
               icicle-toggle-search-replace-common-match :enable icicle-searching-p :keys "M-;"))
           (define-key icicle-menu-map [icicle-toggle-search-replace-whole]
             '(menu-item "Toggle Replacing Whole Search Hit" icicle-toggle-search-replace-whole
               :enable icicle-searching-p :keys "C-,"))
           (define-key icicle-menu-map [icicle-toggle-highlight-all-current]
             '(menu-item "Toggle All-Current Search Highlighting"
               icicle-toggle-highlight-all-current :enable icicle-searching-p :keys "C-^"))
           (define-key icicle-menu-map [icicle-toggle-regexp-quote]
             '(menu-item "Toggle Escaping Special Chars" icicle-toggle-regexp-quote :keys "C-`"))
           (define-key icicle-menu-map [icicle-toggle-fuzzy-completion]
             '(menu-item "Toggle Fuzzy Prefix Completion" icicle-toggle-fuzzy-completion
               :keys "C-("))
           (define-key icicle-menu-map [icicle-toggle-incremental-completion]
             '(menu-item "Toggle Incremental Completion" icicle-toggle-incremental-completion
               :keys "C-#"))
           (define-key icicle-menu-map [icicle-toggle-expand-to-common-match]
             '(menu-item "Toggle Longest Common Match" icicle-toggle-expand-to-common-match
               :keys "C-;"))
           (define-key icicle-menu-map [icicle-toggle-ignored-space-prefix]
             '(menu-item "Toggle Ignoring Space Prefix" icicle-toggle-ignored-space-prefix
               :keys "M-_"))
           (define-key icicle-menu-map [icicle-toggle-ignored-extensions]
             '(menu-item "Toggle Ignored File Extensions" icicle-toggle-ignored-extensions
               :keys "C-."))
           (define-key icicle-menu-map [icicle-toggle-remote-file-testing]
             '(menu-item "Toggle Remote File Handling" icicle-toggle-remote-file-testing
               :enable (not icicle-searching-p) :keys "C-^"))
           (define-key icicle-menu-map [icicle-toggle-angle-brackets]
             '(menu-item "Toggle Angle Brackets" icicle-toggle-angle-brackets :keys "C-<"))
           (define-key icicle-menu-map [icicle-toggle-highlight-historical-candidates]
             '(menu-item "Toggle Highlighting Past Inputs"
               icicle-toggle-highlight-historical-candidates :keys "C-pause"))
           (define-key icicle-menu-map [icicle-toggle-case-sensitivity]
             '(menu-item "Toggle Case Sensitivity" icicle-toggle-case-sensitivity :keys "C-A"))
           (define-key icicle-menu-map [icicle-toggle-proxy-candidates]
             '(menu-item "Toggle Including Proxy Candidates" icicle-toggle-proxy-candidates
               :keys "C-M-_"))
           (define-key icicle-menu-map [icicle-toggle-transforming]
             '(menu-item "Toggle Duplicate Removal" icicle-toggle-transforming :keys "C-$"))
           (define-key icicle-menu-map [icicle-toggle-alternative-sorting]
             '(menu-item "Swap Alternative Sort" icicle-toggle-alternative-sorting :keys "C-M-,"))
           (define-key icicle-menu-map [icicle-change-alternative-sort-order]
             '(menu-item "Change Alternative Sort Order" icicle-change-alternative-sort-order
               :keys "M-,"))
           (define-key icicle-menu-map [icicle-change-sort-order]
             '(menu-item "Change Sort Order" icicle-change-sort-order
               :enable (not icicle-inhibit-sort-p) :keys "C-,"))
           (when (fboundp 'doremi)
             (define-key icicle-menu-map [icicle-doremi-inter-candidates-min-spaces]
               '(menu-item "Inter-Candidate Space - Do Re Mi"
                 icicle-doremi-inter-candidates-min-spaces :visible icicle-mode :keys "C-x |"))
             (define-key icicle-menu-map [icicle-doremi-candidate-width-factor]
               '(menu-item "Candidate Column Width - Do Re Mi"
                 icicle-doremi-candidate-width-factor :visible icicle-mode :keys "C-x w")))
           (define-key icicle-menu-map [icicle-separator-toggle] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-custom-menu)) ; Use Customize menu, if available.
           (defvar icicle-custom-menu-map (make-sparse-keymap)
             "Icicles submenu for Customize menu.")
           (define-key menu-bar-custom-menu [icicles] (cons "Icicles" icicle-custom-menu-map))
           (define-key icicle-custom-menu-map [icicle-customize-apropos-groups]
             '(menu-item "Groups Matching Regexp..." icicle-customize-apropos-groups
               :visible icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-apropos-faces]
             '(menu-item "Faces Matching Regexp..." icicle-customize-apropos-faces
               :visible icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-face]
             '(menu-item "+ Face..." icicle-customize-face :visible icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-apropos-options]
             '(menu-item "Options Matching Regexp..." icicle-customize-apropos-options
               :visible icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-apropos]
             '(menu-item "Settings Matching Regexp..." icicle-customize-apropos
               :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-separator-customize] '("--"))
           (define-key icicle-menu-map [icicle-customize-apropos-groups]
             '(menu-item "Groups Matching Regexp..." icicle-customize-apropos-groups))
           (define-key icicle-menu-map [icicle-customize-apropos-faces]
             '(menu-item "Faces Matching Regexp..." icicle-customize-apropos-faces))
           (define-key icicle-menu-map [icicle-customize-face]
             '(menu-item "+ Face..." icicle-customize-face))
           (define-key icicle-menu-map [icicle-customize-apropos-options]
             '(menu-item "Options Matching Regexp..." icicle-customize-apropos-options))
           (define-key icicle-menu-map [icicle-customize-apropos]
             '(menu-item "Settings Matching Regexp..." icicle-customize-apropos))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-edit-menu)) ; Use Edit menu, if available.
           (defvar icicle-edit-menu-map (make-sparse-keymap)
             "Icicles submenu for Edit menu.")
           (define-key menu-bar-edit-menu [icicles] (cons "Icicles" icicle-edit-menu-map))
           (define-key icicle-edit-menu-map [icicle-complete-thesaurus-entry]
             '(menu-item "Complete with Thesaurus..." icicle-complete-thesaurus-entry
               :visible icicle-mode
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-edit-menu-map [icicle-insert-thesaurus-entry]
             '(menu-item "+ Insert Thesaurus Entry..." icicle-insert-thesaurus-entry
               :visible icicle-mode
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-edit-menu-map [icicle-insert-kill]
             '(menu-item "+ Paste Copied Text..." icicle-insert-kill :visible icicle-mode
               :enable (not buffer-read-only) :keys "C-- C-y")))
          (t
           (define-key icicle-menu-map [icicle-separator-edit] '("--"))
           (define-key icicle-menu-map [icicle-complete-thesaurus-entry]
             '(menu-item "Complete with Thesaurus..." icicle-complete-thesaurus-entry
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-menu-map [icicle-insert-thesaurus-entry]
             '(menu-item "+ Insert Thesaurus Entry..." icicle-insert-thesaurus-entry
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-menu-map [icicle-insert-kill]
             '(menu-item "+ Paste Copied Text..." icicle-insert-kill
               :enable (not buffer-read-only) :keys "C-- C-y"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-file-menu)) ; Use File menu, if available.
           (defvar icicle-file-menu-map (make-sparse-keymap)
             "Icicles submenu for File menu.")
           (define-key menu-bar-file-menu [icicles] (cons "Icicles" icicle-file-menu-map))
           (define-key icicle-file-menu-map [icicle-kill-buffer]
             '(menu-item "+ Kill Buffer..." icicle-kill-buffer :visible icicle-mode :keys "C-x k"))
           (define-key icicle-file-menu-map [icicle-delete-file]
             '(menu-item "+ Delete File..." icicle-delete-file :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (when (condition-case nil (require 'recentf) (error nil))
             (define-key icicle-file-menu-map [icicle-recent-file-other-window]
               '(menu-item "+ Open Recent File (Other Window)..."
                 icicle-recent-file-other-window :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-file-menu-map [icicle-recent-file]
               '(menu-item "+ Open Recent File..." icicle-recent-file :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-file-menu-map [icicle-dired-saved-file-candidates-other-window]
             '(menu-item "Open Dired for Saved Completion Candidates..."
               icicle-dired-saved-file-candidates-other-window :visible icicle-mode
               :enable (and icicle-saved-completion-candidates
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-file-menu-map [icicle-dired-project-other-window]
             '(menu-item "Open Dired for Project..." icicle-dired-project-other-window
               :visible icicle-mode
               :enable (and icicle-saved-completion-sets
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-file-menu-map [icicle-locate-file-other-window]
             '(menu-item "+ Open File Under Directory (Other Window)..."
               icicle-locate-file-other-window :visible icicle-mode))
           (define-key icicle-file-menu-map [icicle-locate-file]
             '(menu-item "+ Open File Under Directory..." icicle-locate-file
               :visible icicle-mode))
           (define-key icicle-file-menu-map [icicle-file-other-window]
             '(menu-item "+ Open File or Directory (Other Window)..."
               icicle-file-other-window :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-file-menu-map [icicle-file]
             '(menu-item "+ Open File or Directory..." icicle-file :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-kill-buffer]
             '(menu-item "+ Kill Buffer..." icicle-kill-buffer))
           (define-key icicle-menu-map [icicle-delete-file]
             '(menu-item "+ Delete File..." icicle-delete-file
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (when (condition-case nil (require 'recentf) (error nil))
             (define-key icicle-menu-map [icicle-recent-file-other-window]
               '(menu-item "+ Open Recent File (Other Window)..." icicle-recent-file-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-recent-file]
               '(menu-item "+ Open Recent File..." icicle-recent-file
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-dired-saved-file-candidates-other-window]
             '(menu-item "Open Dired for Saved Completion Candidates..."
               icicle-dired-saved-file-candidates-other-window
               :enable (and icicle-saved-completion-candidates
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-dired-project-other-window]
             '(menu-item "Open Dired for Project..." icicle-dired-project-other-window
               :visible icicle-mode
               :enable (and icicle-saved-completion-sets
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-locate-file-other-window]
             '(menu-item "+ Open File Under Directory (Other Window)..."
               icicle-locate-file-other-window))
           (define-key icicle-menu-map [icicle-locate-file]
             '(menu-item "+ Open File Under Directory..." icicle-locate-file))
           (define-key icicle-menu-map [icicle-file-other-window]
             '(menu-item "+ Open File or Directory (Other Window)..."
               icicle-file-other-window
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-file]
             '(menu-item "+ Open File or Directory ..." icicle-file
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))))
    (define-key icicle-menu-map [icicle-buffer-list]
      '(menu-item "+ Buffer List..." icicle-buffer-list))
    (define-key icicle-menu-map [icicle-remove-buffer-config]
      '(menu-item "+ Remove Buffer Configuration..." icicle-remove-buffer-config
        :enable icicle-buffer-configs))
    (define-key icicle-menu-map [icicle-add-buffer-config]
      '(menu-item "New Buffer Configuration..." icicle-add-buffer-config))
    (define-key icicle-menu-map [icicle-buffer-config]
      '(menu-item "+ Choose Buffer Configuration..." icicle-buffer-config
        :enable icicle-buffer-configs))
    (define-key icicle-menu-map [icicle-remove-buffer-candidate]
      '(menu-item "+ Don't Always Include Buffer..." icicle-remove-buffer-candidate
        :enable icicle-buffer-extras))
    (define-key icicle-menu-map [icicle-add-buffer-candidate]
      '(menu-item "+ Always Include Buffer..." icicle-add-buffer-candidate))
    (define-key icicle-menu-map [icicle-kill-buffer]
      '(menu-item "+ Kill Buffer..." icicle-kill-buffer))
    (define-key icicle-menu-map [icicle-delete-windows]
      '(menu-item "+ Delete Windows on Buffer..." icicle-delete-windows :keys "C-u C-x 0"))
    (define-key icicle-menu-map [icicle-buffer-other-window]
      '(menu-item "+ Switch to Buffer (Other Window)..." icicle-buffer-other-window
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-buffer]
      '(menu-item "+ Switch to Buffer..." icicle-buffer
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-bookmark-map)) ; Use Bookmarks menu, if available.
           (require 'bookmark)          ; `bookmark-buffer-name' is not autoloaded.
           (defvar icicle-bookmark-menu-map (make-sparse-keymap)
             "Icicles submenu for Bookmarks menu.")
           (define-key menu-bar-bookmark-map [icicles] (cons "Icicles" icicle-bookmark-menu-map))
           (define-key icicle-bookmark-menu-map [icicle-goto-global-marker]
             '(menu-item "+ Go To Global Marker..." icicle-goto-global-marker
               :visible icicle-mode
               :enable (consp (icicle-markers global-mark-ring)) :keys "C-- C-x C-SPC"))
           (define-key icicle-bookmark-menu-map [icicle-goto-marker]
             '(menu-item "+ Go To Marker..." icicle-goto-marker
               :visible icicle-mode
               :enable (consp (icicle-markers mark-ring)) :keys "C-- C-SPC"))
           (define-key icicle-bookmark-menu-map [icicle-separator-goto] '("--"))
           (define-key icicle-bookmark-menu-map [icicle-bookmark-other-window]
             '(menu-item "+ Jump to Bookmark (Other Window)..." icicle-bookmark-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-bookmark-menu-map [icicle-bookmark]
             '(menu-item "+ Jump to Bookmark..." icicle-bookmark :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-bookmark-other-window]
             '(menu-item "+ Jump To Bookmark (Other Window)..." icicle-bookmark-other-window
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-bookmark]
             '(menu-item "+ Jump To Bookmark..." icicle-bookmark
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-separator-bookmark-buffer] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-search-tags-menu)) ; Use Tags menu, if available - `menu-bar+.el'
           (defvar icicle-search-tags-menu-map (make-sparse-keymap)
             "Icicles submenu for Tags submenu of Search menu.")
           (define-key menu-bar-search-tags-menu [icicles]
             (cons "Icicles" icicle-search-tags-menu-map))
           (define-key icicle-search-tags-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-pop-tag-mark]
             '(menu-item "+ Back (Pop Tag Mark)" icicle-pop-tag-mark :visible icicle-mode
               :enable (and (boundp 'find-tag-marker-ring)
                        (not (ring-empty-p find-tag-marker-ring))
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-search-tags-menu-map [icicle-find-first-tag-other-window]
             '(menu-item "+ Find First Tag ..." icicle-find-first-tag-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-find-tag]
             '(menu-item "+ Find Tag ..." icicle-find-tag :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          ((and (not icicle-touche-pas-aux-menus-flag) ; Use Search, if available and no Tags menu.
                (boundp 'menu-bar-search-menu))
           (defvar icicle-search-menu-map (make-sparse-keymap) "Icicles submenu for Search menu.")
           (define-key menu-bar-search-menu [icicles] (cons "Icicles" icicle-search-menu-map))
           (defvar icicle-search-tags-menu-map (make-sparse-keymap)
             "Icicles submenu for Tags submenu of Search menu.")
           (define-key icicle-search-menu-map [icicles-tags]
             (cons "Tags" icicle-search-tags-menu-map))
           (define-key icicle-search-tags-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-pop-tag-mark]
             '(menu-item "+ Back (Pop Tag Mark)" icicle-pop-tag-mark :visible icicle-mode
               :enable (and (boundp 'find-tag-marker-ring)
                        (not (ring-empty-p find-tag-marker-ring))
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-search-tags-menu-map [icicle-find-first-tag-other-window]
             '(menu-item "+ Find First Tag ..." icicle-find-first-tag-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-find-tag]
             '(menu-item "+ Find Tag ..." icicle-find-tag :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-search-tags-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-pop-tag-mark]
             '(menu-item "+ Back (Pop Tag Mark)" icicle-pop-tag-mark :visible icicle-mode
               :enable (and (boundp 'find-tag-marker-ring)
                        (not (ring-empty-p find-tag-marker-ring))
                        (not (window-minibuffer-p
                              (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-find-first-tag-other-window]
             '(menu-item "Find First Tag ..." icicle-find-first-tag-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-find-tag]
             '(menu-item "Find Tag ..." icicle-find-tag :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-search-menu)) ; Use Search menu, if available.
           (defvar icicle-search-menu-map (make-sparse-keymap) "Icicles submenu for Search menu.")
           (define-key menu-bar-search-menu [icicles] (cons "Icicles" icicle-search-menu-map))
           (define-key icicle-search-menu-map [icicle-goto-global-marker]
             '(menu-item "+ Go To Global Marker..." icicle-goto-global-marker
               :visible icicle-mode
               :enable (consp (icicle-markers global-mark-ring)) :keys "C-- C-x C-SPC"))
           (define-key icicle-search-menu-map [icicle-goto-marker]
             '(menu-item "+ Go To Marker..." icicle-goto-marker
               :visible icicle-mode
               :enable (consp (icicle-markers mark-ring)) :keys "C-- C-SPC"))
           (define-key icicle-search-menu-map [icicle-separator-goto] '("--"))
           (define-key icicle-search-menu-map [icicle-search-highlight-cleanup]
             '(menu-item "Remove Search Highlighting..." icicle-search-highlight-cleanup
               :visible icicle-mode
               :enable (or icicle-search-overlays (overlayp icicle-search-current-overlay)
                        (overlayp icicle-search-refined-overlays) icicle-search-refined-overlays)))
           (define-key icicle-search-menu-map [icicle-compilation-search]
             '(menu-item "+ Search Compilation/Grep Hits (Regexp)..."
               icicle-compilation-search :visible icicle-mode
               :enable (and (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
                        (condition-case nil (eq (current-buffer) (compilation-find-buffer))
                          (error nil)))
               :keys "C-`"))
           (define-key icicle-search-menu-map [icicle-grep-saved-file-candidates]
             '(menu-item "Grep Saved File-Name Candidates..."
               icicle-grep-saved-file-candidates
               :visible icicle-mode :enable icicle-saved-completion-candidates))
           (define-key icicle-search-menu-map [icicle-imenu-non-interactive-function]
             '(menu-item "+ Search Non-Command Fn Definition (Regexp)..."
               icicle-imenu-non-interactive-function
               :visible icicle-mode :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-search-menu-map [icicle-imenu-command]
             '(menu-item "+ Search Command Definition (Regexp)..." icicle-imenu-command
               :visible icicle-mode :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-search-menu-map [icicle-imenu]
             '(menu-item "+ Search Definition (Regexp)..." icicle-imenu
               :visible icicle-mode :enable imenu-generic-expression))
           (define-key icicle-search-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-all-regions]
             '(menu-item "+ Search All Saved Regions (Regexp)..." icicle-search-all-regions
               :visible icicle-mode :keys "C-u C-`"))
           (define-key icicle-search-menu-map [icicle-search-region]
             '(menu-item "+ Search Saved Region (Regexp)..." icicle-search-region
               :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-file]
             '(menu-item "+ Search File (Regexp)..." icicle-search-file
               :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-buffer]
             '(menu-item "+ Search Buffer (Regexp)..." icicle-search-buffer
               :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-text-property]
             '(menu-item "+ Search Text Property..." icicle-search-text-property
               :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-word]
             '(menu-item "+ Search for Word..." icicle-search-word :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-menu-map [icicle-search-keywords]
             '(menu-item "+ Search with Keywords (Regexps)..." icicle-search-keywords
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-menu-map [icicle-search]
             '(menu-item "+ Search (Regexp)..." icicle-search :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
               :keys "C-`"))
           (define-key icicle-search-menu-map [icicle-occur]
             '(menu-item "+ Occur (Regexp)..." icicle-occur :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-search-highlight-cleanup]
             '(menu-item "Remove Search Highlighting..." icicle-search-highlight-cleanup
               :enable (or icicle-search-overlays (overlayp icicle-search-current-overlay)
                        (overlayp icicle-search-refined-overlays)
                        icicle-search-refined-overlays)))
           (define-key icicle-menu-map [icicle-compilation-search]
             '(menu-item "+ Search Compilation/Grep Hits (Regexp)..." icicle-compilation-search
               :enable (and (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
                        (condition-case nil (eq (current-buffer) (compilation-find-buffer))
                          (error nil)))))
           (define-key icicle-menu-map [icicle-grep-saved-file-candidates]
             '(menu-item "Grep Saved File-Name Candidates..."
               icicle-grep-saved-file-candidates :enable icicle-saved-completion-candidates))
           (define-key icicle-menu-map [icicle-imenu-non-interactive-function]
             '(menu-item "Search Non-Command Fn Definition (Regexp)..."
               icicle-imenu-non-interactive-function :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-menu-map [icicle-imenu-command]
             '(menu-item "Search Command Definition (Regexp)..." icicle-imenu-command
               :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-menu-map [icicle-imenu]
             '(menu-item "+ Search Definition (Regexp)..." icicle-imenu
               :enable imenu-generic-expression))
           (define-key icicle-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search))
           (define-key icicle-menu-map [icicle-search-all-regions]
             '(menu-item "+ Search All Saved Regions (Regexp)..." icicle-search-all-regions))
           (define-key icicle-menu-map [icicle-search-region]
             '(menu-item "+ Search Saved Region (Regexp)..." icicle-search-region))
           (define-key icicle-menu-map [icicle-search-file]
             '(menu-item "+ Search File (Regexp)..." icicle-search-file))
           (define-key icicle-menu-map [icicle-search-buffer]
             '(menu-item "+ Search Buffer (Regexp)..." icicle-search-buffer))
           (define-key icicle-menu-map [icicle-search-text-property]
             '(menu-item "+ Search Text Property..." icicle-search-text-property))
           (define-key icicle-menu-map [icicle-search-word]
             '(menu-item "+ Search for Word..." icicle-search-word
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-search-keywords]
             '(menu-item "+ Search with Keywords (Regexps)..." icicle-search-keywords
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-search]
             '(menu-item "+ Search (Regexp)..." icicle-search
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-occur]
             '(menu-item "+ Occur (Regexp)..." icicle-occur
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))))

  ;; Install Icicles menu-bar menu.
  (define-key icicle-mode-map [menu-bar icicles] (cons "Icicles" icicle-menu-map))

  ;; Optional `icicle-mode-map' bindings - governed by `icicle-top-level-key-bindings'.
  (icicle-bind-top-level-commands)

  ;; Bind `S-TAB' in `icicle-mode-map', for generic `S-TAB'.  Emacs 22+.
  (when (fboundp 'map-keymap)
    (dolist (key icicle-generic-S-tab-keys)
      (define-key icicle-mode-map key 'icicle-generic-S-tab))) ; `S-TAB'

  ;; Install or update `icicle-mode-map'.
  (if icicle-minor-mode-map-entry
      (setcdr icicle-minor-mode-map-entry icicle-mode-map)
    (setq icicle-minor-mode-map-entry (cons 'icicle-mode icicle-mode-map))
    (add-to-list 'minor-mode-map-alist icicle-minor-mode-map-entry)))

(defun icicle-update-help-string ()
  "Update the bindings within the Icicles completion help string."
  (setq icicle-completion-help-string
        (icicle-S-iso-lefttab-to-S-TAB
         (substitute-command-keys
          (format "\\<minibuffer-local-completion-map> 
                     Icicles Minibuffer Completion
                     -----------------------------

Minibuffer input can be completed in several ways.
These are the main Icicles actions and their minibuffer key bindings:

 * Show this help.                           \\[icicle-completion-help]
     For help on individual completion candidates, see \"Show help on
     individual completion candidates\", below.

 * Abandon or commit your input.
     Abandon input                           \\[icicle-abort-recursive-edit]
     Commit input to Emacs                   RET
       Complete partial input, then commit   \\<minibuffer-local-must-match-map>\
\\[icicle-apropos-complete-and-exit]\\<minibuffer-local-completion-map>

 * Toggle Icicles options on the fly.        Key:   \tCurrently:
     Highlighting of past inputs             \\[icicle-toggle-highlight-historical-candidates]\t%S
     Removal of duplicate candidates         \\[icicle-toggle-transforming]\t%S
     Change sort order                       \\[icicle-dispatch-C-comma]\t%s
     Change alternative sort order           \\[icicle-dispatch-M-comma]\t%s
     Swap alternative sort                   \\[icicle-toggle-alternative-sorting]\t- (swaps) -
     Case sensitivity                        \\[icicle-toggle-case-sensitivity]\t%S
     Escaping of special regexp chars        \\[icicle-toggle-regexp-quote]\t%S
     Incremental completion                  \\[icicle-toggle-incremental-completion]\t%S
     Input expansion to common match         \\[icicle-toggle-expand-to-common-match]\t%S
     Change apropos match function           \\[icicle-next-apropos-match-function]\t%s
     Fuzzy prefix completion                 \\[icicle-toggle-fuzzy-completion]\t%S
     Inclusion of proxy candidates           \\[icicle-toggle-proxy-candidates]\t%S
     Angle brackets for key names            \\[icicle-toggle-angle-brackets]\t%S
     Ignoring certain file extensions        \\[icicle-dispatch-C-.]\t%S
     Checking for remote file names          \\[icicle-dispatch-C-^]\t%S
     Ignoring space prefix                   \\[icicle-toggle-ignored-space-prefix]\t%S
     Using `C-' for multi-command actions    \\[icicle-toggle-C-for-actions]\t%S
     Using `~' for your home directory       \\[icicle-toggle-~-for-home-dir]\t%S
     `icicle-search' all-current highlights  \\[icicle-dispatch-C-^]\t%S
     Whole-word searching                    \\[icicle-dispatch-M-q]\t%S
     Removal of `icicle-search' highlighting \\[icicle-dispatch-C-.]\t%S
     Replacement of whole search hit         \\[icicle-dispatch-C-comma]\t%S
     Replacement of expanded common match    \\[icicle-toggle-search-replace-common-match]\t%S

 * Change the set of completion candidates.  Modify your input.
     Edit your input                         (just edit in minibuffer)
     Erase your input (clear minibuffer)     \\[icicle-erase-minibuffer-or-history-element]
     Erase input portion that doesn't match  \\[icicle-kill-failed-input]
     Retrieve previous completion inputs     \\[icicle-retrieve-previous-input], \
\\[icicle-retrieve-next-input]
     Match another regexp (chaining)         \\[icicle-narrow-candidates]
     Satisfy another predicate (chaining)    \\[icicle-narrow-candidates-with-predicate]
     Remove a candidate from set of matches  delete, S-mouse-2
     Yank text at cursor into minibuffer     \\[icicle-insert-string-at-point]
     Insert text (string) from a variable    \\[icicle-insert-string-from-variable]
     Insert `icicle-list-join-string'        \\[icicle-insert-list-join-string]
     Insert previously entered input         \\[icicle-insert-history-element]
     Insert key description (key completion) \\[icicle-dispatch-M-q]

 * Complete your current input in the minibuffer.
     Apropos (regexp) completion             \\[icicle-apropos-complete]
       Without displaying candidates         \\[icicle-prefix-complete-no-display]
       Complete and match another regexp     \\[icicle-apropos-complete-and-narrow]
     Prefix completion
       As much as possible                   \\[icicle-prefix-complete]
         Without displaying candidates       \\[icicle-prefix-complete-no-display]
       A word at a time                      \\[icicle-prefix-word-complete]
     Complete and commit (if required match) \\<minibuffer-local-must-match-map>\
\\[icicle-apropos-complete-and-exit]\\<minibuffer-local-completion-map>
     Complete search string using past input \\[icicle-apropos-complete]

 * Display/navigate completions for current input (in *Completions*).
     Show completion candidates
       Prefix completion                     \\[icicle-prefix-complete] (repeat)
       Apropos completion                    \\[icicle-apropos-complete]
     Move between minibuffer and list        \\<completion-list-mode-map>\
\\[icicle-insert-completion]
     Cycle among completion candidates       right, left, \
\\[icicle-move-to-next-completion], \\[icicle-move-to-previous-completion]
       Within a *Completions* column         down, up
     Choose a completion candidate           \\[choose-completion], \
\\[mouse-choose-completion]\\<minibuffer-local-completion-map>

 * Cycle among input candidates.
     Prefix-completion candidates            down, up
     Apropos-completion candidates           next, prior
     Minibuffer history items                \\[next-history-element], \
\\[previous-history-element]
     Completion history items                \\[icicle-retrieve-previous-input], \
\\[icicle-retrieve-next-input]

 * Show help on individual completion candidates.
     Current candidate                       C-M-RET, C-M-mouse-2
     Next, previous prefix-match candidate   C-M-down, C-M-up
     Next, previous apropos-match candidate  C-M-next, C-M-prior

 * Choose a previous input from the minibuffer history.
     Complete to insert a previous input     \\[icicle-insert-history-element]
     Complete against history items          \\[icicle-history], \
\\[icicle-keep-only-past-inputs]
     Restrict candidates to history items    \\[icicle-keep-only-past-inputs]
     List history items first in Completions \\[icicle-toggle-alternative-sorting]
     Cycle among minibuffer history items    \\[next-history-element], \
\\[previous-history-element]
     Search among minibuffer history items   \
\\[next-matching-history-element], \\[previous-matching-history-element]

 * Delete history entries
     Delete current entry (cycling)          \\[icicle-erase-minibuffer-or-history-element]
     Delete any or all entries               \\[icicle-clear-current-history]

 * Multi-commands: Act on completion candidates.
   For alternative action, use `C-S-' instead of `C-', but
   `C-|' and `M-|' are alternative action versions of `C-!' and `M-!'.
     Current candidate                       C-RET, C-mouse-2
     Next, previous prefix-match candidate   C-down, C-up
     Next, previous apropos-match candidate  C-next, C-prior
     Act on each matching candidate, in turn C-!
     Act on the list of matching candidates  M-!
     Delete object named by candidate        S-delete
     Remove candidate from set of matches    delete, S-mouse-2
     Save candidate (add to those saved)     insert, M-S-mouse-2
     Object-action: apply a fn to candidate  M-RET

 * Search and replace (e.g. `C-`').
     Use alternative action keys (prefix `C-S-') to navigate.
     Toggle input highlighting at all hits   \\[icicle-dispatch-C-^]
     Toggle whole-word searching             \\[icicle-dispatch-M-q]
     Toggle escaping of special regexp chars \\[icicle-toggle-regexp-quote]
     Toggle removal of search highlighting   \\[icicle-dispatch-C-.]

     Replace all                             C-|
     Redefine the replacement string         \\[icicle-dispatch-M-comma]
     Toggle literal replacement              \\[icicle-toggle-literal-replacement]
     Toggle replacement of whole search hit  \\[icicle-dispatch-C-comma]
     Toggle replacement of common match      \\[icicle-toggle-search-replace-common-match]

 * Perform set operations on candidate sets.
     Remove candidate from current set       delete, S-mouse-2
     Add current candidate to saved set      insert, M-S-mouse-2
     Save candidates in current set to...
       `icicle-saved-completion-candidates'  \\[icicle-candidate-set-save]
       another variable                      \\[icicle-candidate-set-save-to-variable]
       a cache file                          \\[icicle-candidate-set-save-persistently]
     Retrieve saved set from...
       `icicle-saved-completion-candidates'  \\[icicle-candidate-set-retrieve]
       another variable                      \\[icicle-candidate-set-retrieve-from-variable]
       a cache file                          \\[icicle-candidate-set-retrieve-persistent]
     Add candidates in current set           \\[icicle-candidate-set-save-more]
     Save, add selected candidates           \\[icicle-candidate-set-save-selected], \
\\[icicle-candidate-set-save-more-selected]  with region
     Clear all saved candidates              \\[icicle-candidate-set-save-selected] \
with empty region
     Add new or update existing saved set
       \\[icicle-add/update-saved-completion-set]
     Remove a saved completion set
       \\[icicle-remove-saved-completion-set]
     Swap current and saved sets             \\[icicle-candidate-set-swap]
     Define current set by evaluating sexp   \\[icicle-candidate-set-define]
     Restrict candidates to history items    \\[icicle-keep-only-past-inputs]
     Set complement                          \\[icicle-candidate-set-complement]
     Set difference                          \\[icicle-candidate-set-difference]
     Set union                               \\[icicle-candidate-set-union]
     Set intersection                        \\[icicle-candidate-set-intersection]
     Set intersection using regexp           \\[icicle-narrow-candidates]
     Set intersection using predicate        \\[icicle-narrow-candidates-with-predicate]
       Save current predicate to a variable  \\[icicle-save-predicate-to-variable]
       Insert string variable as input       \\[icicle-insert-string-from-variable]

 * Adjust Icicles options incrementally on the fly (uses Do Re Mi).
     `icicle-candidate-width-factor'         \\[icicle-doremi-candidate-width-factor]
     `icicle-inter-candidates-min-spaces'    \\[icicle-doremi-inter-candidates-min-spaces]

Remember: You can always input any character (e.g. \\[icicle-prefix-complete]) that is bound
          to a command by preceding it with \\<global-map>\\[quoted-insert].

Though it has no direct connection with completion, you can use \
`\\<minibuffer-local-completion-map>\\[icicle-pp-eval-expression-in-minibuffer]'
in the minibuffer at any time to evaluate an Emacs-Lisp expression.
This calls `icicle-pp-eval-expression-in-minibuffer', which displays
the result in the echo area or a popup buffer, *Pp Eval Output*.
It also provides some of the Emacs-Lisp key bindings during expression
editing.

----------------------------------------------------------------------
 
Customize Icicles: `M-x icicle-customize-icicles-group'.
Summary of customizable options and faces (alphabetical order).

Some of the binary options can be toggled; their toggle keys are
mentioned here.

* `case-fold-search', `completion-ignore-case',
  (`C-u') `read-file-name-completion-ignore-case'
                                         - Case sensitivity? (`C-A')
* `completion-ignored-extensions'        - Ignored filenames (`C-.')
* `icicle-add-buffer-name-flag'          - Add candidate's buffer?
* `icicle-add-proxy-candidates-flag'     - Include proxies? (`C-M-_')
* `icicle-alternative-sort-function'     - Other sort (`M-,', `C-M-,')
* `icicle-top-level-key-bindings'        - Bind top-level commands
* `icicle-buffer-*'                      - `icicle-buffer' options
* `icicle-candidate-width-factor'        - Width %%, candidate columns
* `icicle-change-region-background-flag' - Change region color?
* `icicle-change-sort-order-completion-flag' - Control `C-,' behavior
* `icicle-color-themes'                  - For `icicle-color-theme'
* `icicle-complete-keys-self-insert-flag'- `S-TAB' for self-insert?
* `icicle-Completions-display-min-input-chars' - Remove *Completions*
                                           if fewer chars input
* `icicle-Completions-frame-at-right-flag'- *Completions* at right?
* `icicle-Completions-window-max-height'  - Max lines in *Completions*
* `icicle-cycle-into-subdirs-flag'       - Explore subdirectories?
* `icicle-cycling-respects-completion-mode-flag' - Completion mode
                                           affects cycling mode?
* `icicle-default-thing-insertion'       - Control behavior of \
\\<minibuffer-local-completion-map>\\[icicle-insert-string-at-point]
* `icicle-default-value'                 - How to treat default value
* `icicle-expand-input-to-common-match-flag'- Expand input? (`C-;')
* `icicle-fuzzy-completion-flag'         - Fuzzy completion? (`C-(')
* `icicle-highlight-historical-candidates-flag'
                                         - Highlight past input?
* `icicle-highlight-input-initial-whitespace-flag'
                                         - Highlight input whitespace?
* `icicle-ignore-space-prefix-flag'      - See initial space? (`M-_')
* `icicle-incremental-completion-delay'  - Before update *Completions*
* `icicle-incremental-completion-flag'   - Icompletion? (`C-#')
* `icicle-incremental-completion-threshold'- # of candidates for delay
* `icicle-input-string'                  - String inserted by `C-='
* `icicle-inter-candidates-min-spaces'   - Min spaces among candidates
* `icicle-key-descriptions-use-<>-flag' - Key shown as \"<>\"? (`C-<')
* `icicle-keymaps-for-key-completion'    - `S-TAB' = key-complete maps
* `icicle-kmacro-ring-max'               - Icicles `kmacro-ring-max'
* `icicle-list-end-string', `icicle-list-join-string'
                                         - Multi-completion join/end
* `icicle-list-nth-parts-join-string'    - Join split-candidate parts
* `icicle-mark-position-in-candidate'    - Mark position in cycling
* `icicle-minibuffer-setup-hook'         - Functions run after setup
* `icicle-modal-cycle-up-keys', `icicle-modal-cycle-down-keys'
                                         - Keys for modal cycling
* `icicle-next-apropos-match-function'   - Change match func (`M-(')
* `icicle-point-position-in-candidate'   - Cursor position in cycling
* `icicle-redefine-standard-commands-flag'- Redefine std commands?
* `icicle-regexp-quote-flag'             - Escape chars? (`C-`')
* `icicle-regexp-search-ring-max'        - `regexp-search-ring-max'
* `icicle-region-alist'                  - Alist of saved regions
* `icicle-region-auto-open-files-flag'   - Open saved-region files?
* `icicle-region-background'             - Background for region
* `icicle-region-alist'                  - List of regions
* `icicle-regions-name-length-max'       - # chars to name a region
* `icicle-require-match-flag'            - Override REQUIRE-MATCH?
* `icicle-saved-completion-sets'         - Completion sets for `\\[icicle-candidate-set-retrieve]'
* `icicle-search-cleanup-flag'           - Remove search highlighting?
                                           (`C-.')
* `icicle-search-context-match-predicate'- Search-context predicate
* `icicle-search-highlight-all-current-flag'- In each hit (`C-^')
* `icicle-search-highlight-context-levels-flag' -
                                           Highlight match subgroups?
* `icicle-search-highlight-threshold'    - # hits to highlight at once
* `icicle-search-hook'                   - Functions run by `C-c `'
* `icicle-search-replace-common-match-flag' - Replace ECM? (`M-;')
* `icicle-search-replace-literally-flag' - Replace text literally?
* `icicle-search-replace-whole-candidate-flag' - Replace input match
                                           or whole search hit?(`C-,')
* `icicle-search-ring-max'               - Icicles `search-ring-max'
* `icicle-search-whole-word-flag'        - Find whole words? (`M-q')
* `icicle-show-Completions-help-flag'    - Show *Completions* help?
* `icicle-show-Completions-initially-flag'- Show *Completions* first?
* `icicle-sort-function'                 - Sort candidates (`C-,')
* `icicle-sort-functions-alist'          - Functions for sorting
* `icicle-special-candidate-regexp'      - To highlight special cands
* `icicle-TAB-shows-candidates-flag'     - 1st `TAB' shows candidates?
* `icicle-test-for-remote-files-flag'    - Check remote files? (`C-^')
* `icicle-thing-at-point-functions'      - Functions to yank things
* `icicle-top-level-when-sole-completion-flag' -
                                           Exit if single completion?
* `icicle-touche-pas-aux-menus-flag'     - Add to standard menus?
* `icicle-transform-function'            - Remove duplicates (`C-$')
* `icicle-update-input-hook'             - Fns run when input changes
* `icicle-use-~-for-home-dir-flag'       - Use `~' for $HOME? (`M-~')
* `icicle-use-C-for-actions-flag'        - `C-' for actions? (`M-g')
* `icicle-use-candidates-only-once-flag' - Remove used candidate?
* `icicle-word-completion-keys'          - Keys for word completion
* `icicle-WYSIWYG-Completions-flag'      - WYSIWYG for *Completions*?
* `icicle-yank-function'                 - Yank function to use

Faces that highlight input in minibuffer.

* `icicle-complete-input'               - Input when it is complete
* `icicle-match-highlight-minibuffer'   - Matched part of input
* `icicle-whitespace-highlight'         - Initial whitespace in input

Faces that highlight candidates in buffer *Completions*.

* `icicle-common-match-highlight-Completions' - Max common substring
* `icicle-current-candidate-highlight'  - Highlight cycle candidate
* `icicle-historical-candidate'         - Highlight candidates used
* `icicle-match-highlight-Completions'  - Matched part of input

Faces that highlight for command `icicle-search'.

* `icicle-search-current-input'         - What input matches
* `icicle-search-main-regexp-current'   - Current match of 1st regexp
* `icicle-search-main-regexp-others'    - Other matches of 1st regexp

----------------------------------------------------------------------
 
Some top-level Icicles commands (alphabetical order, with exceptions).
Some are bound in Icicle mode.  Bind the others to keys you like.
See recommended bindings in `icicles.el'.
Multi-commands are indicated by `+': They act any number of times.
You can tell a multi-command when you execute it by the fact that the
input prompt is prefixed by `+'.

+ `clear-option' (alias)               - Set binary option to nil
+ `icicle-add-buffer-candidate'        - To always-candidate buffer
+ `icicle-remove-buffer-candidate'     -   From same
  `icicle-add-buffer-config'           - To `icicle-buffer-configs'
+ `icicle-remove-buffer-config'        -   From same
  `icicle-add-region'                  - To `icicle-region-alist'
+ `icicle-remove-region'               -   From same
  `icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
+ `icicle-remove-saved-completion-set' -   From same
+ `icicle-apply'                       - Apply function to alist items
  `icicle-apropos'                     - `apropos', but shows matches
  `icicle-apropos-command'             - Enhanced `apropos-command'
  `icicle-apropos-variable'            - Enhanced `apropos-variable'
  `icicle-apropos-zippy'               - Show matching Zippy quotes
+ `icicle-bookmark'(`-other-window')   - Jump to bookmark
+ `icicle-buffer'(`-other-window')     - Switch to buffer (`C-x b')
+ `icicle-buffer-config'               - Pick `icicle-buffer' options
+ `icicle-buffer-list'                 - Choose a list of buffer names
  `icicle-change-alternative-sort-order' - Choose an alternative sort
  `icicle-change-sort-order'           - Choose a sort order
+ `icicle-clear-current-history'       - Clear current history entries
+ `icicle-clear-history'               - Clear entries from a history
+ `icicle-color-theme'                 - Change color theme
+ `icicle-comint-command'              - Reuse command (`C-c TAB')
+ `icicle-comint-search'               - Reuse command (`C-c `')
+ `icicle-command-abbrev'              - `M-x' + abbrevs (`C-x SPC')
+ `icicle-compilation-search'          - Show hits (`C-c `')
+ `icicle-complete-keys'               - Complete keys (`S-TAB')
  `icicle-complete-thesaurus-entry'    - Complete word (`C-c /')
  `icicle-completion-help'             - Display this help
+ `icicle-customize-face'              - Multi-`customize-face'
  `icicle-customize-icicles-group'     - Customize options and faces
+ `icicle-delete-file'                 - Delete file/directory
+ `icicle-delete-windows'              - Delete windows (`C-u C-x 0')
+ `icicle-doc'                         - Show doc for fn, var, or face
  `icicle-doremi-candidate-width-factor' - +/- candidate column width
  `icicle-doremi-inter-candidates-min-spaces' - +/- candidate spacing
+ `icicle-execute-extended-command'    - Execute command - multi-`M-x'
+ `icicle-execute-named-keyboard-macro' - Execute named keyboard macro
+ `icicle-file'(`-other-window')       - Visit file/dir (`C-x C-f')
+ `icicle-find-file'(`-other-window')  -         same: relative only
+ `icicle-find-file-absolute'(`-other-window') - same: absolute only
+ `icicle-find-first-tag'(`-other-window') - Find source def (tag)
+ `icicle-font'                        - Change font of frame
+ `icicle-frame-bg'                    - Change background of frame
+ `icicle-frame-fg'                    - Change foreground of frame
+ `icicle-fundoc'                      - Show function description
+ `icicle-goto-global-marker'          - Go to a global marker
+ `icicle-goto-marker'                 - Go to a marker in this buffer
+ `icicle-imenu'                       - Navigate among Imenu entries
+ `icicle-Info-goto-node'              - Multi-cmd `Info-goto-node'
+ `icicle-Info-index'                  - Multi-command `Info-index'
+ `icicle-Info-menu'                   - Multi-command `Info-menu'
+ `icicle-insert-kill'                 - Like `yank', without rotating
+ `icicle-insert-thesaurus-entry'      - Insert thesaurus entry
+ `icicle-kill-buffer'                 - Kill buffer (`C-x k')
+ `icicle-kmacro'                      - Call keyboard macro (`S-f4')
+ `icicle-locate-file'(`-other-window') - Visit file in a directory
  `icy-mode' or `icicle-mode'          - Toggle Icicle mode
  `icicle-next-apropos-match-function' - Change match func (`M-(')
+ `icicle-occur'                       - Enhanced `occur' (`C-c '')
+ `icicle-other-window-or-frame'       - Other window/frame (`C-x o')
+ `icicle-plist'                       - Show symbols, property lists
+ `icicle-recent-file'(`-other-window') - Open recently used file
+ `icicle-reset-option-to-nil'         - Set binary option to nil
  `icicle-save-string-to-variable'     - Save text for use with \
`\\[icicle-insert-string-from-variable]'
+ `icicle-search'                      - Search (`C-c `')
+ `icicle-search-keywords'             - Search for keywords (`C-c ^')
+ `icicle-search-region'               - Search multiple regions
+ `icicle-find-tag'                    - Find definition (tag) (`M-.')
+ `icicle-search-text-property'        - Search for face... (`C-c \"')
+ `icicle-search-word'                 - Search for whole word
+ `icicle-select-frame'                - Select a frame by name
+ `icicle-select-region'               - Select a region
+ `icicle-select-window'               - Select window by buffer name
  `icicle-send-bug-report'             - Send Icicles bug report
+ `icicle-set-option-to-t'             - Set binary option to t
  `icicle-toggle-~-for-home-dir'       - Toggle using `~' for $HOME
  `icicle-toggle-alternative-sorting'  - Swap alternative sort
  `icicle-toggle-angle-brackets'       - Toggle angle brackets
  `icicle-toggle-case-sensitivity'     - Toggle case sensitivity
  `icicle-toggle-C-for-actions'        - Toggle using `C-' for actions
  `icicle-toggle-expand-to-common-match' - Toggle input ECM expansion
  `icicle-toggle-fuzzy-completion'     - Toggle fuzzy completion
  `icicle-toggle-highlight-all-current' - Toggle max search highlight
  `icicle-toggle-highlight-historical-candidates'
                                       - Toggle past-input highlight
  `icicle-toggle-ignored-extensions'   - Toggle ignored files
  `icicle-toggle-ignored-space-prefix' - Toggle ignoring space prefix
  `icicle-toggle-incremental-completion' - Toggle icompletion
+ `icicle-toggle-option'               - Toggle binary user option
  `icicle-toggle-proxy-candidates'     - Toggle proxy candidates
  `icicle-toggle-regexp-quote'         - Toggle regexp escaping
  `icicle-toggle-search-cleanup'       - Toggle highlight removal
  `icicle-toggle-search-replace-common-match' - Toggle ECM replacement
  `icicle-toggle-search-whole-word'    - Toggle whole-word searching
  `icicle-toggle-sorting'              - Toggle sorting
  `icicle-toggle-transforming'         - Toggle duplicate removal
  `icicle-toggle-WYSIWYG-Completions'  - Toggle WYSIWYG *Completions*
+ `icicle-vardoc'                      - Show variable description
+ `icicle-where-is'                    - `where-is' multi-command
  `icicle-yank-insert'                 - `yank' + completion (`C-y')
+ `toggle' (alias)                     - Toggle binary user option

Send an Icicles bug report: `\\[icicle-send-bug-report]'.

----------------------------------------------------------------------
 
These are all of the top-level bindings in Icicle mode:

\\{icicle-mode-map}---------------------------------------\
-------------------------------
 
These are all of the minibuffer bindings during completion:

\\{minibuffer-local-completion-map}---------------------------------------\
-------------------------------
"
                  icicle-highlight-historical-candidates-flag icicle-transform-function
                  (and icicle-sort-function (car (rassoc icicle-sort-function
                                                         icicle-sort-functions-alist)))
                  (and icicle-alternative-sort-function (car (rassoc icicle-sort-function
                                                                     icicle-sort-functions-alist)))
                  (not case-fold-search)                      icicle-regexp-quote-flag
                  icicle-incremental-completion-flag
                  icicle-expand-input-to-common-match-flag
                  (car (rassq icicle-apropos-complete-match-fn icicle-apropos-match-fns-alist))
                  icicle-fuzzy-completion-flag                icicle-add-proxy-candidates-flag
                  icicle-key-descriptions-use-<>-flag         (and completion-ignored-extensions t)
                  icicle-test-for-remote-files-flag           icicle-ignore-space-prefix-flag
                  icicle-use-C-for-actions-flag               icicle-use-~-for-home-dir-flag
                  icicle-search-highlight-all-current-flag    icicle-search-whole-word-flag
                  icicle-search-cleanup-flag                
                  icicle-search-replace-whole-candidate-flag
                  icicle-search-replace-common-match-flag)))))

(defun icicle-S-iso-lefttab-to-S-TAB (strg)
  "Return string STRG, but with \"S-iso-lefttab\" replaced by \"S-TAB\"."
  (replace-regexp-in-string "S-iso-lefttab" "S-TAB" strg))

(defun icicle-rebind-non-completion-keys ()
  "Rebind some keys in maps other than minibuffer maps and `icicle-mode-map'"
  ;; Replace some standard Info bindings.
  (if (not (boundp 'Info-mode-map))
      (eval-after-load "info"
        '(progn
          (icicle-remap 'Info-goto-node  'icicle-Info-goto-node-cmd  Info-mode-map) ; `g'
          (icicle-remap 'Info-index      'icicle-Info-index-cmd      Info-mode-map) ; `i'
          (icicle-remap 'Info-menu       'icicle-Info-menu-cmd       Info-mode-map))) ; `m'
    (icicle-remap 'Info-goto-node  'icicle-Info-goto-node-cmd  Info-mode-map)
    (icicle-remap 'Info-index      'icicle-Info-index-cmd      Info-mode-map)
    (icicle-remap 'Info-menu       'icicle-Info-menu-cmd       Info-mode-map))

  ;; Bind some keys in Dired mode.
  (unless (lookup-key dired-mode-map "\M-s") ; Dired `M-s'
    (define-key dired-mode-map "\M-s" 'icicle-search-dired-marked))
  (unless (lookup-key dired-mode-map [(control meta ?<)]) ; Dired `C-M-<'
    (define-key dired-mode-map [(control meta ?<)]
      'icicle-dired-saved-file-candidates-other-window))
  (unless (lookup-key dired-mode-map [(control ?{)]) ; Dired `C-{'
    (define-key dired-mode-map [(control ?{)] 'icicle-dired-project-other-window))
  (unless (lookup-key dired-mode-map [(control meta ?>)]) ; Dired `C-M->'
    (define-key dired-mode-map [(control meta ?>)] 'icicle-dired-save-marked))
  (unless (lookup-key dired-mode-map [(control ?>)]) ; Dired `C->'
    (define-key dired-mode-map [(control ?>)] 'icicle-dired-save-marked-more))
  (unless (lookup-key dired-mode-map [(control meta ?})]) ; Dired `C-M-}'
    (define-key dired-mode-map [(control meta ?})] 'icicle-dired-save-marked-to-variable))
  (unless (lookup-key dired-mode-map [(control ?})]) ; Dired `C-}'
    (define-key dired-mode-map [(control ?})] 'icicle-dired-save-marked-as-project))

  ;; Bind `S-TAB' in major maps, for key completion.
  (when (fboundp 'map-keymap)           ; Emacs 22.
    (icicle-bind-S-TAB-in-keymaps-from (current-global-map))
    (mapcar #'icicle-bind-S-TAB-for-map-variable icicle-keymaps-for-key-completion))

  ;; Prevent `this-command' from being set to `handle-switch-frame'.
  (define-key global-map [handle-switch-frame] 'icicle-skip-this-command)
  (define-key global-map [switch-frame] 'icicle-handle-switch-frame))

(defun icicle-bind-S-TAB-for-map-variable (keymap-var)
  "Bind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored."
  (let ((temp  keymap-var))
    (when (boundp temp)
      (setq temp (symbol-value temp))
      (when (keymapp temp) (icicle-bind-S-TAB-in-keymaps-from temp)))))

(defun icicle-bind-S-TAB-in-keymaps-from (map)
  "Bind `S-TAB' to `icicle-complete-keys' in keymaps accessible from MAP."
  (dolist (key+map (accessible-keymaps map))
    (let ((map  (cdr key+map)))
      (when (and (keymapp map) (not (stringp (car-safe (last map))))) ; Try to exclude menu maps.
        (dolist (key icicle-generic-S-tab-keys)
          (unless (lookup-key map key)
            (condition-case nil (define-key map key 'icicle-complete-keys) (error nil))))))))

(defun icicle-restore-non-completion-keys ()
  "Restore some bindings changed by `icicle-rebind-non-completion-keys'."
  (if (not (boundp 'Info-mode-map))
      (eval-after-load "info"
        '(progn
          (icicle-unmap 'Info-goto-node Info-mode-map 'icicle-Info-goto-node-cmd)
          (icicle-unmap 'Info-index     Info-mode-map 'icicle-Info-index-cmd)
          (icicle-unmap 'Info-menu      Info-mode-map 'icicle-Info-menu-cmd)))
    (icicle-unmap 'Info-goto-node Info-mode-map 'icicle-Info-goto-node-cmd)
    (icicle-unmap 'Info-index     Info-mode-map 'icicle-Info-index-cmd)
    (icicle-unmap 'Info-menu      Info-mode-map 'icicle-Info-menu-cmd))

  (define-key dired-mode-map "\M-s" nil)
  (define-key dired-mode-map "\C-\M-r" nil)
  (define-key dired-mode-map [(control meta ?\))] nil)
  (define-key dired-mode-map [(control ?\))] nil)

  ;; Unbind `S-TAB' in major maps.
  (when (fboundp 'map-keymap)           ; Emacs 22.
    (icicle-unbind-S-TAB-in-keymaps-from (current-global-map))
    (mapcar #'icicle-unbind-S-TAB-for-map-variable icicle-keymaps-for-key-completion))

  ;; Restore prevention of `this-command' being `handle-switch-frame'.
  (define-key global-map [handle-switch-frame] nil)
  (define-key global-map [switch-frame] 'handle-switch-frame))

(defun icicle-unbind-S-TAB-for-map-variable (keymap-var)
  "Unbind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored."
  (let ((temp  keymap-var))
    (when (boundp temp)
      (setq temp (symbol-value temp))
      (when (keymapp temp) (icicle-unbind-S-TAB-in-keymaps-from temp)))))

(defun icicle-unbind-S-TAB-in-keymaps-from (map)
  "Unbind `S-TAB' in keymaps accessible from MAP."
  (dolist (key+map (accessible-keymaps map))
    (let ((map  (cdr key+map)))
      (when (and (keymapp map) (not (stringp (car-safe (last map))))) ; Try to exclude menu maps.
        (dolist (key icicle-generic-S-tab-keys)
          (when (eq (lookup-key map key) 'icicle-complete-keys)
            (condition-case nil (define-key map key nil) (error nil))))))))
 
;;(@* "Other Icicles functions that define Icicle mode")

;;; Other Icicles functions that define Icicle mode ------------------

(defun icicle-skip-this-command ()
  "Prevent `handle-switch-frame' from being added to `this-command'."
  (interactive)
  (setq this-command last-command))

(defun icicle-handle-switch-frame (event)
  "Call `handle-switch-frame', but don't add it to `this-command'."
  (interactive "e")
  (handle-switch-frame event)
  (setq this-command last-command))

(defun icicle-rebind-completion-maps (turn-on-p)
  "Rebind minibuffer completion maps to be able to cycle completions.
Also, update the bindings in the minibuffer-completion help variables."
  (cond
    (turn-on-p                          ; TURN IT ON ********************************

     ;; `minibuffer-local-map': default minibuffer map.
     (let ((map  minibuffer-local-map))
       (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
         '(menu-item "Quit" icicle-abort-recursive-edit
           :help "Cancel minibuffer input or recursive edit"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf separator-help] '("--"))
       (define-key map [menu-bar minibuf completion-help]
         '(menu-item "Icicles Help" icicle-completion-help
           :help "Display help on minibuffer input and completion"))
       (define-key map [menu-bar minibuf separator-last] '("--"))
       (define-key map [menu-bar minibuf icicle-clear-current-history]
         '(menu-item "Clear History Entries" icicle-clear-current-history
           :help "Clear current minibuffer history of selected entries."))
       (define-key map [menu-bar minibuf icicle-insert-history-element]
         '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
           :enable (consp (symbol-value minibuffer-history-variable))
           :help "Use completion to insert a previously entered input in the minibuffer."))

       (define-key map [(control ?g)]  'icicle-abort-recursive-edit) ; `C-g'
       (define-key map [M-S-backspace] 'icicle-erase-minibuffer) ; `M-S-backspace'
       (define-key map [M-S-delete]    'icicle-erase-minibuffer) ; `M-S-delete'
       (define-key map [(meta ?.)]     'icicle-insert-string-at-point) ; `M-.'
       (define-key map [(control ?=)]  'icicle-insert-string-from-variable) ; `C-='
       (define-key map [(meta ?o)]     'icicle-insert-history-element) ; `M-o'
       (define-key map [(meta ?i)]     'icicle-clear-current-history) ; `M-i'
       (define-key map [(meta ?k)]     'icicle-erase-minibuffer-or-history-element) ; `M-k'
       (define-key map [(meta ?:)]     'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
       (define-key map [(control ?a)]  'icicle-beginning-of-line+) ; `C-a'
       (define-key map [(control ?e)]  'icicle-end-of-line+) ; `C-e'
       (define-key map "\n"            'icicle-insert-newline-in-minibuffer) ; `C-j' (newline)
       (when (fboundp 'yank-secondary)  ; Defined in `second-sel.el'.
         (define-key map "\C-\M-y" 'icicle-yank-secondary))) ; `C-M-y'

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map  minibuffer-local-ns-map))
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-recursive-edit
             :help "Cancel minibuffer input or recursive edit"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help] '("--"))
         (define-key map [menu-bar minibuf completion-help]
           '(menu-item "Icicles Help" icicle-completion-help
             :help "Display help on minibuffer input and completion"))
         (define-key map [menu-bar minibuf separator-last] '("--"))
         (define-key map [menu-bar minibuf icicle-clear-current-history]
           '(menu-item "Clear History Entries" icicle-clear-current-history
             :help "Clear current minibuffer history of selected entries."))
         (define-key map [menu-bar minibuf icicle-insert-history-element]
           '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
             :enable (consp (symbol-value minibuffer-history-variable))
             :help "Use completion to insert a previously entered input in the minibuffer."))

         (define-key map [(control ?g)]  'icicle-abort-recursive-edit) ; `C-g'
         (define-key map [M-S-backspace] 'icicle-erase-minibuffer) ; `M-S-backspace'
         (define-key map [M-S-delete]    'icicle-erase-minibuffer) ; `M-S-delete'
         (define-key map [(meta ?.)]     'icicle-insert-string-at-point) ; `M-.'
         (define-key map [(control ?=)]  'icicle-insert-string-from-variable) ; `C-='
         (define-key map [(meta ?o)]     'icicle-insert-history-element) ; `M-o'
         (define-key map [(meta ?i)]     'icicle-clear-current-history) ; `M-i'
         (define-key map [(meta ?k)]     'icicle-erase-minibuffer-or-history-element) ; `M-k'
         (define-key map [(meta ?:)]     'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
         (define-key map [(control ?a)]  'icicle-beginning-of-line+) ; `C-a'
         (define-key map [(control ?e)]  'icicle-end-of-line+) ; `C-e'
         (define-key map "\n"            'icicle-insert-newline-in-minibuffer) ; `C-j' (newline)
         (when (fboundp 'yank-secondary) ; Defined in `second-sel.el'.
           (define-key map "\C-\M-y" 'icicle-yank-secondary)))) ; `C-M-y'

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
       (let ((map  minibuffer-local-isearch-map))
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-recursive-edit
             :help "Cancel minibuffer input or recursive edit"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help] '("--"))
         (define-key map [menu-bar minibuf completion-help]
           '(menu-item "Icicles Help" icicle-completion-help
             :help "Display help on minibuffer input and completion"))
         (define-key map [menu-bar minibuf separator-last] '("--"))
         (define-key map [menu-bar minibuf icicle-clear-current-history]
           '(menu-item "Clear History Entries" icicle-clear-current-history
             :help "Clear current minibuffer history of selected entries."))
         (define-key map [menu-bar minibuf icicle-insert-history-element]
           '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
             :enable (consp (symbol-value minibuffer-history-variable))
             :help "Use completion to insert a previously entered input in the minibuffer."))

         (define-key map [(control ?g)]  'icicle-abort-recursive-edit) ; `C-g'
         (define-key map [M-S-backspace] 'icicle-erase-minibuffer) ; `M-S-backspace'
         (define-key map [M-S-delete]    'icicle-erase-minibuffer) ; `M-S-delete'
         (define-key map [(meta ?.)]     'icicle-insert-string-at-point) ; `M-.'
         (define-key map [(control ?=)]  'icicle-insert-string-from-variable) ; `C-='
         (define-key map [(meta ?o)]     'icicle-insert-history-element) ; `M-o'
         (define-key map [(meta ?i)]     'icicle-clear-current-history) ; `M-i'
         (define-key map [(meta ?k)]     'icicle-erase-minibuffer-or-history-element) ; `M-k'
         (define-key map [(meta ?:)]     'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
         (define-key map [(control ?a)]  'icicle-beginning-of-line+) ; `C-a'
         (define-key map [(control ?e)]  'icicle-end-of-line+) ; `C-e'
         (define-key map "\n"            'icicle-insert-newline-in-minibuffer))) ; `C-j' (newline)

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-bind-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (if (not (eq minibuffer-local-completion-map (keymap-parent minibuffer-local-must-match-map)))
         (icicle-bind-completion-keys minibuffer-local-must-match-map)
       (define-key minibuffer-local-must-match-map [(control ?g)]
         'icicle-abort-recursive-edit)  ; `C-g' - need it anyway, even if inherit completion map.
       ;; Override the binding of `C-j' to `minibuffer-complete-and-exit'.
       (define-key minibuffer-local-must-match-map "\n"
         'icicle-insert-newline-in-minibuffer)) ; `C-j' (newline)
     (define-key minibuffer-local-must-match-map [S-return] ; `S-RET'
       'icicle-apropos-complete-and-exit)

     ;; `minibuffer-local-filename-completion-map' and `minibuffer-local-must-match-filename-map'
     ;; were introduced in Emacs 22, and they inherit from `minibuffer-local-completion' and
     ;; `minibuffer-local-must-match-map', respectively, so we need not do anything here for them.

     ;; `completion-list-mode-map': map for *Completions* buffer.
     ;; Abort on `C-g' or `q'.  Switch to minibuffer on `C-insert'.  Do not allow normal input.
     (let ((map  completion-list-mode-map))
       (define-key map [(control ?g)]     'icicle-abort-recursive-edit) ; `C-g'
       (define-key map "q"                'icicle-abort-recursive-edit) ; `q'
       (define-key map [(control insert)] 'icicle-insert-completion) ; `C-insert'
       (dolist (key icicle-prefix-cycle-next-keys) (define-key map key 'icicle-next-line)) ; `down'
       (dolist (key icicle-prefix-cycle-previous-keys)
         (define-key map key 'icicle-previous-line)) ; `up'
       (dolist (key icicle-generic-S-tab-keys)
         (define-key map key 'icicle-move-to-previous-completion)) ; `S-TAB'
       (define-key map [left]             'icicle-move-to-previous-completion) ; `left'
       (define-key map [right]            'icicle-move-to-next-completion) ; `right'
       (define-key map [(control ?i)]     'icicle-move-to-next-completion) ; `TAB'
       (define-key map [tab]              'icicle-move-to-next-completion) ; `TAB'
       (when (boundp 'mouse-wheel-down-event) ; Emacs 22+ -  `wheel-down', `wheel-up'
         (define-key map (vector mouse-wheel-down-event) 'icicle-scroll-Completions-up)
         (define-key map (vector mouse-wheel-up-event) 'icicle-scroll-Completions))
       (define-key map [S-down-mouse-2]   'icicle-mouse-remove-candidate) ; `S-mouse-2'
       (define-key map [S-mouse-2]        'ignore)
       (define-key map [C-S-down-mouse-2] 'icicle-mouse-candidate-alt-action) ; `C-S-mouse-2'
       (define-key map [C-S-mouse-2]      'ignore)
       (define-key map [C-down-mouse-2]   'icicle-mouse-candidate-action) ; `C-mouse-2'
       (define-key map [C-mouse-2]        'ignore)
       (define-key map [C-M-return]       'icicle-help-on-candidate) ; `C-M-RET'
       (define-key map [C-M-down-mouse-2] 'icicle-mouse-help-on-candidate) ; `C-M-mouse-2'
       (define-key map [C-M-mouse-2]      'ignore)
       (define-key map [M-S-down-mouse-2] 'icicle-mouse-save/unsave-candidate) ; `M-S-mouse-2'
       (define-key map [M-S-mouse-2]      'ignore)
       (define-key map [M-down-mouse-2]   'icicle-mouse-candidate-read-fn-invoke) ; `M-mouse-2'
       (define-key map [M-mouse-2]        'ignore)
       (define-key map [C-down-mouse-3]   'icicle-Completions-mouse-3-menu) ; `C-mouse-3'
       (define-key map [C-mouse-3]        'ignore)
       (define-key map [M-down-mouse-3]   'icicle-mouse-candidate-set-save-more) ; `M-mouse-3'
       (define-key map [M-mouse-3]        'ignore)
       (define-key map [M-S-down-mouse-3] 'icicle-mouse-candidate-set-save) ; `M-S-mouse-3'
       (define-key map [M-S-mouse-3]      'ignore)
       (define-key map [mouse-3]          'icicle-mouse-save-then-kill) ; `mouse-3'
       (define-key map [(control ?>)]     'icicle-candidate-set-save-more) ; `C->'
       (define-key map [(control meta ?>)] 'icicle-candidate-set-save) ; `C-M->'
       (define-key map [(control ?\))]    'icicle-candidate-set-save-more-selected) ; `C-)'
       (define-key map [(control meta ?\))] 'icicle-candidate-set-save-selected) ; `C-M-)'
       (define-key map [(control meta ?<)] 'icicle-candidate-set-retrieve) ; `C-M-<'
       (define-key map [(control ?l)]      'icicle-retrieve-previous-input) ; `C-l'
       (define-key map [(control ?a)]      'icicle-beginning-of-line+) ; `C-a'
       (define-key map [(control ?e)]      'icicle-end-of-line+) ; `C-e'
       ;; (suppress-keymap map) ; Inhibit character self-insertion.
       ))

    (t                                  ; TURN IT OFF *******************************

     ;; `minibuffer-local-map': default minibuffer map.
     (let ((map  minibuffer-local-map))
       (define-key map [menu-bar minibuf quit]
         '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf separator-help]                nil)
       (define-key map [menu-bar minibuf completion-help]               nil)
       (define-key map [menu-bar minibuf separator-last]                nil)
       (define-key map [menu-bar minibuf icicle-clear-current-history]  nil)
       (define-key map [menu-bar minibuf icicle-insert-history-element] nil)

       (define-key map [(control ?g)]  'abort-recursive-edit) ; `C-g'
       (define-key map [M-S-backspace] nil) ; `M-S-DEL'
       (define-key map [M-S-delete]    nil) ; `M-S-delete'
       (define-key map [(meta ?.)]     nil) ; `M-.'
       (define-key map [(control ?=)]  nil) ; `C-='
       (define-key map [(meta ?o)]     nil) ; `M-o'
       (define-key map [(meta ?i)]     nil) ; `M-i'
       (define-key map [(meta ?k)]     nil) ; `M-k'
       (define-key map [(meta ?:)]     nil) ; `M-:'
       (define-key map [(control ?a)]  nil) ; `C-a'
       (define-key map [(control ?e)]  nil) ; `C-e'
       (define-key map "\n"            'exit-minibuffer) ; `C-j'
       (define-key map "\C-\M-y"       nil)) ; `C-M-y'    

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map  minibuffer-local-ns-map))
         (define-key map [menu-bar minibuf quit]
           '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help]                nil)
         (define-key map [menu-bar minibuf completion-help]               nil)
         (define-key map [menu-bar minibuf separator-last]                nil)
         (define-key map [menu-bar minibuf icicle-clear-current-history]  nil)
         (define-key map [menu-bar minibuf icicle-insert-history-element] nil)

         (define-key map [(control ?g)]  'abort-recursive-edit) ; `C-g'
         (define-key map [M-S-backspace] nil) ; `M-S-DEL'
         (define-key map [M-S-delete]    nil) ; `M-S-delete'
         (define-key map [(meta ?.)]     nil) ; `M-.'
         (define-key map [(control ?=)]  nil) ; `C-='
         (define-key map [(meta ?o)]     nil) ; `M-o'
         (define-key map [(meta ?i)]     nil) ; `M-i'
         (define-key map [(meta ?k)]     nil) ; `M-k'
         (define-key map [(meta ?:)]     nil) ; `M-:'
         (define-key map [(control ?a)]  nil) ; `C-a'
         (define-key map [(control ?e)]  nil) ; `C-e'
         (define-key map "\n"            'exit-minibuffer) ; `C-j'
         (define-key map "\C-\M-y"       nil))) ; `C-M-y'    

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
       (let ((map  minibuffer-local-isearch-map))
         (define-key map [menu-bar minibuf quit]
           '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help]                nil)
         (define-key map [menu-bar minibuf completion-help]               nil)
         (define-key map [menu-bar minibuf separator-last]                nil)
         (define-key map [menu-bar minibuf icicle-clear-current-history]  nil)
         (define-key map [menu-bar minibuf icicle-insert-history-element] nil)

         (define-key map [(control ?g)]  'abort-recursive-edit) ; `C-g'
         (define-key map [M-S-backspace] nil) ; `M-S-DEL'
         (define-key map [M-S-delete]    nil) ; `M-S-delete'
         (define-key map [(meta ?.)]     nil) ; `M-.'
         (define-key map [(control ?=)]  nil) ; `C-='
         (define-key map [(meta ?o)]     nil) ; `M-o'
         (define-key map [(meta ?i)]     nil) ; `M-i'
         (define-key map [(meta ?k)]     nil) ; `M-k'
         (define-key map [(meta ?:)]     nil) ; `M-:'
         (define-key map [(control ?a)]  nil) ; `C-a'
         (define-key map [(control ?e)]  nil) ; `C-e'
         (define-key map "\n"            'exit-minibuffer))) ; `C-j'

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-restore-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (if (not (eq minibuffer-local-completion-map (keymap-parent minibuffer-local-must-match-map)))
         (icicle-restore-completion-keys minibuffer-local-must-match-map)
       (define-key minibuffer-local-must-match-map "\n" 'minibuffer-complete-and-exit)) ; `C-j'
     (define-key minibuffer-local-must-match-map [S-return] nil)

     ;; `minibuffer-local-filename-completion-map' and `minibuffer-local-must-match-filename-map'
     ;; were introduced in Emacs 22, and they inherit from `minibuffer-local-completion' and
     ;; `minibuffer-local-must-match-map', respectively, so we need not do anything here for them.

     ;; `completion-list-mode-map': map for *Completions* buffer.
     (let ((map  completion-list-mode-map))
       (define-key map [(control ?g)]       nil)
       (define-key map "q"                  nil)
       (define-key map [(control insert)]   nil)
       (dolist (key icicle-prefix-cycle-next-keys)     (define-key map key nil))
       (dolist (key icicle-prefix-cycle-previous-keys) (define-key map key nil))
       (dolist (key icicle-generic-S-tab-keys)         (define-key map key nil))
       (define-key map [(control ?i)]       nil)
       (define-key map [tab]                nil)
       (define-key map [S-down-mouse-2]     nil)
       (define-key map [S-mouse-2]          nil)
       (define-key map [C-S-down-mouse-2]   nil)
       (define-key map [C-S-mouse-2]        nil)
       (define-key map [C-down-mouse-2]     nil)
       (define-key map [C-mouse-2]          nil)
       (define-key map [C-M-return]         nil)
       (define-key map [C-M-down-mouse-2]   nil)
       (define-key map [C-M-mouse-2]        nil)
       (define-key map [M-S-down-mouse-2]   nil)
       (define-key map [M-S-mouse-2]        nil)
       (define-key map [M-down-mouse-2]     nil)
       (define-key map [M-mouse-2]          nil)
       (define-key map [C-down-mouse-3]     nil)
       (define-key map [M-down-mouse-3]     nil)
       (define-key map [M-mouse-3]          nil)
       (define-key map [M-S-down-mouse-3]   nil)
       (define-key map [M-S-mouse-3]        nil)
       (define-key map [mouse-3]            nil)
       (define-key map [C-mouse-3]          nil)
       (define-key map [(control ?>)]       nil)
       (define-key map [(control meta ?>)]  nil)
       (define-key map [(control ?\))]      nil)
       (define-key map [(control meta ?\))] nil)
       (define-key map [(control meta ?<)]  nil)
       (define-key map [(control ?l)]       nil)
       (define-key map [(control ?a)]       nil)
       (define-key map [(control ?e)]       nil)
       ;; Do these last:
       (define-key map [left]               'previous-completion)
       (define-key map [right]              'next-completion)
       )))
  (when (and (interactive-p) turn-on-p)
    (message (substitute-command-keys
              "Use `\\<minibuffer-local-completion-map>\
\\[icicle-completion-help]' in minibuffer for help."))))

(defun icicle-remap (old new map &optional oldmap)
  "Bind command NEW in MAP to all keys currently bound to OLD.
If command remapping is available, use that.  Otherwise, bind NEW to
whatever OLD is bound to in MAP, or in OLDMAP, if provided."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap old) new) ; Ignore OLDMAP for Emacs 22.
    (substitute-key-definition old new map oldmap)))

(defun icicle-unmap (command map current)
  "In MAP, unbind any keys that are bound to COMMAND.
If command remapping is available, remap COMMAND to nil in MAP,
unbinding it.
Otherwise, bind COMMAND to whatever CURRENT is bound to in MAP."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap command) nil)
    (substitute-key-definition current command map)))

(defun icicle-rebind-global (old new map)
  "Bind command NEW in MAP to all keys currently bound globally to OLD."
  (substitute-key-definition old new map (current-global-map)))

(defun icicle-bind-completion-keys (map)
  "Bind keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.

  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
      '(menu-item "Quit" icicle-abort-recursive-edit
        :help "Cancel minibuffer input or recursive edit"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET"))
    (define-key map [menu-bar minibuf separator-help] '("--"))
    (define-key map [menu-bar minibuf completion-help]
      '(menu-item "Icicles Help" icicle-completion-help
        :help "Display help on minibuffer input and completion"))
    (define-key map [menu-bar minibuf separator-last] '("--"))
    (define-key map [menu-bar minibuf icicle-clear-current-history]
      '(menu-item "Clear History Entries" icicle-clear-current-history
        :help "Clear current minibuffer history of selected entries."))
    (define-key map [menu-bar minibuf icicle-insert-history-element]
      '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
        :enable (consp (symbol-value minibuffer-history-variable))
        :help "Use completion to insert a previously entered input in the minibuffer.")))
  (define-key map [menu-bar minibuf icicle-retrieve-next-input]
    '(menu-item "Restore Next Completion Input" icicle-retrieve-next-input
      :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                       'icicle-previous-raw-file-name-inputs
                                     'icicle-previous-raw-non-file-name-inputs)))
      :help "Cycle forward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
  (define-key map [menu-bar minibuf icicle-retrieve-previous-input]
    '(menu-item "Restore Previous Completion Input" icicle-retrieve-previous-input
      :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                       'icicle-previous-raw-file-name-inputs
                                     'icicle-previous-raw-non-file-name-inputs)))
      :help "Cycle backward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
  (define-key map [menu-bar minibuf separator-C-l] '("--"))
  (define-key map [menu-bar minibuf ?\?] nil)
  (define-key map [menu-bar minibuf space] nil)
  (define-key map [menu-bar minibuf tab] nil)
  (define-key map [menu-bar minibuf alt-action-list-all]
    '(menu-item "Alt Act on List of Candidates" icicle-all-candidates-list-alt-action
      :help "Apply the alternative action to the list of matching completion candidates"
      :enable icicle-all-candidates-list-alt-action-fn))
  (define-key map [menu-bar minibuf alt-action-all]
    '(menu-item "Alt Act on Each Candidate" icicle-all-candidates-alt-action
      :help "Apply the alternative action to each matching completion candidates"
      :enable icicle-candidate-alt-action-fn))
  (define-key map [menu-bar minibuf action-list-all]
    '(menu-item "Act on List of Candidates" icicle-all-candidates-list-action
      :help "Apply the command action to the list of matching completion candidates"
      :enable icicle-all-candidates-list-action-fn))
  (define-key map [menu-bar minibuf action-all]
    '(menu-item "Act on Each Candidate" icicle-all-candidates-action
      :help "Apply the command action to each matching completion candidates"
      :enable icicle-candidate-action-fn))
  (define-key map [menu-bar minibuf separator-actions] '("--"))
  (define-key map [menu-bar minibuf set-define]
    '(menu-item "Define Candidates by Lisp Sexp" icicle-candidate-set-define
      :help "Define the set of current completion candidates by evaluating a sexp"))
  (define-key map [menu-bar minibuf icicle-keep-only-past-inputs]
    '(menu-item "Keep Only Previously Entered" icicle-keep-only-past-inputs
      :enable (and icicle-completion-candidates (consp (symbol-value minibuffer-history-variable)))
      :help "Removed candidates that you have not previously chosen and entered"))
  (define-key map [menu-bar minibuf set-union]
    '(menu-item "Add (Union) Saved Candidate Set" icicle-candidate-set-union
      :enable icicle-saved-completion-candidates
      :help "Set union between the current and saved completion candidates"))
  (define-key map [menu-bar minibuf set-difference]
    '(menu-item "Subtract Saved Candidate Set" icicle-candidate-set-difference
      :enable icicle-saved-completion-candidates
      :help "Set difference between the current and saved completion candidates"))
  (define-key map [menu-bar minibuf set-intersection]
    '(menu-item "Intersect Saved Candidate Set" icicle-candidate-set-intersection
      :enable icicle-saved-completion-candidates
      :help "Set intersection between the current and saved candidates"))
  (define-key map [menu-bar minibuf icicle-save-predicate-to-variable]
    '(menu-item "Save Predicate to Variable" icicle-save-predicate-to-variable
      :help "Save the current completion predicate to a variable"))
  (define-key map [menu-bar minibuf icicle-narrow-candidates-with-predicate]
    '(menu-item "Satisfy Also Predicate..." icicle-narrow-candidates-with-predicate
      :help "Match another input pattern (narrow completions set)"
      :enable icicle-completion-candidates))
  (define-key map [menu-bar minibuf icicle-narrow-candidates]
    '(menu-item "Match Also Regexp..." icicle-narrow-candidates
      :enable icicle-completion-candidates
      :help "Match another input pattern (narrow completions set)"))
  (define-key map [menu-bar minibuf set-complement]
    '(menu-item "Complement Candidates" icicle-candidate-set-complement
      :help "Complement the set of current completion candidates"))
  (define-key map [menu-bar minibuf separator-set1] '("--"))
  (define-key map [menu-bar minibuf set-swap]
    '(menu-item "Swap Saved and Current Sets" icicle-candidate-set-swap
      :enable icicle-saved-completion-candidates
      :help "Swap the saved and current sets of completion candidates"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more-selected]
    '(menu-item "Add Selected (Region) Candidates"
      icicle-candidate-set-save-more-selected
      :help "Add the candidates in the region to the saved candidates"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-selected]
    '(menu-item "Save Selected (Region) Candidates"
      icicle-candidate-set-save-selected
      :help "Save the candidates in the region, for later recall"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more]
    '(menu-item "Add Candidates in Current Set"
      icicle-candidate-set-save-more
      :help "Add current completion candidates to saved candidates set"))
  (define-key map [menu-bar minibuf set-retrieve-from-cache-file]
    '(menu-item "    from Cache File..."
      icicle-candidate-set-retrieve-persistent
      :help "Retrieve saved completion candidates from a cache file, making them current"))
  (define-key map [menu-bar minibuf set-retrieve-from-variable]
    '(menu-item
      "    from Variable..." icicle-candidate-set-retrieve-from-variable
      :help "Retrieve saved completion candidates from variable, making them current"))
  (define-key map [menu-bar minibuf set-retrieve]
    '(menu-item "Retrieve Saved Candidates" icicle-candidate-set-retrieve
      :enable icicle-saved-completion-candidates
      :help "Retrieve the saved set of completion candidates, making it current"))
  (define-key map [menu-bar minibuf set-save-to-cache-file]
    '(menu-item "    to Cache File..." icicle-candidate-set-save-persistently
      :help "Save current completion candidates to a cache file, for later recall"))
  (define-key map [menu-bar minibuf set-save-to-variable]
    '(menu-item "    to Variable..." icicle-candidate-set-save-to-variable
      :help "Save current completion candidates to a variable, for later recall"))
  (define-key map [menu-bar minibuf set-save]
    '(menu-item "Save Candidates" icicle-candidate-set-save
      :help "Save the set of current completion candidates, for later recall"))
  (define-key map [menu-bar minibuf separator-set2] '("--"))
  (define-key map [menu-bar minibuf word-complete]
    '(menu-item "Word-Complete" icicle-prefix-word-complete
      :help "Complete at most one word of prefix"))
  (define-key map [menu-bar minibuf prefix-complete]
    '(menu-item "Prefix-Complete" icicle-prefix-complete
      :help "Complete prefix as far as possible"))
  (define-key map [menu-bar minibuf apropos-complete]
    '(menu-item "Apropos-Complete" icicle-apropos-complete :keys "S-TAB"
      :help "Complete regular expression as far as possible and list completions"))

  ;; Remap some commands for completion.
  (icicle-remap 'self-insert-command           'icicle-self-insert map (current-global-map))
  (icicle-remap 'universal-argument            'icicle-universal-argument ; `C-u'
                map (current-global-map))
  (icicle-remap 'negative-argument             'icicle-negative-argument ; `M--'
                map (current-global-map))
  (icicle-remap 'digit-argument                'icicle-digit-argument ; `C-9'
                map (current-global-map))
  (icicle-remap 'backward-delete-char-untabify 'icicle-backward-delete-char-untabify ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-backward-char          'icicle-delete-backward-char ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-char                   'icicle-delete-char ; `C-d', `deletechar'
                map (current-global-map))
  (icicle-remap 'backward-kill-word            'icicle-backward-kill-word ; `M-DEL'
                map (current-global-map))
  (icicle-remap 'kill-word                     'icicle-kill-word ; `M-d'
                map (current-global-map))
  (icicle-remap 'backward-kill-sexp            'icicle-backward-kill-sexp ; `C-M-backspace'
                map (current-global-map))
  (icicle-remap 'kill-sexp                     'icicle-kill-sexp ; `C-M-k', `C-M-delete'
                map (current-global-map))
  (icicle-remap 'backward-kill-sentence        'icicle-backward-kill-sentence ; `C-x DEL'
                map (current-global-map))
  (icicle-remap 'backward-kill-paragraph       'icicle-backward-kill-paragraph ; `C-backspace'
                map (current-global-map))
  (icicle-remap 'kill-paragraph                'icicle-kill-paragraph ; `C-delete'
                map (current-global-map))
  (icicle-remap 'kill-line                     'icicle-kill-line ; `C-k', `deleteline'
                map (current-global-map))
  (icicle-remap 'reposition-window             'icicle-kill-failed-input ; `C-M-l'
                map (current-global-map))
  (icicle-remap 'transpose-chars               'icicle-transpose-chars ; `C-t'
                map (current-global-map))
  (icicle-remap 'transpose-words               'icicle-transpose-words ; `M-t'
                map (current-global-map))
  (icicle-remap 'transpose-sexps               'icicle-transpose-sexps ; `C-M-t'
                map (current-global-map))
  (icicle-remap 'yank-pop                      'icicle-yank-pop ; `M-y', `M-insert'
                map (current-global-map))
  (icicle-remap 'mouse-yank-secondary          'icicle-mouse-yank-secondary ; `M-mouse-2'
                map (current-global-map))

  ;; Bind additional keys.
  (dolist (key icicle-word-completion-keys)
    (define-key map key 'icicle-prefix-word-complete)) ; `M-SPC'
  (dolist (key icicle-generic-S-tab-keys) (define-key map key 'icicle-apropos-complete)) ; `S-TAB'
  (dolist (key icicle-prefix-complete-keys) (define-key map key 'icicle-prefix-complete)) ; `TAB'
  (dolist (key icicle-apropos-complete-no-display-keys)
    (define-key map key 'icicle-apropos-complete-no-display)) ; `C-M-S-TAB'
  (dolist (key icicle-prefix-complete-no-display-keys)
    (define-key map key 'icicle-prefix-complete-no-display)) ; `C-M-TAB'

  (cond (icicle-use-C-for-actions-flag
         (unless icicle-cycling-respects-completion-mode-flag
           (dolist (key icicle-prefix-cycle-previous-keys)
             (define-key map key 'icicle-previous-prefix-candidate)) ; `up'
           (dolist (key icicle-prefix-cycle-next-keys)
             (define-key map key 'icicle-next-prefix-candidate)) ; `down'
           (dolist (key icicle-apropos-cycle-previous-keys)
             (define-key map key 'icicle-previous-apropos-candidate)) ; `prior'
           (dolist (key icicle-apropos-cycle-next-keys)
             (define-key map key 'icicle-next-apropos-candidate))) ; `next'
         (define-key map [(control up)]    'icicle-previous-prefix-candidate-action) ; `C-up'
         (define-key map [(control down)]  'icicle-next-prefix-candidate-action) ; `C-down'
         (define-key map [(control prior)] 'icicle-previous-apropos-candidate-action) ; `C-prior'
         (define-key map [(control next)]  'icicle-next-apropos-candidate-action)) ; `C-next'
        (t
         (unless icicle-cycling-respects-completion-mode-flag
           (dolist (key icicle-prefix-cycle-previous-keys)
             (define-key map key 'icicle-previous-prefix-candidate-action)) ; `up'
           (dolist (key icicle-prefix-cycle-next-keys)
             (define-key map key 'icicle-next-prefix-candidate-action)) ; `down'
           (dolist (key icicle-apropos-cycle-previous-keys)
             (define-key map key 'icicle-previous-apropos-candidate-action)) ; `prior'
           (dolist (key icicle-apropos-cycle-next-keys)
             (define-key map key 'icicle-next-apropos-candidate-action))) ; `next'
         (define-key map [(control up)]    'icicle-previous-prefix-candidate) ; `C-up'
         (define-key map [(control down)]  'icicle-next-prefix-candidate) ; `C-down'
         (define-key map [(control prior)] 'icicle-previous-apropos-candidate) ; `C-prior'
         (define-key map [(control next)]  'icicle-next-apropos-candidate))) ; `C-next'

  (define-key map [(control meta up)]        'icicle-help-on-previous-prefix-candidate) ; `C-M-up'
  (define-key map [(control meta down)]      'icicle-help-on-next-prefix-candidate) ; `C-M-down'
  (define-key map [(control meta prior)]  'icicle-help-on-previous-apropos-candidate) ; `C-M-prior'
  (define-key map [(control meta next)]      'icicle-help-on-next-apropos-candidate) ; `C-M-next'
  (define-key map [(control help)]           'icicle-help-on-candidate) ; `C-help'
  (define-key map [(control meta help)]      'icicle-help-on-candidate) ; `C-M-help'
  (define-key map [(control f1)]             'icicle-help-on-candidate) ; `C-f1'
  (define-key map [(control meta f1)]        'icicle-help-on-candidate) ; `C-M-f1'
  (define-key map [(control meta return)]    'icicle-help-on-candidate) ; `C-M-RET'
  (define-key map [(meta return)]            'icicle-candidate-read-fn-invoke) ; `M-RET'
  (define-key map [(control shift return)]   'icicle-candidate-alt-action) ; `C-S-RET'
  (define-key map [(control shift up)]     'icicle-previous-prefix-candidate-alt-action) ; `C-S-up'
  (define-key map [(control shift down)]     'icicle-next-prefix-candidate-alt-action) ; `C-S-down'
  (define-key map [(control shift prior)]
    'icicle-previous-apropos-candidate-alt-action) ;`C-S-prior'
  (define-key map [(control shift next)]    'icicle-next-apropos-candidate-alt-action) ; `C-S-next'
  (define-key map [delete]                   'icicle-remove-candidate) ; `delete'
  (define-key map [(shift delete)]           'icicle-delete-candidate-object) ; `S-delete'
  (define-key map [(control ?w)]             'icicle-kill-region) ; `C-w'
  (define-key map [(control return)]         'icicle-candidate-action) ; `C-RET'
  (define-key map [(control ?!)]             'icicle-all-candidates-action) ; `C-!'
  (define-key map [(control ?|)]             'icicle-all-candidates-alt-action) ; `C-|'
  (define-key map [(meta ?!)]                'icicle-all-candidates-list-action) ; `M-!'
  (define-key map [(meta ?|)]                'icicle-all-candidates-list-alt-action) ; `M-|'
  (define-key map [(control meta ?/)]        'icicle-prefix-complete) ; `C-M-/', for `dabbrev.el'.
  (define-key map [(meta ?h)]                'icicle-history) ; `M-h'
  (define-key map [(meta pause)]             'icicle-keep-only-past-inputs) ; `M-pause'
  (define-key map [(control pause)]     'icicle-toggle-highlight-historical-candidates) ; `C-pause'
  (define-key map [(control insert)]         'icicle-switch-to-Completions-buf) ; `C-insert'
  (define-key map [insert]                   'icicle-save/unsave-candidate) ; `insert'

  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    ;; Note: `setup-keys.el' binds `C-o' to `1on1-fit-minibuffer-frame' if defined.
    (define-key map [(control ?a)]           'icicle-beginning-of-line+) ; `C-a'
    (define-key map [(control ?e)]           'icicle-end-of-line+) ; `C-e'
    (define-key map [(control ?=)]           'icicle-insert-string-from-variable) ; `C-='
    ;; Replaces `tab-to-tab-stop':
    (define-key map [(meta ?i)]              'icicle-clear-current-history) ; `M-i'
    ;; Replaces `kill-sentence':
    (define-key map [(meta ?k)]              'icicle-erase-minibuffer-or-history-element) ; `M-k'
    (define-key map [(meta ?o)]              'icicle-insert-history-element) ; `M-o'
    (define-key map [(meta ?.)]              'icicle-insert-string-at-point) ; `M-.'
    (define-key map [(meta ?:)]              'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
    (when (fboundp 'yank-secondary)     ; Defined in `second-sel.el'.
      (define-key map "\C-\M-y"              'icicle-yank-secondary)) ; `C-M-y'
    (define-key map [M-S-backspace]          'icicle-erase-minibuffer) ; `M-S-backspace'
    (define-key map [M-S-delete]             'icicle-erase-minibuffer) ; `M-S-delete'
    )

  ;; Need `C-g', even if `minibuffer-local-completion-map' inherits from `minibuffer-local-map'.
  (define-key map [(control ?g)]             'icicle-abort-recursive-edit) ; `C-g'
  (define-key map [(meta ?q)]                'icicle-dispatch-M-q) ; `M-q'
  (define-key map [(control ?l)]             'icicle-retrieve-previous-input) ; `C-l'
  (define-key map [(control shift ?l)]       'icicle-retrieve-next-input) ; `C-L' (`C-S-l')
  (define-key map [(meta ?$)]                'icicle-candidate-set-truncate) ; `M-$'
  (define-key map [(control ?~)]             'icicle-candidate-set-complement) ; `C-~'
  (define-key map [(control ?-)]             'icicle-candidate-set-difference) ; `C--'
  (define-key map [(control ?+)]             'icicle-candidate-set-union) ; `C-+'
  (define-key map [(control ?*)]             'icicle-candidate-set-intersection) ; `C-*'
  (define-key map [(control ?>)]             'icicle-candidate-set-save-more) ; `C->'
  (define-key map [(control meta ?>)]        'icicle-candidate-set-save) ; `C-M->'
  (define-key map [(control ?\()]            'icicle-toggle-fuzzy-completion) ; `C-('
  (define-key map [(meta ?\()]               'icicle-next-apropos-match-function) ; `M-('
  (define-key map [(control ?\))]            'icicle-candidate-set-save-more-selected) ; `C-)'
  (define-key map [(control meta ?\))]       'icicle-candidate-set-save-selected) ; `C-M-)'
  (define-key map [(control meta ?<)]        'icicle-candidate-set-retrieve) ; `C-M-<'
  (define-key map [(control meta ?})]        'icicle-candidate-set-save-to-variable) ; `C-M-}'
  (define-key map [(control meta ?{)]       'icicle-candidate-set-retrieve-from-variable) ; `C-M-{'
  (define-key map [(control ?})]             'icicle-candidate-set-save-persistently) ; `C-}'
  (define-key map [(control ?{)]            'icicle-candidate-set-retrieve-persistent) ; `C-{'
  (define-key map [(control ?%)]             'icicle-candidate-set-swap) ; `C-%'
  (define-key map [(control ?:)]             'icicle-candidate-set-define) ; `C-:'
  (define-key map [(control meta ?j)]        'icicle-insert-list-join-string) ; `C-M-j'
  (define-key map [(control ?,)]             'icicle-dispatch-C-comma) ; `C-,'
  (define-key map [(control ?`)]             'icicle-toggle-regexp-quote) ; `C-`'
  (define-key map [(control meta ?`)]        'icicle-toggle-literal-replacement) ; `C-M-`'
  (define-key map [(control ?<)]             'icicle-toggle-angle-brackets) ; `C-<'
  (define-key map [(control meta ?_)]        'icicle-toggle-proxy-candidates) ; `C-M-_'
  (define-key map [(control ?$)]             'icicle-toggle-transforming) ; `C-$'
  (define-key map [(control ??)]             'icicle-completion-help) ; `C-?'
  (define-key map [(control ?.)]             'icicle-dispatch-C-.) ; `C-.'
  (define-key map [(control ?#)]             'icicle-toggle-incremental-completion) ; `C-#'
  (define-key map [(control ?\;)]            'icicle-toggle-expand-to-common-match) ; `C-;'
  (define-key map [(meta ?\;)]               'icicle-toggle-search-replace-common-match) ; `M-;'
  (define-key map [(control ?^)]             'icicle-dispatch-C-^) ; `C-^'
  (define-key map [(control shift ?a)]       'icicle-toggle-case-sensitivity) ; `C-S-a' (`C-A')
  (define-key map [(meta ?~)]                'icicle-toggle-~-for-home-dir) ; `M-~'
  (define-key map [(meta ?g)]                'icicle-toggle-C-for-actions) ; `M-g'
  (define-key map [(meta ?,)]                'icicle-dispatch-M-comma) ; `M-,'
  (define-key map [(control meta ?,)]        'icicle-toggle-alternative-sorting) ; `C-M-,'
  (define-key map [(meta ?*)]                'icicle-narrow-candidates) ; `M-*'
  (define-key map [(meta ?&)]                'icicle-narrow-candidates-with-predicate) ; `M-&'
  (define-key map [(meta ?_)]                'icicle-toggle-ignored-space-prefix) ; `M-_'
  (define-key map [(control meta ?&)]        'icicle-save-predicate-to-variable) ; `C-M-&'
  (define-key map [(shift ?\ )]              'icicle-apropos-complete-and-narrow) ; `S-SPC'
  (when (fboundp 'doremi)
    (define-key map "\C-xw"                  'icicle-doremi-candidate-width-factor) ; `C-x w'
    (define-key map "\C-x|"                  'icicle-doremi-inter-candidates-min-spaces)) ; `C-x |'
  ;; `minibuffer-completion-help' got wiped out by remap for self-insert.
  (define-key map "?"                        'icicle-self-insert) ; `?'
  (define-key map " "                        'icicle-self-insert) ; " "
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map "\n"                     'icicle-insert-newline-in-minibuffer)) ; `C-j'
  )

(defun icicle-restore-completion-keys (map)
  "Restore standard keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [menu-bar minibuf quit]
      '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET"))
    (define-key map [menu-bar minibuf separator-help]                        nil)
    (define-key map [menu-bar minibuf completion-help]                       nil)
    (define-key map [menu-bar minibuf separator-last]                        nil)
    (define-key map [menu-bar minibuf icicle-clear-current-history]          nil)
    (define-key map [menu-bar minibuf icicle-insert-history-element]         nil))
  (define-key map [menu-bar minibuf icicle-retrieve-next-input]              nil)
  (define-key map [menu-bar minibuf icicle-retrieve-previous-input]          nil)
  (define-key map [menu-bar minibuf separator-C-l]                           nil)
  (define-key map [menu-bar minibuf alt-action-list-all]                     nil)
  (define-key map [menu-bar minibuf alt-action-all]                          nil)
  (define-key map [menu-bar minibuf action-list-all]                         nil)
  (define-key map [menu-bar minibuf action-all]                              nil)
  (define-key map [menu-bar minibuf separator-actions]                       nil)
  (define-key map [menu-bar minibuf set-define]                              nil)
  (define-key map [menu-bar minibuf icicle-keep-only-past-inputs]            nil)
  (define-key map [menu-bar minibuf set-union]                               nil)
  (define-key map [menu-bar minibuf set-difference]                          nil)
  (define-key map [menu-bar minibuf set-intersection]                        nil)
  (define-key map [menu-bar minibuf icicle-save-predicate-to-variable]       nil)
  (define-key map [menu-bar minibuf icicle-narrow-candidates-with-predicate] nil)
  (define-key map [menu-bar minibuf icicle-narrow-candidates]                nil)
  (define-key map [menu-bar minibuf set-complement]                          nil)
  (define-key map [menu-bar minibuf separator-set1]                          nil)
  (define-key map [menu-bar minibuf set-swap]                                nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more-selected] nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-selected]      nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more]          nil)
  (define-key map [menu-bar minibuf set-retrieve-from-cache-file]            nil)
  (define-key map [menu-bar minibuf set-retrieve-from-variable]              nil)
  (define-key map [menu-bar minibuf set-retrieve]                            nil)
  (define-key map [menu-bar minibuf set-save-to-cache-file]                  nil)
  (define-key map [menu-bar minibuf set-save-to-variable]                    nil)
  (define-key map [menu-bar minibuf set-save]                                nil)
  (define-key map [menu-bar minibuf separator-set2]                          nil)
  (define-key map [menu-bar minibuf word-complete]                           nil)
  (define-key map [menu-bar minibuf prefix-complete]                         nil)
  (define-key map [menu-bar minibuf apropos-complete]                        nil)
  (define-key map [menu-bar minibuf ?\?]
    '(menu-item "List Completions" minibuffer-completion-help
      :help "Display all possible completions"))
  (define-key map [menu-bar minibuf space]
    '(menu-item "Complete Word" minibuffer-complete-word :help "Complete at most one word"))
  (define-key map [menu-bar minibuf tab]
    '(menu-item "Complete" minibuffer-complete :help "Complete as far as possible"))

  ;; Unmap commands that were bound for completion.
  (icicle-unmap 'self-insert-command           map 'icicle-self-insert)
  (icicle-unmap 'universal-argument            map 'icicle-universal-argument)
  (icicle-unmap 'negative-argument             map 'icicle-negative-argument)
  (icicle-unmap 'digit-argument                map 'icicle-digit-argument)
  (icicle-unmap 'backward-delete-char-untabify map 'icicle-backward-delete-char-untabify)
  (icicle-unmap 'delete-backward-char          map 'icicle-delete-backward-char)
  (icicle-unmap 'delete-char                   map 'icicle-delete-char)
  (icicle-unmap 'backward-kill-word            map 'icicle-backward-kill-word)
  (icicle-unmap 'kill-word                     map 'icicle-kill-word)
  (icicle-unmap 'backward-kill-sexp            map 'icicle-backward-kill-sexp)
  (icicle-unmap 'kill-sexp                     map 'icicle-kill-sexp)
  (icicle-unmap 'backward-kill-sentence        map 'icicle-backward-kill-sentence)
  (icicle-unmap 'backward-kill-paragraph       map 'icicle-backward-kill-paragraph)
  (icicle-unmap 'kill-paragraph                map 'icicle-kill-paragraph)
  (icicle-unmap 'kill-line                     map 'icicle-kill-line)
  (icicle-unmap 'reposition-window             map 'icicle-kill-failed-input)
  (icicle-unmap 'transpose-chars               map 'icicle-transpose-chars)
  (icicle-unmap 'transpose-words               map 'icicle-transpose-words)
  (icicle-unmap 'transpose-sexps               map 'icicle-transpose-sexps)
  (icicle-unmap 'yank-pop                      map 'icicle-yank-pop)
  (icicle-unmap 'mouse-yank-secondary          map 'icicle-mouse-yank-secondary)

  ;; Restore additional bindings.
  ;; Do the option keys first, so they can be rebound as needed.
  (dolist (key icicle-word-completion-keys) (define-key map key nil))
  (dolist (key icicle-generic-S-tab-keys) (define-key map key nil))
  (dolist (key icicle-prefix-complete-keys) (define-key map key nil))
  (dolist (key icicle-apropos-complete-no-display-keys) (define-key map key nil))
  (dolist (key icicle-prefix-complete-no-display-keys) (define-key map key nil))
  (unless icicle-cycling-respects-completion-mode-flag
    (dolist (key icicle-prefix-cycle-previous-keys) (define-key map key nil))
    (dolist (key icicle-prefix-cycle-next-keys) (define-key map key nil))
    (dolist (key icicle-apropos-cycle-previous-keys) (define-key map key nil))
    (dolist (key icicle-apropos-cycle-next-keys) (define-key map key nil)))
  (define-key map [(control up)]             nil)
  (define-key map [(control down)]           nil)
  (define-key map [(control prior)]          nil)
  (define-key map [(control next)]           nil)
  (define-key map [(control meta up)]        nil)
  (define-key map [(control meta down)]      nil)
  (define-key map [(control meta prior)]     nil)
  (define-key map [(control meta next)]      nil)
  (define-key map [(control help)]           nil)
  (define-key map [(control meta help)]      nil)
  (define-key map [(control f1)]             nil)
  (define-key map [(control meta f1)]        nil)
  (define-key map [(control meta return)]    nil)
  (define-key map [(meta return)]            nil)
  (define-key map [(control shift return)]   nil)
  (define-key map [(control shift up)]       nil)
  (define-key map [(control shift down)]     nil)
  (define-key map [(control shift prior)]    nil)
  (define-key map [(control shift next)]     nil)
  (define-key map [delete]                   nil)
  (define-key map [(shift delete)]           nil)
  (define-key map [(control ?w)]             nil)
  (define-key map [(control return)]         nil)
  (define-key map [(control ?!)]             nil)
  (define-key map [(control ?!)]             nil)
  (define-key map [(control ?|)]             nil)
  (define-key map [(meta ?!)]                nil)
  (define-key map [(meta ?|)]                nil)
  (define-key map [(control meta ?/)]        nil)
  (define-key map [(meta ?h)]                nil)
  (define-key map [(meta pause)]             nil)
  (define-key map [(control pause)]          nil)
  (define-key map [(control insert)]         nil)
  (define-key map [insert]                   nil)

  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [(control ?a)]           nil)
    (define-key map [(control ?e)]           nil)
    (define-key map [(control ?=)]           nil)
    (define-key map [(meta ?i)]              nil)
    (define-key map [(meta ?k)]              nil)
    (define-key map [(meta ?o)]              nil)
    (define-key map [(meta ?.)]              nil)
    (define-key map [(meta ?:)]              nil)
    (define-key map "\C-\M-y"                nil)   
    (define-key map [M-S-backspace]          nil)
    (define-key map [M-S-delete]             nil)
    )

  (define-key map [(meta ?q)]                nil)
  (define-key map [(control ?l)]             nil)
  (define-key map [(control shift ?l)]       nil)
  (define-key map [(meta ?$)]                nil)
  (define-key map [(control ?~)]             nil)
  (define-key map [(control ?-)]             nil)
  (define-key map [(control ?+)]             nil)
  (define-key map [(control ?*)]             nil)
  (define-key map [(control ?>)]             nil)
  (define-key map [(control meta ?>)]        nil)
  (define-key map [(control ?\()]            nil)
  (define-key map [(meta ?\()]               nil)
  (define-key map [(control ?\))]            nil)
  (define-key map [(control meta ?\))]       nil)
  (define-key map [(control meta ?<)]        nil)
  (define-key map [(control meta ?})]        nil)
  (define-key map [(control meta ?{)]        nil)
  (define-key map [(control ?})]             nil)
  (define-key map [(control ?{)]             nil)
  (define-key map [(control ?%)]             nil)
  (define-key map [(control ?:)]             nil)
  (define-key map [(control meta ?j)]        nil)
  (define-key map [(control ?,)]             nil)
  (define-key map [(control ?`)]             nil)
  (define-key map [(control meta ?`)]        nil)
  (define-key map [(control ?<)]             nil)
  (define-key map [(control meta ?_)]        nil)
  (define-key map [(control ?$)]             nil)
  (define-key map [(control ??)]             nil)
  (define-key map [(control ?.)]             nil)
  (define-key map [(control ?#)]             nil)
  (define-key map [(control ?\;)]            nil)
  (define-key map [(meta ?\;)]               nil)
  (define-key map [(control ?^)]             nil)
  (define-key map [(control shift ?a)]       nil)
  (define-key map [(meta ?~)]                nil)
  (define-key map [(meta ?g)]                nil)
  (define-key map [(meta ?,)]                nil)
  (define-key map [(control meta ?,)]        nil)
  (define-key map [(meta ?*)]                nil)
  (define-key map [(meta ?&)]                nil)
  (define-key map [(meta ?_)]                nil)
  (define-key map [(control meta ?&)]        nil)
  (define-key map [(shift ?\ )]              nil)
  (when (fboundp 'doremi)
    (define-key map "\C-xw"                  nil)
    (define-key map "\C-x|"                  nil))
  ;; Do these last. -----------------
  (define-key map [(control ?i)]             'minibuffer-complete)
  (define-key map [tab]                      'minibuffer-complete)
  (define-key map "?"                        'minibuffer-completion-help)
  (define-key map " "                        'minibuffer-complete-word)
  (define-key map [(control ?g)]             'abort-recursive-edit)
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map "\n"                     'exit-minibuffer)
    )
  (define-key map [(meta ?p)]                'previous-history-element)
  (define-key map [(meta ?n)]                'next-history-element)
  (define-key map [up]                       'previous-history-element)
  (define-key map [down]                     'next-history-element)
  (define-key map [(meta ?v)]                'switch-to-completions)
  (define-key map [prior]                    'switch-to-completions)
  (define-key map [next]                     'next-history-element))

(defun icicle-minibuffer-setup ()
  "Run in minibuffer on activation, to enable completion cycling.
Usually run by inclusion in `minibuffer-setup-hook'."
  (when (and icicle-mode (window-minibuffer-p (selected-window)) (not executing-kbd-macro))
    ;; The pre- and post-command hooks are local to the
    ;; minibuffer, so they are added here, not in `icicle-mode'.
    ;; They are removed in `icicle-mode' when mode is exited.
    (unless (fboundp 'define-minor-mode) (make-local-hook 'pre-command-hook))
    (add-hook 'pre-command-hook 'icicle-top-level-prep)
    (add-hook 'pre-command-hook 'icicle-run-icicle-pre-command-hook nil t)
    (unless (fboundp 'define-minor-mode) (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook 'icicle-run-icicle-post-command-hook nil t)
    ;; Change the region background here dynamically.  It would be better to
    ;; just use a buffer-local face, but those don't yet exist.
    (when (= 1 (recursion-depth))
      (setq icicle-saved-region-background (face-background 'region)))
    (when icicle-change-region-background-flag
      (set-face-background 'region icicle-region-background))
    ;; Reset prompt, because some commands (e.g. `find-file') don't use `read-file-name'
    ;; or `completing-read'.  Reset other stuff too.
    (setq icicle-candidate-nb                   nil
          icicle-completion-candidates          nil
          icicle-current-completion-mode        nil
          icicle-completing-p                   nil
          icicle-default-directory              default-directory
          icicle-incremental-completion-p       icicle-incremental-completion-flag
          icicle-initial-value                  nil
          icicle-last-completion-command        nil
          icicle-last-completion-candidate      nil
          icicle-last-input                     nil
          icicle-input-fail-pos                 nil
          icicle-saved-proxy-candidates         nil
          icicle-pre-minibuffer-buffer          (cadr (buffer-list))
          icicle-prompt                         "")
    (unless icicle-add-proxy-candidates-flag
      (setq icicle-saved-proxy-candidates
            (prog1 icicle-proxy-candidates
              (setq icicle-proxy-candidates icicle-saved-proxy-candidates))))
    (while icicle-saved-candidate-overlays
      (delete-overlay (car icicle-saved-candidate-overlays))
      (setq icicle-saved-candidate-overlays (cdr icicle-saved-candidate-overlays)))
    (when icicle-cycling-respects-completion-mode-flag
      (dolist (map (if (boundp 'minibuffer-local-filename-completion-map)
                       (list minibuffer-local-completion-map
                             minibuffer-local-filename-completion-map
                             minibuffer-local-must-match-map)
                     (list minibuffer-local-completion-map minibuffer-local-must-match-map)))
        (dolist (key icicle-modal-cycle-up-keys)
          (define-key map key 'icicle-previous-candidate-per-mode))
        (dolist (key icicle-modal-cycle-down-keys)
          (define-key map key 'icicle-next-candidate-per-mode))))
    (icicle-update-ignored-extensions-regexp)
    (when (memq icicle-default-value '(preselect-start preselect-end))
      (icicle-select-minibuffer-contents))
    (when (and icicle-show-Completions-initially-flag
               (icicle-completing-p)    ; Function initializes variable `icicle-completing-p'.
               (sit-for icicle-incremental-completion-delay)) ; Let user interrupt.
      ;; Apropos completion, by default.
      (icicle-apropos-complete))        ; Defined in `icicles-cmd.el'.
    (run-hooks 'icicle-minibuffer-setup-hook)))

(defun icicle-select-minibuffer-contents ()
  "Select minibuffer contents and leave point at its beginning."
  (let ((min  (icicle-minibuffer-prompt-end)))
    (set-mark (if (eq 'preselect-start icicle-default-value) (point-max) min))
    (goto-char (if (eq 'preselect-start icicle-default-value) min (point-max)))))

;; $$$ (defadvice next-history-element (after icicle-select-minibuffer-contents activate)
;;   "Select minibuffer contents and leave point at its beginning."
;;   (when (and icicle-mode (memq icicle-default-value '(preselect-start preselect-end)))
;;     (icicle-select-minibuffer-contents)
;;     (setq deactivate-mark nil)))

(defun icicle-cancel-Help-redirection ()
  "Cancel redirection of focus from *Help* buffer to minibuffer.
Focus was redirected during `icicle-help-on-candidate'."
  (let* ((help-window  (get-buffer-window "*Help*" 0))
         (help-frame   (and help-window (window-frame help-window))))
    (when help-frame (redirect-frame-focus help-frame))))

(defun icicle-run-icicle-pre-command-hook ()
  "Run `icicle-pre-command-hook' functions.
Used in `pre-command-hook'."
  (run-hooks 'icicle-pre-command-hook))

(defun icicle-run-icicle-post-command-hook ()
  "Run `icicle-post-command-hook' functions.
Used in `post-command-hook'."
  (run-hooks 'icicle-post-command-hook))

(defun icicle-set-calling-cmd ()
  "Remember last command that called for completion.
Used in `completion-setup-hook'."
  (setq icicle-cmd-calling-for-completion this-command))

(defun icicle-update-ignored-extensions-regexp ()
  "Update ignored extensions if `completion-ignored-extensions' changed."
  (when (and (icicle-file-name-input-p) ; Defined in `icicles-fn.el'.
             (not (equal icicle-ignored-extensions completion-ignored-extensions)))
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'"))
    ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
    ;; `completion-ignored-extensions' changes.
    (setq icicle-ignored-extensions completion-ignored-extensions)))

;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
(defun icicle-restore-region-face ()
  "Restore region face.  It was changed during minibuffer activity
if `icicle-change-region-background-flag' is non-nil."
  (when icicle-change-region-background-flag
    (set-face-background 'region icicle-saved-region-background)))

(defun icicle-activate-mark ()
  "Prevent region from being deactivated.  Use in `icicle-post-command-hook'."
  (when (and (window-minibuffer-p (selected-window))
             icicle-completing-p
             (not executing-kbd-macro))
    (setq deactivate-mark nil)))

(defun icicle-redefine-standard-commands ()
  "Replace certain standard Emacs commands with Icicles versions."
  (when (and (fboundp 'icicle-completing-read) icicle-redefine-standard-commands-flag)
    (when (fboundp 'bbdb-complete-name)
      (defalias 'bbdb-complete-name         'icicle-bbdb-complete-name))
    (defalias 'customize-apropos            'icicle-customize-apropos)
    (defalias 'customize-apropos-faces      'icicle-customize-apropos-faces)
    (defalias 'customize-apropos-groups     'icicle-customize-apropos-groups)
    (defalias 'customize-apropos-options    'icicle-customize-apropos-options)
    (defalias 'customize-face               'icicle-customize-face)
    (defalias 'customize-face-other-window  'icicle-customize-face-other-window)
    (defalias 'dabbrev-completion           'icicle-dabbrev-completion)
    (defalias 'lisp-complete-symbol         'icicle-lisp-complete-symbol)
    (defalias 'read-from-minibuffer         'icicle-read-from-minibuffer)
    (defalias 'read-string                  'icicle-read-string)
    (defalias 'repeat-complex-command       'icicle-repeat-complex-command)
    ))

(defun icicle-restore-standard-commands ()
  "Restore standard Emacs commands replaced in Icicle mode."
  (when (and (fboundp 'old-completing-read) icicle-redefine-standard-commands-flag)
    (when (fboundp 'old-bbdb-complete-name)
      (defalias 'bbdb-complete-name         'old-bbdb-complete-name))
    (defalias 'customize-apropos            'old-customize-apropos)
    (defalias 'customize-apropos-faces      'old-customize-apropos-faces)
    (defalias 'customize-apropos-groups     'old-customize-apropos-groups)
    (defalias 'customize-apropos-options    'old-customize-apropos-options)
    (defalias 'customize-face               'old-customize-face)
    (defalias 'customize-face-other-window  'old-customize-face-other-window)
    (defalias 'dabbrev-completion           'old-dabbrev-completion)
    (defalias 'lisp-complete-symbol         'old-lisp-complete-symbol)
    (defalias 'read-from-minibuffer         'old-read-from-minibuffer)
    (defalias 'read-string                  'old-read-string)
    (defalias 'repeat-complex-command       'old-repeat-complex-command)
    ))

;;; In Emacs versions before 22:
;;; Save original `read-file-name'.  We redefine it as `icicle-read-file-name' (which calls it).
;;; Then we restore it when you quit Icicle mode.  (In Emacs 22+, no redefinition is needed.)
(unless (or (boundp 'read-file-name-function) (fboundp 'orig-read-file-name))
  (fset 'orig-read-file-name (symbol-function 'read-file-name)))

(defun icicle-redefine-std-completion-fns ()
  "Replace some standard functions with versions for Icicle mode."
  (when (fboundp 'icicle-completing-read)
    (defalias 'choose-completion            'icicle-choose-completion)
    (defalias 'choose-completion-string     'icicle-choose-completion-string)
    (defalias 'completing-read              'icicle-completing-read)
    (defalias 'completion-setup-function    'icicle-completion-setup-function)
    (defalias 'display-completion-list      'icicle-display-completion-list)
    (defalias 'exit-minibuffer              'icicle-exit-minibuffer)
    (when (fboundp 'face-valid-attribute-values)
      (defalias 'face-valid-attribute-values 'icicle-face-valid-attribute-values))
    (defalias 'minibuffer-complete-and-exit 'icicle-minibuffer-complete-and-exit)
    (defalias 'mouse-choose-completion      'icicle-mouse-choose-completion)
    (defalias 'next-history-element         'icicle-next-history-element)
    (defalias 'read-face-name               'icicle-read-face-name)
    (if (boundp 'read-file-name-function) ; Emacs 22+
        (setq icicle-old-read-file-name-fn
              (prog1 (and (not (eq read-file-name-function 'icicle-read-file-name))
                          read-file-name-function)
                (setq read-file-name-function 'icicle-read-file-name)))
      (defalias 'read-file-name             'icicle-read-file-name)) ; Emacs 20, 21
    (when (fboundp 'icicle-read-number)
      (defalias 'read-number                'icicle-read-number))
    (defalias 'switch-to-completions        'icicle-switch-to-completions)
    (when (fboundp 'icicle-completing-read-multiple)
      (defalias 'completing-read-multiple   'icicle-completing-read-multiple)
      (setq crm-local-completion-map  icicle-crm-local-completion-map
            crm-local-must-match-map  icicle-crm-local-must-match-map))
    ))    

(defun icicle-restore-std-completion-fns ()
  "Restore some standard functions that were replaced in Icicle mode."
  (when (fboundp 'old-completing-read)
    (defalias 'choose-completion            'old-choose-completion)
    (defalias 'choose-completion-string     'old-choose-completion-string)
    (defalias 'completing-read              'old-completing-read)
    (defalias 'completion-setup-function    'old-completion-setup-function)
    (defalias 'display-completion-list      'old-display-completion-list)
    (defalias 'exit-minibuffer              'old-exit-minibuffer)
    (when (fboundp 'face-valid-attribute-values)
      (defalias 'face-valid-attribute-values 'old-face-valid-attribute-values))
    (defalias 'minibuffer-complete-and-exit 'old-minibuffer-complete-and-exit)
    (defalias 'mouse-choose-completion      'old-mouse-choose-completion)
    (defalias 'next-history-element         'old-next-history-element)
    (defalias 'read-face-name               'old-read-face-name)
    (if (boundp 'read-file-name-function) ; Emacs 22+
        (setq read-file-name-function
              (and (not (eq icicle-old-read-file-name-fn 'icicle-read-file-name))
                   icicle-old-read-file-name-fn))
      (defalias 'read-file-name             'orig-read-file-name)) ; Emacs 20, 21
    (when (fboundp 'old-read-number)
      (defalias 'read-number                'old-read-number))
    (defalias 'switch-to-completions        'old-switch-to-completions)
    (when (fboundp 'old-completing-read-multiple)
      (defalias 'completing-read-multiple   'old-completing-read-multiple)
      (setq crm-local-completion-map  old-crm-local-completion-map
            crm-local-must-match-map  old-crm-local-must-match-map))
    ))

;; Free vars here: `icicle-saved-kmacro-ring-max' is bound in `icicles-var.el'.
(defun icicle-redefine-standard-options ()
  "Replace certain standard Emacs options with Icicles versions."
  (when (boundp 'icicle-search-ring-max)
    (setq icicle-saved-search-ring-max        search-ring-max ; Save it.
          search-ring-max                     icicle-search-ring-max)
    (setq icicle-saved-regexp-search-ring-max regexp-search-ring-max ; Save it.
          regexp-search-ring-max              icicle-regexp-search-ring-max))
  (when (boundp 'icicle-kmacro-ring-max)
    (setq icicle-saved-kmacro-ring-max        kmacro-ring-max ; Save it.
          kmacro-ring-max                     icicle-kmacro-ring-max)))

(defun icicle-restore-standard-options ()
  "Restore standard Emacs options replaced in Icicle mode."
  (when (boundp 'icicle-saved-search-ring-max)
    (setq search-ring-max        icicle-saved-search-ring-max)
    (setq regexp-search-ring-max icicle-saved-regexp-search-ring-max)))

;; This is used only in Emacs 22+, but we define it always anyway.
(defun icicle-undo-std-completion-faces ()
  "Get rid of standard completion-root highlighting in *Completions*."
  ;; Do this because the standard Emacs 22 highlighting can interfere with
  ;; apropos-completion highlighting.
  (when (fboundp 'face-spec-reset-face)
    (when (facep 'completions-common-part)
      (face-spec-reset-face 'completions-common-part)
      (set-face-attribute 'completions-common-part nil :inherit nil))
    (when (facep 'completions-first-difference)
      (face-spec-reset-face 'completions-first-difference)
      (set-face-attribute 'completions-first-difference nil :inherit nil))))

(defun icicle-bind-isearch-keys ()
  "Bind Isearch commands for Icicle mode."
  (dolist (key icicle-generic-S-tab-keys)
    (define-key isearch-mode-map key
      (lambda ()
        (interactive)
        (isearch-done)
        (icicle-search (point-min) (point-max) (icicle-isearch-complete-past-string) t))))
  (dolist (key icicle-isearch-complete-keys)
    (define-key isearch-mode-map key 'icicle-isearch-complete))
  (cond ((fboundp 'isearch-moccur)      ; Defined in `moccur.el'.
         (define-key isearch-mode-map (kbd "C-o") 'isearch-moccur))
        ((fboundp 'isearch-occur)       ; Defined in `occur-schroeder.el'.
         (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))))

(defun icicle-unbind-isearch-keys ()
  "Unbind Isearch commands for Icicle mode."
  (dolist (key icicle-generic-S-tab-keys) (define-key isearch-mode-map key nil))
  (dolist (key icicle-isearch-complete-keys) (define-key isearch-mode-map key nil))
  (define-key isearch-mode-map "\M-\t" 'isearch-complete)
  (when (fboundp 'isearch-moccur)       ; Restore `moccur.el' binding.
    (define-key isearch-mode-map (kbd "M-o") 'isearch-moccur))
  (define-key isearch-mode-map (kbd "C-o") nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mode.el ends here
