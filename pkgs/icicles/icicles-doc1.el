;;; icicles-doc1.el --- Minibuffer input completion and cycling.
;;
;; Filename: icicles-doc1.el
;; Description: Minibuffer completion and cycling.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Tue Aug  1 14:21:16 1995
;; Version: 22.0
;; Last-Updated: Sun Oct 14 16:09:21 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 22331
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-doc1.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Icicles documentation, part 1.
;;
;;  Files `icicles-doc1.el' and `icicles-doc2.el' contain the doc for
;;  Icicles, including how to install and use Icicles.  You can also
;;  read the Icicles doc, in formatted form, on the Emacs-Wiki Web
;;  site: http://www.emacswiki.org/cgi-bin/wiki/Icicles.  Emacs Wiki
;;  also has a few addtional pages about Icicles.  In particular, if
;;  you are new to Emacs, as well as Icicles, see this page:
;;  http://www.emacswiki.org/cgi-bin/wiki/EmacsNewbieWithIcicles.
 
;;(@* "Installing Icicles")
;;
;;  To use this library:
;;
;;    Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;      (require 'icicles) ; Load this library.
;;      (icicle-mode 1)    ; Turn on Icicle mode.
;;
;;    It is best to add this code *after* any code that creates or
;;    changes key bindings, so Icicles can pick up all of your key
;;    definitions (bindings).  However, if you make new bindings, you
;;    can always exit and then reenter Icicle mode to pick them up.
;;
;;    You will need all of these libraries (loaded by `icicles.el'):
;;
;;      `icicles-cmd.el'
;;      `icicles-doc1.el' (not loaded - doc only)
;;      `icicles-doc2.el' (not loaded - doc only)
;;      `icicles-face.el'
;;      `icicles-fn.el'
;;      `icicles-mac.el'
;;      `icicles-mcmd.el'
;;      `icicles-mode.el'
;;      `icicles-opt.el'
;;      `icicles-var.el'
;;
;;    These libraries are optional, but recommended:
;;
;;      `hexrgb.el'
;;      `icicles-menu.el'
;;      `icomplete+.el'
;;
;;    However, do not load `hexrgb.el' if you do not have a windowing
;;    system (window manager).  It manipulates colors in ways that are
;;    not possible in a console.
;;
;;    Note: Icicles is designed for use with a windowing system; you
;;    probably will not want to use Icicles without one.  In
;;    particular, Icicles takes advantage of keys, such as `S-TAB',
;;    that are unavailable in a console (e.g. xterm).  If you have a
;;    windowing system, but you want to start Emacs from a console
;;    window, consider using an Emacs server and client: `emacsclient'
;;    or `gnuclient'.  If you want to use Icicles with no windowing
;;    system, you will want to rebind several keys - see file
;;    `icicles-mode.el' for key bindings.
;;
;;    It is of course best to byte-compile all of the libraries
;;    (except `icicles-doc1.el' and `icicles-doc2.el').  You will
;;    likely get some byte-compiler warning messages.  These are
;;    probably benign - ignore them.  Icicles is designed to work with
;;    multiple versions of Emacs, and that fact provokes compiler
;;    warnings.  If you get byte-compiler errors (not warnings), then
;;    please report a bug, using `M-x icicle-send-bug-report'.
;;
;;    After startup, you can turn Icicle mode on or off at any time
;;    interactively, using command `icy-mode' (aka `icicle-mode' -
;;    prefix `icy' is unique to this command, so it is easier to
;;    complete).
;;
;;    Note: If you turn on Icicle mode in your init file, it's best to
;;    do so as late as possible - after you or any libraries that you
;;    load do any key binding.  This is because Icicles uses the
;;    current global key bindings to determine which keys to bind for
;;    minibuffer completion and cycling.  To pick up the latest
;;    bindings at any time, you can of course enter Icicle mode
;;    interactively using command `icy-mode' (if necessary, exit, then
;;    re-enter).
 
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
;;  Documentation in file `icicles-doc1.el'
;;  ---------------------------------------
;;
;;  (@> "Nutshell View of Icicles")
;;    (@> "Cycle Completion Candidates")
;;    (@> "Display Completion Candidates")
;;    (@> "Prefix Completion and Apropos Completion")
;;    (@> "Chains of Simple Match Patterns - Progressive Completion")
;;    (@> "Chip Away the Non-Elephant")
;;    (@> "Choose Before You Act")
;;    (@> "Help on Completion Candidates")
;;    (@> "Perform Multiple Operations In One Command")
;;    (@> "Icicles Search")
;;    (@> "Complete Keys Too")
;;    (@> "Available for Almost Any Input")
;;    (@> "Component Icicles Libraries")
;;    (@> "If You Are an Emacs-Lisp Programmer")
;;
;;  (@> "Inserting Text Found Near the Cursor")
;;  (@> "Background on Vanilla Emacs Input Completion")
;;  (@> "Cycling Completions")
;;  (@> "Traversing Minibuffer Histories")
;;  (@> "Apropos Completions")
;;  (@> "Longest-Common-Match Completion")
;;  (@> "Progressive Completion")
;;    (@> "`M-*': Matching Additional Regexps")
;;    (@> "Successive Approximation...")
;;    (@> "`M-&': Satisfying Additional Predicates")
;;
;;  (@> "Inserting a Regexp from a Variable")
;;  (@> "What About Special-Character Conflicts?")
;;  (@> "Alternative Libraries: Other Methods of Choosing Default Values")
;;  (@> "Exiting the Minibuffer Without Confirmation")
;;  (@> "*Completions* Display")
;;  (@> "Icompletion")
;;    (@> "icomplete+.el Displays the Number of Other Prefix Candidates")
;;    (@> "Icicles Highlights the Input that Won't Complete")
;;    (@> "Icompletion in *Completions*: Apropos and Prefix Completion")
;;
;;  (@> "Sorting Candidates and Removing Duplicates")
;;    (@> "Changing the Sort Order")
;;    (@> "Defining New Sort Orders")
;;    (@> "Different Sorts for Different Sorts of Uses")
;;  (@> "Get Help on Candidates")
;;    (@> "Use Candidate Help Like You Use Emacs Command `apropos'")
;;    (@> "Other Icicles Apropos Commands")
;;
;;  (@> "Multi-Commands")
;;    (@> "What Is a Multi-Command?")
;;    (@> "How Does a Multi-Command Work?")
;;
;;  (@> "Key Completion")
;;    (@> "Completing Keys")
;;    (@> "`S-TAB' Is Everywhere - Start With It")
;;    (@> "Completing Keys By Name")
;;    (@> "Completing Prefix Keys")
;;    (@> "Meta Key Bindings")
;;    (@> "Navigate the Key-Binding Hierarchy")
;;    (@> "Local Bindings Are Highlighted")
;;    (@> "Completing Keys By Just Hitting Them")
;;    (@> "Key and Command Help")
;;    (@> "`S-TAB' Is a Multi-Command")
;;    (@> "Possible Source of Confusion")
;;    (@> "Three-Key Emacs")
;;    (@> "Entering Special and Foreign Characters")
;;    (@> "Handling Keymaps That Are Inaccessible From the Global Map")
;;
;;  (@> "Icicles Multi `M-x'")
;;    (@> "Examples of Using Multi `M-x'")
;;      (@* "What about describe-variable and describe-function?")
;;    (@> "Multi `M-x' Turns Every Command into a Multi-Command")
;;
;;  (@> "Choose All Completion Candidates")
;;  (@> "Sets of Completion Candidates")
;;    (@> "Saving and Retrieving Completion Candidates")
;;    (@> "Different Places for Saving and Retrieving Candidates")
;;    (@> "Set Operations")
;;  (@> "Google Matching")
;;    (@> "Domain of Discourse")
;;    (@> "Global Filtering")
;;    (@> "Word Matching and String Matching")
;;    (@> "AND Matching and OR Matching")
;;    (@> "NOT Matching")
;;
;;  (@> "File-Name Input and Locating Files Anywhere")
;;  (@> "Persistent Sets of Completion Candidates")
;;  (@> "Dealing With Large Candidate Sets")
;;  (@> "History Enhancements")
;;    (@> "What Input, What History?")
;;    (@> "Overview of Minibuffer History Enhancements")
;;    (@> "Using Completion to Insert Previous Inputs: `M-o'")
;;    (@> "Putting Previous Candidates First: `C-M-,'")
;;    (@> "Matching Only Historical Candidates: `M-h' and `M-pause'")
;;
;;  (@> "Isearch Completion")
;;
;;  Documentation in file `icicles-doc2.el'
;;  ---------------------------------------
;;
;;  (@> "Icicles Search Commands, Overview")
;;    (@> "Introduction: On Beyond Occur...")
;;    (@> "How Icicles Search Works")
;;    (@> "Why Use 2 Search Patterns?")
;;    (@> "Search Multiple Buffers, Files, and Saved Regions")
;;    (@> "User Options for Icicles Searching")
;;    (@> "Using Regexps With Icicles Search")
;;
;;  (@> "Search and Replace")
;;  (@> "Other Icicles Search Commands")
;;    (@> "Icicles Imenu")
;;    (@> "Compile/Grep Search")
;;    (@> "Input Reuse in Interactive Interpreter Modes")
;;    (@> "Define Your Own Icicles Search Commands")
;;
;;  (@> "Multiple Regions")
;;  (@> "Icicles Info Enhancements")
;;    (@> "Icicles Completion for Info")
;;    (@> "Using Icicle-Search With Info")
;;
;;  (@> "Using Complex Completion Candidates")
;;  (@> "Icicles OO: Object-Action Interaction")
;;    (@> "Apropos Completion")
;;    (@> "M-RET")
;;    (@> "`icicle-object-action' and `icicle-anything'")
;;    (@> "Icicles with Anything")
;;
;;  (@> "Multi-Completions")
;;    (@> "icicle-doc, icicle-vardoc, icicle-fundoc, icicle-plist")
;;    (@> "How Multi-Completions Work")
;;    (@> "Multi-Completions vs `completing-read-multiple'")
;;
;;  (@> "Fuzzy Completion")
;;  (@> "Completion in Other Buffers")
;;    (@> "Dynamic Abbreviation")
;;    (@> "Thesaurus Completion")
;;    (@> "Shell Command Completion")
;;
;;  (@> "Customization and General Tips")
;;    (@> "Using Icicles with Delete Selection Mode")
;;    (@> "Icicles User Options and Faces")
;;
;;  (@> "File-Name and Directory-Name Completion Tips")
;;  (@> "Key Bindings")
;;    (@> "Global Bindings")
;;    (@> "Icicles-Mode Bindings")
;;    (@> "Minibuffer Bindings")
;;
;;  (@> "Customizing Key Bindings")
;;    (@> "Customizing Menu-Bar Menus")
;;    (@> "Customizing Minibuffer Bindings")
;;
;;  (@> "Icicles Redefines Some Standard Commands")
;;  (@> "Programming Multi-Completions")
;;    (@> "Variable icicle-list-use-nth-parts")
;;    (@> "Variable icicle-candidate-properties-alist")
;;    (@> "What You See Is Not What You Get")
;;
;;  (@> "Text Properties in *Completions*")
;;    (@> "Using Property icicle-special-candidate")
;;    (@> "Applying Text Properties to a Candidate String")
;;  (@> "Defining Icicles Commands (Including Multi-Commands)")
;;    (@> "Nothing To It!")
;;    (@> "Multi-Commands Are Easy To Define Too")
;;    (@> "Are Users Dependent on Icicles To Use Multi-Commands?")
;;
;;  (@> "Defining Multiple-Choice Menus")
;;  (@> "Defining Icicles Multi `M-x'")
;;    (@> "How Multi `M-x' is Defined")
;;
;;  (@> "Defining Multi-Commands the Hard Way")
;;  (@> "Global Filters")
;;  (@> "Note to Programmers")
;;  (@> "La Petite Histoire")
;;  (@> "Note on Non-Nil `pop-up-frames' on MS Windows")
 
;;(@* "Nutshell View of Icicles")
;;
;;  Nutshell View of Icicles
;;  ------------------------
;;
;;  Load this library, turn on Icicle mode, and you're good to go.
;;  You can turn Icicle mode off or on at any time with command
;;  `icy-mode'.
;;
;;  Beyond that, the most important thing to know about Icicles is
;;  that you can get help on Icicle mode and minibuffer completion
;;  whenever completion is possible.  You do that either by using item
;;  Help of the menu-bar Icicles menu or Minibuf menu, or by hitting
;;  `C-?' (`icicle-completion-help').
;;
;;  You now know enough to use Icicles.  If you have doc-phobia or are
;;  easily overwhelmed by explanations, then *read no more* - just try
;;  it!  One good way to start is to explore menus Icicles and
;;  Minibuf; you can access most Icicles features using these menus,
;;  without at the same time struggling to learn new key bindings.
;;  The Icicles menu is available all of the time (that is, whenever
;;  you are in Icicle mode), and the Minibuf menu is available
;;  whenever the minibuffer is active.  During minibuffer input
;;  completion, you can also press Control and right-click
;;  (`C-mouse-3') on a completion candidate in buffer *Completions*,
;;  and choose from a contextual menu, Completion Candidate.
;;
;;  If you want a little more explanation than the help page (`C-?'),
;;  then read this section ("Nutshell View of Icicles"), but no more.
;;  This section shows a sample of what you can do in Icicle mode.
;;
;;  If you really want to know more about Icicles by reading instead
;;  of trying, then read beyond this section.  There is a lot you can
;;  learn, but there is not much that you need to learn, to use
;;  Icicles usefully.  Don't be afraid to get in and get wet.  Above
;;  all, don't be overwhelmed by the doc - if it helps, fine.
;;
;;(@* "Cycle Completion Candidates")
;;  ** Cycle Completion Candidates **
;;
;;   M-x  t o o l  next
;;
;;  That is, type "tool" and then hit the `next' key, which is often
;;  labeled "Page Down".  Each time you hit `next', another match for
;;  your input (`tool') replaces it in the minibuffer.
;;
;;   M-x ediff-toggle-use-toolbar  next
;;   M-x scroll-bar-toolkit-scroll next
;;   M-x tool-bar-mode             next
;;   M-x tooltip-mode              next
;;   M-x ediff-toggle-use-toolbar ; Back to the beginning
;;
;;  Keys `next' and `prior' ("Page Up") cycle among all of the
;;  commands that contain (match) the minibuffer input - `tool', in
;;  this case.  Just hit `RET' (Return) when you get to the command
;;  you want.
;;
;;  You can use a regular expression, to narrow the field of matching
;;  inputs:
;;
;;   M-x  i s e . + c h a r   next
;;   M-x isearch-*-char       next
;;   M-x isearch-delete-char  next
;;   ...
;;
;;  See (@> "Cycling Completions") for more about cycling completion
;;  candidates.
;;
;;(@* "Display Completion Candidates")
;;  ** Display Completion Candidates **
;;
;;  You can display all of the matches for the current minibuffer
;;  input, in the *Completions* buffer, with `S-TAB' (Shift TAB).  So,
;;  for instance, `S-TAB' with `M-x ise.+char' in the minibuffer
;;  displays all commands whose names contain `ise' followed
;;  (somewhere) by `char'.
;;
;;(@* "Prefix Completion and Apropos Completion")
;;  ** Prefix Completion and Apropos Completion **
;;
;;  You can get the standard Emacs "prefix" completion, instead of
;;  this "apropos completion", by using `TAB' instead of `S-TAB'.  You
;;  can cycle prefix-completion candidates by using the `up' and
;;  `down' arrow keys instead of `next' and `prior'.
;;
;;  See (@> "Apropos Completions") for more about apropos and prefix
;;  completion.
;;
;;(@* "Chains of Simple Match Patterns - Progressive Completion")
;;  ** Chains of Simple Match Patterns - Progressive Completion **
;;
;;  To see which functions contain `char', `delete', and `back' in
;;  their names, in any order:
;;
;;   C-h f  c h a r  S-TAB - Display all function names that contain
;;   `char'.
;;
;;   M-*  d e l e t e  - Narrow that set of names to those that also
;;   contain `delete'.
;;
;;   M-*  b a c k  - Narrow the set of matching names further, to
;;   those that also contain `back'.
;;
;;  This displays a list of functions like this in buffer
;;  *Completions* (your list might be somewhat different):
;;
;;    backward-delete-char        backward-delete-char-untabify
;;    delete-backward-char        icicle-backward-delete-char-untabify
;;    icicle-delete-backward-char
;;    quail-conversion-backward-delete-char
;;
;;  Since you are completing input to `C-h f', you can then cycle to a
;;  name using `next' and hit `RET', or click `mouse-2', to see the
;;  doc for that function.  If, instead, you were completing input to
;;  `M-x', you could choose a command to execute.  And so on.
;;
;;  The thing to notice here is that you can use `M-*' to input chains
;;  of multiple simple regexps, to narrow down the set of completion
;;  candidates progressively.  This is analogous to piping the result
;;  of `grep' to another `grep', and piping that result to another
;;  `grep'...
;;
;;  Here are a couple others to try (I'm always forgetting the order
;;  in these compound names):
;;
;;   C-h f  w i n d o w  S-TAB M-*  f r a m e
;;
;;   C-h f  w i n d o w  S-TAB M-*  b u f f e r
;;
;;  As a shortcut, you can use just `S-SPC' instead of `S-TAB M-*'.
;;  See (@> "Progressive Completion") for more about progressive
;;  completion with `M-*'.
;;
;;(@* "Chip Away the Non-Elephant")
;;  ** Chip Away the Non-Elephant **
;;
;;  There's a joke about a sculptor who, when asked how he created
;;  such a life-like statue of an elephant, said that he just chipped
;;  steadily away, removing marble that didn't resemble an elephant.
;;
;;  Icicles lets you sculpt this way too - it is in fact a common
;;  Icicles usage idiom.  There are two ways to say, "I don't want
;;  that" when it comes to possible completions:
;;
;;  * The `delete' key or `S-mouse-2' says, "Get rid of this
;;    completion candidate."
;;
;;  * `C-~' says "I want all possible completions *except* those that
;;    are the current matches.  That is, "Remove all of this, and let
;;    me see what's left."  `C-~' takes the complement of the current
;;    set of matches, using the initial set of possible candidates as
;;    the universe of discourse.
;;
;;  In other words, instead of coming up with input that you want a
;;  completion to match, get rid of one or all of the candidates that
;;  do match.  You can keep clicking `mouse-2' while holding Shift, or
;;  keep hitting `delete' (without Shift), to chip away at the set of
;;  possible completions.  If there are several candidates in a row
;;  that you want to eliminate, just hold down the `delete' key until
;;  they're gone.
;;
;;  So that you can use `delete' this way to delete candidates one
;;  after the other, in order, the next candidate is chosen each time
;;  you delete one.  This means that it becomes the current candidate
;;  in the minibuffer.  You can, however, use `C-l' (bound to command
;;  `icicle-retrieve-previous-input') to clear the minibuffer and
;;  retrieve your last real input - see (@> "History Enhancements").
;;
;;  `delete' works well to delete isolated candidates or groups of
;;  candidates that are in order, one right after the other, and you
;;  can of course combine it with positive matching.
;;
;;  Note: In Emacs releases prior to Emacs 22, `delete' has no real
;;  effect on file-name completion candidates.  It removes them
;;  temporarily, but they are not really removed as possible
;;  candidates, so `TAB' and `S-TAB' will still show them as
;;  candidates.
;;
;;  `C-~' is particularly handy in combination with progressive
;;  completion (`M-*') to narrow down a set of candidates, especially
;;  when you are not exactly sure what you are looking for.  You can
;;  repeat `C-~' with different inputs to eliminate everything matched
;;  by each of them.  In other words, this is a variable-size chisel,
;;  and you can use it to remove very large chips.
;;
;;  For instance, suppose you are looking for a standard Emacs command
;;  involving buffers.  You try `M-x buff S-TAB', but that shows
;;  zillions of matches.  Suppose that you know you don't want a
;;  command in some 3rd-party package.  You proceed to eliminate
;;  those, progressively, using something like this:
;;
;;    M-* ediff C-~ ibuffer C-~ icicle C-~ Buffer-menu C-~ ps- C-~
;;        ido C-~ search-buffers C-~ moccur C-~ swbuff C-~
;;
;;  And so on.  That is, instead of using `M-*' repeatedly to specify
;;  multiple patterns that candidates must match, you use `C-~'
;;  repeatedly (after an initial `M-*'), to chip away candidates you
;;  don't want.  You could, alternatively, hold down the `delete' key
;;  to eliminate each of these groups of command names.  There are
;;  over 100 commands whose names begin with `ediff', however, so `M-*
;;  C-~' can be quicker in that case.  It can definitely be quicker
;;  when apropos matching is involved.  And you can of course combine
;;  the fine chiseling of `delete' with the variable-size chiseling of
;;  `C-~'.
;;
;;  See (@> "Sets of Completion Candidates" for more about `C-~'.
;;
;;(@* "Choose Before You Act")
;;  ** Choose Before You Act **
;;
;;  The opposite operation from chipping away at a set of candidates
;;  to refine it is to build up a set of candidates that you want to
;;  act on.  This too is easy with Icicles.  In some user interfaces,
;;  including Dired in Emacs, you can mark items in a checklist and
;;  then, when you've selected the items you want and verified the
;;  list, act on those that are selected.  You might do this, for
;;  instance, if you were deleting some files.  Icicles lets you
;;  interact with completion candidates this same way.
;;
;;  You do this by building up a saved set of candidates, and then
;;  retrieving these saved candidates later.  You can use the
;;  retrieved candidates just as you would any current set of
;;  candidates.  One of the things you can do is act on all of them,
;;  that is, act on each, in turn.  You do that with `C-!'.
;;
;;  Of course, if you can use a regexp to match exactly the candidates
;;  you want to act on, then you need not bother to save and retrieve
;;  them, before acting on them: you can see them all alone in buffer
;;  *Completions*.  Here's an exercise in choosing candidates to save
;;  with the mouse in *Completions*:
;;
;;  C-x C-f  i c i  TAB - Match all file names that begin with `ici'.
;;
;;  Click `mouse-1' inside (or to the left of) `icicles-face.el'.
;;  Click `mouse-3' inside (or to the right of) `icicles-mode.el'.
;;  Click `mouse-3' again, in the same place.
;;  Click `M-S-mouse-2' on each of `icicles.el' and `icicles-cmd.el'.
;;
;;  The candidates that you selected - those between `icicles-face.el'
;;  and `icicles-mode.el', inclusive, plus `icicles.el' and
;;  `icicles-cmd.el', are highlighted specially in buffer
;;  *Completions*, and feedback in the minibuffer tells you that they
;;  were saved.
;;
;;  Next, use `C-M-<'.  This retrieves the set of saved candidates;
;;  that is, it replaces the current set of candidates with the saved
;;  candidates.  If you now use `C-!', it applies the action to each
;;  candidate.  In this case, the action is to visit the file (`C-x
;;  C-f').
;;
;;  See (@> "Choose All Completion Candidates") for more about `C-!'.
;;  See (@> "Sets of Completion Candidates") for more about saving and
;;  retrieving sets of candidates.
;;
;;(@* "Help on Completion Candidates")
;;  ** Help on Completion Candidates **
;;
;;  Sometimes, you'd like to be able to ask for help about individual
;;  completion candidates while you're in the process of choosing one.
;;  That's the purpose of the Icicles `C-M-' key bindings available
;;  during completion.
;;
;;  The simplest such bindings are `C-M-RET' and `C-M-mouse2'.  They
;;  each do the same thing: provide help on the current candidate.
;;  You can use them during cycling or whenever you've narrowed the
;;  choice down to a single candidate.  You can check this way, before
;;  you execute a command you're unsure of.
;;
;;  During completion, you can also cycle among the doc strings for
;;  the candidates that match your input, using `C-M-down' and
;;  `C-M-up' (for prefix matching), `C-M-next' and `C-M-prior' (for
;;  apropos matching).  This gives you a very useful on-the-fly
;;  apropos feature - use it while you're completing a command, to
;;  check the difference between several possible commands.  Or just
;;  use it to browse doc strings, to learn more about Emacs.
;;
;;(@* "Perform Multiple Operations In One Command")
;;  ** Perform Multiple Operations In One Command **
;;
;;    C-x C-f  i c i  TAB - Find a file whose name starts with `ici'.
;;
;;    down (that is, down arrow) ... until you get to icicles-cmd.el
;;
;;    RET - Open file icicles-cmd.el.
;;
;;  Nothing new here.  Now try the same thing, but use `C-RET' instead
;;  of `RET'.  The command is not ended, and you can continue to
;;  choose files to open:
;;
;;    C-x C-f  i c i  TAB - Find a file whose name starts with `ici'.
;;
;;    down ... until you get to icicles-cmd.el
;;
;;    C-RET - Open file icicles-cmd.el.
;;
;;    down ... until you get to icicles-opt.el
;;
;;    C-RET - Open file icicles-opt.el.
;;
;;    down ... until you get to icicles.el
;;
;;    RET - Open file icicles.el (end).
;;
;;  You just opened three files in a single command.  Command
;;  `icicle-find-file' (`C-x C-f') is an Icicles multi-command.  You
;;  can tell if a command is a multi-command when you execute it - if
;;  so, the input prompt is prefixed by `+'.  So, for example, when
;;  you used `C-x C-f', the prompt was "+ File or directory:".
;;  Icicles menu items that are multi-commands are also prefixed by
;;  `+'.
;;
;;  By the way, you can also tell when a command allows input
;;  completion and, if so, whether it is strict or lax (permissive)
;;  completion.  The colored box character just to the left of the
;;  prompt (after the plus sign, in the case of a multi-command) shows
;;  this: A red box with an equals sign (`=') means your input must
;;  match one of the completion candidates (strict completion).  An
;;  empty, cyan box means your input need not match (lax completion).
;;
;;  In addition to using `down' (or `next') and choosing (acting on)
;;  candidates with `C-RET', you can combine these operations by using
;;  `C-down' (or `C-next'): act on candidates in succession.  And, as
;;  mentioned, you can use `C-!'  to act on all candidates at once.
;;
;;  There are many possible uses of multi-commands.  They all make use
;;  of the same key bindings, which begin with `C-'.  These keys are
;;  analogous to the `C-M-' keys that provide help on completion
;;  candidates.
;;
;;  See (@> "Multi-Commands") for more information about Icicles
;;  multi-commands.
;;
;;(@* "Icicles Search")
;;  ** Icicles Search **
;;
;;  Icicles provides a unique way of searching.  Command
;;  `icicle-search' (`C-c `') is a multi-command.  In this case, the
;;  completion candidates are the buffer occurrences that match a
;;  regexp that you input.  `C-RET' visits a search-hit candidate, and
;;  `C-next' visits a candidate and prepares to visit the next in
;;  succession.  If you visit file `icicles-doc1.el', which contains
;;  the text you are reading now, do this in that buffer:
;;
;;    C-c `
;;    Search for (regexp): . * r e c u r s i v e . *  RET - Search for
;;    regexp `.*recursive.*'.
;;
;;    Choose an occurrence: S-TAB - Show the search hits, in buffer
;;    *Completions* (optional).
;;
;;    C-next ... - Cycle among the search hits, navigating to them in
;;    turn.
;;
;;    S-TAB next ... - Cycle among the search hits without navigating.
;;
;;    next ... C-RET next ... C-RET - Cycle to particular hits and
;;    visit (only) those hits.
;;
;;    next ... RET - Cycle to a hit and stay there (end).
;;
;;    
;;    C-c `
;;    Search for (regexp): M-p RET - Search again for `.*recursive.*'
;;    (input history).
;;
;;    S-TAB  e d i t  C-next ... - Search for substring `edit' within
;;    all search hits for `.*recursive.*'.  Cycle among the matches.
;;    The longest common substring is`abort-recursive-edit', so that
;;    is highlighted inside the (differently) highlighted
;;    `.*recursive.*' hits.  Whatever you type filters the initial set
;;    of hits.
;;
;;    M-k - Empty the minibuffer, then S-TAB.  All `.*recursive.*'
;;    hits are restored as candidates.  Again, whatever your input is
;;    (nothing, now), the set of candidates is dynamically updated to
;;    match it.
;;
;;    b \ w + g S-TAB C-next ... - Search for matches of regexp
;;    `b\w+g' within all search hits for `.*recursive.*' - that is,
;;    `b' followed by at least one other word character, followed by
;;    `g'.  Whatever the regexp `b\w+g' matches (`bstring',
;;    `bypassing') is highlighted inside each candidate.
;;
;;    RET - Stop searching at the current candidate (end).
;;
;;  Now try the same thing, but first select some text.  The search is
;;  confined to the active region (selection) instead of the entire
;;  buffer.
;;
;;  Now try the same thing (without a region), but use a negative
;;  prefix argument such as `C--' with `C-c `'.  This time, after you
;;  input the regexp to search for, you are prompted for one or more
;;  files to search.  This too is multi-command input: you can input
;;  any number of file names, using completion.
;;
;;    C-- C-c `
;;    Search for (regexp): . * r e c u r s i v e . *  RET - Search for
;;    regexp `.*recursive.*'.
;;
;;    Choose file (`RET' when done): i c i TAB - Choose among file
;;    candidates that begin with `ici' (shown in *Completions*).
;;
;;    C-! - Choose all matching file names: icicles-cmd.el,
;;    icicles-doc1.el, icicles-doc2.el, icicles-face.el,
;;    icicles-fn.el, icicles-mac.el, icicles-mcmd.el, icicles-mode.el,
;;    icicles-opt.el, icicles-var.el, and icicles.el.
;;
;;    Choose an occurrence: S-TAB - Show the hits in buffer
;;    *Completions* (optional).
;;
;;    C-next ... - Cycle among the search hits in all chosen
;;    files...
;;
;;  Just as you can choose particular search hits to visit, using
;;  `C-RET', so you can use `C-RET' to choose particular files (whose
;;  names match the input, e.g. ici) to search.  Just as you can visit
;;  search hits in order, using `C-next' (or `C-down'), so you can use
;;  `C-next' (or `C-down') to choose files to visit, one after the
;;  other.
;;
;;  See (@> "Icicles Search Commands, Overview") for more information
;;  about searching with Icicles.
;;
;;(@* "Complete Keys Too")
;;  ** Complete Keys Too **
;;
;;  Try `S-TAB' at the top level (without first invoking a command
;;  that reads input).  Icicles presents all of the possible keys and
;;  their bindings in the current context - for completion.  For
;;  example, if you are in Dired mode, the completion candidates
;;  include all key sequences in the global map and the Dired-mode map
;;  (and any current minor-mode maps, such as Icicle mode).
;;
;;  You can then type part of a key name or a command name, and hit
;;  `S-TAB' again to apropos-complete your input.  You can navigate
;;  down the key-sequence hierarchy by completing a key sequence piece
;;  by piece:
;;
;;    S-TAB to see the available keys at top level
;;
;;    Click (using `mouse-2') candidate `C-x  =  ...', to see the keys
;;    that start with `C-x'
;;
;;    Click `r  =  ...', to see the keys that start with `C-x r'
;;
;;    Click `b  =  bookmark-jump', to invoke that command and visit a
;;    bookmark
;;
;;  Whenever you're completing a prefix key, such as `C-x', you can
;;  click `..' to navigate back up the key-sequence hierarchy.  For
;;  instance, if you are completing `C-x p', click `..' to go back to
;;  completing `C-x', and then click `..' to go back to the top level.
;;
;;  The available keys at any level include the following important
;;  keys, which means that you can use Icicles key completion to do
;;  almost anything in Emacs:
;;
;;  * `M-x' - Execute an arbitrary command (`M-x' is treated as
;;    `ESC-x', so complete first `ESC  =  ...', then
;;    `x  =  icicle-execute-extended-command')
;;
;;  * `M-:' - Evaluate any Emacs-Lisp expression (again, this is
;;    treated as `ESC-:', so complete first `ESC  =  ...', then
;;    `:  =  eval-expression')
;;
;;  * `menu-bar  =  ...' - Invoke any menu-bar menu (continue
;;    completing, to navigate the entire menu hierarchy)
;;
;;  You can start directly with a key prefix, and then hit `S-TAB' to
;;  complete it - you need not start with `S-TAB'.  You can use
;;  Icicles key completion to learn key bindings - `C-M-mouse-2'
;;  displays help on any key.
;;
;;  Instead of clicking a completion candidate with `mouse-2', you can
;;  of course type part of the key name or command name, and then
;;  complete the name and enter it.  Gotcha: `S-TAB' uses apropos
;;  completion, by default, so remember that typing `.' matches any
;;  character (except a newline).  To match only `..', either use
;;  prefix completion (`TAB') or escape the regexp special character:
;;  `\.\.' (or use `^\.').
;;
;;  See (@> "Key Completion") for more about Icicles key completion.
;;
;;(@* "Available for Almost Any Input")
;;  ** Available for Almost Any Input **
;;
;;  All of this works not only for the input of commands, with `M-x',
;;  but for the input of nearly anything.  For instance, you can use
;;  `C-x b' (`switch-to-buffer') and cycle among buffer names.  Or use
;;  `C-h v' (`describe-variable') and cycle among variable names.  It
;;  works whenever a command reads input with completion.
;;
;;  Whenever you're in Icicle mode, you see "Icy" in the mode-line.
;;
;;(@* "Component Icicles Libraries")
;;  ** Component Icicles Libraries **
;;
;;  Icicles is composed of the following libraries.  When you load the
;;  driver library, `icicles.el', the others are all loaded
;;  automatically .
;;
;;    `icicles.el'      - driver library
;;    `icicles-doc1.el' - first part of the doc (this!)
;;    `icicles-doc2.el' - second part of the doc
;;    `icicles-cmd.el'  - top-level commands
;;    `icicles-face.el' - faces
;;    `icicles-fn.el'   - non-interactive functions
;;    `icicles-mac.el'  - macros
;;    `icicles-mcmd.el' - minibuffer commands
;;    `icicles-mode.el' - Icicle mode definition
;;    `icicles-opt.el'  - user options (variables)
;;    `icicles-var.el'  - internal variables
;;
;;  Libraries `icicles-doc1.el' and `icicles-doc2.el' are not really
;;  libraries.  They contain only comments, with the Icicles doc.
;;
;;  Library `icicles-menu.el' is a separate Icicles library.  It does
;;  not require any of the other libraries, and they don't require
;;  `icicles-menu.el', but `icicles-menu.el' is especially useful when
;;  used with the other libraries.
;;
;;(@* "If You Are an Emacs-Lisp Programmer")
;;  ** If You Are an Emacs-Lisp Programmer **
;;
;;  If you are an Emacs-Lisp programmer, this is the no-brainer,
;;  nutshell view of how to take advantage of Icicles in your own code
;;  that calls `completing-read' or `read-file-name':
;;
;;    Add this line to your library: (require 'icicles nil t)
;;
;;  That's really all you need to do.  And there is no consequence if
;;  users don't have Icicles (no load error is raised, because of the
;;  non-nil third argument).  In other words, there is no reason not
;;  to add this soft `require', unless your library somehow conflicts
;;  with Icicles features.  (Even then, users will not be affected
;;  unless they turn on Icicle mode.)
;;
;;
;;  For more (and there is a lot more), read on...
 
;;(@* "Inserting Text Found Near the Cursor")
;;
;;  Inserting Text Found Near the Cursor
;;  ------------------------------------
;;
;;  Most of Icicles is about completing text that you type in the
;;  minibuffer against some set of possible completion candidates.
;;  This feature is not.  It is related only in the sense that it is
;;  also about inputting text that is already available elsewhere.
;;
;;  Some Emacs commands provide, as the default value for minibuffer
;;  input, a word or other text at the cursor position (aka "point").
;;  You can insert this default value in the minibuffer with `M-n'.
;;  Icicles option `icicle-init-value-flag' can be used to
;;  automatically insert the default value into the minibuffer as an
;;  initial value, if you prefer that behavior (I do; many people do
;;  not).
;;
;;  Sometimes you would like to use the text at the cursor, but the
;;  command asking for input does not let you retrieve that text as
;;  the default value.  For example, if the text at point is a file
;;  name, you might like to use it with `C-x C-f' to open that file.
;;  Or, if the text is a URL, you might want to visit it using a Web
;;  browser.
;;
;;  Some Emacs-Lisp libraries, such as `ffap.el', have as their
;;  specific purpose to help you do this.  "Ffap" stands for
;;  `find-file-at-point', the main command in that library.  It tries
;;  to interpret the text at point and "do the right thing" with it:
;;  visit a file, open a URL in a Web browser, and so on.
;;
;;  If you like, you can use library `ffap.el' with Icicles.  All
;;  Icicles features are then available during file-name and URL
;;  completion.  And if you like `ffap.el', you might also like to try
;;  my extension library `ffap-.el'.  However, if you use ffap with
;;  Icicles, you might not want to use the ffap key bindings,
;;  preferring the Icicles bindings or the standard Emacs bindings for
;;  keys such as `C-x C-f'.  (In that case, do not call function
;;  `ffap-bindings'.)
;;
;;  Icicles provides a simple way to take advantage of `ffap-guesser',
;;  the ffap function that guesses which string at the cursor position
;;  you want to grab, without sacrificing any key bindings to ffap.
;;  Just use `M-.' (command `icicle-insert-string-at-point') at any
;;  time in the minibuffer.  It grabs text at or near the cursor and
;;  yanks it into the minibuffer.  By default, the text it grabs is
;;  whatever `ffap-guesser' guesses.
;;
;;  Using `M-.' on demand, instead of binding keys to ffap commands,
;;  lets you control which buffer text you use as minibuffer input and
;;  how that text should be interpreted (file name, URL, and so on).
;;  By default, `M-.' uses `ffap-guesser', but you can change this by
;;  customizing user option `icicle-thing-at-point-functions'.
;;
;;  Actually, `M-.' acts differently if you use it successively.
;;  Successive uses of `M-.' grab and insert either 1) alternative
;;  bits of text (different text "things") or 2) successive bits of
;;  text.  The default behavior is #1, but you can change this choice
;;  by customizing option `icicle-default-thing-insertion' (setting it
;;  to `more-of-the-same', instead of `alternatives').
;;
;;  As an example of grabbing successive bits of text (#2), suppose
;;  that the cursor is at the beginning of the word "use" in the
;;  previous paragraph.  Then, during minibuffer input, suppose that
;;  you use `M-. M-. M-.'.  Each time you hit `M-.', another word is
;;  inserted in the minibuffer:
;;
;;    use
;;    use it
;;    use it successively
;;    ...
;;
;;  The rest of this section is a bit technical, so you might want to
;;  skip it if you are reading the Icicles doc for the first time.  It
;;  details the behavior and definitions of options
;;  `icicle-default-thing-insertion' and
;;  `icicle-thing-at-point-functions', and how to temporarily override
;;  those settings interactively.
;;
;;  Option `icicle-thing-at-point-functions' controls which text at or
;;  near the cursor `M-.' inserts into the minibuffer.  It is a cons
;;  cell, that is, an ordered pair:
;;
;;  * The car (first part) is a list of functions that grab different
;;    kinds of strings at or near point (#1, above).  Any number of
;;    functions can be used.  They are used in sequence by `M-.'.  By
;;    default, there are four functions, which alternatively grab:
;;
;;    1) whatever `ffap-guesser' returns (e.g. file name at point)
;;    2) the symbol or file name at point
;;    3) the word at point
;;    4) the URL at point
;;
;;  * The cdr (second part) is a function that advances point one text
;;    thing (#2, above).  Each time command `M-.' is used
;;    successively, this is called to grab more things of text (of the
;;    same kind).  The default function grabs successive words.
;;
;;  If either the car or cdr is empty, then the other alone determines
;;  the behavior of `M-.'.  Otherwise, option
;;  `icicle-default-thing-insertion' determines whether the car or the
;;  cdr is used by `M-.'.
;;
;;  For example, if the value of `icicle-default-thing-insertion' is
;;  `alternatives' (the default value), then repeated use of `M-.'
;;  inserts a different kind of thing at point: ffap guess, file name,
;;  word, or URL.  If you set `icicle-default-thing-insertion' to
;;  `more-of-the-same', then repeated use of `M-.' inserts successive
;;  words into the minibuffer, as shown in the example above.
;;
;;  You need not make a final choice once and for all between
;;  `alternatives' and `more-of-the-same'.  You can also make an
;;  interactive choice by using a prefix argument (`C-u') at any time
;;  to override the value of `icicle-default-thing-insertion'.  If you
;;  use plain `C-u', then `M-.' inserts alternative strings.  If you
;;  use a numeric prefix argument N (not just plain `C-u'), then it is
;;  the same as using `M-.' N times with `more-of-the-same' as the
;;  value of `icicle-default-thing-insertion'.
;;
;;  And, if the numeric argument is negative, then text is grabbed to
;;  the left of the cursor, instead of to the right.  In the example
;;  above, if you used `M-- M-. M-. M-.', then the successive
;;  insertions would be as follows:
;;
;;  differently
;;  differently if
;;  differently if you
;;  ...
;;
;;  If you used `M--3 M-.', then you would immediately insert
;;  `differently if you'.
;;
;;  In the case of `alternatives', there are four possibilities, by
;;  default.  The first function in the list is `ffap-guesser'.  The
;;  second function grabs text that has the syntax of an Emacs-Lisp
;;  symbol name, which in practice can also be a file name or a URL -
;;  it can include characters such as -, /, +, ., :, @, and _.  The
;;  third function grabs a word, which includes letters, ' and -.  The
;;  fourth function grabs a URL, adding prefix "http://" if needed.
;;  These are the functions used by default, but you can add to them
;;  or replace them.
;;
;;  If you use my library `thingatpt+.el', then the cursor need not be
;;  exactly on the text for the second and third alternatives - the
;;  symbol or word *nearest* the cursor is grabbed.
;;
;;  See Also:
;;
;;  * (@> "Inserting a Regexp from a Variable") for information on
;;    inserting a variable's string value.
;;
;;  * (@> "Moving Between the Minibuffer and Other Buffers") for
;;    another way to insert buffer text in the minibuffer.
 
;;(@* "Background on Vanilla Emacs Input Completion")
;;
;;  Background on Vanilla Emacs Input Completion
;;  --------------------------------------------
;;
;;  This section reviews standard Emacs behavior regarding input
;;  completion.  It does not describe any Icicles completion features.
;;
;;  When you are prompted in the minibuffer to enter something, you
;;  are sometimes presented with a default value.  This might be
;;  automatically inserted after the prompt, initially.  If not, you
;;  can retrieve the default value yourself, using `M-n' (Emacs 21 or
;;  later).
;;
;;  Often, there is more than one reasonable default value that might
;;  make sense.  Depending on what you're being asked to enter, these
;;  "candidate default values" might be command names, buffer names,
;;  existing file names, variable names, and so on.
;;
;;  For most Emacs functions that prompt you for input, the person who
;;  wrote the function decided on the reasonable set of default
;;  values, and passed these to an "input-completing function" such as
;;  `completing-read' or `read-file-name', which prompts you and reads
;;  your input.  The programmer also decided whether you will be
;;  *required* to pick one of the default values or you will be free
;;  to enter something else.  The programmer might also have told the
;;  input-completing function to require that your input pass some
;;  special test (predicate).
;;
;;  Be aware that standard Emacs terminology does not refer to such a
;;  set of default values as "default values"; they are called
;;  "completions".  By "default value", standard Emacs terminology
;;  means only the single value that you can access via `M-n'.
;;  Icicles refers to all such potential inputs indifferently as
;;  "default values", "completions", "completion candidates", and
;;  "candidates".  Whenever completion is not requiring you to pick
;;  one of the available candidates, they are effectively only default
;;  choices.
;;
;;  So, how do you get access to the default values that the
;;  programmer has made available to you, in order to choose one?  You
;;  hit certain keys to complete the current contents of the
;;  minibuffer (excluding the prompt).  This current, partial input is
;;  considered as a prefix of one of the default values, and it is
;;  completed in the minibuffer to the entire default value
;;  (completion).
;;
;;  Keys `TAB', `RET' (Return), and `SPC' (Space) perform different
;;  degrees of this "prefix completion" in standard Emacs.  If you
;;  type a prefix of one of the available default values, you can
;;  complete the value this way in the minibuffer, and then enter
;;  (commit) it, using `RET'.
;;
;;  But if your partial input matches the prefix of more than one
;;  default value, then completion pops up the list of all matching
;;  completions for you to choose from (in buffer *Completions*).  You
;;  choose a candidate by clicking it with `mouse-2' or placing the
;;  cursor on it and hitting `RET'.
;;
;;  Because this is the way you access the default values supplied to
;;  an input-completing function, I call those values
;;  "prefix-completion candidates".  If there is no partial input yet
;;  (empty minibuffer), then the entire list of default values
;;  supplied to the input-completing function appears in the pop-up
;;  *Completions* buffer.  See the Emacs manual (`C-h i') for more on
;;  this general mechanism of prefix completion (called simply
;;  "completion" there).
;;
;;  Calls to `completing-read' and `read-file-name' are not the only
;;  places where input completion is used.  When you use `M-x'
;;  (command `execute-extended-command'), completion is also
;;  available.
 
;;(@* "Cycling Completions")
;;
;;  Cycling Completions
;;  -------------------
;;
;;  Icicles lets you use the `up' and `down' arrow keys or `C-p' and
;;  `C-n' to cycle through the list of candidate prefix completions
;;  that match whatever input is present in the minibuffer (or all
;;  candidate completions, if there is no input in the minibuffer).
;;  In the minibuffer, each candidate replaces your partial input, in
;;  turn, when you cycle.  The prefix (root) that was completed is
;;  underlined in the minibuffer completion candidate.
;;
;;  Suppose you use `C-x b' (command `switch-to-buffer').  You can
;;  then use `C-n' until the right buffer name appears in the
;;  minibuffer, then hit `RET'.  Or you can type some text that begins
;;  one or more of the buffer names, and then use `C-n' to cycle among
;;  those names that match that prefix.  If there are many candidates,
;;  typing part of the name to narrow the field can save time.
;;
;;  Another example: Suppose you use `C-h v' (`describe-variable') and
;;  type `cal'.  Use `C-n' to cycle among all variables that start
;;  with `cal', until you find the one you want (then hit `RET').
;;
;;  In other words, the current partial input in the minibuffer
;;  determines a matching set of default values, and those are the
;;  values that you can cycle through.  You can at any time erase or
;;  change the partial input - the list of matching candidates
;;  automatically reflects the change.
;;
;;  This also means that it's good to have a quick way to clear the
;;  minibuffer of any input, so Icicles also provides minibuffer key
;;  binding `M-k' to do that.
;;
;;  A visible and audible signal lets you know when you have reached
;;  one end of the list of completion candidates, but you can of
;;  course continue to cycle, wrapping around.
;;
;;  If the completion candidates are already displayed in buffer
;;  *Completions* when you try to cycle among them (because you hit
;;  `TAB'), then the current candidate is highlighted in *Completions*
;;  as you access it in the minibuffer with the `up' and `down' arrow
;;  keys.  If you change the minibuffer input, then the *Completions*
;;  list is updated accordingly, to reflect the new set of matching
;;  candidates.  The root that was completed (the minibuffer input) is
;;  highlighted in each candidate of the *Completions* display.  The
;;  *Completions* window is automatically scrolled as needed, to show
;;  the current candidate.
;;
;;  Don't become a cycling drone!  Input some text to narrow the set
;;  of candidates, before cycling among them to choose one.  This is a
;;  good habit to adopt, generally, in Icicles.  Most of the power of
;;  Icicles comes in your ability to filter a set of candidates.  This
;;  is especially true when it comes to regexp filtering (see
;;  (@> "Apropos Completions")).
;;
;;  Cycling and filtering work hand in hand.  If the set of candidates
;;  is small to begin with, then just cycling might be quick enough -
;;  that is the case if you move among a small set of buffers, for
;;  instance.  But with Icicles you can profitably use cycling on even
;;  a very large set of candidates - by filtering the set first.  The
;;  reason this is not very practical with vanilla Emacs is that
;;  filtering by a prefix only is not very potent.
;;
;;  Tip: Whenever you type or delete text in the minibuffer, your
;;  partial input is remembered.  When you cycle completion
;;  candidates, your input is replaced by each candidate, but you can
;;  at any time refresh the minibuffer to retrieve what you last
;;  typed.  You do this with `C-l', which is bound in the minibuffer
;;  to command `icicle-retrieve-previous-input'.  Editing a completion
;;  candidate that you have cycled into the minibuffer counts as
;;  input.  Editing tells Icicles to remember what is in the
;;  minibuffer as your last real input.  If you want to replace the
;;  candidate and go back to editing the input you had already typed
;;  before cycling, then use `C-l' - don't just delete characters from
;;  the candidate.  See (@> "History Enhancements").
;;
;;  You can change the keys that are bound to completion-candidate
;;  cycling (`up', `down', `C-p', and `C-n') - see 
;;  (@> "Customizing Key Bindings").
;;
;;  If you are an Emacs-Lisp programmer, then you can use
;;  `completing-read' and `read-file-name' to define your own
;;  commands, enabling them to take advantage of Icicles completion
;;  and cycling.  The definition of command `icicle-recent-file' is a
;;  good model to follow.  Emacs has a `recentf-mode' that lets you
;;  open recently accessed files.  But this mode makes you open a file
;;  using a menu interface.  Command `icicle-recent-file' lets you use
;;  the usual `find-file' minibuffer interface, with completion and
;;  cycling among your recent files.  See sections 
;;  (@> "Defining Icicles Commands") and (@> "Note to Programmers")
;;  for more on defining your own commands with `completing-read' and
;; `read-file-name'.
 
;;(@* "Traversing Minibuffer Histories")
;;
;;  Traversing Minibuffer Histories
;;  -------------------------------
;;
;;  Perhaps you are already used to accessing past inputs using the
;;  `down' and `up' arrow keys, `C-n' and `C-p', `M-n' and `M-p', or
;;  `next' and `prior'.  If not, try it.  You can go backward and
;;  forward in the minibuffer histories (there are different history
;;  lists for different kinds of input).  You can't really cycle them
;;  (with wraparound), but when you get to one end you can reverse the
;;  direction.
;;
;;  Anyway, the input-cycling behavior that Icicles offers is in
;;  addition to this standard traversal of histories.  Since there
;;  are, by default, several extra pairs of keys used for history
;;  traversal, rebinding some of them to use for Icicles completion is
;;  no real loss.
;;
;;  By default, Icicles rebinds all of the key sequences that you
;;  normally use for commands `next-line' and `previous-line', so that
;;  they perform prefix-completion cycling in the minibuffer.  In
;;  vanilla Emacs, this means keys `down', `up', `C-n', and `C-p'.
;;
;;  Icicles also rebinds `next' and `prior' for apropos-completion
;;  cycling.  You still have `M-n' and `M-p' available to access past
;;  inputs (history).  And the rebindings are only for minibuffer
;;  input; global bindings are not affected.
;;
;;  You can at any time switch back and forth between input-history
;;  traversal (`M-n', `M-p') and prefix completion (`C-n', `C-p' or
;;  `down', `up').
;;
;;  See Also:
;;
;;  * (@> "History Enhancements") for new ways to use Emacs history
;;    lists with Icicles
;;
;;  * (@> "Customizing Key Bindings") for how to change the default Icicles
;;    key bindings
;;
;;    Note: If you are thinking about customizing key bindings just so
;;    that you can use `up' `down' for the minibuffer history, then
;;    consider setting `icicle-cycling-respects-completion-mode-flag'
;;    to t instead; it should give you the behavior you want.  If you
;;    still want to customize keys to do this, then
;;    see (@> "Customizing Key Bindings").
 
;;(@* "Apropos Completions")
;;
;;  Apropos Completions
;;  -------------------
;;
;;  Icicles offers a new way to complete your partial input in the
;;  minibuffer.  Instead of considering the string of input characters
;;  to be the prefix of various complete names, you can look for names
;;  that match that string anywhere.  This is the single most
;;  important feature that Icicles offers.
;;
;;  This is similar in effect to using command `apropos' to find
;;  "apropos completions" of a string (except it works also for file
;;  and buffer names), so that's the term I use for this.  The more
;;  correct characterization of this is that of the previous
;;  paragraph, however: names that match the given string.
;;
;;  Just as with prefix completion, Icicles lets you cycle among the
;;  apropos candidates.  To do this, you use keys `next' and `prior'
;;  (or `C-v' and `M-v').  The root that was completed is underlined
;;  in the minibuffer completion candidate.
;;
;;  For example, suppose you use `M-x' to enter a command.  You don't
;;  remember the exact command name, but it has something to do with
;;  lines, so you type `M-x line', then hit `next' repeatedly, until
;;  you see the right "line" command - `transpose-lines', perhaps.
;;  Prefix completion cannot find this command, because "line" is not
;;  a prefix of "transpose-lines".
;;
;;  Because `M-x' expects a command name, only command names are
;;  inserted into the minibuffer as the apropos-completion candidates
;;  for `M-x'.  Likewise, in other contexts, where names of other
;;  kinds of object are expected, apropos completion inserts only
;;  names of objects of the appropriate type.  Prefix completion works
;;  the same way.
;;
;;  For example, using `next' and `prior' with `C-x b at' lets you
;;  cycle through all buffers (such as `*scratch*') that have "at" in
;;  their name - only buffer names appear as candidates.
;;
;;  Apropos completion uses a regular expression (regexp) as its input
;;  string.  You can type `M-x \bes', for instance, to find commands
;;  with "es" at the start of a word within the command name (`\b'
;;  matches the start of a word).  It will find `eshell-test' and
;;  `color-theme-blue-eshell', but not `count-lines' - "es" does not
;;  start a word in `count-lines'.  Similarly, for file names, buffer
;;  names, and so on.
;;
;;  Prefix completion is actually a special case of apropos
;;  completion, where the regexp starts with "^".  (That is not how it
;;  is implemented, however.)
;;
;;  What if you want to see the list of all completion candidates that
;;  match the minibuffer input? Instead of cycling candidates blindly,
;;  just hit `S-TAB' (Shift TAB) at any time to display the matching
;;  candidates in pop-up buffer *Completions*.  This is analogous to
;;  `TAB' for prefix completion.
;;
;;  Everything said in section (@> "Cycling Completions") about the
;;  *Completions* buffer for prefix completion is also true for
;;  apropos completion.  It is updated to reflect the current set of
;;  matching candidates, and the current completion is highlighted.
;;  The root that was completed is highlighted within each candidate
;;  (first occurrence only).  Root highlighting is more important in
;;  the case of apropos completion, because the match position is
;;  different in different candidates.  In the case of apropos
;;  completion, the root is not the input string, taken literally, but
;;  the part of a candidate that the input matches.
;;  See (@> "*Completions* Display") for additional ways to use the
;;  minibuffer with *Completions*.
;;
;;  Regexp matching is perhaps the most powerful feature of Icicles.
;;  Enjoy!  Explore!  You can at any time switch back and forth
;;  between prefix completion (`down', `up'), apropos completion
;;  (`next', `prior'), and history traversal (`M-n', `M-p').
 
;;(@* "Longest-Common-Match Completion")
;;
;;  Longest-Common-Match Completion
;;  -------------------------------
;;
;;  Apropos (regexp) matching and prefix completion each match a
;;  pattern against a completion candidate.  This operation concerns
;;  only a single candidate; it does not take into account the fact
;;  that there are others.  Since the matching operation is repeated
;;  anyway for each candidate, however, we can also find the longest
;;  string that includes the same match (apropos or prefix) for all
;;  candidates.
;;
;;  For prefix completion, Emacs already completes your input to the
;;  longest common prefix match.  Icicles extends this notion to
;;  apropos completion.
;;
;;  For example, if you enter `M-x minib' and hit `TAB', Emacs
;;  completes your input to `minibuffer', which is the longest prefix
;;  match for `minib' among all command names.  The actual string that
;;  matches prefix `minib' among all candidates is, itself, `minib'.
;;
;;  If you hit `S-TAB', then each matching candidate contains a
;;  substring that matches your regexp input `minib'.  In this case,
;;  that substring is `minib', just as in the prefix-matching case.
;;  And, as in the prefix case, each matching candidate also includes
;;  a longer substring, `minibuffer', which includes what your input
;;  matches for each candidate.
;;
;;  Icicles replaces your regexp input in the minibuffer by the
;;  longest such string.  Icicles highlights this longest common match
;;  in buffer *Completions* using face
;;  `icicle-common-match-highlight-Completions' (magenta foreground,
;;  by default).  What your input matches directly is highlighted in
;;  *Completions* using face `icicle-match-highlight-Completions' (red
;;  foreground, by default).
;;
;;  It is of course possible that a given regexp match different
;;  candidates differently, so that there is no common match.  In that
;;  case, only the individual matches are highlighted in *Completions*
;;  - you will see only red, no magenta, highlighting.  For example,
;;  if your regexp input is `min.*buf' then various different
;;  substrings (such as `minibuf' from `minibuffer-complete' and
;;  `mint-truncate-buf' from `comint-truncate-buffer') are highlighted
;;  in red, but these share no common substring.
;;
;;  You will also see only red highlighting if what your input matches
;;  directly is the same as the longest common match.  For example, if
;;  a function `moccur-regexp-read-from-minibuf' is defined (it is in
;;  library `color-moccur.el'), and your input to `C-h f' is
;;  `min[^-]*buf', then only `minibuf' is highlighted in red.
;;
;;  Longest-common-match completion is convenient, but when
;;  apropos-completing you often need to try variants of a regexp,
;;  editing it and observing which candidates match in *Completions*,
;;  until you get the regexp right.  Longest-common-match completion
;;  has the disadvantage that you lose your regexp as input, which
;;  makes it hard to edit it!  To retrieve it, use `C-l'
;;  (`icicle-retrieve-previous-input') during completion.  You can
;;  repeat `C-l' to retrieve older completion inputs, and you can use
;;  `C-S-l' (that is, `C-L') to cycle previous inputs in the other
;;  direction - see (@> "History Enhancements").  You can set option
;;  `icicle-expand-input-to-common-match-flag' to nil to turn off
;;  longest-common-match completion altogether, if you prefer.
 
;;(@* "Progressive Completion")
;;
;;  Progressive Completion
;;  ----------------------
;;
;;  Perhaps the best way to explain this feature is to use a familiar
;;  analogy.  Unix or GNU/Linux command `grep' takes a
;;  regular-expression argument, and matches it against lines in
;;  files.  A common idiom that people use is to chain, or cascade,
;;  multiple calls to `grep', using the output of one as the input to
;;  the next.  For example:
;;
;;    grep plant *.txt | grep food | grep mineral
;;
;;  The output of the search for "plant" is used as the input for the
;;  search for "food", and the output of that search serves as the
;;  input for the search for "mineral".  The order of the three
;;  component searches can make a difference in terms of performance,
;;  but not in terms of the result, which is always the set of lines
;;  in files *.txt that match "plant" AND "food" AND "mineral", in any
;;  order.  Each of the `grep' operations defines a set of matches,
;;  and the chain of `grep' operations effects the intersection of
;;  those sets.
;;
;;  Of course, you could try to accomplish the same thing with a
;;  single call to `grep' using a complex regexp.  But why would you?
;;
;;  The same idea is behind the Icicles feature of progressive
;;  completion: instead of trying to come up with a complex regexp
;;  that does what you want, try getting there a step at a time:
;;
;;   1. Match an input regexp against the set of all possible
;;      completions.
;;
;;   2. Narrow the set of matched candidates by matching them against
;;      another input regexp (or by filtering them with a predicate).
;;
;;   3. Narrow those results down by matching them against a third
;;      input regexp (or by filtering them with another predicate).
;;
;;   4... And so on.
;;
;;(@* "`M-*': Matching Additional Regexps")
;;  ** `M-*': Matching Additional Regexps **
;;
;;  During completion, `M-*' is bound in the minibuffer to command
;;  `icicle-narrow-candidates', which prompts for a new regexp and
;;  matches it against the current set of completion candidates.
;;
;;  For example, suppose that you want to know about an Emacs function
;;  that deletes the character to the left of point (that is,
;;  backward).  You don't recall if it's `delete-character-back',
;;  `delete-backward-char', `character-delete-backward', or whatever.
;;  You take a guess that the name contains `delete', `char', and
;;  `back'.
;;
;;   1. `C-h f char S-TAB' displays function names that contain
;;      `char'.
;;
;;   2. `M-* delete' narrows that set of function names to those that
;;      also contain `delete'.
;;
;;   3. `M-* back' narrows the set of matching names further, to those
;;      that also contain `back'.
;;
;;  This displays a list of functions like this in *Completions* (your
;;  list might be somewhat different):
;;
;;    backward-delete-char        backward-delete-char-untabify
;;    delete-backward-char        icicle-backward-delete-char-untabify
;;    icicle-delete-backward-char
;;    quail-conversion-backward-delete-char
;;
;;  Then, of course, you can pick one (or you can use `C-M-next'
;;  repeatedly to view the doc of each of these functions in turn -
;;  see (@> "Get Help on Candidates")).
;;
;;  You get the idea.  This feature is both very simple to use and
;;  very useful.  It's easy to appreciate using multiple simple
;;  matching steps (regexp or not) instead of a single regexp.  Try it
;;  once, and you'll be hooked.
;;
;;(@* "Successive Approximation...")
;;  ** Successive Approximation... **
;;
;;  You can use as many invocations of `M-*' (and of `M-&', described
;;  in the next section) as you like, in any order.  It works with
;;  both prefix completion and apropos completion.  You can, for
;;  instance, first use `TAB' to require the target to start with some
;;  string, and then use `M-*' to specify other patterns that parts of
;;  it must also match.  However, it of course makes no sense to use
;;  `TAB' instead of `S-TAB' after you use `M-*': once you've said
;;  that the target must start with "fo" there is no sense saying that
;;  it also starts with "ti"!
;;
;;  As a shortcut, instead of using `S-TAB' followed by `M-*', you can
;;  use `S-SPC' (command `icicle-apropos-complete-and-narrow') to do
;;  the same thing.  If you narrow down the possible candidates to a
;;  single candidate using `S-SPC', then that candidate is chosen and
;;  you exit the minibuffer immediately.  This means that you can, if
;;  you like, use only `S-SPC' (possibly multiple times) to choose a
;;  candidate - no need to hit `RET' to confirm your choice.
;;
;;  For lack of a better name, I call this process of completion by
;;  successive approximation, or progressively narrowing the candidate
;;  set, "progressive completion".  If the name "incremental
;;  completion" (= icompletion) were not already taken to mean
;;  incremental completion *help* (which performs no completion), then
;;  that might be a good name for this.  This might also be called
;;  "stepped", "cascaded", or "piecewise" completion.
;;
;;  Another possible name for it would be "multiple completion", but I
;;  use that to stand for simultaneous (parallel) completion of
;;  multiple parts of a compound target, which is something different
;;  (see (@> "Multi-Completions")).  Progressive completion is a set
;;  of mini-completions that are wired in series, not in parallel.
;;
;;  Note that when you use `M-*' (or `S-SPC') in the minibuffer, it
;;  calls `completing-read' or `read-file-name', which creates a
;;  recursive minibuffer.  That is, the minibuffer depth is increased.
;;  (This is not the case for `M-&', however.)  In vanilla Emacs,
;;  there is no indicator of the current minibuffer depth, and this
;;  can sometimes be disorienting.  Each time you use `M-*' you push
;;  down one level of minibuffer recursion (that is, minibuffer depth
;;  is incremented).  Each time you use, say, `C-g', you pop up one
;;  level of minibuffer recursion (that is, minibuffer depth is
;;  decremented).
;;
;;  If you use my library `oneonone.el', then you get visual feedback
;;  on the current minibuffer depth.  One-On-One Emacs gives you a
;;  standalone minibuffer frame, and it changes the background hue
;;  (color) of that frame slightly with each change in minibuffer
;;  depth.  This is especially helpful with Icicles, where use of
;;  `M-*' is common.
;;
;;  There is a slight difference in behavior between Icicles commands
;;  and some other Emacs commands when you accept input after `M-*'.
;;  When possible, Icicles accepts your input and passes it
;;  immediately to the top level, bypassing any intermediate recursive
;;  minibuffer levels that are waiting for inupt.  However, Emacs
;;  commands that are defined with literal-string `interactive' specs,
;;  such as (interactive "fFile: "), do not use `completing-read' or
;;  `read-file-name', so there is no way for Icicles to take this
;;  shortcut with them.  In that case, you will simply need to hit
;;  `RET' again to accept your input at each recursive minibuffer
;;  level, until you get back to the top level.  Sorry for this
;;  inconvenience!  If you are an Emacs-Lisp programmer, note that
;;  this is one reason to use `completing-read' and `read-file-name'
;;  when you write commands that use completion.
;;
;;  Note: If you use progressive completion with file names in Emacs
;;  20 or 21, `M-*' calls `completing-read', not `read-file-name'.
;;  This is because `read-file-name' does not accept a PREDICATE
;;  argument before Emacs 22.  The effect is that instead of there
;;  being a default directory for completion, the current directory at
;;  the time you hit `M-*' is tacked onto each file name, to become
;;  part of the completion candidates themselves.  Yes, this is a
;;  hack.  It works, but be aware of the behavior.
;;
;;(@* "`M-&': Satisfying Additional Predicates")
;;  ** `M-&': Satisfying Additional Predicates **
;;
;;  If you use Icicles, then you will use `M-*' very often.  This
;;  section describes a seldom-used feature that can be useful in
;;  certain contexts.  If you are new to Icicles or you are unfamiliar
;;  with Emacs Lisp, then you might want to just skim this section or
;;  skip it and come back to it later.
;;
;;  Just as you can use `M-*' to narrow the set of candidates by
;;  matching an additional regexp, so you can use `M-&' (bound to
;;  `icicle-narrow-candidates-with-predicate') to narrow by satisfying
;;  an additional predicate.  The idea is the same; the only
;;  difference is that, instead of typing a regexp to match, you type
;;  a predicate to satisfy.
;;
;;  The predicate is a boolean function of a single completion
;;  candidate.  At the prompt, you enter its name or its lambda-form
;;  definition (anonymous function).  The predicate is used the same
;;  way as the PREDICATE argument to `completing-read' and
;;  `read-file-name'.  This means that the candidate argument to the
;;  predicate is whatever is used in the original call to
;;  `completing-read' or `read-file-name'; it is not just a string
;;  such as you see in buffer *Completions*.  To provide an
;;  appropriate predicate, you must be familiar with the kind of
;;  candidate expected by the command you invoked before just before
;;  `M-&'.
;;
;;  For example:
;;
;;  * Command `describe-function' (`C-h f') uses candidates that are
;;    symbols.  An appropriate predicate would accept a symbol as
;;    argument.
;;
;;  * Command `icicle-search' (`C-c `') uses candidates that have this
;;    form: (CONTEXT . MARKER), where CONTEXT is a string, the search
;;    hit (search context), and MARKER is a buffer marker that locates
;;    the CONTEXT.  An appropriate predicate would accept such a
;;    candidate as argument.
;;
;;  Although entering a lambda expression at a prompt might not seem
;;  too convenient, you can at least retrieve previously entered
;;  predicates (using `M-p' and so on).
;;
;;  You can also use `C-M-&' (bound to
;;  `icicle-save-predicate-to-variable') at any time during completion
;;  to save the current predicate as a string-valued variable.  By
;;  default, the variable is `icicle-input-string'.  You can then
;;  retrieve the saved string later, using `C-=' at the prompt for
;;  `M-&'.  The current predicate is what is saved.  You can build up
;;  a complex predicate, and then save it for later use.
;;
;;  The inconvenience of typing an Emacs-Lisp sexp must be balanced
;;  against the power of applying predicates on the fly.  Whereas
;;  regexp matching is purely syntactic, with a predicate you can
;;  perform semantic tests.  During search, for instance, you can look
;;  not only for a syntax match; you can look for matching search
;;  candidates that also belong to a particular class of objects
;;  (e.g. function, variable, type) or that satisfy some other
;;  semantic property.
;;  See also (@> "Icicles Search Commands, Overview").
;;
;;  See Also:
;;
;;  * (@> "Sets of Completion Candidates") for another way to perform
;;    a set intersection on sets of candidate completions.
;;
;;  * (@> "Icicles Search Commands, Overview") for a way to search
;;    using two regexps - command `icicle-search' uses the same idea
;;    as that behind progressive completion.
;;
;;  * (@> "Compile/Grep Search") for a way to grep files using
;;    multiple levels of regexps, and performing selected
;;    replacements.
 
;;(@* "Moving Between the Minibuffer and Other Buffers")
;;
;;  Moving Between the Minibuffer and Other Buffers
;;  -----------------------------------------------
;;
;;  Sometimes, when the minibuffer is active, you might want to move
;;  the cursor and focus from the minibuffer back to the original
;;  buffer from which you activated the minibuffer.  When you are in
;;  Icicle mode, the `pause' key is bound (by default) to command
;;  `icicle-switch-to/from-minibuffer', which does that.  This lets
;;  you start minibuffer input (with or without completion), and then
;;  interrupt it to search, edit, and so on, in the original buffer.
;;  This same command (bound to `pause') then lets you switch back to
;;  the minibuffer - it acts as a toggle for the input focus; go back
;;  and forth as much as you like.
;;
;;  This can be especially useful when you use multi-commands (see
;;  (@> "Multi-Commands")).  In that case, you often keep the
;;  minibuffer active for completion while performing multiple
;;  completion actions.  It can be handy to interrupt this to perform
;;  some normal editing or search, and then resume multi-command
;;  actions.
;;
;;  Another use for this feature is to select text in the original
;;  buffer and then insert it in the minibuffer.  See also 
;;  (@> "Inserting Text Found Near the Cursor") for another way to do
;;  that.
;;
;;  A somewhat related toggle is available using `C-insert'.  This
;;  lets you switch the focus between the minibuffer and buffer
;;  *Completions*.  See (@> "*Completions* Display") for more
;;  information.
 
;;(@* "Inserting a Regexp from a Variable")
;;
;;  Inserting a Regexp from a Variable
;;  ----------------------------------
;;
;;  Regexps are powerful, but they can sometimes be complex to compose
;;  and hard to remember once composed.  A shortcut is to compose a
;;  regexp that you want to use and assign it to an Emacs variable.
;;  Then, whenever you are typing input in the minibuffer, you can use
;;  `C-=' (bound to command `icicle-insert-string-from-variable') to
;;  insert the regexp.
;;
;;  If you use `C-u C-=' (provide a prefix argument) then you are
;;  prompted for the variable to use.  You can use any variable.
;;  Without `C-u', the default variable is used (no prompting),
;;  `icicle-input-string'.  So, for example, if `icicle-input-string'
;;  had value "[a-zA-Z]+" then it would match any completion candidate
;;  composed only of letters.  You can customize
;;  `icicle-input-string'.
;;
;;  For convenience, instead of using Lisp evaluation of a sexp such
;;  as (setq icicle-input-string "[a-zA-Z]+") or (setq my-var ".*"),
;;  you can use Icicles command `icicle-save-string-to-variable' to
;;  save a regexp to a variable.  You are prompted for the regexp to
;;  save.  Just as for `icicle-insert-string-from-variable', with a
;;  prefix argument you are prompted for the variable to use; with no
;;  prefix argument the regexp is saved to variable
;;  `icicle-input-string'.
;;
;;  This shortcut feature is especially convenient for use with
;;  command `icicle-search' - you can use it to search text for
;;  sentences, paragraphs, file names, URLs, dates, times, function
;;  definitions, and any other text entities that you can specify by
;;  regexp.  Create a library of regexp-valued variables that are
;;  useful to you, and use `C-=' to quickly access them in
;;  `icicle-search'.  See (@> "Icicles Search Commands, Overview") for
;;  more information.
;;
;;  See Also: (@> "Inserting Text Found Near the Cursor").
 
;;(@* "What About Special-Character Conflicts?")
;;
;;  What About Special-Character Conflicts?
;;  ---------------------------------------
;;
;;  Regular-expression syntax treats some characters specially, but
;;  some of these special characters have another special meaning in
;;  Emacs when used with file-name inputs.  What about the conflict
;;  between interpreting characters such as `$', `\', `.', `?', and
;;  `*' as 1) regexp special characters and 2) special characters for
;;  file-name input?  For example, when inputting a file name, should
;;  `*' be treated as a regexp multiple-occurrences operator or as a
;;  file-name wildcard?
;;
;;  In Emacs file-name input:
;;
;;  - `$' can be used to prefix environment variables.
;;
;;  - `*' and `?' can be used as wildcards, effectively inputting
;;    multiple file names at once.
;;
;;  - `.' and `..' can be used to navigate a directory hierarchy.
;;
;;  - `\' is a directory separator, like `/', on MS Windows, at least.
;;
;;  Icicles handles the conflict by interpreting such characters as
;;  regexp special characters only during input completion and cycling
;;  - and then only if you do not escape them (with `\').  If present
;;  in the input when you finally accept it (using `RET'), they take
;;  on their normal Emacs meanings for file-name input:
;;  environment-variable prefix, wildcard, directory abbreviation, or
;;  directory separator.
;;
;;  That is, whenever there is a potential conflict of interpretation,
;;  the regexp meaning is used for completion and cycling, and the
;;  standard interpretation for file-name input is used for accepting
;;  the input.  So, for example, to get the wildcard interpretation of
;;  `*', just forego regexp completion and cycling.  And vice versa:
;;  forego the wildcard interpretation to use regexp completion and
;;  cycling.
;;
;;  This is in any case the behavior of vanilla Emacs as well.  If, in
;;  vanilla Emacs, you use `ici*' or `ici*.el' as input to `find-file'
;;  and hit `TAB', there is no completion available.  File-name
;;  globbing and completion are independent.
;;
;;  Note: Because `?' is useful in regexp syntax, the standard Emacs
;;        minibuffer binding of `?', which just displays the
;;        completion-candidates list, is not used in Icicles.  In
;;        Icicles, `?' self-inserts in the minibuffer, like any other
;;        printable character.  (Use `TAB' or `S-TAB' to display the
;;        list.)  In standard Emacs, you must quote `?' or
;;        copy-and-paste it, to insert it in the minibuffer for use as
;;        a file-name wildcard.
;;
;;  The interpretation conflict for `\' (on MS Windows) is not really
;;  a problem, anyway.  Although you cannot use a backslash (`\') as a
;;  directory separator during completion and cycling, you can always
;;  use a slash (`/') instead - even on MS Windows.  Just break with
;;  MS-Windows syntax, and get in the habit of using `/' as the
;;  directory-separator character.
;;
;;  Even if you use only slash, not backslash, as a directory
;;  separator when inputting, however, it's possible that you could
;;  run into some trouble (on MS Windows) - you might (knowingly or
;;  not) use `\' as a directory separator in the values of environment
;;  variables that you use as part of file-name input.  Because the
;;  expanded input is treated as a regexp by apropos completion, you
;;  should use only prefix completion with input that includes
;;  environment variables, if their expansions include backslashes.
;;
;;  The interpretation conflict for `$' is also not a real problem.
;;  You can get the effect of both interpretations of `$' at the same
;;  time, because Icicles recognizes that `$' at the end of input
;;  cannot be an environment-variable prefix.  This means, for
;;  example, that you can use a pattern such as `$HOME.*t$' to match
;;  the files in your home directory whose names end in `t'.
;;
;;  The first `$' here is not treated specially during regexp matching
;;  and cycling; the environment variable `$HOME' is expanded by the
;;  shell to a directory name.  The second `$' is treated as the
;;  regexp special character that matches at the end of a line.
;;
;;  Tip: Because slash (`/') is about the only non-word syntax
;;       character that is likely to appear in file-name completions,
;;       you can usually use `\W$' to match only directories (by
;;       matching the `/' at the end of their names).  `\W' is the
;;       regexp pattern that matches any character that does not
;;       appear in words.
;;
;;  Finally, you can toggle interpretation vs escaping of regexp
;;  special characters at any time using `C-`' in the minibuffer
;;  (command `icicle-toggle-regexp-quote').  Escaping special
;;  characters this way means they are no longer special; they simply
;;  match themselves.  This has the effect of reducing apropos
;;  completion to simple substring completion.  If you never want to
;;  use regexp matching (*not* recommended!), you can customize user
;;  option `icicle-regexp-quote-flag', setting it to non-nil.
;;
;;  See Also:
;;
;;  * (@> "Sets of Completion Candidates") for a way to use Icicles
;;    regexp-matching to open Dired on sets of files that you cannot
;;    specify using file-name wildcards.
;;
;;  * (@> "Multi-Commands") for a way to open multiple files whose
;;    names match a regular expression.
;;
;;  * (@> "File-Name and Directory-Name Completion Tips") for:
;;    - Information about abbreviating your home directory as `~' or
;;      expanding it.
;;    - A way to locate and open files by regexp anywhere in your file
;;      system - that is, match against directory-name as well as
;;      file-name components.
 
;;(@* "Alternative Libraries: Other Methods of Choosing Default Values")
;;
;;  Alternative Libraries: Other Methods of Choosing Default Values
;;  ---------------------------------------------------------------
;;
;;  There are other libraries, besides Icicles, that give you
;;  alternative ways to pick a candidate default value.  There are,
;;  for instance, many libraries that provide ways to switch buffers.
;;  Some of these present candidates in the minibuffer and choose one
;;  as soon as you type enough of its name to select it unambiguously
;;  - without your needing to confirm your choice (with `RET', for
;;  example).  Library `ido.el' is an example of such a library.
;;  Library `iswitchb.el' is another such library; it is specialized
;;  for choosing a buffer.
;;
;;  Choosing without confirming can be very quick, but I prefer to
;;  confirm a choice.  In any case, you can also use Icicles to choose
;;  without confirming, if you wish - see (@> "Multi-Commands").  See
;;  also (@> "Exiting the Minibuffer Without Confirmation") for how to
;;  obtain the complete-and-exit behavior of library `iswitchb.el'.
;;
;;  The main reason I prefer Icicles is because of its generality.
;;  You use the same input, cycling, and completion method for
;;  everything.  There is no need to be familiar with one method for
;;  switching buffers, another method for choosing a command, another
;;  for choosing a variable, and so on.  Library `ido.el' is quite
;;  general too, but perhaps a little less so.
;;
;;  Also, I like to be able to edit the value in the minibuffer.  For
;;  instance, for lax (permissive) completion, where you are not
;;  required to enter one of the proposed candidates, you can use
;;  completion to retrieve a candidate that is similar to what you
;;  want to enter, then edit it and hit `RET' to submit the actual
;;  value you want.  Library `ido.el' does have an "edit" command or
;;  mode, but I find Icicles better for letting me edit input.
;;
;;  Icicles has many additional features that are not available in
;;  other libraries, but its main advantage is its generality: you use
;;  the same user interface for input of any kind.
 
;;(@* "Exiting the Minibuffer Without Confirmation")
;;
;;  Exiting the Minibuffer Without Confirmation
;;  -------------------------------------------
;;
;;  Normally, if you exit the minibuffer with input that only
;;  partially matches a completion candidate, the value you input is
;;  exactly what you typed.  That is, exiting does not automatically
;;  complete your input - what you type is what you get.  This is
;;  "lax" (or "permissive") completion, and it is desirable most of
;;  the time, because it lets you input a value that does not
;;  correspond to any of the completion candidates.  This is how, for
;;  instance, you can use `C-x C-f' to open a new file or `C-x b' to
;;  create a new buffer.
;;
;;  However, some people prefer "strict" completion: limiting input to
;;  the available completion candidates.  This can be handy in the
;;  case of switching to a buffer, for instance.  If you have a buffer
;;  named `new-ideas.txt', you might like to be able to type only
;;  `new' followed by `RET', and not have to first complete the input
;;  text.  This is the behavior of libraries `ido.el' and
;;  `iswitchb.el'.
;;
;;  It is the command you use that decides whether `RET' first
;;  completes your input before exiting the minibuffer (strict
;;  completion) or not (lax completion).  This is done in the command
;;  definition by providing a non-nil or nil REQUIRE-MATCH argument to
;;  function `completing-read', which prompts you and reads your
;;  input, possibly completing it.
;;
;;  If you use standard Emacs command `switch-to-buffer' then
;;  completion is lax: `RET' does not complete your input `new' to
;;  `new-ideas.txt'; it simply accepts your input as is, and creates a
;;  new buffer with that name, `new'.
;;
;;(@* "Using `S-RET' to Accept a Partial Match")
;;  ** Using `S-RET' to Accept a Partial Match **
;;
;;  By default, Icicles command `icicle-buffer', not
;;  `switch-to-buffer', is bound to `C-x b' in Icicle mode.  The
;;  default behavior of `icicle-buffer' is the same as the behavior of
;;  `switch-to-buffer' with respect to `RET'.  However, you can obtain
;;  the complete-and-exit `RET' behavior with `icicle-buffer' by
;;  setting option `icicle-buffer-require-match-flag' to
;;  `partial-match-ok'.  This value overrides the REQUIRE-MATCH
;;  argument to `completing-read', in effect forcing it to `t'.
;;
;;  Whenever completion is strict, requiring a match against one of
;;  the completion candidates (typically, an existing file or buffer
;;  name), you can complete and exit the minibuffer all at once, with
;;  only partial input in the minibuffer, by using `RET'.  But what
;;  about apropos completion?  Simply use `S-RET'
;;  (`icicle-apropos-complete-and-exit') instead of `RET': `RET' is
;;  standard in Emacs and uses prefix completion; `S-RET' is specific
;;  to Icicles and uses apropos completion.  For example, you can type
;;  `idea' followed by `S-RET' to switch to buffer `new-ideas.txt'.
;;
;;  Note: If you set user option `icicle-bind-top-level-commands-flag'
;;  to nil, then `C-x b' remains bound to `switch-to-buffer', even in
;;  Icicle mode.
;;
;;(@* "Accepting Partial Matches by Default")
;;  ** Accepting Partial Matches by Default **
;;
;;  For those people who prefer that a partial match always be
;;  accepted immediately, regardless of the context (whether a match
;;  is required or not) and without having to use `RET' or `S-RET',
;;  there is Icicles user option
;;  `icicle-top-level-when-sole-completion-flag'.  If you set this to
;;  non-nil, then, whenever your input matches only one candidate
;;  completion, that candidate is used immediately.  I don't recommend
;;  this practice, but some people might prefer it.
;;
;;  There is one situation where this is handy, and there this option
;;  is bound to non-nil, so that a single match is always accepted.
;;  This is the case when you use `S-SPC' as a progressive-completion
;;  shortcut - see (@> "Progressive Completion").
 
;;(@* "*Completions* Display")
;;
;;  *Completions* Display
;;  ---------------------
;;
;;  Icicles adds a few enhancements to the *Completions* display, for
;;  convenience.  The following apply whenever buffer *Completions* is
;;  displayed:
;;
;;  1. When you cycle completions in the minibuffer, the current
;;     candidate is highlighted in *Completions*.
;;
;;  2. You can use `C-insert' to move back and forth between the
;;     minibuffer and *Completions*.  In each direction, the current
;;     candidate is tracked in the destination buffer.  For example,
;;     if the candidate in the minibuffer is `foobar', after you hit
;;     `C-insert' the cursor is on `foobar' in *Completions*.  In the
;;     other direction, if the cursor is on `foobar' in *Completions*,
;;     after you hit `C-insert' the current input in the minibuffer is
;;     `foobar'.
;;
;;  3. In buffer *Completions*, you can use the arrow keys to navigate
;;     among the candidate completions.  The current candidate (under
;;     the cursor) is highlighted.
;;
;;  4. *Completions* can also serve as a new kind of icompletion help
;;     - see (@> "Icompletion").
;;
;;  5. You can choose multiple candidates during completion, by
;;     clicking them with `mouse-2' while holding the Control key
;;     pressed.  See (@> "Multi-Commands").
;;
;;  6. Icicles varies the number of columns used to display completion
;;     candidates, for a better fit.  You can tweak this with user
;;     option `icicle-candidate-width-factor'.
;;
;;  7. Icicles dynamically resizes the *Completions* window
;;     vertically, to fit the current set of completion candidates.
;;     The window is not resized, however, if buffer *Completions*
;;     appears in its own frame.  (It is also not resized in Emacs
;;     releases prior to 21.)
;;
;;     You can control this automatic resizing generally or on a
;;     per-command basis:
;;
;;     * User option `icicle-Completions-window-max-height' is the
;;       maximum number of lines to show in the *Completions* window.
;;
;;     * You can override the behavior of option
;;       `icicle-Completions-window-max-height' for any given command,
;;       by setting property `icicle-Completions-window-max-height' on
;;       the command symbol to a different maximum window height
;;       value.  This property value is predefined for commands, such
;;       as `icicle-buffer' and `icicle-find-file', that don't involve
;;       the content of the current buffer during completion.  A large
;;       value is used for these commands, so that nearly all of the
;;       frame real estate is given to the *Completions* window.
;;
;;     For example, you can use this code to set the maximum
;;     *Completions* height for command `foo' to 100 and turn off
;;     per-command control of the height for command `bar'.  If you
;;     use such code, evaluate it after you load Icicles.
;;
;;       (put 'foo 'icicle-Completions-window-max-height 100)
;;       (put 'bar 'icicle-Completions-window-max-height nil)
;;
;;  There are lots of Icicles features that enhance the display and
;;  behavior of *Completions* in some way.  Read on...
;;
;;  See Also:
;;
;;  * (@> "Moving Between the Minibuffer and Other Buffers"), for
;;    information on the `pause' key, which is somewhat related to
;;    using `C-insert'.
;;
;;  * (@> "Text Properties in *Completions*") and
;;    (@> "Programming Multi-Completions") for information about using
;;    text properties in *Completions*.  These sections are for
;;    Emacs-Lisp programmers.
 
;;(@* "Icompletion")
;;
;;  Icompletion
;;  -----------
;;
;;  Emacs incremental completion, or icompletion, provided by standard
;;  library `icomplete.el', displays matching prefix completions in
;;  the minibuffer.  This display is updated incrementally as you type
;;  characters.  In spite of the name, icompletion does not, in fact,
;;  provide any completion; it provides completion help, letting you
;;  know which (prefix) completions are available.
;;
;;  Icicles enhances Emacs icompletion in three ways:
;;
;;  1. It works with my library `icomplete+.el' to provide minibuffer
;;     feedback on the number of completion candidates.
;;
;;  2. It highlights the part of your input that does not match any
;;     completion candidate.
;;
;;  3. It provides a new kind of icompletion, using buffer
;;     *Completions*.
;;
;;(@* "icomplete+.el Displays the Number of Other Prefix Candidates")
;;  ** icomplete+.el Displays the Number of Other Prefix Candidates **
;;
;;  Library `icomplete+.el' enhances `icomplete.el' in various ways.
;;  One of these ways is to complement Icicles by displaying the
;;  number of other prefix-completion candidates when cycling.  This
;;  number is displayed whenever you change direction when cycling.
;;  For example:
;;
;;      M-x forward-line   [Matched]  (13 more)
;;
;;  Like `icomplete.el', `icomplete+.el' provides help for only prefix
;;  completions, not apropos completions.  (Reminder: There is no
;;  icompletion for file-name completion - see standard library
;;  `icomplete.el'.)
;;
;;(@* "Icicles Highlights the Input that Won't Complete")
;;  ** Icicles Highlights the Input that Won't Complete **
;;
;;  When you are typing or correcting your input during completion,
;;  Icicles highlights the part of your minibuffer input that prevents
;;  it from matching any completion candidates.  This works for both
;;  prefix completion and apropos completion.
;;
;;  User option `icicle-highlight-input-completion-failure-flag'
;;  controls this highlighting, which is done using face
;;  `icicle-input-completion-fail'.  If this option is nil, then no
;;  such highlighting is done.  If the option value is `strict-only'
;;  then the match-failure highlighting is done only during strict
;;  completion, that is, when your input must match a candidate.  If
;;  the value is `lax-and-strict', then the highlighting is done
;;  during both lax and strict completion.  This is the default
;;  behavior.
;;
;;  You can easily remove the non-matching part of your input, by
;;  using `C-M-l'.  (The deleted mismatch part is actually killed, so
;;  you can subsequently paste it somewhere using `C-y'.)
;;
;;  Note that for this highlighting to occur, option
;;  `icicle-incremental-completion-flag' must also be non-nil.  You
;;  can toggle `icicle-incremental-completion-flag' using `C-#' in the
;;  minibuffer.  One reason you might want to turn this off sometimes
;;  is that when it is non-nil Icicles might recompute completion
;;  candidates multiple times, in order to determine the longest part
;;  that completes.  Also, incremental completion is effectively
;;  turned off when a remote file name is read, so no highlighting
;;  occurs then either.
;;
;;(@* "Icompletion in *Completions*: Apropos and Prefix Completion")
;;  ** Icompletion in *Completions*: Apropos and Prefix Completion **
;;
;;  Buffer *Completions* shows you the current set of candidates for
;;  either prefix or apropos completion.  Together, user options
;;  `icicle-incremental-completion-flag',
;;  `icicle-incremental-completion-delay', and
;;  `icicle-incremental-completion-threshold' control incremental
;;  updating of *Completions*.
;;
;;  If `icicle-incremental-completion-flag' is non-nil, then
;;  *Completions* is automatically updated when you change your input
;;  in the minibuffer - that is, with each character that you type or
;;  delete.  This is another form of icompletion, unique to Icicles.
;;  It uses buffer *Completions*, instead of the minibuffer, to show
;;  the completion help.
;;
;;  The particular non-nil value of
;;  `icicle-incremental-completion-flag' determines when *Completions*
;;  is displayed and updated.  The default value, t, means that
;;  *Completions* is updated only if it is already displayed.  Use t
;;  if you don't want *Completions* to be too intrusive but you want
;;  it to provide the most help when you ask for help (via `TAB' or
;;  `S-TAB').
;;
;;  Any other non-nil value displays and updates *Completions*
;;  whenever there is more than one completion candidate.  That can be
;;  more helpful, but it can also be more distracting.  A value of nil
;;  turns off automatic updating altogether - *Completions* is then
;;  displayed only upon demand.  I find that t represents a good
;;  compromise, providing help when I ask for it, and then continuing
;;  to help until I've finished choosing a candidate.
;;
;;  Option `icicle-incremental-completion-delay' is the number of
;;  seconds to wait before updating *Completions* incrementally.  It
;;  has an effect only when the number of completion candidates is
;;  greater than `icicle-incremental-completion-threshold'.  This
;;  delay can improve performance when there are many candidates.  It
;;  lets you type ahead before any redisplay occurs; otherwise,
;;  redisplay occurs for each character you type or delete.
;;
;;  You can toggle incremental completion at any time (changing
;;  `icicle-incremental-completion-flag' between nil and t) using
;;  command `icicle-toggle-incremental-completion', which is bound to
;;  `C-#' in the minibuffer.  If the number of completion candidates
;;  is very large, then use `C-#' to toggle incremental completion off
;;  - that will save time by not updating *Completions*.  See also
;;  (@> "Dealing With Large Candidate Sets") for other ways to deal
;;  with a large number of candidates.
;;
;;  Note: Incremental completion is effectively turned off when a
;;  remote file name is read, that is, whenever your file-name input
;;  matches a remote-file syntax.
;;
;;  There are several advantages of using *Completions* for
;;  icompletion, as opposed to the minibuffer:
;;
;;  1. Many more candidates can be displayed in *Completions* than can
;;     be displayed by standard icompletion, which uses the minibuffer
;;     for feedback.
;;
;;  2. Standard (minibuffer) icompletion provides feedback only on
;;     matches for prefix completion.  If you use both standard
;;     icompletion and Icicles icompletion, you can have incremental
;;     help for both prefix completion and apropos completion at the
;;     same time, one in the minibuffer and the other in
;;     *Completions*.
;;
;;  3. The other Icicles *Completions* features are available for the
;;     current set of matching candidates: cycling, highlighting of
;;     match root, highlighting of previously used candidates, and so
;;     on.  See (@> "*Completions* Display").
 
;;(@* "Sorting Candidates and Removing Duplicates")
;;
;;  Sorting Candidates and Removing Duplicates
;;  ------------------------------------------
;;
;;  By default, completion candidates are presented in buffer
;;  *Completions* in alphabetic order.  The order in *Completions* is
;;  also the order of cycling among candidates.  Also by default,
;;  duplicate candidates are removed as completion choices.
;;
;;  Some commands however, impose different orders, which are
;;  appropriate in their particular contexts, and some commands do not
;;  remove duplicates.  For example, command `icicle-search' (`C-c `')
;;  uses completion to navigate among search hits.  The unsorted order
;;  of the hit occurrences in the buffer is retained, as are duplicate
;;  matches.  Although some search-hit candidates might have the same
;;  text, they are located at different buffer positions.
;;
;;  For a small minority of commands such as `icicle-search', the
;;  candidate order is fixed.  The completion candidates you see in
;;  buffer *Completions* are just names for (invisible) candidate
;;  objects that contain additional information (buffer and buffer
;;  position, in the case of `icicle-search').  Different such objects
;;  might have the same completion-candidate name, so it is important
;;  that the order of presentation remain constant.  Icicles picks the
;;  candidate object to use, according to which candidate name you
;;  click with `mouse-2' or which candidate name is current during
;;  cycling.
;;
;;  Commands such as `icicle-search' are the exception.  For most
;;  commands, you can interactively control the order of candidates
;;  and whether duplicates are removed.  Use `C-,' during completion
;;  to choose a different sort order or to turn off sorting altogether
;;  (one of the available sort orders is in fact called "turned OFF").
;;  Use `C-$' to toggle the removal of duplicate candidates.  Commands
;;  such as `icicle-search', for which sorting is inappropriate,
;;  prevent you from sorting.
;;
;;(@* "Changing the Sort Order")
;;  ** Changing the Sort Order **
;;
;;  There are a couple of ways to use `C-,' (bound to command
;;  `icicle-change-sort-order').  Its behavior depends on the value of
;;  user option `icicle-change-sort-order-completion-flag', which is
;;  nil by default.  This value means to simply cycle to the next sort
;;  order each time you hit `C-,'.  A non-nil value means to use
;;  completion to choose another sort order.  If you have many
;;  available sort orders, then you might prefer a non-nil value.  In
;;  any case, you can also change this behavior on the fly: using
;;  plain `C-u' (no number) with `C-,' reverses the meaning of
;;  `icicle-change-sort-order-completion-flag' for `C-,'.
;;
;;  However, a numeric prefix argument, such as `C-9', means to simply
;;  reverse the direction of the current sort order; it invokes
;;  command `icicle-reverse-sort-order'.  For example, if candidates
;;  are sorted alphabetically from A to Z, then `C-9 C-,' flips the
;;  sort order, so that from then on sorting is from Z to A.
;;
;;  In addition to the current sort order, which is defined by the
;;  value of user option `icicle-sort-function', an alternative sort
;;  order is available at all times.  It is the value of option
;;  `icicle-alternative-sort-function'.  By default, this sorts
;;  candidates into two alphabetical groups: those previously used as
;;  accepted input, followed by those not yet used.
;;
;;  Just as you can choose a different current sort order using `C-,',
;;  so you can choose a different alternative sort order using `M-,'.
;;
;;  How do you actually use the alternative sort order?  Use `C-M-,'
;;  (command `icicle-toggle-alternative-sorting') to swap the
;;  alternative sort for the current sort.  This is the quickest way
;;  to flip between two sort orders.  If, for example, you set your
;;  alternative sort order to "turned OFF", then this is a quick way
;;  to toggle sorting on and off.
;;
;;  Commands that read buffer names with completion can use another
;;  sort-function user option, `icicle-buffer-sort'.  It has the same
;;  meaning as `icicle-sort-function', but it only applies to those
;;  commands.  It is provided so that you can customize it separately.
;;  You can also define buffer configurations that are used for
;;  completion: named sets of buffers, sort functions, and other
;;  parameters that control completion of buffer names.
;;
;;(@* "Defining New Sort Orders")
;;  ** Defining New Sort Orders **
;;
;;  When you use `C-,' or `M-,', the sort orders that you can choose
;;  from are those in user option `icicle-sort-functions-alist'.  You
;;  can customize this option to add or remove available sort orders.
;;  A better way to define a new sort order is to use macro
;;  `icicle-define-sort-command' in your Emacs init file (~/.emacs).
;;  This defines a new Icicles command, named `icicle-sort-ORDER',
;;  where `ORDER' is the name of the new sort order.  The definition
;;  of the "alphabetical" sort order provides an example:
;;
;;    (icicle-define-sort-command "alphabetical" 
;;                                icicle-case-string-less-p
;;      "Sort completion candidates alphabetically.")
;;
;;  The first argument, "alphabetical", is a string naming the new
;;  sort order.  When you change to this sort order, a message says
;;  "Sorting is now alphabetical".  Whatever sort-order name you
;;  provide is used in the message.
;;
;;  The second argument is the actual function used for sorting.  It
;;  can be any function, including a lambda expression.  The function
;;  takes two string arguments and returns non-nil if and only if the
;;  first string sorts before (is "less than") the second.  In this
;;  case, function `icicle-case-string-less-p' is used, which compares
;;  its two arguments alphabetically (lexicographically).  The third
;;  argument is the doc string for the new sorting command.
;;
;;  The result of this definition is:
;;
;;  1. The creation of command `icicle-sort-alphabetical'.
;;  2. The addition of an entry for the new sort order in option
;;     `icicle-sort-functions-alist'.  The entry associates sort order
;;     "alphabetical" with comparison function
;;     `icicle-case-string-less-p'.
;;
;;  You can invoke the new sorting command any time using `M-x', but
;;  you can also change to the new sort order using `C-,' (or `M-,')
;;  during minibuffer completion.
;;
;;(@* "Different Sorts for Different Sorts of Uses")
;;  ** Different Sorts for Different Sorts of Uses **
;;
;;  There are many different uses of completion in Emacs, and this
;;  means that sorting candidates needs to be flexible - there cannot
;;  be a single sort order that is useful for all purposes.
;;  Completion, and therefore sorting of completion candidates, needs
;;  to deal with different types of candidates and different numbers
;;  of them, in different contexts.
;;
;;  Icicles predefines several sort functions, and you can easily
;;  define more of your own.  You can choose a different sort at any
;;  time, as mentioned above.  A good sort order can be a big help,
;;  depending on the context.  However, sorting isn't free, and it's
;;  helpful to think for a moment about some of the consequences of
;;  sorting, in terms of performance.
;;
;;  What does a sort function do?  It determines which of two strings
;;  should come first, that is, which is "less than" the other.
;;  During sorting, pairs of candidates are compared using the sort
;;  function.  And each time you change your input by typing or
;;  deleting a character, the new set of matching candidates is sorted
;;  (if `icicle-incremental-completion-flag' is non-nil).
;;
;;  The number of candidates to be sorted depends on how you use
;;  Icicles.  Some Icicles users like to use cycling more and
;;  completion less, which means sorting more candidates.  Other users
;;  favor using completion to narrow down the number of matches (which
;;  I recommend).
;;
;;  If there are many candidates matching your input, then many
;;  comparisons will be made each time the candidate set is sorted.
;;  This means that if your sort function is complex, response can be
;;  slow.  A complex sort function might be OK for sorting a small or
;;  medium set of candidates, but it might not be appropriate for
;;  sorting a very large set.
;;
;;  Only you, as a user, can control which sort makes the best sense
;;  for you in any given situation.  If you are likely to have
;;  zillions of candidates in some context, then you probably will
;;  want to change to a sort that computes quickly.  You can, of
;;  course, even choose not to sort at all, but simple sort
;;  comparisons don't noticeably impact performance, even for a very
;;  large number of candidates.
;;
;;  Icicles could offer a threshold option similar to
;;  `icicle-incremental-completion-threshold' (or it could reuse that
;;  option), and not bother to sort if the number of candidates passed
;;  the threshold, but there can be many sort orders of differing
;;  complexity, so a set of thresholds would really be needed, perhaps
;;  one per sort order.
;;
;;  Rather than having you try to manage such complexity ahead of time
;;  using options, it's better to just let you manage it at completion
;;  time: Choose the sort order with knowledge of the possible
;;  candidate set.  For example, if the set of candidates to sort will
;;  include every file on your file system, then you probably will
;;  want to use a simple sort.  On the other hand, there are
;;  situations where you might nevertheless prefer to wait a few
;;  seconds, in order to perform a complex sort that is of particular
;;  use.
;;
;;  In sum, Icicles keeps it simple, and leaves it up to you to choose
;;  the appropriate sort order for any given context.  This design
;;  choice is one reason why Icicles makes it easy to choose a sort
;;  even while you are completing input - each act of completion is
;;  different.
;;
;;  It can help you choose, however, to know which of the predefined
;;  Icicles sort orders are more complex, and therefore tend to be
;;  slower.  Here they are:
;;
;;    Sort Order                      Sort Function Used
;;    ----------                      ------------------
;;    by previous use alphabetically  `icicle-historical-alphabetic-p'
;;    by last use                     `icicle-most-recent-first-p'
;;
;;  The reason these sorts are slower is that they check the current
;;  minibuffer history, to see whether, and where, each candidate is
;;  located in the history list.  If you, like I, have very long
;;  history lists, then this can take a while.  I use histories of
;;  virtually unlimited length - I let library `savehist-20+.el' save
;;  all of my histories from one Emacs session to the next.
;;
;;  Here are some of the Icicles sort orders that exist by default:
;;
;;    - alphabetical
;;      by amount of blue
;;      by amount of green
;;      by amount of red
;;      by color brightness (value)
;;      by color hue
;;      by color name
;;      by color purity (saturation)
;;      by color rgb
;;   22 by command name
;;    - by directories last
;;   22 by key name, local bindings first
;;   22 by key name, prefix keys first
;;    - by last file modification time
;;    - by last use
;;    - by previous use alphabetically
;;    - case-insensitive
;;    - turned OFF  (does not sort at all)
;;
;;  As you can see, some are appropriate only for color completion,
;;  and some are appropriate for file-name completion.  The latter
;;  sort alphabetically when completion is not for file names.
;;
;;  Those with `22' can be used only with Emacs 22.  Those marked with
;;  a hyphen (-) are defined using `icicle-define-sort-command', so
;;  they correspond to explicit commands whose doc you can examine.
;;  The command names in this case are `icicle-sort-' followed by the
;;  sort-order names (with hyphens substituted for spaces),
;;  e.g. `icicle-sort-by-directories-last' and
;;  `icicle-sort-turned-OFF'.
;;
;;  See Also:
;;
;;  * (@> "Customization and General Tips") for more about
;;    `icicle-buffer-sort' and other buffer-name completion
;;    parameters.
;;
;;  * (@> "Global Filters") for a way to filter and sort the domain of
;;    discourse, that is, all possible candidates, prior to any use of
;;    completion.
 
;;(@* "Get Help on Candidates")
;;
;;  Get Help on Candidates
;;  ----------------------
;;
;;  General Icicles help is available at any time during minibuffer
;;  input, by hitting `C-?' (`icicle-completion-help').  This section
;;  is about specific help on individual completion candidates,
;;  instead.
;;
;;  While you are cycling among candidates for input to a command, you
;;  can simultaneously display help on each candidate or any given
;;  candidate.  To show help on each candidate as you cycle, press and
;;  hold the Control and Meta keys while using the vertical arrow
;;  keys, for prefix completion, or the `prior' and `next' keys (often
;;  labeled Page Up and Page Down), for apropos completion.  To show
;;  help on any individual candidate, just navigate to it (by cycling
;;  or using completion), and hit `C-M-RET' - or press Control and
;;  Meta and click it with `mouse-2' (`C-M-mouse-2') in buffer
;;  *Completions*.
;;
;;  For example, if you use standard command `switch-to-buffer' and
;;  you cycle among candidate buffers with `C-M-down' (prefix
;;  completion), then the major and minor modes of each candidate
;;  buffer are described in buffer *Help* as the buffer name appears
;;  in the minibuffer.
;;
;;  By default, you need not use the Meta key for candidate help; the
;;  same bindings work with just the Control key.  So, for example,
;;  you can click `C-mouse-2' to get help on a candidate or use
;;  `C-next' to cycle candidate help information.  However, Icicles
;;  multi-commands often have a different use for these bindings that
;;  do not include Meta.  It is only by default, when a multi-command
;;  has not bound a more specific action to the plain Control
;;  bindings, that you can use the sans-Meta bindings for help on
;;  candidates.
;;
;;  For example, Icicles binds `M-x', `C-x b', and `C-x C-f' to
;;  multi-commands that execute a command, switch to a buffer, and
;;  open a file, respectively.  If you use only the Control key,
;;  without the Meta key, when choosing candidates for these commands,
;;  you will not get help on the candidates; instead, you will execute
;;  a candidate command, switch to a candidate buffer, and open a
;;  candidate file, respectively.  For more information, see
;;  (@> "Multi-Commands").
;;
;;(@* "Use Candidate Help Like You Use Emacs Command `apropos'")
;;  ** Use Candidate Help Like You Use Emacs Command `apropos' **
;;
;;  You can use this candidate-help functionality as a kind of
;;  expanded `apropos' functionality.  As an example, type `C-h v
;;  out', then type `S-TAB' to display all variables that match "out"
;;  (in buffer *Completions*).  Then use `C-M-next' repeatedly to
;;  cycle among those variables, displaying their documentation in the
;;  *Help* buffer as they appear one by one in the minibuffer.  Or
;;  click individual variable names with `C-M-mouse-2', to display
;;  their documentation.  The standard `apropos' commands show only
;;  the first doc-string line; Icicles shows the complete doc string.
;;
;;  This can be handy, for instance, when you are unsure which of
;;  several similarly named candidates to choose.  Seeing a
;;  candidate's documentation along with its name can help you decide.
;;
;;  You can click links in buffer *Help* to look up more info, and
;;  then resume `C-M-next' where you left off, all without leaving
;;  completion.
;;
;;  This also works with menu items, if you load library
;;  `icicles-menu.el' as well as `icicles.el'.  As you cycle among
;;  matching menu items, the corresponding command documentation is
;;  displayed in *Help*.
;;
;;  Help is available for these types of completion candidates, by
;;  default:
;;
;;  * menu items
;;  * commands and other functions
;;  * user options and other variables
;;  * faces
;;  * property lists
;;  * buffers
;;  * files
;;
;;  In addition, any command that uses completion can define its own
;;  candidate help action function and bind it to
;;  `icicle-candidate-help-fn'.
;;
;;  For more information about the types of candidates and their
;;  associated documentation, see the documentation for command
;;  `icicle-help-on-candidate'.  This command is bound to `C-M-RET',
;;  `C-M-mouse-2', `C-help', `C-M-help', `C-f1', and `C-M-f1'.  When
;;  no specific action is defined for candidates, it is also bound to
;;  `C-RET' and `C-mouse-2'.  You can use this to get help on any
;;  completion candidate during completion.  See also the related
;;  help-cycling commands, `icicle-help-on-next-apropos-candidate' and
;;  so on, bound to `C-M-next', `C-M-prior', `C-M-down', and `C-M-up'.
;;
;;  If you use one-buffer-per-frame (`pop-up-frames' non-nil), then
;;  displaying *Help* in one frame might interfere with viewing
;;  *Completions* in another.  For that reason, the *Completions*
;;  frame is raised to the front.  Also, if user option
;;  `icicle-Completions-frame-at-right-flag' is non-nil (default
;;  value: `t'), then the *Completions* frame is moved to the right,
;;  out of the way, whenever you access help on a candidate.
;;
;;(@* "Other Icicles Apropos Commands")
;;  ** Other Icicles Apropos Commands **
;;
;;  There are also Icicles replacements for the standard Emacs
;;  `apropos' commands.  They act the same, but they also let you see
;;  the list of regexp matches incrementally (as with all Icicles
;;  commands), using `S-TAB'.  If you also use my library
;;  `apropos-fn+var.el', then these Icicles commands take advantage of
;;  the apropos enhancements in that library.
;;
;;  The Icicles apropos commands are: `icicle-apropos',
;;  `icicle-apropos-command', `icicle-apropos-function',
;;  `icicle-apropos-option', `icicle-apropos-variable', and
;;  `icicle-apropos-zippy'.
;;
;;  In addition, Icicles commands `icicle-doc', `icicle-fundoc', and
;;  `icicle-vardoc' provide the functionality of standard Emacs
;;  command `apropos-documentation', but with additional features.
;;  See (@> "Multi-Completions").  You can use command `icicle-plist'
;;  to find symbols with certain property-list keys and values.
;;
;;  One difference between Icicles apropos commands and the standard
;;  commands, besides the Icicles enhancements already described, is
;;  that (starting with Emacs 22) the standard commands let you input
;;  a set of keywords, as an alternative to inputting a regexp.
;;  Icicles apropos commands do not allow for keyword input, as such.
;;  However, Icicles progressive completion provides a more powerful
;;  way to search with multiple keywords (in fact, multiple regexps) -
;;  you can of course use it with the Icicles apropos commands.  Also,
;;  there are several problems with the standard Emacs apropos
;;  commands, with respect to interpreting your input as either a set
;;  of keywords or a regexp.  Because they allow two very different
;;  syntaxes as input, the standard apropos commands are forced to
;;  make some limiting compromises for keyword searching.
;;
;;  See Also: (@> "Progressive Completion").
 
;;(@* "Multi-Commands")
;;
;;  Multi-Commands
;;  --------------
;;
;;(@* "What Is a Multi-Command?")
;;  ** What Is a Multi-Command? **
;;
;;  A multi-command is a command that lets you make multiple input
;;  choices in a single command execution: a multiple-choice command.
;;  You can choose multiple items from a set of choices, using buffer
;;  *Completions* as a multiple-choice "menu".  (It's not necessary to
;;  display *Completions*, however.)  Instead of asking you "Which
;;  file do you want to delete?", a multi-command asks you, in effect,
;;  "Which file(S) do you want to delete?".
;;
;;  Nothing especially new here.  Any Emacs command could be defined
;;  to use an input loop, asking for file names until you do something
;;  to signal that you're done inputting.  It could provide for
;;  file-name completion by calling `read-file-name' to read your
;;  input.
;;
;;  * But what if you could also filter the domain of discourse on the
;;    fly, so that the candidate files were only those matching a
;;    regular expression (regexp) that you typed? Then, the command
;;    definition would need to provide for that behavior too.
;;
;;  * And what if you could then take the complement of that set of
;;    candidate file names, with respect to the complete set of files
;;    in the directory? Or subtract (exclude) some set of file names
;;    from the set of matching names, to get the set of possible
;;    choices?
;;
;;  * And what if the set of potential candidates at each step (regexp
;;    match, complement, set difference) could also be displayed in a
;;    multiple-choice menu?
;;
;;  For such multi-command functionality you need Icicles.
;;
;;  You can tell whether a command is a multi-command when you execute
;;  it: if it is a multi-command, then the prompt is prefixed by `+'.
;;  For example, multi-command `icicle-find-file' uses this prompt:
;;
;;    + File or directory:
;;
;;  Normal, non-multi-command `find-file' uses this prompt, which has
;;  no `+':
;;
;;    Find file:
;;
;;  Just remember that `+' means that you can choose any number of
;;  inputs.  For a list of predefined Icicles multi-commands, use
;;  `icicle-completion-help' (`C-?' in the minibuffer during
;;  completion) - search for `+' at the beginning of a line.
;;
;;(@* "How Does a Multi-Command Work?")
;;  ** How Does a Multi-Command Work? **
;;
;;  When an Icicles multi-command prompts you for input, you can make
;;  a single choice and press `RET' to confirm it, as usual, or you
;;  can choose any number of completion candidates, using `C-RET' (or
;;  `C-mouse-2') for each.  You can thus act on multiple candidates,
;;  or even multiple times on the same candidate, during the same
;;  execution of the command.
;;
;;  But you don't have to - you can use any multi-command just as if
;;  it were a normal, single-choice command.
;;
;;  For example, command `icicle-delete-file' lets you delete a single
;;  file or a set of files that match your minibuffer input - all in
;;  the same command execution.  If you type no input, then all files
;;  in the current directory match, and you can delete any number of
;;  them individually.  If you type `~$' and hit `S-TAB'
;;  (`icicle-apropos-complete'), then all files that end in `~' match,
;;  and you can delete any number of them.  Similarly, command
;;  `icicle-buffer-other-window' lets you display any number of
;;  buffers, and so on.
;;
;;  You make multiple choices this way by cycling through the
;;  candidate completions, as usual, and hitting `C-RET' whenever you
;;  want to choose (act on) the current cycle candidate.  Or just
;;  press and hold Control while clicking each chosen candidate with
;;  `mouse-2'.
;;
;;  Similarly, you can use `C-next', `C-prior', `C-down', and `C-up'
;;  to act on successive candidates, forward or backward.  You can
;;  thus just hold down the Control key while cycling, to act on each
;;  candidate in turn, if you want.
;;
;;  Instead of, or in addition to, cycling, you can use completion to
;;  get to a particular candidate you want.  No matter how a candidate
;;  is made current, you can choose the current candidate (perform the
;;  action on it) using `C-RET' or `C-mouse-2'.
;;
;;  For lax (permissive) completion, you can act on any input text
;;  with `C-RET' (but not with the other multi-command keys) -- you
;;  need not choose one of the available candidates.  This means, for
;;  example, that you can create any number of new file buffers with a
;;  single `C-x C-f' invocation, as well as open any number of
;;  existing files.
;;
;;  As always, hitting `RET' (or `S-RET') ends the command.  For most
;;  multi-commands, hitting `RET' performs the same action as `C-RET',
;;  but it is possible to have a command that acts differently for
;;  `RET' and `C-RET'.  That is the case, for instance, when help is
;;  displayed via `C-RET'.
;;
;;  You can use `C-RET' or `C-mouse-2' repeatedly to act multiple
;;  times on the same candidate.  A shortcut is to use `C-u' with
;;  `C-RET' or `C-mouse-2'.  That will work if the candidate action
;;  function is designed to be `C-u' sensitive.  This is the case for
;;  the Icicles multi-commands that read the name of a command or
;;  keyboard macro and execute the command or macro:
;;  `icicle-execute-extended-command' (`M-x'), `icicle-kmacro' (`f5'),
;;  and `icicle-execute-named-keyboard-macro' (`C-x M-e').
;;
;;  So, for example, if you use `C-u 10 C-RET' on command
;;  `forward-char' during `M-x' command completion, the cursor
;;  advances 10 characters.  Another example: `C-x M-e C-u 200 C-RET'
;;  on a keyboard-macro candidate `foo' executes `foo' 200 times.  You
;;  can use all of the numeric prefix argument shortcuts, such as
;;  `M--', `M-7', and `C-6', with the exception of `C--', which has a
;;  different meaning (`icicle-candidate-set-difference') in the
;;  Icicles minibuffer.
;;
;;  If `icicle-use-candidates-only-once-flag' is non-nil, then, when
;;  you act on a candidate, it is removed from the list of available
;;  candidates, for clarity.  Commands where this behavior is
;;  appropriate bind this option to a non-nil value.  This is a user
;;  option, but you normally will not customize it.
;;
;;  You can use `C-g' to exit a multi-command at any time, without
;;  making a final choice using `RET'.  If the actions performed by a
;;  multi-command are easily reversible, `C-g' will often restore
;;  things to the way they were before performing the actions.
;;
;;  Does this `C-RET' stuff sound familiar?  Using a multi-command is
;;  similar to accessing help on a candidate
;;  (see (@> "Get Help on Candidates")).  A multi-command is any
;;  command that has a special action defined for use with `C-RET'
;;  (command `icicle-candidate-action') on the current cycle
;;  candidate.  If no such special action is defined, then help on the
;;  candidate is displayed - displaying help is just the default
;;  action for `C-RET', used when no other action is defined.  You can
;;  always access candidate help using the `C-M-' prefix: `C-M-help',
;;  `C-M-f1', `C-M-RET', `C-M-mouse-2', `C-M-next', `C-M-prior',
;;  `C-M-down', and `C-M-up'.
;;
;;  You can also cycle among elements of a set, performing actions, if
;;  you use my libraries `doremi.el', `doremi-cmd.el', and
;;  `doremi-frm.el'.  Like Icicles, DoReMi lets you see the effect of
;;  a choice immediately, whenever you make changes.  Each library has
;;  its own advantages and special uses.  Advantages of Icicles
;;  include:
;;
;;    - completion to candidate values
;;    - restoration after making changes, letting you preview changes
;;      without actually applying them
;;
;;  See Also:
;;
;;  * (@> "More about Multi-Commands") for more about using
;;    multi-commands.
;;
;;  * (@> "Defining Icicles Commands (Including Multi-Commands)")
;;    for how to define your own multi-commands.
;;
;;  * (@> "Moving Between the Minibuffer and Other Buffers").
 
;;(@* "More about Multi-Commands")
;;
;;  More about Multi-Commands
;;  -------------------------
;;
;;  A multi-command is any command that uses input completion and lets
;;  you perform actions on any number of individual completion
;;  candidates without exiting completion.
;;
;;  The default action is invoked on the current candidate by `C-RET'
;;  (`icicle-candidate-action').  There are three other kinds of
;;  actions on individual candidates:
;;
;;  * alternative actions, invoked by `C-S-RET'
;;  * deletion actions, invoked by `S-delete'
;;  * help actions, invoked by `C-M-RET'
;;
;;  This page provides information about alternative actions and
;;  deletion actions.  See (@> "Get Help on Candidates") for
;;  information about using candidate help.
;;  See (@> "Defining Multi-Commands the Hard Way") for information
;;  about defining a custom candidate-help action for a command.
;;
;;  Note that a given command can define any combination of these
;;  four kinds of actions: none of them, any one of them, any two of
;;  them, any three of them, or all four kinds.
;;
;;(@* "Alternative Actions")
;;  ** Alternative Actions **
;;
;;  Just as you can use `C-RET', `C-mouse-2', `C-next', and so on to
;;  invoke a command's default action on multiple completion
;;  candidates individually, so you can use `C-S-RET'
;;  (`icicle-candidate-alt-action'), `C-S-mouse-2', `C-S-next', and so
;;  on to invoke an alternative action that is associated with the
;;  command.  If the main action of a command `my-find-file' is to
;;  open a file, and the alternative action is to print a file, then
;;  you can use `C-S-RET' to print files even as you are completing
;;  the name of a file to be opened.
;;
;;  Icicles search (e.g. `C-c `') is an example of a command where you
;;  can use the alternative action - it replaces all or part of the
;;  current search hit.  It is likely that there will be more commands
;;  that provide alternative actions in the future.  And Emacs-Lisp
;;  programmers can define their own commands to take advantage of
;;  this feature.
;;
;;  See Also: (@> "Search and Replace").
;;
;;(@* "Deleting Objects")
;;  ** Deleting Objects **
;;
;;  When it is defined for a particular command, minibuffer command
;;  `icicle-delete-candidate-object', bound to `S-delete' (that's the
;;  `delete' key, Shifted), deletes the object or objects named by the
;;  completion candidate on which it operates.  (At least that is the
;;  default behavior - if you customize `icicle-deletion-action-flag'
;;  to nil, then `S-delete' has no effect.)
;;
;;  Which objects are thus targeted by a given candidate (name) is
;;  something that must be defined by the particular command.  The doc
;;  string of a command should always indicate the effect of using
;;  `S-delete', if a deletion action is defined.
;;
;;  As an example of a deletion action, Icicles command
;;  `icicle-buffer-other-window', bound to `C-x 4 b', opens buffers
;;  named by the individual candidates you act on, using `C-RET'.  But
;;  it also kills any buffer that you act on, using `S-delete'.  This
;;  is not the alternative action for the command (which is bound to
;;  `C-S-RET'); it is the deletion action.  Similarly, command
;;  `icicle-select-region', bound to `C-u C-x C-x', selects regions,
;;  but you can also use `S-delete' with it to remove individual
;;  regions from the list of saved regions, `icicle-region-alist'.
;;
;;  When you use `S-delete' with commands, such as
;;  `icicle-select-region' (`C-u C-x C-x'), that allow duplicate
;;  candidate names that represent different candidate objects, it
;;  deletes only the object associated with the current candidate
;;  (name).  When you use `S-delete' with other commands (which might
;;  or might not allow duplicate candidate names), where it is not
;;  important to distinguish the different objects represented by the
;;  same name, it deletes all objects with the same target name.
;;
;;  Some multi-commands define a deletion action; some do not.
;;  Consult the doc for any given command to see if it does.  Whenever
;;  it is defined, the meaning of "delete" depends on the particular
;;  command you use.
;;
;;  Note: Although `delete' does not work for file-name candidates in
;;  Emacs releases prior to Emacs 22, `S-delete' does work.
;;  See (@> "Nutshell View of Icicles") for information about this
;;  limitation of `delete'.
;;
;;  If you are an Emacs-Lisp programmer, then you can define your own
;;  multi-commands that provide a deletion action via `S-delete'.
;;  There are two ways to do this.  Both involve binding
;;  `icicle-delete-candidate-object':
;;
;;  * Bind it to a deletion function.  The function must accept a
;;    completion candidate string and perform the deletion.
;;
;;  * Bind it to a symbol (variable) whose value is a list of
;;    completion-candidate objects.  The entries in the list must be
;;    completion candidates for the current call to `completing-read',
;;    but the list itself need not be the TABLE argument to
;;    `completing-read'.  The list can be an alist, a list of strings,
;;    or a list of symbols.  The object that corresponds to the
;;    current candidate when `S-delete' is invoked is deleted from the
;;    list.  If, in addition, the list variable is a user option, then
;;    the updated list value is saved in the user's custom file.
;;
;;  For more information about using this feature in Emacs-Lisp code,
;;  see the doc of function `icicle-delete-current-candidate-object'
;;  (`S-delete') and variable `icicle-delete-candidate-object'.
 
;;(@* "Key Completion")
;;
;;  Key Completion
;;  --------------
;;
;;  Here's another weird Icicles feature: completing key sequences,
;;  instead of commands.  (This feature works only for Emacs 22 and
;;  later.)
;;
;;  What on earth for?  Ever want to use one of those myriad `C-x' key
;;  sequences, but forget just what it was?  The standard solution to
;;  that is to use `C-x C-h', to display all of the `C-x' bindings
;;  together with their commands.
;;
;;  OK, but then you have to scroll down the list of bindings,
;;  searching for the command you want, and then use its key binding.
;;  You can use `C-M-s' to search for a substring of the command name,
;;  in case you don't recall the exact name, but why not use Icicles
;;  completion for this?  Why not match against possible key sequences
;;  and commands?
;;
;;(@* "Completing Keys")
;;  ** Completing Keys **
;;
;;  To complete keys in Icicles, start the key sequence as usual, and
;;  then hit `S-TAB' (command `icicle-complete-keys').  For example,
;;  use `C-x' or `C-x 4', and then hit `S-TAB' to complete the prefix
;;  `C-x' or `C-x 4' (or whatever).  You're then completing against
;;  candidates that are composed of two parts, separated by "  =  ":
;;
;;  * a key binding that completes what you've typed so far -
;;    e.g. `C-j' (that is, `C-x C-j')
;;
;;  * the command it is bound to - e.g. `dired-jump-other-window'
;;
;;  So, for example, this is a single completion candidate:
;;
;;    C-j  =  dired-jump-other-window
;;
;;  You can match your minibuffer input against the key name, the
;;  command name, or both.
;;
;;  Suppose, for instance, that you want to use a version-control
;;  command, and you remember that all such commands are bound to key
;;  sequences that begin with `C-x v'.  You enter as much of the key
;;  sequence as you remember (`C-x v'), and then you hit `S-TAB'.  You
;;  can then use completion (either apropos or prefix) against the
;;  matching key sequences and command names to invoke the right
;;  command.  And, as a bonus, you are reminded of its key sequence.
;;  You can thus use Icicles key completion to execute a command and,
;;  at the same time, learn its key binding.
;;
;;(@* "`S-TAB' Is Everywhere - Start With It")
;;  ** `S-TAB' Is Everywhere - Start With It **
;;
;;  In Icicle mode, whenever you are not in the minibuffer or buffer
;;  `*Completions*', key `S-TAB' initiates key completion.  That is,
;;  you don't need to first type part of a key sequence to use it -
;;  you can start with it.  Hit `S-TAB' at any time, and you're
;;  completing a key sequence, even if you haven't yet hit any keys.
;;  This lets you see all key sequences that are available in a given
;;  context.  For example, in Dired, keys special to that mode are
;;  included (and are highlighted as local bindings -
;;  see (@> "Local Bindings Are Highlighted")).
;;
;;  When completing a key sequence, you can type part of a command
;;  name, then hit `S-TAB' to apropos-complete against the command
;;  name.  In this respect, `S-TAB' acts like `M-x', but the key
;;  binding is also part of the completion candidate, so you can also
;;  match key names.
;;
;;(@* "Completing Keys By Name")
;;  ** Completing Keys By Name **
;;
;;  So, just how do you complete input against a set of
;;  binding-plus-command completion candidates?  You can always cycle
;;  among the candidates, of course, and then choose one.  But what
;;  about completion?  Just type text to match candidates, then use
;;  `S-TAB' or `TAB' as usual to complete the text.  Text?  Yes.
;;  Completion candidates are always, ultimately, strings.
;;
;;  Suppose that you type `C-x S-TAB' to show all key sequences that
;;  begin with `C-x'.  You might see a candidate that looks like this:
;;
;;    C-q  =  toggle-read-only
;;
;;  You can then type "C-q" or "d-onl" or any other substring, and
;;  then use `S-TAB' to complete the candidate.  (This second use of
;;  `S-TAB' invokes the command `icicle-apropos-complete', which has
;;  nothing to do with `icicle-complete-keys', which was invoked by
;;  the first `S-TAB'.  The first was invoked outside the minbuffer;
;;  the second was invoked from the minibuffer, during completion.)
;;
;;(@* "Completing Prefix Keys")
;;  ** Completing Prefix Keys **
;;
;;  What happens if the completion candidate is itself a prefix key?
;;  For example, `C-x S-TAB' shows some candidates whose commands are
;;  shown as "...", like this:
;;
;;    4  =  ...      5  =  ...
;;    6  =  ...      C-k  =  ...
;;    ESC  =  ...    RET  =  ...
;;
;;  These represent prefix keys (`C-x 4', `C-x C-k', and so on).  If
;;  you choose such a candidate, then you just continue completing -
;;  buffer `*Completions*' is updated to show the completions of the
;;  compound prefix: `C-x 4', `C-x RET', or whichever you choose.  The
;;  minibuffer prompt shows the completion so far; if you choose
;;  `RET', for instance, then it shows `C-x RET' while prompting you
;;  for the rest of the key sequence.
;;
;;  By default, completion candidates are sorted in buffer
;;  *Completions* with local bindings listed first.  You can use
;;  `C-M-,' at any time during completion to toggle between this order
;;  and sorting with the prefix-key candidates shown first.  You can
;;  use `C-,' at any time to change the sort order among these two
;;  orders and sorting sorting by command name.  (Except in contexts,
;;  such as searching, when candidate sorting is not possible.  In
;;  those contexts, `C-,' has a different meaning.)
;;
;;  Gotcha: Commands that are remapped do not show up with the
;;  bindings you think they have.  For example, `C-x C-f' is bound to
;;  `icicle-find-file' in Icicle mode, by default, but `C-x S-TAB'
;;  does not include the completion candidate `C-f  =
;;  icicle-find-file'.  Instead, `S-TAB' at the top level (without
;;  first doing `C-x') shows a (pseudo) prefix key `remap  =  ..', and
;;  if you follow that then you'll see the candidate `find-file  =
;;  icicle-find-file'.  The binding of `C-x C-f' does not appear as
;;  such, because `find-file' is remapped to command
;;  `icicle-find-file': whatever `find-file' was bound to is
;;  indirectly bound to `icicle-find-file'. This indirection shows up
;;  in Icicles key completion as (pseudo) prefix key `remap  =  ..'.
;;
;;(@* "Meta Key Bindings")
;;  ** Meta Key Bindings **
;;
;;  If you use `S-TAB' at the top level, and you look for the key
;;  sequence `M-x' or `M-:' or even `M-d', you won't find it.  Meta
;;  key bindings are there, but many of them are disguised as keys in
;;  the `ESC' prefix keymap - e.g. `ESC x' for `M-x'.  That is, you
;;  must first choose the `ESC` prefix key: `ESC  =  ...', and then
;;  choose the `x' key or whatever.  That's just the way Emacs
;;  works.  So, yes, you can use Icicles key completion to execute any
;;  Emacs command, even one that is not bound to a key sequence, and
;;  you can use it to evaluate any EmacsLisp expression.  See
;;  (@> "Three-Key Emacs").
;;
;;(@* "Navigate the Key-Binding Hierarchy")
;;  ** Navigate the Key-Binding Hierarchy **
;;
;;  Choosing a completion candidate such as `C-x  =  ...' effectively
;;  navigates down the key-binding hierachy (prefix-key hierarchy), to
;;  complete against all keys with prefix `C-x'.  Choosing `5  =  ...'
;;  to complete the prefix `C-x' then navigates down another level, to
;;  complete keys that have prefix `C-x 5'.
;;
;;  What about navigating back up the hierarchy, say from the `C-x 5'
;;  keys to the `C-x' keys, or from the `C-x' keys to the keys with no
;;  prefix?  The special completion candidate `..' does that.  By
;;  default, it is always the first candidate in the *Completions*
;;  list.  It is of course not available unless you are completing a
;;  prefix; that is, it is not available at the top level.
;;
;;  This feature means that you can navigate the key-binding hierachy
;;  just as you would navigate the file system hierarchy (using, say,
;;  `C-x C-f') or the menu-bar hierarchy (using library
;;  `icicles-menu.el').  (In fact, since menu-bar bindings are also
;;  key bindings, you can also use key completion to navigate the
;;  menu-bar hierarchy - just complete prefix key `menu-bar'!)
;;
;;  Icicles key completion thus provides a general browser for key
;;  bindings, which you can also use to learn about keys and their
;;  associated comands, without necessarily executing them - see
;;  (@> "Key and Command Help").
;;
;;  Gotcha: `S-TAB' uses apropos completion, by default, so remember
;;  that typing `.' matches any character (except a newline).  To
;;  match only `..', either use prefix completion (`TAB') or escape
;;  the regexp special character: `\.\.' (or use `^\.').
;;
;;(@* "Local Bindings Are Highlighted")
;;  ** Local Bindings Are Highlighted **
;;
;;  Sometimes it helps to know which key sequences are local bindings,
;;  that is, bindings that are specific to the current mode.  For
;;  example, Dired mode defines keys specific to Dired buffer, such as
;;  `* %', `% g', and `!'.  To help you distinguish local key bindings
;;  from others (global and minor-mode bindings), local bindings are
;;  highlighted in buffer *Completions* using face
;;  `icicle-special-candidate'.
;;
;;(@* "Completing Keys By Just Hitting Them")
;;  ** Completing Keys By Just Hitting Them **
;;
;;  It may seem odd that you must complete a key sequence by entering
;;  the names of keys, rather than just hitting the keys themselves:
;;  e.g. typing "C-f" rather than hitting `C-f'.  However, if keys
;;  themselves were used for completing, then they could not be used
;;  normally during key-sequence completion.  You could not move the
;;  cursor around the minibuffer using `C-f' or `right' (right arrow),
;;  because those keys would be treated as input for completion.  You
;;  could not use `up' or `down' to cycle among completion candidates
;;  for the same reason.  Likewise, you could not use printing
;;  (self-inserting) keys, such as `a' and `$', to match command
;;  names.  Having to use key names, instead of keys, for completion
;;  is a small price to pay for being able to complete key sequences.
;;
;;  Nevertheless, Icicles also provides a way for you to type key
;;  sequences directly, even if it is something of a workaround:
;;  precede each key with `M-q' (`icicle-insert-key-description',
;;  during key completion) - think of `q' for "quote".  This inserts
;;  the key description of whatever key you hit next.  This key
;;  description (name) can be used to match key-completion candidates.
;;  So, for example, instead of typing "C-f", you can hit `M-q' and
;;  then hit `C-f'.  The key description "C-f" is inserted in the
;;  minibuffer.  If you use `M-q C-M-right', then "C-M-right" is
;;  inserted.  Try it: `S-TAB M-q C-M-right' -> "C-M-right".  Then hit
;;  `TAB' or `S-TAB' to complete the candidate all the way to this:
;;
;;    C-M-right  =  enlarge-frame-horizontally
;;
;;  Note: Whether or not angle brackets are used is governed by user
;;  option `icicle-key-descriptions-use-<>-flag'.  By default, this is
;;  nil, so angle brackets are not used, which I think improves
;;  readability.  If you set this to non-nil, then you will see
;;  "<C-M-right>" instead of "C-M-right", both as a completion
;;  candidate and as what is inserted when you use `M-q'.  You can
;;  toggle this option value at any time using `C-<'
;;  (`icicle-toggle-angle-brackets') in the minibuffer.  You can also
;;  provide a prefix argument to `M-q' to flip the behavior of
;;  `icicle-key-descriptions-use-<>-flag' for that occurrence only.
;;
;;(@* "Key and Command Help")
;;  ** Key and Command Help **
;;
;;  That points out another use of key completion, opposite to
;;  learning the bindings of commands: learning the commands bound to
;;  given keys.  In other words, `S-TAB M-q' does both what `C-h w'
;;  (`where-is') does and what `C-h c' (`describe-key-briefly') does.
;;  It also does what `C-h b' (`describe-bindings') does.
;;
;;  The point here is not that `S-TAB M-q' is quicker than `C-h w' or
;;  `C-h c' or `C-h b' - it's not.  The point is that key completion
;;  can be handy in several ways, and it can teach you various things
;;  about keys and commands as you use it.
;;
;;  In addition to this key-completion help about bindings, you can
;;  display help on the commands that are the right sides of the
;;  `S-TAB' completion-candidate equations, by using the multi-command
;;  help keys (see (@> "Help on Completion Candidates")).  That is,
;;  while completing, you can use `C-M-mouse-2', `C-M-RET',
;;  `C-M-next', and so on to describe the command named in the current
;;  completion candidate.
;;
;;(@* "`S-TAB' Is a Multi-Command")
;;  ** `S-TAB' Is a Multi-Command **
;;
;;  Yes, `S-TAB' as `icicle-complete-keys' is a multi-command (see
;;  (@> "Multi-Commands")).  This means that you can, within the same
;;  execution of `S-TAB', invoke any number of keys by clicking
;;  (`C-mouse-2') their names in buffer *Completions* or choosing them
;;  any other way (`C-RET', `C-next', and so on).
;;
;;  Since you can navigate up and down the key-binding hierarchy, you
;;  could even stay within a single `S-TAB' invocation to do nearly
;;  everything you want in Emacs (see (@> "Three-Key Emacs"))!
;;
;;(@* "Possible Source of Confusion")
;;  ** Possible Source of Confusion **
;;
;;  Keep in mind that `S-TAB' has two different uses in Icicles when
;;  you are providing input in the minibuffer:
;;
;;  * If input completion is available, then `S-TAB' performs apropos
;;    completion (it is, in effect, bound to
;;    `icicle-apropos-complete').
;;
;;  * If input completion is not available, then `S-TAB' performs key
;;    completion (it is, in effect, bound to `icicle-complete-keys').
;;
;;  This is by design; it takes advantage of the fact that these two
;;  contexts are mutually exclusive.  However, this economy comes at a
;;  risk of potential confusion.  It's important that you know whether
;;  or not completion is available when you are inputting text.  If
;;  input completion is not available, but you think it is, then
;;  hitting `S-TAB' might give you a surprise by key completing.  That
;;  behavior is normal - you can use key-completion to input special
;;  characters, for instance.  But if you think that you are instead
;;  completing the original input requested, then you can become
;;  confused.
;;
;;  You can always know whether or not completion is possible when you
;;  are inputting text in the minibuffer, by looking at the start of
;;  the minibuffer prompt.  When input completion is available, the
;;  prompt is prefixed by a highlighted string, by default either ` '
;;  (space) or `=', indicating whether the completion is lax
;;  (permissive) or strict (must-match), respectively.  The actual
;;  strings used, as well as the highlighting faces used, are the
;;  values of `icicle-completing-prompt-prefix' and
;;  `icicle-completing-mustmatch-prompt-prefix'.
;;
;;  So, when you are inputting, keep an eye out for this highlighting.
;;  If you don't see it when you are prompted for input, it means that
;;  input completion is not available, and it also means that `S-TAB'
;;  is available, not for input completion, but for key completion.
;;  Another clue can be found in the prompt text.  For key completion,
;;  it says "Complete keys: ".
;;
;;  If you nevertheless find this overloaded use of `S-TAB' confusing,
;;  you can change the binding of the key `S-TAB' in
;;  `icicle-mode-map'.  In that case, take a look at the definition of
;;  `icicle-generic-S-tab', which is bound to `S-TAB' in
;;  `icicle-mode-map'.  That generic command "does the right thing",
;;  calling either `icicle-apropos-complete' or
;;  `icicle-complete-keys', depending on whether you are inputting
;;  with or without completion.
;;
;;(@* "Three-Key Emacs")
;;  ** Three-Key Emacs **
;;
;;  Icicles key completion piles a lot of stuff into `S-TAB'.  Just as
;;  `M-x' lets you execute any Emacs command, so does `S-TAB'.  But
;;  `S-TAB' also lets you insert characters.  With the exeception of
;;  inserting multi-byte characters, you might say that it gives you
;;  all of Emacs in one key binding.
;;
;;  Of course, you need a couple other keys, as well.  How many?
;;  Suppose you had limited accessibility in terms of input devices.
;;  Maybe you use Emacs on a cell phone, without voice recognition -
;;  or whatever.  How many keys, buttons, or whatnot do you need to
;;  use Emacs?
;;
;;  1. You need one for `C-g', to interrupt commands.
;;  2. You need one to start telling Emacs what to do.
;;  3. You might need one to choose from a set of possible things to
;;     do.
;;  4. You need one to tell Emacs you're done telling it what to
;;     do.
;;
;;  (#2 and #3 might be combined somehow.)
;;
;;  What does vanilla Emacs offer out of the box in this regard?
;;
;;  * You can get by with just `mouse-1' and the menu-bar menus, but
;;    they don't cover all of Emacs.  You can't use them to enter
;;    text, for instance.  Of course, you could add more menus, to be
;;    able to do more.
;;
;;  * You can use `M-x' plus `RET' to execute any command.  But how
;;    would you insert text?
;;
;;  * Similarly, for `M-:', which lets you evaluate any EmacsLisp
;;    sexp.  You still need a way to type characters.
;;
;;  Icicles key completion lets you do almost anything in Emacs with
;;  three or four keys, buttons, or whatever:
;;
;;  * `S-TAB' - Offers every key sequence as a possible choice to
;;              execute.
;;  * `next'  - Cycles among candidates, for choosing.
;;  * `RET'   - Chooses the current candidate.
;;  * And of course `C-g'.
;;
;;  `S-TAB' includes key `M-x (represented as prefix-key `ESC'
;;  followed by `x'), which offers all commands (even those not bound)
;;  as possible choices.  It also includes key `M-:' (`ESC' followed
;;  by `:'), which lets you execute any Emacs-Lisp expression.  That's
;;  almost all of Emacs!
;;
;;  You could even perhaps get away with only three mouse buttons, and
;;  no keyboard:
;;
;;  * `mouse-1' - Choose candidates, scroll, and so on (direct access,
;;    no cycling).
;;
;;  * `mouse-2' - Do what `S-TAB' does (bind it to
;;    `icicle-complete-keys' and `icicle-apropos-complete').
;;
;;  * `mouse-3' - Do what `C-g' does (bind it to `keyboard-quit' and
;;    `icicle-abort-minibuffer-input').
;;
;;  Here, `mouse-2' and `mouse-3' aren't even used as mouse (pointer)
;;  functions; any keys or buttons would do.  You could use just
;;  `mouse-1' plus a Shift key and a Control key.
;;
;;  Would you want to use Emacs only this way?  Of course not, if you
;;  had a choice.  Typing the character `a' by cycling through every
;;  possible key binding/command combination and hitting `RET' when
;;  you get to `a  =  self-insert-command' would be the epitome of
;;  tedium.  Likewise, doing everything with a single pointer-device
;;  button.  Using only three or four keys or buttons is definitely
;;  not the ideal way to take advantage of Emacs.
;;
;;  But you are probably not limited to just 3 or 4 keys or buttons.
;;  The real point here is that Icicles `S-TAB' opens the door to
;;  almost everything in Emacs.  And if you do have a normal keyboard,
;;  then you can type letters and such to match command names and key
;;  sequences.  Key `next' matches substrings (regexps, actually),
;;  which makes choice even quicker.
;;
;;  Why only "almost" everything in Emacs?  Because you cannot use
;;  Icicles `S-TAB' to input multi-byte characters (e.g. Chinese,
;;  Japanese, Unicode).  Such characters are grouped in Emacs into
;;  character groups called "generic characters", and it is the
;;  generic characters, not the individual multi-byte characters that
;;  are bound to `self-insert-command'.  Icicles excludes these
;;  special key bindings, because you cannot simply execute
;;  `self-insert-command' to insert these characters.  (It is possible
;;  to implement a completion extension to input such characters, but
;;  that feature has not yet been implemented in Icicles.)
;;
;;  Enjoy!
;;
;;(@* "Entering Special and Foreign Characters")
;;  ** Entering Special and Foreign Characters **
;;
;;  Command `self-insert-command' is bound to each key that is
;;  associated with a character that can be inserted in text.  It is
;;  the binding of the key `a' and the key `$'.  It is also the
;;  binding of keys that your keyboard might not even have - keys that
;;  correspond to special or odd characters and characters in other
;;  languages.
;;
;;  To Icicles key completion, these keys are like any other keys, and
;;  `self-insert-command' is like any other command.  However, because
;;  there are many, many keys bound to it, it can be distracting to
;;  allow such keys as completion candidates.  If option
;;  `icicle-complete-keys-self-insert-flag' is nil (the default
;;  value), then such keys are excluded as candidates.
;;
;;  If it is non-nil, then you can use key completion to insert
;;  characters that your keyboard has no keys for.  This provides a
;;  sort of universal input-method feature that works, in principle,
;;  for all characters (but see below, for exceptions).
;;
;;  To use this feature, just choose a character description (name)
;;  with the mouse or by cycling, if you cannot type its description
;;  (name) with your keyboard.  You can even insert characters this
;;  way that your system has no font for - they will be displayed as
;;  empty boxes, but they will be correctly inserted.
;;
;;  There is an exception, however.  You cannot insert some characters
;;  this way, because they don't have a one-to-one relation with keys
;;  that are bound to `self-insert-command'.  In such cases, "keys"
;;  are bound to `self-insert-command' that represent not single
;;  characters but groups of characters.  Icicles filters out these
;;  keys, so they are not available as completion candidates.  The
;;  problematic keys are in Korean, Chinese, Japanese, Ethiopic,
;;  Indian, Tibetan, and some Unicode character sets.
;;
;;  Because insertion of special characters is useful, but is a
;;  special case of key completion, there is a separate Icicles
;;  command that you can use just for that: `icicle-insert-char'.  It
;;  is a specialized version of `icicle-complete-keys' that uses
;;  `self-insert-command' as the only possible command for completion.
;;
;;(@* "Handling Keymaps That Are Inaccessible From the Global Map")
;;  ** Handling Keymaps That Are Inaccessible From the Global Map **
;;
;;  Actually, `S-TAB' is not bound to `icicle-complete-keys' in every
;;  keymap.  That would be inconvenient, in general.  By default, it
;;  is so bound in each keymap that is accessible from the global
;;  keymap, as determined by function `accessible-keymaps'.
;;
;;  You've seen, above, how you can navigate through prefix keys,
;;  starting with the global map.  In DiredMode, for instance, you can
;;  use `S-TAB' at the top level, then choose the prefix key `*' in
;;  *Completions*, then choose a key, such as `/' (to mark
;;  directories), in the `*' keymap.
;;
;;  However, the act of binding of `S-TAB' in keymaps that are
;;  accessible from the global map does not bind it in the `*' prefix
;;  keymap itself.  To handle this case, Icicles explicitly does for
;;  `dired-mode-map' what it does for the global map: it binds `S-TAB'
;;  in each (non-menu) keymap that is accessible from
;;  `dired-mode-map'.  (Menu maps are always skipped.)  Because of
;;  this, you can use `* S-TAB' to show all key completions of `*'.
;;
;;  This treatment of `dired-mode-map' is done by default.  But you
;;  might have other keymaps that you would like to treat similarly -
;;  keymaps that Icicles might be unaware of.  You do this by
;;  including them in the list value of user option
;;  `icicle-keymaps-for-key-completion'.  The list entries are
;;  Emacs-Lisp symbols that are bound to keymaps, each of which should
;;  define at least one prefix key.  If you add a keymap variable to
;;  this list, then `S-TAB' will be bound so that you can use it to
;;  complete the prefix keys defined by that map.
;;
;;  Notice that there is no keymap variable that corresponds to prefix
;;  key `*' in Dired mode.  You need only provide a keymap (variable
;;  `dired-mode-map') from which the prefix key is accessible; it is
;;  not necessary to also provide a variable that corresponds to the
;;  prefix keymap itself.
;;
;;  If a keymap listed in `icicle-keymaps-for-key-completion' is not
;;  defined when Icicle mode is entered, then it is ignored.  If you
;;  later define that keymap, then just exit and reenter Icicle mode
;;  for the `S-TAB' binding to take effect.  For example, use `M-x
;;  icy-mode' twice after entering Calendar mode, to be able to
;;  complete `calendar-mode' prefix keys such as `t' - `t S-TAB'.
 
;;(@* "Icicles Multi `M-x'")
;;
;;  Icicles Multi `M-x'
;;  -------------------
;;
;;  How about a multi-command replacement for `M-x'?  Instead of
;;  executing a single command, it would execute any number of
;;  commands.
;;
;;  When you use `M-x' in vanilla Emacs, you are actually executing
;;  the standard Emacs command `execute-extended-command'.  That
;;  command prompts you for the name of another command, which you
;;  input.  It uses `completing-read' to do this, which is why you can
;;  take advantage of Icicles features when you use `M-x'.  Nothing
;;  new here.
;;
;;  Command `icicle-execute-extended-command' is simply a
;;  multi-command version of `execute-extended-command'.  It does the
;;  same thing, except that it also lets you execute multiple
;;  commands, one by one, using `C-RET' (or `C-next' and so on),
;;  without ever exiting the minibuffer.
;;
;;  If option `icicle-bind-top-level-commands-flag' is non-`nil', then
;;  `M-x' is bound to `icicle-execute-extended-command' whenever you
;;  are in Icicle mode.  If you never use it as a multi-command, you
;;  won't notice any difference from `execute-extended-command'.
;;
;;(@* "Examples of Using Multi `M-x'")
;;  ** Examples of Using Multi `M-x' **
;;
;;  Example: Repeat a command multiple times.  Yes, `C-x z' does this
;;  already (and better) - this is just an illustration.  `M-x
;;  forward-ch TAB' completes to `forward-char'.  Then, use `C-RET' to
;;  execute that command.  Repeat as many times as you want.  Use a
;;  prefix arg if you like.
;;
;;  To switch to another command in the same `M-x' invocation: Erase
;;  the minibuffer (`M-k'), complete the second command, then use
;;  `C-RET'.  As long as you haven't yet used `RET', `S-RET', `C-g'
;;  (or, say, `C-]'), you remain within the same invocation of `M-x'.
;;
;;  What about executing a command that, itself, reads an input
;;  argument?  That's OK.  And if that command reads its input with
;;  completion, then you can use `C-RET' on the completion candidates
;;  for that input.
;;
;;  Example: `M-x describe-fa TAB C-RET' gives you the prompt for
;;  command `describe-face'.
;;
;;  1. Type `ici S-TAB' to see the available Icicles faces.
;;
;;  2. Hit `next' until face `icicle-complete-input' is highlighted.
;;
;;  3. Hit `C-RET' to display its documentation.
;;
;;  4. Type `C-next' a few times to see the doc of other Icicles
;;     faces.
;;
;;  5. Use `M-k' to erase the minibuffer, then type `search S-TAB' to
;;     see faces about searching.
;;
;;  6. Cycle through them with `next', then use `C-RET' on
;;     `icicle-search-main-regexp-current' to show its documentation.
;;
;;  7. Use `C-next' to do the same for face
;;     `icicle-search-main-regexp-others'.
;;
;;  8. Use `RET' to finish with command `describe-face' - but you're
;;     still in the same invocation of `M-x'.
;;
;;  9. Change the input to `describe-coding-system' and play again,
;;     this time with coding-system names...
;;
;;  Remember, if you get confused or lost: `C-]'
;;  (`abort-recursive-edit') or `M-x top-level' will always straighten
;;  you out.
;;
;;(@* "What about describe-variable and describe-function?")
;;  *** What about describe-variable and describe-function? ***
;;
;;  Sadly, if you try the last example with `describe-variable' or
;;  `describe-function', you might be in for a surprise.  In Emacs 20,
;;  they both work fine.  In later Emacs versions, `describe-variable'
;;  gives you the message "You did not specify a variable", and
;;  `describe-function' displays a *Help* buffer that says that each
;;  function you choose is really a keyboard macro!
;;
;;  Why?  It's a bit complex, but worth hearing about if you want to
;;  understand multi M-x better.
;;
;;  When you choose a command that reads an argument in the minibuffer
;;  and you then hit a multi-command key such as `C-RET' to choose an
;;  argument, Icicles tries to apply the command you chose to the
;;  argument you chose.  However, completion candidates are always
;;  strings, and the command you chose might expect something other
;;  than a string.  That is the case for `describe-variable', for
;;  instance.  The case of describe-function' is special: it
;;  interprets a string argument blindly as a keyboard macro sequence.
;;
;;  Icicles is smart enough to pick up a `wrong-type-argument' error,
;;  if the command you choose barfs on a string argument.  In that
;;  case, Icicles converts the string to a symbol (or a number) and
;;  tries again, using the symbol (or the number).
;;
;;  And that's why `describe-variable' works in Emacs 20 but not in
;;  later versions: In Emacs 20, `describe-variable' (sanely) raises a
;;  type error if you pass it a string, and Icicles is able to save
;;  the day by then passing it the corresponding symbol.  In later
;;  versions of Emacs, however, instead of raising an error with the
;;  message "You did not specify a variable", `describe-variable' just
;;  displays the message - no error, so no way for Icicles to
;;  recuperate.
;;
;;  I've reported this design misfeature to the Emacs developers, and
;;  I hope it will be fixed in a future Emacs version.  Until then, at
;;  least you know...  The more general lesson is this: Icicles can
;;  turn every command into a multi-command, but multi-command actions
;;  won't work for every command.
;;
;;(@* "Multi `M-x' Turns Every Command into a Multi-Command")
;;  ** Multi `M-x' Turns Every Command into a Multi-Command **
;;
;;  Most of the time, of course, you do not execute commands
;;  successively by name; instead, you use key bindings.  The point
;;  here is that even if you have a binding for a command, Icicles
;;  `M-x' lets you use any command as a multi-command, which can
;;  sometimes be advantageous.
;;
;;  For example, Icicles defines and binds a real multi-command to
;;  `C-x 0' in Icicle mode, which lets you delete any number of
;;  windows.  But, even without such a multi-command, you can get a
;;  similar effect by using `M-x delete-windows-on'.  In this way, you
;;  can turn ordinary Emacs commands that use completion into
;;  multi-commands.
;;
;;  The other point is that you can move from one command to another
;;  within the same execution of `M-x'.  This is a different feature
;;  from being able to use any command that uses completion as a
;;  multi-command.  Both features have their uses.
;;
;;  See Also: (@> "Defining Icicles Multi `M-x'").
 
;;(@* "Choose All Completion Candidates")
;;
;;  Choose All Completion Candidates
;;  --------------------------------
;;
;;  The previous section describes how you can use `C-RET'
;;  (`icicle-candidate-action') to choose (act on) multiple completion
;;  candidates, individually.  If you hold down the Control key while
;;  you cycle through the candidates, you can run through each of
;;  them, one by one.
;;
;;  Command `icicle-all-candidates-action', which is bound to `C-!' in
;;  the minibuffer, is a shorthand way of doing that: act on all
;;  candidates that match the current input.  In many contexts, `C-!'
;;  reports on any objects that were not acted upon successfully (in
;;  buffer *Help*).
;;
;;  All multi-commands let you use `C-!' in this way.  Whenever a
;;  command defines a special action for `C-RET' to perform on the
;;  current completion candidate, you can use `C-!' to perform it on
;;  all candidates at once.
;;
;;  Perhaps you already use `% m' (command `dired-mark-files-regexp')
;;  in Dired to mark all files that match a given regular expression,
;;  and then operate on all of the marked files in some way (search
;;  with `A', query-replace with `Q', open with `F', delete with `D',
;;  and so on).  When you execute a multi-command, `C-!' lets you do
;;  something similar.
;;
;;  How does it work?  It applies `icicle-candidate-action-fn' to each
;;  completion candidate that (apropos- or prefix-) matches the
;;  current input in the minibuffer.  More precisely, if
;;  `icicle-all-candidates-action-fn' is non-nil, then that is applied
;;  to the list of all matching candidates.  If it is nil, then
;;  `icicle-candidate-action-fn' is applied to each matching
;;  candidate, in turn.
;;
;;  Whereas `icicle-candidate-action-fn' knows about only a single
;;  matching candidate, `icicle-all-candidates-action-fn' has access
;;  to all of them at once, and this lets it act differently from a
;;  simple iteration of `icicle-candidate-action-fn'.  For example,
;;  `icicle-customize-face' uses `icicle-all-candidates-action-fn' so
;;  that `C-!' opens a single Customize buffer for all matching faces,
;;  rather than opening a separate Customize buffer for each, as would
;;  be done by iterating `icicle-candidate-action-fn'.
;;
;;  Most top-level Icicles commands are multi-commands.  Command
;;  `icicle-delete-file' is an example.  Instead of entering a file
;;  name at the prompt (e.g. using completion or cycling), you can
;;  type a regular expression, use `S-TAB' to see all matching files,
;;  and then use `C-!' to delete all of them at once.
;;
;;  You get the idea: Use the minibuffer to determine a set of objects
;;  by pattern matching, and then act on all elements of the set.
 
;;(@* "Sets of Completion Candidates")
;;
;;  Sets of Completion Candidates
;;  -----------------------------
;;
;;  Whereas `C-RET' acts on individual objects, `C-!'  acts on an
;;  entire set of objects at once, via their names: the set of all
;;  current completion candidates.  There are additional Icicles
;;  commands that act, not on individual completion candidates, but on
;;  one or more sets of completion candidates.
;;
;;  One of these is `M-*', which effectively narrows the set of
;;  completion candidates by taking the intersection of the candidate
;;  sets defined by various input regexps.
;;
;;  This section presents some more Icicles commands that act on sets
;;  of completion candidates.  The basic idea is that you can perform
;;  set operations using the current set of completion candidates,
;;  changing it into a different set.  You can, then, for example, use
;;  `C-!' to act on everything in a custom-defined set.  Or you can
;;  define a custom set that you want to use often (for example, a
;;  list of project files), save it persistently, and then retrieve it
;;  later to use for completion.
;;
;;(@* "Saving and Retrieving Completion Candidates")
;;  ** Saving and Retrieving Completion Candidates **
;;
;;  Set operations such as union and difference act on two sets.  The
;;  current set of completion candidates is always one of these sets.
;;  If an operation, such as set complement, acts on a single set,
;;  then it acts on the current set.
;;
;;  When two sets are involved, the other set is called the "saved
;;  set".  This just means that at some previous time in your sesssion
;;  you saved some completion candidates as the value of variable
;;  `icicle-saved-completion-candidates'.
;;
;;  By default, the saved set is not persistent; it is saved only
;;  until the next save in the same Emacs session overwrites it or
;;  adds to it.  See (@> "Persistent Sets of Completion Candidates")
;;  for ways to save candidates persistently.
;;
;;  One way you can save candidates is to use
;;  `icicle-candidate-set-save', bound to `C-M->'.  This saves all of
;;  the current candidates.  Another way is to select candidates in
;;  buffer *Completions* using the (active) region, and then use
;;  `icicle-candidate-set-save-selected', bound to `C-M-)'.  This
;;  saves any candidates that are at least partially in the region.
;;
;;  Command `icicle-mouse-candidate-set-save', bound to `M-S-mouse-3'
;;  in *Completions* combines these two: if the region is active, then
;;  the selected candidates become the saved set; otherwise, all
;;  candidates are saved.  This binding makes it easy to save
;;  candidates using the mouse: select them (e.g. drag or double-click
;;  `mouse-1', or click `mouse-1' then `mouse-3'), then use
;;  `M-S-mouse-3' to save.
;;
;;  Note that if no candidates are selected, `C-M->' saves all
;;  candidates, but `C-M-)' resets the set of saved completions to
;;  none, because you have asked to replace that set with no
;;  candidates.
;;
;;  You can process the list of saved candidates in any way you like
;;  using Emacs Lisp.  For example, you can save a list of file names
;;  that match a regexp, then print the list or process the individual
;;  files in some way.  Here, for instance, is how to save the set of
;;  file names that contain either `dir' or `ici':
;;
;;    `C-x C-f \(dir\|ici\) S-TAB C-M-> C-g'
;;
;;  You can retrieve a set of saved candidates with command
;;  `icicle-candidate-set-retrieve', bound to `C-M-<'.  This replaces
;;  the current set of candidates with those retrieved.
;;
;;  You can add additional candidates to the set of saved candidates
;;  in these ways:
;;
;;  * `C->' (`icicle-candidate-set-save-more') adds all of the current
;;    candidates.
;;
;;  * `C-)' (`icicle-candidate-set-save-more-selected') adds any
;;    candidates that you have selected using the region in
;;    *Completions*.
;;
;;  * `M-mouse-3' (`icicle-mouse-candidate-set-save-more') acts the
;;    same as `C-)' or `C->', depending on whether or not the region
;;    is active in *Completions*: it adds selected or all candidates.
;;
;;  * Extending the region with `mouse-3', and then clicking `mouse-3'
;;    again in the same place, acts the same as `C-)'.  That is, click
;;    `mouse-1', then click `mouse-3' twice in another location, to
;;    save all candidates between the `mouse-1' and `mouse-3'
;;    positions.
;;
;;  * `insert' (`icicle-save-candidate') adds just the current
;;    completion candidate (e.g. during cycling).  Clicking a
;;    candidate in *Completions* with `M-S-mouse-2' does the same
;;    thing.
;;
;;  Note that the `insert' key is in a sense the opposite of `delete'
;;  key, during completion.  You use `insert' to save a candidate for
;;  later use, and you use `delete' to remove a candidate from current
;;  consideration.  Similarly, `M-S-mouse-2' is the opposite of
;;  `S-mouse-2'.
;;
;;  Note also that the save and retrieve bindings are `C-M->' and
;;  `C-M-<', but the save-more binding is `C->'.  (`C-<' is totally
;;  unrelated - it toggles how keys are represented.)
;;
;;  In buffer *Completions*, candidates that have been saved are
;;  highlighted using face `icicle-saved-candidate'.
;;
;;  Matching, saving, and retrieving candidates is a powerful way to
;;  interact with completion.  One important use is to prepare a list
;;  of candidates on which to act, and then act on them all at once
;;  using `C-!'.  This is a good way to proceed when you want to
;;  double-check what to act on, before you actually act.  This is the
;;  same idea behind marking files in Dired and then operating on the
;;  marked files with `x'.  It corresponds to what is represented in
;;  some user interfaces by filling out a checklist followed by
;;  clicking `OK'.
;;
;;(@* "Different Places for Saving and Retrieving Candidates")
;;  ** Different Places for Saving and Retrieving Candidates **
;;
;;  You can save completion candidates to a different variable from
;;  `icicle-saved-completion-candidates' by using a numeric prefix
;;  argument to command `icicle-candidate-set-save'; that is, use `C-u
;;  N C-M->'.  Alternatively, use `C-M-}', which is bound to command
;;  `icicle-candidate-set-save-to-variable'.  You are prompted for the
;;  name of the variable, and you can use completion when inputting
;;  it.  During this completion, the only available candidates are
;;  variables that you have used for saved candidates (but completion
;;  is lax, so you can type a new variable name).  The same behavior
;;  works also for `C->', `C-M-)', and `C-)'.
;;
;;  To retrieve completion candidates that were previously saved to a
;;  variable other than `icicle-saved-completion-candidates', so that
;;  they become the current set of candidates, use `C-u N C-M-<',
;;  where N is an integer, or `C-M-{' (`icicle-candidate-set-retrieve'
;;  or `icicle-candidate-set-retrieve-from-variable').
;;
;;  Using a plain prefix argument (`C-u' without a number) with
;;  `C-M->' and `C-M-<' saves or retrieves a candidate set using a
;;  cache file, not a variable.
;;  See (@> "Persistent Sets of Completion Candidates").
;;
;;  When you save candidates to a different variable from
;;  `icicle-saved-completion-candidates', they are not shown in buffer
;;  *Completions* using face `icicle-saved-candidate'.  When you save
;;  candidates to a cache file, they are also saved to
;;  `icicle-saved-completion-candidates', so they are shown in
;;  *Completions* using face `icicle-saved-candidate'.
;;
;;  You can use Icicles commands `icicle-dired-saved-file-candidates'
;;  and `icicle-dired-saved-file-candidates-other-window' to open
;;  Dired on a saved list of file names - only those files are listed
;;  in the Dired buffer.
;;
;;(@* "Set Operations")
;;  ** Set Operations **
;;
;;  The other available set-operation commands for use with completion
;;  candidates, besides saving and retrieving, are these:
;;
;;  * `icicle-candidate-set-swap', bound to `C-%'.  Swap the saved and
;;    current sets of completion candidates.
;;
;;  * `icicle-candidate-set-define', bound to `C-:'.  Define the
;;    current set of completion candidates by evaluating an input
;;    sexpr.  The sexpr must evaluate to a list of strings, such as is
;;    returned by `all-completions'.  You can use this to substitute
;;    any list of strings, and then operate on them as completions,
;;    using any Icicles functionalities.  Keep in mind, however, that
;;    the completions must be of the proper type for the context in
;;    which they are used.  For example, if you are executing a
;;    command, they must be command names.
;;
;;  * `icicle-candidate-set-complement', bound to `C-~'.  Complement
;;    the current set of candidates: replace the current candidate set
;;    with its set complement.  This means all possible completions of
;;    the appropriate type that do *not* match the current input.  You
;;    can combine this with progressive completion (`M-*') to
;;    progressively eliminate candidates that match different inputs.
;;    This process-of-elimination matching is a common Icicles usage
;;    idiom.
;;
;;  * `icicle-candidate-set-union', bound to `C-+'.  Replace the
;;    current candidate set by its union with the saved set of
;;    candidates.
;;
;;  * `icicle-candidate-set-difference', bound to `C--'.  Replace the
;;    current candidate set by its set difference with the saved set
;;    of candidates.  That is, the saved candidates are subtracted
;;    from the current candidates, and the result becomes the current
;;    candidate set.  To obtain the opposite set difference,
;;    subtracting the current candidates from the saved candidates,
;;    just use `icicle-candidate-set-swap' followed by
;;    `icicle-candidate-set-difference'.
;;
;;  * `icicle-candidate-set-intersection', bound to `C-*'.  Replace
;;    the current candidate set by its intersection with the saved set
;;    of candidates.  Unlike the set intersection provided by `M-*',
;;    `C-*' is, in itself, a one-time operation.  `M-*' can be
;;    repeated, using the previous intersection as one of the sets to
;;    be intersected in a new operation.  Both `C-*' and `M-*' use the
;;    current set of matching candidates as one of the sets being
;;    intersected.  But `M-*' reads another input regexp to define the
;;    other set to be intersected, whereas `C-*' uses the saved
;;    candidates set as the other set.  `M-*' is useful for chaining,
;;    to achieve progressive approximation.  `C-*' is useful to
;;    perform an intersection on a set from a previous input reading.
;;
;;  * `icicle-candidate-set-truncate', bound to `M-$'.  Truncate the
;;    set of completion candidates, so that it includes only the first
;;    N candidates (as displayed in *Completions*).  You are prompted
;;    for N.  You can use this when the order of candidates represents
;;    priority in some way, so that you are interested only in the
;;    topmost candidates.
;;
;;  You can operate on or choose from all input values in the set that
;;  results from any of these set operations.  For example, you can
;;  use `C-~' to see the list of objects that do not match the current
;;  input, to cycle among those objects, or to operate on any or all
;;  of them.  Use `C-~' at any time to switch to the complement of the
;;  current set of candidates.
;;
;;  Example: To cycle through all files whose names do not end in
;;           `el', you can do the following:
;;
;;  1. Use `C-f' to read a file name.
;;  2. Type `el$' to match all file names that end in `el'.
;;  3. Use `S-TAB' to show the matching files.
;;  4. Use `C-~' to flip to the complement: files not ending in `el'.
;;  5. Use `next' or `prior' to cycle among the new set of candidates.
;;
;;  A minibuffer message briefly confirms each of the set operations.
;;
;;  When buffer *Completions* is displayed, the union, difference, and
;;  intersection commands scroll the buffer when repeated, just like
;;  `TAB' and `S-TAB' do.  Repeating `icicle-candidate-set-complement'
;;  complements the complement, of course, giving the original set.
;;
;;  Once you have established a set of completion candidates using any
;;  of the candidate-set commands, you can cycle among the candidates
;;  of that set using either prefix or apropos cycling (that is,
;;  `next'/`prior' or `down'/`up').  However, switching from prefix to
;;  apropos cycling (or completion), or vice versa, establishes a new
;;  completion set of the appropriate type, as usual.  Switching
;;  completion type signifies that you are finished with the specially
;;  defined completion set, and you want to redefine it using apropos
;;  or prefix cycling or completion.
;;
;;  Note: Prefix icompletion (`icomplete.el' or `icomplete+.el' - see
;;        (@> "Icompletion")) does not take into account the candidate
;;        set resulting from a set operation: it always displays the
;;        normal set of prefix completions in the minibuffer.
;;
;;  Note: You might have noticed that, as a mnemonic device, the keys
;;        bound to the various set operations use the corresponding
;;        binary arithmetic or Boolean operators: `~' (unary negation)
;;        for complement (not); `*' (multiplication) for intersection
;;        (and); `+' (addition) for union (or); and `-' (subtraction)
;;        for difference.  Note too that the `C--' and `C-+' bindings
;;        mean that you cannot use these key sequences for prefix
;;        arguments - you must use `C-u N', or `M-N' instead, where N
;;        is a possibly signed integer.
;;
;;  See Also:
;;
;;  * (@> "Multi-Commands") for information about `C-RET'.
;;
;;  * (@> "Choose All Completion Candidates") for information about
;;    `C-!'.
;;
;;  * (@> "Progressive Completion") for information about `M-*'.
;;
;;  * (@> "File-Name Input and Locating Files Anywhere") and
;;    (@> "Persistent Sets of Completion Candidates"), for information
;;    about saving completion candidates persistently and retrieving
;;    them later.
;;
;;  * (@> "History Enhancements"), (@> "Google Matching"), and
;;    (@> "Icicles Search Commands, Overview") for examples of other
;;    set operations on input candidates.
 
;;(@* "Google Matching")
;;
;;  Google Matching
;;  ---------------
;;
;;  This section presents nothing new - but you might not want to skip
;;  it.  It points out something that you might not have picked up
;;  yet.  You've learned about Icicles regexp matching and candidate
;;  set operations, but it can be worthwhile to compare how Icicles
;;  matches inputs against completion candidates with how Google
;;  matches search strings against Web pages.  Summary: You can do
;;  pretty much the same things, but the way you accomplish them is
;;  different.
;;
;;(@* "Domain of Discourse")
;;  ** Domain of Discourse **
;;
;;  In Google, the domain of discourse, that is, the possible set of
;;  search hits, is the set of Web pages.  There are also search
;;  fields that limit the domain of discourse by file type, page
;;  number, update date, page position, freedom of use, and even
;;  morality ("Safe Search").
;;
;;  In Icicles (Emacs), the domain of discourse changes automatically,
;;  depending on the current context.  For command-name input, it is
;;  the set of all named commands; for variable-name input, it is the
;;  set of variable names; and so on.
;;
;;(@* "Global Filtering")
;;  ** Global Filtering **
;;
;;  In Google, you can limit searching to specific Web sites, or
;;  exclude certain Web sites from searching.
;;
;;  In Icicles, you can add extra completion candidates, using
;;  variable `icicle-extra-candidates', and you can filter out
;;  candidates, globally, using filter variables
;;  `icicle-must-match-regexp', `icicle-must-not-match-regexp', and
;;  `icicle-must-pass-predicate'.  These are internal Icicles
;;  variables.  You don't normally change them directly; instead, a
;;  command can use them to limit or extend the effective domain of
;;  discourse.  See (@> "Global Filters").
;;
;;  Variable `icicle-must-pass-predicate' applies to the textual
;;  candidates that are displayed in buffer *Completions*.  You can
;;  also apply a predicate to the full alist-entry candidates that are
;;  supplied to `completing-read' or `read-file-name'.  As a
;;  programmer, you can of course do that when your code calls these
;;  functions.  As an Icicles user, you can use `M-&' to define and
;;  apply predicates to such alist-entry candidates on the fly, while
;;  completing.  See (@> "Icicles Search Commands, Overview").
;;
;;(@* "Word Matching and String Matching")
;;  ** Word Matching and String Matching **
;;
;;  Google matches words, by default, but you can specify an "exact
;;  phrase" to get literal string matching.
;;
;;  By default, Icicles (apropos-)matches regexps, but you can use
;;  `\b' in a regexp to perform word matching, and you can use `C-`'
;;  (`icicle-toggle-regexp-quote') to perform exact (literal)
;;  matching.  See (@> "What About Special-Character Conflicts?").
;;
;;(@* "AND Matching and OR Matching")
;;  ** AND Matching and OR Matching **
;;
;;  Google has search fields for AND matching ("with all of the
;;  words") and OR matching ("with at least one of the words").
;;
;;  In Icicles, you can use progressive completion to perform AND
;;  matching: use `M-*' to introduce each term to match.
;;  Alternatively, you can use `C-*'
;;  (`icicle-candidate-set-intersection').  You can use `C-+'
;;  (`icicle-candidate-set-union') to perform OR matching.  See
;;  (@> "Progressive Completion") and
;;  (@> "Sets of Completion Candidates").
;;
;;(@* "NOT Matching")
;;  ** NOT Matching **
;;
;;  Google has a search field for terms that must not occur in search
;;  hits: "without the words".
;;
;;  In Icicles, you can use `C-~' (`icicle-candidate-set-complement')
;;  to exclude matching completion candidates.  You can combine this
;;  with progressive completion, to exclude any number of terms: `toto
;;  C-~ M-* titi C-~ M-* foobar' excludes all candidates matching
;;  toto, titi, or foobar.  Use this process-of-eliminiation technique
;;  to progressively pare down the set of possible candidates.  See
;;  (@> "Sets of Completion Candidates") and
;;  (@> "Progressive Completion").
 
;;(@* "File-Name Input and Locating Files Anywhere")
;;
;;  File-Name Input and Locating Files Anywhere
;;  -------------------------------------------
;;
;;  Most Icicles commands that target file or directory names make use
;;  of function `read-file-name' to read the file name with
;;  completion.  This includes all Icicles commands defined with
;;  `icicle-define-file-command'.  An example is multi-command
;;  `icicle-find-file', which visits one or more files.
;;
;;  When `read-file-name' is used to read input, only the file name
;;  itself, not the directory portion, is used for matching.  The
;;  behavior is thus the same whether or not the directory is present
;;  in the minibuffer.  If you prefer, you can delete the directory
;;  first, using `M-k' (the `default-directory' is used, by default).
;;
;;  This means, in particular, that you can use apropos completion to
;;  match a substring, without needing to prefix the substring with
;;  `.*' in the minibuffer.  For example, to match the file named
;;  `favorite-foo-file.bar' in directory `/some/path/to/my/', you need
;;  not use `/some/path/to/my/.*foo'; it is sufficient to use either
;;  `foo' or `/some/path/to/my/foo'.
;;
;;  It is possible, however, for a command to target file names but
;;  make use of `completing-read' instead of `read-file-name'.  In
;;  that case, there is, a priori, no notion of `default-directory';
;;  the completion candidates are treated simply as strings.  Since
;;  `completing-read' has no notion of file names, it is also the case
;;  that file-name wildcards such as `*' are not taken into account by
;;  it.  Unless the command that calls `completing-read' does
;;  something special to interpret such wildcards, you cannot use them
;;  in input strings.  (You can of course use `*' in regexps - see
;;  (@> "What About Special-Character Conflicts?") for the
;;  distinction.)
;;
;;  Examples of Icicles multi-commands that read a file name using
;;  `completing-read', not `read-file-name', are `icicle-locate-file',
;;  `icicle-locate-file-other-window', `icicle-recent-file', and
;;  `icicle-recent-file-other-window'.  The advantage of using
;;  `completing-read' is the flip side of the main disadvantage: There
;;  is no notion of `default-directory'.  This means that these
;;  commands let you regexp-match against any part of the absolute
;;  file name, including directory components.  This makes sense for
;;  these commands, because the set of completion candidates can
;;  include absolute file names from many different directories.
;;
;;  Commands `icicle-recent-file' and
;;  `icicle-recent-file-other-window' let you open any file that you
;;  have visited recently, perhaps in a previous Emacs session.
;;  Commands `icicle-locate-file' and
;;  `icicle-locate-file-other-window' can be used to find a file when
;;  you do not know what directory it is in.  They look throughout a
;;  given directory, including throughout all of its subdirectories.
;;
;;  By default, the target directory is the current directory, but if
;;  you supply a prefix argument then you are prompted for the
;;  directory to search.  If you use the root of your file system as
;;  the search directory, then the locate-file commands will match
;;  completion candidates anywhere in your file system.
;;
;;  This can be quite useful.  It gives you much of the power of the
;;  Unix `find' command just for completing input!  And with
;;  incremental completion (see (@> "Icompletion")), you can see what
;;  matches your input as you type.
;;
;;  Obviously, if you use your entire file system as the set of
;;  completion candidates then gathering and matching such a large set
;;  of file names can take some time.  On my hard drive, for instance,
;;  there are 36 GB full of files, and it takes about 40 seconds to
;;  gather all of the file names.  In spite of this inconvenience,
;;  this functionality can be useful.  And of course searching a
;;  shallower directory tree presents less of a performance penalty -
;;  you pay for what you get.
;;
;;  There is a way, however, of having your cake and eating it too.
;;  You can gather all of the file names in your file system once, and
;;  save that list of completion candidates to a cache file on disk,
;;  as a snapshot.
;;  See (@> "Persistent Sets of Completion Candidates"), for more on
;;  this.
;;
;;  See Also: (@> "Dealing With Large Candidate Sets").
 
;;(@* "Persistent Sets of Completion Candidates")
;;
;;  Persistent Sets of Completion Candidates
;;  ----------------------------------------
;;
;;  Section (@> "Sets of Completion Candidates") describes how you can
;;  save the current set of completion candidates and reuse it later.
;;  This is not a persistent save, however; the candidates are simply
;;  saved in variable `icicle-saved-completion-candidates' for the
;;  duration of your Emacs session (or until you save candidates
;;  again).
;;
;;  You can save the current set of completions (whatever it is)
;;  persistently by supplying a plain prefix argument (`C-u') when you
;;  use `C-M->' (`icicle-candidate-set-save').  Alternatively, you can
;;  use `C-}', bound to `icicle-candidate-set-save-to-cache-file',
;;  which does the same thing.  To retrieve completion candidates that
;;  were previously saved to a cache file, so that they become the
;;  current set of candidates, use either `C-u C-M-<' or `C-{'
;;  (`icicle-candidate-set-retrieve' or
;;  `icicle-candidate-set-retrieve-from-cache-file').
;;
;;  Note that using a numeric prefix argument (`C-u' with a number)
;;  with `C-M->' and `C-M-<' saves or retrieves a
;;  completion-candidates set using a variable that you name, not a
;;  cache file.  See (@> "Sets of Completion Candidates").
;;
;;  If you have used the Emacs file-name cache (see the Emacs manual,
;;  node "File Name Cache"), then you have already used a cache file
;;  of (file-name) completion candidates.  In vanilla Emacs, you use
;;  `C-TAB' during file-name input to complete to a cached file name.
;;  In Icicles, you use `C-{'.
;;
;;  In Icicles, the cached candidates are not limited to file names,
;;  and you can have any number of cache files, to save different sets
;;  of completion candidates.  Each cache file saves the set of
;;  candidates that was current when you created (saved) the set.
;;
;;  The fact that a cache file can contain just those candidates that
;;  were current when you saved it is a considerable advantage, when
;;  combined with Icicles features for sculpting the current set of
;;  matching candidates.  As far as I know, Icicles is the only
;;  package to offer this feature.  You spend a few moments to
;;  fine-tune a set of candidates, using, for example, `M-*', `C-~',
;;  and `delete', and then save it for later use.  From then on, you
;;  can match against exactly those candidates anytime you want.
;;
;;  For example, you might have a software project that involves only
;;  certain directories and perhaps only certain kinds of files in
;;  those directories are of interest as completion candidates.  Those
;;  directories and files can even be in disparate locations.  Start
;;  with command `icicle-locate-file'.  Then use progressive
;;  completion to match the directories and files you want and chip
;;  away at those you don't want.  Once you get just the set you need
;;  for your project, save that set using `C-}'.  You can have any
;;  number of saved sets, for different projects or different purposes
;;  in the same project.
;;
;;  You name the sets of saved candidates, and these names are
;;  associated with the cache files in user option
;;  `icicle-saved-completion-sets'.  This is an alist of entries, each
;;  of which is of the form (SET-NAME . CACHE-FILE-NAME).  You can
;;  customize this option, or set it in your init file (~/.emacs).
;;
;;  You can use command `icicle-add/update-saved-completion-set' to
;;  add a new set to `icicle-saved-completion-sets' or update
;;  (replace) an existing such set.  You can use command
;;  `icicle-remove-saved-completion-set' to remove a saved set.
;;
;;  As an alternative to customizing `icicle-saved-completion-sets' or
;;  using command `icicle-add/update-saved-completion-set', you can
;;  simply try to save a set of completion candidates persistently,
;;  using `C-u C-M->' or `C-}'.  You are then prompted for the names
;;  of the candidate set and cache file to use, and the names you
;;  enter are automatically entered in option
;;  `icicle-saved-completion-sets'.  That option is automatically
;;  saved to your custom file, so the next time you use Emacs you can
;;  retrieve any saved set of candidates that you like.
;;
;;  When you try to retrieve a persistent set of completion
;;  candidates, you are similarly prompted for the candidate-set name
;;  and the cache-file name.
;;
;;  In addition to saving the current set of completion candidates to
;;  a cache file, you can add individual strings as future completion
;;  candidates to any cache file, and you can remove candidates from a
;;  cache file individually.  You do this using commands
;;  `icicle-add-candidate-to-saved-completion-set' and
;;  `icicle-remove-candidate-from-saved-completion-set'.
;;
;;  Adding an individual candidate is similar to using the Emacs
;;  file-name cache commands that add file names to the cache, but it
;;  adds only a single candidate.  For file names, adding a directory
;;  name effectively provides completion for all of its files as well,
;;  so there is no need to add each file name as well as the directory
;;  name.  Alternatively, you can always use `C-}' to add all file
;;  names that match your current input.
;;
;;  Section (@> "File-Name Input and Locating Files Anywhere") tells
;;  you how you can locate any file in your file system.  If you save
;;  the set of all file names persistently, you will increase the
;;  performance of using it - it is much faster to retrieve the list
;;  of all file names than it is to generate it.
;;
;;  With 36 GB of files in my file system, my all-file-system cache
;;  file is 20 MB, and retrieving the file-name completions from it
;;  takes only a few seconds.  With this feature, Icicles essentially
;;  gives you the functionality of the Unix `locate' command, but with
;;  the addition of real-time regexp matching.  Here is all you do:
;;
;;    M-x icicle-locate-file RET
;;    C-#        ; Once or twice: turn off incremental completion.
;;    C-{        ; Retrieve all file names from your cache file.
;;               ; You are prompted for the set name and file name.
;;    foo.*bar   ; Regexp to match names with `foo' followed by `bar'.
;;    S-TAB      ; Update *Completions* display (because of `C-#').
;;
;;  Of course, once you have retrieved a set of candidates from your
;;  cache file, you can access them again without re-reading the file.
;;  When they are retrieved from your cache they are saved in variable
;;  `icicle-saved-completion-candidates', so the next time you want to
;;  use them, just retrieve them from this variable with `C-M-<'.
;;
;;  See Also:
;;
;;  * (@> "File-Name Input and Locating Files Anywhere") for information on
;;    finding files located anywhere in your file system
;;
;;  * (@> "Icompletion") for information on `C-#' (toggle incremental
;;    completion)
;;
;;  * (@> "Sets of Completion Candidates") for information on `C-M->' (save
;;    current candidates)
;;
;;  * (@> "Dealing With Large Candidate Sets")
 
;;(@* "Dealing With Large Candidate Sets")
;;
;;  Dealing With Large Candidate Sets
;;  ---------------------------------
;;
;;  One of the advantages Icicles provides is the ability to deal with
;;  large sets of completion candidates with ease.  There are other
;;  libraries that also let you cycle among various choices of
;;  different kinds (buffers, files, and so on), but cycling quickly
;;  loses its effectiveness as the number of candidates increases.
;;
;;  Icicles apropos matching lets you work with a large initial set of
;;  candidates by filtering them, quickly reducing the number
;;  candidates to cycle through.  Filtering by a prefix only (vanilla
;;  Emacs) is not very potent.  Until you get used to Icicles, you
;;  will be surprised at your ability to manipulate even humongous
;;  sets of choices.
;;
;;  Nevertheless, there can be times when a candidate set is so large
;;  that you need to use a few tricks to deal with it efficiently.
;;  There are two main things that take time when dealing with a large
;;  set: computing the set and displaying it (with highlighting) in
;;  buffer *Completions*.  In particular, incremental completion
;;  display is costly because it does both of these, recompute the set
;;  and redisplay it, each time you type or delete a character in the
;;  minibuffer.
;;
;;  Here are some tips to improve performance with a large set of
;;  candidates:
;;
;;  * Turn off incremental completion display in buffer *Completions*.
;;    You can do this on the fly at any time by using `C-#' in the
;;    minibuffer - use `C-#' again to turn it back on.  See
;;    (@> "Icompletion").
;;
;;  * Compute a large candidate set only once, cache the result, and
;;    reuse it later by reading the cache instead of recomputing.
;;    This is useful, for instance, for the candidate set of all files
;;    on your file system.  You can cache a set of candidates in
;;    either a variable (quickest, but not persistent) or a disk file
;;    (slower, persistent).
;;    See (@> "Persistent Sets of Completion Candidates").
;;
;;  * Compute a large candidate set (and perhaps cache it or filter
;;    it) without displaying it in *Completions*, by using `C-M-TAB'
;;    or `C-M-S-TAB' instead of `TAB' or `S-TAB', respectively.  These
;;    are bound to commands `icicle-prefix-complete-no-display' and
;;    `icicle-apropos-complete-no-display'.  For example, when
;;    initially computing the set of all files on your file system for
;;    `M-x C-u icicle-locate-file', use `C-M-S-TAB' to compute the
;;    set, then use `C-}' to save it to a cache file - you need never
;;    display it.
 
;;(@* "History Enhancements")
;;
;;  History Enhancements
;;  --------------------
;;
;;  This section is about accessing and reusing previous input that
;;  you have typed in the minibuffer.
;;
;;(@* "What Input, What History?")
;;  ** What Input, What History? **
;;
;;
;;  First, what is meant by "input" and "input history"?  In vanilla
;;  Emacs and in this doc, "minibuffer history" and "input history"
;;  generally refer to input that you have typed (or cycled or
;;  completed) in the minibuffer and then entered using `RET' (or
;;  `S-RET').  Emacs provides different history lists for this,
;;  depending on the kind of input.  The most general such list is the
;;  value of variable `minibuffer-history'.
;;
;;  But what about input that you type in the minibuffer (e.g. during
;;  completion) but you do not enter with `RET'?  That is not recorded
;;  in any standard history list, so you cannot recall it using `M-p'
;;  and `M-n'.
;;
;;  The Icicles doc speaks ambiguously of "minibuffer input".  This
;;  always refers to something that you type in the minibuffer, but
;;  sometimes it means input that you enter with `RET' and sometimes
;;  it does not.  The context and the use of phrases such as "entered"
;;  and "entered with `RET'" should make clear what is meant.  Input
;;  that you type during completion but that you do not necessarily
;;  enter is sometimes referred to in the Icicles doc as "completion
;;  input".
;;
;;  Because completion is so important to Icicles, because cycling
;;  replaces the input you type in the minibuffer, and because you
;;  sometimes need to retrieve such typed input that was never
;;  entered, Icicles also records this input.  You can retrieve it
;;  during completion using `C-l' (`icicle-retrieve-previous-input')
;;  and `C-S-l', that is, `C-L', (`icicle-retrieve-next-input').  Use
;;  these commands to cycle among your past completion inputs
;;  (backward and forward, respectively).
;;
;;  User option `icicle-completion-history-max-length' limits the
;;  number of completion inputs to save.
;;
;;  If you customize user option `icicle-C-l-uses-completion-flag' to
;;  non-nil, then, instead of cycling, `C-l' lets you use Icicles
;;  completion to retrieve a past completion input (`C-L' does the
;;  same thing).  Using completion to retrieve a past input does not
;;  also choose that input as the candidate for the main completion;
;;  it just replaces your current minibuffer input with it.  Because
;;  `C-l' completion uses a recursive minibuffer, you can also use
;;  `C-g' to cancel this completion and return to the main completion.
;;
;;  You can temporarily reverse the effect of
;;  `icicle-C-l-uses-completion-flag' by using a prefix argument
;;  (`C-u') with `C-l'.  Thus, `C-u C-l' uses completion if
;;  `icicle-C-l-uses-completion-flag' is nil and cycles if it is
;;  non-nil.
;;
;;  The other sections here describe Icicles enhancements for
;;  minibuffer histories.  They are thus concerned only with inputs
;;  that you enter, not with completion inputs that are not entered.
;;
;;(@* "Overview of Minibuffer History Enhancements")
;;  ** Overview of Minibuffer History Enhancements **
;;
;;  Icicles enhances the minibuffer history in five independent ways:
;;
;;  1. Command `icicle-insert-history-element' (bound to `M-o' in the
;;     minibuffer) lets you use (lax) completion to insert a history
;;     element in the minibuffer.
;;
;;  2. Candidates displayed in *Completions* are highlighted using
;;     face `icicle-historical-candidate' (blue foreground, by
;;     default), when they have been used previously, so you can more
;;     easily recognize them.  This highlighting is controlled by
;;     option `icicle-highlight-historical-candidates-flag'.  You can
;;     toggle this from the minibuffer at any time using `C-pause'.
;;
;;  3. Command `icicle-toggle-alternative-sorting', (`C-M-,' in the
;;     minibuffer) re-sorts completion candidates, placing previously
;;     used candidates first.  This is a toggle: repeat it to return
;;     to the original order.
;;
;;  4. Command `icicle-keep-only-past-inputs' (`M-pause' in the
;;     minibuffer) restricts the current set of completion candidates
;;     to those that you have used previously.  In other words, it
;;     keeps only those candidates that are highlighted in blue.  To
;;     use `M-pause', you must first have used `TAB' or `S-TAB' to
;;     establish an explicit candidate set.  If you use `C-u M-pause',
;;     then the previously used candidates are ordered
;;     chronologically, most recent first.  Without `C-u', the normal
;;     sort order is used (`icicle-sort-function').
;;
;;  5. Command `icicle-history' (`M-h' in the minibuffer) matches the
;;     current input against the minibuffer history directly.  It can
;;     be used during completion.
;;
;;  These enhancements are described below in more detail.  Each of
;;  them lets you see the complete list of previous inputs that match
;;  your current input.  In vanilla Emacs, the history lists are never
;;  shown as such; you can access previous inputs only one at a time,
;;  in order (with `M-p').  In vanilla Emacs, you can use a regexp to
;;  search the history list (via `M-r'), but the regexp matching is
;;  not dynamic, and the first match found is the (only) one you get.
;;
;;  Displaying previous inputs that match the current input sounds
;;  like a minor advantage, but it is actually quite helpful in
;;  practice.  Among other things, it means that you can work with
;;  long history lists in a practical way.
;;
;;(@* "Using Completion to Insert Previous Inputs: `M-o'")
;;  ** Using Completion to Insert Previous Inputs: `M-o' **
;;
;;  Unlike the other minibuffer history enhancements, described below,
;;  which are available only during minibuffer completion, you can use
;;  `M-o' (`icicle-insert-history-element') anytime you are asked for
;;  minibuffer input.  It provides a recursive minibuffer in which you
;;  can match a previous input using completion.  After you hit `RET'
;;  to accept your choice, it is inserted in the minibuffer just as if
;;  you had typed it.  This has the advantage over cycling with `M-n'
;;  or `M-p' and searching with `M-s' or `M-r', that you can use
;;  Icicles completion and cycling to quickly access a previous input,
;;  no matter how long ago you entered it.
;;
;;  When completion is available for reading input, if you use `M-o'
;;  to choose a previously entered input, this just inserts that input
;;  in the minibuffer.  What is in the minibuffer after you use `M-o'
;;  is not automatically chosen for the main completion - you can edit
;;  the minibuffer contents before entering it with `RET'.  You can
;;  also use `C-g' during the `M-o' completion to cancel it and return
;;  to the main completion.
;;
;;(@* "Putting Previous Candidates First: `C-M-,'")
;;  ** Putting Previous Candidates First: `C-M-,' **
;;
;;  At any time, Icicles has two different sort orders that you can
;;  use (and customize).  These are the values of user options
;;  `icicle-sort-function' and `icicle-alternative-sort-function'.  By
;;  default, the former sorts alphabetically, and the latter puts all
;;  previously used inputs first, before the candidates you have not
;;  yet used.  Each of these groups, used and unused candidates, is
;;  then sorted alphabetically, separately.  So, with the default
;;  alternative sort, you can see all matching candidates (used and
;;  unused), but you privilege those used previously - they are the
;;  first listed in *Completions* and the first available for cycling.
;;
;;  If you prefer, by customizing these user options, you can use
;;  `icicle-historical-alphabetic-p' as the main sort function (option
;;  `icicle-sort-function') and some other sort function
;;  (e.g. `icicle-case-string-less-p') as the alternative sort
;;  function.
;;
;;  You can toggle at any time between normal sorting and alternative
;;  sorting, using command `icicle-toggle-alternative-sorting'.
;;  During completion, this is bound to `C-M-,'.  Together with
;;  toggling between normal sorting and not sorting at all, which is a
;;  sort-order choice available through `C-,', this gives you quite a
;;  lot of flexibility.  Some commands, such as `icicle-complete-keys'
;;  (bound to `S-TAB' except during completion), use different sort
;;  orders.
;;
;;(@* "Matching Only Historical Candidates: `M-h' and `M-pause'")
;;  ** Matching Only Historical Candidates: `M-h' and `M-pause' **
;;
;;  Both `M-h' and `M-pause' can be used toward the same end.  They
;;  both work for all input types.  They both use the appropriate
;;  history list for the current command.  They both provide apropos
;;  completion and cycling for the minibuffer history (as well as
;;  prefix completion, of course).  Use them as another way to search
;;  through a history list or complete to one of its elements.
;;
;;  For example, If you use `C-x C-f' to find a file, and then use
;;  `M-h' or `M-pause', the completion candidates will be the names of
;;  files that you have previously accessed (file names you have input
;;  in the minibuffer), and which match the current minibuffer input.
;;
;;  `M-h' lets you complete your input against the minibuffer input
;;  history.  `M-pause' lets you restrict the current explicit set of
;;  completion candidates to those that are also in the minibuffer
;;  history.
;;
;;  They provide similar functionality in different ways.  The
;;  difference is that `M-pause' takes the current set of matching
;;  candidates into account.  It is a completion-candidates set
;;  operation, similar to those described in section
;;  (@> "Sets of Completion Candidates").
;;
;;  This means, in particular, that with `M-pause' you can first
;;  perform set operations on the set of candidates, and then use that
;;  result to restrict the history search.  For example, you can first
;;  complement the candidate set using `C-~', then use `M-pause' to
;;  restrict those candidates to matches in the history list.  In this
;;  way, you avoid including matches from the original match set when
;;  searching the history.
;;
;;  Example: You are in a directory with lots of files that have the
;;  prefix `foo' and lots of C-language source files.  You happen to
;;  be interested in another file, however.  One way to get to that
;;  file is to use Dired's ability to mark files by matching a regexp
;;  and then use Dired's ability to omit the marked files from view.
;;  You can scan through those that remain, and pick the one you want.
;;  However, it turns out that even then there are many files to scan.
;;  You accessed the one you want now just the other day, but the file
;;  date is unfortunately not significant.
;;
;;  In Icicles, you use regexp matching and take the set complement of
;;  the hits, just like in Dired: `C-x C-f foo.*\.c$' defines the
;;  candidate set as all files whose names start with `foo' and have
;;  extension `c'.  `C-~' then defines the candidate set as all files
;;  whose names are not like that.  Finally, you use `M-pause' to
;;  restrict the file-name candidates to names that you have used
;;  before.  You've accessed many, many files recently, so just
;;  cycling through the history with `M-p' would be tedious.  You
;;  could match a regexp against the file history, but how can you
;;  come up with a regexp that finds anti-matches?
;;
;;  A consequence of this difference between `M-h' and `M-pause' is
;;  that using `TAB' or `S-TAB' after `M-pause' abandons use of the
;;  minibuffer history and starts a new set of completion candidates.
;;  It simply completes the current input in the context of the
;;  current command; `TAB' and `S-TAB' have nothing to do with the
;;  minibuffer history in this case.  Using `TAB' or `S-TAB' after
;;  `M-h', however, re-completes your input against the current
;;  history list.
;;
;;  Another consequence is that you can use `down' or `C-down' on the
;;  candidates displayed by `M-h', but not on those displayed by
;;  `M-pause'.  For example, to cycle through the doc for each
;;  variable that starts with `icicle-' which you have previously
;;  input, you can use `C-h v icicle- M-h', then repeatedly use
;;  `C-down'.
;;
;;  Also, file-name and directory-name completion works differently in
;;  these two commands.  By default, the current directory is (as
;;  always) inserted into the minibuffer by commands such as
;;  `find-file', so either `M-h' or `M-pause' after `C-x C-f' will
;;  match previously input file names from the current directory.
;;
;;  However, in the case of `M-h', the entire minibuffer input is
;;  matched against the history list, which is a list of absolute file
;;  names.  `M-pause' works only with the current candidate set,
;;  which, if you have already used `TAB' or `S-TAB' in the current
;;  directory, is a set of relative file names in that directory.
;;
;;  This difference has a consequence for apropos (regexp) completion
;;  with `M-h'.  It means that to match a file name using a substring
;;  you must, in the minibuffer, either not specify a directory (erase
;;  it) or explicitly use `.*' before the file-name substring.
;;
;;  For example, with `M-h', `/foo/bar/lph' will not apropos-match the
;;  previously input file name `/foo/bar/alphabet-soup.el'; you should
;;  use either `/foo/bar/.*lph' or `lph' (no directory).
;;
;;  In the case of `M-pause', however, the input is matched against
;;  the history list as restricted by the existing completion list.
;;  And, since apropos file-name completion uses only the relative
;;  file name, without the directory name, as a regexp, the candidate
;;  list that is restricted has already matched the input regexp.  The
;;  action of `M-pause' is simply to filter the list of candidates,
;;  keeping those that are in the history list.  This means that, with
;;  `M-pause', the input `/foo/bar/lph' will match against the
;;  previously input file name `/foo/bar/alphabet-soup.el'.
;;
;;  If this all sounds confusing, just give it a try; it is much
;;  harder to describe than it is to experience.
;;
;;  A final tip: You can clear (reset) the minibuffer history that is
;;  used for the current command, by using `M-:'
;;  (`icicle-pp-eval-expression') to evaluate the following Emacs-Lisp
;;  sexp during completion (note: `set', not `setq'):
;;
;;    (set minibuffer-history-variable nil)
 
;;(@* "Isearch Completion")
;;
;;  Isearch Completion Against the Search History
;;  ---------------------------------------------
;;
;;  Icicles provides two unrelated enhancements for searching:
;;
;;  - An extension to standard Emacs incremental search that lets you
;;    use Icicles completion against previous search strings.  This is
;;    described in this section.
;;
;;  - Top-level Icicles commands that provide an entirely new and
;;    different way for you to search.  This is described in section
;;    (@> "Icicles Search Commands, Overview").
;;
;;  When you search incrementally (`C-s'), Emacs (21 or later) lets
;;  you complete your input to a string that you have looked for
;;  previously.  In Icicle mode, this feature is enhanced so that you
;;  can use all of the completion behavior provided by Icicles.
;;
;;  In vanilla Emacs, you use `M-TAB' to complete against the search
;;  ring (that is, the search history).  In Icicles, you use `S-TAB'
;;  (`icicle-isearch-complete') to do this - that's what Icicles users
;;  are in the habit of using for (apropos) completion.  They are also
;;  in the habit of using `TAB' for prefix completion, but in Isearch
;;  `TAB' inserts a tab, which is a useful character to include in
;;  search strings.
;;
;;  When you use `S-TAB' while searching, Isearch exits momentarily,
;;  giving way to Icicles completion in the minibuffer (Isearch
;;  actually uses the echo area, not the minibuffer).  You can then
;;  use either `S-TAB' or `TAB' to complete your search string.  After
;;  you finish completing (e.g. by hitting `RET'), Isearch resumes
;;  with the new, completed search string.  It's pretty seamless, and
;;  easier to try than to describe.
;;
;;  One reminder: Using `S-TAB' vs `TAB' for (regexp vs non-regexp)
;;  completion against previous search strings has nothing to do with
;;  regexp vs non-regexp searching.  You can of course use either kind
;;  of searching before or after having used either kind of
;;  completion.  Isearch uses different search rings for regexp and
;;  non-regexp searching.  The kind of search in progress (regexp or
;;  not) at the moment you call for completion determines which search
;;  ring provides the candidates for completion.
 
;;  The Icicles doc is continued in file `icicles-doc2.el'.
 
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

;; You need not load this file.  It contains only documentation.

(provide 'icicles-doc1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-doc1.el ends here
