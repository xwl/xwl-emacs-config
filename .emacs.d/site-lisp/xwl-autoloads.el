
;;;### (autoloads (xwl-makefile-clean xwl-makefile-byte-compile xwl-makefile-all)
;;;;;;  "xwl-makefile" "xwl-makefile.el" (19387 23874))
;;; Generated autoloads from xwl-makefile.el

(autoload 'xwl-makefile-all "xwl-makefile" "\
Not documented

\(fn)" nil nil)

(autoload 'xwl-makefile-byte-compile "xwl-makefile" "\
Not documented

\(fn)" t nil)

(autoload 'xwl-makefile-clean "xwl-makefile" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (sawfish-interaction sawfish-console sawfish-rep-info
;;;;;;  sawfish-info sawfish-complete-symbol sawfish-apropos sawfish-info-variable
;;;;;;  sawfish-info-function sawfish-describe-variable sawfish-describe-function
;;;;;;  sawfish-eval-print-last-sexp sawfish-eval-last-sexp sawfish-eval-expression
;;;;;;  sawfish-eval-defun sawfish-eval-buffer sawfish-eval-region
;;;;;;  sawfish-mode) "sawfish" "sawfish.el" (18862 33836))
;;; Generated autoloads from sawfish.el

(autoload 'sawfish-mode "sawfish" "\
Major mode for editing sawfish files and for interacting with sawfish.

Special commands:

\\{sawfish-mode-map}

\(fn)" t nil)

(autoload 'sawfish-eval-region "sawfish" "\
Evaluate the region bounded by START and END.

TARGET-BUFFER is the optional target for the return value of the
evaluation.

\(fn START END &optional TARGET-BUFFER)" t nil)

(autoload 'sawfish-eval-buffer "sawfish" "\
Evaluate the whole buffer.

\(fn)" t nil)

(autoload 'sawfish-eval-defun "sawfish" "\
Evaluate the top level form at or near `point'.

INSERT-VALUE is a prefix parameter, if it is non-NIL the value of the
expression is inserted into the buffer after the form.

\(fn INSERT-VALUE)" t nil)

(autoload 'sawfish-eval-expression "sawfish" "\
Evaluate SEXP and display the value in the minibuffer.

If the optional parameter INSERT-VALUE is supplied as a non-NIL value the
value of SEXP will be inserted into the current buffer.

\(fn SEXP &optional INSERT-VALUE)" t nil)

(autoload 'sawfish-eval-last-sexp "sawfish" "\
Version of `eval-last-sexp' that interacts with sawfish.

\(fn TO-BUFFER)" t nil)

(autoload 'sawfish-eval-print-last-sexp "sawfish" "\
Not documented

\(fn)" t nil)

(autoload 'sawfish-describe-function "sawfish" "\
Display the doc-string for FUNCTION.

\(fn FUNCTION)" t nil)

(autoload 'sawfish-describe-variable "sawfish" "\
Display the doc-string for VARIABLE.

\(fn VARIABLE)" t nil)

(autoload 'sawfish-info-function "sawfish" "\
Display the Info documentation for FUNCTION.

\(fn FUNCTION)" t nil)

(autoload 'sawfish-info-variable "sawfish" "\
Display the Info documentation for VARIABLE.

\(fn VARIABLE)" t nil)

(autoload 'sawfish-apropos "sawfish" "\
Show all bound sawfish symbols whose names match REGEXP.

\(fn REGEXP)" t nil)

(autoload 'sawfish-complete-symbol "sawfish" "\
Attempt to complete the symbol at `point'.

\(fn)" t nil)

(autoload 'sawfish-info "sawfish" "\
View the sawfish info file.

\(fn)" t nil)

(autoload 'sawfish-rep-info "sawfish" "\
View the librep info file.

\(fn)" t nil)

(autoload 'sawfish-console "sawfish" "\
Run the sawfish client as an inferior lisp.

\(fn)" t nil)

(autoload 'sawfish-interaction "sawfish" "\
Create a sawfish interaction buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (pack-windows) "pack-windows" "pack-windows.el"
;;;;;;  (18862 33836))
;;; Generated autoloads from pack-windows.el

(autoload 'pack-windows "pack-windows" "\
Resize all windows vertically to display as much information as possible.

Only windows that are on the left edge of the frame are taken into
account. The vertical space available in the frame is first divided
among all these windows. Then any window requireing less lines than it
got to display its whole buffer is shrinked, and the freed space is
divided equally among all the other windows.

If some vertical space remains afterwards, it is given in totality to
the currently selected window.

Do not shrink any window to less than `window-min-height'.

Shrink windows iteratively, performing at most `pack-windows-max-iteration'
iterations. The number of iterations really performed will be
displayed in the echo area if `pack-windows-verbose' is non-nil.

\(fn)" t nil)

;;;***

;;;### (autoloads (oddmuse-kill-url oddmuse-browse-this-page oddmuse-browse-page
;;;;;;  emacswiki-post oddmuse-insert-pagename oddmuse-revert oddmuse-post
;;;;;;  oddmuse-follow oddmuse-edit oddmuse-toggle-minor) "oddmuse"
;;;;;;  "oddmuse.el" (18862 33836))
;;; Generated autoloads from oddmuse.el

(autoload 'oddmuse-toggle-minor "oddmuse" "\
Toggle minor mode state.

\(fn &optional ARG)" t nil)

(autoload 'oddmuse-edit "oddmuse" "\
Edit a page on a wiki.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a prefix argument to force a reload of the page.

\(fn WIKI PAGENAME)" t nil)

(autoload 'oddmuse-follow "oddmuse" "\
Figure out what page we need to visit
and call `oddmuse-edit' on it.

\(fn ARG)" t nil)

(autoload 'oddmuse-post "oddmuse" "\
Post the current buffer to the current wiki.
The current wiki is taken from `oddmuse-wiki'.

\(fn SUMMARY)" t nil)

(autoload 'oddmuse-revert "oddmuse" "\
Revert this oddmuse page.

\(fn)" t nil)

(autoload 'oddmuse-insert-pagename "oddmuse" "\
Insert a PAGENAME of current wiki with completion.

\(fn PAGENAME)" t nil)

(autoload 'emacswiki-post "oddmuse" "\
Post the current buffer to the EmacsWiki.
If this command is invoked interactively: with prefix argument, prompts pagename,
otherwise set pagename as basename of `buffer-file-name'.

This command is intended to post current EmacsLisp program easily.

\(fn &optional PAGENAME SUMMARY)" t nil)

(autoload 'oddmuse-browse-page "oddmuse" "\
Ask a WWW browser to load an oddmuse page.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to browse.

\(fn WIKI PAGENAME)" t nil)

(autoload 'oddmuse-browse-this-page "oddmuse" "\
Ask a WWW browser to load current oddmuse page.

\(fn)" t nil)

(autoload 'oddmuse-kill-url "oddmuse" "\
Make the URL of current oddmuse page the latest kill in the kill ring.

\(fn)" t nil)

;;;***

;;;### (autoloads (nuke-trailing-whitespace) "nuke-trailing-whitespace"
;;;;;;  "nuke-trailing-whitespace.el" (18862 33836))
;;; Generated autoloads from nuke-trailing-whitespace.el

(autoload 'nuke-trailing-whitespace "nuke-trailing-whitespace" "\
Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on `write-file-hooks'.

Unless called interactively, this function uses
`nuke-trailing-whitespace-p' to determine how to behave.
However, even if this variable is t, this function will query for
replacement if the buffer is read-only.

\(fn)" t nil)

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize" "htmlize.el" (19245
;;;;;;  65113))
;;; Generated autoloads from htmlize.el

(autoload 'htmlize-buffer "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (graphviz-dot-mode) "graphviz-dot-mode" "graphviz-dot-mode.el"
;;;;;;  (18862 33836))
;;; Generated autoloads from graphviz-dot-mode.el

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "\
Major mode for the dot language. \\<graphviz-dot-mode-map>
TAB indents for graph lines.

\\[graphviz-dot-indent-graph]	- Indentaion function.
\\[graphviz-dot-preview]	- Previews graph in a buffer.
\\[graphviz-dot-view]	- Views graph in an external viewer.
\\[graphviz-dot-indent-line]	- Indents current line of code.
\\[graphviz-dot-complete-word]	- Completes the current word.
\\[electric-graphviz-dot-terminate-line]	- Electric newline.
\\[electric-graphviz-dot-open-brace]	- Electric open braces.
\\[electric-graphviz-dot-close-brace]	- Electric close braces.
\\[electric-graphviz-dot-semi]	- Electric semi colons.

Variables specific to this mode:

  graphviz-dot-dot-program            (default `dot')
       Location of the dot program.
  graphviz-dot-view-command           (default `doted %s')
       Command to run when `graphviz-dot-view' is executed.
  graphviz-dot-view-edit-command      (default nil)
       If the user should be asked to edit the view command.
  graphviz-dot-save-before-view       (default t)
       Automatically save current buffer berore `graphviz-dot-view'.
  graphviz-dot-preview-extension      (default `png')
       File type to use for `graphviz-dot-preview'.
  graphviz-dot-auto-indent-on-newline (default t)
       Whether to run `electric-graphviz-dot-terminate-line' when
       newline is entered.
  graphviz-dot-auto-indent-on-braces (default t)
       Whether to run `electric-graphviz-dot-open-brace' and
       `electric-graphviz-dot-close-brace' when braces are
       entered.
  graphviz-dot-auto-indent-on-semi (default t)
       Whether to run `electric-graphviz-dot-semi' when semi colon
       is typed.
  graphviz-dot-toggle-completions  (default nil)
       If completions should be displayed in the buffer instead of a
       completion buffer when \\[graphviz-dot-complete-word] is
       pressed repeatedly.

This mode can be customized by running \\[graphviz-dot-customize].

Turning on Graphviz Dot mode calls the value of the variable
`graphviz-dot-mode-hook' with no args, if that value is non-nil.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;;;***

;;;### (autoloads (goto-last-change) "goto-last-change" "goto-last-change.el"
;;;;;;  (18964 12702))
;;; Generated autoloads from goto-last-change.el

(autoload 'goto-last-change "goto-last-change" "\
Set point to the position of the last change.
Consecutive calls set point to the position of the previous change.
With a prefix arg (optional arg MARK-POINT non-nil), set mark so \\[exchange-point-and-mark]
will return point to the current position.

\(fn &optional MARK-POINT)" t nil)

;;;***

;;;### (autoloads (file-template-find-file-not-found-hook file-template-auto-insert
;;;;;;  file-template-insert) "file-template" "file-template.el"
;;;;;;  (18911 33708))
;;; Generated autoloads from file-template.el

(autoload 'file-template-insert "file-template" "\
Insert template into buffer, performing tag expansions.
See `file-template-tag-alist' for list of predefined tags.

Use this function when you don't want to insert the default template
associated with the file type in `file-template-mapping-alist'.
Otherwise, use `file-template-auto-insert'.

\(fn TEMPLATE)" t nil)

(autoload 'file-template-auto-insert "file-template" "\
Insert default template into buffer.

\(fn)" t nil)

(autoload 'file-template-find-file-not-found-hook "file-template" "\
Hook to (optionally) insert the default template when a new file is created.

\(fn)" nil nil)

;;;***

;;;### (autoloads (etags-select-find-tag etags-select-find-tag-at-point
;;;;;;  etags-select-go-if-unambiguous etags-select-use-short-name-completion
;;;;;;  etags-select-highlight-delay etags-select-highlight-tag-after-jump
;;;;;;  etags-select-mode-hook etags-select-no-select-for-one-match
;;;;;;  etags-select-mode) "etags-select" "etags-select.el" (19166
;;;;;;  65062))
;;; Generated autoloads from etags-select.el

(let ((loads (get 'etags-select-mode 'custom-loads))) (if (member '"etags-select" loads) nil (put 'etags-select-mode 'custom-loads (cons '"etags-select" loads))))

(defvar etags-select-no-select-for-one-match t "\
*If non-nil, don't open the selection window if there is only one
matching tag.")

(custom-autoload 'etags-select-no-select-for-one-match "etags-select" t)

(defvar etags-select-mode-hook nil "\
*List of functions to call on entry to etags-select-mode mode.")

(custom-autoload 'etags-select-mode-hook "etags-select" t)

(defvar etags-select-highlight-tag-after-jump t "\
*If non-nil, temporarily highlight the tag after you jump to it.")

(custom-autoload 'etags-select-highlight-tag-after-jump "etags-select" t)

(defvar etags-select-highlight-delay 1.0 "\
*How long to highlight the tag.")

(custom-autoload 'etags-select-highlight-delay "etags-select" t)

(defface etags-select-highlight-tag-face '((t (:foreground "white" :background "cadetblue4" :bold t))) "\
Font Lock mode face used to highlight tags." :group (quote etags-select-mode))

(defvar etags-select-use-short-name-completion nil "\
*Use short tag names during completion.  For example, say you
have a function named foobar in several classes and you invoke
`etags-select-find-tag'.  If this variable is nil, you would have
to type ClassA::foo<TAB> to start completion.  Since avoiding
knowing which class a function is in is the basic idea of this
package, if you set this to t you can just type foo<TAB>.

Only works with GNU Emacs.")

(custom-autoload 'etags-select-use-short-name-completion "etags-select" t)

(defvar etags-select-go-if-unambiguous nil "\
*If non-nil, jump by tag number if it is unambiguous.")

(custom-autoload 'etags-select-go-if-unambiguous "etags-select" t)

(autoload 'etags-select-find-tag-at-point "etags-select" "\
Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

(autoload 'etags-select-find-tag "etags-select" "\
Do a find-tag, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

;;;***

;;;### (autoloads (dos-mode) "dos" "dos.el" (19284 32504))
;;; Generated autoloads from dos.el

(autoload 'dos-mode "dos" "\
Major mode for editing Dos scripts.

The `dos-help-mode' command shows this page.

Start a new script from `dos-template' or `dos-template-mini'. Navigate between
sections using `dos-outline', `imenu', or `outline-minor-mode'. Use `dos-sep' to
save keystrokes. Read help for Dos command with `dos-help-cmd'. Run script using
`dos-run' and `dos-run-args'.

\\{dos-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (color-theme-select) "color-theme" "color-theme.el"
;;;;;;  (18862 33836))
;;; Generated autoloads from color-theme.el

(autoload 'color-theme-select "color-theme" "\
Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (boxquote-unbox boxquote-unbox-region boxquote-fill-paragraph
;;;;;;  boxquote-kill boxquote-narrow-to-boxquote-content boxquote-narrow-to-boxquote
;;;;;;  boxquote-text boxquote-where-is boxquote-shell-command boxquote-describe-key
;;;;;;  boxquote-describe-variable boxquote-describe-function boxquote-boxquote
;;;;;;  boxquote-paragraph boxquote-defun boxquote-yank boxquote-kill-ring-save
;;;;;;  boxquote-insert-file boxquote-buffer boxquote-region boxquote-title)
;;;;;;  "boxquote" "boxquote.el" (18919 23358))
;;; Generated autoloads from boxquote.el

(autoload 'boxquote-title "boxquote" "\
Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that the title will
be formatted using `boxquote-title-format'.

\(fn TITLE)" t nil)

(autoload 'boxquote-region "boxquote" "\
Draw a box around the left hand side of a region bounding START and END.

\(fn START END)" t nil)

(autoload 'boxquote-buffer "boxquote" "\
Apply `boxquote-region' to a whole buffer.

\(fn)" t nil)

(autoload 'boxquote-insert-file "boxquote" "\
Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a title that
is the result applying `boxquote-file-title-funciton' to FILENAME.

\(fn FILENAME)" t nil)

(autoload 'boxquote-kill-ring-save "boxquote" "\
Like `kill-ring-save' but remembers a title if possible.

The title is acquired by calling `boxquote-kill-ring-save-title'. The title
will be used by `boxquote-yank'.

\(fn)" t nil)

(autoload 'boxquote-yank "boxquote" "\
Do a `yank' and box it in with `boxquote-region'.

If the yanked entry was placed on the kill ring with
`boxquote-kill-ring-save' the resulting boxquote will be titled with
whatever `boxquote-kill-ring-save-title' returned at the time.

\(fn)" t nil)

(autoload 'boxquote-defun "boxquote" "\
Apply `boxquote-region' the current defun.

\(fn)" t nil)

(autoload 'boxquote-paragraph "boxquote" "\
Apply `boxquote-region' to the current paragraph.

\(fn)" t nil)

(autoload 'boxquote-boxquote "boxquote" "\
Apply `boxquote-region' to the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-describe-function "boxquote" "\
Call `describe-function' and boxquote the output into the current buffer.

\(fn)" t nil)

(autoload 'boxquote-describe-variable "boxquote" "\
Call `describe-variable' and boxquote the output into the current buffer.

\(fn)" t nil)

(autoload 'boxquote-describe-key "boxquote" "\
Call `describe-key' and boxquote the output into the current buffer.

If the call to this command is prefixed with \\[universal-argument] you will also be
prompted for a buffer. The key defintion used will be taken from that buffer.

\(fn KEY)" t nil)

(autoload 'boxquote-shell-command "boxquote" "\
Call `shell-command' with COMMAND and boxquote the output.

\(fn COMMAND)" t nil)

(autoload 'boxquote-where-is "boxquote" "\
Call `where-is' with DEFINITION and boxquote the result.

\(fn DEFINITION)" t nil)

(autoload 'boxquote-text "boxquote" "\
Insert TEXT, boxquoted.

\(fn TEXT)" t nil)

(autoload 'boxquote-narrow-to-boxquote "boxquote" "\
Narrow the buffer to the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-narrow-to-boxquote-content "boxquote" "\
Narrow the buffer to the content of the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-kill "boxquote" "\
Kill the boxquote and its contents.

\(fn)" t nil)

(autoload 'boxquote-fill-paragraph "boxquote" "\
Perform a `fill-paragraph' inside a boxquote.

\(fn ARG)" t nil)

(autoload 'boxquote-unbox-region "boxquote" "\
Remove a box created with `boxquote-region'.

\(fn START END)" t nil)

(autoload 'boxquote-unbox "boxquote" "\
Remove the boxquote that contains `point'.

\(fn)" t nil)

;;;***

;;;### (autoloads (debian-bug debian-bug-get-bug-as-email debian-bug-get-bug-as-file
;;;;;;  debian-bug-web-package debian-bug-web-packages debian-bug-web-this-bug-under-mouse
;;;;;;  debian-bug-web-bug debian-bug-web-developer-page debian-bug-web-bugs
;;;;;;  debian-bug-intent-to-package debian-bug-request-for-package
;;;;;;  debian-bug-wnpp) "debian-bug" "debian/debian-bug.el" (18862
;;;;;;  33836))
;;; Generated autoloads from debian/debian-bug.el

(autoload 'debian-bug-wnpp "debian-bug" "\
Submit a WNPP bug report to Debian.
Optional argument ACTION can be provided in programs.

\(fn &optional ACTION)" t nil)

(autoload 'debian-bug-request-for-package "debian-bug" "\
Shortcut for `debian-bug-wnpp' with RFP action.

\(fn)" t nil)

(autoload 'debian-bug-intent-to-package "debian-bug" "\
Shortcut for `debian-bug-wnpp' with ITP action (for Debian developers).

\(fn)" t nil)

(autoload 'debian-bug-web-bugs "debian-bug" "\
Browse the BTS for this package via `browse-url'.
With optional argument prefix ARCHIVED, display archived bugs.

\(fn &optional ARCHIVED)" t nil)

(autoload 'debian-bug-web-developer-page "debian-bug" "\
Browse the web for this package's developer page.

\(fn)" t nil)

(autoload 'debian-bug-web-bug "debian-bug" "\
Browse the BTS for BUG-NUMBER via `browse-url'.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug-web-this-bug-under-mouse "debian-bug" "\
Browse the BTS via `browse-url' for the bug report number under mouse.
In a program, mouse location is in EVENT.

\(fn EVENT)" t nil)

(autoload 'debian-bug-web-packages "debian-bug" "\
Search Debian web page for this package via `browse-url'.

\(fn)" t nil)

(autoload 'debian-bug-web-package "debian-bug" "\
Search Debian web page in ARCHIVE for this package via `browse-url'.

\(fn ARCHIVE)" t nil)

(autoload 'debian-bug-get-bug-as-file "debian-bug" "\
Read bug report #BUG-NUMBER as a regular file.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug-get-bug-as-email "debian-bug" "\
Read bug report #BUG-NUMBER via Email interface.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug "debian-bug" "\
Submit a Debian bug report.

\(fn)" t nil)

;;;***

;;;### (autoloads (deb-find deb-view-mode deb-view deb-view-dired-view)
;;;;;;  "deb-view" "debian/deb-view.el" (18862 33836))
;;; Generated autoloads from debian/deb-view.el

(autoload 'deb-view-dired-view "deb-view" "\
View Debian package control and data files.
Press \"q\" in either window to kill both buffers
and return to the dired buffer. See deb-view.

\(fn)" t nil)

(autoload 'deb-view "deb-view" "\
View Debian package DEBFILE's control and data files.
Press \"q\" in either window to kill both buffers.

In dired, press ^d on the dired line of the .deb file to view.
Or, execute: ESCAPE x deb-view RETURN, and enter the .deb file name
at the prompt.

\(fn DEBFILE)" t nil)

(autoload 'deb-view-mode "deb-view" "\
View mode for Debian Archive Files.

\(fn)" t nil)

(autoload 'deb-find "deb-view" "\
Search for deb files.
Use the method specified by the variable deb-find-method, and collect
output in a buffer.  See also the variable deb-find-directory.

This command uses a special history list, so you can
easily repeat a `deb-find' command.

\(fn)" t nil)

;;;***

;;;### (autoloads (apt-sources-mode) "apt-sources" "debian/apt-sources.el"
;;;;;;  (18862 33836))
;;; Generated autoloads from debian/apt-sources.el

(autoload 'apt-sources-mode "apt-sources" "\
Major mode for editing apt's sources.list file.
Sets up command `font-lock-mode'.

\\{apt-sources-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (dictionary-popup-matching-words dictionary-mouse-popup-matching-words
;;;;;;  dictionary-match-words dictionary-lookup-definition dictionary-search
;;;;;;  dictionary dictionary-mode) "dictionary" "dictionary-el/dictionary.el"
;;;;;;  (18862 33836))
;;; Generated autoloads from dictionary-el/dictionary.el

(autoload 'dictionary-mode "dictionary" "\
This is a mode for searching a dictionary server implementing
 the protocol defined in RFC 2229.

 This is a quick reference to this mode describing the default key bindings:

 * q close the dictionary buffer
 * h display this help information
 * s ask for a new word to search
 * d search the word at point
 * n or Tab place point to the next link
 * p or S-Tab place point to the prev link

 * m ask for a pattern and list all matching words.
 * D select the default dictionary
 * M select the default search strategy

 * Return or Button2 visit that link
 * M-Return or M-Button2 search the word beneath link in all dictionaries


\(fn)" nil nil)

(autoload 'dictionary "dictionary" "\
Create a new dictonary buffer and install dictionary-mode

\(fn)" t nil)

(autoload 'dictionary-search "dictionary" "\
Search the `word' in `dictionary' if given or in all if nil.
It presents the word at point as default input and allows editing it.

\(fn WORD &optional DICTIONARY)" t nil)

(autoload 'dictionary-lookup-definition "dictionary" "\
Unconditionally lookup the word at point.

\(fn)" t nil)

(autoload 'dictionary-match-words "dictionary" "\
Search `pattern' in current default dictionary using default strategy.

\(fn &optional PATTERN &rest IGNORED)" t nil)

(autoload 'dictionary-mouse-popup-matching-words "dictionary" "\
Display entries matching the word at the cursor

\(fn EVENT)" t nil)

(autoload 'dictionary-popup-matching-words "dictionary" "\
Display entries matching the word at the point

\(fn &optional WORD)" t nil)

;;;***

;;;### (autoloads (inferior-haskell-find-haddock inferior-haskell-find-definition
;;;;;;  inferior-haskell-info inferior-haskell-type inferior-haskell-load-file
;;;;;;  switch-to-haskell) "inf-haskell" "haskell-mode-2.4/inf-haskell.el"
;;;;;;  (18231 1814))
;;; Generated autoloads from haskell-mode-2.4/inf-haskell.el

(defalias 'run-haskell 'switch-to-haskell)

(autoload 'switch-to-haskell "inf-haskell" "\
Show the inferior-haskell buffer.  Start the process if needed.

\(fn &optional ARG)" t nil)

(autoload 'inferior-haskell-load-file "inf-haskell" "\
Pass the current buffer's file to the inferior haskell process.
If prefix arg \\[universal-argument] is given, just reload the previous file.

\(fn &optional RELOAD)" t nil)

(autoload 'inferior-haskell-type "inf-haskell" "\
Query the haskell process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `haskell-doc-mode'.

\(fn EXPR &optional INSERT-VALUE)" t nil)

(autoload 'inferior-haskell-info "inf-haskell" "\
Query the haskell process for the info of the given expression.

\(fn SYM)" t nil)

(autoload 'inferior-haskell-find-definition "inf-haskell" "\
Attempt to locate and jump to the definition of the given expression.

\(fn SYM)" t nil)

(autoload 'inferior-haskell-find-haddock "inf-haskell" "\
Find and open the Haddock documentation of SYM.
Make sure to load the file into GHCi or Hugs first by using C-c C-l.
Only works for functions in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it.

\(fn SYM)" t nil)

;;;***

;;;### (autoloads (haskell-hoogle literate-haskell-mode haskell-mode)
;;;;;;  "haskell-mode" "haskell-mode-2.4/haskell-mode.el" (18271
;;;;;;  25098))
;;; Generated autoloads from haskell-mode-2.4/haskell-mode.el

(autoload 'haskell-mode "haskell-mode" "\
Major mode for editing Haskell programs.
Blank lines separate paragraphs, comments start with `-- '.
\\<haskell-mode-map>
Literate scripts are supported via `literate-haskell-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Modules can hook in via `haskell-mode-hook'.  The following modules
are supported with an `autoload' command:

   `haskell-decl-scan', Graeme E Moss
     Scans top-level declarations, and places them in a menu.

   `haskell-doc', Hans-Wolfgang Loidl
     Echoes types of functions or syntax of keywords when the cursor is idle.

   `haskell-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

Module X is activated using the command `turn-on-X'.  For example,
`haskell-indent' is activated using `turn-on-haskell-indent'.
For more information on a module, see the help for its `X-mode'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `haskell-doc' is irregular in using `turn-(on/off)-haskell-doc-mode'.)

Use `haskell-version' to find out what version this is.

Invokes `haskell-mode-hook'.

\(fn)" t nil)

(autoload 'literate-haskell-mode "haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(autoload 'haskell-hoogle "haskell-mode" "\
Do a Hoogle search for QUERY.

\(fn QUERY)" t nil)

;;;***

;;;### (autoloads (haskell-indent-mode) "haskell-indent" "haskell-mode-2.4/haskell-indent.el"
;;;;;;  (18271 24057))
;;; Generated autoloads from haskell-mode-2.4/haskell-indent.el

(autoload 'haskell-indent-mode "haskell-indent" "\
``intelligent'' Haskell indentation mode that deals with
the layout rule of Haskell.  \\[haskell-indent-cycle] starts the cycle
which proposes new possibilities as long as the TAB key is pressed.
Any other key or mouse click terminates the cycle and is interpreted
except for RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Note: \\[indent-region] which applies \\[haskell-indent-cycle] for each line
of the region also works but it stops and asks for any line having more
than one possible indentation.
Use TAB to cycle until the right indentation is found and then RET to go the
next line to indent.

Invokes `haskell-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (haskell-doc-show-type haskell-doc-mode) "haskell-doc"
;;;;;;  "haskell-mode-2.4/haskell-doc.el" (18271 24003))
;;; Generated autoloads from haskell-mode-2.4/haskell-doc.el

(autoload 'haskell-doc-mode "haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional ARG)" t nil)

(defalias 'turn-on-haskell-doc-mode 'haskell-doc-mode)

(autoload 'haskell-doc-show-type "haskell-doc" "\
Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

;;;***

;;;### (autoloads (haskell-decl-scan-mode) "haskell-decl-scan" "haskell-mode-2.4/haskell-decl-scan.el"
;;;;;;  (18271 24057))
;;; Generated autoloads from haskell-mode-2.4/haskell-decl-scan.el

(autoload 'haskell-decl-scan-mode "haskell-decl-scan" "\
Minor mode for declaration scanning for Haskell mode.
Top-level declarations are scanned and listed in the menu item \"Declarations\".
Selecting an item from this menu will take point to the start of the
declaration.

\\[haskell-ds-forward-decl] and \\[haskell-ds-backward-decl] move forward and backward to the start of a declaration.

Under XEmacs, the following keys are also defined:

\\[fume-list-functions] lists the declarations of the current buffer,
\\[fume-prompt-function-goto] prompts for a declaration to move to, and
\\[fume-mouse-function-goto] moves to the declaration whose name is at point.

This may link with `haskell-doc' (only for Emacs currently).

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

To turn on declaration scanning for all Haskell buffers, add this to
.emacs:

  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

To turn declaration scanning on for the current buffer, call
`turn-on-haskell-decl-scan'.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (automatically set by the Haskell mode of
Moss&Thorn) is `bird', a Bird-style literate script is assumed.  If it
is nil or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-mode-hook'.

\(fn &optional ARG)" nil nil)

;;;***

;;;### (autoloads (haskell-cabal-mode) "haskell-cabal" "haskell-mode-2.4/haskell-cabal.el"
;;;;;;  (18231 1867))
;;; Generated autoloads from haskell-mode-2.4/haskell-cabal.el

(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(autoload 'haskell-cabal-mode "haskell-cabal" "\
Major mode for Cabal package description files.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-c-mode) "haskell-c" "haskell-mode-2.4/haskell-c.el"
;;;;;;  (18170 47169))
;;; Generated autoloads from haskell-mode-2.4/haskell-c.el

(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-c-mode))

(autoload 'haskell-c-mode "haskell-c" "\
Major mode for Haskell FFI files.

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "ruby/ruby-mode.el" (18862
;;;;;;  33836))
;;; Generated autoloads from ruby/ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing ruby scripts.
\\[ruby-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (rs-gnus-leafnode-kill-thread rs-gnus-summary-score-statistics
;;;;;;  rs-gnus-summary-sort-by-recipient rs-gnus-summary-limit-to-recipient
;;;;;;  rs-gnus-summary-mark-as-expirable-next-article rs-gnus-summary-mark-as-expirable-dont-move
;;;;;;  rs-gnus-summary-mark-as-expirable-and-next-line rs-gnus-summary-non-boring-headers
;;;;;;  rs-gnus-summary-more-headers rs-gnus-summary-mark-lists-expirable
;;;;;;  rs-gnus-summary-line-initialize rs-gnus-balloon-1 rs-gnus-balloon-0
;;;;;;  rs-gnus-summary-line-list-subject rs-gnus-summary-limit-to-label
;;;;;;  rs-gnus-summary-line-label rs-gnus-summary-line-score rs-gnus-summary-line-message-size
;;;;;;  rs-gnus-summary-line-content-type rs-gnus-summary-line-content-type-alist
;;;;;;  rs-gnus-summary-tree-lines rs-gnus-summary-tree-arrows-mt
;;;;;;  rs-gnus-summary-tree-lines-rs rs-gnus-summary-tree-arrows-01
;;;;;;  rs-gnus-summary-tree-arrows-test rs-gnus-summary-tree-arrows-wide
;;;;;;  rs-gnus-summary-tree-arrows-latin rs-gnus-summary-tree-arrows-ascii
;;;;;;  rs-gnus-summary-tree-arrows-ascii-default) "rs-gnus-summary"
;;;;;;  "slightly-modified/rs-gnus-summary.el" (18867 57586))
;;; Generated autoloads from slightly-modified/rs-gnus-summary.el

(autoload 'rs-gnus-summary-tree-arrows-ascii-default "rs-gnus-summary" "\
Use default tree layout with ascii arrows.

\(fn)" t nil)

(autoload 'rs-gnus-summary-tree-arrows-ascii "rs-gnus-summary" "\
Use tree layout with ascii arrows.

\(fn)" t nil)

(autoload 'rs-gnus-summary-tree-arrows-latin "rs-gnus-summary" "\
Use default tree layout with ascii arrows (RS).

\(fn)" t nil)

(defalias 'rs-gnus-summary-tree-arrows 'rs-gnus-summary-tree-arrows-wide)

(autoload 'rs-gnus-summary-tree-arrows-wide "rs-gnus-summary" "\
Use tree layout with wide unicode arrows.

\(fn)" t nil)

(autoload 'rs-gnus-summary-tree-arrows-test "rs-gnus-summary" "\
Use tree layout with unicode arrows.

\(fn)" t nil)

(autoload 'rs-gnus-summary-tree-arrows-01 "rs-gnus-summary" "\
Use tree layout with unicode arrows.

\(fn)" t nil)

(autoload 'rs-gnus-summary-tree-lines-rs "rs-gnus-summary" "\
Use tree layout with unicode lines.

\(fn)" t nil)

(autoload 'rs-gnus-summary-tree-arrows-mt "rs-gnus-summary" "\
Use tree layout with unicode arrows.

\(fn)" t nil)

(autoload 'rs-gnus-summary-tree-lines "rs-gnus-summary" "\
Use tree layout with unicode lines.

\(fn)" t nil)

(defvar rs-gnus-summary-line-content-type-alist '(("^text/plain" " ") ("^text/html" "h") ("^multipart/mixed" "m") ("^multipart/alternative" "a") ("^multipart/related" "r") ("^multipart/signed" "s") ("^multipart/encrypted" "e") ("^multipart/report" "t")) "\
Alist of regular expressions and summary line indicators.")

(custom-autoload 'rs-gnus-summary-line-content-type-alist "rs-gnus-summary" t)

(autoload 'rs-gnus-summary-line-content-type "rs-gnus-summary" "\
Display content type of message in summary line.

This function is intended to be used in `gnus-summary-line-format-alist', with
\(defalias 'gnus-user-format-function-X 'rs-gnus-summary-line-content-type).
See (info \"(gnus)Group Line Specification\").

You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'.

\(fn HEADER)" nil nil)

(autoload 'rs-gnus-summary-line-message-size "rs-gnus-summary" "\
Return pretty-printed version of message size.

Like `gnus-summary-line-message-size' but more verbose.  This function is
intended to be used in `gnus-summary-line-format-alist', with
\(defalias 'gnus-user-format-function-X 'rs-gnus-summary-line-message-size).

See (info \"(gnus)Group Line Specification\").

\(fn HEAD)" nil nil)

(autoload 'rs-gnus-summary-line-score "rs-gnus-summary" "\
Return pretty-printed version of article score.

See (info \"(gnus)Group Line Specification\").

\(fn HEAD)" nil nil)

(autoload 'rs-gnus-summary-line-label "rs-gnus-summary" "\
Display label of message in summary line.

This function is intended to be used in `gnus-summary-line-format-alist', with
\(defalias 'gnus-user-format-function-X 'rs-gnus-summary-line-label).
See (info \"(gnus)Group Line Specification\").

You need to add `X-Gnus-Label' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'.

\(fn HEADER)" nil nil)

(autoload 'rs-gnus-summary-limit-to-label "rs-gnus-summary" "\
Limit the summary buffer to articles that match a label.

\(fn REGEXP &optional NOT-MATCHING)" t nil)

(autoload 'rs-gnus-summary-line-list-subject "rs-gnus-summary" "\
Return modified subject for mailing lists.

This function is intended to be used in `gnus-summary-line-format-alist', with
\(defalias 'gnus-user-format-function-X 'rs-gnus-summary-line-list-subject).

See (info \"(gnus)Group Line Specification\").

\(fn HEAD)" nil nil)

(autoload 'rs-gnus-balloon-0 "rs-gnus-summary" "\
Show some informations about the current article.
Informations include size, score, content type and number.
Used as help echo for the summary buffer.

\(fn WINDOW OBJECT POSITION)" nil nil)

(autoload 'rs-gnus-balloon-1 "rs-gnus-summary" "\
Show full \"From\", \"Subject\", \"To\", and \"Date\" of the current article.
Used as help echo for the summary buffer.

\(fn WINDOW OBJECT POSITION)" nil nil)

(autoload 'rs-gnus-summary-line-initialize "rs-gnus-summary" "\
Setup my summary line.

\(fn)" t nil)

(autoload 'rs-gnus-summary-mark-lists-expirable "rs-gnus-summary" "\
Mark some articles (lists, ...) as expirable.

\(fn)" t nil)

(autoload 'rs-gnus-summary-more-headers "rs-gnus-summary" "\
Force redisplaying of the current article with ...

\(fn)" t nil)

(autoload 'rs-gnus-summary-non-boring-headers "rs-gnus-summary" "\
Force redisplaying of the current article with ...

\(fn)" t nil)

(autoload 'rs-gnus-summary-mark-as-expirable-and-next-line "rs-gnus-summary" "\
Mark N articles forward as expirable and go to next line.
Useful in a summary buffer with read articles.

\(fn N)" t nil)

(autoload 'rs-gnus-summary-mark-as-expirable-dont-move "rs-gnus-summary" "\
Mark this article expirable.  Don't move point.

\(fn)" t nil)

(autoload 'rs-gnus-summary-mark-as-expirable-next-article "rs-gnus-summary" "\
Mark this article expirable.  Move to next article.

\(fn)" t nil)

(autoload 'rs-gnus-summary-limit-to-recipient "rs-gnus-summary" "\
Limit the summary buffer to articles with the given RECIPIENT.

If NOT-MATCHING, exclude RECIPIENT.

To and Cc headers are checked.  You need to include them in
`nnmail-extra-headers'.

\(fn RECIPIENT &optional NOT-MATCHING)" t nil)

(autoload 'rs-gnus-summary-sort-by-recipient "rs-gnus-summary" "\
Sort the summary buffer by recipient name alphabetically.
If `case-fold-search' is non-nil, case of letters is ignored.
Argument REVERSE means reverse order.

\(fn &optional REVERSE)" t nil)

(autoload 'rs-gnus-summary-score-statistics "rs-gnus-summary" "\
Display score statistics for current summary buffer.

If ANCIENT, also count ancient articles.  Returns a list: (high
default low).

\(fn &optional ANCIENT QUIET)" t nil)

(autoload 'rs-gnus-leafnode-kill-thread "rs-gnus-summary" "\
Kill thread from here using leafnode.

\(fn)" t nil)

;;;***

;;;### (autoloads (bbdb-vcard-export-update-all) "bbdb-vcard-export"
;;;;;;  "slightly-modified/bbdb-vcard-export.el" (19037 54896))
;;; Generated autoloads from slightly-modified/bbdb-vcard-export.el

(autoload 'bbdb-vcard-export-update-all "bbdb-vcard-export" "\
Update the vcard Contacts directory from the bbdb database

\(fn OUTPUT-DIR CODING-SYSTEM)" t nil)

;;;***

;;;### (autoloads (twit) "twittering-mode" "twittering-mode/twittering-mode.el"
;;;;;;  (19387 22687))
;;; Generated autoloads from twittering-mode/twittering-mode.el

(autoload 'twit "twittering-mode" "\
Start twittering-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (wget-web-page wget) "wget" "wget-el/wget.el" (18862
;;;;;;  33836))
;;; Generated autoloads from wget-el/wget.el

(autoload 'wget "wget" "\
Wget interface to download URI asynchronously.
If argument ARG is non-nil, ask some options.
Called with prefix argument, turn argument ARG t.

If you are in dired mode which is seeing ftp directory,
`wget' regard current line file name as URI.

\(fn URI &optional ARG)" t nil)

(autoload 'wget-web-page "wget" "\
Wget interface to download whole Web page.
If argument ARG is non-nil, ask options.
Called with prefix argument, turn argument ARG t.

Second argument URI is string.
wget-web-page downlod whole Web page from it following relative link.

\(fn URI &optional ARG)" t nil)

;;;***

;;;### (autoloads (smart-operator-self-insert-command smart-operator-mode)
;;;;;;  "smart-operator" "xwl-elisp/smart-operator.el" (19276 26976))
;;; Generated autoloads from xwl-elisp/smart-operator.el

(autoload 'smart-operator-mode "smart-operator" "\
Insert operators with surrounding spaces smartly.

\(fn &optional ARG)" t nil)

(autoload 'smart-operator-self-insert-command "smart-operator" "\
Insert the entered operator plus surrounding spaces.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (less-minor-mode-off less-minor-mode-on auto-less-minor-mode
;;;;;;  less-quit less-scroll-down-line less-scroll-up-line less-minor-mode)
;;;;;;  "less" "xwl-elisp/less.el" (19261 56902))
;;; Generated autoloads from xwl-elisp/less.el

(autoload 'less-minor-mode "less" "\
Toggle less-minor-mode.

With less-minor-mode enabled, you could use `less' like keys to view files.
\\{less-minor-mode-map}.

\(fn &optional ARG)" t nil)

(autoload 'less-scroll-up-line "less" "\
Scroll up one line.

\(fn)" t nil)

(autoload 'less-scroll-down-line "less" "\
Scroll down one line.

\(fn)" t nil)

(autoload 'less-quit "less" "\
Quit `less-minor-mode'.

\(fn)" t nil)

(autoload 'auto-less-minor-mode "less" "\
Turn on `less-minor-mode' for files not matching `auto-less-exclude-regexp'.

This is a useful hook to add to `find-file-hook'.

\(fn)" nil nil)

(autoload 'less-minor-mode-on "less" "\
Turn on `less-minor-mode'.

\(fn)" nil nil)

(autoload 'less-minor-mode-off "less" "\
Turn off `less-minor-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (finkinfo-mode) "finkinfo-mode" "xwl-elisp/finkinfo-mode.el"
;;;;;;  (18867 57586))
;;; Generated autoloads from xwl-elisp/finkinfo-mode.el

(autoload 'finkinfo-mode "finkinfo-mode" "\
Major mode for fink info files in Mac OS X.
\\{finkinfo-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (easy-todo-mode) "easy-todo" "xwl-elisp/easy-todo.el"
;;;;;;  (19120 65217))
;;; Generated autoloads from xwl-elisp/easy-todo.el

(autoload 'easy-todo-mode "easy-todo" "\
Major mode for managing todos.
\\{easy-todo-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (cwit) "cwit" "xwl-elisp/cwit.el" (19264 47723))
;;; Generated autoloads from xwl-elisp/cwit.el

(autoload 'cwit "cwit" "\
Create a *Cwit* buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (holiday-solar-term holiday-lunar cal-china-x-birthday-from-chinese)
;;;;;;  "cal-china-x" "xwl-elisp/cal-china-x.el" (19368 45037))
;;; Generated autoloads from xwl-elisp/cal-china-x.el

(autoload 'cal-china-x-birthday-from-chinese "cal-china-x" "\
Return birthday date this year in Gregorian form.

LUNAR-MONTH and LUNAR-DAY are date number used in chinese lunar
calendar.

\(fn LUNAR-MONTH LUNAR-DAY)" t nil)

(autoload 'holiday-lunar "cal-china-x" "\
Like `holiday-fixed', but with LUNAR-MONTH and LUNAR-DAY.

When there are multiple days(like Run Yue or 闰月, e.g.,
2006-08-30, which is 07-07 in lunar calendar, the chinese
valentine's day), we use NUM to define which day(s) as
holidays. The rules are:

NUM = 0, only the earlier day.
NUM = 1, only the later day.
NUM with other values(default), all days(maybe one or two).

emacs23 introduces a similar `holiday-chinese', a quick test
shows that it does not recognize Run Yue at all.

\(fn LUNAR-MONTH LUNAR-DAY STRING &optional NUM)" nil nil)

(autoload 'holiday-solar-term "cal-china-x" "\
A holiday(STR) on SOLAR-TERM day.
See `cal-china-x-solar-term-name' for a list of solar term names .

\(fn SOLAR-TERM STR)" nil nil)

;;;***

;;;### (autoloads (buffer-action-run buffer-action-compile) "buffer-action"
;;;;;;  "xwl-elisp/buffer-action.el" (19360 59338))
;;; Generated autoloads from xwl-elisp/buffer-action.el

(autoload 'buffer-action-compile "buffer-action" "\
Run `compile' by checking project builders and `buffer-action-table'.

Project builders are like make, ant, etc.  When running for the
first time, you can edit the command in minibuffer, then it would
use last command without bothering you any more.  If you want to
edit it again, please add C-u prefix.

\(fn)" t nil)

(autoload 'buffer-action-run "buffer-action" "\
Run the binary file according to `buffer-action-table'.

When running for the first time, you can edit the command in
minibuffer, else use last command without bothering you any
more. If you want to edit it again, please add C-u prefix.

\(fn)" t nil)

;;;***

;;;### (autoloads (dashboard) "dashboard" "xwl-elisp/dashboard/dashboard.el"
;;;;;;  (19214 33075))
;;; Generated autoloads from xwl-elisp/dashboard/dashboard.el

(autoload 'dashboard "dashboard" "\
Create a *Dashboard* buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (ga) "ga" "xwl-elisp/ga/ga.el" (19214 33075))
;;; Generated autoloads from xwl-elisp/ga/ga.el

(autoload 'ga "ga" "\
Create or switch to a ga buffer.

\(fn &optional BACKEND)" t nil)

;;;***

(provide 'xwl-autoloads)
