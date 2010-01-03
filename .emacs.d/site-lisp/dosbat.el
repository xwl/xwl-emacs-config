;;;; dosbat.el --- Dos Batch File mode

;; Copyright (C) 2003 Glen Peterson

;; Author: Glen Peterson <glen@organicdesign.org>
;; Contributors: Glen Peterson
;; Created: 9 October 2003
;; Version: $Revision: 0.01 $
;; Keywords: languages, DOS, Batch File

;;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Functions for editing DOS Batch files

;; Send bugs to glen@organicdesign.org


;;;; Code:
(provide 'dosbat)

;(require 'cl)
;(require 'compile)
;(require 'font-lock)

;; Define core `bat' group.
(defgroup bat nil
  "Major mode for editing Microsoft DOS batch files."
  :prefix "bat-"
  :group 'languages)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version information

(defconst dosbat-version "0.0.1"
  "Version number of dosbat DOS batch file mode.")

(defun dosbat-version ()
  "Returns the value of the variable `dosbat-version'."
  dosbat-version)

(defconst dosbat-maintainer-address "glen@organicdesign.org")

(defconst bat-font-lock-keywords
  (list

;        '( "at\\( \\|trib \\)" (0 font-lock-keyword-face t))
;        '( "assoc " (0 font-lock-keyword-face t))
;        '( "break " (0 font-lock-keyword-face t))
;        '( "ca\\(cls\\|ll\\) " (0 font-lock-keyword-face t))
;        '( "cd " (0 font-lock-keyword-face t))
;        '( "ch\\(cp\\|dir\\|kdsk\\|kntfs\\) " (0 font-lock-keyword-face t))
;        '( "cls " (0 font-lock-keyword-face t))
;        '( "cmd " (0 font-lock-keyword-face t))
;        '( "co\\(lor \\|mp \\|mpact \\|nvert \\|py \\)" (0 font-lock-keyword-face t))
;        '( "date " (0 font-lock-keyword-face t))
;        '( "del " (0 font-lock-keyword-face t))
;        '( "di\\(r \\|skcomp \\|skcopy \\|oskey \\)" (0 font-lock-keyword-face t))
;        '( "echo " (0 font-lock-keyword-face t))
;        '( "endlocal " (0 font-lock-keyword-face t))
;        '( "erase " (0 font-lock-keyword-face t))
;        '( "exit " (0 font-lock-keyword-face t))
;        '( "fc " (0 font-lock-keyword-face t))
;        '( "find\\( \\|str \\)" (0 font-lock-keyword-face t))
;        '( "for\\( \\|mat \\)" (0 font-lock-keyword-face t))
;        '( "ftype " (0 font-lock-keyword-face t))
;        '( "goto " (0 font-lock-keyword-face t))
;        '( "graftabl " (0 font-lock-keyword-face t))
;        '( "help " (0 font-lock-keyword-face t))
;        '( "if " (0 font-lock-keyword-face t))
;        '( "label " (0 font-lock-keyword-face t))
;        '( "md " (0 font-lock-keyword-face t))
;        '( "mkdir " (0 font-lock-keyword-face t))
;        '( "mo\\(de\\|re\\|ve\\) " (0 font-lock-keyword-face t))
;        '( "pa\\(th\\|use\\) " (0 font-lock-keyword-face t))
;        '( "popd " (0 font-lock-keyword-face t))
;        '( "pr\\(int\\|ompt\\) " (0 font-lock-keyword-face t))
;        '( "pushd " (0 font-lock-keyword-face t))
;        '( "rd " (0 font-lock-keyword-face t))
;        '( "re\\(cover \\|n \\|name \\|place \\)" (0 font-lock-keyword-face t))
;        '( "rmdir " (0 font-lock-keyword-face t))
;        '( "[\n@ ]set\\( \\|$\\|local \\local$\\)" (0 font-lock-keyword-face t))
;        '( "shift " (0 font-lock-keyword-face t))
;        '( "sort " (0 font-lock-keyword-face t))
;        '( "start " (0 font-lock-keyword-face t))
;        '( "subst " (0 font-lock-keyword-face t))
;        '( "ti\\(me\\|tle\\) " (0 font-lock-keyword-face t))
;        '( "tree " (0 font-lock-keyword-face t))
;        '( "type " (0 font-lock-keyword-face t))
;        '( "ver\\( \\|ify \\)" (0 font-lock-keyword-face t))
;        '( "vol " (0 font-lock-keyword-face t))
;        '( "xcopy " (0 font-lock-keyword-face t))
        ; These need to override the above
        '( "^:[^ \t\n]+[ \t]*$" (0 font-lock-reference-face t))
        '( "goto [^ \t\n]+[ \t]*$" (0 font-lock-reference-face t))
        '( "%[^ %]+%" (0 font-lock-constant-face t))
        '( "%\\(~[a-zA-Z]+\\|\\)[0-9*]" (0 font-lock-constant-face t))
        '( "%%[a-zA-Z]" (0 font-lock-constant-face t))
        ; comments are last to cover-up anything
        '( "^rem [^\n]*$" (0 font-lock-comment-face t))
        )
  "Additional expressions to highlight in bat mode.")


;;;###autoload
(defun bat-mode ()
  "Major mode for editing DOS batch files.

Special commands:
Turning on bat-mode calls the value of the variable `bat-mode-hook',
if that value is non-nil.

Font lock mode:

Turning on font lock mode causes various bat syntactic structures to be
highlighted. To turn this on whenever you visit a bat file, add
the following to your .emacs file:
  \(add-hook 'bat-mode-hook 'turn-on-font-lock\)
"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "DOS bat")
  (setq major-mode 'bat-mode)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(bat-font-lock-keywords t))
  (turn-on-font-lock)
  (run-hooks 'bat-mode-hook))

;;; dosbat.el ends here
