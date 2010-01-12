;; xwl-main.el --- Main entry for The One True Editor

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary

;; Tested under GNU/Linux, Windows XP and Mac OS X.

;; Key Binding Conventions
;; ------------------------
;; `backtab' (TODO: or <S-tab> ?)
;;     for completing commands(e.g., muse, texinfo, TeX, LaTeX commands),
;;     just like in shell.
;;
;; `C-x C-e', `C-x C-r', `C-x C-b'
;;     eval last sexp, eval region, eval buffer
;;
;;     Used in elisp, scheme, latex, etc.
;;
;; `C-c i *'
;;     Invoke external programs, e.g., scheme, mysql.
;;
;; `C-c e *'
;;     EMMS keys
;;
;; `C-c b *'
;;     boxquote
;;
;; `C-c m *'
;;     misc bindings
;;
;; `C-c g c'
;;    compile(makeinfo, latex, ...) buffer, use buffer-action.el

;;; Code

(load "~/.emacs.d/site-lisp/config/xwl-path.el")

(require 'xwl-autoloads)
(when (eq system-type 'windows-nt) (require 'xwl-w32))
(require 'xwl-util)
(require 'xwl-vim)
(require 'xwl-private)
(require 'xwl-mode-line)
(require 'xwl-help)
(require 'xwl-buffer)
(require 'xwl-convenience)
(require 'xwl-bindings)
(require 'xwl-outline)
(require 'xwl-calendar)
(require 'xwl-programming)
(require 'xwl-shell)
(require 'xwl-dired)
(require 'xwl-redefined)
(require 'xwl-gnus)
(require 'xwl-dictionary)
(require 'xwl-web)
(require 'xwl-erc)
(ignore-errors (require 'xwl-emms))
(require 'xwl-org)
;; (require 'xwl-muse)
(require 'xwl-tex)
(require 'xwl-misc)
(when window-system
  (require 'xwl-window)
  (require 'xwl-color-theme))

;;; xwl-main.el ends here
