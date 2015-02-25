;;; xwl-var.el --- Global variables

;; Copyright (C) 2010, 2011, 2013, 2014 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

(setq xwl-w32? (eq system-type 'windows-nt))

(setq xwl-w32-redirect-locally? t)

(defun xwl-at-company ()
  (file-exists-p "~/.emacs.d/site-lisp/config/kaixya.el"))

(setq xwl-at-company? (xwl-at-company))

(setq xwl-w3m-arguments '())

(setq xwl-black-background? xwl-at-company?)
(setq xwl-proxy-server nil
      xwl-proxy-port nil)

(setq xwl-local-fonts nil)

(let ((local-file "~/.emacs.d/site-lisp/config/local.el"))
  (when (file-exists-p local-file)
    (load local-file)))

(provide 'xwl-var)

;;; xwl-var.el ends here
