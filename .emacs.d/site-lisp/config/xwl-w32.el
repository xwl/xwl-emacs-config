;;; xwl-vim.el --- w32 specific utilites

;; Copyright (C) 2010 William Xu

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

(defun xwl-w32-get-drives ()
  "Get a list of drive names from get_drives.py."
  (when (executable-find "python")
    (read
     (shell-command-to-string
      (concat "python "
	      (shell-quote-argument
	       (expand-file-name "~/w32/get_drives.py")))))))

(setq xwl-w32-drive-separator ".")

;; Prepend drive name on buffer on w32
(defadvice uniquify-get-proposed-name (after prepend-drive-name activate)
  (let ((d (upcase (substring (ad-get-arg 1) 0 1))))
    (setq ad-return-value
          (concat d xwl-w32-drive-separator
                  (cdr (or (assoc d xwl-w32-drives)
                           (assoc (downcase d) xwl-w32-drives)))
                  ":/" ad-return-value))))

(when xwl-w32?
  (setq xwl-w32-drives (xwl-w32-get-drives)))

(provide 'xwl-w32)
;;; xwl-w32.el ends here
