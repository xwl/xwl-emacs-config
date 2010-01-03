;;; xwl-makefile.el --- For Makefile                -*- emacs-lisp -*-

;; Copyright (C) 2009 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

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

(require 'autoload)

(load "xwl-path.el")
(require 'xwl-subr)

(setq xwl-makefile-autoloads-file-base "xwl-autoloads"
      ;; (format "xwl-autoloads-%S" window-system)
      xwl-makefile-autoloads-file (concat xwl-makefile-autoloads-file-base ".el")
      xwl-makefile-subdir-list
      ;; "22" 
      '("." "wget-el" "ruby" "qterm" "dictionary-el" "debian" 
        "xwl-elisp" "xwl-elisp/ga" "xwl-elisp/dashboard" "haskell-mode-2.4"
        "slightly-modified" "xwl-elisp/wubi"
        ))

(setq xwl-makefile-files
      (xs-flatmap
       (lambda (d)
         (directory-files d t "^[^.]+\\.el\\'" t))
       xwl-makefile-subdir-list))

(defun xwl-makefile-ensure-directory ()
  (unless (file-exists-p "xwl-makefile.el")
    (error "Must run me under directory of xwl-makefile.el")))

;;;###autoload
(defun xwl-makefile-all ()
  (xwl-makefile-ensure-directory)

  (let ((autoloads-not-up2date t))
         ;; (xs-accumulate
;;           (lambda (a b) (or a b))
;;           nil
;;           (mapcar (lambda (f)
;;                     (file-newer-than-file-p f xwl-makefile-autoloads-file))
;;                   xwl-makefile-files))))

    ;; autoloads
    (when autoloads-not-up2date
      (with-current-buffer (find-file-noselect xwl-makefile-autoloads-file)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (mapc (lambda (f) (generate-file-autoloads f)) xwl-makefile-files)
          (insert (format "\n(provide '%s)\n" xwl-makefile-autoloads-file-base))
          (save-buffer))))

    (xwl-makefile-byte-compile)

    (message "xwl-makefile-all done")))

;;;###autoload
(defun xwl-makefile-byte-compile ()
  (interactive)
  (mapc (lambda (f)
          (when (file-newer-than-file-p f (replace-regexp-in-string
                                           "\\.el\\'" ".elc" f))
            (byte-compile-file f)))
        xwl-makefile-files)

  (message "xwl-makefile-byte-compile done"))

;;;###autoload
(defun xwl-makefile-clean ()
  (xwl-makefile-ensure-directory)

  (mapc (lambda (f)
          (let ((elc (concat f "c")))
            (when (file-exists-p elc)
            (message "(delete-file \"%s\")..." elc)
            (delete-file elc))))
        xwl-makefile-files)

  (message "xwl-makefile-clean done"))


(provide 'xwl-makefile)

;;; xwl-makefile.el ends here
