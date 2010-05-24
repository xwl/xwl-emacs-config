;;; xwl-wubi.el --- Chinese Wu Bi input method

;; Copyright (C) 2010 William Xu

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

;;; Code:

;; FIXME: how to make it autoload?
(require 'wubi)
(register-input-method
 "chinese-wubi" "Chinese" 'quail-use-package "wubi" "wubi")

(setq wubi-phrases-file "~/.wubi-phrases.el")

(eval-after-load 'wubi
  '(progn
     (ignore-errors (wubi-load-local-phrases))
     ))

(setq default-input-method "chinese-wubi")

(setq xwl-input-methods
      '("chinese-wubi"
        "japanese"
        "japanese-katakana"
        "chinese-py")
      xwl-current-input-methods xwl-input-methods)

(defun xwl-cycle-input-method ()
  "Cycle `xwl-input-method-alist'."
  (interactive)
  (if (null (cdr xwl-current-input-methods))
      (setq xwl-current-input-methods xwl-input-methods)
    (setq xwl-current-input-methods (cdr xwl-current-input-methods)))
  (set-input-method (car xwl-current-input-methods)))

(setq xwl-traditional-p t)

;; (load "wubi")
;; (load "wubi-rules")

(defun xwl-toggle-simplified/traditional-input-method ()
  (interactive)
  (setq xwl-traditional-p (not xwl-traditional-p))
  (if xwl-traditional-p
      (progn
        (load "wubi-b5")
        (load "wubi-rules-b5")
        (setq wubi-phrases-file "~/.wubi-phrases-b5.el")
        (wubi-load-local-phrases))
    (load "wubi")
    (load "wubi-rules")
    (setq wubi-phrases-file "~/.wubi-phrases.el")
    (wubi-load-local-phrases)))

(global-set-key (kbd "C-?") 'xwl-cycle-input-method)

;; (global-set-key (kbd "C-?") 'xwl-cycle-input-method)
;; (global-set-key (kbd "C-,") 'wubi-toggle-quanjiao-banjiao)
(global-set-key (kbd "C-c m W") 'wubi-load-local-phrases)

(provide 'xwl-wubi)
