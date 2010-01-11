;;; xwl-bindings.el --- Key bindings

;; Copyright (C) 2008, 2009 William Xu

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

(require 'xwl-util)

;;; `C-c i *': Invoke external programs, e.g., scheme, mysql.

(setq 未临-外部命令-映射 (make-sparse-keymap))
(defalias '未临-外部命令-前缀 未临-外部命令-映射)
(define-key mode-specific-map "i" '未临-外部命令-前缀)

(define-key 未临-外部命令-映射 (kbd "m") 'sql-mysql)
(define-key 未临-外部命令-映射 (kbd "s") 'run-scheme)
(define-key 未临-外部命令-映射 (kbd "p") 'run-python)

(defun 未临-新年好 ()
  (interactive)
  (message "森宁好啊！"))

;;; Fn 功能键

(global-set-key (kbd "<f2>") 'woman)
(global-set-key (kbd "<f5>") 'ibuffer) ;'dashboard) ;'w3m)
(global-set-key (kbd "<f7>") 'xwl-bbdb)

;; 'org-agenda-list
(global-set-key (kbd "<f8>") (lambda ()
                               (interactive) 
                               ;;   ;; (find-file "~/notes/todo.org")
                               ;;   (case system-type
                               ;;     ((darwin)
                               ;;      (delete-other-windows)
                               ;;      (find-file "~/notes/todo")
                               ;;      (split-window-horizontally)
                               ;;      (find-file "~/notes/todo-finland"))
                               ;;     (t
                               ;;      (delete-other-windows)
                               ;;      (find-file "~/notes/todo")
                               ;;      (split-window-horizontally)
                               ;;      (find-file "~/notes/todo-nokia"))) 
                               (org-agenda 1 "h")
                               (command-execute (kbd "C-x 1"))
                               ;; 'org-agenda
                               ))

(global-set-key (kbd "<f9>") 'shell) ;'eshell) ;xwl-term ;xwl-run-scsh
;; (global-set-key (kbd "<f11>") 'repeat)
(global-set-key (kbd "<f13>") 'kill-this-buffer)

;; operators
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

(if (xwl-tty-p)
    (global-set-key (kbd "C-h") 'delete-backward-char)

  (keyboard-translate ?\C-h ?\C-?)
  (keyboard-translate ?\C-? ?\C-d))

(defun xwl-heaven ()
  (interactive)
  (global-set-key (kbd "<right>") 'windmove-right)
  (global-set-key (kbd "<left>")  'windmove-left)
  (global-set-key (kbd "<up>")    'windmove-up)
  (global-set-key (kbd "<down>")  'windmove-down))

(defun xwl-modern ()
  (interactive)
  (global-set-key (kbd "<right>") (kbd "C-f"))
  (global-set-key (kbd "<left>")  (kbd "C-b"))
  (global-set-key (kbd "<up>")    (kbd "C-p"))  
  (global-set-key (kbd "<down>")  (kbd "C-n")))

(xwl-heaven)
  
;; (global-set-key (kbd "C-c l") 'windmove-right)
;; (global-set-key (kbd "C-c h")  'windmove-left)
;; (global-set-key (kbd "C-c k")    'windmove-up)
;; (global-set-key (kbd "C-c j")  'windmove-down)

(global-set-key (kbd "ESC C-s") 'isearch-forward)
(global-set-key (kbd "ESC C-r") 'isearch-backward)
(global-set-key (kbd "ESC \\") 'just-one-space)
(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key (kbd "M-%") 'replace-regexp)
(global-set-key (kbd "M-_") 'highlight-changes-previous-change)
(global-set-key (kbd "M-+") 'highlight-changes-next-change)
(global-set-key (kbd "M-s") 'dictionary-search)
(global-set-key (kbd "C-M-k") 'kill-paragraph)
(global-set-key (kbd "M-K") 'kill-sexp)
;; (global-set-key (kbd "M-H") 'mark-sexp)

(defun sl-mark-symbol ()
  "Like `mark-sexp' but smarter."
  (interactive)
  (set-mark (beginning-of-thing 'symbol))
  (goto-char (end-of-thing 'symbol)))

(global-set-key (kbd "M-H") 'sl-mark-symbol)

(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "M-C") 'capitalize-region)
(global-set-key (kbd "M-U") 'upcase-region)
(global-set-key (kbd "M-L") 'downcase-region)

;; C-x
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

;; (global-set-key (kbd "C-x r C-@") 'rm-set-mark)
;; (global-set-key (kbd "C-x r C-x C-x") 'rm-exchange-point-and-mark)
;; (global-set-key (kbd "C-x r C-w") 'rm-kill-region)
;; (global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
;; (global-set-key (kbd "C-x r C-y") 'yank-rectangle)

;; C-c
(global-set-key (kbd "C-c f") 'ffap)
(global-set-key (kbd "C-c o") '(lambda () (interactive)
				 (call-interactively 'occur)
				 (other-window 1)))
(global-set-key (kbd "C-c j") 'imenu)

(global-set-key (kbd "C-c m a") 'his-align-cols)
;; (global-set-key (kbd "C-c m m") 'apply-macro-to-region-lines)
(global-set-key (kbd "C-c m q") 'query-replace-regexp)
(global-set-key (kbd "C-c m h") 'htmlize-file)
(global-set-key (kbd "C-c m v") 'visit-tags-table)
(global-set-key (kbd "C-c m S") '(lambda () (interactive)
				   (unless (get-buffer ".wubi_scratch")
				     (find-file "~/.wubi_scratch"))
				   (switch-to-buffer ".wubi_scratch")))
(global-set-key (kbd "C-c m e") '(lambda () (interactive)
				   (call-interactively 'eval-region)
				   (message "eval-region...done")))
(global-set-key (kbd "C-c C-\\") 'c-backslash-region)

(global-set-key (kbd "C-c a") 'apropos)

;; (global-set-key (kbd "C-c m c") (lambda ()
;;                                 (interactive)
;;                                 (let ((inhibit-read-only t))
;;                                   (delete-region (point-min) (point-max)))))


;; deal with japanese keyboard
(keyboard-translate ?\¥ ?\\)
(global-set-key (kbd "M-¥") (kbd "M-\\"))
(global-set-key (kbd "C-¥") 'hippie-expand)
(global-set-key (kbd "C-x C-¥") 'goto-last-change)
(global-set-key (kbd "C-M-¥") 'indent-region)

(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key (kbd "C-2") 'set-mark-command)

(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c m s") '(lambda () (interactive) (find-file "~/.scratch")))
(global-set-key (kbd "C-c m r") 'revert-buffer)

(global-set-key (kbd "M-S") 'xwl-search-jp)


(provide 'xwl-bindings)

;;; xwl-bindings.el ends here
