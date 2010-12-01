;;; xwl-convenience.el --- Must have cookies, keep it small and stable

;; Copyright (C) 2007, 2008, 2009, 2010 William Xu

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

;; coding system
(case system-type
  ((windows-nt)
   (prefer-coding-system 'gbk))
  (t
   (prefer-coding-system 'utf-8-emacs)))

;; (require 'auto-enca)
;; (modify-coding-system-alist 'file "" 'enca-detect-coding)

(setq-default auto-fill-function 'do-auto-fill
              ;; default is 70
              fill-column 80)

(add-hook 'log-edit-mode-hook (lambda () (setq fill-column 70)))

(column-number-mode 1)
(line-number-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'expression)

(setq scroll-step 1
      ;; FIXME: This will cause eshell jumping when at the bottom of the buffer.
      ;; scroll-margin 3
      scroll-conservatively most-positive-fixnum
      hscroll-step 1
      hscroll-margin 3)

(setq scroll-preserve-screen-position 'always)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq inhibit-startup-message t)

(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'overwrite-mode   'disabled t)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq require-final-newline t)
(setq-default truncate-lines t
	      truncate-partial-width-windows t)

(setq-default indent-tabs-mode nil)

(setq history-delete-duplicates t)

(setq default-major-mode 'org-mode)

(setq split-width-threshold 150)

;; (global-visual-line-mode 1)

;; (setq display-time-format "<%V-%u> %m/%d/%H:%M")
(setq display-time-format "%a(%V) %m.%d/%H:%M")

(display-time)

(fset 'yes-or-no-p 'y-or-n-p)

(keyboard-translate ?\C-h ?\C-?)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq x-select-enable-clipboard t)

(mouse-avoidance-mode 'animate)

;; expansions, abbreviations & completions
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-whole-kill
;;	senator-try-expand-semantic
	try-expand-dabbrev-visible
	try-expand-dabbrev-from-kill
	try-expand-dabbrev-all-buffers
	try-expand-all-abbrevs
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-list
;	try-complete-lisp-symbol-partially
;;	try-complete-lisp-symbol
        try-expand-line
	try-expand-line-all-buffers))

(setq save-abbrevs t)

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(abbrev-mode 1)

;; back up
(setq version-control t
      kept-old-versions 2
      kept-new-versions 3
      delete-old-versions t
      backup-by-copying t
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t)

(setq backup-directory-alist
      '(("." . "~/var/emacs_backups" )))

;; same filename, different paths
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; FIXME: conflict with ido-hacks.el
;; (setq completion-styles '(partial-completion initials))
;; (setq completion-pcm-complete-word-inserts-delimiters t)

;; session
(setq session-initialize t)
(setq session-globals-exclude '(load-history
                                register-alist
                                vc-comment-ring
                                flyspell-auto-correct-ring))

(setq session-globals-regexp "-\\(ring\\|history\\)\\'")

(setq-default save-place t)

(add-hook 'minibuffer-setup-hook 'turn-off-auto-fill)

;; buffer scrolling
(global-set-key (kbd "M-p") 'less-scroll-down-line)
(global-set-key (kbd "M-n") 'less-scroll-up-line)

(add-hook 'makefile-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "M-n") 'less-scroll-up-line)
	     (local-set-key (kbd "M-p") 'less-scroll-down-line)))

(setq mouse-wheel-scroll-amount (list 1))

;; Am i weird or Emacs is weird ?
(global-set-key (kbd "C-,") 'next-buffer)
(global-set-key (kbd "C-.") 'previous-buffer)

(global-set-key (kbd "C-c [")  'previous-error)
(global-set-key (kbd "C-c ]")  'next-error)

(winner-mode 1)
(global-set-key (kbd "C-<") 'winner-undo)
(global-set-key (kbd "C->") 'winner-redo)

(global-set-key (kbd "C-c n t") 'visual-line-mode)
(global-set-key (kbd "C-c m D") 'toggle-debug-on-error)

;; (setq inhibit-eol-conversion t)

;; less
(global-set-key (kbd "C-c v") 'less-minor-mode)
(eval-after-load 'less
  '(progn
     (setq auto-less-exclude-regexp
           (concat auto-less-exclude-regexp
                   "\\|"
                   (regexp-opt '("todo.org"
                                 "outgoing"
                                 "*gud"
                                 "*anything"
                                 ))))
     (setq auto-less-exclude-modes
           (append auto-less-exclude-modes
                   '()))
     ))

(global-less-minor-mode 1)

(defadvice save-buffers-kill-terminal (around disable-less activate)
  (global-less-minor-mode -1)
  ad-do-it)

;; (setq cua-enable-cua-keys nil)
;; (cua-mode 1)

;; (global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "C-w"))
;; (global-unset-key (kbd "M-w"))
;; (global-set-key (kbd "C-S-v") 'scroll-up)
;; (global-set-key (kbd "M-V") 'scroll-down)

(setq-default tab-width 4)

(provide 'xwl-convenience)

;;; xwl-convenience.el ends here
