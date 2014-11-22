;;; xwl-convenience.el --- Must have cookies, keep it small and stable

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 William Xu

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
;; (case system-type
;;   ((windows-nt)
;;    (prefer-coding-system 'gbk))
;;   (t
(prefer-coding-system 'utf-8-emacs)
;;))

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

      ;; scroll-conservatively most-positive-fixnum
      hscroll-step 1
      hscroll-margin 3)

(setq scroll-preserve-screen-position 'always)

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

(setq split-width-threshold 140)

;; (global-visual-line-mode 1)

;; (setq display-time-format "<%V-%u> %m/%d/%H:%M")
(setq display-time-format "%a(%V) %m.%d/%H:%M")

(display-time)

(fset 'yes-or-no-p 'y-or-n-p)

(keyboard-translate ?\C-h ?\C-?)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

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

(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
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

;; (setq completion-styles '(initials partial-completion ))
;(basic partial-completion emacs22)
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

;; (setq mouse-wheel-scroll-amount (list 1))

;; Am i weird or Emacs is weird ?
(global-set-key (kbd "C-c ,") 'previous-buffer)
(global-set-key (kbd "C-c .") 'next-buffer)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)

(global-set-key (kbd "C-c [")  'previous-error)
(global-set-key (kbd "C-c ]")  'next-error)

(winner-mode 1)
(global-set-key (kbd "C-c <") 'winner-undo)
(global-set-key (kbd "C-c >") 'winner-redo)
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
                                 "*All*"
                                 "*magit"
                                 "*Custom"

                                 ;; cedet
                                 "*SPP parse hack"
                                 "*CEDET Global*"
                                 ))))
     (setq auto-less-exclude-modes
           (append auto-less-exclude-modes
                   '(text-mode prog-mode)))
     ))

(global-less-minor-mode 1)

(defadvice save-buffers-kill-terminal (around disable-less activate)
  (let ((global-less-minor-mode nil))
    ad-do-it))

;; (setq cua-remap-control-v nil)

(setq cua-enable-cua-keys nil)
(cua-mode 1)

;; (global-unset-key (kbd "C--"))
;; (global-unset-key (kbd "C-_"))
;; (global-unset-key (kbd "C-y"))
;; (global-unset-key (kbd "M-w"))
;; (global-unset-key (kbd "C-w"))

(eval-after-load 'org
  '(progn
     (defun org-yank (&optional arg)
       (interactive "P")
       (org-yank-generic  (if cua-mode 'cua-paste 'yank) arg))
     ))

;; (global-set-key (kbd "C-S-v") 'scroll-up)
;; (global-set-key (kbd "M-V") 'scroll-down)

;; (setq-default tab-width 4)

;; (transient-mark-mode -1)

(setq-default line-spacing 2)

(eval-after-load 'ido
  '(progn
     (defun ido-completing-read (prompt choices &optional _predicate require-match
                                        initial-input hist def _inherit-input-method)
       "Redefined to support non-string argument `choices' as used in `completing-read'. "
       (let ((ido-current-directory nil)
             (ido-directory-nonreadable nil)
             (ido-directory-too-big nil)
             (ido-context-switch-command 'ignore)
             (ido-choice-list ;(if (and (listp choices)
                              ;         (stringp (car choices)))
                              ;    choices
                                (all-completions "" choices _predicate)));)
         ;; Initialize ido before invoking ido-read-internal
         (ido-common-initialization)
         (ido-read-internal 'list prompt hist def require-match initial-input)))

     (setq completing-read-function 'ido-completing-read)


(defun ido-read-internal (item prompt history &optional default require-match initial)
  "Ido bug when `history' is a cons. repro in ffap.  "
  (let
      ((ido-cur-item item)
       (ido-entry-buffer (current-buffer))
       (ido-process-ignore-lists t)
       (ido-process-ignore-lists-inhibit nil)
       (ido-set-default-item t)
       ido-default-item
       ido-selected
       ido-final-text
       (done nil)
       (icomplete-mode nil) ;; prevent icomplete starting up
       ;; Exported dynamic variables:
       ido-cur-list
       ido-ignored-list
       (ido-rotate-temp nil)
       (ido-keep-item-list nil)
       (ido-use-merged-list nil)
       (ido-try-merged-list t)
       (ido-pre-merge-state nil)
       (ido-case-fold ido-case-fold)
       (ido-enable-prefix ido-enable-prefix)
       (ido-enable-regexp ido-enable-regexp)
       (ido-show-confirm-message nil)
       )

    (ido-setup-completion-map)
    (setq ido-text-init initial)
    (setq ido-input-stack nil)

    (run-hooks 'ido-setup-hook)

    (while (not done)
      (ido-trace "\n_LOOP_" ido-text-init)
      (setq ido-exit nil)
      (setq ido-rescan t)
      (setq ido-rotate nil)
      (setq ido-text "")
      (when ido-set-default-item
	(setq ido-default-item
	      (cond
	       ((eq item 'buffer)
		(if (bufferp default) (buffer-name default) default))
	       ((stringp default)
		(if (memq item '(file dir))
		    (file-name-nondirectory default)
		  default))
	       ((eq item 'file)
		(and ido-enable-last-directory-history
		     (let ((d (assoc ido-current-directory ido-last-directory-list)))
		       (and d (cdr d)))))))
	(if (member ido-default-item ido-ignore-item-temp-list)
	    (setq ido-default-item nil))
	(ido-trace "new default" ido-default-item)
	(if ido-default-item
	    (setq ido-initial-position 0))
	(setq ido-set-default-item nil))

      (if ido-process-ignore-lists-inhibit
	  (setq ido-process-ignore-lists nil))

      (if (and ido-use-merged-list (memq ido-try-merged-list '(t wide)) (not ido-keep-item-list))
	  (let ((olist ido-cur-list)
		(oign ido-ignored-list)
		(omat ido-matches)
		(l (ido-make-merged-file-list ido-text-init
					      (eq ido-use-merged-list 'auto)
					      (eq ido-try-merged-list 'wide))))
	    (ido-trace "merged" l)
	    (cond
	     ((not l)
	      (if (eq ido-try-merged-list 'wide)
		  (setq ido-pre-merge-state
			(list "" ido-current-directory olist oign omat)
			ido-cur-list nil
			ido-ignored-list nil
			ido-matches nil
			ido-keep-item-list t
			ido-try-merged-list (if (eq ido-use-merged-list 'auto) 'auto nil)
			ido-use-merged-list nil)
		(setq ido-cur-list olist
		      ido-ignored-list oign
		      ido-matches omat
		      ido-keep-item-list t
		      ido-try-merged-list (if (eq ido-use-merged-list 'auto) 'auto nil)
		      ido-use-merged-list nil)))
	     ((eq l t)
	      (setq ido-use-merged-list nil))
	     ((eq l 'input-pending-p)
	      (setq ido-try-merged-list t
		    ido-use-merged-list nil))
	     (t
	      (setq ido-pre-merge-state
		    (list ido-text-init ido-current-directory olist oign omat))
	      (ido-set-current-directory (car (cdr (car l))))
	      (if (ido-final-slash ido-text-init)
		  (setq ido-text-init ""))
	      (setq ido-cur-list l
		    ido-ignored-list nil
		    ido-matches l
		    ido-rescan nil
		    ido-keep-item-list t
		    ido-use-merged-list t)
	      (ido-trace "Merged" t)
	      ))))

      (cond
       (ido-keep-item-list
	(setq ido-keep-item-list nil
	      ido-rescan nil))
       ((eq ido-cur-item 'file)
	(setq ido-ignored-list nil
	      ido-cur-list (and (not ido-directory-nonreadable)
				(not ido-directory-too-big)
				(ido-make-file-list ido-default-item))))
       ((eq ido-cur-item 'dir)
	(setq ido-ignored-list nil
	      ido-cur-list (and (not ido-directory-nonreadable)
				(not ido-directory-too-big)
				(ido-make-dir-list ido-default-item))))
       ((eq ido-cur-item 'buffer)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-buffer-list ido-default-item)))
       ((eq ido-cur-item 'list)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-choice-list ido-default-item)))
       (t nil))
      (setq ido-rotate-temp nil)

      (if ido-process-ignore-lists-inhibit
	  (setq ido-process-ignore-lists t
		ido-process-ignore-lists-inhibit nil))

      (ido-set-matches)
      (if (and ido-matches (eq ido-try-merged-list 'auto))
	  (setq ido-try-merged-list t))
      (let ((max-mini-window-height (or ido-max-window-height
					(and (boundp 'max-mini-window-height)
					     max-mini-window-height)))
	   (ido-completing-read t)
	   (ido-require-match require-match)
	   (ido-use-mycompletion-depth (1+ (minibuffer-depth)))
	   (show-paren-mode nil)
	   ;; Postpone history adding till later
	   (history-add-new-input nil))
	;; prompt the user for the file name
	(setq ido-exit nil)
	(setq ido-final-text
	      (catch 'ido
		(read-from-minibuffer (ido-make-prompt item prompt)
				      (prog1 ido-text-init
					(setq ido-text-init nil))
				      ido-completion-map nil history))))
      (ido-trace "read-from-minibuffer" ido-final-text)
      (if (get-buffer ido-completion-buffer)
	  (kill-buffer ido-completion-buffer))

      (ido-trace "\n_EXIT_" ido-exit)

      (cond
       ((eq ido-exit 'refresh)
	(if (and (eq ido-use-merged-list 'auto)
		 (or (input-pending-p)))
	    (setq ido-use-merged-list nil
		  ido-keep-item-list t))
	nil)

       ((eq ido-exit 'done)
	(setq done t
	      ido-selected ido-text
	      ido-exit nil))

       ((memq ido-exit '(edit chdir))
	(cond
	 ((memq ido-cur-item '(file dir))
	  (let* ((read-file-name-function nil)
		 (edit (eq ido-exit 'edit))
		 (d ido-current-directory)
		 (f ido-text-init)
		 (new t))
	    (setq ido-text-init "")
	    (while new
	      (setq new (if edit
			    (condition-case nil
				(read-file-name (concat prompt "[EDIT] ")
						(expand-file-name d)
						(concat d f) nil f)
			      (quit (concat d f)))
			   f)
		    d (or (file-name-directory new) "/")
		    f (file-name-nondirectory new)
		    edit t)
	      (if (or
		   (file-directory-p d)
		   (and (yes-or-no-p (format "Create directory %s? " d))
			(condition-case nil
			    (progn (make-directory d t) t)
			  (error
			   (message "Could not create directory")
			   (sit-for 1)
			   nil))))
		  (progn
		    (ido-set-current-directory d nil (eq ido-exit 'chdir))
		    (setq ido-text-init f
			  new nil))))))
	 (t
	  (setq ido-text-init
		(condition-case nil
		    (read-string (concat prompt "[EDIT] ") ido-final-text)
		  (quit ido-final-text)))))

	nil)

       ((eq ido-exit 'keep)
	(setq ido-keep-item-list t))

       ((memq ido-exit '(dired fallback find-file switch-to-buffer insert-buffer insert-file))
	(setq done t))

       ((memq ido-exit '(updir push))
	;; cannot go up if already at the root-dir (Unix) or at the
	;; root-dir of a certain drive (Windows or MS-DOS).
        (if (ido-is-tramp-root)
	    (when (string-match "\\`\\(/\\([^/]+[:@]\\)*\\)\\([^/]+\\)[:@]\\'" ido-current-directory)
	      (setq ido-text-init (match-string 3 ido-current-directory))
	      (ido-set-current-directory (match-string 1 ido-current-directory))
	      (setq ido-set-default-item t))
	  (unless (ido-is-root-directory)
	    (when (eq ido-exit 'push)
	      (setq ido-input-stack (cons (cons ido-cur-item ido-text) ido-input-stack))
	      (setq ido-cur-item 'dir)
	      (setq ido-text-init (file-name-nondirectory (substring ido-current-directory 0 -1)))
	      (ido-trace "push" ido-input-stack))
	    (ido-set-current-directory (file-name-directory (substring ido-current-directory 0 -1)))
	    (setq ido-set-default-item t))))

       ((eq ido-exit 'pop)
	(ido-trace "pop" ido-input-stack)
	(let ((elt (car ido-input-stack)))
	  (setq ido-input-stack (cdr ido-input-stack))
	  (ido-set-current-directory (concat ido-current-directory ido-text))
	  (setq ido-cur-item (car elt))
	  (setq ido-text-init (cdr elt))))

       ((eq ido-exit 'pop-all)
	(ido-trace "pop-all" ido-input-stack)
	(while ido-input-stack
	  (let ((elt (car ido-input-stack)))
	    (setq ido-input-stack (cdr ido-input-stack))
	    (ido-set-current-directory (concat ido-current-directory ido-text))
	    (setq ido-cur-item (car elt))
	    (setq ido-text-init (cdr elt)))))

       ;; Handling the require-match must be done in a better way.
       ((and require-match
	     (not (memq require-match '(confirm confirm-after-completion)))
	     (not (if ido-directory-too-big
		      (file-exists-p (concat ido-current-directory ido-final-text))
		    (ido-existing-item-p))))
	(error "Must specify valid item"))

       (t
	(setq ido-selected
	      (if (or (eq ido-exit 'takeprompt)
		      (null ido-matches))
		  ido-final-text
		;; else take head of list
		(ido-name (car ido-matches))))

	(cond
	 ((memq item '(buffer list))
	  (setq done t))

	 ((string-equal "./" ido-selected)
	  nil)

	 ((string-equal "../" ido-selected)
	  ;; cannot go up if already at the root-dir (Unix) or at the
	  ;; root-dir of a certain drive (Windows or MS-DOS).
	  (or (ido-is-root-directory)
	      (ido-set-current-directory (file-name-directory (substring ido-current-directory 0 -1))))
	  (setq ido-set-default-item t))

	 ((and (string-match (if ido-enable-tramp-completion ".[:@]\\'" ".:\\'") ido-selected)
	       (ido-is-root-directory) ;; Ange-ftp or Tramp
	       (not (ido-local-file-exists-p ido-selected)))
	  (ido-set-current-directory ido-current-directory ido-selected)
	  (ido-trace "tramp prefix" ido-selected)
	  (if (ido-is-slow-ftp-host)
	      (setq ido-exit 'fallback
		    done t)
	    (setq ido-set-default-item t)))

	 ((or (string-match "[/\\][^/\\]" ido-selected)
	      (and (memq system-type '(windows-nt ms-dos))
		   (string-match "\\`[a-zA-Z]:" ido-selected)))
	  (ido-set-current-directory (file-name-directory ido-selected))
	  (setq ido-set-default-item t))

	 ((string-match "\\`~" ido-selected)
	  (ido-set-current-home ido-selected))

	 ((ido-final-slash ido-selected)
	  (if ido-enable-last-directory-history
	      (let ((x (assoc ido-current-directory ido-last-directory-list)))
		(if x
		    (setcdr x ido-selected)
		  (setq ido-last-directory-list
			(cons (cons ido-current-directory ido-selected) ido-last-directory-list)))))
	  (ido-set-current-directory ido-current-directory ido-selected)
	  (if ido-input-stack
	      ; automatically pop stack elements which match existing files or directories
	      (let (elt)
		(while (and (setq elt (car ido-input-stack))
			    (file-exists-p (concat ido-current-directory (cdr elt))))
		  (if (setq ido-input-stack (cdr ido-input-stack))
		      (ido-set-current-directory ido-current-directory (cdr elt))
		    (setq ido-text-init (cdr elt)))
		  (setq ido-cur-item (car elt))))
	    (setq ido-set-default-item t)))

	 (t
	  (setq done t))))))
    (add-to-history (or (if (symbolp history) history (car history))
                        'minibuffer-history)
                    ido-selected)
    ido-selected))

     ))

(eval-after-load 'warnings
  '(progn
     (add-to-list 'warning-suppress-types '(undo discard-info))
     ))

(when (> emacs-major-version 23)
  (electric-pair-mode 1)
  (electric-indent-mode 1))

(blink-cursor-mode -1)

(global-hl-line-mode 1)

(setq user-full-name "William Xu")


(provide 'xwl-convenience)

;;; xwl-convenience.el ends here
