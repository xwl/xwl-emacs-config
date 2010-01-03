;;; xwl-depreated.el --- deprecated stufffs

;; Copyright (C) 2007 William Xu

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
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (autoload 'xwl-depreated "xwl-depreated")

;;; Code:

;; elpa
;; ----

;; (let ((buffer (url-retrieve-synchronously
;; 	       "http://tromey.com/elpa/package-install.el")))
;;   (save-excursion
;;     (set-buffer buffer)
;;     (goto-char (point-min))
;;     (re-search-forward "^$" nil 'move)
;;     (eval-region (point) (point-max))
;;     (kill-buffer (current-buffer))))

;; (require 'package)
;; (package-initialize)

;; Use posting-style is the better way

;; (defun xwl-sendmail-select ()
;;   "Select sendmail methods. You know, some ML doesn't allow
;; sendmail directly from localhost without a valid domain name."
;;   (save-excursion
;;     (let ((to (save-excursion
;;                 (message-narrow-to-headers)
;;                 (or (message-fetch-field "to")
;;                     "")))
;;           (gcc (save-excursion
;;                  (message-narrow-to-headers)
;;                  (or (message-fetch-field "Gcc")
;;                      ""))))
;;       (cond ((string-match "lists.sourceforge.net" to)
;;              (message "Will sendmail by gmail")
;;              (xwl-sendmail-by-gmail))
;;             ((or (string-match ".*abc.net.*" to)
;;                  (catch 'return
;;                    (progn (mapc
;;                            (lambda (el)
;;                              (if (string-match el gcc)
;;                                  (throw 'return t)))
;;                            xwl-ce-groups)
;;                           nil)))
;;              (xwl-sendmail-by-ce))
;;             (t
;;              ;;(xwl-sendmail-by-localhost)
;;              (xwl-sendmail-by-gmail)))
;;       )))

;; (add-hook 'message-setup-hook 'xwl-sendmail-select)

;; send-hook

;; Fix "From" for trac changelogs.
;; (add-hook 'gnus-article-prepare-hook 'xwl-fix-trac-log-author)

(defun xwl-fix-trac-log-author ()
  (cond ((string-match "\\`nnrss:.*Revisions.*" gnus-newsgroup-name)
         (save-excursion
           (goto-char (point-min))
           (unless (re-search-forward "^From:" nil t 1)
             (let ((author "nobody"))
               (re-search-forward "^Author:")
               (forward-line 1)
               (skip-chars-forward "[[:space:]]")
               (setq author (buffer-substring-no-properties
                             (point)
                             (progn (end-of-line)
                                    (skip-chars-backward "[[:space:]]")
                                    (point))))
               (gnus-summary-edit-article 2)
               (goto-char (point-min))
               (insert (format "From: %s <%s@abc.net>\n" author author))
               (let ((gnus-article-prepare-hook nil))
                 (gnus-article-edit-done))))))))


;; ,----
;; | template.el
;; `----

(require 'template)
(setq template-default-directories
      (append '("~/.templates")
              template-default-directories)
      template-date-format "%02m-%02d-%Y %02H:%02M:%02S"
      template-auto-insert t)
(setq template-expansion-alist
      '(("nick_name" (insert "xwl"))))
(template-initialize)

(defadvice template-new-file (around inhibit-less)
  "Inhibit `xwl-toggle-find-file-less' temporarily."
  (let ((less-on? (memq 'less-minor-mode-on find-file-hook)))
    (when less-on?
      (xwl-toggle-find-file-less))
    ad-do-it
    (when less-on?
      (xwl-toggle-find-file-less))))

(ad-activate 'template-new-file)





(load-file "~/repo/cvs/cedet/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)
;; (semantic-load-enable-guady-code-helpers)

;; Show semantic tags in speedbar.
(require 'semantic-sb)



  (require 'speedbar)

  (defconst my-speedbar-buffer-name " SPEEDBAR")
  ; (defconst my-speedbar-buffer-name " SPEEDBAR") ; try this if you get "Wrong type argument: stringp, nil"

  (defun my-speedbar-no-separate-frame ()
    (interactive)
    (when (not (buffer-live-p speedbar-buffer))
      (setq speedbar-buffer (get-buffer-create my-speedbar-buffer-name)
            speedbar-frame (selected-frame)
            dframe-attached-frame (selected-frame)
            speedbar-select-frame-method 'attached
            speedbar-verbosity-level 0
            speedbar-last-selected-file nil)
      (set-buffer speedbar-buffer)
      (speedbar-mode)
      (speedbar-reconfigure-keymaps)
      (speedbar-update-contents)
      (speedbar-set-timer 1)
      (make-local-hook 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook
                (lambda () (when (eq (current-buffer) speedbar-buffer)
                             (setq speedbar-frame nil
                                   dframe-attached-frame nil
                                   speedbar-buffer nil)
                             (speedbar-set-timer nil)))))
    (set-window-buffer (selected-window) 
                       (get-buffer my-speedbar-buffer-name)))




;;; Woman, Bookmark, Perldoc, Webjump, Imenu, etags

;; use ido-completing-read

(require 'woman)
(defun woman-file-name (topic &optional re-cache)
  "Get the name of the UN*X man-page file describing a chosen TOPIC.
When `woman' is called interactively, the word at point may be
automatically used as the topic, if the value of the user option
`woman-use-topic-at-point' is non-nil.  Return nil if no file can
be found.  Optional argument RE-CACHE, if non-nil, forces the
cache to be re-read."
  ;; Handle the caching of the directory and topic lists:
  (if (and (not re-cache)
	   (or
	    (and woman-expanded-directory-path woman-topic-all-completions)
	    (woman-read-directory-cache)))
      ()
    (message "Building list of manual directory expansions...")
    (setq woman-expanded-directory-path
	  (woman-expand-directory-path woman-manpath woman-path))
    (message "Building completion list of all manual topics...")
    (setq woman-topic-all-completions
	  (woman-topic-all-completions woman-expanded-directory-path))
    (woman-write-directory-cache))
  ;; There is a problem in that I want to offer case-insensitive
  ;; completions, but to return only a case-sensitive match.  This
  ;; does not seem to work properly by default, so I re-do the
  ;; completion if necessary.
  (let (files
	(default (current-word)))
    (or (stringp topic)
	(and (if (boundp 'woman-use-topic-at-point)
		 woman-use-topic-at-point
	       ;; Was let-bound when file loaded, so ...
	       (setq woman-use-topic-at-point woman-use-topic-at-point-default))
	     (setq topic (or (current-word t) "")) ; only within or adjacent to word
	     (test-completion topic woman-topic-all-completions))
	(setq topic
	      (let* ((word-at-point (current-word))
		     (default
		       (when (and word-at-point
				  (test-completion
				   word-at-point woman-topic-all-completions))
			 word-at-point)))
		;; (completing-read
                (ido-completing-read
		 (if default
		     (format "Manual entry (default %s): " default)
		   "Manual entry: ")
                 (mapcar (lambda (el) (car el))
                         woman-topic-all-completions)
                 nil 1
		 nil
		 'woman-topic-history
		 default))))
    ;; Note that completing-read always returns a string.
    (if (= (length topic) 0)
	nil				; no topic, so no file!
      (cond
       ((setq files (woman-file-name-all-completions topic)))
       ;; Complete topic more carefully, i.e. use the completion
       ;; rather than the string entered by the user:
       ((setq files (all-completions topic woman-topic-all-completions))
	(while (/= (length topic) (length (car files)))
	  (setq files (cdr files)))
	(setq files (woman-file-name-all-completions (car files)))))
      (cond
       ((null files) nil)		; no file found for topic.
       ((null (cdr files)) (car (car files))) ; only 1 file for topic.
       (t
	;; Multiple files for topic, so must select 1.
	;; Unread the command event (TAB = ?\t = 9) that runs the command
	;; `minibuffer-complete' in order to automatically complete the
	;; minibuffer contents as far as possible.
	(setq unread-command-events '(9)) ; and delete any type-ahead!
	(completing-read "Manual file: " files nil 1
			 (try-completion "" files) 'woman-file-history))))))

(require 'bookmark)
(defun bookmark-completing-read (prompt &optional default)
  "Prompting with PROMPT, read a bookmark name in completion.
PROMPT will get a \": \" stuck on the end no matter what, so you
probably don't want to include one yourself.
Optional second arg DEFAULT is a string to return if the user enters
the empty string."
  (bookmark-maybe-load-default-file)	; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt (bookmark-all-names))
    (let* ((completion-ignore-case bookmark-completion-ignore-case)
	   (default default)
	   (prompt (if default
		       (concat prompt (format " (%s): " default))
		     (concat prompt ": ")))
	   (str
	    (ido-completing-read prompt
                                 (mapcar (lambda (el) (car el))
                                         bookmark-alist)
				 nil
				 0
				 nil
				 'bookmark-history)))
      (if (string-equal "" str) default str))))

(require 'webjump)
(defun webjump ()
  "Jumps to a Web site from a programmable hotlist.

See the documentation for the `webjump-sites' variable for how to customize the
hotlist.

Please submit bug reports and other feedback to the author, Neil W. Van Dyke
<nwv@acm.org>."
  (interactive)
  (let* ((completion-ignore-case t)
	 (item (assoc-string
		(ido-completing-read "WebJump to site: "
                                     (mapcar (lambda (el) (car el))
                                             webjump-sites)
                                     nil
                                     t
                                     ; "Google"
                                     )
		webjump-sites t))
	 (name (car item))
	 (expr (cdr item)))
    (browse-url (webjump-url-fix
		 (cond ((not expr) "")
		       ((stringp expr) expr)
		       ((vectorp expr) (webjump-builtin expr name))
		       ((listp expr) (eval expr))
		       ((symbolp expr)
			(if (fboundp expr)
			    (funcall expr name)
			  (error "WebJump URL function \"%s\" undefined"
				 expr)))
		       (t (error "WebJump URL expression for \"%s\" invalid"
				 name)))))))

;; (defalias 'webjump-url-encode 'emms-lyrics-url-quote)

(require 'imenu)
(defun imenu--completion-buffer (index-alist &optional prompt)
  "Let the user select from INDEX-ALIST in a completion buffer with PROMPT.

Return one of the entries in index-alist or nil."
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
	choice
	(prepared-index-alist
	 (if (not imenu-space-replacement) index-alist
	   (mapcar
	    (lambda (item)
	      (cons (subst-char-in-string ?\s (aref imenu-space-replacement 0)
					  (car item))
		    (cdr item)))
	    index-alist))))
    (when (stringp name)
      (setq name (or (imenu-find-default name prepared-index-alist) name)))
    (cond (prompt)
	  ((and name (imenu--in-alist name prepared-index-alist))
	   (setq prompt (format "Index item (default %s): " name)))
	  (t (setq prompt "Index item: ")))
    (let ((minibuffer-setup-hook minibuffer-setup-hook))
      ;; Display the completion buffer.
      (if (not imenu-eager-completion-buffer)
	  (add-hook 'minibuffer-setup-hook 'minibuffer-completion-help))
      (setq name (ido-completing-read prompt
                                      (mapcar (lambda (el) (car el))
                                              prepared-index-alist)
				  nil t nil 'imenu--history-list name)))

    (when (stringp name)
      (setq choice (assoc name prepared-index-alist))
      (if (imenu--subalist-p choice)
	  (imenu--completion-buffer (cdr choice) prompt)
	choice))))


(require 'etags)

(case emacs-major-version
  ((22)
   (defun find-tag-tag (string)
     (let* ((completion-ignore-case (if (memq tags-case-fold-search '(t nil))
                                        tags-case-fold-search
                                      case-fold-search))
            (default (funcall (or find-tag-default-function
                                  (get major-mode 'find-tag-default-function)
                                  'find-tag-default)))
            (spec (ido-completing-read (if default
                                           (format "%s (default %s): "
                                                   (substring string 0 (string-match "[ :]+\\'" string))
                                                   default)
                                         string)
                                       (funcall 'tags-complete-tag "" nil t)
                                       nil nil nil nil default)))
       (if (equal spec "")
           (or default (error "There is no default tag"))
         spec))))
  (t
   (defun find-tag-tag (string)
     "Read a tag name, with defaulting and completion."
     (let* ((completion-ignore-case (if (memq tags-case-fold-search '(t nil))
                                        tags-case-fold-search
                                      case-fold-search))
            (default (funcall (or find-tag-default-function
                                  (get major-mode 'find-tag-default-function)
                                  'find-tag-default)))
            (spec (completing-read (if default
                                       (format "%s (default %s): "
                                               (substring string 0 (string-match "[ :]+\\'" string))
                                               default)
                                     string)
                                   (tags-lazy-completion-table)
                                   nil nil nil nil default)))
       (if (equal spec "")
           (or default (error "There is no default tag"))
         spec)))))



;; todo

(setq todo-file-do   "~/.todo/do"
      todo-file-done "~/.todo/done"
      todo-file-top  "~/.todo/top")

(defun xwl-todo-find-do ()
  "Visit `todo-file-do' by turning off `less-minor-mode'."
  (interactive)
  (find-file todo-file-do)
  (less-minor-mode -1))

(defun xwl-todo-forward-category ()
  "Move forward category in current todo buffer."
  (interactive)
  (end-of-line)
  (search-forward-regexp (regexp-opt '("*/* --- ")) nil t)
  (beginning-of-line))

(defun xwl-todo-backward-category ()
  "Move backward category in current todo buffer."
  (interactive)
  (beginning-of-line)
  (search-backward-regexp (regexp-opt '("*/* --- ")) nil t)
  (beginning-of-line))

(defun xwl-todo-mode-hook ()
  (highlight-regexp (concat "^" (regexp-opt '("*/* --- ")) ".*")
                    'font-lock-constant-face)
  (local-set-key (kbd "a") 'todo-add-category)
  (local-set-key (kbd "i") 'todo-insert-item-here)
  (local-set-key (kbd "I") 'todo-insert-item)
  (local-set-key (kbd "e") 'todo-edit-item)
  (local-set-key (kbd "D") 'todo-delete-item)
  (local-set-key (kbd "=") 'todo-forward-category)
  (local-set-key (kbd "q") 'next-buffer)
  (local-set-key (kbd "^")
                 (lambda () (interactive)
                   (todo-quit)
                   (xwl-todo-find-do)
                   (goto-char (point-min))))
  (local-set-key (kbd "N") 'xwl-todo-forward-category)
  (local-set-key (kbd "P") 'xwl-todo-backward-category))

(add-hook 'todo-mode-hook 'xwl-todo-mode-hook)

;; deal with large buffer

(setq font-lock-support-mode 'fast-lock-mode)

(setq font-lock-support-mode 'lazy-lock-mode
      lazy-lock-defer-on-scrolling t
      font-lock-maximum-decoration t)

(setq font-lock-maximum-size
      '((c-mode     . 256000)
	(c++-mode   . 256000)
	(rmail-mode . 1048576)
	(tex-mode   . 1048576)))



;; Chinese
;; -------

;; FIXME: this doesn't work under mac?
;;(require 'chinese-gbk)
;;(when window-system
;;    (require 'characters-gbk)
;;    (require 'fontset-gbk)
    ;; use Chinese instead of Japanese charsets to decode utf-8
    ;;   (require 'gbk-utf-mode)
    ;;   (utf-translate-cjk-mode 1))
;;    )

;; (when (string= (substring emacs-version 0 2) "21")
;;   (set-terminal-coding-system 'chinese-gbk)
;;   (set-keyboard-coding-system 'chinese-gbk))


;; enable tab completion for `shell-command'.
;; (setq shell-command-enable-completions t)
;; (shell-command-activate-advices)
;; (setq shell-command-prompt "%w%$ ")

(global-set-key (kbd "C-x C--") 'redo)

(defvar xwl-emacs-start-time (current-time))

(defun xwl-emacs-uptime ()
  (interactive)
  (let* ((diff (time-subtract (current-time) xwl-emacs-start-time))
         (str (format "Emacs started at %s, more than %d days(= %.2f hours) ago"
                      (format-time-string "%Y/%m/%d %H:%M:%S" xwl-emacs-start-time)
                      (time-to-number-of-days diff)
                      (/ (time-to-seconds diff) 3600.0))))
    (message str)))

(define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
(define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
(define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
(define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)


(autoload 'todoo "todoo" "TODO Mode" t)
(add-to-list 'auto-mode-alist '("TODO$" . todoo-mode))


;; (eval-after-load "tex"
;;   '(progn
(require 'tex)
(defun TeX-insert-macro (symbol)
  "Insert TeX macro SYMBOL with completion.

AUCTeX knows of some macros and may query for extra arguments, depending on
the value of `TeX-insert-macro-default-style' and whether `TeX-insert-macro'
is called with \\[universal-argument]."
  ;; When called with a prefix (C-u), only ask for mandatory arguments,
  ;; i.e. all optional arguments are skipped.  See `TeX-parse-arguments' for
  ;; details.  Note that this behavior may be changed in favor of a more
  ;; flexible solution in the future, therefore we don't document it at the
  ;; moment.
  (interactive (list (ido-completing-read (concat "Macro (default "
                                                  TeX-default-macro
                                                  "): "
                                                  TeX-esc)
                                          (mapcar (lambda (i) (car i))
                                                  (TeX-symbol-list)) nil nil nil
                                          'TeX-macro-history)))
  (cond ((string-equal symbol "")
         (setq symbol TeX-default-macro))
        ((interactive-p)
         (setq TeX-default-macro symbol)))
  (TeX-parse-macro symbol (cdr-safe (assoc symbol (TeX-symbol-list)))))

))


(setq xwl-wgetpaste-command 
      (if (eq system-type 'windows-nt)
          ;; "c:/cygwin/bin/wget.exe"
          "SET WGETRC=c:/Documents and Settings/wixu/.wgetrc; wget-real.exe"
        "wget"))

(defun xwl-wgetpaste-ubuntu-cn (beg end &optional class)
  (interactive "r")
  (unless class
    (if current-prefix-arg
        (setq class (ido-completing-read "Use mode: " xwl-wgetpaste-ubuntu-cn-classes))
      (setq class "lisp")))
  (let ((content (buffer-substring-no-properties beg end))
        (data-file "wget.post")
        (host "http://paste.ubuntu.org.cn"))
    ;; 1. Prepare content
    (with-temp-buffer 
      (insert
       (format "poster=%s&class=%s&paste=1&code2=%s"
               xwl-wgetpaste-username class (xwl-wgetpaste-escape content)))
      (write-file data-file))
    ;; 2. Send it
    ;; (cmd (format "%s --post-data=\"poster=%s&class=%s&paste=1&code2=%s\" http://paste.ubuntu.org.cn -P /tmp"
    (let* ((cmd (format "%s --post-file=%s %s -P /tmp"
                        xwl-wgetpaste-command data-file host))
           (ret (concat host 
                        "/"
                        (replace-regexp-in-string
                         ".*Saving to: `[^0-9]*\\([0-9]\\{4,\\}\\)'.*"
                         "\\1"
                         (replace-regexp-in-string
                          "\n" " " (shell-command-to-string cmd))))))
      (when mark-active
        (deactivate-mark))
      (delete-file data-file)
      (kill-new ret)
      (message "%s" ret))))

(defun xwl-wgetpaste-escape (string)
  "Escape special characters used in wget."
  (let ((ret (replace-regexp-in-string "%" "%25" string)))
    (mapc (lambda (i)
            (setq ret (replace-regexp-in-string (car i) (cdr i) ret)))
          '(("\\\\" . "%5c")
            ("\`" . "%60")
            ("\"" . "%22")
            ("\\+" . "%2b")
            ("&" . "%26")))
    (setq ret (replace-regexp-in-string " " "+" ret))))


(if (eq window-system 'mac)
    (progn
      (require 'carbon-font)
      (require 'fixed-width-fontset)

      (setq xwl-font-encode-family-list
            `((japanese-jisx0208 . ,xwl-japanese-font)
              (katakana-jisx0201 . ,xwl-japanese-font)
              (japanese-jisx0212 . ,xwl-japanese-font)
              (chinese-gb2312 . ,xwl-chinese-font)
              (chinese-gbk . ,xwl-chinese-font)
              (gb18030 . ,xwl-chinese-font)
              (chinese-big5-1 . ,xwl-chinese-font)
              (chinese-big5-2 . ,xwl-chinese-font)))

      (fixed-width-create-fontset
       "xwl" '(12 7 8 9 10 14 16 18 20 24) xwl-font-encode-family-list)

      (setq xwl-fontset-name "-*-*-medium-r-normal--14-*-*-*-*-*-fontset-xwl")
      (set-default-font xwl-fontset-name)

      (setq default-frame-alist
            `((font . ,xwl-fontset-name)
              (width . 180)
              (height . 40)
              ,@default-frame-alist)))

  )


;;;; edict
;; -------

;; FIXME
;; (require 'edict)
(defun edict-display (key-list match-list)
  "Edict-display displayes the strings in a separate window that is
not selected."
  (let* ((text-window (get-buffer-window (current-buffer)))
	 (edict-window (get-buffer-window edict-match-buffer))
	 ;; We have available some of this window's height plus any we've already
	 ;; already gotten.
	 (avail-height (+ (window-height text-window)
			  (if edict-window
			      (window-height edict-window)
			    0)))
	 ;; We limit the height to half of what's available, but no more than we need,
	 ;; and no less than window-min-height.  We must remember to include 1 line for
	 ;; the mode-line in our minimum figure.
	 (height (min (max window-min-height (+ (length match-list) 1))
		      (/ avail-height 2)))
         (transpose-window-p (not edict-window)))
    (if (not edict-window)
	(progn
	  ;; We don't have a window, so remember our existing configuration,
	  ;; and either find an acceptable window to split, or use the current
	  ;; window.
	  (edict-note-windows)
	  (let ((use-window (edict-find-acceptable-window text-window)))
	    (if use-window
		(setq edict-window use-window
		      text-window (split-window)) ; text-window height))
	      (setq edict-window text-window))))
      ;; We have a window already.  Just adjust its size appropriately.
      ;; (unless (equal height (window-height edict-window))
	(let ((selected (selected-window)))
	  (select-window edict-window)
	  ;; (enlarge-window (- height (window-height edict-window)))
	  (select-window selected)))
        ;; )
    (set-buffer edict-match-buffer)
    (let ((min (point-min))
          (inhibit-read-only t))
      ;; Replace everything.
      (erase-buffer)
      (mapcar (function (lambda (string-item)
			  (insert string-item)
			  (newline)))
	      match-list)
      (when (eq *edict-window-location* 'bottom)
	(let ((w text-window))
	  (setq text-window edict-window
		edict-window w)))
      ;; OK, now let's move the exact matches to the top.
      (goto-char min)
      ;; Be careful to preserve the order.
      ;; An exact match is any of "^key ", "[key]", "/key/", or "/to key/".
      (dolist (key (reverse key-list))
	(let* ((pattern (concat "^" key " \\|\\[" key "\\]\\|\\/" key
				"\\/\\|\\/to " key "\\/" ))
	       (top-lines nil))
	  ;; First pull them out of the buffer into a list (top-lines).
	  ;; Then re-insert them at the top.
	  (while (re-search-forward pattern nil t)
	    (forward-line 0)
	    (let ((p (point)))
	      (forward-line 1)
	      (push (buffer-substring p (point)) top-lines)
	      (delete-region p (point))))
	  (goto-char min)
	  (mapcar 'insert top-lines)))
      ;; OK, display it all.
      (set-window-buffer edict-window edict-match-buffer)
      (set-window-start edict-window min)
      (select-window edict-window)
      (when transpose-window-p
        (his-transpose-windows 1))))
  t)

;; (global-set-key (kbd "M-S") 'xwl-edict-search-kanji)

(defun xwl-edict-search-kanji (word)
  "Search the word at point when given.
It presents the word at point as default input and allows editing
it."
  (interactive
   (list (read-string "Search kanji: " (current-word))))
  (unless word
    (setq word (read-string "Search kanji: ")))
  (edict-init)
  (edict-search-and-display (edict-clean-up-kanji word)
                            '日本語))


(when (= emacs-major-version 22)
  (eval-after-load "vc"
    '(progn
       (defun vc-print-log (&optional focus-rev)
         "List the change log of the current buffer in a window.
If FOCUS-REV is non-nil, leave the point at that revision."
         (interactive)
         (vc-ensure-vc-buffer)
         (let ((file buffer-file-name))
           (or focus-rev (setq focus-rev (vc-workfile-version file)))
           ;; Don't switch to the output buffer before running the command,
           ;; so that any buffer-local settings in the vc-controlled
           ;; buffer can be accessed by the command.
           (condition-case err
               (progn
                 (vc-call print-log file "*vc-change-log*")
                 (set-buffer "*vc-change-log*"))
             (wrong-number-of-arguments
              ;; If this error came from the above call to print-log, try again
              ;; without the optional buffer argument (for backward compatibility).
              ;; Otherwise, resignal.
              (if (or (not (eq (cadr err)
                               (indirect-function
                                (vc-find-backend-function (vc-backend file)
                                                          'print-log))))
                      (not (eq (caddr err) 2)))
                  (signal (car err) (cdr err))
                ;; for backward compatibility
                (vc-call print-log file)
                (set-buffer "*vc*"))))
           (pop-to-buffer (current-buffer))
           (vc-exec-after
            `(let ((inhibit-read-only t))
               (vc-call-backend ',(vc-backend file) 'log-view-mode)
               (goto-char (point-max)) (forward-line -1)
               (while (looking-at "=*\n")
                 (delete-char (- (match-end 0) (match-beginning 0)))
                 (forward-line -1))
               (goto-char (point-min))
               (if (looking-at "[\b\t\n\v\f\r ]+")
                   (delete-char (- (match-end 0) (match-beginning 0))))
               ;; (shrink-window-if-larger-than-buffer)
               ;; move point to the log entry for the current version
               (vc-call-backend ',(vc-backend file)
                                'show-log-entry
                                ',focus-rev)
               (set-buffer-modified-p nil)
               (change-log-mode)
               (less-minor-mode-on)
               (goto-char (point-min)))))))))

;;; xwl-depreated.el ends here

