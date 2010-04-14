;;; xwl-misc.el --- miscellaneous

;; Copyright (C) 2007, 2008, 2009, 2010 William Xu

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

;;; Chinese Wubi Input Method

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

(global-set-key (kbd "C-SPC") 'toggle-input-method)
(global-set-key (kbd "C-/") 'toggle-input-method)
(global-set-key (kbd "C-?") 'xwl-cycle-input-method)

;; (global-set-key (kbd "C-?") 'xwl-cycle-input-method)
(global-set-key (kbd "C-,") 'wubi-toggle-quanjiao-banjiao)
(global-set-key (kbd "C-c m W") 'wubi-load-local-phrases)

;;; BBDB

;; bbdb TODO
(ignore-errors
  (progn
(require 'bbdb)
(bbdb-initialize)

(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-user-mail-names
      (regexp-opt '("willam.xwl@gmail.com"
                    "willam.xwl@hotmail.com")))
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)

(setq bbdb-complete-name-allow-cycling t
      bbdb-use-pop-up nil
      bbdb-file-coding-system 'utf-8
      ;; bbdb-quiet-about-name-mismatches t
      )

(define-key bbdb-mode-map (kbd "TAB") 'bbdb-toggle-records-display-layout)

(defun xwl-bbdb ()
  (interactive)
  (if (get-buffer "*BBDB*")
      (switch-to-buffer "*BBDB*")
    (bbdb "" nil)
    (other-window 1)
    (delete-other-windows)))

(defun my-bbdb-search (field str)
  "Search records whose FIELD matches STR."
  (interactive
   (list
    (ido-completing-read
     "Search: "
     (append (mapcar (lambda (el) (car el)) (bbdb-propnames))
             '("name")))
    (read-string "Match: ")))
  (if (string= field "name")
      (bbdb-name str nil)
    (let ((matches '())
          (case-fold-search bbdb-case-fold-search)
          (invert (bbdb-search-invert-p)))
      (when (stringp field)
        (setq field (intern field)))
      (mapc
       (lambda (record)
         (condition-case nil
             (let ((matchedp
                    (string-match
                     str
                     (cdr (assoc field (bbdb-record-raw-notes record))))))
               (when (or (and (not invert) matchedp)
                         (and invert (not matchedp)))
                 (setq matches (append matches (list record)))))
           (error nil)))
       (bbdb-records))
      (bbdb-display-records matches))))

(defun my-bbdb-create (name)
  "Add a new entry to the bbdb database.

This is different from `bbdb-create', where `my-bbdb-create' only
prompts for name field."
  (interactive "sName: ")
  (let ((record
         (vector name "" nil nil nil nil nil nil
                 (make-vector bbdb-cache-length nil))))
    (bbdb-invoke-hook 'bbdb-create-hook record)
    (bbdb-change-record record t)
    (bbdb-display-records (list record))))

(defun my-bbdb-display-all ()
  (interactive)
  (bbdb-display-records (bbdb-records)))

(define-key bbdb-mode-map (kbd "a") 'my-bbdb-display-all)
(define-key bbdb-mode-map (kbd "\/") 'my-bbdb-search)

))


;;; Misc misc

(global-set-key (kbd "C-c b r") 'boxquote-region)
(global-set-key (kbd "C-c b t") 'boxquote-title)
(global-set-key (kbd "C-c b f") 'boxquote-describe-function)
(global-set-key (kbd "C-c b v") 'boxquote-describe-variable)
(global-set-key (kbd "C-c b k") 'boxquote-describe-key)
(global-set-key (kbd "C-c b !") 'boxquote-shell-command)

(global-set-key (kbd "C-c m p") 'pack-windows)

;; repair some missed keys on console
;; (require 'wx-key)

;; (require 'nuke-trailing-whitespace)
(defun nuke-trailing-whitespace-check-mode ()
  "Redefinition. Generally don't bother me!"
  (not (memq major-mode
             nuke-trailing-whitespace-never-major-modes)))

;; (unless xwl-at-company?
;;   (add-hook 'write-file-functions 'xwl-write-file-functions))

(add-hook 'wordnet-mode-hook 'less-minor-mode-on)


;; qterm
(setq qterm-faces '(""
                    ;; "朗声" "鬼叫" "喃喃" "轻声" "一声大喝" "大叫"
                    ;; "柔柔" "大吼"
                    ))

(setq qterm-signature
      '(lambda ()
         (format "\n\n%s--\n%s%s"
                 ansit-color-magenta
                 (ansi-color-filter-apply
                  (shell-command-to-string "fortune"))
                 ansit-color-close)))

;; (add-to-list 'auto-mode-alist '("todo" . easy-todo-mode))

(eval-after-load 'doc-view
  (add-hook 'doc-view-mode-hook 'less-minor-mode-on))

;; (load "/sw/share/emacs/site-lisp/ledger/ledger.el")

;; (global-set-key (kbd "<f4>") 'cwit)
;; (setq cwit-update-interval 120)
;; (setq cwit-use-local t)


(add-hook 'dashboard-mode-hook 'less-minor-mode-on)

;; (setq tramp-verbose 10)
;; "M-x tramp-submit-bug".
;; (setq tramp-debug-buffer t)
(ignore-errors
  (progn

(require 'mmm-auto)
(require 'mmm-sample)
(setq mmm-global-mode 'maybe)

(setq mmm-classes-alist
      `((text-html :submode html-mode
                   :front "<html>"
                   :front-offset (beginning-of-line 0)
                   :back "</html>"
                   :back-offset (end-of-line 1)
                   :face mmm-code-submode-face)

        (c-in-scheme :submode c-mode    ; in chicken scheme
                     :front "#>"
                     :front-offset (beginning-of-line 0)
                     :back "<#"
                     :back-offset (end-of-line 1)
                     :face mmm-code-submode-face)

        ,@mmm-classes-alist))

(setq mmm-mode-ext-classes-alist
      '((message-mode nil text-html)
        (gnus-article-edit-mode nil text-html)
        (text-mode nil text-html)
        ;; (scheme-mode nil c-in-scheme)
        ))

;; (setq mmm-global-classes
;;       (append '(text-html)
;;               mmm-global-classes))

(defun xwl-mmm-refresh ()
  "Re-apply mmm-mode when buffer contents have been modified."
  (when (and mmm-mode (buffer-modified-p))
    (mmm-apply-all)))

(add-hook 'post-command-hook 'xwl-mmm-refresh)

))

;; wikipedia-mode
(add-to-list 'auto-mode-alist '(".wikipedia" . wikipedia-mode))
(add-hook 'wikipedia-mode-hook 'auto-fill-mode)
(add-hook 'wikipedia-mode-hook (lambda ()
                                 (local-unset-key (kbd "M-n"))
                                 (local-unset-key (kbd "M-p"))
                                 (local-unset-key (kbd "C-c r"))
                                 ))

(defadvice find-file-noselect (around config-drag-n-drop activate)
  "配置文件 drag-n-drop 行为。"
  (let ((file (ad-get-arg 0)))

    ;; 添加音乐、视频文件至 EMMS
    (when (featurep 'emms)
      (when (string-match
             (emms-player-get emms-player-mplayer 'regex)
             ;; Let's skip URL here, or if may confuse browser-url-emacs.
             (replace-regexp-in-string "^\\(http\\|mms\\)" ""  file))
        (emms-add-file file)
        (unless emms-player-playing-p
          (with-current-emms-playlist
            (goto-char (point-max))
            (forward-line -1)
            (emms-playlist-mode-play-smart)))
        (error (format "File `%s' opened in EMMS" file))))

    ;; 粘贴邮件附件
    (save-excursion
      (goto-char (point-max))           ; for handling mmm-mode
      (when (eq major-mode 'message-mode)
        (insert
         (format
          "<#part type=\"%s\" filename=\"%s\" disposition=attachment>
<#/part>"
          (mm-default-file-encoding file)
          file))
        (error (format "File `%s' attached" file))))

    ad-do-it))


;; ,----
;; | file hooks
;; `----

(setq xwl-sensitive-files
      (mapcar 'expand-file-name
              '("/Users/william/notes/todo.org"
                "~/notes/life_blog")))

(defun xwl-find-file-hook ()
  (let ((file (expand-file-name (buffer-file-name))))
    ;; ;; gpg todo
    ;; (when (and (xs-find-first (lambda (f)
    ;;                             (string= file f))
    ;;                           xwl-sensitive-files)
    ;;            (xwl-has-pgg-header-p))
    ;;   (pgg-decrypt)
    ;;   (write-file file)
    ;;   ;; Apply local file var, etc.
    ;;   (revert-buffer))
    ))

(defun xwl-has-pgg-header-p ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (mapconcat 'identity pgg-armor-header-lines "\\|")
     nil t 1)))

(add-hook 'find-file-hook 'xwl-find-file-hook)

(defun xwl-write-file-functions ()
  (let ((f (buffer-file-name)))
    ;; (when (string= (file-name-nondirectory file) "todo.org")
    ;;   (xwl-pgg-encrypt))

    ;; Function should return nil when success.
    ;; (defun xwl-write-file-functions ()
    ;;   (xwl-update-date)

    (when (string-match (regexp-opt (list (file-truename xwl-site-lisp))) f)
      (copyright-update)

      (unless (and (boundp 'qterm-log-file)
                   (string= (file-truename qterm-log-file) f))
        (nuke-trailing-whitespace)))

    ;; should return nil
    nil))

(add-hook 'write-file-functions 'xwl-write-file-functions)

(defun xwl-kill-buffer-hook ()
  (let ((file (buffer-file-name)))
    ;; ;; pgp
    ;; (when (and file
    ;;            (file-exists-p file)
    ;;            (xs-find-first (lambda (f)
    ;;                             (string= file f))
    ;;                           xwl-sensitive-files)
    ;;            (not (xwl-has-pgg-header-p)))
    ;;   (let ((inhibit-read-only t))
    ;;     (pgg-encrypt '("weilin"))
    ;;     (write-file file)))
    ))

(add-hook 'kill-buffer-hook 'xwl-kill-buffer-hook)

;; FIXME: Do not eval-after-load, it will cause tramp recursive loading. Bug?
(require 'tramp)
(when (boundp 'tramp-default-proxies-alist)
  (add-to-list 'tramp-default-proxies-alist
               '("localhost" "\\`root\\'" "/ssh:%h#2222:")))

(add-hook 'ga-mode-hook 'less-minor-mode-on)

(setq ga-pkgsrc-dir "~/repo/cvs/pkgsrc")

(global-set-key (kbd "<f10>") 'ga)

(setq ga-backend-methods
      '((apt-get ;; "ssh william@localhost -p 2222 sudo apt-get")
         "sudo apt-get")
        (fink "sudo fink")
        (pkgsrc "sudo")
        (apt-cyg "c:/cygwin/bin/sh.exe '/home/william/w32/apt-cyg'")
        (yum "sudo yum")))

(add-to-list 'auto-mode-alist
             '("macbluetelnet.*\\(\\.h\\|\\.mm\\|\\.m\\)$" . objc-mode))

(add-to-list 'auto-mode-alist '("\\.info$" . finkinfo-mode))

;; (remove-hook 'find-file-hook 'bracketphobia-hide)

(defun xwl-after-init-hook ()
  ;; maximize frame
  (case window-system
    ((mac)
     (require 'maxframe)
     (setq mf-display-padding-width 0)
     (setq mf-display-padding-height (- mf-display-padding-height 10))

     (setq mf-max-width 1900)

     ;; (add-hook 'window-setup-hook 'maximize-frame t)
     ;; (maximize-frame)
     )
    ((w32)
     (w32-send-sys-command #xf030)))

  (unless noninteractive
    ;; (shell-command "sudo ~/bin/.xwl-after-start-hook")
    ;; (setq display-time-mail-file 'no-check)

    ;; On w32: `emacsclient.exe --server-file c:\repo\xwl-emacs-environment\.emacs.d\server\server -n %*'
    (ignore-errors (server-start))

    (when (executable-find "fortune-zh")
      (setq xwl-idle-timer
            (run-with-idle-timer 300 t 'xwl-run-when-idle-hook)))

    ;; EMMS
    ;; (emms-add-directory-tree emms-source-file-default-directory)
    ;; (emms-playlist-sort-by-score)
    ;; (xwl-erc-select)
    (unless (xwl-check-holidays)
      (find-file "~/.scratch")
      ;; (xwl-todo-find-do)
      (delete-other-windows)
      (message (substring (emacs-version) 0 16)))
    ;; (run-with-timer 0 86400 'xwl-running-daily) ; dialy stuffs
    ;; (xwl-weather-update)

    ;; Run this as the last step.
    ;; (run-at-time 3
    ;;              nil
    ;;              '(lambda ()
    (when (fboundp 'color-theme-xwl-console)
      (color-theme-xwl-console))

    ;; FIXME: how to set this only after window has been maximized?
    (run-at-time 5
                 nil
                 '(lambda ()
                    ;; (add-hook 'after-make-frame-functions
                    ;;           (lambda ()
                    (let ((col (- (round (/ (frame-width) 2)) 2)))
                      (setq erc-fill-column (- col 6)) ; 6 for leading timestamp.
                      (setq twittering-fill-column col))))

    (when window-system
      (require 'highlight-tail)
      (setq highlight-tail-colors  '(("#bc2525" . 0)))
      ;; '(("#d8971d" . 0)))
      (highlight-tail-reload))
    ;; ))
    ))

(add-hook 'after-init-hook 'xwl-after-init-hook)

(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)

(eval-after-load 'file-template
  '(progn
     (setq file-template-mapping-alist
           (append file-template-mapping-alist
                   '(("\\.texinfo$" . "template.texinfo"))))))

(savehist-mode 1)

(eval-after-load 'scroll-all
  '(progn
     (defun less-scroll-up-line (&optional dummy)
       "Scroll up one line."
       (interactive)
       (scroll-up 1))

     (defun less-scroll-down-line  (&optional dummy)
       "Scroll down one line."
       (interactive)
       (scroll-down 1))

     (defun scroll-all-check-to-scroll ()
       "Check `this-command' to see if a scroll is to be done."
       (when (memq this-command
                   '(next-line previous-line scroll-up scroll-down
                               beginning-of-buffer end-of-buffer
                               less-scroll-up-line
                               less-scroll-down-line
                               ))
         (scroll-all-function-all this-command nil)))

     ))

(add-hook 'kill-emacs-hook
          (lambda ()
            (let ((default-directory "~/.emacs.d/site-lisp/"))
              (xwl-makefile-byte-compile))))

;; (autoload 'typing-of-emacs "The Typing Of Emacs, a game." t)

;; (require 'faith)

;; (require 'page-break)
;; (turn-on-page-break-mode)

(setq image-file-name-regexps
      (mapcar (lambda (el)
                (concat "\\." el "$"))
              '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm"
                "pbm" "pgm" "ppm" "pnm")))

(setq xwl-run-when-idle-hook nil)  ; Functions to run when Emacs is idle.
(add-hook 'xwl-run-when-idle-hook 'xwl-fortune-of-the-day)

(eval-after-load 'image-mode
  '(progn
     (define-key image-mode-map (kbd "l") 'image-forward-hscroll)
     (define-key image-mode-map (kbd "h") 'image-backward-hscroll)
     ))

;; ,----
;; | battery
;; `----

(require 'battery)
(setq battery-mode-line-format "[%b%p%% %t]")

(when (fboundp 'battery-status-function)
  (setq battery-mode-line-string
        (battery-format battery-mode-line-format
                        (funcall battery-status-function)))

  (defadvice battery-update-handler (around check-on-line)
    "If on-line (not using battery), don't display on mode line."
    (if (>=  (string-to-number
              (battery-format "%p" (funcall battery-status-function)))
             90)
        (progn
          (setq battery-mode-line-string "")
          (force-mode-line-update))
      ad-do-it))

  (ad-activate 'battery-update-handler)

  (run-with-timer 0 battery-update-interval 'battery-update-handler))

;; ,----
;; | occur
;; `----

(defun xwl-occur-previous-line ()
  (interactive)
  (previous-line 1)
  (occur-mode-goto-occurrence)
  (other-window 1))

(defun xwl-occur-next-line ()
  (interactive)
  (next-line 1)
  (occur-mode-goto-occurrence)
  (other-window 1))

;; (eval-after-load 'replace
;;   '(progn
(define-key occur-mode-map (kbd "p") 'xwl-occur-previous-line)
(define-key occur-mode-map (kbd "n") 'xwl-occur-next-line)
(define-key occur-mode-map (kbd "M-p") 'less-scroll-down-line)
(define-key occur-mode-map (kbd "M-n") 'less-scroll-up-line)
(define-key occur-mode-map (kbd "q") 'xwl-hide-buffer)
;; ))

;; ,----
;; | Window
;; `----

;; redo and undo
(winner-mode 1)
(global-set-key (kbd "C-c <") 'winner-undo)
(global-set-key (kbd "C-c >") 'winner-redo)

;; jump by name
;; (require 'winring)
;; (winring-initialize)

;; (setq winring-show-names nil)
;; (define-key winring-map "n" 'winring-next-configuration)
;; (define-key winring-map "o" 'winring-new-configuration)

;; jump by registers
;; C-x r w
;; C-x r j

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ,----
;; | sawfish
;; `----

; (load-file "/usr/share/emacs/site-lisp/sawfish/sawfish.el") ; oops
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(defun xwl-sawfish-mode-hook ()
  (local-set-key (kbd "C-c <f1>  i")   'sawfish-info)
  (local-set-key (kbd "C-c <f1>  C-v") 'sawfish-info-variable)
  (local-set-key (kbd "C-c <f1>  v")   'sawfish-describe-variable)
  (local-set-key (kbd "C-c <f1>  C-f") 'sawfish-info-function)
  (local-set-key (kbd "C-c <f1>  f")   'sawfish-describe-function)
  (local-set-key (kbd "C-c <f1>  a")   'sawfish-apropos)

  (local-set-key (kbd "ESC TAB") 'sawfish-complete-symbol))

(add-hook 'sawfish-mode-hook 'xwl-sawfish-mode-hook)

;; ,----
;; | console key: C-up, C-down, etc.
;; `----

;; (unless window-system
;;   (load "linux")
;;   (load "kn-prefix")
;;   (load "kn-prefix-autoloads"))

;; ,----
;; | misc
;; `----

;; fvwm-mode
(autoload 'fvwm-mode "fvwm-mode" "Mode for editing fvwm files" t)

;; visual blank, tab, end-of-line ?
;(require 'blank-mode)

;; mmm-mode
;; (set-face-background 'mmm-code-submode-face nil)
;; (set-face-background 'mmm-default-submode-face nil)

;; fortune
(defun xwl-fortune-of-the-day ()
  "$ fortune-zh"
  (interactive)
  (message
   (ansi-color-filter-apply
    (shell-command-to-string "fortune-zh"))))

(global-set-key (kbd "C-c m f") 'xwl-fortune-of-the-day)

;; octave
(autoload 'octave-help "octave-hlp" nil t)

;; ,----
;; | ispell, flyspell
;; `----

(setq ispell-alternate-dictionary "/usr/share/dict/words")
;;      ispell-personal-dictionary "/home/william/.ispell_william")

;; (mapcar* '(lambda (hook) (add-hook hook 'ispell-message))
;; 	 '(
;; 	   message-send-hook
;; 	   mail-send-hook
;; 	   mh-before-send-letter-hook
;; 	   ))

;; ispell-program-name, looking for aspell,ispell at startup!

;; RSS: newsticker

;; (setq newsticker-url-list-defaults
;;       '(("douban-tokyo-love-story" "http://www.douban.com/feed/group/11197/discussion")
;; 	("newsmth-blog" "http://www.newsmth.com/pc/rssrec.php")))

;; (global-set-key (kbd "C-c m n") 'newsticker-show-news)

;; (add-hook 'newsticker-mode-hook 'less-minor-mode-on)

;; (setq inhibit-eol-conversion nil)


;; ,----
;; | pgg
;; `----

(require 'pgg)

(defun pgg-encrypt (rcpts &optional sign start end passphrase)
  "(Redefined) Encrypt the current buffer for RCPTS.

If optional argument SIGN is non-nil, do a combined sign and encrypt.

If optional arguments START and END are specified, only encrypt within
the region.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."

  (interactive (list (split-string (read-string "Recipients: ") "[ \t,]+")))
  (let* ((start (or start (point-min)))
         (end (or end (point-max)))
         (status (pgg-encrypt-region start end rcpts sign passphrase)))
    ;; (when (interactive-p) ; xwl
    (pgg-display-output-buffer start end status) ; )
    status))

(defun pgg-decrypt (&optional start end passphrase)
  "(Redefined) Decrypt the current buffer.

If optional arguments START and END are specified, only decrypt within
the region.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (interactive "")
  (let* ((start (or start (point-min)))
         (end (or end (point-max)))
         (status (pgg-decrypt-region start end passphrase)))
    ;; (when (interactive-p) ; xwl
    (pgg-display-output-buffer start end status) ;)
    status))

(defun xs-find-first (predicate sequence)
  "Find first element of SEQUENCE that satifies PREDICATE."
  (let ((ret nil)
        (s sequence)
        i)
    (while s
      (setq i (car s)
            s (cdr s))
      (when (funcall predicate i)
        (setq ret i
              s nil)))
    ret))

;; ,----
;; | auto-complete
;; `----

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode 1)
(setq ac-auto-start 3)

(defun auto-complete-mode-maybe ()
  "What buffer `auto-complete-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           ;; xwl: Enable for all mode.
           ;; (memq major-mode ac-modes)
           )
      (auto-complete-mode 1)))

(eval-after-load 'auto-complete
  '(progn
     (define-key ac-completing-map "\M-n" 'ac-next)
     (define-key ac-completing-map "\M-p" 'ac-previous)
     ))

(add-hook 'log-edit-mode-hook (lambda () (smart-operator-mode -1)))

(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;; twittering-mode
(setq twittering-username "xwl"
      twittering-password pwtwitter)

(if xwl-at-company?
    (setq twittering-proxy-use t
          twittering-proxy-server "172.16.42.137"
          twittering-proxy-port 8080)
  (setq twittering-host-url (xds "\\?[jCOI*XOI'QO@lPO9nZ*9m[:,aY)'=")
        twittering-api-url (xds "\\?[jCOI*XOI'QO@lPO9nZ*9m[:,aY)'mPO9g")
        twittering-search-url (xds "\\?[jCOI*XOI'QO@lPO9nZ*9m[:,aY)'mZ)M_ZdEf")))

(setq twittering-time-format "%a %m.%d/%H:%M:%S"
      twittering-status-format
      "%i %@ %s, from %f%L%r%R:\n%FILL{%T}\n")

(setq twittering-mode-string "twittering")

(add-hook 'twittering-mode-hook 'less-minor-mode-on)
(add-hook 'twittering-mode-hook (lambda ()
                                  (twittering-icon-mode 1)
                                  (setq twittering-reverse-mode t)))

(eval-after-load 'twittering-mode
  '(progn
     (define-key twittering-mode-map "c" 'twittering-current-timeline)

     (define-key twittering-mode-map "n" 'twittering-goto-next-status)
     (define-key twittering-mode-map "p" 'twittering-goto-previous-status)
     (define-key twittering-mode-map "N" 'twittering-goto-next-status-of-user)
     (define-key twittering-mode-map "P" 'twittering-goto-previous-status-of-user)

     (define-key twittering-mode-map "q" 'xwl-hide-buffer)

     ))

;; ,----
;; | Track cahnges for some buffer
;; `----
(defadvice switch-to-buffer (before
                             highlight-changes-for-some-buffer
                             activate)
  (xwl-highlight-changes-for-some-buffer))

(defun xwl-highlight-changes-for-some-buffer ()
  (cond ((memq major-mode (list ;; 'erc-mode
                           'twittering-mode))
         (let ((buffer-read-only nil)
               (inhibit-read-only t))
           (highlight-changes-mode -1)
           (highlight-changes-mode 1)))
        ((memq major-mode (list 'erc-mode))
           (when (memq (current-buffer) (erc-buffer-list))
             (goto-char (point-max))
             (forward-line -1)))))

;; ,----
;; | s60lxr
;; `----

(global-set-key (kbd "C-c s") 'xwl-s60lxr-search)

(defun xwl-s60lxr-search (filename str)
  (interactive "s(s60lxr) File named: \ns(s60lxr) Containing: ")
  (unless xwl-s60lxr-release
    (xwl-s60lxr-generate-releases))
  (xwl-browse-url-firefox-tab-only
   (format "http://s60lxr/search?v=%s&filestring=%s&string=%s"
           xwl-s60lxr-release filename str)))

(setq xwl-s60lxr-release nil)

(defun xwl-s60lxr-generate-releases ()
  "Also set default `xwl-s60lxr-release'."
  (with-current-buffer
      (let ((url-proxy-services nil))
        (url-retrieve-synchronously "http://s60lxr"))
    (goto-char (point-min))
    (let ((releases '()))
      (when (re-search-forward
             "<span class=\"var-sel\">\\(.+\\)</span>" nil t 1)
        (push (match-string 1) releases)
        ;; Set default release
        (setq xwl-s60lxr-release (match-string 1)))
      (while (re-search-forward
              "<a class='varlink' href=.*>\\(.+\\)</a>" nil t 1)
        (push (match-string 1) releases))
      (kill-buffer (current-buffer))
      releases)))

(defun xwl-s60lxr-select-release (release)
  (interactive
   (list
    (ido-completing-read "Use release: "
                        (xwl-s60lxr-generate-releases))))
  (setq xwl-s60lxr-release release))

;;; Redefine shell-command to return exit status when running synchronously.

(eval-after-load 'simple
  '(progn
     (defun shell-command (command &optional output-buffer error-buffer)
       "Execute string COMMAND in inferior shell; display output, if any.
With prefix argument, insert the COMMAND's output at point.

If COMMAND ends in ampersand, execute it asynchronously.
The output appears in the buffer `*Async Shell Command*'.
That buffer is in shell mode.

Otherwise, COMMAND is executed synchronously.  The output appears in
the buffer `*Shell Command Output*'.  If the output is short enough to
display in the echo area (which is determined by the variables
`resize-mini-windows' and `max-mini-window-height'), it is shown
there, but it is nonetheless available in buffer `*Shell Command
Output*' even though that buffer is not automatically displayed.

To specify a coding system for converting non-ASCII characters
in the shell command output, use \\[universal-coding-system-argument] \
before this command.

Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

The optional second argument OUTPUT-BUFFER, if non-nil,
says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in current buffer.  (This cannot be done asynchronously.)
In either case, the buffer is first erased, and the output is
inserted after point (leaving mark after it).

If the command terminates without error, but generates output,
and you did not specify \"insert it in the current buffer\",
the output can be displayed in the echo area or in its buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.
Otherwise,the buffer containing the output is displayed.

If there is output and an error, and you did not specify \"insert it
in the current buffer\", a message about the error goes at the end
of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional third argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER."

       (interactive
        (list
         (read-shell-command "Shell command: " nil nil
                             (let ((filename
                                    (cond
                                     (buffer-file-name)
                                     ((eq major-mode 'dired-mode)
                                      (dired-get-filename nil t)))))
                               (and filename (file-relative-name filename))))
         current-prefix-arg
         shell-command-default-error-buffer))
       ;; Look for a handler in case default-directory is a remote file name.
       (let ((handler
              (find-file-name-handler (directory-file-name default-directory)
                                      'shell-command))
             exit-status)
         (if handler
             (funcall handler 'shell-command command output-buffer error-buffer)
           (if (and output-buffer
                    (not (or (bufferp output-buffer)  (stringp output-buffer))))
               ;; Output goes in current buffer.
               (let ((error-file
                      (if error-buffer
                          (make-temp-file
                           (expand-file-name "scor"
                                             (or small-temporary-file-directory
                                                 temporary-file-directory)))
                        nil)))
                 (barf-if-buffer-read-only)
                 (push-mark nil t)
                 ;; We do not use -f for csh; we will not support broken use of
                 ;; .cshrcs.  Even the BSD csh manual says to use
                 ;; "if ($?prompt) exit" before things which are not useful
                 ;; non-interactively.  Besides, if someone wants their other
                 ;; aliases for shell commands then they can still have them.
                 (setq exit-status (call-process shell-file-name nil
                                                 (if error-file
                                                     (list t error-file)
                                                   t)
                                                 nil shell-command-switch command))
                 (when (and error-file (file-exists-p error-file))
                   (if (< 0 (nth 7 (file-attributes error-file)))
                       (with-current-buffer (get-buffer-create error-buffer)
                         (let ((pos-from-end (- (point-max) (point))))
                           (or (bobp)
                               (insert "\f\n"))
                           ;; Do no formatting while reading error file,
                           ;; because that can run a shell command, and we
                           ;; don't want that to cause an infinite recursion.
                           (format-insert-file error-file nil)
                           ;; Put point after the inserted errors.
                           (goto-char (- (point-max) pos-from-end)))
                         (display-buffer (current-buffer))))
                   (delete-file error-file))
                 ;; This is like exchange-point-and-mark, but doesn't
                 ;; activate the mark.  It is cleaner to avoid activation,
                 ;; even though the command loop would deactivate the mark
                 ;; because we inserted text.
                 (goto-char (prog1 (mark t)
                              (set-marker (mark-marker) (point)
                                          (current-buffer))))
                 exit-status)
             ;; Output goes in a separate buffer.
             ;; Preserve the match data in case called from a program.
             (save-match-data
               (if (string-match "[ \t]*&[ \t]*\\'" command)
                   ;; Command ending with ampersand means asynchronous.
                   (let ((buffer (get-buffer-create
                                  (or output-buffer "*Async Shell Command*")))
                         (directory default-directory)
                         proc)
                     ;; Remove the ampersand.
                     (setq command (substring command 0 (match-beginning 0)))
                     ;; If will kill a process, query first.
                     (setq proc (get-buffer-process buffer))
                     (if proc
                         (if (yes-or-no-p "A command is running.  Kill it? ")
                             (kill-process proc)
                           (error "Shell command in progress")))
                     (with-current-buffer buffer
                       (setq buffer-read-only nil)
                       (erase-buffer)
                       (display-buffer buffer)
                       (setq default-directory directory)
                       (setq proc (start-process "Shell" buffer shell-file-name
                                                 shell-command-switch command))
                       (setq mode-line-process '(":%s"))
                       (require 'shell) (shell-mode)
                       (set-process-sentinel proc 'shell-command-sentinel)
                       ;; Use the comint filter for proper handling of carriage motion
                       ;; (see `comint-inhibit-carriage-motion'),.
                       (set-process-filter proc 'comint-output-filter)
                       ))
                 ;; Otherwise, command is executed synchronously.
                 (shell-command-on-region (point) (point) command
                                          output-buffer nil error-buffer)))))))))



(provide 'xwl-misc)

;;; xwl-misc.el ends here
