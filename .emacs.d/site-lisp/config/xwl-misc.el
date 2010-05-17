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

;; ,----
;; | Misc misc
;; `----

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

;; (load "/sw/share/emacs/site-lisp/ledger/ledger.el")

;; (global-set-key (kbd "<f4>") 'cwit)
;; (setq cwit-update-interval 120)
;; (setq cwit-use-local t)

;; (setq tramp-verbose 10)
;; "M-x tramp-submit-bug".
;; (setq tramp-debug-buffer t)

;; (ignore-errors
;;   (progn

;; (require 'mmm-auto)
;; (require 'mmm-sample)
;; (setq mmm-global-mode 'maybe)

;; (setq mmm-classes-alist
;;       `((text-html :submode html-mode
;;                    :front "<html>"
;;                    :front-offset (beginning-of-line 0)
;;                    :back "</html>"
;;                    :back-offset (end-of-line 1)
;;                    :face mmm-code-submode-face)

;;         (c-in-scheme :submode c-mode    ; in chicken scheme
;;                      :front "#>"
;;                      :front-offset (beginning-of-line 0)
;;                      :back "<#"
;;                      :back-offset (end-of-line 1)
;;                      :face mmm-code-submode-face)

;;         ,@mmm-classes-alist))

;; (setq mmm-mode-ext-classes-alist
;;       '((message-mode nil text-html)
;;         (gnus-article-edit-mode nil text-html)
;;         (text-mode nil text-html)
;;         ;; (scheme-mode nil c-in-scheme)
;;         ))

;; ;; (setq mmm-global-classes
;; ;;       (append '(text-html)
;; ;;               mmm-global-classes))

;; (defun xwl-mmm-refresh ()
;;   "Re-apply mmm-mode when buffer contents have been modified."
;;   (when (and mmm-mode (buffer-modified-p))
;;     (mmm-apply-all)))

;; (add-hook 'post-command-hook 'xwl-mmm-refresh)

;; ))

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

;; webjump
(global-set-key (kbd "C-c m w") 'webjump)

(eval-after-load 'webjump
  '(progn
     (mapc (lambda (el)
             (add-to-list 'webjump-sites el))
           '(;; ("gentoo software search" . "http://www.rommel.stw.uni-erlangen.de/~fejf/pfs/")
             ("douban"                 . "http://www.douban.com")
             ("gmail"                  . "http://www.gmail.com")
             ;; ("bloglines"              . "http://www.bloglines.com/feeds")
             ("gitweb"                 . "http://git.savannah.gnu.org/gitweb/")
             ("remember the milk"      . "http://www.rememberthemilk.com")
             ))
     ))

;; bookmark
(eval-after-load 'bookmark
  '(progn
     (defadvice bookmark-default-handler (around just-switch-to-already-opened-file activate)
       (let ((done nil)
             (f (bookmark-get-filename bmk-record)))
         (when (not (file-directory-p f))
           (let ((b (get-buffer (file-name-nondirectory f))))
             (when (and b (string= (buffer-file-name b) f))
               (switch-to-buffer b)
               (setq done t))))
         (unless done
           ad-do-it)))
     ))

;; imenu
(eval-after-load 'imenu
  '(progn
     (defadvice imenu-default-create-index-function (around add-more-default-index activate)
       (let ((imenu-generic-expression
              (cons (list "Outlines"
                          (format "\\(%s\\|%s\\)\\(%s\\)"
                                  outline-regexp
                                  (concat "^" comment-start "+ | ")
                                  ".*")
                          2)
                    imenu-generic-expression)))
         ad-do-it))
     ))

(setq auto-mode-alist
      `(("\\.h$"               . c++-mode)
        ("\\.lrc$"             . emms-lyrics-mode)
        ("\\.sh$"              . shell-script-mode)
        ("\\.m$"               . objc-mode)
         ;; octave-mode)
        ("\\.java$"            . java-mode)
        ("\\.l$"               . c-mode)
        ("\\.jl$"              . sawfish-mode)
        ("\\.hs$"              . haskell-mode)
        ("fonts.conf"          . xml-mode)
        ("\\(rc\\|.conf\\)$"   . conf-mode)
        ("\\(.mac\\|.lst\\)$"  . asm-mode)
        ("\\(.html\\|.htm\\)$" . html-mode)
        ("[sS][cC]onstruct"    . python-mode)
        ("Makefile.*"          . makefile-mode)
        ("^/etc/conf.d"        . conf-mode)
        ("\\.cnf$"             . conf-mode)
        ("\\.ebuild$"          . shell-script-mode)
        ("\\.d$"               . shell-script-mode)
        ("CMakeLists\\.txt\\'" . cmake-mode)
        ("\\.cmake\\'"         . cmake-mode)

        ,@auto-mode-alist))

(when (fboundp 'tramp-cleanup-all-buffers)
  (add-hook 'kill-emacs-hook 'tramp-cleanup-all-buffers))

(setq mac-pass-command-to-system nil)

(require 'smart-operator)

(defun xwl-text-mode-hook ()
  (auto-compression-mode 1)
  (abbrev-mode 1)
  ;; (flyspell-mode 1)

  (smart-operator-mode 1)
  (local-unset-key (kbd "."))
  ;; (local-set-key (kbd "M-S") 'wordnet-search)
  (local-set-key (kbd "M-s") 'dictionary-search)

  (local-unset-key (kbd "M-S")))
;; (local-set-key (kbd "TAB") 'ispell-complete-word))

(add-hook 'text-mode-hook 'xwl-text-mode-hook)

(defun xwl-kill-emacs-hook ()
  (when (fboundp 'gnus-group-exit)
    (gnus-group-exit)))

(add-hook 'kill-emacs-hook 'xwl-kill-emacs-hook)

(defadvice forward-page (after forward-page-and-first-column activate)
  (move-beginning-of-line nil))

(recentf-mode 1)
(add-to-list 'recentf-keep 'file-remote-p)

(defun xwl-recentf-open-files ()
  (interactive)
  (let* ((alist (mapcar '(lambda (el)
                           (cons (file-name-nondirectory el) el))
                        recentf-list))
         (filename (ido-completing-read "Open recent file: "
                                        (mapcar 'car alist))))
    (find-file (cdr (assoc filename alist)))))

(global-set-key (kbd "C-c F") 'xwl-recentf-open-files)

(setq ns-show-menu-bar-p t)

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

(setq ga-pkgsrc-dir "~/repo/cvs/pkgsrc")

(global-set-key (kbd "<f10>") 'xwl-ga)

(defun xwl-ga ()
  (interactive)
  (or (some (lambda (b)
              (when (string-match "\\`\\*Ga/" (buffer-name b))
                (switch-to-buffer b)))
            (buffer-list))
      (call-interactively 'ga)))

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
  (case window-system
    (ns ;; FIXME
     (run-at-time 2 nil 'ns-toggle-fullscreen))
    ((w32)
     (w32-send-sys-command #xf030)))

  ;; FIXME: how to set this only after window has been maximized?
  (run-at-time 5
               nil
               '(lambda ()
                  (let ((col (round (/ (frame-width) 2))))
                    (setq erc-fill-column (- col 2)) ; 6 for leading timestamp.
                    (setq twittering-fill-column col))))

  ;; (shell-command "sudo ~/bin/.xwl-after-start-hook")
  ;; (setq display-time-mail-file 'no-check)

  ;; On w32: `emacsclient.exe --server-file c:\repo\xwl-emacs-environment\.emacs.d\server\server -n %*'
  (ignore-errors (server-start))

  (when (executable-find "fortune-zh")
    (setq xwl-idle-timer
          (run-with-idle-timer 300 t 'xwl-run-when-idle-hook)))

  ;; (run-with-timer 0 86400 'xwl-running-daily) ; dialy stuffs
  ;; (xwl-weather-update)

  (when (fboundp 'color-theme-xwl-console)
    (color-theme-xwl-console))

  (when window-system
    (require 'highlight-tail)
    (setq highlight-tail-colors
          ;; '(("#bc2525" . 0)))
          '(("#d8971d" . 0)))
    (highlight-tail-reload))

  (appt-activate 1)

  (unless (xwl-check-holidays)
    (find-file "~/.scratch")
    ;; (xwl-todo-find-do)
    (delete-other-windows)
    (message (substring (emacs-version) 0 16))))

(unless noninteractive
  (add-hook 'after-init-hook 'xwl-after-init-hook))

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
         (scroll-all-function-all this-command nil)))))

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

(when (eq system-type 'gnu/linux)
  (require 'battery)
  (setq battery-mode-line-format "[%b%p%% %t]")

  (when (fboundp battery-status-function)
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
  )

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

;; jump by name
;; (require 'winring)
;; (winring-initialize)

;; (setq winring-show-names nil)
;; (define-key winring-map "n" 'winring-next-configuration)
;; (define-key winring-map "o" 'winring-new-configuration)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ,----
;; | sawfish
;; `----

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

(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
(ac-config-default)

(defun auto-complete-mode-maybe ()
  "What buffer `auto-complete-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (not (memq major-mode '(erc-mode shell-mode)))

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

;; ,----
;; | twittering-mode
;; `----

(setq twittering-username "xwl"
      twittering-password pwtwitter)

(if xwl-at-company?
    (setq twittering-proxy-use t
          twittering-proxy-server "172.16.42.137"
          twittering-proxy-port 8080)
  (setq twittering-web-host (xds "\\?[jCOI*XOI'QO@lPO9nZ*9m[:,aY)'=")
        twittering-api-host (xds "\\?[jCOI*XOI'QO@lPO9nZ*9m[:,aY)'mPO9g")
        twittering-api-search-host (xds "\\?[jCOI*XOI'QO@lPO9nZ*9m[:,aY)'mZ)M_ZdEf")))

(setq twittering-status-format
      "%i %C{%a %m.%d/%H:%M:%S} %s, from %f%L%r%R:\n%FILL{       %T}\n"
      ;; "%i %C{%a %m.%d/%H:%M:%S} %s, from %f%L%r%R:\n%FILL{%T}\n"
      )

(setq twittering-update-status-function
      'twittering-update-status-from-pop-up-buffer)

(add-hook 'twittering-mode-hook (lambda ()
                                  (twittering-icon-mode 1)
                                  (setq twittering-reverse-mode t)
                                  (twittering-enable-unread-status-notifier)))

(eval-after-load 'twittering-mode
  '(progn
     (define-key twittering-mode-map (kbd "c") 'twittering-current-timeline)

     (define-key twittering-mode-map (kbd "n") 'twittering-goto-next-status)
     (define-key twittering-mode-map (kbd "p") 'twittering-goto-previous-status)
     (define-key twittering-mode-map (kbd "N") 'twittering-goto-next-status-of-user)
     (define-key twittering-mode-map (kbd "P") 'twittering-goto-previous-status-of-user)

     (define-key twittering-mode-map (kbd "q") 'xwl-hide-buffer)

     (define-key twittering-mode-map (kbd "C-c C-g") nil)
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

;; ,----
;; | weather
;; `----

(setq xwl-weather-string "")
(setq xwl-weather-list nil)
(setq xwl-weather-checked-day nil)

(defun xwl-weather-update (&optional force)
  (interactive)
  (if (or force
          (null xwl-weather-checked-day)
          (> (string-to-number (format-time-string "%d" (current-time)))
             xwl-weather-checked-day))
      (progn
        (message "updating weather info...")
        (setq xwl-weather-checked-day
              (string-to-number (format-time-string "%d" (current-time))))
        (setq xwl-weather-list
              (remove
               ""
               (split-string
                (shell-command-to-string
                 "/home/william/repo/darcs/guile/scripts/weather-man.scm 北京")
                "\n")))
        (setq xwl-weather-string
              (replace-regexp-in-string
               "风力："
               ""
               (concat "["
                       (mapconcat (lambda (el) el)
                                  (remove
                                   ""
                                   (cdr (split-string (car xwl-weather-list) ";")))
                                  "")
                       "/"
                       (mapconcat (lambda (el) el)
                                  (remove
                                   ""
                                   (cdr (split-string (cadr xwl-weather-list) ";")))
                                  "")
                       "]")))
        (message "done"))
    (message "weather already up-to-date")))

;; (run-with-idle-timer (* 10 60) (* 24 60 60) 'xwl-weather-update)

(defun xwl-weather-show ()
  (interactive)
  (let ((str ""))
    (mapc (lambda (day) (setq str (concat str day "\n")))
          xwl-weather-list)
    (message str)))

(provide 'xwl-misc)

;;; Local Variables: ***
;;; outline-regexp: ";; | " ***
;;; End: ***

;;; xwl-misc.el ends here
