;;; xwl-misc.el --- miscellaneous

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 William Xu

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

(add-to-list 'auto-mode-alist '(".easy-todo" . easy-todo-mode))

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
  "$ fortune-zh || fortune"
  (interactive)
  (message "%s"
           (ansi-color-filter-apply
            (shell-command-to-string "fortune-zh  2>/dev/null || fortune"))))

(global-set-key (kbd "C-c m F") 'xwl-fortune-of-the-day)

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

;; TODO, why this conflicts with which-func mode in c-mode.
;; (eval-after-load 'imenu
;;   '(progn
;;      (defadvice imenu-default-create-index-function (around add-more-default-index activate)
;;        (let ((imenu-generic-expression
;;               (cons (list "Outlines"
;;                           (format "\\(%s\\|%s\\)\\(%s\\)"
;;                                   outline-regexp
;;                                   (concat "^" comment-start "+ | ")
;;                                   ".*")
;;                           2)
;;                     imenu-generic-expression)))
;;          ad-do-it))
;;      ))

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

(require 'electric-spacing)

(defun xwl-text-mode-hook ()
  (auto-compression-mode 1)
  (abbrev-mode 1)
  ;; (flyspell-mode 1)

  (electric-spacing-mode 1)
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
(setq recentf-max-saved-items 100)

(defun xwl-recentf-open-files ()
  (interactive)
  (let ((dups '())
        (unis '())
        (name-full '()))
    ;; find duplicates
    (dolist (path recentf-list)
      (let ((file (file-name-nondirectory path)))
        (if (member file unis)
            (push file dups)
          (push file unis))))
    ;; construct list
    (dolist (path recentf-list)
      (let ((file (file-name-nondirectory path)))
        (if (member file dups)
            (push (cons path path) name-full)
          (push (cons file path) name-full))))
    (find-file
     (cdr (assoc (completing-read "Open recent file: " (mapcar 'car name-full))
                 name-full)))))

(global-set-key (kbd "C-c F") 'xwl-recentf-open-files)

(setq auto-insert-query nil)
(add-hook 'find-file-hook 'auto-insert)

;; ,----
;; | file hooks
;; `----

(setq xwl-sensitive-files
      (mapcar 'expand-file-name
              '("/Users/william/.notes/todo.org"
                "~/.notes/life_blog")))

(defun xwl-find-file-hook ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\r\n$" nil t 1)
      (let ((revert-without-query (list (buffer-file-name))))
        (revert-buffer-with-coding-system 'dos)))))

(add-hook 'find-file-hook 'xwl-find-file-hook)

(defun xwl-write-file-functions ()
  (let ((f (buffer-file-name)))
    ;; (when (string= (file-name-nondirectory file) "todo.org")
    ;;   (xwl-pgg-encrypt))

    ;; Function should return nil when success.
    ;; (defun xwl-write-file-functions ()
    ;;   (xwl-update-date)

    (cond
     ((and (string-match (regexp-opt (list (file-truename xwl-emacs-top))) f)
           (not (string-match "twittering-mode" f)))
      (copyright-update)

      ;; (unless (and (boundp 'qterm-log-file)
      ;;              (string= (file-truename qterm-log-file) f))
      ;;   (nuke-trailing-whitespace)))
     ;;(t
      ;; (unless xwl-at-company?
      ;;   (nuke-trailing-whitespace))
      ))

    ;; should return nil
    nil))

(add-hook 'write-file-functions 'xwl-write-file-functions)

(unless xwl-at-company?
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (save-excursion
;;               (goto-char (point-min))
;;               (when (search-forward
;;                      "# To remove '+' lines, make them ' ' lines"
;;                      nil t 1)
;;                 (delete-trailing-whitespace)))))

(defun xwl-kill-buffer-hook ()
  )

(add-hook 'kill-buffer-hook 'xwl-kill-buffer-hook)

;; FIXME: Do not eval-after-load, it will cause tramp recursive loading. Bug?
(require 'tramp)
(when (boundp 'tramp-default-proxies-alist)
  (add-to-list 'tramp-default-proxies-alist
               '("localhost" "\\`root\\'" "/ssh:%h#2222:")))

(setq password-cache-expiry nil)

(add-to-list 'tramp-methods
              '("plinkxwl"
                (tramp-login-program "plink")
                (tramp-login-args
                 (("-l" "%u")
                  ("-P" "%p")
                  ("-ssh")
                  ("%h")
                  ("-i" "c:/Users/wixu/xwl/.ssh/putty.ppk")))
                (tramp-remote-shell "/bin/sh")
                (tramp-remote-shell-args
                 ("-c"))
                (tramp-default-port 22)))

(setq ga-pkgsrc-dir "~/repo/cvs/pkgsrc")

(global-set-key (kbd "<f10>") 'xwl-ga)

(defun xwl-ga ()
  (interactive)
  (or (some (lambda (b)
              (when (string-match "\\`\\*Ga/" (buffer-name b))
                (switch-to-buffer b)))
            (buffer-list))
      (call-interactively 'ga)))

(setq ga-chicken-repository "/Users/william/repo/svn/chicken-eggs")

(setq ga-backend-methods
      '((apt-get ;; "ssh william@localhost -p 2222 sudo apt-get")
         "sudo apt-get")
        (fink "fink")
        (pkgsrc "sudo")
        (apt-cyg "c:/cygwin/bin/sh.exe '/home/william/w32/apt-cyg'")
        (yum "sudo yum")
        (chicken "sudo chicken-install")
        (brew "brew")
        (tlmgr "sudo tlmgr")))

(add-to-list 'auto-mode-alist
             '("macbluetelnet.*\\(\\.h\\|\\.mm\\|\\.m\\)$" . objc-mode))

(add-to-list 'auto-mode-alist '("\\.info$" . finkinfo-mode))

;; (remove-hook 'find-file-hook 'bracketphobia-hide)

;; FIXME: conflict with cedet, causing cursor random bouncing.
;; (add-hook 'color-theme-xwl-console-hook
;;           (lambda ()
;;             (when window-system
;;               (require 'highlight-tail)
;;               (setq highlight-tail-colors
;;                     `((,(if xwl-black-background? "#bc2525" "#d8971d") . 0)))
;;               (highlight-tail-reload))))

(defun xwl-after-init-hook ()
  ;; FIXME: how to set this only after window has been maximized?
  (run-at-time 3
               nil
               ;; (add-hook 'window-configuration-change-hook
               '(lambda ()
                  ;; (when window-system
                  ;;    (xwl-fullscreen))
                  (sit-for 0.5)
                  (let ((col (round ;(/ (frame-width) 2)
                              (frame-width)
                              )))
                    (setq erc-fill-column (- col 2))) ; 6 for leading timestamp.
                  ))

  ;; (shell-command "sudo ~/bin/.xwl-after-start-hook")
  ;; (setq display-time-mail-file 'no-check)

  ;; On w32: `emacsclient.exe --server-file ~\.emacs.d\server\server -n %*'
  (unless (server-running-p)
    (let ((server-dir "~/.emacs.d/server"))
      (unless (file-exists-p server-dir)
        (mkdir server-dir)))
    (server-start))

  (run-with-idle-timer 600 t (lambda () (run-hooks 'xwl-run-when-idle-hook)))

  ;; (run-with-timer 0 86400 'xwl-running-daily) ; dialy stuffs
  ;; (xwl-weather-update)

  (when (and xwl-black-background?
             (fboundp 'color-theme-xwl-console))
    (run-at-time 1 nil 'color-theme-xwl-console))

  ;; (when (or (string= system-name "zen.local")
  ;;           (eq system-type 'gnu/linux))
  ;;   (xwl-fullscreen))
  (xwl-fullscreen)

  (run-hooks 'color-theme-xwl-console-hook)

  (appt-activate 1)

  (unless (xwl-check-holidays)
    (find-file "~/.emacs.d/scratch")
    ;; (xwl-todo-find-do)
    (delete-other-windows)
    (message (substring (emacs-version) 0 16))))

(when (or (not noninteractive))
  (add-hook 'emacs-startup-hook 'xwl-after-init-hook))

(savehist-mode 1)

(eval-after-load 'scroll-all
  '(progn
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

(add-hook 'kill-emacs-hook (lambda ()
                             (let ((default-directory xwl-emacs-top))
                               (xwl-makefile-all))))

;; (autoload 'typing-of-emacs "The Typing Of Emacs, a game." t)

;; (require 'faith)

(require 'page-break)
(turn-on-page-break-mode)

(setq image-file-name-regexps
      (mapcar (lambda (el)
                (concat "\\." el "$"))
              '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm"
                "pbm" "pgm" "ppm" "pnm")))

(when (or (executable-find "fortune-zh")
          (executable-find "fortune"))
  (add-hook 'xwl-run-when-idle-hook 'xwl-fortune-of-the-day))

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

  (when (and (fboundp battery-status-function)
             (not (string-match-p "N/A"
                                  (battery-format "%p" (funcall battery-status-function)))))

    (setq battery-mode-line-string
          (battery-format battery-mode-line-format
                          (funcall battery-status-function)))

    (defadvice battery-update-handler (around check-on-line activate)
      "If on-line (not using battery), don't display on mode line."
      (if (>=  (string-to-number
                (battery-format "%p" (funcall battery-status-function)))
               90)
          (progn
            (setq battery-mode-line-string "")
            (force-mode-line-update))
        ad-do-it))

    (display-battery-mode 1)
    ))

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
  (hl-line-highlight)
  (other-window 1))

;; (eval-after-load 'replace
;;   '(progn
(define-key occur-mode-map (kbd "p") 'xwl-occur-previous-line)
(define-key occur-mode-map (kbd "n") 'xwl-occur-next-line)
(define-key occur-mode-map (kbd "M-p") 'less-scroll-down-line)
(define-key occur-mode-map (kbd "M-n") 'less-scroll-up-line)
(define-key occur-mode-map (kbd "q") 'xwl-hide-buffer)
(define-key occur-mode-map (kbd "C-o") nil)
;; ))

(define-key occur-mode-map (kbd "r") 'occur-edit-mode)

(add-hook 'occur-edit-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-o"))))

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
(setq ac-auto-start 3
      ac-ignore-case nil)

(add-to-list 'ac-dictionary-directories
             (concat xwl-emacs-top "auto-complete/dict"))
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

;; ,----
;; | Track cahnges for some buffer
;; `----
;; (defadvice switch-to-buffer (before
;;                              highlight-changes-for-some-buffer
;;                              activate)
;;   (xwl-highlight-changes-for-some-buffer))

;; (defun xwl-highlight-changes-for-some-buffer ()
;;   (cond ((memq major-mode (list ;; 'erc-mode
;;                            'twittering-mode))
;;          (let ((buffer-read-only nil)
;;                (inhibit-read-only t))
;;            (highlight-changes-mode -1)
;;            (highlight-changes-mode 1)))
;;         ((memq major-mode (list 'erc-mode))
;;            (when (memq (current-buffer) (erc-buffer-list))
;;              (goto-char (point-max))
;;              (forward-line -1)))))

;; ,----
;; | s60lxr
;; `----

;; (global-set-key (kbd "C-c s") 'xwl-s60lxr-search)

(defun xwl-s60lxr-search (filename str)
  (interactive "s(s60lxr) File named: \ns(s60lxr) Containing: ")
  (require 'org)
  (let ((host (org-reverse-string "moc.aikon.eporue.rxl04s")))
    (unless xwl-s60lxr-release
      (xwl-s60lxr-generate-releases))
    (xwl-browse-url-firefox-tab-only
     (format "http://%s/search?v=%s&filestring=%s&string=%s"
             host
             xwl-s60lxr-release
             (url-hexify-string filename)
             (url-hexify-string str)))))

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
    (completing-read "Use release: "
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

(setq eval-expression-print-length 100)
;; print-length eval-expression-print-length)

(defadvice shell-command (after enable-less activate)
  (let ((b "*Shell Command Output*"))
    (when (get-buffer b)
      (with-current-buffer b
        (less-minor-mode-on)))))

(setq auth-sources
      (cons '(:source "~/.authinfo" :host t :protocol t)
            auth-sources))

(add-hook 'color-theme-xwl-console-hook
          (lambda ()
            (eval-after-load 'hl-line
              '(progn
                 (when xwl-black-background?
                   (set-face-background hl-line-face ;"magenta4"
                                        ;"color-22"
                                        "color-236"
                                        ))))))

(set-face-background 'region "wheat")

(when xwl-black-background?
  (set-face-background 'show-paren-match "color-130")
  (set-face-background 'region "color-24"))

(setq thing-at-point-url-path-regexp "[a-zA-Z0-9.?=%,&/:_#@+~-]+")

(define-global-minor-mode global-goto-address-mode
  goto-address-mode goto-address-mode
  :group 'convenience)

(global-goto-address-mode 1)

(setq org-google-weather-icon-directory
      "~/w32/GNOME_Weather_Icons_by_DarKobra/48x48/status")

(eval-after-load 'hexl-mode
  '(progn
     (define-key hexl-mode-map (kbd "C-o") nil)
     ))

(global-set-key (kbd "C-c k") 'xwl-kill-buffer-name)
(global-set-key (kbd "C-c K") 'xwl-kill-buffer-full-name)
(global-set-key (kbd "C-c m x") 'xwl-w32-start-program)

(when (eq system-type 'darwin)
  (run-at-time "10:30pm" 86400 (lambda ()
                                 (let ((s "Swee la Swee la"))
                                   (xwl-notify "Time!" s)
                                   (xwl-shell-command-asynchronously
                                    (concat "say " s))))))

(eval-after-load 'ns-win
  '(progn
     (defun ns-insert-file ()
       "(Redefined) Insert contents of file `ns-input-file' like insert-file but with less
prompting.  If file is a directory perform a `find-file' on it."
       (interactive)
       (let ((f))
         (setq f (car ns-input-file))
         (setq ns-input-file (cdr ns-input-file))
         (find-file f)
         ;; (if (file-directory-p f)
         ;;     (find-file f)
         ;;   (push-mark (+ (point) (car (cdr (insert-file-contents f))))))
         ))
     ))

;; Display eval output in a way similar to shell-command.

(defadvice eval-expression (around display-large-output-other-buffer activate)
  (let ((inhibit-read-only t))
    (display-message-or-buffer (pp ad-do-it))))

(defadvice eval-last-sexp (around display-large-output-other-buffer activate)
  (let ((inhibit-read-only t))
    (display-message-or-buffer (pp ad-do-it))))

;; (unless (string-match "\\`be" system-name)
;;   (require 'nyan-mode)                  ; need XPM support.
;;   (nyan-mode 1)
;;   )

(defadvice ispell-message (around check-language activate)
  (unless (and (getenv "LANG")
               (string-match "zh_CN" (getenv "LANG")))
    ad-do-it))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . sh-mode))

;; 1. Prepend drive name on buffer on w32
;; 2. show SDK week on linux
;; (defadvice uniquify-get-proposed-name (after prepend-drive-name activate)
;;   (let ((dir (ad-get-arg 1)))
;;     (cond
;;      ((string-match "\\(sdk\\|repo\\)/\\([^/]+\\|ng\\)/" dir)
;;       (let ((wk (match-string 2 dir)))
;;         (setq ad-return-value
;;               (concat wk ":/" ad-return-value))))
;;      ((eq system-type 'windows-nt)
;;       (let ((d (upcase (substring dir 0 1))))
;;         (setq ad-return-value
;;               (concat d xwl-w32-drive-separator
;;                       (cdr (or (assoc d xwl-w32-drives)
;;                                (assoc (downcase d) xwl-w32-drives)))
;;                       ":/" ad-return-value)))))))

(eval-after-load 'calc
  '(progn
     (define-key calc-mode-map (kbd "<S-tab>") 'calc-roll-up)
     ))

(eval-after-load 'image-dired
  '(progn
     (define-key image-dired-thumbnail-mode-map (kbd "p") (kbd "C-b RET"))
     (define-key image-dired-thumbnail-mode-map (kbd "n") (kbd "C-f RET"))

     (define-key image-dired-thumbnail-mode-map (kbd "<left>") (kbd "C-b RET"))
     (define-key image-dired-thumbnail-mode-map (kbd "<right>") (kbd "C-f RET"))
     (define-key image-dired-thumbnail-mode-map (kbd "<up>") (kbd "C-p RET"))
     (define-key image-dired-thumbnail-mode-map (kbd "<down>") (kbd "C-n RET"))

     ;; show original size?

     (defun image-dired-really-slideshow-start ()
       (interactive)
       (when (<= (- (point-max) (point)) 2)
         (goto-char (point-min)))
       (command-execute (kbd "RET"))
       (sleep-for 2)
       (command-execute (kbd "C-f"))
       (image-dired-really-slideshow-start))

     ))

(defun xwl-goto-emacs-bug (id)
  (interactive
   `(,(if (looking-at "[[:digit:]]+")
          (save-excursion
            (skip-chars-backward "[[:digit:]]+" (line-beginning-position))
            (looking-at "[[:digit:]]+")
            (match-string 0))
        (read-string "Bug Id: "))))
  (message "Opening bug %s...  " id)
  (browse-url (concat "http://debbugs.gnu.org/cgi/bugreport.cgi?bug=" id)))

(when (eq system-type 'darwin)
  (defun xwl-warn-low-battery ()
    "Check bluetooth keyboard, trackpad batteries.  "
    (let* ((warn-value 5)
           (cmd-format "ioreg -n %s | grep -i batterypercent | sed 's/^[ \t|]*//' | grep '^\"Batter' | sed 's/.*= //'")
           (trackpad (string-to-number (shell-command-to-string (format cmd-format "BNBTrackpadDevice"))))
           (kb (string-to-number (shell-command-to-string (format cmd-format "AppleBluetoothHIDKeyboard")))))
      (when (<= trackpad warn-value)
        (xwl-notify "Trackpad" (format "Battery low! %d%% remaining." trackpad)))
      (when (<= kb warn-value)
        (xwl-notify "Keyboard" (format "Battery low! %d%% remaining." kb)))))

  (run-with-timer 0 (* 24 60) 'xwl-warn-low-battery)
  )

(eval-after-load 'package
  '(progn
     (defadvice package-menu-execute (around disable-less activate)
       (let ((inhibit-read-only t))
         ad-do-it))
     ))

(add-hook 'xwl-run-when-idle-hook 'recentf-save-list)
;; (add-hook 'xwl-run-when-idle-hook (lambda () (command-execute (kbd "<f8>")))) ; org agenda

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'delete-frame)

  (xwl-after-init-hook))

(setq vc-command-messages t)

(add-hook 'log-edit-mode-hook (lambda () (electric-spacing-mode -1)))

(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

(require 'hi-lock)

(setq  hi-lock-auto-select-face t)

(defun xwl-highlight-phrase ()
  (interactive)
  (if (thing-at-point 'symbol)
      (highlight-phrase (thing-at-point 'symbol) (hi-lock-read-face-name))
    (call-interactively 'highlight-phrase)))

(defun xwl-unhighlight-regexp ()
  (interactive)
  (if (thing-at-point 'symbol)
      (unhighlight-regexp (thing-at-point 'symbol))
    (call-interactively 'unhighlight-regexp)))

(defun xwl-highlight-line ()
  (interactive)
  (require 'org)
  (let ((str (org-current-line-string)))
    (if (not (string-match "^[ \t]*$" str))
        (highlight-lines-matching-regexp
         (regexp-opt (list str)) (hi-lock-read-face-name))
      (call-interactively 'highlight-lines-matching-regexp))))

(define-key hi-lock-map (kbd "C-x w h") 'xwl-highlight-phrase)
(define-key hi-lock-map (kbd "C-x w u") 'xwl-unhighlight-regexp)
(define-key hi-lock-map (kbd "C-x w l") 'xwl-highlight-line)

;;; delete window, winner undo. won't respect dedicated window setup.
(global-set-key (kbd "C-c s") 'sticky-window-keep-window-visible)
(global-set-key (kbd "C-x 0") 'sticky-window-delete-window)
(global-set-key (kbd "C-x 1") 'sticky-window-delete-other-windows)


;;; writer mode

;; http://bzg.fr/emacs-strip-tease.html

(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (round
      (/ (- (frame-pixel-width)
            (* 100 (frame-char-width)))
         3.1)))))

;; Get rid of the indicators in the fringe
;; (mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;;         fringe-bitmaps)

;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))


;; If you want to hide the mode-line in all new buffers

;; Alternatively, you can paint your mode-line in White but then
;; you'll have to manually paint it in black again
;; (custom-set-faces
;;  '(mode-line-highlight ((t nil)))
;;  '(mode-line ((t (:foreground "white" :background "white"))))
;;  '(mode-line-inactive ((t (:background "white" :foreground "white")))))


(defvar xwl-writer-mode nil)
(defun xwl-writer-mode ()
  (interactive)
  (setq xwl-writer-mode (not xwl-writer-mode))
  (if xwl-writer-mode
      (progn
        (unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
          (toggle-frame-fullscreen)
          (sleep-for 1))
        (bzg-big-fringe-mode 1)
        (if xwl-black-background?
            (set-face-background 'fringe "#111111")
          (set-face-background 'fringe "white"))
        (setq cursor-type 'hbar) ;; Use a minimal cursor
        (hidden-mode-line-mode 1)
        (add-hook 'window-configuration-change-hook 'hidden-mode-line-mode))
    (when (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
      (toggle-frame-fullscreen))
    (bzg-big-fringe-mode -1)
    (setq cursor-type 'box)
    (hidden-mode-line-mode -1)
    (remove-hook 'window-configuration-change-hook 'hidden-mode-line-mode)))



(add-to-list 'auto-mode-alist '("sources.list" . conf-mode))

(require 'pangu-spacing)
(setq pangu-spacing-real-insert-separtor t)
;; (global-pangu-spacing-mode 1)
;; FIXME: slow down 7M or bigger files hugely!!
;; (add-hook 'text-mode-hook (lambda () (pangu-spacing-mode 1)))

;; (evil-mode 1)

(setq evil-highlight-closing-paren-at-point-states '(normal emacs insert replace))

(require 'evil)
(add-hook 'find-file-hook (lambda ()
                            (less-minor-mode-off)
                            (evil-local-mode 1)))

(define-key evil-insert-state-map  "\C-t" 'evil-force-normal-state)
(define-key evil-normal-state-map  "\C-t" 'evil-force-normal-state)
(define-key evil-motion-state-map  "\C-t" 'evil-force-normal-state)
(define-key evil-visual-state-map  "\C-t" 'evil-force-normal-state)
(define-key evil-replace-state-map "\C-t" 'evil-force-normal-state)

;; activate it here (why disable it at definition? )
(defadvice show-paren-function (around evil activate)
  "Match parentheses in Normal state."
  (if (if (memq 'not evil-highlight-closing-paren-at-point-states)
          (memq evil-state evil-highlight-closing-paren-at-point-states)
        (not (memq evil-state evil-highlight-closing-paren-at-point-states)))
      ad-do-it
    (let ((pos (point)) syntax narrow)
      (setq pos
            (catch 'end
              (dotimes (var (1+ (* 2 evil-show-paren-range)))
                (if (zerop (mod var 2))
                    (setq pos (+ pos var))
                  (setq pos (- pos var)))
                (setq syntax (syntax-class (syntax-after pos)))
                (cond
                 ((eq syntax 4)
                  (setq narrow pos)
                  (throw 'end pos))
                 ((eq syntax 5)
                  (throw 'end (1+ pos)))))))
      (if pos
          (save-excursion
            (goto-char pos)
            (save-restriction
              (when narrow
                (narrow-to-region narrow (point-max)))
              ad-do-it))
        ;; prevent the preceding pair from being highlighted
        (dolist (ov '(show-paren--overlay
                      show-paren--overlay-1
                      show-paren-overlay
                      show-paren-overlay-1))
          (let ((ov (and (boundp ov) (symbol-value ov))))
            (when (overlayp ov) (delete-overlay ov))))))))

(eval-after-load 'package
  '(progn
     (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
     ))

(setq mac-system-move-file-to-trash-use-finder t)
(setq delete-by-moving-to-trash t)

(eval-after-load 'ack
  '(progn
     (define-key ack-mode-map "n" 'next-error-no-select)
     (define-key ack-mode-map "p" 'previous-error-no-select)
     ))

(setq frame-title-format '((:eval (buffer-file-name))))

;; Don't bother asking whether to make a system file read only or not.  Whatsoever!
(eval-after-load 'files
  '(progn
     (defun find-file-noselect (filename &optional nowarn rawfile wildcards)
       "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional second arg NOWARN non-nil means suppress any warning messages.
Optional third arg RAWFILE non-nil means the file is read literally.
Optional fourth arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, return a list of buffers that are visiting
the various files."
       (setq filename
             (abbreviate-file-name
              (expand-file-name filename)))
       (if (file-directory-p filename)
           (or (and find-file-run-dired
                    (run-hook-with-args-until-success
                     'find-directory-functions
                     (if find-file-visit-truename
                         (abbreviate-file-name (file-truename filename))
                       filename)))
               (error "%s is a directory" filename))
         (if (and wildcards
                  find-file-wildcards
                  (not (string-match "\\`/:" filename))
                  (string-match "[[*?]" filename))
             (let ((files (condition-case nil
                              (file-expand-wildcards filename t)
                            (error (list filename))))
                   (find-file-wildcards nil))
               (if (null files)
                   (find-file-noselect filename)
                 (mapcar #'find-file-noselect files)))
           (let* ((buf (get-file-buffer filename))
                  (truename (abbreviate-file-name (file-truename filename)))
                  (attributes (file-attributes truename))
                  (number (nthcdr 10 attributes))
                  ;; Find any buffer for a file which has same truename.
                  (other (and (not buf) (find-buffer-visiting filename))))
             ;; Let user know if there is a buffer with the same truename.
             (if other
                 (progn
                   (or nowarn
                       find-file-suppress-same-file-warnings
                       (string-equal filename (buffer-file-name other))
                       (message "%s and %s are the same file"
                                filename (buffer-file-name other)))
                   ;; Optionally also find that buffer.
                   (if (or find-file-existing-other-name find-file-visit-truename)
                       (setq buf other))))
             ;; Check to see if the file looks uncommonly large.
             (when (not (or buf nowarn))
               (abort-if-file-too-large (nth 7 attributes) "open" filename))
             (if buf
                 ;; We are using an existing buffer.
                 (let (nonexistent)
                   (or nowarn
                       (verify-visited-file-modtime buf)
                       (cond ((not (file-exists-p filename))
                              (setq nonexistent t)
                              (message "File %s no longer exists!" filename))
                             ;; Certain files should be reverted automatically
                             ;; if they have changed on disk and not in the buffer.
                             ((and (not (buffer-modified-p buf))
                                   (let ((tail revert-without-query)
                                         (found nil))
                                     (while tail
                                       (if (string-match (car tail) filename)
                                           (setq found t))
                                       (setq tail (cdr tail)))
                                     found))
                              (with-current-buffer buf
                                (message "Reverting file %s..." filename)
                                (revert-buffer t t)
                                (message "Reverting file %s...done" filename)))
                             ((yes-or-no-p
                               (if (string= (file-name-nondirectory filename)
                                            (buffer-name buf))
                                   (format
                                    (if (buffer-modified-p buf)
                                        "File %s changed on disk.  Discard your edits? "
                                      "File %s changed on disk.  Reread from disk? ")
                                    (file-name-nondirectory filename))
                                 (format
                                  (if (buffer-modified-p buf)
                                      "File %s changed on disk.  Discard your edits in %s? "
                                    "File %s changed on disk.  Reread from disk into %s? ")
                                  (file-name-nondirectory filename)
                                  (buffer-name buf))))
                              (with-current-buffer buf
                                (revert-buffer t t)))))
                   (with-current-buffer buf

                     ;; Check if a formerly read-only file has become
                     ;; writable and vice versa, but if the buffer agrees
                     ;; with the new state of the file, that is ok too.
                     ;; xwl: shut up.----------------------------------
                     ;; (let ((read-only (not (file-writable-p buffer-file-name))))
                     ;;   (unless (or nonexistent
                     ;; 	      (eq read-only buffer-file-read-only)
                     ;; 	      (eq read-only buffer-read-only))
                     ;;     (when (or nowarn
                     ;; 	      (let* ((new-status
                     ;; 		      (if read-only "read-only" "writable"))
                     ;; 		     (question
                     ;; 		      (format "File %s is %s on disk.  Make buffer %s, too? "
                     ;; 			      buffer-file-name
                     ;; 			      new-status new-status)))
                     ;; 		(y-or-n-p question)))
                     ;;       (setq buffer-read-only read-only)))
                     ;;   (setq buffer-file-read-only read-only))
                     ;; xwl:++++++++++++++++++++++++++++++++++++++++++

                     (unless (or (eq (null rawfile) (null find-file-literally))
                                 nonexistent
                                 ;; It is confusing to ask whether to visit
                                 ;; non-literally if they have the file in
                                 ;; hexl-mode or image-mode.
                                 (memq major-mode '(hexl-mode image-mode)))
                       (if (buffer-modified-p)
                           (if (y-or-n-p
                                (format
                                 (if rawfile
                                     "The file %s is already visited normally,
and you have edited the buffer.  Now you have asked to visit it literally,
meaning no coding system handling, format conversion, or local variables.
Emacs can only visit a file in one way at a time.

Do you want to save the file, and visit it literally instead? "
                                   "The file %s is already visited literally,
meaning no coding system handling, format conversion, or local variables.
You have edited the buffer.  Now you have asked to visit the file normally,
but Emacs can only visit a file in one way at a time.

Do you want to save the file, and visit it normally instead? ")
                                 (file-name-nondirectory filename)))
                               (progn
                                 (save-buffer)
                                 (find-file-noselect-1 buf filename nowarn
                                                       rawfile truename number))
                             (if (y-or-n-p
                                  (format
                                   (if rawfile
                                       "\
Do you want to discard your changes, and visit the file literally now? "
                                     "\
Do you want to discard your changes, and visit the file normally now? ")))
                                 (find-file-noselect-1 buf filename nowarn
                                                       rawfile truename number)
                               (error (if rawfile "File already visited non-literally"
                                        "File already visited literally"))))
                         (if (y-or-n-p
                              (format
                               (if rawfile
                                   "The file %s is already visited normally.
You have asked to visit it literally,
meaning no coding system decoding, format conversion, or local variables.
But Emacs can only visit a file in one way at a time.

Do you want to revisit the file literally now? "
                                 "The file %s is already visited literally,
meaning no coding system decoding, format conversion, or local variables.
You have asked to visit it normally,
but Emacs can only visit a file in one way at a time.

Do you want to revisit the file normally now? ")
                               (file-name-nondirectory filename)))
                             (find-file-noselect-1 buf filename nowarn
                                                   rawfile truename number)
                           (error (if rawfile "File already visited non-literally"
                                    "File already visited literally"))))))
                   ;; Return the buffer we are using.
                   buf)
               ;; Create a new buffer.
               (setq buf (create-file-buffer filename))
               ;; find-file-noselect-1 may use a different buffer.
               (find-file-noselect-1 buf filename nowarn
                                     rawfile truename number))))))
     ))

(unless window-system
  (menu-bar-mode -1))
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

(provide 'xwl-misc)

;;; Local Variables: ***
;;; outline-regexp: ";; | " ***
;;; End: ***

;;; xwl-misc.el ends here
