;;; xwl-gnus.el --- Gnus config

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012 William Xu

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

(setq message-directory "~/.emacs.d/Mail/")
(setq gnus-directory "~/.emacs.d/News/")

;;; Backends

(setq gnus-interactive-exit nil)

(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-topics
      gnus-subscribe-options-newsgroup-method 'gnus-subscribe-topics)

(setq gnus-nov-is-evil nil
      gnus-asynchronous t
      gnus-fetch-old-headers nil
      gnus-use-cross-reference nil)

(setq gnus-select-method
      (if (not (xwl-host-accessible? "news.gmane.org"))
          `(nntp "news.gmane.org"
                 (nntp-address ,(xwl-redirect-host))
                 (nntp-port-number 10119))
        '(nntp "news.gmane.org")))
;;              (nntp-open-connection-function nntp-open-via-rlogin-and-telnet)
;;              (nntp-via-address "tomato")
;;              (nntp-address "news.gmane.org")
;;              (nntp-end-of-line "\n")
;;              (nntp-via-rlogin-command "ssh")))

;; Use `B' in Group buffer to subscribe to some groups from a different
;; newsgroup server.

;; nntp: aioe.cjb.net, news.mozilla.org

;;         (nntp "128.230.129.221"
;;               (nntp-address "localhost")
;;               (nntp-port-number 9119))
;;         (nntp "news.cn99.com")
;;               (nntp-address "localhost")
;;               (nntp-port-number 8119))
;;        (nttp "aioe.cjb.net")

;; (nntp "news.mozilla.org")
;; (nntp "news.yaako.com")
;; (nntp "webking.online.jn.sd.cn")
;; (nntp "news.newsfan.net") ; due to diffcult to sent in gb2312 issue
        ;; (nnslashdot "")

(setq gnus-secondary-select-methods
      `((nnfolder "")

        ;; FIXME: on w32, unable to setup starttls or gnutls-bin for sending by
        ;; gmail using IMAP. Maybe try hamster some timer later.
        ;;   http://www.arcorhome.de/newshamster/tgl/misc/hamster_en.html

        ,@(if (not (xwl-host-accessible? "news.gmane.org"))
              `( ;; (nnimap "imap.gmail.com"
                ;;         (nnimap-address ,(xwl-redirect-host))
                ;;         (nnimap-server-port 10993)
                ;;         (nnimap-stream ssl))

                (nntp "nntp.aioe.org"
                      (nntp-address ,(xwl-redirect-host))
                      (nntp-port-number 12119))
                )
            '(;; (nnimap "imap.gmail.com"
              ;;         (nnimap-server-port 993)
              ;;         (nnimap-stream ssl))
              ;; (nntp "news.cn99.com")
              (nntp "nntp.aioe.org")
              ))

        ;; (nntp "news.gwene.org")
        ))


;;; Receiving Mails

(setq mail-sources '((file :path "~/incoming_mail/default")
                     (file)
                     ))

(eval-after-load "gnus"
  '(progn
     ;; see gnus-demon-handlers for help.
     (setq gnus-demon-timestep 1)

     ;; When idle 2 minutes, then check news every 3 minutes.
     ;; (if gnus-plugged
     ;;     (gnus-demon-add-handler 'xwl-gnus-group-get-new-news 30 10)
     ;;   (gnus-demon-add-handler 'xwl-gnus-group-get-new-news 30 nil))

     (gnus-demon-add-handler 'xwl-gnus-group-get-new-news 120 5)

     ;; FIXME: can i remove this?
     (load "rs-gnus-summary.el")

     ))

(setq xwl-gnus-important-groups
      '("nnimap+imap.gmail.com:important.now"
        "nnfolder:important.now"
        ))

(defun xwl-gnus-group-get-new-news ()
  (interactive)
  (if gnus-plugged
      ;; Only get news for groups with a level lower than 4.  This is
      ;; because some levels' updating takes too long time.
      (gnus-group-get-new-news 3)
    ;; 看看 gnus agent batch 有没有抓回新文章。
    (gnus-read-active-file)
    (gnus-get-unread-articles)
    (gnus-group-list-groups))

  (let ((new (apply '+
                    (mapcar (lambda (i)
                              (let ((n (gnus-group-unread i)))
                                (if (numberp n) n 0)))
                            xwl-gnus-important-groups))))
    (if (zerop new)
        (setq xwl-mail-notify-string "")
      (setq xwl-mail-notify-string (format "Mail(%d)" new))
      ;; (xwl-notify "" (format  "收到 %d 封信啦!" new))
      )
    (force-mode-line-update)))


;;; Sending Mails

;; starttls.el, pop3.el, starttls/gnutls-bin
(require 'starttls)

;; (when (eq system-type 'windows-nt)
;;   (setq starttls-use-gnutls nil))

(setq mail-user-agent 'gnus-user-agent)

(defun xwl-sendmail-by-localhost ()
  (interactive)
  (setq message-send-mail-function 'message-send-mail-with-sendmail))

;; http://www.emacswiki.org/cgi-bin/wiki/MultipleSMTPAccounts

;; (from server port user passwd key cert)
(setq xwl-smtp-accounts
      `((ssl "william.xwl@gmail.com" "smtp.gmail.com" 587 "william.xwl@gmail.com" ,pwgmail nil nil)
        ;(plain "william.xwl@gmail.com" "smtp.gmail.com" 587 "william.xwl@gmail.com" ,pwgmail)
        ))

(defun xwl-set-smtp-plain (server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'"
           server port user))

(defun xwl-set-smtp-ssl (server port user password key cert)
   "Set related SMTP and SSL variables for supplied parameters."
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials (list (list server port key cert)))
   (message "Setting SMTP server to `%s:%s' for user `%s'(SSL enabled)"
            server port user))

(defun xwl-set-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (acc-type address . auth-spec) in xwl-smtp-accounts
          when (string-match address from)
          do (cond
              ((eql acc-type 'plain)
               (return (apply 'xwl-set-smtp-plain auth-spec)))
              ((eql acc-type 'ssl)
               (return (apply 'xwl-set-smtp-ssl auth-spec)))
              (t (error "Unrecognized SMTP account type: `%s'." acc-type)))
          finally (error "(xwl-set-smtp) Cannot interfere SMTP information."))))

(if (eq system-type 'windows-nt)
    (progn                              ; msmtp
      (setq sendmail-program "msmtp")
      (eval-after-load 'message
        '(progn
           (xwl-sendmail-by-localhost))))
  (add-hook 'message-send-hook 'xwl-set-smtp))


;;; Message Mode

(unless (eq system-type 'windows-nt)
  (add-hook 'message-send-hook 'ispell-message)
  (add-hook 'mail-send-hook  'ispell-message)
  (add-hook 'mh-before-send-letter-hook 'ispell-message)
  )

(setq message-sendmail-envelope-from 'header)

(eval-after-load "message"
  '(progn
     (define-key message-mode-map (kbd "ESC TAB") 'bbdb-complete-name)
     (define-key message-mode-map (kbd "<backtab>") 'bbdb-complete-name)))

;; set outgoing coding
(setq xwl-prefer-utf8-p t)
(setq mm-coding-system-priorities '(utf-8))

; mule-gbk stuff, for utf8 <-> gb*

;; (coding-system-put 'chinese-gbk :mime-charset 'gb2312)
;; (utf-translate-cjk-load-tables)

;; (defadvice message-send (around set-outgoing-encoding)
;;   (if xwl-prefer-utf8-p
;;       ad-do-it
;;     (let ((mm-coding-system-priorities '(gb2312 gbk utf-8)))
;;       ad-do-it)))

;; (ad-activate 'message-send)

;; One can mail the *Group* buffer, select different posting styles
;; according to group name at point for matching. So better avoiding
;; composing mail by `C-x m', instead, `m' at a proper group line in
;; *Group* buffer.

(global-unset-key (kbd "C-x m"))

(defadvice gnus-group-post-news (around insert-daily-template activate)
  (let ((group (gnus-group-group-name)))
    ad-do-it
    (cond
     ;; nnfolder:blog - send mails as text/html.
     ((string-match "nnfolder:blog" group)
      (electric-spacing-mode -1)
      (save-excursion
        (forward-paragraph)
        (forward-line)
        (insert "<#multipart type=mixed>
<#part type=text/html charset=\"utf-8\" nofile=yes>
<html>
<body>


")
        (when (re-search-forward "^--" nil t 1)
          (move-beginning-of-line 1)
          (insert "<pre>\n")
          (goto-char (point-max))
          (insert "</pre>\n")
          )
        (insert "
</body>
</html>
<#/multipart>
")))
     )

    ;; (longlines-mode 1)
    ))


;;; Chinese Stuffs

(define-coding-system-alias 'x-gbk 'gb18030)

(eval-after-load "gnus"
  '(progn
     (setq gnus-group-name-charset-group-alist
           '(("nnrss.*" . utf-8)        ; `G R'
             (".*" . gb18030)))

     (add-to-list 'gnus-group-charset-alist '("nnrss.*" utf-8))
     ))

;; (setq gnus-group-name-charset-method-alist
;;       '(((nntp "news.newsfan.net") . gb2312)))

;; (setq gnus-summary-show-article-charset-alist
;;       '((1 . utf-8)
;;         (2 . gb2312)
;; 	(3 . big5)))


;;; Essential: It's all about groups!

;; 1. List Groups, Nonlist Groups
;; 2. Group Parameters
;; 3. Posting Style Based On Group
;; 4. Split Received Mails Into Groups
;; 5. Expire Groups

;;;; 1. List Groups, Nonlist Groups, Important Groups

;; '((group . to-list) ...)
(setq xwl-company-list-table
      (mapcar
       (lambda (i)
         (cons (replace-regexp-in-string "@.*" "" i) i))
       '(
         ;; "daily@abc.net"
         )))

(setq xwl-company-nonlist-groups
      '( ;; "vce"
        ))

;; '((group . to-address) ...)
(setq xwl-list-table
      (mapcar
       (lambda (i)
         (cons (replace-regexp-in-string "@.*" "" i) i))
       '(
         "bug-gnu-emacs@gnu.org"
         ;; to remove

         ;; "cocoa-dev@lists.apple.com"
         "pongba@googlegroups.com"
         "python-cn@googlegroups.com"
         )))


;;;; 2. Gnus Parameters

(setq gnus-gcc-mark-as-read t)		; mark Gcc mail as read

(setq gnus-parameters
      `(,@(mapcar
           (lambda (i)
             `(,(car i)
               (to-list . ,(cdr i))
               (gcc-self . t)))
           xwl-list-table)

        ,@(mapcar
           (lambda (i)
             `(,(car i)
               (to-list . ,(cdr i))))
           xwl-company-list-table)

        ;; (,xwl-nnrss-groups (total-expire . t))

        ("nnimap+imap.gmail.com.*"
         (gcc-self . t))

        (".*important.*"
         (gcc-self . t))

        ;; (xds "[)cjY>c_YJ,+[)nlZ>0q[<9`Y>0eQ)MpCdEmYH==")
        ("blog-life"
         (to-address . ,(xds "[)cjY>c_YO_*Y:,`Y>0eZ>0q[<9qZ>FaQODlY>c)QJ,aY)'="))
         (gcc-self . t))

        ("blog-tech"
         (to-address . ,(xds "Z>0q[<9nY*E'QOAm[ODlP)0k"))
         (gcc-self . t))

        ("trash"
         (total-expire . t))))


;;;; 3. Posting Style

(setq user-full-name "William Xu"
      user-mail-address "william.xwl@gmail.com"	; don't use `<mail>'!
      mail-signature t)

;; The entire alist will be iterated over!
(setq gnus-posting-styles
      `((".*"
	 (name user-full-name)
	 (address user-mail-address)
	 (organization "the Church of Emacs")
         (signature
          (format "William\n\nhttp://xwl.appspot.com\n"
                  ;; (shell-command-to-string "fortune")
                  ))
         ;; (eval (xwl-sendmail-by-gmail))
         )

        (,(regexp-opt '("cn.fan" "cn.comp.os.linux"))
         (name "未临")
         (signature
          (format "William\n\nhttp://xwl.appspot.com\n\n%s\n"
                  (if (file-exists-p "~/.notes/favorites_now")
                      (xwl-fortune-favorites-vertically "~/.notes/favorites_now")
                    ""))))

        ("blog-*"
         (signature nil
;;           (format "William\n</pre>\n\n%s"
;;                   ;; (xwl-fortune-favorites-vertically "~/.notes/favorites_now")
;;                   (xwl-qiushibaike-random)
;;                   )
          ))

        ;; Keep company mail at the end.
;;         (,(regexp-opt (append (mapcar 'car xwl-company-list-table)
;;                               xwl-company-nonlist-groups))
;;          (address "william@abc.net")
;;          (signature "William\n"))
        ))



;;;; 4. Split Received Mails

(setq mail-archive-file-name "~/.emacs.d/outgoing")

(setq mail-source-delete-incoming t)

;; (setq gnus-message-archive-method
;;       '(nnfolder "archive"
;; 		 (nnfolder-directory   "~/src/backup/gnus/Mail/archive")
;; 		 (nnfolder-active-file "~/src/backup/gnus/Mail/archive/active")
;; 		 (nnfolder-get-new-mail nil)
;; 		 (nnfolder-inhibit-expiry t)))

;; (setq gnus-message-archive-group   ; nnfolder+archive:outgoing.important
;;       `(("important" "outgoing.important")
;;         (,(concat "^\\(" (regexp-opt xwl-company-groups) "\\)$")
;;          "outgoing.ce")
;; 	(".*" "outgoing.news")))

(setq gnus-message-archive-group "outgoing") ; nnfolder+archive:outgoing

;; set some default email and news headers
(setq message-default-mail-headers "Fcc: ~/.emacs.d/outgoing"
      message-default-news-headers "Fcc: ~/.emacs.d/outgoing")

(setq nnmail-split-fancy-match-partial-words t)

(setq nnmail-split-fancy
      `(|
        (from ,(concat ".*"
                       (regexp-opt '("@localhost"
                                     "Cron Daemon"))
                       ".*")
              "local")

        ;; ignore my own mail, (since i gcc all)
        ;;           (from ,(regexp-opt
        ;;                   '("william.xwl@gmail.com"
        ;;                     "william.xwl@hotmail.com"))
        ;;                 "outgoing.tolist")

        ,@(mapcar (lambda (i)
                    `(any ,(cdr i) ,(car i)))
                  `(,@xwl-company-list-table ,@xwl-list-table))

        (: xwl-split-mailing-lists)

        ;; mailinglists that are subscribed by newsgroup
        (any ,(mapconcat 'identity
                         '("sawfish-list@gnome.org")
                         "\\|")
             "cc-trash")

        (to "william@localhost" "rss")

        (from ".*@message.cmbchina.com" "life")
        (from "*.*shinseiretailalert@shinseibank.com.*" "life")

        (from ".*@posterous.com" "posterous")

        (to "william.xwl@gmail.com" (: xwl-notify-important))

        ;; (xds "\\?[jD;A8YNFgY?Dl[?EgYd[f[N<lQNI(CdEl")
        (from ".*@\\(mails.thu.edu.cn\\|tsinghua.org.cn\\)"    "tsinghua")
        (to ,(xds "\\?M*QNcjXN,8YNFgY:,'Z)clQ)_(PJ,mZdZlP)+=") "tsinghua")
        (to ,(xds "[)cjY>c_YJ,+[)o8X>0'YNFgY:,aY)'=")          "hotmail")
        ;; (to ,(xds "\\?Ll[)MgY>clH?c_X>0mCdEmYH==") (: xwl-notify-important))

        (to ".*@newsmth.*" "newsmth")

        ;; "trash"
        "general"
        ))

(setq nnmail-split-methods 'nnmail-split-fancy)

(setq nnimap-split-inbox '("INBOX"))
(setq nnimap-split-rule 'nnmail-split-fancy)

(defun xwl-split-mailing-lists ()
  "e.g., foo@googlegroups.com -> foo"
  (let ((re (concat "^List-Post:.*<mailto:\\([-a-zA-Z._]+\\)@"
                    (regexp-opt '("googlegroups.com"
                                  "lists.apple.com"
                                  "lists.sourceforge.net"
                                  ;; "gnu.org"
                                  )))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward re nil t)
        (match-string 1)))))

(defun xwl-notify-important ()
;;   (xwl-shell-command-asynchronously
;;     "zenity --info --text \"You've Got Mail \!\" --title \"Gnus\"")
  "important.now")


;;;; 5. Expire Groups

(setq nnmail-expiry-wait-function
      (lambda (group)
	(cond
         ((string-match "nnimap.*" group)
          'never)

         ;; list
	 ((string-match (regexp-opt
                         (mapcar 'car `( ; xwl-company-list-table
                                              ,@xwl-list-table)))
                        group)
	  14)
         ;; trash
	 ((or (string= "trash" group)
              (string= "cc-trash" group))
          3)

         ;; ((string-match xwl-nnrss-groups group 7))

	 (t 'never))))

;; (setq nnmail-fancy-expiry-targets
;;       '(("Newsgroups" ".+" "")))


;;; Group

;; ,----[ level 1 ]
;; | company mail
;; | important.now
;; `----

;; ,----[ level 2 ]
;; | other mails
;; `----

;; ,----[ level 3 ]
;; | others mail subscribe, newsgroups
;; `----

;; ,----[ level > 3 ]
;; | rss
;; `----

(setq gnus-activate-level 5)

;; important mails first
(defun xwl-gnus-group-check-company-mails (&optional level)
  (interactive)
  (let ((mail-sources (list xwl-company-source)))
    (gnus-group-get-new-news (or level 1))))

(defun xwl-gnus-group-mode-hook ()
  (xwl-vi-like-hook)
  ;; (local-set-key (kbd "Q") 'gnus-group-exit)
  (local-unset-key (kbd "q"))
  (local-unset-key (kbd "Q"))
;;   (local-set-key (kbd "<") 'gnus-group-prev-unread-group)
;;   (local-set-key (kbd ">") 'gnus-group-next-unread-group)
;;   (local-set-key (kbd "p") 'previous-line)
;;   (local-set-key (kbd "n") 'next-line)

  (gnus-topic-mode))

(eval-after-load "gnus-group"
  '(progn
     (define-key gnus-group-mode-map (kbd "g") 'xwl-gnus-group-get-new-news)
     ;; (define-key gnus-group-mode-map (kbd "g") 'xwl-disable-key)

     (define-key gnus-group-mode-map (kbd "m") nil)

;;      (define-key gnus-group-mode-map (kbd "m") (lambda ()
;;                                                  (interactive)
;;                                                  (gnus-group-mail 0)))

     (define-key gnus-group-mode-map (kbd "a") (lambda ()
                                                 (interactive)
                                                 (gnus-group-post-news 0)))

     (define-key gnus-group-mode-map (kbd "N") 'gnus-topic-goto-next-topic)
     (define-key gnus-group-mode-map (kbd "P") 'gnus-topic-goto-previous-topic)

     (define-key gnus-group-mode-map (kbd "q") 'next-buffer)

     ;; Make subscribing easier
     (require 'gmane-list)
     (defun xwl-gnus-group-unsubscribe-group (group)
       (interactive
        (list
         (completing-read "Group: "
                          (mapcar 'symbol-name xwl-gmane-list))))
       (gnus-group-unsubscribe-group group))

     (define-key gnus-group-mode-map (kbd "U") 'xwl-gnus-group-unsubscribe-group)

     ))

(add-hook 'gnus-group-mode-hook 'xwl-gnus-group-mode-hook)

(setq gnus-permanently-visible-groups
      (regexp-opt `(;; "savings"
                    ;; "outgoing"
                    ;; "nnimap+imap.gmail.com:important.now"
                    ;; "nnimap+imap.gmail.com:important.INBOX"
                    ;; "life"
                    ;; ,@xwl-company-groups
                    "blog-life"
                    "blog-tech"
                    ;; "important.now"
                    )))



;;; Summary

;; Make mails sent by myself display my name instead of "=>blahblah" in
;; the summary buffer.
(setq gnus-ignored-from-addresses nil)

;; date
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
	(604800               . "%a %H:%M") ; this week
	((gnus-seconds-month) . "%d")
	((gnus-seconds-year)  . "%m/%d")
	(t                    . "%Y/%m/%d")))

;; Note!  Do `^, g' to update changes by `nnmail-extra-headers'! See
;; info for more.
(setq gnus-extra-headers '(Content-Type To Newsgroups))
(setq nnmail-extra-headers gnus-extra-headers)

(defalias 'gnus-user-format-function-ct 'rs-gnus-summary-line-content-type)

(defun gnus-user-format-function-from (head)
  "Trim `From:' to 20 bytes."
  (let ((re "[\" ]")
        (from (gnus-header-from head)))
    (when (string-match ".+@newsmth.net-SPAM.no (\\(.+\\))" from)
      (setq from (replace-match "\\1" nil nil from)))
    (setq from
          (replace-regexp-in-string
           (format "^%s+\\|%s+$" re re) ""
           (replace-regexp-in-string "<.*>" "" from)))
    (when (> (length from) 20)
      (setq from (concat (substring from 0 18) "..")))
    (format "%-20s" from)))

;;(setq gnus-summary-line-format "%U%R%z%-6d  %5k  %-20f%B%s\n")
(setq gnus-summary-line-format
      "%U%R%z%10&user-date; %u&ct; %5k  %4i  %u&from; %B(%t) %s\n")

(defun xwl-gnus-summary-tree-plain ()
  "My old plain summary tree."
  (interactive)
  (setq gnus-sum-thread-tree-root            "" ; "* "
        gnus-sum-thread-tree-false-root      "" ; "* "
        gnus-sum-thread-tree-single-leaf     "\\"
        gnus-sum-thread-tree-single-indent   ""
        gnus-sum-thread-tree-indent          "  "
        gnus-sum-thread-tree-leaf-with-other "| "
        gnus-sum-thread-tree-vertical        ""))

;; vi
(defun xwl-vi-like-hook ()
    (local-set-key (kbd "k") 'previous-line)
    (local-set-key (kbd "j") 'next-line)
    (local-set-key (kbd "l") 'forward-char)
    (local-set-key (kbd "h") 'backward-char))

(defun xwl-gnus-summary-mode-hook ()
  (xwl-vi-like-hook)

  (define-key gnus-summary-mode-map (kbd "p") 'gnus-summary-prev-same-subject)
  (define-key gnus-summary-mode-map (kbd "n") 'gnus-summary-next-same-subject)
  (define-key gnus-summary-mode-map (kbd "q") 'delete-other-windows)
  (define-key gnus-summary-mode-map (kbd "Q") 'gnus-summary-exit)

  (define-key gnus-summary-mode-map (kbd ",") 'gnus-summary-prev-thread)
  (define-key gnus-summary-mode-map (kbd ".") 'gnus-summary-next-thread)

  (define-key gnus-summary-mode-map (kbd "P") 'xwl-scroll-other-window-down-one-line)
  (define-key gnus-summary-mode-map (kbd "N") 'xwl-scroll-other-window-up-one-line)
  (define-key gnus-summary-mode-map (kbd "<") 'scroll-other-window-down)
  (define-key gnus-summary-mode-map (kbd ">") 'scroll-other-window)
  (define-key gnus-summary-mode-map (kbd "/ n") 'gnus-summary-insert-new-articles)

  (define-key gnus-summary-mode-map (kbd "r") (lambda () (interactive)
    		       (gnus-summary-show-article)
    		       (other-window 1)))

  (define-key gnus-summary-mode-map (kbd "RET") (lambda () (interactive)
    		       (gnus-summary-show-article)
    		       (other-window 1)))

  (define-key gnus-summary-mode-map (kbd "C-o") nil))

(add-hook 'gnus-summary-mode-hook 'xwl-gnus-summary-mode-hook)

(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)

;; save/copy some articles?
;;
;; - `B c': copy article to some group
;; - `*': put it in the cache, and use `Y c' to show it later
(setq gnus-use-cache 'passive)

(case window-system
  ((darwin)
   (xwl-gnus-summary-tree-plain))
  (t
   (rs-gnus-summary-tree-arrows-wide)))

;; ,----
;; | threading
;; `----
(setq gnus-summary-gather-subject-limit 'fuzzy
      gnus-simplify-subject-fuzzy-regexp "^\\[.+\\]")


;;; Article

(setq gnus-visible-headers
      (concat "^\\("
	      (regexp-opt
	       '("From" "To" "CC" "Subject" "Date"
		 "User-Agent" "X-Mailer" "X-Newsreader"
		 "NNTP-Posting-Host"
		 "Organization"
		 ;; "Content-Type" "Content-Transfer-Encoding"
                 "Newsgroups"))
	      "\\):"))


;; 用 `C-u g' 显示原始文章样式。W w, W Q
;; (remove-hook 'gnus-article-prepare-hook 'gnus-article-fill-long-lines)

(setq xwl-enter-first-article t)
(add-hook 'gnus-article-prepare-hook
          '(lambda ()
             ;; FIXME, why do i have to re-eval?
             (when  (and xwl-enter-first-article window-system)
               ;; (when (and (fboundp 'color-theme-xwl-console)
               ;;            xwl-black-background?)
               ;;   (color-theme-xwl-console))
               (setq xwl-enter-first-article nil))))

(defun xwl-gnus-article-show-ip ()
  "Show author's ip info in newsgroups."
  (save-excursion
    (message-narrow-to-headers)
    (when (search-forward-regexp
           "NNTP-Posting-Host: \\([0-9.]+\\)" nil t) ; a-zA-Z
      (end-of-line)
      (insert-and-inherit " (")
      (insert-and-inherit
       (car
        (split-string
         (shell-command-to-string
          (concat "pyip.py " (match-string-no-properties 1)))
         "\n")))
      (insert-and-inherit ")"))))

;; (add-hook 'gnus-article-prepare-hook 'xwl-gnus-article-mode-hook)

(setq message-yank-prefix;; nil)
      "> ")

;; TODO
;; (fset 'w3m-safe-view-this-url (symbol-function 'w3m-safe-view-this-url))
(defun w3m-safe-view-this-url ()
  "Modified by xwl."
  (interactive)
  (let ((w3m-pop-up-windows nil)
	(url (w3m-url-valid (w3m-anchor))))
    (cond
     (url
      (or (when (fboundp w3m-goto-article-function)
            (funcall w3m-goto-article-function url))
          (browse-url url)))
     ((w3m-url-valid (w3m-image))
      (if (w3m-display-graphic-p)
	  (w3m-toggle-inline-image)
	(w3m-view-image)))
     (t
      (w3m-message "No URL at point")))))

;; (defadvice mm-inline-text-html-render-with-w3m (around follow-link (handle)
;; 						       activate)
;;   "Follow a link in nnrss group."
;;   ad-do-it
;;   (cond ((string-match "\\`nnrss:.*Revisions.*" gnus-newsgroup-name)
;;          (xwl-rss-expand-trac-log))
;; ;;         ((string-match "\\`nnrss:.*" gnus-newsgroup-name)
;; ;;          (xwl-rss-visit-link))
;;         ))

(defun xwl-rss-expand-trac-log ()
  "Expand full trac change log."
  (save-excursion
    (let ((inhibit-read-only t)
          (article-beg (point))
          (article-end (progn (goto-char (point-max))
                              (1- (re-search-backward "link" nil t 1))))
          (log ""))
      (delete-region article-beg article-end)
      (let* ((url (w3m-url-valid (w3m-anchor)))
             (buf (url-retrieve-synchronously url)))
        ;; extract log message
        (when buf
          (with-current-buffer buf
            (let ((str (delete-and-extract-region (point-min) (point-max))))
              (setq str (decode-coding-string str 'utf-8))
              (insert str)
              (w3m-region (point-min) (point-max) nil 'utf-8)
              (goto-char (point-max))
              (when (re-search-backward "^Changeset" nil t 1)
                (let ((log-beg (point)))
                  ;; delete the update section
                  (when (re-search-forward "^View differences" nil t 1)
                    (let ((update-beg (progn (beginning-of-line)
                                             (point))))
                      (re-search-forward "Update" nil t 1)
                      (delete-region update-beg (progn (end-of-line)
                                                       (point)))))
                  (re-search-forward "^Legend" nil t 1)
                  (forward-line 0)
                  (xwl-strip-space-region log-beg (point))
                  ;; Add one blank line after Changeset
                  (save-excursion
                    (goto-char log-beg)
                    (end-of-line)
                    (insert "\n"))
                  (setq log (buffer-substring-no-properties log-beg (point))))))
            (kill-buffer (current-buffer)))))
      (goto-char (point-max))
      (insert log))))


;;; Scoring

(add-hook 'message-sent-hook 'gnus-score-followup-thread)

(add-hook 'gnus-summary-exit-hook 'gnus-group-save-newsrc)
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
(add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)


;;; MIME

(setq mm-default-directory "~/Downloads")

;; Use "symbol link files" for attached files, instead of making copies.
(setq gnus-gcc-externalize-attachments 'all)

;; See `~/.mailcap' about actions based on MIME.

;; apply darcs patch

(eval-after-load 'gnus-art
  '(progn
     (define-key gnus-mime-button-map (kbd "a") 'his-gnus-darcs-apply-part)

     (defun article-fill-long-lines ()
       "Fill lines that are wider than the window width."
       (interactive)
       (save-excursion
         (let ((inhibit-read-only t)
               (width (window-width (get-buffer-window (current-buffer)))))
           ;; (switch-to-buffer "*Article*")
           (save-restriction
             (article-goto-body)
             (let ((adaptive-fill-mode nil)) ;Why?  -sm
               (while (not (eobp))
                 (end-of-line)
                 (when (>= (current-column) (min fill-column width))
                   (narrow-to-region (min (1+ (point)) (point-max))
                                     (point-at-bol))
                   (let ((goback (point-marker)))
                     (back-to-indentation)
                     (skip-chars-forward "> ")
                     (set-fill-prefix)
                     (fill-paragraph nil)
                     (goto-char (marker-position goback)))
                   (widen))
                 (forward-line 1)))))))

     ))

(defun his-gnus-darcs-apply-part (repo)
  "Apply the MIME part under point to a Darcs repository."
  (interactive "DApply to Darcs repository: ")
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point)
                                 'gnus-data)))
    (when data
      (mm-with-unibyte-buffer
        (mm-insert-part data)
        (his-send-region-to-command (point-min)
                                   (point-max)
                                   "darcs" "apply"
                                   (format "--repodir=%s"
                                           (expand-file-name repo))
                                   "-a")))))

(defun his-send-region-to-command (beg end command &rest args)
  "Call COMMAND with ARGS, and display output in a special buffer."
  (let* ((coding-system-for-write 'binary)
         (buf (with-current-buffer
                  (get-buffer-create "*Shell Command Output*")
                (setq buffer-read-only nil)
                (erase-buffer)
                (current-buffer)))
         (exit-status (apply 'call-process-region
                             beg end
                             command
                             nil buf nil
                             args)))
    (with-current-buffer buf
      (setq mode-line-process
            (cond ((null exit-status)
                   " - Error")
                  ((stringp exit-status)
                   (format " - Signal [%s]" exit-status))
                  ((not (equal 0 exit-status))
                   (format " - Exit [%d]" exit-status)))))
    (if (with-current-buffer buf (> (point-max)
                                    (point-min)))
        ;; There's some output, display it
        (display-message-or-buffer buf)
      ;; No output; error?
      (cond ((null exit-status)
             (message "(Command failed with error)"))
            ((equal 0 exit-status)
             (message "(Command succeeded with no output)"))
            ((stringp exit-status)
             (message "(Command killed by signal %s)"
                      exit-status))
            (t
             (message "(Command failed with code %d and no output)"
                      exit-status output))))))


;;; RSS

;; "G R"

(setq nnrss-use-local t)

(defun xwl-gnus-group-make-rss-group-noninteractively (url)
  "Given a URL, discover if there is an RSS feed.
If there is, use Gnus to create an nnrss group"
  (if (not url)
      (setq url (read-from-minibuffer "URL to Search for RSS: ")))
  (let ((feedinfo (nnrss-discover-feed url)))
    (if feedinfo
        (let ((title (gnus-newsgroup-savable-name
                      (gnus-newsgroup-savable-name
                       (or (cdr (assoc 'title feedinfo)) ""))))
              (desc (cdr (assoc 'description feedinfo)))
              (href (cdr (assoc 'href feedinfo)))
              (encodable (mm-coding-system-p 'utf-8)))
          (when encodable
            ;; Unify non-ASCII text.
            (setq title (mm-decode-coding-string
                         (mm-encode-coding-string title 'utf-8) 'utf-8)))
          (gnus-group-make-group (if encodable
                                     (mm-encode-coding-string title 'utf-8)
                                   title)
                                 '(nnrss ""))
          (push (list title href desc) nnrss-group-alist)
          (nnrss-save-server-data nil))
      (error "No feeds found for %s" url))))

(setq xwl-gnus-rss-list
      '(
        ))

(defun xwl-gnus-group-update-rss-group ()
  "Add rss groups in `xwl-gnus-rss-list'."
  (interactive)
  (with-current-buffer "*Group*"
    (mapc
     (lambda (url)
       (condition-case nil
           (xwl-gnus-group-make-rss-group-noninteractively url)
         (error nil)))
     xwl-gnus-rss-list)
    (message "done")))

(defun xwl-rss-visit-link ()
  "Open current article's link with w3m."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "link" nil t 1)
      (w3m-safe-view-this-url)          ; TODO, use firefox
      ;; (w3m-external-view-this-url)
      (message "Link opened"))))


(eval-after-load 'gnus
  '(progn
     (define-key gnus-article-mode-map (kbd "q") 'delete-window)
     (define-key gnus-article-mode-map (kbd "v") 'xwl-rss-visit-link)
     ))

;;; Agent

;; How to put a method into agent mode? `J a' in server buffer.

;; FIXME: This will oops gnus-agent-batch??
;; (setq gnus-agent-synchronize-flags t)


;;; Misc

;; gnus-group-make-web-group

(defun xwl-base64-decode-from ()
  "Decode \"foo@bar\" part in address \"Name <foo@bar>\".
This function does nothing when \"foo@bar\" is not base64 encoded."
  (with-current-buffer nntp-server-buffer
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (let* ((start (re-search-forward "^From: " nil t 1))
                 (end (line-end-position))
                 (s ""))
            (when start
              (setq s (buffer-substring-no-properties start end))
              (when (string-match "^=\\?[^@]+\\?=$" s)
                (let ((coding (progn (string-match "=\\?\\(.+\\)\\?[bB]\\?" s)
                                     (match-string 1 s)))
                      (decoded-s (progn (string-match "\\?[bB]\\?\\(.*\\)\\?=" s)
                                        (base64-decode-string (match-string 1 s))))
                      (name "")
                      (address ""))
                  (when (string-match "\\(.+\\)\\( +<?.+@.+>?\\)" decoded-s)
                    (setq name (match-string 1 decoded-s)
                          address (match-string 2 decoded-s))
                    (goto-char start)
                    (delete-and-extract-region start end)
                    (insert
                     (format "=?%s?B?%s?=%s" coding (base64-encode-string name) address)))))))
        (error nil)))))

(eval-after-load 'nnimap
  '(progn
     (defadvice nnimap-split-to-groups (before xwl-base64-decode-from activate)
       (xwl-base64-decode-from))
     ))

;; ,----
;; | picon
;; `----
(eval-after-load 'gnus-art
  '(progn
     (add-to-list 'gnus-picon-databases "/sw/share/picons")
     ))

(setq gnus-treat-from-picon 'head)

(setq gnus-picon-style 'right)

(setq gnus-treat-body-boundary nil)

(eval-after-load 'gnus-picon
  '(progn
     (defun gnus-picon-transform-address (header category)
       (gnus-with-article-headers
         (let ((addresses
                (mail-header-parse-addresses
                 ;; mail-header-parse-addresses does not work (reliably) on
                 ;; decoded headers.
                 (or
                  (ignore-errors
                    (mail-encode-encoded-word-string
                     (or (mail-fetch-field header) "")))
                  (mail-fetch-field header))))
               spec file point cache len)
           (dolist (address addresses)
             (setq address (car address))
             (when (and (stringp address)
                        (setq spec (gnus-picon-split-address address)))
               (if (setq cache (cdr (assoc address gnus-picon-cache)))
                   (setq spec cache)
                 (when (setq file (or (gnus-picon-find-face
                                       address gnus-picon-user-directories)
                                      (gnus-picon-find-face
                                       (concat "unknown@"
                                               (mapconcat
                                                'identity (cdr spec) "."))
                                       gnus-picon-user-directories)))
                   (setcar spec (cons (gnus-picon-create-glyph file)
                                      (car spec))))

                 (dotimes (i (1- (length spec)))
                   (when (setq file (gnus-picon-find-face
                                     (concat "unknown@"
                                             (mapconcat
                                              'identity (nthcdr (1+ i) spec) "."))
                                     gnus-picon-domain-directories t))
                     (setcar (nthcdr (1+ i) spec)
                             (cons (gnus-picon-create-glyph file)
                                   (nth (1+ i) spec)))))
                 (setq spec (nreverse spec))
                 (push (cons address spec) gnus-picon-cache))

               (gnus-article-goto-header header)
               (mail-header-narrow-to-field)
               (case gnus-picon-style
                 (right
                  (when (= (length addresses) 1)
                    (setq len (apply '+ (mapcar (lambda (x)
                                                  (condition-case nil
                                                      (car (image-size (car x)))
                                                    (error 0))) spec)))
                    (when (> len 0)
                      (goto-char (point-at-eol))
                      (insert (propertize
                               " " 'display
                               (cons 'space
                                     (list :align-to (- 78 ;; (window-width)
                                                        1 len))))))
                    (goto-char (point-at-eol))
                    (setq point (point-at-eol))
                    (dolist (image spec)
                      (unless (stringp image)
                        (goto-char point)
                        (gnus-picon-insert-glyph image category 'nostring)))))
                 (inline
                   (when (search-forward address nil t)
                     (delete-region (match-beginning 0) (match-end 0))
                     (setq point (point))
                     (while spec
                       (goto-char point)
                       (if (> (length spec) 2)
                           (insert ".")
                         (if (= (length spec) 2)
                             (insert "@")))
                       (gnus-picon-insert-glyph (pop spec) category))))))))))

     ))

;; TODO: 似乎通常 gnus 启动后，头几封信会发丢失，后面就不会发生了。所以尽量不要
;; 重启 gnus?
(setq gnus-agent-queue-mail nil)

(setq imap-use-utf7 nil)

;; window/buffer/layout configuration
(gnus-add-configuration
 '(article (horizontal 1.0 (summary 0.5 point)
                       (article 1.0))))

;; shortup note
;; article: `K H' -- view text/html mail in external browser

(provide 'xwl-gnus)

;;; xwl-gnus.el ends here
