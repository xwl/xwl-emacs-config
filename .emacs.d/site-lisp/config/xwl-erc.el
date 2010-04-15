;;; xwl-erc.el --- erc config

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
;; | Erc - irc client, bitlbee
;; `----

;; ERC (M-x erc-select)
;; Set system locale to zh_CN.utf-8 first!
;; /me do sth; /ctcp xwl version
;; http://rafb.net/paste

;; irc.pchome.net:7000
;; 211.92.88.40:7000 #linuxfire
;; irc.debian.org #debian-zh

;; Send offline message to registered user on freenode:
;;   /msg MemoServ send ID message

(setq erc-track-enable-keybindings t)

(require 'erc)

(setq erc-echo-notices-in-minibuffer-flag t
      erc-default-coding-system '(utf-8 . utf-8)
      erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit))
      erc-kill-buffer-on-part t
      erc-auto-query t)

(setq erc-nick "xwl"
      erc-user-full-name "William Xu")

(setq erc-common-server-suffixes nil
      erc-mode-line-format "%t %a")

(when (xwl-tty-p)
  (global-set-key (kbd "C-c RET") (kbd "C-c C-@")))

(setq erc-join-buffer 'bury
      erc-auto-query 'bury)

;; ,----
;; | autojoin, identify, op
;; ----

(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("rootdir.de"
         "&bitlbee")
        ("freenode.net"
         "#osxchat"
         ;; "#bsdchat"
         ;; "#sxemacs"
         "#emacs" ;; "#guile" "#sawfish" "#haskell"
         ;; "#org-mode"
         ;; "#gnus"
         "#scheme"
         "#chicken"
         ;; "#gentoo-alt"
         ; "#gentoo-cn" ;; "#gentoo-ppc" "#conkeror"
         "#cpp-tw"
         ;; "#fink"
         ;; "#macdev"
         ;; "#macosx"
         ;; "#pkgsrc"
         "#beijinglug"
         )
        ("oftc.net"
         "#debian-zh"
         "#emacs-cn"
         ;; "#bitlbee"
         )
;;         ("linuxfire"
;;          "#linuxfire")

        ("irc.lnx.nokia.com"
         "#avkon" "#orbit" "#mac" "#linux" "#symbianperformance" "#qt")

        ))

;; Password has already been provided in erc-select call:
;;
;; (defun xwl-erc-auto-identify (server nick)
;;   (unless (string-match "localhost" server) ; bitlbee
;;     (erc-message "PRIVMSG"
;;                  (format "NickServ identify %s" pwbitlbee))))

;; (add-hook 'erc-after-connect 'xwl-erc-auto-identify)

(defun his-bitlbee-identify ()
   "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
   (when (and (string= "localhost" erc-session-server)
              (string= "&bitlbee" (buffer-name)))
     (erc-message "PRIVMSG" (format "%s identify %s"
                                    (erc-default-target)
                                    pwbitlbee))))

(add-hook 'erc-join-hook 'his-bitlbee-identify)

(defun xwl-erc-auto-op ()
  (let ((b (buffer-name)))
    (cond
     ((string= b "#emacs-cn")
      (erc-message "PRIVMSG" (concat "chanserv op " b)))
     ((string= b "#avkon")
      (erc-message "PRIVMSG" (concat "userserv login xwl " pwerc))
      (erc-message "PRIVMSG" (concat "chanserv op " b))))))

(add-hook 'erc-join-hook 'xwl-erc-auto-op)

;; ,----
;; | match, ignore
;; `----

(require 'erc-match)
(erc-match-mode 1)
(setq erc-current-nick-highlight-type 'nick-or-keyword)
(setq erc-keywords '("xwl" "emms"))
(setq erc-pals nil)

(defface erc-query-buffer-face '((t (:bold t :foreground "magenta")))
  "ERC face for your query buffers."
  :group 'erc-faces)

(defun xwl-toggle-erc-busy ()
  "Toggle `erc-default-face' in `erc-track-faces-priority-list'
so as to keep an eye on work when necessarily."
  (interactive)
  (if (memq 'erc-default-face erc-track-faces-priority-list)
      (progn
	(setq erc-track-faces-priority-list
	      (remove 'erc-default-face
		      erc-track-faces-priority-list))
	(message "Keep an eye on work"))
    (setq erc-track-faces-priority-list
	  (append erc-track-faces-priority-list
		  '(erc-default-face)))
    (message "Ah, time for tea")))

(global-set-key (kbd "C-c n e") 'xwl-toggle-erc-busy)

(setq erc-ignore-list nil)
(setq erc-hide-list
      '("JOIN" "PART" "QUIT" "MODE"))

;; ,----
;; | track, fill
;; `----

(require 'erc-track)

(setq erc-track-showcount t)

(setq erc-track-switch-direction 'importance)

(setq erc-track-faces-priority-list
      '(erc-query-buffer-face
        erc-current-nick-face
	erc-keyword-face
	erc-pal-face
	erc-default-face
	))

(setq erc-track-priority-faces-only 'all)

;; (defadvice erc-track-switch-buffer (before place-point-to-bottom activate)
;;   (when (memq (current-buffer) (erc-buffer-list))
;;     (goto-char (point-max))
;;     (forward-line -1)))

(require 'erc-fill)
(erc-fill-mode 1)
(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 10
      erc-fill-prefix "      ")

;; trim erc nicks
(setq erc-format-nick-function 'xwl-erc-format-nick)

(defun xwl-erc-format-nick (&optional user channel-data)
  "Like `erc-format-nick' but trim nick to a fixed length. "
  (let ((nick (erc-format-nick user channel-data)))
    (when (> (length nick) 7)
      (setq nick (concat (substring nick 0 4)
                         ".."
                         (substring (substring nick 7) -1))))
    nick))

;; ,----
;; | timestamp
;; `----

(require 'erc-stamp)
(erc-timestamp-mode 1)

(setq erc-timestamp-only-if-changed-flag t
      erc-timestamp-format "%H:%M ")

(setq erc-insert-timestamp-function
      ;; 'erc-insert-timestamp-left
      'ks-timestamp)

(setq xwl-erc-datestamp-format " === [%a(%V) %Y/%m/%d] ===\n")

(defvar xwl-erc-last-datestamp nil)
(make-variable-buffer-local 'xwl-erc-last-datestamp)

(defun ks-timestamp (string)
  (erc-insert-timestamp-left string)
  (let ((datestamp (erc-format-timestamp (current-time)
                                         xwl-erc-datestamp-format)))
    (unless (string= datestamp xwl-erc-last-datestamp)
      (erc-insert-timestamp-left datestamp)
      (setq xwl-erc-last-datestamp datestamp))))

;; ,----
;; | log
;; `----

(require 'erc-log)
(erc-log-mode 1)
(setq erc-log-channels-directory "~/var/erc/"
      erc-save-buffer-on-part t
      erc-log-file-coding-system 'utf-8
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(unless (file-exists-p erc-log-channels-directory)
  (mkdir erc-log-channels-directory t))

;; ,----
;; | notify
;; `----

(defun xwl-notify (title message)
  (case system-type
    ((darwin)
     (xwl-growl title message))))

(setq growlnotify-command (executable-find "growlnotify"))

(defun xwl-growl (title message)
  (start-process "growl" " growl" growlnotify-command title "-a" "Emacs")
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))

(defun xwl-erc-text-matched-hook (match-type nickuserhost message)
  "Shows a growl notification, when user's nick was mentioned.
If the buffer is currently not visible, makes it sticky."
  (when (and (erc-match-current-nick-p nickuserhost message)
             (not (string-match (regexp-opt '("Users"
                                              "User"
                                              "topic set by"
                                              "Welcome to "
                                              "nickname"
                                              "identified"
                                              "invalid"
                                              ))
                                message)))
    (let ((s (concat "ERC: " (buffer-name (current-buffer)))))
      (case system-type
        ((darwin)
         (xwl-growl s message))
        ((gnu/linux)
         (xwl-shell-command-asynchronously
          (format "zenity --info --text \"%s\"" s)))))))

(add-hook 'erc-text-matched-hook 'xwl-erc-text-matched-hook)

(defun xwl-erc-PRIVMSG (proc parsed)
  (let ((buf (buffer-name (current-buffer))))
    (unless (string-match ":\\|&" buf)
      (let ((s (concat "ERC: " (buffer-name (current-buffer)))))
        (case system-type
          ((darwin)
           (xwl-growl s message))
          ((gnu/linux)
           (xwl-shell-command-asynchronously
            (format "zenity --info --text \"%s\"" s))))))))

(add-hook 'erc-server-PRIVMSG-functions 'xwl-erc-PRIVMSG)

;; ,----
;; | misc
;; `----

(require 'erc-goodies)
(erc-readonly-mode 1)
(erc-smiley-mode 1)

(defun xwl-erc-cmd-WHOIS (nick)
  "Run /whois easily by key sequences."
  (interactive
   (list
    (ido-completing-read
     "/whois "
     (erc-get-channel-nickname-list))))
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (concat "/whois " nick))
    (erc-send-current-line)
    (goto-char (point-max))))

(defun xwl-erc-cmd-bitlbee-blist ()
  "Run `blist' easily by key sequences."
  (interactive)
  (if (string= "&bitlbee" (buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "blist")
        (erc-send-current-line)
        (goto-char (point-max)))
    (message "not in &bitlbee buffer")))

(defun xwl-erc-mode-hook ()
  (auto-fill-mode -1)

  (define-key erc-mode-map (kbd "C-c C-w") 'xwl-erc-cmd-WHOIS)
  (define-key erc-mode-map (kbd "C-c C-b") 'xwl-erc-cmd-bitlbee-blist)

  (define-key erc-mode-map (kbd "M-m") 'erc-bol)

  (define-key erc-mode-map (kbd "q") (lambda ()
                                       (interactive)
                                       (if less-minor-mode
                                           (xwl-hide-buffer)
                                         (insert "q"))))
  )

(add-hook 'erc-mode-hook 'xwl-erc-mode-hook)

(defun xwl-erc-select ()
  (interactive)

  (unless pwbitlbee
    (setq pwbitlbee (read-passwd "irc password: "))
    (setq pwerc pwbitlbee
          pwdeb pwbitlbee))

  (if xwl-at-company?
      (let ((sv "localhost")
            (nick "xwl__"))
        (when (eq system-type 'windows-nt)
          (setq sv (xwl-redirect-host))
          (setq nick "xwl_"))

        (erc :server sv :port 16667 :nick nick :password pwerc)
        (erc :server sv :port 16669 :nick nick :password pwdeb)
        (erc :server sv :port 16668 :nick nick :password pwerc))

    (erc :server "irc.debian.org"       :port 6669 :nick "xwl" :password pwdeb)
    (erc :server "irc.freenode.net"     :port 6667 :nick "xwl" :password pwerc)

    ;; (erc :server "irc.linuxfire.com.cn" :port 6667 :nick "xwl" :password "")
    ;; (erc :server "irc.mozilla.org"      :port 6667 :nick "xwl" :password "")
    ))

(defun xwl-erc-select-bitlbee ()
  (interactive)
  (if (get-buffer "&bitlbee")
      (message "Buffer &bitlbee already exists?")
    (erc-select :server "localhost"
                :port (if xwl-at-company? 26667 6667)
                :nick "william"
                :password pwbitlbee)))

(global-set-key (kbd "C-c n E") 'xwl-erc-select)

(add-hook 'erc-join-hook 'less-minor-mode-on)

(erc-truncate-mode 1)

(add-hook 'erc-mode-hook (lambda () (abbrev-mode 1)))

;; spelling, TODO, fix this.
;; (require 'erc-spelling)
;; (erc-spelling-mode -1)

;; ;; sound
;; (erc-sound-enable)
;; (setq erc-sound-path "/home/william/music/sound")
;; (setq erc-default-sound "/home/william/music/sound/reflection.mp4")
;; (setq erc-play-command "mplayer")

;; FIXME
;; bbdb
;; (require 'erc-bbdb)
;; (erc-bbdb-mode 1)
;; (setq erc-bbdb-popup-type nil)

;;; Local Variables: ***
;;; outline-regexp: ";; | " ***
;;; End: ***

(provide 'xwl-erc)

;;; xwl-erc.el ends here
