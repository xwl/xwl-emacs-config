;;; xwl-erc.el --- erc config

;; Copyright (C) 2007, 2008, 2009, 2010, 2011 William Xu

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

(require 'xwl-util)

;; ,----
;; | general
;; `----

;; irc.pchome.net:7000

;; Send offline message to registered user on freenode:
;;   /msg MemoServ send ID message

(setq erc-echo-notices-in-minibuffer-flag t
      erc-default-coding-system '(utf-8 . utf-8)
      erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit))
      erc-kill-buffer-on-part t
      erc-auto-query t)

(setq erc-nick "xwl")

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
      '(;; ("rootdir.de"
        ;;  "&bitlbee")
        ;; ("freenode.net"
        ;;  "#osxchat" "#emacs" "#scheme" "#chicken" "#cpp-tw" ;; "#chinalug"
        ;;  ; "#qt-qml" "#qt"
        ;;  )
        ("oftc.net"
         ;; "#debian-zh"
         "#emacs-cn")      ; "#bitlbee"
        ;; ("linuxfire"
        ;;  "#linuxfire")
        ;; ("irc.lnx.nokia.com"
        ;;  "#mac" "#linux" "#symbianperformance")
        ))

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
(erc-match-mode 1)
(setq erc-current-nick-highlight-type 'nick-or-keyword)
(setq erc-keywords '("xwl" "emms"))
(setq erc-pals nil)

;; (eval-after-load 'erc-match
;;   '(progn
;;      (set-face-foreground erc-query-buffer-face "magenta")
;;      ))

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

(erc-track-mode 1)

(setq erc-track-showcount t
      erc-track-enable-keybindings t)

(setq erc-track-switch-direction 'importance
      erc-track-faces-priority-list
      '(erc-query-buffer-face
        erc-current-nick-face
	erc-keyword-face
	erc-pal-face
	erc-default-face))

(setq erc-track-priority-faces-only 'all)

(defadvice erc-track-switch-buffer (around place-point-to-bottom activate)
  (cond
   ((memq (current-buffer) (erc-buffer-list))
    (goto-char (point-max))
    (forward-line -1)
    ad-do-it)
   ((and (fboundp 'twittering-buffer-p)
         (twittering-buffer-p)
         twittering-unread-status-info)
    (switch-to-buffer (caar twittering-unread-status-info)))
   (t
    ad-do-it)))

(erc-fill-mode 1)
(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 10
      erc-fill-prefix nil)

;; trim erc nicks
(setq erc-format-nick-function 'xwl-erc-format-nick)

(defun xwl-erc-format-nick (&optional user channel-data)
  "Like `erc-format-nick' but trim nick to a fixed length. "
  (let ((nick (erc-format-nick user channel-data)))
    (when (> (length nick) 7)
      (setq nick (concat (substring nick 0 4)
                         ".."
                         (substring nick -1))))
    nick))

;; (defadvice erc-faces-in (around add-query-faces activate)
;;   (let ((faces ad-do-it))
;;     (when (or (erc-query-buffer-p) (string-match "&" (buffer-name)))
;;       (add-to-list 'faces 'erc-query-buffer-face))
;;     faces))

(eval-after-load 'erc-track
  '(progn
     (defun erc-faces-in (str)
       "Return a list of all faces used in STR."
       (let ((i 0)
             (m (length str))
             (faces (erc-list (get-text-property 0 'face str))))
         (while (and (setq i (next-single-property-change i 'face str m))
                     (not (= i m)))
           (dolist (face (erc-list (get-text-property i 'face str)))
             (add-to-list 'faces face)))
         ;; special faces for query & group(like msn groups) buffers
         (when (or (erc-query-buffer-p)
                   (string-match "&" (buffer-name)))
           (add-to-list 'faces 'erc-query-buffer-face))
         faces))
     ))

;; ,----
;; | timestamp
;; `----

(erc-timestamp-mode 1)

(setq erc-timestamp-only-if-changed-flag t
      erc-timestamp-format "%H:%M ")

(setq erc-insert-timestamp-function 'erc-insert-timestamp-left)

(setq xwl-erc-datestamp-format " === [%a(%V) %Y/%m/%d] ===\n")

(defvar xwl-erc-last-datestamp nil)
(make-variable-buffer-local 'xwl-erc-last-datestamp)

(defadvice erc-insert-timestamp-left (around insert-datestamp activate)
  ad-do-it
  (let ((datestamp (erc-format-timestamp (current-time)
                                         xwl-erc-datestamp-format)))
    (unless (string= datestamp xwl-erc-last-datestamp)
      (ad-set-arg 0 datestamp)
      ad-do-it
      (setq xwl-erc-last-datestamp datestamp))))

;; ,----
;; | log
;; `----

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

(defun xwl-erc-text-matched-hook (match-type nickuserhost message)
  "Shows a growl notification, when user's nick was mentioned.
If the buffer is currently not visible, makes it sticky."
  (when (and (erc-match-current-nick-p nickuserhost message)
             (not (string-match
                   (regexp-opt
                    '("Users" "User" "topic set by" "Welcome to " "nickname"
                      "identified" "invalid" "your unique" "now you hidden"
                      "identified for" "nickname" "your hidden host"))
                   message)))
    (xwl-notify (concat "ERC: " (buffer-name)) message)))

(add-hook 'erc-text-matched-hook 'xwl-erc-text-matched-hook)

;; (defun xwl-erc-PRIVMSG (proc parsed)
;;   (let ((b (buffer-name)))
;;     (unless (string-match ":\\|&" b)
;;       (xwl-notify (concat "****** ERC: " b) message))))

;; (unless (eq system-type 'windows-nt)
;;   (add-hook 'erc-server-PRIVMSG-functions 'xwl-erc-PRIVMSG))

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

(defun xwl-erc-select-bitlbee ()
  (interactive)
  (if (get-buffer "&bitlbee")
      (message "Buffer &bitlbee already exists?")
    (erc-select :server "localhost"
                :port (if xwl-at-company? 26667 6667)
                :nick "william"
                :password pwbitlbee)))

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

;; "<nick>" => "nick | "

(setq xwl-vertical-bar "｜") ; "|"

(defun erc-format-privmessage (nick msg privp msgp)
  "Format a PRIVMSG in an insertible fashion."
  (let* ((mark-s (if msgp (if privp "*" "") "-"))
	 (mark-e (if msgp (if privp "*" (concat " " xwl-vertical-bar)) "-"))
	 (str	 (format "%s%s%s %s" mark-s nick mark-e msg))
	 (nick-face (if privp 'erc-nick-msg-face 'erc-nick-default-face))
	 (msg-face (if privp 'erc-direct-msg-face 'erc-default-face)))
    ;; add text properties to text before the nick, the nick and after the nick
    (erc-put-text-property 0 (length mark-s) 'face msg-face str)
    (erc-put-text-property (length mark-s) (+ (length mark-s) (length nick))
			   'face nick-face str)
    (erc-put-text-property (+ (length mark-s) (length nick)) (length str)
			   'face msg-face str)
    str))

(defun erc-format-my-nick ()
  "Return the beginning of this user's message, correctly propertized."
  (if erc-show-my-nick
      (let ((open "")
	    (close (concat " " xwl-vertical-bar " "))
	    (nick (erc-current-nick)))
	(concat
	 (erc-propertize open 'face 'erc-default-face)
	 (erc-propertize nick 'face 'erc-my-nick-face)
	 (erc-propertize close 'face 'erc-default-face)))
    (let ((prefix (concat " " xwl-vertical-bar " ")))
      (erc-propertize prefix 'face 'erc-default-face))))

(defun erc-fill-static ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (save-match-data
    (goto-char (point-min))
    (looking-at "^\\(\\S-+\\)")
    (let ((nick (match-string 1)))
        (let ((fill-column (- erc-fill-column (erc-timestamp-offset)))
              (fill-prefix (make-string (+ 1 erc-fill-static-center) 32)))
          (insert (make-string (max 0 (- erc-fill-static-center
                                         (length nick)
                                         1
                                         2 ; "| "
                                         ))
                               32))
          (erc-fill-regarding-timestamp))
        (erc-restore-text-properties))))

(defadvice erc-open (around disable-read-only activate)
  (let ((inhibit-read-only t))
    ad-do-it))


;;; Local Variables: ***
;;; outline-regexp: ";; | " ***
;;; End: ***

(provide 'xwl-erc)

;;; xwl-erc.el ends here
