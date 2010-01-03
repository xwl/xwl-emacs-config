;;; xwl-erc.el --- erc config

;; Copyright (C) 2007, 2008, 2009 William Xu

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

;;; Erc - irc client, bitlbee

;; ERC (M-x erc-select)
;; Set system locale to zh_CN.utf-8 first!
;; /me do sth; /ctcp xwl version
;; http://rafb.net/paste

;; irc.pchome.net:7000
;; 211.92.88.40:7000 #linuxfire
;; irc.debian.org #debian-zh

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

;; autojoin

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

(setq erc-join-buffer 'bury
      erc-auto-query 'bury)

;; match & track
(require 'erc-match)
(erc-match-mode 1)
(setq erc-current-nick-highlight-type 'nick-or-keyword)
(setq erc-keywords '("xwl" "emms"))
(setq erc-pals nil)

(defface erc-query-buffer-face '((t (:bold t :foreground "magenta")))
  "ERC face for your query buffers."
  :group 'erc-faces)

;; erc-join-hook
;; (defun xwl-erc-join-hook ()
;;   (erc-match-mode 1)

;; (global-set-key (kbd "C-c C-2") 'erc-track-switch-buffer)

(setq erc-track-faces-priority-list
      '(erc-query-buffer-face
        erc-current-nick-face
	erc-keyword-face
	erc-pal-face
	erc-default-face
	))

(setq erc-track-priority-faces-only 'all)

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

(require 'erc-track)

(setq erc-track-switch-direction 'importance)

;; (defun xwl-erc-track-switch-buffer-by-face-priority ()
;;   "Run `erc-track-switch-buffer' but by `erc-track-faces-priority-list'."
;;   (interactive)
;;   (when (and erc-track-mode erc-modified-channels-alist)
;;     (let ((buf nil)                     ; buf to switch to firstly
;;           (faces erc-track-faces-priority-list)
;;           (channels-1 erc-modified-channels-alist)
;;           (channels erc-modified-channels-alist)
;;           (f nil)
;;           (c nil))
;;       (while faces
;;         (setq f (car faces))
;;         (setq channels channels-1)
;;         (while channels
;;           (setq c (car channels))
;;           (if (eq (cddr c) f)
;;               (setq channels nil
;;                     faces nil
;;                     buf (car c))
;;             (setq channels (cdr channels))))
;;         (setq faces (cdr faces)))
;;       (if buf
;;           (switch-to-buffer buf)
;;         (call-interactively 'erc-track-switch-buffer)))))

;; (global-set-key (kbd "C-c C-@") 'xwl-erc-track-switch-buffer-by-face-priority)
;; (global-set-key (kbd "C-c RET") 'xwl-erc-track-switch-buffer-by-face-priority)

;; fill
(require 'erc-fill)
(erc-fill-mode 1)
(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 10
      erc-fill-prefix "      ")

;; timestamp
(require 'erc-stamp)
(erc-timestamp-mode 1)

(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%H:%M ")

;; (setq xwl-erc-timestamp-last nil)

;; (load "~/.erc/.timestamp-last.el")

;; (defun xwl-erc-insert-timestamp-left-by-day (string)
;;   "If last timestamp is one or more "
;;   (if (or (null xwl-erc-timestamp-last)  ; nil
;;           (> (- (time-to-days (current-time)) ; >=1 days
;;                 (time-to-days xwl-erc-timestamp-last))
;;              0)
;;           (< (string-to-number (format-time-string "%H")) ; 18-00-03
;;              (string-to-number
;;               (format-time-string "%H" xwl-erc-timestamp-last))))
;;       (progn
;;         (setq xwl-erc-timestamp-last (current-time))
;;         (erc-insert-timestamp-left
;;          (erc-format-timestamp
;;           (current-time) (concat "%Y.%m.%d " erc-timestamp-format)))
;;         (let ((buf (find-file-noselect "~/.erc/.timestamp-last.el")))
;;           (with-current-buffer buf
;;             (let ((inhibit-read-only t))
;;               (kill-region (point-min) (point-max))
;;               (insert (format "(setq xwl-erc-timestamp-last '%s)\n"
;;                               (current-time)))
;;               (save-buffer)))
;;           (kill-buffer buf)))
;;     (erc-insert-timestamp-left
;;      (erc-format-timestamp (current-time) erc-timestamp-format))))

(setq erc-insert-timestamp-function
      ;; 'xwl-erc-insert-timestamp-left-by-day)
      'erc-insert-timestamp-left)

;; spelling, TODO, fix this.
;; (require 'erc-spelling)
;; (erc-spelling-mode -1)

;; ignore
(setq erc-ignore-list nil)
(setq erc-hide-list 
      '("JOIN" "PART" "QUIT" "MODE"))

;; ;; sound
;; (erc-sound-enable)
;; (setq erc-sound-path "/home/william/music/sound")
;; (setq erc-default-sound "/home/william/music/sound/reflection.mp4")
;; (setq erc-play-command "mplayer")

;; log
(require 'erc-log)
(erc-log-mode 1)
(setq erc-log-channels-directory "~/var/erc/"
      erc-save-buffer-on-part t
      erc-log-file-coding-system 'utf-8
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(unless (file-exists-p erc-log-channels-directory)
  (mkdir erc-log-channels-directory t))

;; FIXME
;; bbdb
;; (require 'erc-bbdb)
;; (erc-bbdb-mode 1)
;; (setq erc-bbdb-popup-type nil)

;; goodies
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

  (define-key erc-mode-map (kbd "M-m") 'erc-bol))

(add-hook 'erc-mode-hook 'xwl-erc-mode-hook)

(defun xwl-erc-select ()
  (interactive)
  (if xwl-at-company-p
      (let ((sv "localhost")
            (nick "xwl__"))
        (when (eq system-type 'windows-nt)
          (setq sv "172.28.206.207")
          (setq nick "xwl_"))

        (erc-select :server sv :port 16667 :nick nick :password pwerc) 
        (erc-select :server sv :port 16669 :nick nick :password pwdeb) 
        (erc-select :server sv :port 16668 :nick nick :password pwerc))
    (erc-select :server "irc.debian.org" ; "localhost"
                :port 6669               ; 16669
                :nick "xwl"
                :password pwdeb)

    (erc-select :server "irc.freenode.net" ; "localhost"
                :port 6667                 ; 16667
                :nick "xwl"
                :password pwerc)

;;   (erc-select :server "irc.linuxfire.com.cn"
;;               :port 6667
;;               :nick "xwl"
;;               :password "")

;;;   (erc-select :server "irc.mozilla.org"
;;; 	      :port 6667
;;; 	      :nick "xwl"
;;; 	      :password "")

 ;; (when (and (not (get-buffer "localhost:6667"))
 ;;            (eq system-type 'darwin))
 ;;   (erc-select :server "localhost"
 ;;               :port 6667
 ;;               :nick "william"
 ;;               :password pwbitlbee))

;;   (erc-select :server "nsx-r"
;; 	      :port 6667
;; 	      :nick "william"
;; 	      :password "")
  ))

(global-set-key (kbd "C-c n E") 'xwl-erc-select)

(add-hook 'erc-join-hook 'less-minor-mode-on)

(erc-truncate-mode 1)

(add-hook 'erc-mode-hook (lambda () (abbrev-mode 1)))

;; auto identify
(defun xwl-erc-auto-identify (server nick)
  (unless (string-match "localhost" server) ; bitlbee
    (erc-message "PRIVMSG"
                 (format "NickServ identify %s" pwbitlbee))))

(add-hook 'erc-after-connect 'xwl-erc-auto-identify)

;; auto identify bitblee
(defun his-bitlbee-identify ()
   "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
   (when (and (string= "localhost" erc-session-server)
              (string= "&bitlbee" (buffer-name)))
     (erc-message "PRIVMSG" (format "%s identify %s"
                                    (erc-default-target)
                                    pwbitlbee))))

(add-hook 'erc-join-hook 'his-bitlbee-identify)

;; auto op
(defun xwl-erc-auto-op ()
  (let ((b (buffer-name)))
    (cond
     ((string= b "#emacs-cn")
      (erc-message "PRIVMSG" (concat "chanserv op " b)))
     ((string= b "#avkon")
      (erc-message "PRIVMSG" (concat "userserv login xwl " pwerc))
      (erc-message "PRIVMSG" (concat "chanserv op " b))))))

(add-hook 'erc-join-hook 'xwl-erc-auto-op)

(when (xwl-tty-p)
  (global-set-key (kbd "C-c RET") (kbd "C-c C-@")))

(provide 'xwl-erc)

;;; xwl-erc.el ends here
