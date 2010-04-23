;;; xwl-mode-line.el --- mode line display stuffs

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
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;; General

(setq xwl-mouse-tooltip "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")

(setq default-mode-line-format
      '("%e"
        #("-" 0 1 (help-echo xwl-mouse-tooltip))
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        mode-line-frame-identification
        mode-line-buffer-identification
        #("   " 0 3 (help-echo xwl-mouse-tooltip))
        mode-line-position
        (vc-mode vc-mode)
        #("  " 0 2 (help-echo xwl-mouse-tooltip))
        (which-func-mode
         ("" which-func-format #("--" 0 2 (help-echo xwl-mouse-tooltip))))
        (global-mode-string
         (#("  " 0 2 (help-echo xwl-mouse-tooltip)) global-mode-string))
        mode-line-modes
        #("-%-" 0 3 (help-echo xwl-mouse-tooltip)))

      mode-line-format default-mode-line-format)

(defun xwl-organize-mode-line ()
  (interactive)
  (setq global-mode-string
        '("" appt-mode-string " "
          display-time-string " "
          ;; erc-modified-channels-object
          ;; xwl-memory-usage-string " "
          ;; xwl-week-at-school-string " "
          ;; battery-mode-line-string " "
          ;; xwl-mail-notify-string " "

          ;; cwit-mode-line-string " "
	  ;; rcirc-activity-string
          ;; xwl-weather-string

          xwl-gmail-notify-string
          twittering-unread-mode-line-string " "

          emms-mode-line-string
          emms-playing-time-string
          emms-lyrics-string))
  (force-mode-line-update))

(xwl-organize-mode-line)

;;; Gmail Notify

(require 'xml)

(setq xwl-gmail-user "william.xwl"
      xwl-gmail-password pwgmail)

(setq xwl-gmail-notify-string "")

(defun xwl-gmail-notify ()
  (unless xwl-gmail-password
    (setq xwl-gmail-password (read-passwd "Gmail password: ")))
  (xwl-gmail-get-unread xwl-gmail-user xwl-gmail-password)
  (when xwl-at-company?
    (xwl-gmail-get-unread xwl-gmail-user1 xwl-gmail-password1)))

(defun xwl-gmail-get-unread (user password)
  (xwl-shell-command-asynchronously-with-callback
   (format "curl -s --user \"%s:%s\" %s https://mail.google.com/mail/feed/atom"
           user password (if (boundp 'xwl-proxy-server)
                             (format "-x %s:%d" xwl-proxy-server xwl-proxy-port)
                           ""))
   'xwl-gmail-notify-callback))

(defun xwl-gmail-notify-callback ()
  (let* ((xml-list (xml-parse-region (point-min) (point-max)))
         (get-node-value (lambda (node)
                           (some (lambda (n)
                                   (when (and (listp n) (eq (car n) node))
                                     (car (reverse n))))
                                 (cdar xml-list))))
         (account (replace-regexp-in-string
                   ".* +\\([^ ]+\\)@.*" "\\1" (funcall get-node-value 'title)))
         (unread (string-to-number (funcall get-node-value 'fullcount))))
    (xwl-gmail-notify-format account unread)
    (kill-buffer)))

;; FIXME: this is toooo tedious..
(defun xwl-gmail-notify-format (account unread)
  (if xwl-at-company?
      (progn
        (cond ((string= account xwl-gmail-user)
               (if (string= xwl-gmail-notify-string "")
                   (setq xwl-gmail-notify-string (format "g(%d,0) " unread))
                 (setq xwl-gmail-notify-string
                       (replace-regexp-in-string
                        "([0-9]+," (format "(%d," unread) xwl-gmail-notify-string))))
              ((string= account xwl-gmail-user1)
               (if (string= xwl-gmail-notify-string "")
                   (setq xwl-gmail-notify-string (format "g(0,%d) " unread))
                 (setq xwl-gmail-notify-string
                       (replace-regexp-in-string
                        ",[0-9]+" (format ",%d" unread) xwl-gmail-notify-string)))))
        (when (string-match "g(0,0)" xwl-gmail-notify-string)
          (setq xwl-gmail-notify-string "")))
    (if (zerop unread)
        (setq xwl-gmail-notify-string "")
      (setq xwl-gmail-notify-string (format "g(%d) " unread))))
  (force-mode-line-update))

(add-hook 'xwl-timers-hook (lambda ()
                             (setq xwl-gmail-notify-timer
                                   (run-with-timer 0 (* 60 5) 'xwl-gmail-notify))))

;;; Misc

;; memory status
(setq xwl-memory-usage-string "")

(defun xwl-memory-usage-update-handler ()
  "Update memory usage report on mode line."
  (setq xwl-memory-usage-string
        (concat
         (car
          (split-string
           (shell-command-to-string
            "free -m | grep cache: | awk '{ print $3}'")))
         "M"))
  (force-mode-line-update))

(when (string= (xwl-os-type) "linux-gnu")
  (run-with-timer 0 60 'xwl-memory-usage-update-handler))

;; mail status (through fetchmail)
(setq xwl-mail-notify-string "")

(defun xwl-mail-notify-update-handler ()
  (interactive)
  (let ((new
         (car
          (split-string
           (shell-command-to-string
            "grep ^From ~/incoming_mail/default | wc -l")))))
    (if (string= new "0")
        (setq xwl-mail-notify-string "")
      (setq xwl-mail-notify-string (format "Mail(%s)" new))
      (force-mode-line-update)
;;       (shell-command
;;        (format "~/bin/growlnotify -a Emacs.app -m '收到 %s 封信啦!'" new))
      )))

;; (setq xwl-mail-notify-update-handler-timer
;;       (run-with-timer 0 120 'xwl-mail-notify-update-handler))

;; frame title
(setq frame-title-format "菩提本无树 明镜亦非台 本来无一物 何处惹尘埃")

(defun xwl-frame-fortune-of-day ()
  (setq frame-title-format
        (replace-regexp-in-string
         " +" " " (replace-regexp-in-string
                   "\n" " " (xwl-fortune-favorites
                             "~/notes/favorites_now")))))

(when (file-exists-p "~/notes/favorites_now")
  (setq xwl-frame-fortune-of-day-timer
        (run-at-time 0 600 'xwl-frame-fortune-of-day)))


(provide 'xwl-mode-line)

;;; xwl-mode-line.el ends here
