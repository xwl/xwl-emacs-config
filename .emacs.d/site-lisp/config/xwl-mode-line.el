;;; xwl-mode-line.el --- mode line display stuffs

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012 William Xu

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

(setq-default mode-line-format
              `(,@(remq 'mode-line-modes
                        (reverse (cdr (reverse mode-line-format))))
                mode-line-modes
                ,@(last mode-line-format)))

(setq global-mode-string
      '("" appt-mode-string " "
        display-time-string " "
        ;; erc-modified-channels-object ; specified in `mode-line-modes'.
        ;; xwl-memory-usage-string " "
        ;; xwl-week-at-school-string " "
        ;; battery-mode-line-string " "
        ;; xwl-mail-notify-string " "

        ;; cwit-mode-line-string " "
        ;; rcirc-activity-string
        ;; xwl-weather-string

        ;; xwl-gmail-notify-string
        ;; twittering-unread-mode-line-string " "

        emms-mode-line-string
        emms-playing-time-string
        emms-lyrics-string))

;;; Gmail Notify

(when pwgmail
  (setq gmail-notifier-username "william.xwl"
        gmail-notifier-password pwgmail)

  (when xwl-at-company?
    (setq gmail-notifier-curl-command
          (concat "curl "
                  (if (boundp 'xwl-proxy-server)
                      (format "-x %s:%d" xwl-proxy-server xwl-proxy-port)
                    ""))))

  (setq gmail-notifier-timer-interval (* 30 60))

  (add-hook 'gmail-notifier-new-mails-hook
            (lambda ()
              (xwl-notify "Gmail" (format "You've got %d new mails"
                                          (length gmail-notifier-unread-entries)))
              ;; (xwl-shell-command-asynchronously "say Lao-ban, ni you shee you-jan.")
              ))

  (add-hook 'xwl-timers-hook 'gmail-notifier-start)

  )

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
(set-frame-name ;; "菩提本無樹 明鏡亦非台 本來無一物 何處惹塵埃") ; bug in windows 7?
 (concat "the Church of Emacs,  starting from " (current-time-string)))

(defun xwl-frame-fortune-of-day ()
  (set-frame-name
   (replace-regexp-in-string
    " +" " " (replace-regexp-in-string
              "\n" " " (xwl-fortune-favorites
                        "~/.notes/favorites_now")))))

(when (file-exists-p "~/.notes/favorites_now")
  (setq xwl-frame-fortune-of-day-timer
        (run-at-time 0 600 'xwl-frame-fortune-of-day)))


(provide 'xwl-mode-line)

;;; xwl-mode-line.el ends here
