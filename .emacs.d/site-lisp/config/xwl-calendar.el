;;; xwl-calendar.el --- calendar related stuffs

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

(require 'cal-china-x)

(require 'xwl-util)

;; (setq calendar-week-start-day 1)

(setq calendar-latitude +39.55
      calendar-longtitude +116.25
      calendar-location-name "Beijing")
;; changting(+25.52, +116.20)

(global-set-key (kbd "<f12>")
                (lambda () (interactive)
                  (let ((cal "*Calendar*"))
                    (if (get-buffer cal)
                        (progn
                          (split-window-vertically -9)
                          (other-window 1))
                      ;; 禁用垂直分割。
                      (let ((split-width-threshold 9999))
                        (calendar)))
                    (switch-to-buffer cal)
                    (calendar-cursor-holidays))))

(setq appt-issue-message t
      mark-holidays-in-calendar t
      view-calendar-holidays-initially t)

(when (file-readable-p "~/.diary")
  (setq diary-file "~/.diary")
  (setq mark-diary-entries-in-calendar t))

(setq appt-message-warning-time 5)

(setq appt-disp-window-function
      (lambda (min-to-app new-time appt-msg)
        (xwl-notify "appt" (format "\"距約會 %s 還有 %s 分鍾\"" appt-msg min-to-app))
        (appt-disp-window min-to-app new-time appt-msg)))

(defun xwl-current-year ()
  (string-to-number (format-time-string "%Y" (current-time))))

(defun xwl-increment-date (date)
  (calendar-gregorian-from-absolute (1+ (calendar-absolute-from-gregorian date))))

(defun xwl-holiday-fixed-range (month start-day end-day string)
  "[start-day, end-day], range is within a month."
  (let* ((date (list month start-day (xwl-current-year)))
         (ret `((holiday-fixed ,(calendar-extract-month date)
                               ,(calendar-extract-day date)
                               ,string))))
    (while (not (= (calendar-extract-day date) end-day))
      (setq date (xwl-increment-date date))
      (setq ret (append ret `((holiday-fixed ,(calendar-extract-month date)
                                             ,(calendar-extract-day date)
                                             ,string)))))
    ret))

(defun xwl-holiday-lunar-range (month start-day end-day string)
  "[start-day, end-day], range is within a month."
  (let* ((date (list month start-day (xwl-current-year)))
         (ret `((holiday-lunar ,(calendar-extract-month date)
                               ,(calendar-extract-day date)
                               ,string))))
    (while (not (= (calendar-extract-day date) end-day))
      (setq date (xwl-increment-date date))
      (setq ret (append ret `((holiday-lunar ,(calendar-extract-month date)
                                             ,(calendar-extract-day date)
                                             ,string)))))
    ret))

(setq xwl-company-holidays '())
;;       (append (xwl-holiday-lunar-range 1 1 3 "*company 元旦*")
;;               (xwl-holiday-fixed-range 1 25 31 "*company 春节*")
;;               (xwl-holiday-fixed-range 4 4 6 "*company 清明节*")
;;               (xwl-holiday-fixed-range 5 1 3 "*company 劳动节*")
;;               (xwl-holiday-fixed-range 5 28 30 "*company 端午节*")
;;               (xwl-holiday-fixed-range 10 1 8 "*company 国庆节*")))

(setq cal-china-x-important-holidays
      `(,@cal-china-x-chinese-holidays

        (holiday-lunar 5 11 (xds ",f`I,,h<,,J],g\\c..p9"))
        (holiday-lunar 12 30 "過年啦！")
        ;; (holiday-lunar 4 13  (xds "Xd0hYp;edfKecA&dc-Omm@<="))
        ;; (holiday-lunar 6 7 (xds "\\>c_Y*E_XJ9`XOA'X>I_\\J<="))

        (holiday-lunar 7 28 (xds"Xdc_Y`9,[J[q@>AgZeIfQ>F,"))

        (holiday-fixed 4 16 (xds ",Y:G,C`+,X)HA*D^Q>F,@H=="))

        ,@xwl-company-holidays))

(setq rockets-schedule
      '(
        ;; 10.30  休斯敦火箭     孟菲斯灰熊     "08:30 上海体育   CCTV5 新传"
        ))

(setq cal-china-x-general-holidays
      (let ((events '())
            (nba rockets-schedule)
            date event)
        (while nba
          (setq date (car nba))
          (setq event (format "%S %S %S" (nth 1 nba) (nth 2 nba) (nth 3 nba)))
          (setq nba (cdr (cdr (cdr (cdr nba)))))
          (setq events (cons `(holiday-fixed ,(floor date)
                                             ,(% (floor (* 100 date)) 100)
                                             ,event)
                             events)))
        events))

(setq other-holidays
      '((holiday-fixed 10 10 "民国双十节")
        (holiday-fixed 10 11 "靚穎生日快樂！")
	;; (holiday-fixed 3  8  "妇女节")
        (holiday-fixed 3  12 "植树节")
        (holiday-fixed 5  4  "青年节")
        ;; (holiday-fixed 6  1  "儿童节")
        (holiday-fixed 9  10 "教师节")
        (holiday-lunar 1 15 "元宵节")
        (holiday-lunar 7 7  "七夕节")
        (holiday-lunar 9 9  "重阳节")))

(setq calendar-holidays
      (append cal-china-x-important-holidays
              cal-china-x-general-holidays
              other-holidays))

(define-key calendar-mode-map (kbd "j") 'calendar-forward-week)
(define-key calendar-mode-map (kbd "k") 'calendar-backward-week)
(define-key calendar-mode-map (kbd "l") 'calendar-forward-day)
(define-key calendar-mode-map (kbd "h") 'calendar-backward-day)

;; (add-hook 'today-visible-calendar-hook 'calendar-star-date)

(defun xwl-display-diary ()
  (when (eq (face-at-point) 'diary)
    (save-window-excursion
      (message (nth 1 (car (diary-view-entries)))))))

(add-hook 'calendar-move-hook 'xwl-display-diary)

(defadvice insert-diary-entry (around disable-less activate)
  (let ((find-file-hook (remove 'less-minor-mode-on find-file-hook)))
    ad-do-it))

(provide 'xwl-calendar)

;; xwl-calendar.el ends here
