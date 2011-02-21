;;; xwl-notify.el --- growl like notify

;; Copyright (C) 2010, 2011  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;; notify

(setq xwl-notify-emacs-image (file-truename "~/w32/emacs_4_48x48x32.png"))

;;;###autoload
(defun xwl-notify (title message)
  (case system-type
    ((darwin)
     (xwl-growl title message))
    ((windows-nt)
     (xwl-snarl title message))
    ((gnu/linux)
     (xwl-zenity title message)))
  ;; (xwl-shell-command-asynchronously
  ;;  (concat "mplayer " (file-truename "~/Music/emms-test/白狐狸.mp3")))
  )

;;;###autoload
(defun xwl-growl (title message)
  (xwl-shell-command-asynchronously
   (format "growlnotify --image %s -t \"%s\" -m \"%s\""
           xwl-notify-emacs-image title message)))

;; http://www.fullphat.net/index.php
;; http://tlhan-ghun.de/?q=node/59
;;;###autoload
(defun xwl-snarl (title message)
  (when (featurep 'emms)
    (xwl-shell-command-asynchronously
     (format "Snarl_CMD.exe snShowMessage 5 \"%s\" \"%s\" \"%s\""
             (emms-i18n-iconv 'utf-8 'gb18030 title)
             (emms-i18n-iconv 'utf-8 'gb18030 message)
             xwl-notify-emacs-image))))

;;;###autoload
(defun xwl-zenity (title message)
  (xwl-shell-command-asynchronously
   (format "zenity --info --text \"%s\"" message)))

(provide 'xwl-notify)
;;; xwl-notify.el ends here
