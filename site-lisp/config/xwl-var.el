;;; xwl-var.el --- Global variables

;; Copyright (C) 2010, 2011, 2013, 2014 William Xu

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

(setq xwl-w32? (eq system-type 'windows-nt))

(setq xwl-w32-redirect-locally? t)

(defun xwl-at-company ()
  ;; (message "瞅瞅我们是不是在公司网络呢…")
  ;;   (zerop (shell-command
  ;;          "traceroute -w 2 -m 2 www.google.com  | grep abc"))

  ;; (zerop (shell-command "ping -n 1 172.16.42.42"))

  ;; (and (not (eq system-type 'darwin))
  ;;        (or (zerop (shell-command "ipconfig | grep 172.28"))
  ;;            (zerop (shell-command "ipconfig | grep 10.162"))))

  (let (count cmd)
    (case  system-type
      ((windows-nt) (setq count "-n 1"
                          cmd "ipconfig"))
      (t (setq count "-c 1"
               cmd "/sbin/ifconfig")))
    (or (zerop (shell-command (concat cmd " | grep 10.233")))
        (and (string= (user-login-name) "wixu")
             (not (eq system-type 'darwin))
             (zerop (shell-command
                     (format "ping %s 172.16.42.42" count)))))))

(setq xwl-at-company? (xwl-at-company))

(setq xwl-w3m-arguments '())

(setq xwl-black-background? (and (memq system-type '(windows-nt gnu/linux))
                                 (not (member system-name '("3CNL16305" "linux-xwl" )))))

(provide 'xwl-var)

;;; xwl-var.el ends here
