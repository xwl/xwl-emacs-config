;;; xwl-var.el --- Global variables

;; Copyright (C) 2010 William Xu

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

(setq xwl-w32-redirect-locally? nil)

(defun xwl-at-company ()
  (message "瞅瞅我们是不是在公司网络呢…")
  ;;   (zerop (shell-command
  ;;          "traceroute -w 2 -m 2 www.google.com  | grep abc"))

  ;; (zerop (shell-command "ping -c 1 172.28.8.246"))

  ;; (and (not (eq system-type 'darwin))
;;        (or (zerop (shell-command "ipconfig | grep 172.28"))
;;            (zerop (shell-command "ipconfig | grep 10.162"))))

  ;; (string= (user-login-name) "wixu"))
  (not (eq system-type 'darwin)))

(setq xwl-at-company? (xwl-at-company))

(setq xwl-w3m-arguments '())

(provide 'xwl-var)

;;; xwl-var.el ends here
