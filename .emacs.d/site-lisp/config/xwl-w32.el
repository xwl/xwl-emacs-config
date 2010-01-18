;;; xwl-vim.el --- w32 specific utilites 

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

(defun xwl-w32-get-drives ()
  "Get a list of drive names from get_drives.py."
  (read
   (shell-command-to-string  
    (concat "python "
	    (shell-quote-argument (expand-file-name "~/w32/get_drives.py"))))))

(setq xwl-w32-drives (xwl-w32-get-drives))

;; Prepend drive name on buffer on w32
(defadvice uniquify-get-proposed-name (after prepend-drive-name activate)
  (let ((d (upcase (substring (ad-get-arg 1) 0 1))))
    (setq ad-return-value
          (concat d "="
                  (cdr (or (assoc d xwl-w32-drives) 
                           (assoc (downcase d) xwl-w32-drives)))
                  ":/" ad-return-value))))

(defun xwl-w32-redirect ()
  (setq xwl-w32-redirect-locally? t)

  ;; ;; redirect
  ;; (let ((cmd (apply 'shell-quote-argument 
  ;;                   (remove-if-not 
  ;;                    'file-exists-p
  ;;                    (mapcar 'expand-file-name
  ;;                            '("~/usr/desproxy/desproxy.exe"
  ;;                              "q:/usr/desproxy/desproxy.exe")))
  ;;                   )))
  ;;   ;; kill old connection?
  ;;   (let ((kill-p nil)
  ;;         (asked-p nil))
  ;;     (dolist (p (process-list))
  ;;       (when (string-match "desproxy" (process-name p))
  ;;         (unless asked-p
  ;;           (setq kill-p (y-or-n-p "Kill old connections? ")
  ;;                 asked-p t))
  ;;         (when kill-p
  ;;           (kill-process p)))))

  ;;   (mapcar (lambda (i)
  ;;             (let ((host (nth 0 i))
  ;;                   (port (nth 1 i))
  ;;                   (local-port (nth 2 i)))
  ;;               (xwl-shell-command-asynchronously
  ;;                (format "%s %s %d %s %d %d"
  ;;                        cmd host port xwl-proxy-server xwl-proxy-port local-port))))
  ;;           ;; host port localport
  ;;           '(("irc.debian.org" 6669 16669)
  ;;             ("irc.freenode.net" 6667 16667)
  ;;             ("irc.lnx.nokia.com" 6668 16668)

  ;;             ("news.gmane.org" 119 10119)
  ;;             ("news.cn99.com" 119 11119)
  ;;             ("imap.gmail.com" 993 10993)

  ;;             ("dict.org" 2628 12628)
  ;;             ;; ("repo.or.cz" 22 10022)
  ;;             ("github.com" 22 10022)

              ;; im.bitlbee.org

              ;; ("123.115.112.196" 22 20022)
              ;; ("125.34.173.96" 5800 5800) ; vnc server
              ;; ("125.34.173.96" 5900 5900)

              ;; ("www.call-with-current-continuation.org" 80 10080)
              ;; )))
  )

(defun xwl-w32-redirect-host ()
  (if xwl-w32-redirect-locally?
      "localhost"
    "172.28.206.207"))

(provide 'xwl-w32)
;;; xwl-w32.el ends here
