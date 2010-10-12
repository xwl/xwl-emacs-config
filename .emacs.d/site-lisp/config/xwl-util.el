;;; xwl-util.el --- Utility functions

;; Copyright (C) 2007, 2008, 2009, 2010  William Xu

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

(require 'cl)
(require 'url-extra)

(defun xwl-compat-select (list predicate)
  "Return `cdr' of matched element in LIST by applying PREDICATE on
`car' of elements.

This is useful for sharing .emacs on multiple platforms, where
each OS has different set of tools. "
  (let ((l list)
        (ret nil))
    (while l
      (if (funcall predicate (caar l))
          (progn
            (setq ret (cadar l))
            (setq l nil))
        (setq l (cdr l))))
    (if (stringp ret)
        ret
      (eval ret))))

(defun xwl-compat-select-by-executable (list)
  (xwl-compat-select list 'executable-find))

(defun xwl-compat-select-by-window-system (list)
  (xwl-compat-select list (lambda (w) (eq window-system w))))

(defun xwl-shell-command-asynchronously (cmd)
  (start-process-shell-command cmd nil cmd))

(defun xwl-hide-buffer ()
  "Hide current buffer, and enlarge the other one if exists."
  (interactive)
  ;; (xwl-highlight-changes-for-some-buffer)
  (delete-windows-on (buffer-name)))

(defun xwl-os-type ()
  "Return envrionment $OSTYPE."
  (interactive)
  (message (car (split-string (shell-command-to-string "echo $OSTYPE")))))

;; Should be re-defun later.
(defun xds (any) "abcdefg")
(defun xes (any) "abcdefg")

(defun xwl-switch-or-create (buffer fun)
  "Switch to BUFFER when it exists, else create it with FUN."
  (let ((b (get-buffer buffer)))
    (if b
        (switch-to-buffer b)
      (funcall fun))))

(defun xwl-check-holidays ()
  (calendar)
  (with-current-buffer "*Calendar*"
    (or (ignore-errors (diary-view-entries))
        (when (check-calendar-holidays (calendar-current-date))
          (calendar-cursor-holidays)))))

(defun xwl-running-daily ()
  "Staffs to run daily."
  (xwl-check-holidays)
  (plan))

(defun xwl-insert-date ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d" (current-time))))

(defun xwl-update-date ()
  "Auto update '[Ll]ast [Uu]pdated:' part if exists when saving.
This should not affect `buffer-undo-list'."
  (interactive)
  (let ((old-list buffer-undo-list))
    (save-excursion
      (beginning-of-buffer)
      (when (search-forward-regexp "Last\\ updated:" nil t)
        (xwl-delete-line)
        (insert " ")
        (xwl-insert-date)))
    (setq buffer-undo-list old-list))
  nil)

(global-set-key (kbd "C-c m d") 'xwl-insert-date)

(add-hook 'before-save-hook 'xwl-update-date)

;; named-let, Thanks to Riastradh@#emacs
(defmacro his-named-let (name parameters &rest body)
  `(labels
       ((,name ,(mapcar 'car parameters) ,@body))
     (,name ,@(mapcar 'cadr parameters))))

(defun xwl-term (&optional create)
  (interactive)
  (cond
   (create
    (term "/bin/bash")
    (rename-buffer "*terminal*" t))
   ((not (get-buffer "xwl-term"))
    (term "/bin/bash")
    (rename-buffer "xwl-term" t))
   (t (switch-to-buffer "xwl-term"))))

(defun xwl-tty-p ()
  (string= (frame-parameter nil 'font) "tty"))

(defun xwl-redirect-host ()
  (if xwl-w32-redirect-locally?
      "localhost"
    "172.28.206.207"))

;; Well, as Gnus and other components depend on this, we have to eval this much earlier.
(when (and xwl-at-company? xwl-w32?)
  (setq xwl-proxy-server "172.16.42.137"
        xwl-proxy-port 8080)

  (setq url-proxy-services
        `(("http" . ,(format "%s:%d" xwl-proxy-server xwl-proxy-port))))

  (setq xwl-w3m-arguments
        (list "-o" (format "http_proxy=http://%s:%d"
                           xwl-proxy-server
                           xwl-proxy-port))))


(setq xwl-timers-hook-started? nil)

(defun xwl-timers-hook ()
  "Timers to invoke on the fly.
Run it at an appropriate time, like when we twittering?"
  )

(defun xwl-delete-frame ()
  "Delete frames created by compilation-mode or log-edit-mode. "
  (when (cdr (frame-list))
    (delete-frame
     (car (sort (frame-list)
                (lambda (f1 f2) (< (frame-width f1) (frame-width f2))))))))

(defun xwl-fullscreen ()
  (interactive)
  (case window-system
    ((x)
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
    ((w32)
     (w32-send-sys-command #xf030))
    ((ns)
     (if (string= system-name "tokyolove.local")
         (progn
           (set-frame-width nil 170)
           (set-frame-height nil 48))
       (ns-toggle-fullscreen)))
    ((mac)
     (if (string= system-name "tokyolove.local")
         (progn
           (set-frame-width nil 170)
           (set-frame-height nil 48))
       (set-frame-parameter (selected-frame) 'fullscreen 'maximized)))))

(defun xwl-pure-fullscreen (&optional exit-fullscreen)
  (interactive "P")
  (case window-system
    ((x)
     (set-frame-parameter nil 'fullscreen (and (not exit-fullscreen) 'fullboth)))
     ))


(provide 'xwl-util)

;;; xwl-util.el ends here
