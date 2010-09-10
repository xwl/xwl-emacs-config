;;; xwl-bindings.el --- Key bindings

;; Copyright (C) 2008, 2009, 2010 William Xu

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

;;; `C-c i *': Invoke external programs, e.g., scheme, mysql.

(setq 未临-外部命令-映射 (make-sparse-keymap))
(defalias '未临-外部命令-前缀 未临-外部命令-映射)
(define-key mode-specific-map "i" '未临-外部命令-前缀)

(define-key 未临-外部命令-映射 (kbd "m") 'sql-mysql)
(define-key 未临-外部命令-映射 (kbd "s") 'run-scheme)
(define-key 未临-外部命令-映射 (kbd "p") 'run-python)

(defun 未临-新年好 ()
  (interactive)
  (message "森宁好啊！"))

;;; Fn 功能键

(global-set-key (kbd "<f2>") 'woman)
(global-set-key (kbd "<f5>") 'ibuffer) ;'dashboard) ;'w3m)

(global-set-key (kbd "<f7>") '(lambda ()
                                (interactive)
                                (require 'xwl-bbdb)
                                (call-interactively 'xwl-bbdb)))

;;'eshell) ;xwl-term ;xwl-run-scsh
(global-set-key (kbd "<f9>") (lambda ()
                               (interactive)
                               (if current-prefix-arg
                                   (call-interactively 'shell)
                                 (xwl-switch-or-create "*shell*" 'shell))))

(global-set-key (kbd "<f11>")
                (lambda ()
                  (interactive)
                  (xwl-switch-or-create
                   ":home"
                   (lambda ()
                     (interactive)
                     (require 'xwl-twittering)
                     (twit)

                     (mapc 'twittering-visit-timeline
                           `(":replies"
                             ":direct_messages"
                             "xwl/followers"
                             ":retweets_of_me"
                             ;; ":public"

                             "xwl/tianxiashi"
                             "xwl/hl"

                             ":search/emacs/"
                             ":search/twittering-mode/"
                             ))

                     (switch-to-buffer ":home")
                     ))

                  (unless xwl-timers-hook-started?
                    (run-hooks 'xwl-timers-hook)
                    (setq xwl-timers-hook-started? t)

                    (command-execute (kbd "<f6>"))
                    ;; (command-execute (kbd "C-c n E"))
                    )))

(global-set-key (kbd "<f13>") 'kill-this-buffer)

;; operators
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

(if (xwl-tty-p)
    (global-set-key (kbd "C-h") 'delete-backward-char)

  (keyboard-translate ?\C-h ?\C-?)
  (keyboard-translate ?\C-? ?\C-d))

(global-set-key (kbd "ESC C-s") 'isearch-forward)
(global-set-key (kbd "ESC C-r") 'isearch-backward)
(global-set-key (kbd "ESC \\") 'just-one-space)
(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key (kbd "M-%") 'replace-regexp)
(global-set-key (kbd "M-_") 'highlight-changes-previous-change)
(global-set-key (kbd "M-+") 'highlight-changes-next-change)
(global-set-key (kbd "C-M-k") 'kill-paragraph)
(global-set-key (kbd "M-K") 'kill-sexp)
;; (global-set-key (kbd "M-H") 'mark-sexp)

(defun xwl-mark-ascii-symbol ()
  "Mark ascii-symbol by `thing-at-point'."
  (interactive)
  (require 'thingatpt)
  (set-mark (beginning-of-thing 'ascii-symbol))
  (goto-char (end-of-thing 'ascii-symbol)))

(global-set-key (kbd "M-H") 'xwl-mark-ascii-symbol)

(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "M-C") 'capitalize-region)
(global-set-key (kbd "M-U") 'upcase-region)
(global-set-key (kbd "M-L") 'downcase-region)

;; C-x
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

;; C-c
(global-set-key (kbd "C-c f") 'ffap)
(global-set-key (kbd "C-c 4 f") 'ffap-other-window)

(global-set-key (kbd "C-c o") '(lambda () (interactive)
				 (call-interactively 'occur)
				 (other-window 1)))
(global-set-key (kbd "C-c j") 'imenu)

(global-set-key (kbd "C-c m a") 'his-align-cols)
;; `a' will be rebinded in c++ mode, so bind `A' also.
(global-set-key (kbd "C-c m A") 'his-align-cols)

;; (global-set-key (kbd "C-c m m") 'apply-macro-to-region-lines)
(global-set-key (kbd "C-c m q") 'query-replace-regexp)
(global-set-key (kbd "C-c m h") 'htmlize-file)
(global-set-key (kbd "C-c m v") 'visit-tags-table)
(global-set-key (kbd "C-c m S") '(lambda () (interactive)
				   (unless (get-buffer ".wubi_scratch")
				     (find-file "~/.wubi_scratch"))
				   (switch-to-buffer ".wubi_scratch")))
(global-set-key (kbd "C-c m e") '(lambda () (interactive)
				   (call-interactively 'eval-region)
				   (message "eval-region...done")))
(global-set-key (kbd "C-c C-\\") 'c-backslash-region)

(global-set-key (kbd "C-c a") 'apropos)

;; (global-set-key (kbd "C-c m c") (lambda ()
;;                                 (interactive)
;;                                 (let ((inhibit-read-only t))
;;                                   (delete-region (point-min) (point-max)))))

;; deal with japanese keyboard
(keyboard-translate ?\¥ ?\\)
(global-set-key (kbd "M-¥") (kbd "M-\\"))
(global-set-key (kbd "C-¥") 'hippie-expand)
(global-set-key (kbd "C-x C-¥") 'goto-last-change)
(global-set-key (kbd "C-M-¥") 'indent-region)

(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key (kbd "C-2") 'set-mark-command)

(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c m s") '(lambda () (interactive) (find-file "~/.scratch")))
(global-set-key (kbd "C-c m r") 'revert-buffer)

;;; These Will Load Some Modules Dynamically.

(global-set-key (kbd "M-s") '(lambda ()
                               (interactive)
                               (require 'xwl-dictionary)
                               (call-interactively 'dictionary-search)))

(global-set-key (kbd "M-S") '(lambda ()
                               (interactive)
                               (require 'xwl-dictionary)
                               (call-interactively 'xwl-search-jp)))

(global-set-key (kbd "C-c n E") ' xwl-erc-select)

(defun xwl-erc-select ()
  (interactive)
  (if xwl-at-company?
      (let ((sv "localhost")
            (nick "xwl__"))
        (when (eq system-type 'windows-nt)
          (setq sv (xwl-redirect-host))
          (setq nick "xwl_"))

        (erc :server sv :port 16667 :nick nick :password pwerc)
        (erc :server sv :port 16669 :nick nick :password pwdeb)
        ;; (erc :server sv :port 16668 :nick nick :password pwerc)
        )

    (erc :server "irc.debian.org"       :port 6669 :nick "xwl" :password pwdeb)
    (erc :server "irc.freenode.net"     :port 6667 :nick "xwl" :password pwerc)

    ;; (erc :server "irc.linuxfire.com.cn" :port 6667 :nick "xwl" :password "")
    ;; (erc :server "irc.mozilla.org"      :port 6667 :nick "xwl" :password "")
    ))

(global-set-key (kbd "<f3>") '(lambda ()
                                (interactive)
                                (if (and (boundp 'emms-playlist-buffer)
                                         (buffer-live-p emms-playlist-buffer))
                                    (emms-playlist-mode-go)
                                  ;; (emms-playlist-mode-go-popup)
                                  (when (y-or-n-p "EMMS not started, start it now? ")
                                    (require 'xwl-emms)
                                    (emms-add-directory-tree
                                     emms-source-file-default-directory)))))

(global-set-key (kbd "C-c e a") 'xwl-emms-add-directory-tree)

(defun xwl-emms-add-directory-tree ()
  (interactive)
  (unless (fboundp 'emms-add-directory-tree)
    (require 'xwl-emms))
  (call-interactively
   'emms-add-directory-tree))

(global-set-key (kbd "<f6>") '(lambda ()
                                (interactive)
                                (if xwl-at-company?
                                    (message "Hmm, only run at home")
                                  (xwl-gnus))))

(setq xwl-gnus-agent-timer nil)

(defun xwl-gnus ()
  (interactive)
  (let ((buf (get-buffer "*Group*")))
    (if buf
        (progn
          (switch-to-buffer buf)
          (setq xwl-mail-notify-string ""))

      ;; (if (not (eq window-system 'ns))
      ;;     (call-interactively 'gnus)
      (call-interactively 'gnus-unplugged)
      ;; )

      (gnus-demon-init)
      (when (fboundp 'color-theme-xwl-console)
        (color-theme-xwl-console))

      ;; TODO: 23.1 mac port doesn't work with this.
      ;; (unless gnus-plugged
      ;;   (unless xwl-gnus-agent-timer
      ;;     (setq xwl-gnus-agent-timer
      ;;           (run-with-timer
      ;;            0 (* 3600 24) (lambda ()
      ;;                           (xwl-shell-command-asynchronously
      ;;                            (concat
      ;;                             ;; Company PC is always on, so we won't have
      ;;                             ;; too many instances running at the same
      ;;                             ;; time...
      ;;                             (if xwl-at-company?
      ;;                                 ""
      ;;                               "(ps -ef|grep \"emacs --eval\" | grep -v grep) || ")
      ;;                             "emacs --eval \"(progn (require 'xwl-gnus-agent))\""
      ;;                             ))))))
      ;;   (message "Gnus agent timer started"))
      )))

(global-set-key (kbd "<f8>") (lambda ()
                               (interactive)
                               ;;   ;; (find-file "~/notes/todo.org")
                               ;;   (case system-type
                               ;;     ((darwin)
                               ;;      (delete-other-windows)
                               ;;      (find-file "~/notes/todo")
                               ;;      (split-window-horizontally)
                               ;;      (find-file "~/notes/todo-finland"))
                               ;;     (t
                               ;;      (delete-other-windows)
                               ;;      (find-file "~/notes/todo")
                               ;;      (split-window-horizontally)
                               ;;      (find-file "~/notes/todo-nokia")))
                               (xwl-switch-or-create
                                "*Org Agenda*"
                                '(lambda ()
                                   (org-agenda 1 "h")
                                   (delete-other-windows)
                                   ;; 'org-agenda
                                   ))))

(global-set-key (kbd "C-c t") '(lambda ()
                                 (interactive)
                                 (require 'xwl-org)
                                 (call-interactively 'xwl-org-todo)))

(defun xwl-org-todo (todo)
  "Read TODO from minibuffer and append it to car of `org-agenda-files'. "
  (interactive "sTODO: ")
  (let ((f (car org-agenda-files)))
    (with-current-buffer (find-file-noselect f)
      (goto-char (point-max))
      (unless (bolp)
        (newline))
      (insert "** TODO " todo)
      (newline)
      (save-buffer))))

(global-set-key (kbd "C-/") '(lambda ()
                               (interactive)
                               (require 'xwl-wubi)
                               (toggle-input-method)))

(global-set-key (kbd "C-x C-j") '(lambda ()
                                   (interactive)
                                   (require 'xwl-dired)
                                   (call-interactively 'dired-jump)))

(global-set-key (kbd "C-c n r") '(lambda ()
                                   (interactive)
                                   (revert-buffer-with-coding-system 'gb18030)))

(global-set-key (kbd "<f4>")
                (lambda ()
                  (interactive)
                  (let* ((n "*top*")
                         (b (get-buffer n)))
                    (if b
                        (switch-to-buffer b)
                      (ansi-term "top")
                      (rename-buffer n)
                      (local-set-key "q" '(lambda ()
                                            (interactive)
                                            (kill-buffer (current-buffer))))
                      (hl-line-mode 1)))))


;; set-fill-column
(global-unset-key (kbd "C-x f"))

(global-set-key (kbd "C-c m R") 'xwl-revert-buffer-with-sudo)
(global-set-key (kbd "C-c m o") 'his-one-whitespace-between-ce)
(global-set-key (kbd "C-c m t") 'his-transpose-windows)


(global-set-key (kbd "C-s") '(lambda ()
                                    (interactive)
                                    (if current-prefix-arg
                                        (call-interactively 'his-isearch-current-symbol)
                                      (call-interactively 'isearch-forward))))
(global-set-key (kbd "C-r") '(lambda ()
                               (interactive)
                               (if current-prefix-arg
                                   (call-interactively 'his-isearch-current-symbol)
                                 (call-interactively 'isearch-backward))))

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-o") '(lambda ()
                               (interactive)
                               (other-window -1)))

(global-set-key (kbd "C-S-o") 'other-frame)
(global-set-key (kbd "M-O") '(lambda ()
                               (interactive)
                               (other-frame -1)))

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-o") nil)
     (define-key dired-mode-map (kbd "M-o") nil)
     ))

(global-set-key (kbd "C-x o") 'xwl-forward-char)
(global-set-key (kbd "C-x O") 'xwl-backward-char)

(global-set-key (kbd "C-c m u") 'twittering-update-status-interactive)

(provide 'xwl-bindings)

;;; xwl-bindings.el ends here
