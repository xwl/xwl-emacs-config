;;; xwl-shell.el --- comint modes

;; Copyright (C) 2007, 2008, 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Last updated: 2010/04/15

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

;; comint, *shell*

(eval-after-load 'ansi-color
  '(progn
     (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)))

(add-hook 'comint-mode-hook 'turn-off-auto-fill)
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; xterm's isearch alike
(setq xwl-shell-shell-search-keyword "")
(make-local-variable 'xwl-shell-shell-search-keyword)

(defun xwl-shell-shell-search-backward (&optional keyword)
  (interactive
   (list (read-from-minibuffer
          (if (and xwl-shell-shell-search-keyword
                   (not (string= xwl-shell-shell-search-keyword "")))
              (format "Search backward shell command (regexp = %s): "
                      xwl-shell-shell-search-keyword)
            "Search backward shell command (regexp): "))))
  (unless (string= keyword "")
    (setq xwl-shell-shell-search-keyword keyword))
  (comint-previous-matching-input xwl-shell-shell-search-keyword 1))

(defun xwl-shell-shell-search-forward (&optional keyword)
  (interactive
   (list (read-from-minibuffer
          (if (and xwl-shell-shell-search-keyword
                   (not (string= xwl-shell-shell-search-keyword "")))
              (format "Search forward shell command (regexp = %s): "
                      xwl-shell-shell-search-keyword)
            "Search forward shell command (regexp): "))))
  (unless (string= keyword "")
    (setq xwl-shell-shell-search-keyword keyword))
  (comint-next-matching-input xwl-shell-shell-search-keyword 1))

(eval-after-load 'comint
  '(progn
     (define-key comint-mode-map (kbd "M-p") (lambda ()
                                               (interactive)
                                               (comint-previous-input 1)))
     (define-key comint-mode-map (kbd "M-n") (lambda ()
                                               (interactive)
                                               (comint-next-input 1)))
     (define-key comint-mode-map (kbd "M-r") 'xwl-shell-shell-search-backward)
     (define-key comint-mode-map (kbd "M-s") 'xwl-shell-shell-search-forward)

     (define-key comint-mode-map (kbd "C-l") (lambda ()
                                               (interactive)
                                               (if current-prefix-arg
                                                   (call-interactively 'recenter)
                                                 (let ((inhibit-read-only t))
                                                   (erase-buffer)
                                                   (comint-send-input)
                                                   (when xwl-w32?
                                                     (insert " "))
                                                   ))))

     (define-key comint-mode-map (kbd "RET")
       (lambda ()
         (interactive)
         (if (and xwl-w32? (eq major-mode 'shell-mode))
             (let ((matched (some (lambda (i)
                                    (if (looking-back
                                         (concat "> *" (car i)))
                                        (cadr i)))
                                  '(("ls" "-x --color=always")
                                    ("cd" "%home%")))))
               (when matched
                 (insert " " matched))
               (call-interactively 'comint-send-input)
               (insert " "))
           (call-interactively 'comint-send-input))))

     ;; Respect global settings.
     (define-key comint-mode-map (kbd "C-c .") 'previous-buffer)
     ))


;; ,----
;; | shell
;; `----

(eval-after-load 'shell
  '(progn
     (when (eq system-type 'windows-nt)
       (defadvice shell (after insert-one-space activate)
         (insert " ")))

     (define-key shell-mode-map (kbd "C-c m R") ;; 'rename-uniquely)
       'rename-buffer)

     (define-key shell-mode-map (kbd "C-d") 'delete-char)
     (define-key shell-mode-map (kbd "C-c C-c") 'xwl-disable-key)
     ))


;; ,----
;; | eshell
;; `----

(eval-after-load 'eshell
  '(progn
     (defun xwl-eshell-mode-hook ()
       ;; (local-set-key (kbd "M-m") '(lambda ()
       ;;                               (interactive)
       ;;                               (back-to-indentation)
       ;;                               (search-forward " " nil t 2)))

       (local-set-key (kbd "M-m") 'eshell-bol)
       (local-set-key (kbd "C-a") 'eshell-bol)

       (local-set-key (kbd "C-w") '(lambda ()
                                     (interactive)
                                     (let ((inhibit-read-only t))
                                       (call-interactively 'kill-region))))

       (local-set-key (kbd "C-l") '(lambda ()
                                     (interactive)
                                     (let ((inhibit-read-only t))
                                       (erase-buffer)
                                       (eshell-send-input))))

       (local-set-key (kbd "C-c m R") 'rename-buffer)

       )

     (add-hook 'eshell-mode-hook 'xwl-eshell-mode-hook)

     ))

;; ,----
;; | ansi-term
;; `----

;; (global-set-key (kbd "<f10>") '(lambda ()
;;                                  (interactive)
;;                                  (xwl-switch-or-create "*ansi-term*" 'ansi-term)))

(setq xwl-newsmth-buffer-name "newsmth")

(defun xwl-bbs-heartbeat ()
  "Keep bbs connection alive."
  (mapc (lambda (i)
          (let ((buf (get-buffer i)))
            (when buf
              (term-send-string (get-buffer-process (current-buffer)) ""))))
        (list xwl-newsmth-buffer-name)))

(defun xwl-newsmth ()
  (interactive)
  (call-interactively 'ansi-term)
  (rename-buffer xwl-newsmth-buffer-name)
  ;; set input/output coding system to gbk
  (set-buffer-process-coding-system 'gbk 'gbk)
  (term-send-string (get-buffer-process (current-buffer))
                    "ssh william9@bbs.newsmth.net\n")
  ;; FIXME: Apart from using external "expect" utility, any elisp way to wait
  ;; for this?
  (sleep-for 3)
  (term-send-string (get-buffer-process (current-buffer))
                    (concat pwbbs "\n"))
  (term-send-raw)
  (run-at-time t 120 'xwl-bbs-heartbeat))

;; (global-set-key (kbd "<f11>") '(lambda ()
;;                                  (interactive)
;;                                  (xwl-switch-or-create xwl-newsmth-buffer-name 'xwl-newsmth)))


(eval-after-load 'term
  '(progn
     (define-key term-raw-map (kbd "M-v") 'term-paste)
     (define-key term-raw-map (kbd "C-c f") 'ffap)
     (define-key term-raw-map (kbd "RET")
       '(lambda ()
          (interactive)
          (save-excursion
            (beginning-of-line)
            (if (re-search-forward "http:\\/\\/" (line-end-position) t 1)
                (ffap (ffap-url-at-point))
              (term-send-raw)))))
     ))


(provide 'xwl-shell)

;;; xwl-shell.el ends here
