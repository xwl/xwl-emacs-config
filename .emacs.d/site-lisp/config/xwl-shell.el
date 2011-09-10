;;; xwl-shell.el --- comint modes

;; Copyright (C) 2007, 2008, 2009, 2010, 2011 William Xu

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

;; comint, *shell*

(eval-after-load 'ansi-color
  '(progn
     (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)))

(add-hook 'comint-mode-hook 'turn-off-auto-fill)
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(eval-after-load 'comint
  '(progn
     (define-key comint-mode-map (kbd "M-p") (lambda ()
                                               (interactive)
                                               (comint-previous-input 1)))
     (define-key comint-mode-map (kbd "M-n") (lambda ()
                                               (interactive)
                                               (comint-next-input 1)))

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
                                  '(;;("ls" "-x --color=always")
                                    ;; ("cd" "%home%")
                                    ("cd" "$home")))))
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
     (define-key shell-mode-map (kbd "C-c C-c") nil)

(defvar xwl-w32-disk-directory-cache nil
  "Store last visited directory for each disk.
e.g.,
  '((\"j:\" . \"j:/sf/mw\"))")

(make-variable-buffer-local 'xwl-w32-disk-directory-cache)

(defun shell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with \\[shell-dirtrack-mode].
If Emacs gets confused, you can resync with the shell with \\[dirs].
\(The `dirtrack' package provides an alternative implementation of this
feature - see the function `dirtrack-mode'.)

See variables `shell-cd-regexp', `shell-chdrive-regexp', `shell-pushd-regexp',
and  `shell-popd-regexp', while `shell-pushd-tohome', `shell-pushd-dextract',
and `shell-pushd-dunique' control the behavior of the relevant command.

Environment variables are expanded, see function `substitute-in-file-name'."
  (if shell-dirtrackp
      ;; We fail gracefully if we think the command will fail in the shell.
      (condition-case chdir-failure
	  (let ((start (progn (string-match
			       (concat "^" shell-command-separator-regexp)
			       str) ; skip whitespace
			      (match-end 0)))
		end cmd arg1)
	    (while (string-match shell-command-regexp str start)
	      (setq end (match-end 0)
		    cmd (comint-arguments (substring str start end) 0 0)
		    arg1 (comint-arguments (substring str start end) 1 1))
	      (if arg1
		  (setq arg1 (shell-unquote-argument arg1)))
              ;; Set default-directory correctly such that the TAB
              ;; completion could still work when changing disk on w32
              ;; by, e.g., typing "k:" on the command line. (xwl)
              (when (and (eq system-type 'windows-nt)
                         (string= ":" (substring cmd (1- (length cmd)))))
                (let* ((d (expand-file-name default-directory))
                       (drive (substring d 0 2)))
                  (setq xwl-w32-disk-directory-cache
                        (cons (cons drive d)
                              (remove-if (lambda (el) (string= (car el) drive))
                                         xwl-w32-disk-directory-cache))))
                (setq arg1 (or (some (lambda (el)
                                       (when (string= (car el) cmd) (cdr el)))
                                     xwl-w32-disk-directory-cache)
                               (concat cmd "/"))
                      cmd "cd"))
	      (cond ((string-match (concat "\\`\\(" shell-popd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-popd (comint-substitute-in-file-name arg1)))
		    ((string-match (concat "\\`\\(" shell-pushd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-pushd (comint-substitute-in-file-name arg1)))
		    ((string-match (concat "\\`\\(" shell-cd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-cd (comint-substitute-in-file-name arg1)))
		    ((and shell-chdrive-regexp
			  (string-match (concat "\\`\\(" shell-chdrive-regexp
						"\\)\\($\\|[ \t]\\)")
					cmd))
		     (shell-process-cd (comint-substitute-in-file-name cmd))))
	      (setq start (progn (string-match shell-command-separator-regexp
					       str end)
				 ;; skip again
				 (match-end 0)))))
	(error "Couldn't cd"))))

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
