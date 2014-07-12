;;; xwl-programming.el --- programming config

;; Copyright (C) 2007, 2009, 2010, 2011, 2012, 2013, 2014 William Xu

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

;;; C Mode Common

;; (require 'cc-mode)
;; M-x semanticdb-save-all-db
;; M-x semanticdb-create-system-database
;; M-x bovinate
;; M-x hide-ifdef-mode

;; C-c C-e (expand macros)
;; C-c C-\ (indent beautifully)

(defun xwl-c-mode-common-hook ()
  (setq c-cleanup-list '( ;;brace-else-brace
                         scope-operator
                         empty-defun-braces
                         defun-close-semi

                         space-before-funcall
                         compact-empty-funcall

                         one-liner-defun))
  ;; (setq tab-width 4)

  ;; Don't enable for lex/yacc files.
  (when (and (buffer-file-name) ; mmm-mode submodes don't have a  valid buffer name.
             (not (string-match "\\.l$\\|\\.y$" (buffer-file-name))))
    (c-toggle-auto-hungry-state 1))

  (add-to-list 'c-hanging-braces-alist '(brace-list-close))

  ;; (glasses-mode 1)
  ;; (hs-minor-mode 1)
  ;; (hs-hide-all)
  ;; (xwl-hs-minor-mode-hook)

  ;; (when (xwl-hide-ifdef-setup)
  ;;   (hide-ifdef-mode 1))

  (auto-fill-mode -1)
  (electric-spacing-mode 1)
  (abbrev-mode 1)
  (cwarn-mode 1)
  (which-func-mode 1)
  (when (fboundp 'doxymacs-mode)
    (doxymacs-mode 1)
    (doxymacs-font-lock))
  (subword-mode 1)
  ;; (flyspell-prog-mode)
  (gtags-mode 1)

  ;; == keys ==
  (local-unset-key (kbd "."))
  (local-unset-key (kbd "%"))

  (local-unset-key (kbd "C-c ."))

  (local-set-key (kbd "C-c m a") 'align)
  (local-set-key (kbd "ESC TAB") 'semantic-ia-complete-symbol)
  (local-set-key (kbd "<C-home>") 'gdb)
  ;; 'semantic-chart-nonterminal-complexity-token)

  ;; restore electric keys
  (local-set-key "#" 'c-electric-pound)
  (local-set-key "{" 'c-electric-brace)
  (local-set-key "/" 'c-electric-slash)
  (local-set-key "*" 'c-electric-star)
  (local-set-key ";" 'c-electric-semi&comma)
  (local-set-key "," 'c-electric-semi&comma)
  (local-set-key ":" 'c-electric-colon)

  (if (string= c-indentation-style "whitesmith")
      (progn
        (local-set-key "(" (lambda ()
                             (interactive)
                             (if (save-excursion
                                   (backward-word)
                                   (memq font-lock-keyword-face (text-properties-at (point))))
                                 (electric-spacing-insert "(")
                               (electric-spacing-insert "(" t))))
        (local-set-key ")" (lambda ()
                             (interactive)
                             (just-one-space)
                             (insert ")")
                             (indent-according-to-mode)))
        ;; FIXME: this shouldn't be necessary.
        (local-set-key "}"  (lambda ()
                              (interactive)
                              (call-interactively 'c-electric-brace)

                              (save-excursion
                                (forward-line -1)
                                (indent-according-to-mode)))))

    (local-set-key "(" 'c-electric-paren)
    ;; '(lambda ()
    ;;                       (interactive)
    ;;                       (call-interactively 'c-electric-paren)
    ;;                       ;; (insert ")")
    ;;                       (backward-char)))

    (local-set-key ")" 'c-electric-paren)
    (local-set-key "}" 'c-electric-brace)))

(add-hook 'c-mode-common-hook 'xwl-c-mode-common-hook)

;; (require 'hideif)
;; (setq hide-ifdef-shadow t)
;; (setq hide-ifdef-initially t)

;; (add-to-list 'auto-mode-alist '("\\.c$" . c++-mode))

;;; Java
;; ------

;; (require 'jde)

;;; C, C++

(defun xwl-set-c-c++-style ()
  (if (or (string-match "s40_sw" (or (buffer-file-name) "")))
      ;; symbian c++
      (progn
        (c-set-style "whitesmith")
        (setq c-cleanup-list '()))
    (c-set-style "k&r")
    ;; TODO, check this.
    (setq c-basic-offset 4)))

(add-hook 'c-mode-hook 'xwl-set-c-c++-style)
(add-hook 'c++-mode-hook 'xwl-set-c-c++-style)

(add-hook 'c++-mode-hook (lambda ()
                           (setq comment-start "/* "
                                 comment-end "*/")))

(add-to-list 'auto-mode-alist '("\\.hrh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rss\\'" . c++-mode))

;; ,----
;; | symbian
;; `----

(add-to-list 'auto-mode-alist '("\\.loc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.mmp\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.inf\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.rls\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.iby\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.ti\\'"  . c-mode))

(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))

(add-to-list 'auto-mode-alist '("\\.pkg\\'" . lisp-mode))


(eval-after-load 'ffap
  '(progn
     (setq ffap-c-path `("../inc"

                         "../coctlinc"
                         "/epoc32/include"
                         "/epoc32/include/mw"
                         "/epoc32/include/platform/mw"

                         ,@ffap-c-path))
     ))


;;; Obj-C

(add-to-list 'auto-mode-alist
             '("~/repo/git/MPlayerX/" . objc-mode))

;;; Tags, code browsing

;; imenu, cscope, etags
;; speedbar, semantic, eieio, ecb, xref

;; etags
(defun xwl-etags-on-c (dir)
  "Run `etags' on c files(*.h and *.c) under DIR recursively.
Thus generate a TAGs file."
  (interactive "DEtags on: ")
  (shell-command
   (format "find '%s' -type f -regex '.*\\.h$\\|.*\\.c$' | xargs etags "
           dir)))

(defun xwl-etags-on-c++ (dir)
  "Run `etags' on c++ files(*.h, *.c and *.cpp) under DIR recursively.
Thus generate a TAGs file."
  (interactive "DEtags on(TODO,buggy!): ")
  (shell-command
   (format "find \"%s\" -type f -regex \".*\\.h$\\|.*\\.c$\\|.*\\.cpp$\" | xargs etags "
           dir)))

(global-set-key "\M-." 'etags-select-find-tag)


;;; python

(defun xwl-python-mode-hook ()
  (electric-spacing-mode 1)
  ;; (local-unset-key (kbd "."))
  )

(add-hook 'python-mode-hook 'xwl-python-mode-hook)



;; - flymake(syntax checker for codes)

;; ,----
;; | shell-script
;; `----

(defun xwl-sh-run ()
  (interactive)
  (shell-command
   (concat "sh " (file-name-nondirectory (buffer-file-name)))))

(defun xwl-sh-mode-hook()
  ;; (set (make-local-variable 'outline-regexp) "###+ ")
  ;; (outline-minor-mode 1)

  (set (make-local-variable 'beginning-of-defun-function)
       'sh-beginning-of-defun)

  (set (make-local-variable 'end-of-defun-function)
       'sh-end-of-defun)
  )

(add-hook 'sh-mode-hook 'xwl-sh-mode-hook)

(eval-after-load 'sh-script
  '(progn

     (defun sh-beginning-of-defun ()
       (interactive)
       (re-search-backward defun-prompt-regexp nil t 1))

     (defun sh-end-of-defun ()
       (interactive)
       (when (re-search-forward "^}" nil t 1)
         (forward-line)))

     (define-key sh-mode-map (kbd "C-M-a") 'sh-beginning-of-defun)
     (define-key sh-mode-map (kbd "C-M-e") 'sh-end-of-defun)
     (define-key sh-mode-map (kbd "<C-down>") 'xwl-sh-run)

     ))


;;TODO, add same to conf mode

;;; sql
;; -----

(eval-after-load 'sql
  '(progn
     ;; 运行 sql-mysql 时，这个值没设的话，死活无法正确在 sql 语句中含有中文！
     ;;（单单输出是没问题的）
     (setenv "LC_ALL" "zh_CN.utf-8")

     (define-key sql-interactive-mode-map (kbd "RET")
       (lambda ()
         (interactive)
         (save-excursion
           (move-end-of-line 1)
           (unless (looking-back "\\ *;\\ *" (save-excursion
                                               (beginning-of-line)
                                               (point)))
             (insert ";")))
         (comint-send-input)))

     (add-hook 'sql-mode-hook 'electric-spacing-mode)
     (add-hook 'sql-interactive-mode-hook 'electric-spacing-mode)
     ))

(eval-after-load "xwl-private"
  '(progn
     (setq sql-mysql-program "mysql"
           sql-user          "william"
           sql-password      ""         ; pwsql
           sql-database      "foo"
           sql-server        "")))

;;; debug
;; -------

(setq gdb-many-windows t)
(global-set-key (kbd "<C-end>") 'gdb-restore-windows)

;;; changlog entry
;; ----------------

(setq add-log-full-name nil
      add-log-mailing-address nil)

;; FIXME: why this won't work?
;; (add-hook 'log-edit-done-hook 'delete-frame)

(add-hook 'log-edit-mode-hook (lambda () (flyspell-mode 1)))

(eval-after-load 'log-edit
  '(progn
     (defadvice log-edit-done (after delete-frame activate)
       ;; Delete smallest one.  Ideally, should delete the one running this log
       ;; command. But how? FIXME
       (unless (xwl-delete-frame)
         (mapc (lambda (w)
                 (when (string= (buffer-name (window-buffer w))
                                "*Shell Command Output*")
                   (delete-window w)))
               (window-list))))
     ))

;;; doxymacs
;; TODO
;; (require 'doxymacs)
(require 'lisp-mnt)

;;; VC: version control
;; --------------------

(add-to-list 'vc-handled-backends 'DARCS)
(setq vc-darcs-mail-address "William Xu <william.xwl@gmail.com>")

(global-set-key (kbd "C-x v p") (lambda () (interactive) (compile "git push")))
(global-set-key (kbd "C-x v a")
                (lambda ()
                  (interactive)
                  (let* ((f (file-name-nondirectory (buffer-file-name)))
                         (cmd (concat "git add " f)))
                    (message (concat cmd "..."))
                    (if (zerop (shell-command cmd))
                        (message (concat cmd "...done"))
                      (message (concat cmd "...failed"))))))

(eval-after-load 'vc-dir
  '(progn
     (defun vc-dir-hide-up-to-date ()
       "Hide up-to-date items from display."
       (interactive)
       (let ((crt (ewoc-nth vc-ewoc -1))
             (first (ewoc-nth vc-ewoc 0)))
         ;; Go over from the last item to the first and remove the
         ;; up-to-date files and directories with no child files.
         (while (not (eq crt first))
           (let* ((data (ewoc-data crt))
                  (dir (vc-dir-fileinfo->directory data))
                  (next (ewoc-next vc-ewoc crt))
                  (prev (ewoc-prev vc-ewoc crt))
                  ;; ewoc-delete does not work without this...
                  (inhibit-read-only t))
             (when (or
                    ;; Remove directories with no child files.
                    (and dir
                         (or
                          ;; Nothing follows this directory.
                          (not next)
                          ;; Next item is a directory.
                          (vc-dir-fileinfo->directory (ewoc-data next))))
                    ;; Remove files in the up-to-date state.
                    (or (eq (vc-dir-fileinfo->state data) 'up-to-date)
                        ;; xwl: hide unregistered as well.
                        (eq (vc-dir-fileinfo->state data) 'unregistered))
                    )
               (ewoc-delete vc-ewoc crt))
             (setq crt prev)))))

     ;; always diff current file only in vc-dir.
     (define-key vc-dir-mode-map (kbd "=") 'xwl-vc-dir-diff)

     (defadvice vc-dir-marked-files (around xwl-check-current-file-only)
       nil)

     (defun xwl-vc-dir-diff ()
       "Like `vc-diff', but always diff current file only."
       (interactive)
       (ad-activate 'vc-dir-marked-files)
       (unwind-protect
           (call-interactively 'vc-diff)
         (ad-deactivate 'vc-dir-marked-files)))

     ))

(eval-after-load 'vc
  '(progn
     ;; (defadvice vc-checkin (around use-new-frame activate)
     ;;   (let ((pop-up-frames t))
     ;;     ad-do-it))

     (defadvice vc-checkin (around setup-source-log-diff-view activate)
       (let ((b (current-buffer))
             (f buffer-file-name))
         ad-do-it
         (when (eq (vc-backend f) 'Git)
           (delete-other-windows)
           (shell-command (concat "git diff -w " (shell-quote-argument f)))
           (let ((b (get-buffer "*Shell Command Output*")))
             (with-current-buffer b
               (diff-mode)))
           (split-window-vertically)
           (switch-to-buffer b)
           (other-window 1))))
     ))

(setq magit-omit-untracked-dir-contents t)
(global-set-key (kbd "C-c m m") 'magit-status)

(defun xwl-magit-commit-amend-reuse ()
  "commit --amend --reuse-message=HEAD"
  (interactive)
  (magit-git-command "commit --amend --reuse-message=HEAD" default-directory))

(eval-after-load 'magit
  '(progn
     (defun magit-highlight-section ())
     (define-key magit-status-mode-map (kbd "y") 'magit-refresh)
    ))

(eval-after-load 'vc-git
  '(progn
     (defun vc-git-print-log (files buffer &optional shortlog start-revision limit)
  "Get change log associated with FILES.
Note that using SHORTLOG requires at least Git version 1.5.6,
for the --graph option."
  (let ((coding-system-for-read vc-git-commits-coding-system))
    ;; `vc-do-command' creates the buffer, but we need it before running
    ;; the command.
    (vc-setup-buffer buffer)
    ;; If the buffer exists from a previous invocation it might be
    ;; read-only.
    (let ((inhibit-read-only t))
      (with-current-buffer
          buffer
	(apply 'vc-git-command buffer
	       'async files
	       (append
		'("log" "--no-color"); "--follow") ; xwl
		(when shortlog
		  `("--graph" "--decorate" "--date=short" "--follow"
                    ,(format "--pretty=tformat:%s"
			     (car vc-git-root-log-format))
		    "--abbrev-commit"))
		(when limit (list "-n" (format "%s" limit)))
		(when start-revision (list start-revision))
		'("--")))))))

     (defun vc-git-annotate-command (file buf &optional rev)
       (let ((name (file-relative-name file)))
         (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

   ))

;;; skeletons
;; -----------

(require 'semantic/bovine/gcc)

;;  c
(define-skeleton skeleton-c-mode-main-fun
  "generate main(int argc, char *argv[])"
  > "int main(int argc, char *argv[])\n{\n"
  > _ " "
  > "\n\nreturn 0;"
  > "\n}")

(define-skeleton skeleton-c-mode-main-fun1
  "generate main()"
  > "int main()\n{\n"
  > _ " "
  > "\n\nreturn 0;"
  > "\n}")

(define-skeleton skeleton-c-mode-include
  "Generate include<>."
  > "#include <"
  (completing-read
   "Include File: "
   (apply 'append
          (mapcar (lambda (dir) (directory-files dir))
                  (append (semantic-gcc-get-include-paths "c")
                          (remove-if-not 'file-exists-p '("~/include"))))
          ))
  ">\n")

;; c++
(define-skeleton skeleton-c++-mode-main-fun
  "generate int main(int argc, char *argv[])"
  > "int main(int argc, char *argv[])\n{\n"
  > _ " "
  > "\n}")

(define-skeleton skeleton-c++-mode-main-fun1
  "generate int main()"
  > "int main()\n{\n"
  > _ ""
  > "\n}")

(define-skeleton skeleton-c++-mode-include
  "Generate include<>."
  > "#include <"
  (completing-read
   "Include file: "
   (apply 'append
          (mapcar (lambda (dir) (directory-files dir))
                  (semantic-gcc-get-include-paths "c++"))
          ))
  ">\n")

;; java
(define-skeleton skeleton-jde-mode-main-fun
  "Generate: public static void main(String[] args)"
  > "public static void main(String[] args)\n"
  > "{\n"
  > _ " "
  > "\n        }")

(define-skeleton skeleton-jde-mode-print-fun
  "Generate: System.out.println()"
  > "System.out.println("
  > _ ""
  > ");")

;; hs-minor-mode
;; (require 'hideshow)
;; (defun xwl-hs-minor-mode-hook ()
;;   (define-key hs-minor-mode-map (kbd "C-c @ DEL") 'hs-hide-block)
;;  (define-key hs-minor-mode-map (kbd "C-c @ ESC DEL") 'hs-hide-all))


;;; highlight special keywords
(setq xwl-keyword-highlight-modes
      '(php-mode java-mode c-mode c++-mode emacs-lisp-mode scheme-mode
                 text-mode outline-mode org-mode))

(unless (executable-find "grep")
  (when (eq system-type 'windows-nt)
    (if (file-exists-p "c:/usr/git/bin/grep.exe")
        (setq find-program "cmd /c c:/usr/git/bin/find.exe"
              grep-program "cmd /c c:/usr/git/bin/grep.exe")
      (setq find-program "cmd /c c:/usr/bin/find"
            grep-program "cmd /c c:/usr/bin/grep"))))

(eval-after-load 'grep
  '(progn
     (defadvice grep (around append-star activate)
       (interactive
        (progn
          (grep-compute-defaults)
          (let ((default (grep-default-command)))
            (list
             (read-shell-command
              "Run grep (like this): "
              (let ((init
                     (concat (if current-prefix-arg default grep-command)
                             (concat "\"" (thing-at-point 'ascii-symbol) "\" *"))))
                (cons init (- (length init) 2)))
              'grep-history
              (if current-prefix-arg nil default))))))

       ;; Setting process-setup-function makes exit-message-function work
       ;; even when async processes aren't supported.
       (compilation-start (if (and grep-use-null-device null-device)
                              (concat command-args " " null-device)
                            command-args)
                          'grep-mode))
     ))

(global-set-key (kbd "C-c g")
                (lambda ()
                  (interactive)
                  (require 'grep)
                  (let ((cmd
                         (if (string-match "[^*]\\.gz" (shell-command-to-string
                                                        "ls *.gz | head -1"))
                             "zgrep -nH "
                           (concat grep-program " -nH "))))
                    (grep-apply-setting 'grep-command cmd)
                    (call-interactively 'grep))))

(global-set-key (kbd "C-c G")
                (lambda (cmd)
                  (interactive
                   (list (read-shell-command
                          "Grep filename: "
                          (concat find-program " . -type f -follow | grep -nH "))))
                  (shell-command cmd)))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-todo-face)

(modify-face 'font-lock-fixme-face "black" "yellow" nil t nil t nil nil)
(modify-face 'font-lock-todo-face  "black" "yellow" nil t nil nil nil nil)

(defun xwl-highlight-special-keywords ()
  (mapc (lambda (mode)
	  (font-lock-add-keywords
	   mode
	   '(("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t)
	     ("\\<\\(TODO\\)"  1 'font-lock-todo-face  t))))
	xwl-keyword-highlight-modes))

(xwl-highlight-special-keywords)

;;; lex, yacc
;; -----------

(defun xwl-lex-yacc-mode ()
  "Redefine some operators to enhace readability, when editing lex, or
yacc source files."
  (interactive)

  (defun insert-little-line ()
    (interactive)
    (forward-line 0)
    (insert "\t\t| "))

  (defun insert-semicolon ()
    (interactive)
    (insert "           ")		; ten whitespaces
    (forward-line 0)
    (forward-char 8)
    (insert ": "))

  (local-unset-key (kbd "{"))
  (local-unset-key (kbd ";"))
  (local-set-key (kbd "|") 'insert-little-line)
  (local-set-key (kbd ":") 'insert-semicolon))

(defun xwl-paredit-mode-hook ()
  (local-set-key (kbd "C-c C-r") 'paredit-raise-sexp)
  (local-set-key (kbd "C-c C-w") 'paredit-wrap-round)

  (local-set-key (kbd "C-c C-b")   'paredit-forward-barf-sexp)
  (local-set-key (kbd "C-c C-S-b") 'paredit-backward-barf-sexp)
  (local-set-key (kbd "C-c C-s")   'paredit-forward-slurp-sexp)
  (local-set-key (kbd "C-c C-S-s") 'paredit-backward-slurp-sexp))

(add-hook 'paredit-mode-hook 'xwl-paredit-mode-hook)

;;; elisp
;; -------

(defun xwl-lisp-mode-hook ()
  (which-func-mode 1)

  (set (make-local-variable 'outline-regexp) ";;;+ ")
  (outline-minor-mode 1)

  (local-set-key (kbd "<backtab>") 'lisp-complete-symbol)
  (local-set-key (kbd "<S-tab>") 'lisp-complete-symbol)
  (local-set-key (kbd "C-x C-r") 'eval-region)
  (local-set-key (kbd "C-x C-b") 'eval-buffer))

(mapc (lambda (hook)
        (mapc (lambda (func)
                (add-hook hook func))
              '(turn-on-eldoc-mode
                xwl-lisp-mode-hook
                electric-spacing-mode
                enable-paredit-mode)))
      '(lisp-mode-hook emacs-lisp-mode-hook))

;;; scheme
;; --------

(require 'scheme)
(require 'chicken-scheme-extras)

(setq scheme-program-name "csi -no-warnings")
(defun xwl-scheme-mode-hook ()
  (setq comment-add 1)

  (set (make-local-variable 'outline-regexp) ";;;+ ")
  (outline-minor-mode 1)

  (local-set-key (kbd "C-x C-r") 'scheme-send-region)
  (local-set-key (kbd "C-x C-b") 'xwl-scheme-send-buffer))

(add-hook 'scheme-mode-hook 'xwl-scheme-mode-hook)
(add-hook 'scheme-mode-hook 'electric-spacing-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

(defun xwl-scheme-send-buffer ()
  (interactive)
  (scheme-send-region (point-min) (point-max)))

;; change `|' to normal (default, it's set to \")
(modify-syntax-entry ?\| "_   " scheme-mode-syntax-table)

;; (defun xwl-scheme-print-output ()
;;   "Get last session's output."
;;   (interactive)
;;   (with-current-buffer scheme-buffer
;;     (save-excursion
;;       (goto-char (point-max))
;;       (when (<= (current-column) (length "guile> "))
;;         (search-backward "guile>")
;;         (re-search-backward "guile> \\(\\(.*\n\\)+\\)")
;;         (let ((str (match-string-no-properties 1)))
;;           (message (substring str 0 (1- (length str)))))))))


;; (defadvice scheme-send-last-sexp (after scheme-send-last-sexp-advice)
;;   "Print output in minibuf."
;;   (xwl-scheme-print-output))

;; (ad-activate 'scheme-send-last-sexp)

;; (autoload 'scheme-smart-complete "scheme-complete" nil t)
;; (eval-after-load 'scheme
;;   '(progn
;;      (define-key scheme-mode-map (kbd "<S-tab>") 'scheme-smart-complete)
;;      ))

;; (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
;; (add-hook 'scheme-mode-hook
;;   (lambda ()
;;     (make-local-variable 'eldoc-documentation-function)
;;     (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;;     (eldoc-mode)))

;; (setq default-scheme-implementation


;; guile
;; -----

(setq guile-program "guile")

;; guile debugger

;; (defadvice gds-help-symbol (after jump-to-help)
;;   (other-window 1))
;;   )

;; (ad-activate 'gds-help-symbol)

;; (require 'guile-scheme)
;; (setq initial-major-mode 'scheme-interaction-mode)

(defun his-scheme-eval-last-sexp (&optional insert)
  (interactive "P")
  (let ((standard-output (if insert (current-buffer) t))
        (cmd (buffer-substring (save-excursion (backward-sexp) (point)) (point))))
    (with-temp-buffer
      (comint-redirect-send-command-to-process
       cmd (current-buffer) (get-process "scheme") t nil)
      (while (string= (buffer-string) "") (sleep-for 0.01))
     (princ (buffer-string)))))

(defun xwl-run-scsh (&optional scheme?)
  "`run-scsh', rename it to scsh.

If SCHEME?, `run-scheme'."
  (interactive "P")
  (if scheme?
      (run-scheme "mzscheme")
    (progn
      (run-scsh nil)
      (rename-buffer "*scsh*"))))

(add-to-list 'auto-mode-alist '("\\.sx$" . scheme-mode))


;;; haskell

(setq auto-mode-alist
      `(,@auto-mode-alist
        ("\\.[hg]s$"  . haskell-mode)
        ("\\.hi$"     . haskell-mode)
        ("\\.l[hg]s$" . literate-haskell-mode)))

(defun xwl-haskell-mode-hook ()
  (turn-on-haskell-decl-scan)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (turn-on-haskell-simple-indent)

  ;; (glasses-mode 1)
  (electric-spacing-mode)

  (local-set-key (kbd "RET")
                 (lambda ()
                   (interactive)
                   (let ((indent? (save-excursion
                                    (goto-char (line-beginning-position))
                                    (looking-at "[[:space:]]"))))

                     (if indent?
                         (newline)
                       (command-execute (kbd "C-q C-j"))
                       (goto-char (line-beginning-position))))))
  )

(defun xwl-inferior-haskell-mode-hook ()
  ;; (glasses-mode 1)
  (electric-spacing-mode))

(add-hook 'haskell-mode-hook 'xwl-haskell-mode-hook)
(add-hook 'inferior-haskell-mode-hook 'xwl-inferior-haskell-mode-hook)

(global-set-key (kbd "C-c i h") 'run-haskell)


;;; Compilation Mode

;; (eval-after-load 'buffer-action
;;   '(progn
;;      (defadvice buffer-action-compile (before save-layout activate)
;;        (setq xwl-layout-before-compilation (current-window-configuration)))
;;      ))

;; (defadvice compile (around use-new-frame activate)
;;   (let ((pop-up-frames t))
;;     ad-do-it))

(defun xwl-compilation-exit-autoclose (status code msg)
  (cond ((not (and (eq status 'exit) (zerop code)))
         (setq msg "failed"))
        ((with-current-buffer "*compilation*"
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "warning:" nil t 1)))
         (setq msg "has warnings"))
        (t
         ;; (run-at-time 1 nil (lambda ()
         ;;                      ;; (set-window-configuration
         ;;                      ;; xwl-layout-before-compilation)
         ;;                      ;; (delete-windows-on "*compilation*")
         ;;                      (xwl-delete-frame)
         ;;                      ))
         (setq msg "succeed")))
  (cons msg code))

(setq compilation-exit-message-function 'xwl-compilation-exit-autoclose)

; (require 'tramp-util)

;; (defun tramp-compile (command)
;;   "Compile on remote host."
;;   (interactive
;;    (if (or compilation-read-command current-prefix-arg)
;;        (list (read-from-minibuffer "Compile command: "
;;                                    compile-command nil nil
;;                                    '(compile-history . 1)))
;;      (list compile-command)))
;;   (setq compile-command command)
;;   (save-some-buffers (not compilation-ask-about-save) nil)
;;   (let ((d default-directory)
;;         (status 0))
;;     (save-excursion
;;       (pop-to-buffer (get-buffer-create "*Compilation*") t)
;;       (erase-buffer)
;;       (setq default-directory d)))
;;   (setq status
;;         (tramp-handle-shell-command command (get-buffer "*Compilation*")))
;;   (pop-to-buffer (get-buffer "*Compilation*"))
;;   (tramp-minor-mode 1)
;;   (compilation-minor-mode 1)
;;   (when compilation-exit-message-function
;;     (funcall compilation-exit-message-function 'exit status nil)))


;;; buffer-action

;; TODO, remove following two lines.
;; (autoload 'buffer-action-compile "buffer-action")
;; (autoload 'buffer-action-run "buffer-action")

(eval-after-load 'buffer-action
  '(progn
     (setq buffer-action-table
           `(("\\.tex$"
              ,(case system-type
                           ((darwin)
                            "latexmk -pdf %f && ( if grep 'Missing character' %n.log; then bad; fi)")
                           (t
                            "xelatex %f && ( if grep 'Missing character' %n.log; then bad; fi)"))
              "%n.pdf"
              (lambda () (TeX-view))
              ;; ,(cdr (assq system-type
              ;;             '((darwin . "open -a Preview %n.pdf &")
              ;;               (windows-nt . "start %n.pdf" )
              ;;               (gnu/linux . "gnome-open %n.pdf"))))
              )

             ("\\.dot$"
              ,(concat "dot -Tjpg %f -o %n.jpg "
                       ;; (let ((fontname
                       ;;        (cdr (assq system-type
                       ;;                   '((windows-nt . "c:/Windows/Fonts/simhei.ttf")
                       ;;                     (darwin . "/Library/Fonts/Arial Unicode.ttf"))))))
                       ;;   (mapconcat (lambda (i) (format i fontname))
                       ;;              '("-Nfontname='%s'" "-Gfontname='%s'" "-Efontname='%s'")
                       ;;              " "))
                       )
              "%n.jpg"

              ,(cdr (assq system-type '((windows-nt . "start %n.jpg")
                                        (darwin . "open %n.jpg &")))))

             ;; FIXME: return value of abld is unreliable? so let us
             ;; keep the compilation window for now.
             ("\\.mmp$\\|\\.inf$"
              ;; "((dir | grep ABLD.BAT) || bldmake bldfiles) && abld build winscw udeb && xxx"
              "sbs -c winscw_udeb"
              nil
              "epoc" )

             ;; qt
             ((lambda () (not (null (directory-files-and-attributes "." nil "\\.pro$" t))))
              ;; "make"
              "qmake -platform symbian-sbsv2"
              (lambda ()
                (concat "debug/"
                        (replace-regexp-in-string
                         "\\.pro$"
                         ".exe"
                         (caar (directory-files-and-attributes "." nil "\\.pro$" t)))))
              (lambda ()
                (buffer-action-shell-command
                 (concat "debug/"
                         (replace-regexp-in-string
                          "\\.pro$"
                          ".exe &"
                          (caar (directory-files-and-attributes "." nil "\\.pro$" t))))))
              )

             (c++-mode ,(concat buffer-action-c++ " -O2 \"%f\" -lm -I/sw/include -o %n")
                       ,@(if (eq system-type 'windows-nt) '("%n.exe" "%n.exe") '("%n" "./%n")))

             (haskell-mode "ghc --make '%f'" "%n" "./%n")

             ,@buffer-action-table))
     ))

(global-set-key (kbd "C-c c")   'buffer-action-compile)
(global-set-key (kbd "C-c r") '(lambda ()
                                 (interactive)
                                 ;; (call-interactively 'buffer-action-compile)
                                 (buffer-action-run)))
(global-set-key (kbd "C-c p")   (lambda ()
                                  (interactive)
                                  (let* ((n "*compilation*")
                                         (b (get-buffer n)))
                                    (if b
                                        (switch-to-buffer b)
                                      (message "Hey, no %s buffer exists yet" n)))))



;; asm
(require 'asm-mode)
;; (define-key asm-mode-map (kbd ":") '(lambda () (interactive) (smart-insert-operator ":" t)))
;; (define-key asm-mode-map (kbd ",") '(lambda () (interactive) (smart-insert-operator "," t)))
(define-key asm-mode-map (kbd "RET") 'newline)

;; ;; php
;; (defun xwl-php-mode-hook ()
;;   (local-unset-key (kbd "/"))
;;   (local-unset-key (kbd "="))
;;   (local-unset-key (kbd ">"))
;;   (local-unset-key (kbd "<")))

;; (add-hook 'php-mode-user-hook 'xwl-php-mode-hook)

;; (add-hook 'html-mode-hook 'xwl-php-mode-hook)

;; perl

;; TODO: remove this.

;; (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; (setq cperl-invalid-face nil)

;; (eval-after-load "cperl-mode"
;;   '(progn
;;      (add-hook 'cperl-mode-hook 'electric-spacing-mode)
;;      (define-key cperl-mode-map (kbd "C-c <f1> f") 'cperl-perldoc)
;;      ))


;;; cscope

(condition-case nil
    (progn

(require 'ctypes)
(ctypes-auto-parse-mode 1)

(require 'xcscope)
(global-set-key "\C-css" 'cscope-find-this-symbol)
(global-set-key "\C-csd" 'cscope-find-global-definition)
(global-set-key "\C-csg" 'cscope-find-global-definition)
(global-set-key "\C-csG" 'cscope-find-global-definition-no-prompting)
(global-set-key "\C-csc" 'cscope-find-functions-calling-this-function)
(global-set-key "\C-csC" 'cscope-find-called-functions)
(global-set-key "\C-cst" 'cscope-find-this-text-string)
(global-set-key "\C-cse" 'cscope-find-egrep-pattern)
(global-set-key "\C-csf" 'cscope-find-this-file)
(global-set-key "\C-csi" 'cscope-find-files-including-file)
;; --- (The '---' indicates that this line corresponds to a menu separator.)
(global-set-key "\C-csb" 'cscope-display-buffer)
(global-set-key "\C-csB" 'cscope-display-buffer-toggle)
(global-set-key "\C-csn" 'cscope-next-symbol)
(global-set-key "\C-csN" 'cscope-next-file)
(global-set-key "\C-csp" 'cscope-prev-symbol)
(global-set-key "\C-csP" 'cscope-prev-file)
(global-set-key "\C-csu" 'cscope-pop-mark)
;; ---
(global-set-key "\C-csa" 'cscope-set-initial-directory)
(global-set-key "\C-csA" 'cscope-unset-initial-directory)
;; ---
(global-set-key "\C-csL" 'cscope-create-list-of-files-to-index)
(global-set-key "\C-csI" 'cscope-index-files)
(global-set-key "\C-csE" 'cscope-edit-list-of-files-to-index)
(global-set-key "\C-csW" 'cscope-tell-user-about-directory)
(global-set-key "\C-csS" 'cscope-tell-user-about-directory)
(global-set-key "\C-csT" 'cscope-tell-user-about-directory)
(global-set-key "\C-csD" 'cscope-dired-directory)

)
(error "cscope not found"))


;;; Ruby

(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(require 'ruby-electric)


;;; sgml, html, xml, css

(add-hook 'html-mode-hook (lambda () (electric-spacing-mode -1)))
(add-hook 'css-mode-hook 'rainbow-turn-on)

;; dvc

;; (require 'dvc-autoloads)

;; (load "~/.emacs.d/site-lisp/nxml/autostart.el")


(defun xwl-join-region (start end)
  "Pack marked region lines into the first line, separated by one blank.
Useful for packing c/c++ functions with one line or empty body."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-max))
    (while (not (bobp))
      (join-line))
    (when (search-forward "{" nil t 1)
      (insert " "))
    (when (search-forward "}" nil t 1)
      (backward-char)
      (insert " "))))

(global-set-key (kbd "C-c m j") 'xwl-join-region)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; (add-hook 'yaml-mode-hook
;;           '(lambda ()
;;              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; ,----
;; | ediff
;; `----

(require 'ediff)

(global-set-key (kbd "C-x v =") 'vc-ediff)

(global-set-key (kbd "C-c E b") 'ediff-buffers)
(global-set-key (kbd "C-c E B")
                (lambda ()
                  (interactive)
                  (let ((b1 (current-buffer))
                        (b2 (save-excursion
                              (other-window 1)
                              (current-buffer))))
                    (ediff-buffers b1 b2))))

(global-set-key (kbd "C-c E f") 'ediff-files)
(global-set-key (kbd "C-c E p b") 'ediff-patch-buffer)
(global-set-key (kbd "C-c E p f") 'ediff-patch-file)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Ignore whitespace-only diffs.
(setq-default ediff-ignore-similar-regions t)

(defadvice ediff-quit (around do-not-bother-me activate)
  "Just quit, please."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (let ((ctl-buf (current-buffer))
	(ctl-frm (selected-frame))
	(minibuffer-auto-raise t))
    ;; (if (y-or-n-p (format "Quit this Ediff session%s? "
    ;;     		  (if (ediff-buffer-live-p ediff-meta-buffer)
    ;;     		      " & show containing session group" "")))
	(progn
	  (message "")
	  (set-buffer ctl-buf)
	  (ediff-really-quit reverse-default-keep-variants))
      ;; (select-frame ctl-frm)
      ;; (raise-frame ctl-frm)
      ;; (message ""))
    ))

;; refine more ediffs

;; (setq xwl-ediff-refine-number 10)

;; (defadvice ediff-make-fine-diffs (around xwl-refine-more)
(defadvice ediff-install-fine-diff-if-necessary (around xwl-refine-more)
  ;; (let* ((total (min xwl-ediff-refine-number ediff-number-of-differences))
  ;;        (start (max 0 (- ediff-current-difference (/ total 2))))
  ;;        (end (min ediff-number-of-differences
  ;;                  (+ ediff-current-difference (/ total 2)))))

  ;;   (when (zerop start)
  ;;     (setq end (min ediff-current-difference
  ;;                    (+ ediff-current-difference
  ;;                       (- total ediff-current-difference)))))

  ;;   (when (eq end ediff-number-of-differences)
  ;;     (setq start (max 0 (- ediff-current-difference
  ;;                           (- total
  ;;                              (- ediff-number-of-differences
  ;;                                 ediff-current-difference))))))

  ;;   (message " (%d, %d, %d)"  start ediff-current-difference end)

    ;; (dotimes (i (- end start))
    ;;   (ad-set-arg 0 (+ start i))
  ;;   ad-do-it)))


  (dotimes (i ediff-number-of-differences)
    (ad-set-arg 0 i)
    ad-do-it))

;; Stop ediff session when where is no difference.
(defun ediff-setup (buffer-A file-A buffer-B file-B buffer-C file-C
                             startup-hooks setup-parameters
                             &optional merge-buffer-file)
  (run-hooks 'ediff-before-setup-hook)
  ;; ediff-convert-standard-filename puts file names in the form appropriate
  ;; for the OS at hand.
  (setq file-A (ediff-convert-standard-filename (expand-file-name file-A)))
  (setq file-B (ediff-convert-standard-filename (expand-file-name file-B)))
  (if (stringp file-C)
      (setq file-C
            (ediff-convert-standard-filename (expand-file-name file-C))))
  (if (stringp merge-buffer-file)
      (progn
        (setq merge-buffer-file
              (ediff-convert-standard-filename
               (expand-file-name merge-buffer-file)))
        ;; check the directory exists
        (or (file-exists-p (file-name-directory merge-buffer-file))
            (error "Directory %s given as place to save the merge doesn't exist"
                   (abbreviate-file-name
                    (file-name-directory merge-buffer-file))))
        (if (and (file-exists-p merge-buffer-file)
                 (file-directory-p merge-buffer-file))
            (error "The merge buffer file %s must not be a directory"
                   (abbreviate-file-name merge-buffer-file)))
        ))
  (let* ((control-buffer-name
          (ediff-unique-buffer-name "*Ediff Control Panel" "*"))
         (control-buffer (ediff-with-current-buffer buffer-A
                           (get-buffer-create control-buffer-name))))
    (ediff-with-current-buffer control-buffer
      (ediff-mode)

      (make-local-variable 'ediff-use-long-help-message)
      (make-local-variable 'ediff-prefer-iconified-control-frame)
      (make-local-variable 'ediff-split-window-function)
      (make-local-variable 'ediff-default-variant)
      (make-local-variable 'ediff-merge-window-share)
      (make-local-variable 'ediff-window-setup-function)
      (make-local-variable 'ediff-keep-variants)

      (make-local-variable 'window-min-height)
      (setq window-min-height 2)

      (if (featurep 'xemacs)
          (make-local-hook 'ediff-after-quit-hook-internal))

      ;; unwrap set up parameters passed as argument
      (while setup-parameters
        (set (car (car setup-parameters)) (cdr (car setup-parameters)))
        (setq setup-parameters (cdr setup-parameters)))

      ;; set variables classifying the current ediff job
      ;; must come AFTER setup-parameters
      (setq ediff-3way-comparison-job (ediff-3way-comparison-job)
            ediff-merge-job (ediff-merge-job)
            ediff-merge-with-ancestor-job (ediff-merge-with-ancestor-job)
            ediff-3way-job (ediff-3way-job)
            ediff-diff3-job (ediff-diff3-job)
            ediff-narrow-job (ediff-narrow-job)
            ediff-windows-job (ediff-windows-job)
            ediff-word-mode-job (ediff-word-mode-job))

      ;; Don't delete variants in case of ediff-buffer-* jobs without asking.
      ;; This is because one may lose work---dangerous.
      (if (string-match "buffer" (symbol-name ediff-job-name))
          (setq ediff-keep-variants t))

      (if (featurep 'xemacs)
          (make-local-hook 'pre-command-hook))

      (if (ediff-window-display-p)
          (add-hook 'pre-command-hook 'ediff-spy-after-mouse nil 'local))
      (setq ediff-mouse-pixel-position (mouse-pixel-position))

      ;; adjust for merge jobs
      (if ediff-merge-job
          (let ((buf
                 ;; If default variant is `combined', the right stuff is
                 ;; inserted by ediff-do-merge
                 ;; Note: at some point, we tried to put ancestor buffer here
                 ;; (which is currently buffer C.  This didn't work right
                 ;; because the merge buffer will contain lossage: diff regions
                 ;; in the ancestor, which correspond to revisions that agree
                 ;; in both buf A and B.
                 (cond ((eq ediff-default-variant 'default-B)
                        buffer-B)
                       (t buffer-A))))

            (setq ediff-split-window-function
                  ediff-merge-split-window-function)

            ;; remember the ancestor buffer, if any
            (setq ediff-ancestor-buffer buffer-C)

            (setq buffer-C
                  (get-buffer-create
                   (ediff-unique-buffer-name "*ediff-merge" "*")))
            (with-current-buffer buffer-C
              (insert-buffer-substring buf)
              (goto-char (point-min))
              (funcall (ediff-with-current-buffer buf major-mode))
              (widen)                   ; merge buffer is always widened
              (add-hook 'local-write-file-hooks 'ediff-set-merge-mode nil t)
              )))
      (setq buffer-read-only nil
            ediff-buffer-A buffer-A
            ediff-buffer-B buffer-B
            ediff-buffer-C buffer-C
            ediff-control-buffer control-buffer)

      (ediff-choose-syntax-table)

      (setq ediff-control-buffer-suffix
            (if (string-match "<[0-9]*>" control-buffer-name)
                (substring control-buffer-name
                           (match-beginning 0) (match-end 0))
              "")
            ediff-control-buffer-number
            (max
             0
             (1-
              (string-to-number
               (substring
                ediff-control-buffer-suffix
                (or
                 (string-match "[0-9]+" ediff-control-buffer-suffix)
                 0))))))

      (setq ediff-error-buffer
            (get-buffer-create (ediff-unique-buffer-name "*ediff-errors" "*")))

      (with-current-buffer ediff-error-buffer
        (setq buffer-undo-list t))

      (ediff-with-current-buffer buffer-A (ediff-strip-mode-line-format))
      (ediff-with-current-buffer buffer-B (ediff-strip-mode-line-format))
      (if ediff-3way-job
          (ediff-with-current-buffer buffer-C (ediff-strip-mode-line-format)))
      (if (ediff-buffer-live-p ediff-ancestor-buffer)
          (ediff-with-current-buffer ediff-ancestor-buffer
            (ediff-strip-mode-line-format)))

      (ediff-save-protected-variables)  ; save variables to be restored on exit

      ;; ediff-setup-diff-regions-function must be set after setup
      ;; parameters are processed.
      (setq ediff-setup-diff-regions-function
            (if ediff-diff3-job
                'ediff-setup-diff-regions3
              'ediff-setup-diff-regions))

      (setq ediff-wide-bounds
            (list (ediff-make-bullet-proof-overlay
                   '(point-min) '(point-max) ediff-buffer-A)
                  (ediff-make-bullet-proof-overlay
                   '(point-min) '(point-max) ediff-buffer-B)
                  (ediff-make-bullet-proof-overlay
                   '(point-min) '(point-max) ediff-buffer-C)))

      ;; This has effect only on ediff-windows/regions
      ;; In all other cases, ediff-visible-region sets visibility bounds to
      ;; ediff-wide-bounds, and ediff-narrow-bounds are ignored.
      (if ediff-start-narrowed
          (setq ediff-visible-bounds ediff-narrow-bounds)
        (setq ediff-visible-bounds ediff-wide-bounds))

      (ediff-set-keys)                  ; comes after parameter setup

      ;; set up ediff-narrow-bounds, if not set
      (or ediff-narrow-bounds
          (setq ediff-narrow-bounds ediff-wide-bounds))

      ;; All these must be inside ediff-with-current-buffer control-buffer,
      ;; since these vars are local to control-buffer
      ;; These won't run if there are errors in diff
      (ediff-with-current-buffer ediff-buffer-A
        (ediff-nuke-selective-display)
        (run-hooks 'ediff-prepare-buffer-hook)
        (if (ediff-with-current-buffer control-buffer ediff-merge-job)
            (setq buffer-read-only t))
        ;; add control-buffer to the list of sessions--no longer used, but may
        ;; be used again in the future
        (or (memq control-buffer ediff-this-buffer-ediff-sessions)
            (setq ediff-this-buffer-ediff-sessions
                  (cons control-buffer ediff-this-buffer-ediff-sessions)))
        (if ediff-make-buffers-readonly-at-startup
            (setq buffer-read-only t))
        )

      (ediff-with-current-buffer ediff-buffer-B
        (ediff-nuke-selective-display)
        (run-hooks 'ediff-prepare-buffer-hook)
        (if (ediff-with-current-buffer control-buffer ediff-merge-job)
            (setq buffer-read-only t))
        ;; add control-buffer to the list of sessions
        (or (memq control-buffer ediff-this-buffer-ediff-sessions)
            (setq ediff-this-buffer-ediff-sessions
                  (cons control-buffer ediff-this-buffer-ediff-sessions)))
        (if ediff-make-buffers-readonly-at-startup
            (setq buffer-read-only t))
        )

      (if ediff-3way-job
          (ediff-with-current-buffer ediff-buffer-C
            (ediff-nuke-selective-display)
            ;; the merge buffer should never be narrowed
            ;; (it can happen if it is on rmail-mode or similar)
            (if (ediff-with-current-buffer control-buffer ediff-merge-job)
                (widen))
            (run-hooks 'ediff-prepare-buffer-hook)
            ;; add control-buffer to the list of sessions
            (or (memq control-buffer ediff-this-buffer-ediff-sessions)
                (setq ediff-this-buffer-ediff-sessions
                      (cons control-buffer
                            ediff-this-buffer-ediff-sessions)))
            (if ediff-make-buffers-readonly-at-startup
                (setq buffer-read-only t)
              (setq buffer-read-only nil))
            ))

      (if (ediff-buffer-live-p ediff-ancestor-buffer)
          (ediff-with-current-buffer ediff-ancestor-buffer
            (ediff-nuke-selective-display)
            (setq buffer-read-only t)
            (run-hooks 'ediff-prepare-buffer-hook)
            (or (memq control-buffer ediff-this-buffer-ediff-sessions)
                (setq ediff-this-buffer-ediff-sessions
                      (cons control-buffer
                            ediff-this-buffer-ediff-sessions)))
            ))

      ;; the following must be after setting up  ediff-narrow-bounds AND after
      ;; nuking selective display
      (funcall ediff-setup-diff-regions-function file-A file-B file-C)
      (setq ediff-number-of-differences (length ediff-difference-vector-A))
      (when (zerop ediff-number-of-differences) ; xwl
        (mapc 'kill-buffer (list ediff-control-buffer
                                 ediff-error-buffer
                                 ediff-diff-buffer))
        (message "xwl: %S" ediff-diff-buffer)
        (error "No difference"))
      (setq ediff-current-difference -1)

      (ediff-make-current-diff-overlay 'A)
      (ediff-make-current-diff-overlay 'B)
      (if ediff-3way-job
          (ediff-make-current-diff-overlay 'C))
      (if ediff-merge-with-ancestor-job
          (ediff-make-current-diff-overlay 'Ancestor))

      (ediff-setup-windows buffer-A buffer-B buffer-C control-buffer)

      (let ((shift-A (ediff-overlay-start
                      (ediff-get-value-according-to-buffer-type
                       'A ediff-narrow-bounds)))
            (shift-B (ediff-overlay-start
                      (ediff-get-value-according-to-buffer-type
                       'B ediff-narrow-bounds)))
            (shift-C (ediff-overlay-start
                      (ediff-get-value-according-to-buffer-type
                       'C ediff-narrow-bounds))))
        ;; position point in buf A
        (save-excursion
          (select-window ediff-window-A)
          (goto-char shift-A))
        ;; position point in buf B
        (save-excursion
          (select-window ediff-window-B)
          (goto-char shift-B))
        (if ediff-3way-job
            (save-excursion
              (select-window ediff-window-C)
              (goto-char shift-C)))
        )

      (select-window ediff-control-window)
      (ediff-visible-region)

      (run-hooks 'startup-hooks)
      (ediff-arrange-autosave-in-merge-jobs merge-buffer-file)

      (ediff-refresh-mode-lines)
      (setq buffer-read-only t)
      (setq ediff-session-registry
            (cons control-buffer ediff-session-registry))
      (ediff-update-registry)
      (if (ediff-buffer-live-p ediff-meta-buffer)
          (ediff-update-meta-buffer
           ediff-meta-buffer nil ediff-meta-session-number))
      (run-hooks 'ediff-startup-hook)
      )                                 ; eval in control-buffer
    control-buffer))

(add-hook 'ediff-startup-hook 'ediff-next-difference)

(defun xwl-ediff-refine-more (&optional start end)
  (interactive)
  (when start (setq xwl-ediff-refine-start start))
  (when end (setq xwl-ediff-refine-end end))

  (unless (facep 'xwl-ediff-odd-diff-A)
    (copy-face 'ediff-odd-diff-A 'xwl-ediff-odd-diff-A)
    (copy-face 'ediff-even-diff-A 'xwl-ediff-even-diff-A)
    (copy-face 'ediff-odd-diff-B 'xwl-ediff-odd-diff-B)
    (copy-face 'ediff-even-diff-B 'xwl-ediff-even-diff-B)
    (copy-face 'ediff-odd-diff-C 'xwl-ediff-odd-diff-C)
    (copy-face 'ediff-even-diff-C 'xwl-ediff-even-diff-C))

  (unless (face-equal 'ediff-current-diff-A 'ediff-odd-diff-A)
    (copy-face 'ediff-current-diff-A 'ediff-odd-diff-A)
    (copy-face 'ediff-current-diff-A 'ediff-even-diff-A)
    (copy-face 'ediff-current-diff-B 'ediff-odd-diff-B)
    (copy-face 'ediff-current-diff-B 'ediff-even-diff-B)
    (copy-face 'ediff-current-diff-C 'ediff-odd-diff-C)
    (copy-face 'ediff-current-diff-C 'ediff-even-diff-C))

  (ad-activate 'ediff-install-fine-diff-if-necessary))

(defun xwl-ediff-restore-original-refine ()
  (interactive)
  (when (and (facep 'xwl-ediff-odd-diff-A)
             (not (face-equal 'xwl-ediff-odd-diff-A 'ediff-odd-diff-A)))
    (copy-face 'ediff-odd-diff-A 'xwl-ediff-odd-diff-A)
    (copy-face 'ediff-even-diff-A 'xwl-ediff-even-diff-A)
    (copy-face 'ediff-odd-diff-B 'xwl-ediff-odd-diff-B)
    (copy-face 'ediff-even-diff-B 'xwl-ediff-even-diff-B)
    (copy-face 'ediff-odd-diff-C 'xwl-ediff-odd-diff-C)
    (copy-face 'ediff-even-diff-C 'xwl-ediff-even-diff-C))

  (ad-deactivate 'ediff-install-fine-diff-if-necessary))

;; git

;; Check file ~/.gitconfig

;; (when (eq window-system 'w32)
;;   (shell-command "git config --global user.email william.xwl@gmail.com")
;;   (shell-command "git config --global user.name 'William Xu'"))

;; ,----
;; | cedet
;; `----

;; (semantic-mode 1)

;; (eval-after-load 'cedet
;;   '(progn
;;      ;; (global-ede-mode 1)
;;      (semantic-load-enable-gaudy-code-helpers)
;;      (semantic-clang-activate)

;;      (setq xwl-include "/Library/Frameworks/QtCore.framework/Headers")
;;      (semantic-add-system-include xwl-include 'c++-mode)
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat xwl-include "/qconfig.h"))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat xwl-include "/qconfig-dist.h"))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat xwl-include "/qglobal.h"))

;;      ;; (global-less-minor-mode -1)
;;      ;; (let ((f "/usr/include/sys/cdefs.h"))
;;      ;;   (save-window-excursion
;;      ;;     (with-current-buffer (find-file-noselect f)
;;      ;;       (bovinate))
;;      ;;     (add-to-list 'semantic-lex-c-preprocessor-symbol-file f)
;;      ;;     (semantic-gcc-setup)))
;;      ;; (global-less-minor-mode 1)

;;      ;; TODO, bind key to trigger completion.
;;      ;; 'semantic-ia-complete-symbol-menu
;;      ))

;; 2014/06/18 try
(unless (eq system-type 'windows-nt)
  (semantic-load-enable-excessive-code-helpers)
  (setq semantic-decoration-styles
        ;; disable boundary overline!
        (remove-if (lambda (dec) (equal (car dec) "semantic-tag-boundary"))
                   semantic-decoration-styles))
  (global-semantic-stickyfunc-mode -1)

  (eval-after-load 'semantic/imenu
    '(progn
       (defadvice semantic-create-imenu-index (after combine-with-default activate)
         (setq ad-return-value (append (imenu-default-create-index-function)
                                       ad-return-value))))))

(eval-after-load 'diff-mode
  '(progn
     (define-key diff-mode-shared-map (kbd "k") nil)
     (define-key diff-mode-shared-map (kbd "M-o") nil)
     ))

;; s40
(add-to-list 'auto-mode-alist '("\\`Make.*" . makefile-mode))

;;; misc

(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("make.*" . makefile-mode))

(eval-after-load 'gtags
  '(progn
     ;; As tab for completion takes long for large project, disable them for
     ;; very large projects.
     (setq xwl-gtags-large-directories-regexp
           (regexp-opt '("s40_sw" "memory_component_" "linux")))

     (defmacro xwl-advice-gtags-for-large-projects (function)
       `(defadvice ,function (around disable-ido-for-large-project activate)
          (if (string-match xwl-gtags-large-directories-regexp default-directory)
              (let ((completing-read-function 'completing-read-default))
                ad-do-it)
            ad-do-it)))

     (xwl-advice-gtags-for-large-projects gtags-find-any)
     (xwl-advice-gtags-for-large-projects gtags-find-any-reference)

     (xwl-advice-gtags-for-large-projects gtags-find-function)
     (xwl-advice-gtags-for-large-projects gtags-find-symbol)
     (xwl-advice-gtags-for-large-projects gtags-find-text)
     (xwl-advice-gtags-for-large-projects gtags-find-file)
     (xwl-advice-gtags-for-large-projects gtags-find-function-reference)
     (xwl-advice-gtags-for-large-projects gtags-find-symbol-reference)

     (define-key gtags-mode-map (kbd "C-c C-b") 'gtags-pop-stack)
     (define-key gtags-mode-map (kbd "C-c C-f") 'gtags-find-any)
     (define-key gtags-mode-map (kbd "C-c C-r") 'gtags-find-any-reference)

     (define-key gtags-select-mode-map (kbd "C-c C-b") 'gtags-pop-stack)
     (define-key gtags-select-mode-map (kbd "C-c C-f") 'gtags-find-any)
     (define-key gtags-select-mode-map (kbd "C-c C-r") 'gtags-find-any-reference)

     ;; (global-set-key (kbd "C-0") 'gtags-pop-stack)
     ;; (global-set-key (kbd "C-9") 'gtags-find-any)
     ;; (global-set-key (kbd "C-8") 'gtags-find-any-reference)
     ;; (global-set-key (kbd "C-7") 'gtags-find-text)

     (global-set-key (kbd "C-0") 'gtags-pop-stack)
     (global-set-key (kbd "C-9") 'gtags-find-any)
     (global-set-key (kbd "C-8") 'gtags-find-any-reference)
     (global-set-key (kbd "C-7") 'gtags-find-text)

     ;; (global-set-key (kbd "<mouse-2>") '(lambda ()
     ;;                                      (interactive)
     ;;                                      ;; FIXME:  how to jump to mouse position.
     ;;                                      ;; (call-interactively 'mouse-drag-region)
     ;;                                      (call-interactively 'gtags-find-any)))

     ;; (global-set-key (kbd "<mouse-3>") '(lambda ()
     ;;                                      (interactive)
     ;;                                      ;; (goto-char (mouse-position))
     ;;                                      (call-interactively 'gtags-pop-stack)))

     (add-hook 'gtags-select-mode-hook (lambda () (hl-line-mode 1)))

     (define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
     (define-key gtags-select-mode-map (kbd "n") 'next-line)
     (define-key gtags-select-mode-map (kbd "p") 'previous-line)

     ))

(add-hook 'makefile-mode-hook (lambda () (gtags-mode 1)))

;;; imenu

(setq which-func-modes '(emacs-lisp-mode c-mode c++-mode objc-mode perl-mode
                                         cperl-mode python-mode makefile-mode
                                         sh-mode fortran-mode f90-mode ada-mode
                                         diff-mode))
(which-function-mode 1)


(add-to-list 'auto-mode-alist '("\\.pac$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc" . sh-mode))

(provide 'xwl-programming)

;;; xwl-programming.el ends here
