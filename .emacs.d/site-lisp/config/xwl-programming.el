;;; xwl-programming.el --- programming config

;; Copyright (C) 2007, 2009, 2010, 2011 William Xu

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
  (setq tab-width 4)

  ;; Don't enable for lex/yacc files.
  (when (and (buffer-file-name) ; mmm-mode submodes don't have a  valid buffer name.
             (not (string-match "\\.l$\\|\\.y$" (buffer-file-name))))
    (c-toggle-auto-hungry-state 1))

  (add-to-list 'c-hanging-braces-alist '(brace-list-close))

  ;; (glasses-mode 1)
  ;; (hs-minor-mode 1)
  ;; (hs-hide-all)
  ;; (xwl-hs-minor-mode-hook)
  ;; (hide-ifdef-mode 1)
  (auto-fill-mode -1)
  (smart-operator-mode 1)
  (abbrev-mode 1)
  (cwarn-mode 1)
  ;;  (which-func-mode 1)
  (when (fboundp 'doxymacs-mode)
    (doxymacs-mode 1)
    (doxymacs-font-lock))
  (subword-mode 1)
  ;; (flyspell-prog-mode)
  (gtags-mode 1)

  (setq hide-ifdef-initially t)

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
                                 (smart-operator-insert "(")
                               (smart-operator-insert "(" t))))
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

    (local-set-key "(" '(lambda ()
                          (interactive)
                          (call-interactively 'c-electric-paren)
                          (insert ")")
                          (backward-char)))

    (local-set-key ")" 'c-electric-paren)
    (local-set-key "}" 'c-electric-brace)))

(add-hook 'c-mode-common-hook 'xwl-c-mode-common-hook)

;;; Java
;; ------

;; (require 'jde)

;;; C, C++

(defun xwl-set-c-c++-style ()
  ;; (if (file-exists-p "../group")
  ;;     ;; symbian c++
  ;;     (progn
  ;;       (c-set-style "whitesmith")
  ;;       (setq c-cleanup-list '()))
    (c-set-style "k&r")
    ;; TODO, check this.
    (setq c-basic-offset 4))
;)

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
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.iby\\'" . c-mode))
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
  (smart-operator-mode 1)
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

     (add-hook 'sql-mode-hook 'smart-operator-mode-on)
     (add-hook 'sql-interactive-mode-hook 'smart-operator-mode-on)
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

;;; skeletons
;; -----------

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
                  '("/usr/include"
                    "~/include")))) ">\n")

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
                  '("/usr/include"
                    "/usr/include/c++/4.0.0"
                    ;; Qt4 on mac
                    "/sw/lib/qt4-mac/include/QtCore"
                    "/sw/lib/qt4-mac/include/QtGui"
                    "/sw/lib/qt4-mac/include"
                    )))) ">\n")

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

;;; elisp
;; -------

(defun xwl-lisp-mode-hook ()
  ;; (which-func-mode 1)

  (set (make-local-variable 'outline-regexp) ";;;+ ")
  (outline-minor-mode 1)

  (local-set-key (kbd "<backtab>") 'lisp-complete-symbol)
  (local-set-key (kbd "<S-tab>") 'lisp-complete-symbol)
  (local-set-key (kbd "C-x C-r") 'eval-region)
  (local-set-key (kbd "C-x C-b") 'eval-buffer))

(mapc (lambda (hook)
        (mapc (lambda (func)
                (add-hook hook func))
              '(turn-on-eldoc-mode xwl-lisp-mode-hook smart-operator-mode-on)))
      '(lisp-mode-hook emacs-lisp-mode-hook))

;;; scheme
;; --------

(require 'scheme)
(require 'chicken-scheme-extras)

(setq scheme-program-name "csi")
(defun xwl-scheme-mode-hook ()
  (setq comment-add 1)

  (set (make-local-variable 'outline-regexp) ";;;+ ")
  (outline-minor-mode 1)

  (local-set-key (kbd "C-x C-r") 'scheme-send-region)
  (local-set-key (kbd "C-x C-b") 'xwl-scheme-send-buffer))

(add-hook 'scheme-mode-hook 'xwl-scheme-mode-hook)
(add-hook 'scheme-mode-hook 'smart-operator-mode-on)

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

  (glasses-mode 1)
  (smart-operator-mode))

(defun xwl-inferior-haskell-mode-hook ()
  (glasses-mode 1)
  (smart-operator-mode))

(add-hook 'haskell-mode-hook 'xwl-haskell-mode-hook)
(add-hook 'inferior-haskell-mode-hook 'xwl-inferior-haskell-mode-hook)

(global-set-key (kbd "C-c i h") 'run-haskell)


;;; Compilation Mode

;; (eval-after-load 'buffer-action
;;   '(progn
;;      (defadvice buffer-action-compile (before save-layout activate)
;;        (setq xwl-layout-before-compilation (current-window-configuration)))
;;      ))

(defadvice compile (around use-new-frame activate)
  (let ((pop-up-frames t))
    ad-do-it))

(defun xwl-compilation-exit-autoclose (status code msg)
  (cond ((not (and (eq status 'exit) (zerop code)))
         (setq msg "failed"))
        ((with-current-buffer "*compilation*"
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "warning:" nil t 1)))
         (setq msg "has warnings"))
        (t
         (run-at-time 1 nil (lambda ()
                              ;; (set-window-configuration
                              ;; xwl-layout-before-compilation)
                              ;; (delete-windows-on "*compilation*")
                              (xwl-delete-frame)
                              ))
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
           `(("\\.tex$" "xelatex %f" "%n.pdf" "open -a Preview %n.pdf &")

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

             (c++-mode "g++ -O2 \"%f\" -lm -I/sw/include -o %n" ,@(if (eq system-type 'windows-nt)
                                                                      '("%n.exe" "%n.exe")
                                                                    '("%n" "./%n")))
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
;;      (add-hook 'cperl-mode-hook 'smart-operator-mode-on)
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

(add-hook 'html-mode-hook (lambda () (smart-operator-mode -1)))
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

(defun xwl-ediff-revision ()
  "Compare current state with latest revision, no questions please."
  (interactive)
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "Buffer %s is modified. Save buffer? "
                             (buffer-name))))
      (save-buffer (current-buffer)))
  (let ((rev1 "")
        (rev2 "")
        (startup-hooks '()))
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev1 rev2 startup-hooks)))

(global-set-key (kbd "C-x v =") 'xwl-ediff-revision)

(global-set-key (kbd "C-c E b") 'ediff-buffers)
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

;; git

;; Check file ~/.gitconfig

;; (when (eq window-system 'w32)
;;   (shell-command "git config --global user.email william.xwl@gmail.com")
;;   (shell-command "git config --global user.name 'William Xu'"))

;; ,----
;; | cedet
;; `----

;; (semantic-mode 1)

;; (eval-after-load 'semantic-imenu
;;   '(progn
;;      (defadvice semantic-create-imenu-index (after combine-with-default activate)
;;        (setq ad-return-value (append (imenu-default-create-index-function)
;;                                      ad-return-value)))))

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
           (regexp-opt '("s40_sw" "memory_component_")))

     (defmacro xwl-advice-gtags-for-large-projects (function)
       `(defadvice ,function (around disable-ido-for-large-project activate)
          (if (string-match xwl-gtags-large-directories-regexp default-directory)
              (let ((completing-read-function 'completing-read-default))
                ad-do-it)
            ad-do-it)))

     (xwl-advice-gtags-for-large-projects gtags-find-tag)
     (xwl-advice-gtags-for-large-projects gtags-find-rtag)
     (xwl-advice-gtags-for-large-projects gtags-find-file)

     (global-set-key (kbd "C-0") 'gtags-pop-stack)
     (global-set-key (kbd "C-9") 'gtags-find-tag)
     (global-set-key (kbd "C-8") 'gtags-find-file)
     (global-set-key (kbd "C-7") 'gtags-find-rtag)

     (add-hook 'gtags-select-mode-hook (lambda () (hl-line-mode 1)))

     (define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
     (define-key gtags-select-mode-map (kbd "n") 'next-line)
     (define-key gtags-select-mode-map (kbd "p") 'previous-line)

     ))


(provide 'xwl-programming)

;;; xwl-programming.el ends here
