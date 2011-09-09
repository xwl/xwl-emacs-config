;;; xwl-path.el --- path setup

;; Copyright (C) 2009, 2010, 2011 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;;; Code:

(require 'cl)

(setq xwl-site-lisp "~/.emacs.d/site-lisp")

(setq xwl-makefile-subdir-list
      (remove-if-not
       'file-exists-p
       (mapcar (lambda (f) (concat xwl-site-lisp "/" f))
               '("."
                 "auto-complete"
                 "debian"
                 "dictionary-el"
                 "haskell-mode-2.4"
                 "qterm"
                 "ruby"
                 "slightly-modified"
                 "twittering-mode"
                 "wget-el"
                 "emms/lisp"
                 "bbdb-vcard"
                 "nyan-mode"

                 "xwl-elisp"
                 "xwl-elisp/dashboard"
                 "xwl-elisp/ga"

                 "config/autoload"
                 ))))

(mapc (lambda (path) (add-to-list 'load-path path))
      (append
       ;; essential
       (list xwl-site-lisp
             (concat xwl-site-lisp "/config")
             ;; FIXME: byte-compile never stops?
             (concat xwl-site-lisp "/xwl-elisp/wubi"))
       ;; others in site-lisp
       xwl-makefile-subdir-list
       ;; optional
       (list "~/repo/cvs/mmm-mode"
             "~/repo/misc/auctex-11.84"
             "~/repo/cvs/bbdb/lisp"
             "~/repo/git/dvc/lisp"

             ;; Weird w3m! Without this, `emacs -batch' will complain.
             ;; "~/share/emacs/site-lisp/w3m"

             "~/repo/cvs/emacs-w3m"
             "~/repo/svn/chicken-eggs"

             "~/etc"

             )))

(setenv "INFOPATH"
        (mapconcat 'identity
                   `(
                     "~/info/"

                     ;; nextstep stores build in a bundle.
                     ,@(unless (eq system-type 'darwin)
                         '("~/share/info" ))

                     "/sw/share/info"
                     "~/repo/git/dvc/texinfo"

                     "/sw/lib/chicken/1/"

                     ,(getenv "INFOPATH")
                     )
                   ":"))

;; ,----
;; | PATH & exec-path
;; `----

;; Note: w32 will also search for dos batch file under current directory by default.
(when (eq system-type 'darwin)
  ;; 从 .bashrc 拷贝来的！
  (setenv "PATH"
	  (mapconcat 'identity
		     '("/Users/william/bin" "/Users/william/bin/scripts"
		       "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin"
		       "/usr/texbin" "/usr/X11/bin"
		       "/usr/pkg/bin" "/usr/pkg/sbin"
		       "/sw/bin" "/sw/sbin"
		       "/usr/X11R6/bin" )
		     ":")))

(if (eq system-type 'windows-nt)
    ;; Seems gnuwin32 image library has to be set before emacs starts.
    (let ((paths `(;; "c:/Program Files/GnuWin32/bin"
                  ,(file-truename "~/bin/git/bin")
                  "C:/APPS/git/bin"
                   )))
      (setenv "PATH" (mapconcat 'identity `(,(getenv "PATH") ,@paths) ";"))
      (setq exec-path (split-string (getenv "PATH") ";")))
  (setenv "TERM" "xterm-color")
  (setq exec-path (split-string (getenv "PATH") ":")))

;; load this earlier to use bzr version
(unless (featurep 'cedet)
  (unless (eq system-type 'windows-nt)
  (load "~/repo/bzr/cedet/common/cedet.el")
  ;; (global-ede-mode 1)
  (semantic-load-enable-gaudy-code-helpers)))

;; no need of `provide'.

;;; xwl-path.el ends here
