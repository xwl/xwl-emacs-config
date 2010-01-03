;;; xwl-path.el --- path setup

;; Copyright (C) 2009 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;;; Code:

;; ,----
;; | load-path
;; `----

(setq xwl-load-path "~/.emacs.d/site-lisp")

;; (load "~/.emacs.d/site-lisp/xwl-elisp/xwl-subr.el")

(mapc (lambda (path) (add-to-list 'load-path path))
      (list xwl-load-path
            "~/repo/cvs/mmm-mode"
            "~/repo/misc/auctex-11.84"
            "~/repo/cvs/bbdb/lisp"
            "~/repo/git/emms/lisp"

            "~/repo/git/xwl-elisp"
            "~/repo/git/xwl-elisp/dashboard"
            "~/repo/git/xwl-elisp/ga"

            "~/repo/git/dvc/lisp"

            ;; Weird w3m! Without this, `emacs -batch' will complain.
            ;; "~/share/emacs/site-lisp/w3m"

            "~/repo/cvs/emacs-w3m"
            "~/repo/svn/chicken-eggs"

            "~/etc"
                    
            "~/repo/cvs/cedet/common"))

(when (< emacs-major-version 23)
  (setq load-path `("~/repo/git/erc"
                    "~/repo/git/remember"
                    "~/repo/git/org-mode/lisp"
                    "~/repo/cvs/tramp/lisp"

                    ,@load-path)))

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

(let ((orig default-directory))
  (cd xwl-load-path)
  (normal-top-level-add-subdirs-to-load-path)
  (cd orig))

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

(unless (eq system-type 'windows-nt)
  (setenv "TERM" "xterm-color")
  (setq exec-path (split-string (getenv "PATH") ":")))

;; no need of `provide'.

;;; xwl-path.el ends here
