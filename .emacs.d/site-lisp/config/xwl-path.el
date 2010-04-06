;;; xwl-path.el --- path setup

;; Copyright (C) 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;;; Code:

(setq xwl-site-lisp "~/.emacs.d/site-lisp")

(setq xwl-makefile-subdir-list
      (mapcar (lambda (f) (concat xwl-site-lisp "/" f))
              '("."
                "auto-complete-1.0"
                "debian"
                "dictionary-el"
                "haskell-mode-2.4"
                "qterm"
                "ruby"
                "slightly-modified"
                "twittering-mode"
                "wget-el"
                "xwl-elisp"
                "xwl-elisp/dashboard"
                "xwl-elisp/ga"
                "xwl-elisp/wubi"
                )))

(mapc (lambda (path) (add-to-list 'load-path path))
      (append
       ;; essential
       (list xwl-site-lisp
             (concat xwl-site-lisp "/config"))
       ;; others in site-lisp
       xwl-makefile-subdir-list
       ;; optional
       (list "~/repo/cvs/mmm-mode"
             "~/repo/misc/auctex-11.84"
             "~/repo/cvs/bbdb/lisp"
             "~/repo/git/emms/lisp"

             "~/repo/git/dvc/lisp"

             ;; Weird w3m! Without this, `emacs -batch' will complain.
             ;; "~/share/emacs/site-lisp/w3m"

             "~/repo/cvs/emacs-w3m"
             "~/repo/svn/chicken-eggs"

             "~/etc"

             "~/repo/cvs/cedet/common")))

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
    (progn
      ;; (setenv "PATH" (concat (getenv "PATH") ";C:/OpenSSL/bin;C:/Program Files/Haskell/bin;C:/ghc/ghc-6.12.1/bin"))
      ;; (setq exec-path (split-string (getenv "PATH") ";"))
      nil)

  (setenv "TERM" "xterm-color")
  (setq exec-path (split-string (getenv "PATH") ":")))

;; no need of `provide'.

;;; xwl-path.el ends here
