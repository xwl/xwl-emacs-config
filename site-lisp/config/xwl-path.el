;;; xwl-path.el --- path setup

;; Copyright (C) 2009, 2010, 2011, 2012, 2013 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;;; Code:

(require 'cl)

;; ,----
;; | PATH & exec-path
;; `----

;; Note: w32 will also search for dos batch file under current directory by default.
(case system-type
  ((darwin)
   (setenv "PATH"
           (concat "/usr/local/bin:"
                   (shell-command-to-string
                    "source $HOME/.bashrc && printf $PATH")))
   (setq exec-path (split-string (getenv "PATH") ":")))
  
  ((windows-nt)
   ;; Seems gnuwin32 image library has to be set before emacs starts.
   (let ((paths `( ;; "c:/Program Files/GnuWin32/bin"
                  ,(file-truename "~/bin/git/bin")
                  "C:/APPS/git/bin"
                  ,(file-truename "~/bin/glo597wb/bin")
                  )))
     (setenv "PATH" (mapconcat 'identity `(,(getenv "PATH") ,@paths) ";"))
     (setq exec-path (split-string (getenv "PATH") ";")))))

(setenv "TERM" "xterm-color")

(setq xwl-emacs-top "~/.emacs.d/site-lisp/")

(setq xwl-makefile-subdir-list
      (remove-if-not
       'file-exists-p
       (append
        (mapcar (lambda (f) (concat xwl-emacs-top f))
                '("."
                  
                  "debian" "dictionary-el" "haskell-mode-2.4" "qterm" "ruby"
                  "slightly-modified" "wget-el" "bbdb-vcard" "nyan-mode"
                  
                  "xwl-elisp"
                  "xwl-elisp/dashboard"
                  "config/autoload"

                  ;; submodules with lisp subdir.
                  "auto-complete"
                  "auto-complete/lib/popup"
                  "auto-complete/lib/fuzzy"                  
                  "emms/lisp"
                  ))

        (split-string
         (shell-command-to-string
          "cd ~/.emacs.d && git submodule --quiet foreach pwd | grep site-lisp"
          )))))
  
(mapc (lambda (path) (add-to-list 'load-path path))
      (append
       ;; essential
       (list xwl-emacs-top
             (concat xwl-emacs-top "/config")
             ;; FIXME: byte-compile never stops?
             (concat xwl-emacs-top "/xwl/wubi"))
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

;; load this earlier to use bzr version
;; (ignore-errors
;;   (unless (featurep 'cedet)
;;     (unless (eq system-type 'windows-nt)
;;       (load "~/repo/bzr/cedet/common/cedet.el")
;;       ;; (global-ede-mode 1)
;;       (semantic-load-enable-gaudy-code-helpers))))

;; no need of `provide'.

;;; xwl-path.el ends here
