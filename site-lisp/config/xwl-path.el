;;; xwl-path.el --- path setup

;; Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014 William Xu

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
                  ,(file-truename "c:/Program Files/Git/bin")
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
                `(,@(remove-if
                     (lambda (f) (or (not (file-directory-p (concat xwl-emacs-top "/" f)))
                                     (member f '(".." "config" "emms"))))
                     (directory-files xwl-emacs-top))

                  ;; with lisp subdir
                  "xwl-elisp/dashboard"
                  "config/autoload"
                  "auto-complete/lib/popup"
                  "auto-complete/lib/fuzzy"
                  "emms/lisp"
                  ))

        (split-string
         (with-temp-buffer
           (unless (zerop (shell-command
                           "cd ~/.emacs.d && git submodule --quiet foreach pwd | grep site-lisp"
                           (current-buffer)))
             (erase-buffer)
             (insert-file-contents "~/.emacs.d/submodules"))
           (buffer-string))))))

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

;; no need of `provide'.

;;; xwl-path.el ends here
