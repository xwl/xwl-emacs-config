;;; xwl-private.el

;; These will go to `mail.genenal'.
(defvar xwl-mailbox-lists '())

(defvar pw163 nil)
(defvar pwsohu nil)
(defvar pwsina nil)
(defvar pwyahoo nil)
(defvar pwhotmail nil)
(defvar pwbbs nil)
(defvar pwtsh nil)
(defvar pwsun nil)
(defvar pwsql nil)
(defvar pwgmail nil)
(defvar pwerc nil)
(defvar pwdeb nil)
(defvar pwce nil)
(defvar pwlastfm nil)
(defvar pwbitlbee nil)
(defvar pwtwitter nil)

(require 'server)

(when (eq window-system 'w32)
  (defun server-ensure-safe-dir (dir)
    t)
  )

(ignore-errors
  (unless (server-running-p)            ; FIXME: hack for gnus agent script.
    (unless (eq window-system 'x)
      (load "xwl-private-setup.el.gpg"))))

(provide 'xwl-private)

;;; xwl-private.el ends here
