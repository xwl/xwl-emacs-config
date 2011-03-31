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
(unless (server-running-p)                ; FIXME: hack for gnus agent script.
  (load "xwl-private-setup.el.gpg"))

(provide 'xwl-private)

;;; xwl-private.el ends here
