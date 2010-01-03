(unless (file-exists-p "q:")
  (shell-command "c:/xwl/scripts/map_sdk.bat"))

(setq xwl-home "Q:/repo/git/xwl-emacs-config")

(setenv "HOME" xwl-home)

(load (concat xwl-home "/.emacs.d/init.el"))

