;; init.el --- journey of emacs starts from here...
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-section-hook (quote (LaTeX-section-heading LaTeX-section-section)))
 '(Man-width 78)
 '(bbdb-update-records-mode (quote (quote searching)))
 '(bookmark-save-flag 1)
 '(canlock-password "bbb62aab53d41c421d28ea701c910a5c34ea84be")
 '(dired-dwim-target t)
 '(dired-isearch-filenames t)
 '(enable-recursive-minibuffers t)
 '(erc-track-showcount t)
 '(file-template-insert-automatically t)
 '(file-template-paths (quote ("~/insert/" "/usr/share/emacs/insert/" "~/.emacs.d/templates/")))
 '(file-template-search-current-dir nil)
 '(glasses-original-separator "_")
 '(glasses-separator "-")
 '(glasses-uncapitalize-regexp "[a-z]")
 '(global-hi-lock-mode t)
 '(gnus-check-new-newsgroups (quote ask-server))
 '(gnus-read-active-file (quote some))
 '(gnus-read-newsrc-file nil)
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-use-dribble-file nil)
 '(hi-lock-file-patterns-policy (quote ask))
 '(ido-auto-merge-delay-time 1000)
 '(message-syntax-checks (quote ((sender . disabled) (signature . disabled))))
 '(ns-command-modifier (quote meta))
 '(safe-local-variable-values (quote ((line-spacing . 5) (flyspell-mode . t) (longlines-mode . t))))
 '(thinks-from (quote bottom-diagonal)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diary ((((min-colors 88) (class color) (background light)) (:background "yellow"))))
 '(emms-playlist-selected-face ((t (:background "blue4" :foreground "Yellow"))))
 '(emms-playlist-track-face ((t nil)))
 '(erc-input-face ((t (:foreground "magenta3"))))
 '(erc-keyword-face ((t (:foreground "green" :weight bold))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "magenta"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "green4"))))
 '(gnus-signature ((t (:slant italic))))
 '(gnus-summary-high-read ((t (:foreground "magenta"))))
 '(gnus-summary-high-unread ((t (:foreground "magenta"))))
 '(highlight-changes ((((min-colors 88) (class color)) (:background "grey19"))))
 '(holiday ((((class color) (background light)) (:background "RoyalBlue"))))
 '(mmm-default-submode-face ((t (:background "gray18"))))
 '(org-agenda-done ((((class color) (min-colors 16) (background dark)) (:foreground "blue2" :overline t))))
 '(org-done ((t (:foreground "ForestGreen" :strike-through "red"))))
 '(org-scheduled-today ((nil (:foreground "Magenta"))))
 '(org-todo ((t (:foreground "orange1" :weight bold)))))


;; Keep this after custom-set-faces !
(load "~/.emacs.d/site-lisp/config/xwl-main.el")

;;; init.el ends here
