;;; xwl-convenience.el --- must have cookies

;; Copyright (C) 2007, 2008, 2009 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; GPL

;; ,----
;; | less
;; `----

(global-set-key (kbd "C-c v") 'less-minor-mode)



(mapc (lambda (hook) (add-hook hook 'less-minor-mode-on))
      '(help-mode-hook
        wajig-mode-hook
        wordnet-search-mode-hook
        dictionary-mode-hook
        custom-mode-hook
        ;; apropos-mode-hook ; ??
        emms-playlist-mode-hook
        woman-post-format-hook
        Man-mode-hook
        bbdb-mode-hook
        after-revert-hook
        Info-mode-hook ))

(add-hook 'archive-extract-hooks 'less-minor-mode-on)
(add-hook 'find-file-hook 'auto-less-minor-mode)
(add-hook 'after-revert-hook 'auto-less-minor-mode)

(setq auto-less-exclude-regexp (regexp-opt '("todo.org")))

;; ,----
;; | coding system
;; `----

(case system-type
  ((windows-nt)
   (prefer-coding-system 'gbk))
  (t
   (prefer-coding-system 'utf-8-emacs)))

;; (require 'auto-enca)
(setq file-coding-system-alist
      '(("\\.dz\\'" no-conversion . no-conversion)
	("\\.g?z\\(~\\|\\.~[0-9]+~\\)?\\'" no-conversion . no-conversion)
	("\\.tgz\\'" no-conversion . no-conversion)
	("\\.tbz\\'" no-conversion . no-conversion)
	("\\.bz2\\'" no-conversion . no-conversion)
	("\\.Z\\(~\\|\\.~[0-9]+~\\)?\\'" no-conversion . no-conversion)
	("\\.elc\\'" emacs-mule . emacs-mule)
	("\\.utf\\(-8\\)?\\'" . utf-8)
	("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
	("\\.tar\\'" no-conversion . no-conversion)
	("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
	("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system)

        ;; ("" undecided)
	("" utf-8 . utf-8)
        ;; ("" . enca-detect-coding)
        ))

(defun xwl-revert-buffer-with-coding-system ()
  "Revert buffer with 'gbk coding."
  (interactive)
  (revert-buffer-with-coding-system 'gb18030))

(global-set-key (kbd "C-c n r") 'xwl-revert-buffer-with-coding-system)

;; (require 'auto-enca)
;; (modify-coding-system-alist 'file "" 'enca-detect-coding)

;; ,----
;; | auto mode
;; `----

(setq auto-mode-alist
      `(("\\.h$"               . c++-mode)
        ("\\.lrc$"             . emms-lyrics-mode)
        ("\\.sh$"              . shell-script-mode)
        ("\\.m$"               . objc-mode)
         ;; octave-mode)
        ("\\.java$"            . java-mode)
        ("\\.l$"               . c-mode)
        ("\\.jl$"              . sawfish-mode)
        ("\\.hs$"              . haskell-mode)
        ("fonts.conf"          . xml-mode)
        ("\\(rc\\|.conf\\)$"   . conf-mode)
        ("\\(.mac\\|.lst\\)$"  . asm-mode)
        ("\\(.html\\|.htm\\)$" . html-mode)
        ("[sS][cC]onstruct"    . python-mode)
        
        ,@auto-mode-alist))

(add-to-list 'auto-mode-alist '("Makefile.*" . makefile-mode))

(mapc (lambda (el)
        (add-to-list 'auto-mode-alist el))
      '(("^/etc/conf.d" . conf-mode)
        ("\\.cnf$" . conf-mode)
        ("\\.ebuild$" . shell-script-mode)
        ))

(add-hook 'auto-mode-alist '("\\.d$" . shell-script-mode))

(setq auto-mode-alist
      `(("CMakeLists\\.txt\\'" . cmake-mode)
        ("\\.cmake\\'" . cmake-mode)
        ,@auto-mode-alist))

(defadvice save-buffers-kill-emacs (before remove-less activate)
  (remove-hook 'find-file-hook 'less-minor-mode-on))

;; ,----
;; | cookies -- one or two lines for each
;; `----

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default auto-fill-function 'do-auto-fill)
;; (setq default-justification 'full)
(setq default-fill-column 80 ; 72
      adaptive-fill-regexp
      "[	]*\\([-|#;>*]+[	]*\\|(?[0-9]+[.)][	]*\\)*"
      adaptive-fill-first-line-regexp "\\`[	]*\\'"
      fill-column 80); 72)

;;(unless window-system
;;  (set-face-background 'highlight "red")
;;  (set-face-background 'show-paren-match-face "yellow"))

(setq column-number-mode t
      line-number-mode t)

(show-paren-mode 1)
(setq show-paren-style 'expression)

(setq scroll-step 1
      ;; FIXME: This will cause eshell jumping when at the bottom of the buffer.
      ;; scroll-margin 3
      scroll-conservatively most-positive-fixnum
      hscroll-step 1
      hscroll-margin 3)

(setq scroll-preserve-screen-position 'always)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq default-directory "~/")

(setq kill-ring-max 100
      inhibit-startup-message t
      max-lisp-eval-depth 20000
      c-echo-syntactic-information-p nil)

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)
(put 'overwrite-mode 'disabled t)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq require-final-newline 't)
(setq-default truncate-lines t
	      truncate-partial-width-windows t)

(setq-default indent-tabs-mode nil)

(setq history-delete-duplicates t)

(setq default-major-mode 'text-mode)

(setq split-width-threshold 150)

;; (global-visual-line-mode 1)

;; (setq display-time-format "<%V-%u> %m/%d/%H:%M")
(setq display-time-format "%a(%V) %m/%d/%H:%M")

(display-time)

(fset 'yes-or-no-p 'y-or-n-p)

(keyboard-translate ?\C-h ?\C-?)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(global-unset-key (kbd "C-x C-b"))

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq x-select-enable-clipboard t)

(defadvice forward-page (after forward-page-and-first-column activate)
  (move-beginning-of-line nil))

(when (fboundp 'tramp-cleanup-all-buffers)
  (add-hook 'kill-emacs-hook 'tramp-cleanup-all-buffers))

(setq mac-pass-command-to-system nil)

(mouse-avoidance-mode 'animate)

;; ,----
;; | expansions, abbreviations & completions
;; `----

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-whole-kill
;;	senator-try-expand-semantic
	try-expand-dabbrev-visible
	try-expand-dabbrev-from-kill
	try-expand-dabbrev-all-buffers
	try-expand-all-abbrevs
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-list
;	try-complete-lisp-symbol-partially
;;	try-complete-lisp-symbol
        try-expand-line
	try-expand-line-all-buffers))

(setq save-abbrevs t)

(condition-case nil
    (quietly-read-abbrev-file)
  (error nil))

(abbrev-mode 1)

;; disabled now

(when nil
  ;; msf-abbrev
  (require 'msf-abbrev)
  (setq msf-abbrev-verbose nil)
  (setq msf-abbrev-root "~/.emacs.d/mode-abbrevs")
  (msf-abbrev-load)
  )

;; ,----
;; | back up
;; `----

(setq version-control t
      kept-old-versions 2
      kept-new-versions 3
      delete-old-versions t
      backup-by-copying t
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t)

(setq backup-directory-alist
      '(("." . "~/var/emacs_backups" )))

;; same filename, different paths
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; weather
;; -------
(setq xwl-weather-string "")
(setq xwl-weather-list nil)
(setq xwl-weather-checked-day nil)

(defun xwl-weather-update (&optional force)
  (interactive)
  (if (or force
          (null xwl-weather-checked-day)
          (> (string-to-number (format-time-string "%d" (current-time)))
             xwl-weather-checked-day))
      (progn
        (message "updating weather info...")
        (setq xwl-weather-checked-day
              (string-to-number (format-time-string "%d" (current-time))))
        (setq xwl-weather-list
              (remove
               ""
               (split-string
                (shell-command-to-string
                 "/home/william/repo/darcs/guile/scripts/weather-man.scm 北京")
                "\n")))
        (setq xwl-weather-string
              (replace-regexp-in-string
               "风力："
               ""
               (concat "["
                       (mapconcat (lambda (el) el)
                                  (remove
                                   ""
                                   (cdr (split-string (car xwl-weather-list) ";")))
                                  "")
                       "/"
                       (mapconcat (lambda (el) el)
                                  (remove
                                   ""
                                   (cdr (split-string (cadr xwl-weather-list) ";")))
                                  "")
                       "]")))
        (message "done"))
    (message "weather already up-to-date")))

;; (run-with-idle-timer (* 10 60) (* 24 60 60) 'xwl-weather-update)

(defun xwl-weather-show ()
  (interactive)
  (let ((str ""))
    (mapc (lambda (day) (setq str (concat str day "\n")))
          xwl-weather-list)
    (message str)))

(global-set-key (kbd "C-c m w") 'webjump)

(eval-after-load 'webjump
  '(progn
     (mapc (lambda (el)
             (add-to-list 'webjump-sites el))
           '(;; ("gentoo software search" . "http://www.rommel.stw.uni-erlangen.de/~fejf/pfs/")
             ("douban"                 . "http://www.douban.com")
             ("gmail"                  . "http://www.gmail.com")
             ;; ("bloglines"              . "http://www.bloglines.com/feeds")
             ("gitweb"                 . "http://git.savannah.gnu.org/gitweb/")
             ("remember the milk"      . "http://www.rememberthemilk.com")
             ))
     ))

;; FIXME: conflict with ido-hacks.el
(partial-completion-mode 1)

;;; Extra libs

(require 'smart-operator)

(defun xwl-text-mode-hook ()
  (auto-compression-mode 1)
  (abbrev-mode 1)
  ;; (flyspell-mode 1)

  (smart-operator-mode 1)
  (local-unset-key (kbd "."))
  ;; (local-set-key (kbd "M-S") 'wordnet-search)
  (local-set-key (kbd "M-s") 'dictionary-search)

  (local-unset-key (kbd "M-S")))
;; (local-set-key (kbd "TAB") 'ispell-complete-word))

(add-hook 'text-mode-hook 'xwl-text-mode-hook)

(when (fboundp 'browse-kill-ring)
  (require 'browse-kill-ring)
  (browse-kill-ring-default-keybindings)
  (global-set-key (kbd "M-y") 'browse-kill-ring))

;; ;; debian pkg manager
(autoload 'wajig "wajig" "Create a *wajig* buffer." t nil)

(setq wajig-frequent-commands
      '("ps -ef"
        "ps -u william u"
        "lsof -i -nP"))

(add-hook 'wagjig-mode-hook
          (lambda ()
            (define-key wajig-mode-map (kbd "RET")
              (lambda () (interactive)
                (if (ffap-file-at-point)
                    (ffap (ffap-file-at-point))
                  (wajig-show-at-point))))))

(setq wajig-command-prefix '("ssh"
                             ;; "192.168.234.130"
                             "ananas"
                             ))

;; gentoo

;; (autoload 'gentoo "gentoo" "Create a *gentoo* buffer." t nil)

;; (add-hook 'gentoo-mode-hook 'less-minor-mode-on)

(autoload 'fink "fink" "Create a *fink* buffer." t nil)

(add-hook 'fink-mode-hook 'less-minor-mode-on)

;;; Sessions

;; session
;; * In the minibuffer, enter <M-?> to display a completion list with
(setq session-initialize t)
(setq session-globals-exclude
      '(load-history register-alist vc-comment-ring
		     flyspell-auto-correct-ring
		     planner-browser-file-display-rule-ring)
      session-globals-regexp "-\\(ring\\|history\\)\\'")

;; save cursor's place
(require 'saveplace)
(setq-default save-place t)

;; recentf
(recentf-mode 1)

(defun xwl-recentf-open-files ()
  (interactive)
  (let* ((alist (mapcar '(lambda (el)
                           (cons (file-name-nondirectory el) el))
                        recentf-list))
         (filename (ido-completing-read "Open recent file: "
                                        (mapcar 'car alist))))
    (find-file (cdr (assoc filename alist)))))

(global-set-key (kbd "C-c F") 'xwl-recentf-open-files)

;; (add-hook 'kill-emacs-hook 'recentf-cleanup)

(add-to-list 'recentf-keep 'file-remote-p)

(add-hook 'minibuffer-setup-hook 'turn-off-auto-fill)


;;; Bindings - keyboard bindings

;; ,----
;; | window scrolling
;; `----

(global-set-key (kbd "M-p") 'less-scroll-down-line)
(global-set-key (kbd "M-n") 'less-scroll-up-line)

(global-set-key (kbd "<wheel-up>") 'less-scroll-down-line)
(global-set-key (kbd "<wheel-down>") 'less-scroll-up-line)

;; am i weird or Emacs is weird ?
(global-set-key (kbd "C-x <left>") 'next-buffer)
(global-set-key (kbd "C-x <right>") 'previous-buffer)

(global-set-key (kbd "C-<left>")  'previous-error)
(global-set-key (kbd "C-<right>") 'next-error)

;; conflicts

(dolist (hook '(dired-mode-hook
                calendar-move-hook
                gnus-summary-mode-hook
                gnus-group-mode-hook
                clone-buffer-hook))
  (add-hook hook
            (lambda ()
		   (if (not (one-window-p))
		       (local-set-key (kbd "<end>")
                                      'less-scroll-other-window-up-one-line))
		   (local-set-key (kbd "M-n") 'less-scroll-up-line)
		   (local-set-key (kbd "M-p") 'less-scroll-down-line))))

(add-hook 'w3m-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-n") 'less-scroll-up-line)
	    (local-set-key (kbd "M-p") 'less-scroll-down-line)
	    (local-set-key (kbd "<left>") 'w3m-previous-buffer)
	    (local-set-key (kbd "<right>") 'w3m-next-buffer)
	    (local-set-key (kbd "p") 'w3m-previous-buffer)
	    (local-set-key (kbd "n") 'w3m-next-buffer)
	    (local-set-key (kbd "c") 'w3m-delete-buffer)))

(add-hook 'makefile-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "M-n") 'less-scroll-up-line)
	     (local-set-key (kbd "M-p") 'less-scroll-down-line)))

(global-set-key (kbd "C-c n t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c m D") 'toggle-debug-on-error)

(defun xwl-kill-emacs-hook ()
  (when (fboundp 'gnus-group-exit)
    (gnus-group-exit)))

(add-hook 'kill-emacs-hook 'xwl-kill-emacs-hook)

;; ,----
;; | bookmark
;; `----

(eval-after-load 'bookmark
  '(progn
     (defadvice bookmark-default-handler (around just-switch-to-already-opened-file activate)
       (let ((done nil)
             (f (bookmark-get-filename bmk-record)))
         (when (not (file-directory-p f))
           (let ((b (get-buffer (file-name-nondirectory f))))
             (when (and b (string= (buffer-file-name b) f))
               (switch-to-buffer b)
               (setq done t))))
         (unless done
           ad-do-it)))
     ))

;; ,----
;; | imenu
;; `----

(eval-after-load 'imenu
  '(progn
     (defadvice imenu-default-create-index-function (around add-more-default-index activate)
       (let ((imenu-generic-expression 
              (cons (list "Outlines"
                          (format "\\(%s\\|%s\\)\\(%s\\)"                          
                                  outline-regexp
                                  (concat "^" comment-start "+ | ")
                                  ".*")
                          2)
                    imenu-generic-expression)))
         ad-do-it))
     ))

;; (setq inhibit-eol-conversion t)


(provide 'xwl-convenience)

;;; xwl-convenience.el ends here
