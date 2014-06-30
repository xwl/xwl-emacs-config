;;; xwl-dired.el --- Dired config

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(require 'dired)
(require 'ansi-color)
(require 'dired-aux)
(require 'dired-x)
(require 'wdired)
(require 'xwl-util)
(ignore-errors (require 'emms-player-mplayer))

;;; General

(setq dired-recursive-copies 'always)

(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

(define-key dired-mode-map (kbd "* f") 'find-name-dired)
(define-key dired-mode-map (kbd "* g") 'grep-find)

;; open current directory in Finder, Explorer, etc.
(define-key dired-mode-map (kbd "f")
  '(lambda ()
     (interactive)
     (let ((d (shell-quote-argument (dired-current-directory))))
       (case window-system
         ((w32)
          (w32-shell-execute "open" d))
         ((ns mac)
          (xwl-shell-command-asynchronously (format "open -a Finder %s" d)))
         ((x)
          (xwl-shell-command-asynchronously (concat "nautilus --browser " d)))))))

;; open current directory in a console/terminal
(define-key dired-mode-map (kbd "c")
  '(lambda ()
     (interactive)
     (let ((d (dired-current-directory)))
       (case window-system
         ((w32)
          (xwl-shell-command-asynchronously "start cmd.exe"))
         ((ns mac)
          (do-applescript (format "
tell application \"Terminal\"
  activate
  do script \"cd '%s'; bash\"
end tell" d)))
         ((x)
          (xwl-shell-command-asynchronously "gnome-terminal"))))))

(define-key dired-mode-map (kbd "b")
  (lambda ()
    (interactive)
    (let* ((orig (dired-filename-at-point))
           (new (concat orig ".orig")))
      (when (file-exists-p new)
        (error "File `%s' already exists" new))
      (copy-file orig new)
      (chmod orig (file-modes-symbolic-to-number
                   "u+w" (file-modes orig)))
      (revert-buffer))))

(defun xwl-dired-mode-hook ()
  (dired-omit-mode 1)

  (local-unset-key (kbd "<up>"))
  (local-unset-key (kbd "<down>"))
  (local-unset-key (kbd "<left>"))
  (local-unset-key (kbd "<right>"))

  )

(add-hook 'dired-mode-hook 'xwl-dired-mode-hook)
;; (remove-hook 'dired-after-readin-hook 'xwl-dired-mode-hook)

(defun xwl-dired-wvHtml ()
  (concat "wvHtml --charset=gb2312 * "
	  (xwl-dired-get-filename-no-extention) ".html"))

(define-key dired-mode-map (kbd "v") 'xwl-dired-w3m-find-file)

;; Ask for confirm when opening some binary alike(.avi, .dvi, etc) files
;; by accident.
;; (defadvice dired-find-file (around ask-confirm-open-binary-file activate)
;;   ;; (save-window-excursion
;;   (let ((f (dired-get-filename t)))
;;     (if (or (string-match
;;              (concat "\\.\\("
;;                      (regexp-opt '("dvi" "pdf" "avi" "mp3" "sub"))
;;                      "\\)$")
;;              f)
;;             ;; ELF bin file
;;             (string-match "ELF" (dired-show-file-type f)))
;;         (when (y-or-n-p (format "Really open `%s'? " f))
;;           ad-do-it)
;;       ad-do-it)))

(define-key dired-mode-map (kbd "w") (lambda ()
                                       (interactive)
                                       (dired-copy-filename-as-kill 0)))

(defun xwl-dired-w3m-find-file ()
  (interactive)
  (let ((file (file-name-nondirectory (dired-get-filename))))
    (if (y-or-n-p (format "Use emacs-w3m to browse %s? " file))
	(w3m-find-file file))))

(defun xwl-dired-get-filename-no-extention ()
  "In dired, return the filename without extentions. eg.
\"abc.txt\"  --> \"abc\". Right now the filename should only
be with length 3 extentions !"
  (interactive)
  (let ((filename (file-name-nondirectory (dired-get-filename))))
    (message (substring filename 0 (- (length filename) 4)))))

(define-key dired-mode-map (kbd "M-<") (lambda ()
                                         (interactive)
                                         (beginning-of-buffer)
                                         (dired-next-line 2)))

(define-key dired-mode-map (kbd "M->") (lambda ()
                                         (interactive)
                                         (end-of-buffer)
                                         (dired-previous-line 1)))


(setq dired-omit-size-limit nil)

(define-key dired-mode-map (kbd "o") 'dired-omit-mode)

;; (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))


;;; Sort

(setq dired-listing-switches "-lh")

;; Sort directories first
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point)
                          (point-max))))
  (set-buffer-modified-p nil))

;; FIXME, for vc-directory
;; (remove-hook 'dired-after-readin-hook 'sof/dired-sort)

;; Sort methods that affect future sessions
(defun xwl-dired-sort-by-default ()
  (interactive)
  (setq dired-listing-switches "-lh")
  (dired-sort-other dired-listing-switches))

(defun xwl-dired-sort-by-show-all ()
  (interactive)
  (setq dired-listing-switches "-lhA")
  (dired-sort-other dired-listing-switches))

(defun xwl-dired-sort-by-for-solaris ()
  "Solaris `ls' doesn't support `-h' option, stupid!"
  (interactive)
  (setq dired-listing-switches "-lA")
  (dired-sort-other dired-listing-switches))

;; Sort methods that affect current session only
(defun xwl-dired-sort-by-date ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "t"))) ; r

;; TODO: fix this for mac. like: ls | rev | sort | rev
(defun xwl-dired-sort-by-extenstion ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "X")))

(defun xwl-dired-sort-by-invisible-only ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "d .*")))

(defun xwl-dired-sort-by-size ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "S")))

(define-key dired-mode-map (kbd "s") nil)
(define-key dired-mode-map (kbd "s RET") 'xwl-dired-sort-by-default)
(define-key dired-mode-map (kbd "s a") 'xwl-dired-sort-by-show-all)
(define-key dired-mode-map (kbd "s t") 'xwl-dired-sort-by-date)
(define-key dired-mode-map (kbd "s X") 'xwl-dired-sort-by-extenstion)
(define-key dired-mode-map (kbd "s s") 'xwl-dired-sort-by-for-solaris)
(define-key dired-mode-map (kbd "s .") 'xwl-dired-sort-by-invisible-only)
(define-key dired-mode-map (kbd "s z") 'xwl-dired-sort-by-size)


;;; Omit, Search

;; Omit
(setq dired-omit-files
      (concat "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^#.*#$\\|^nohup.out$\\|\\.jlc$"
              "\\|\\$NtUninstallKB.*\\|"
              (regexp-opt '("TAGS" "cscope.out" "distribution.policy.s60"
                            "Distribution.Policy.S60"))))

(setq dired-omit-extensions
      '("CVS/" ".o" "~" ".bin" ".lbin" ".fasl" ".ufsl" ".ln" ".blg"
	".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm"
	".class" ".fas" ".x86f" ".sparcf" ".lo" ".la"
	".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps"
	".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof"
	".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky"
	".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs" ".flc"
        ".hi" ".p_hi" ".p_o" ".hi-boot" ".o-boot" ".p_o-boot"
        ".p_hi-boot" ".hs-boot" ".obj" ".ncb" ".suo" ".user" ".idb"
        ".pdb" ".moc" ".manifest" ".ilk"))

(setq dired-omit-verbose nil)

;;; Actions

;; Run shell command at background
(defun dired-run-shell-command (command)
  (let ((handler
	 (find-file-name-handler
	  (directory-file-name default-directory)
	  'shell-command)))
    (if handler
	(apply handler 'shell-command (list command))
      ;; (shell-command command)))
      (xwl-shell-command-asynchronously command))) ; xwl
  ;; Return nil for sake of nconc in dired-bunch-files.
  nil)

;; 1. When there is only one match, just do it! Don't bother me to type
;;    an extra RET!
;; 2. Use ido style prompt when there are mutiple matches
(defun dired-guess-shell-command (prompt files)
  "Ask user with PROMPT for a shell command, guessing a default from FILES."
  (let ((default (dired-guess-default files))
        default-list val)
    (if (null default)
        ;; Nothing to guess
        (read-shell-command prompt nil 'dired-shell-command-history)
      (setq prompt (replace-regexp-in-string ": $" " " prompt))
      (if (listp default)
          ;; More than one guess
          (setq default-list default
                default (car default)
                prompt (concat
                        prompt
                        (format "{%d guesses} " (length default-list))))
        ;; Just one guess
        (setq default-list (list default)))
      ;; Put the first guess in the prompt but not in the initial value.
      (setq prompt (concat prompt (format "[%s]: " default)))
      (if (= (length default-list) 1)
          (progn
            (message "Running `%s' at background" default)
            (setq val default))
        ;; All guesses can be retrieved with M-n
        (setq val (read-shell-command prompt nil
                                      'dired-shell-command-history
                                      default-list))
        ;; If we got a return, then return default.
        (if (equal val "") default val)))))

;; ;; I don't like to use wildcards in shell command, so simply replace
;; ;; them with operated files.
;; (setq dired-star-subst-regexp "\\(^\\|[ 	]\\)\\*\\([ 	]\\|$\\)")
;; ;; "\\*"
;; (setq dired-quark-subst-regexp "\\(^\\|[ 	]\\)\\?\\([ 	]\\|$\\)")
;; ;; "\\?")

;; Note: when `cdr' part is a lisp expression, it understands `file'
;; argument as the filename.
(setq dired-guess-shell-alist-user
      (case system-type
        ((windows-nt)
         '(("\\.qml$" "c:/Qt/2010.05/qt/bin/qmlviewer.exe")
           (".*" file)))

        ((darwin)
         '((".*" "open")))

        (t
         `((,(if (fboundp 'emms-player-get)
                 (emms-player-get emms-player-mplayer 'regex)
               ".mp3")
            (progn (emms-add-file (concat (dired-current-directory) file))
                   file))

           (,(regexp-opt '(".gif" ".png" ".bmp" ".jpg" ".tif" ".jpeg"))
            ,(xwl-compat-select-by-executable
              '(("qiv" "qiv")
                ("feh" "feh")
                ;; ("xloadimage" "xloadimage -onroot")
                ("open" "open -a Preview")
                )))

           ("\\.htm[l]?$" "firefox")

           ("\\.dvi$"    "xdvi")
           ("\\.rar$"    "unrar x")

           ("\\.pdf$" ,(xwl-compat-select-by-executable
                        '(("xpdf" "xpdf")
                          ("gnome-open" "gnome-open")
                          ("open" "open -a Preview"))))

           ("\\.pdf.gz$" "zxpdf")
           ("\\.chm$" ,(xwl-compat-select-by-executable
                        '(("xchm" "xchm")
                          ("open" "open -a Chmox"))))
           ("\\.djvu$"   "djview")
           ("\\.jar$"    "unzip")
           ("\\.tar.bz2$" "tar jxf")

           ;; (".doc" (xwl-dired-wvHtml))
           (,(regexp-opt '(".doc" ".ppt" ".xls" ".doc"))
            ,(if (string-match "darwin" (xwl-os-type))
                 "open -a Openoffice"
               "soffice"))

           ;; default
           ,@dired-guess-shell-alist-default

           ;; match any files
           (".*" `(,(format "tar zcf %s.tar.gz"
                            (file-name-nondirectory file))
                   ,(format "zip -r %s.jar"
                            (file-name-nondirectory file))
                   "qiv"))))))


;;; Apply xterm color scheme to Dired

(let ((cmd "dircolors")
      (s "'no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:';"
         ))
  (when (and (executable-find cmd)
             (zerop (shell-command (concat "TERM=xterm-color " cmd))))
    (setq s (shell-command-to-string cmd)))

  (setq xwl-dircolors-string
        (replace-regexp-in-string ":$" "" (cadr (split-string s "'")))))

;; colored by file extensions
(setq xwl-dircolors-extensions
      (split-string
       (replace-regexp-in-string
        "=[0-9;]+\\|\\*\\." ""
        (replace-regexp-in-string "^[^*]*" "" xwl-dircolors-string))
       ":"))

(defun xwl-dircolors-get-escape-seq (regexp)
  "Get escape-seq by matching REGEXP against `xwl-dircolors-string'.
e.g., (xwl-dircolors-get-escape-seq \"*.gz\") => \"01;31\""
  (string-match (concat regexp "=\\([^:]+\\);\\([^:]+\\):") xwl-dircolors-string)
  (concat (match-string 2 xwl-dircolors-string) ";" (match-string 1 xwl-dircolors-string)))

(setq dired-font-lock-keywords
      `(
        ;;
        ;; Directory headers.
        ,(list dired-subdir-regexp '(1 dired-header-face))
        ;;
        ;; Dired marks.
        ,(list dired-re-mark '(0 dired-mark-face))
        ;;
        ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
        ;; file name itself.  We search for Dired defined regexps, and then use the
        ;; Dired defined function `dired-move-to-filename' before searching for the
        ;; simple regexp ".+".  It is that regexp which matches the file name.
        ;;
        ;; Marked files.
        ,(list (concat "^[" (char-to-string dired-marker-char) "]")
               '(".+" (dired-move-to-filename) nil (0 dired-marked-face)))
        ;;
        ;; Flagged files.
        ,(list (concat "^[" (char-to-string dired-del-marker) "]")
               '(".+" (dired-move-to-filename) nil (0 dired-flagged-face)))
        ;; People who are paranoid about security would consider this more
        ;; important than other things such as whether it is a directory.
        ;; But we don't want to encourage paranoia, so our default
        ;; should be what's most useful for non-paranoids. -- rms.
  ;; ;;
  ;; ;; Files that are group or world writable.
  ;; (list (concat dired-re-maybe-mark dired-re-inode-size
  ;;       	 "\\([-d]\\(....w....\\|.......w.\\)\\)")
  ;;        '(1 dired-warning-face)
  ;;        '(".+" (dired-move-to-filename) nil (0 dired-warning-face)))
        ;; However, we don't need to highlight the file name, only the
        ;; permissions, to win generally.  -- fx.
        ;; Fixme: we could also put text properties on the permission
        ;; fields with keymaps to frob the permissions, somewhat a la XEmacs.
        ,(list (concat dired-re-maybe-mark dired-re-inode-size
                       "[-d]....\\(w\\)....") ; group writable
               '(1 dired-warning-face))
        ,(list (concat dired-re-maybe-mark dired-re-inode-size
                       "[-d].......\\(w\\).") ; world writable
               '(1 dired-warning-face))
        ;;
        ;; Subdirectories.
        ,(list dired-re-dir
               '(".+" (dired-move-to-filename) nil (0 dired-directory-face)))
        ;;
        ;; Symbolic links.
        ,(list dired-re-sym
               '(".+" (dired-move-to-filename) nil (0 dired-symlink-face)))

        ;; executables
        ,(list dired-re-exe
               `(".+"
                 (dired-move-to-filename)
                 nil
                 (0 (ansi-color--find-face
                           (ansi-color-parse-sequence ,(xwl-dircolors-get-escape-seq "ex"))))))

        ;; colorful by extensions
        ,@(mapcar (lambda (ext)
                    `(,(format ".*\\.%s$" ext)
                      (".+"
                       (dired-move-to-filename)
                       nil
                       (0 (ansi-color--find-face
                           (ansi-color-parse-sequence ,(xwl-dircolors-get-escape-seq ext)))))))
                  xwl-dircolors-extensions)

        ;;
        ;; Files suffixed with `completion-ignored-extensions'.
        (eval .
              ;; It is quicker to first find just an extension, then go back to the
              ;; start of that file name.  So we do this complex MATCH-ANCHORED form.
              (list (concat "\\(" (regexp-opt completion-ignored-extensions) "\\|#\\)$")
                    '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))))
        ;;
        ;; Files suffixed with `completion-ignored-extensions'
        ;; plus a character put in by -F.
        (eval .
              (list (concat "\\(" (regexp-opt completion-ignored-extensions)
                            "\\|#\\)[*=|]$")
                    '(".+" (progn
                             (end-of-line)
                             ;; If the last character is not part of the filename,
                             ;; move back to the start of the filename
                             ;; so it can be fontified.
                             ;; Otherwise, leave point at the end of the line;
                             ;; that way, nothing is fontified.
                             (unless (get-text-property (1- (point)) 'mouse-face)
                               (dired-move-to-filename)))
                      nil (0 dired-ignored-face))))))


(when (eq system-type 'windows-nt)
  ;; FIXME: This could avoid following bug?
  ;;   dired-move-to-filename: No file on this line
  ;;
  ;; when copying files from A directory to B directory side by side in dired,
  ;; where B is initially empty.
  (setq ls-lisp-use-insert-directory-program t))

(provide 'xwl-dired)

;;; xwl-dired.el ends here
