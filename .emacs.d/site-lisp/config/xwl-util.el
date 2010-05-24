;;; xwl-util.el --- Utility functions

;; Copyright (C) 2007, 2008, 2009, 2010  William Xu

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

(require 'cl)
(require 'url-extra)

;;; Compat

(defun xwl-compat-select (list predicate)
  "Return `cdr' of matched element in LIST by applying PREDICATE on
`car' of elements.

This is useful for sharing .emacs on multiple platforms, where
each OS has different set of tools. "
  (let ((l list)
        (ret nil))
    (while l
      (if (funcall predicate (caar l))
          (progn
            (setq ret (cadar l))
            (setq l nil))
        (setq l (cdr l))))
    (if (stringp ret)
        ret
      (eval ret))))

(defun xwl-compat-select-by-executable (list)
  (xwl-compat-select list 'executable-find))

(defun xwl-compat-select-by-window-system (list)
  (xwl-compat-select list (lambda (w) (eq window-system w))))


;; crazycool 的一个函数，显示 ascii 表
(defun ascii-table-show ()
  "Print the ascii table"
  (interactive)
  (with-current-buffer (switch-to-buffer "*ASCII table*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((i   0)
          (tmp 0))
      (insert (propertize
               "                                [ASCII table]\n\n"
               'face font-lock-comment-face))
      (while (< i 32)
        (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
          (insert (concat
                   (propertize (format "%3d " tmp)
                               'face font-lock-function-name-face)
                   (propertize (format "[%2x]" tmp)
                               'face font-lock-constant-face)
                   "    "
                   (propertize (format "%3s" (single-key-description tmp))
                               'face font-lock-string-face)
                   (unless (= tmp (+ 96 i))
                     (propertize " | " 'face font-lock-variable-name-face)))))
        (newline)
        (setq i (+ i 1)))
      (beginning-of-buffer))
    (toggle-read-only 1)))


;;; paste in IRC

(setq xwl-paste-username "xwl")

(setq xwl-paste-ubuntu-cn-classes
      '("applescript" "actionscript-french" "ada" "apache" "asm" "asp" "autoit" "bash"
        "blitzbasic" "c" "c_mac" "caddcl" "cadlisp" "cfdg" "cpp" "csharp" "css" "d"
        "delphi" "diff" "div" "dos" "eiffel" "fortran" "freebasic" "gml" "html4strict"
        "inno" "java" "java5" "javascript" "lisp" "lua" "matlab" "mpasm" "mysql" "nsis"
        "objc" "ocaml" "ocaml-brief" "oobas" "oracle8" "pascal" "perl" "php" "php-brief"
        "python" "qbasic" "robots" "ruby" "sas" "scheme" "sdlbasic" "smarty" "sql"
        "tsql" "vb" "vbnet" "vhdl" "visualfoxpro" "xml"))

(defun xwl-paste-ubuntu-cn (beg end &optional class)
  "Paste region between BEG and END to http://paste.ubuntu.org.cn.

Resulted post url will be appended to your kill ring, so you can
simply yank it when needed."
  (interactive "r")
  (unless class
    (if current-prefix-arg
        (setq class (ido-completing-read "Use mode: " xwl-wgetpaste-ubuntu-cn-classes))
      (setq class (xwl-paste-match-mode))))
  (let ((url "http://paste.ubuntu.org.cn"))
    (with-current-buffer
        (url-extra-http-post url
                       `((poster . ,xwl-paste-username)
                         (class . ,class)
                         (paste . "1")
                         (code2 . ,(buffer-substring-no-properties beg end))
                         ;; (screenshot . "c:/Users/My Pictures/orgmode.PNG")
                         ;; (x . "---xwl\r\nContent-Disposition: form-data;name=screenshot;filename=\"c:/Users/My Pictures/orgmode.PNG\"\r\n---xwl")
                         ))
      (setq deactivate-mark t)
      (goto-char (point-min))
      (if (re-search-forward
           (concat "<li class=\"highlight\"><a href=\"\/\\([0-9]+\\)\">" xwl-paste-username)
           nil t 1)
          (let ((s (concat url "/" (match-string 1))))
            (kill-new s)
            (message s)
            (kill-buffer (current-buffer)))
        (message "paste failed")
        (switch-to-buffer (current-buffer))))))

(defun xwl-paste-ubuntu-cn-image (filename)
  (interactive "fUpload image: ")
  (let ((cmd (format "curl -F screenshot=\"@%s\" -F paste=1 http://paste.ubuntu.org.cn"
                     filename)))
    (when (and (boundp 'xwl-proxy-server) xwl-proxy-server)
      (setq cmd (format "%s -x %s:%d" cmd xwl-proxy-server xwl-proxy-port)))
    (shell-command cmd)))

(setq xwl-paste-match-table
      '(((emacs-lisp-mode lisp-interaction-mode) . "lisp")
        ((c++-mode) . "cpp")))

(defun xwl-paste-match-mode ()
  "Find a suituable mode on the paste host based on current `major-mode'."
  ;; FIXME: No plain text?? I will just use lisp then.
  (let ((mode "lisp"))
    (cond
     ((member (replace-regexp-in-string "-mode" "" (symbol-name major-mode))
              xwl-paste-ubuntu-cn-classes)
      (replace-regexp-in-string "-mode" "" (symbol-name major-mode)))
     (t
      (let ((tbl xwl-paste-match-table)
            match)
        (while tbl
          (setq match (car tbl)
                tbl (cdr tbl))
          (when (member major-mode (car match))
            (setq mode (cdr match))
            (setq tbl nil)))
        mode)))))

;;; notify

(setq xwl-notify-emacs-image (file-truename "~/w32/emacs_4_48x48x32.png"))

(defun xwl-notify (title message)
  (case system-type
    ((darwin)
     (xwl-growl title message))
    ((windows-nt)
     (xwl-snarl title message))
    ((gnu/linux)
     (xwwl-zenity title message))))

(defun xwl-growl (title message)
  (xwl-shell-command-asynchronously
   (format "growlnotify --image %s -t \"%s\" -m \"%s\""
           xwl-notify-emacs-image title message)))

;; http://www.fullphat.net/index.php
;; http://tlhan-ghun.de/?q=node/59
(defun xwl-snarl (title message)
  (require 'xwl-emms)
  (xwl-shell-command-asynchronously
   (format "Snarl_CMD.exe snShowMessage 5 \"%s\" \"%s\" \"%s\""
           (emms-i18n-iconv 'utf-8 'gb18030 title)
           (emms-i18n-iconv 'utf-8 'gb18030 message)
           xwl-notify-emacs-image)))

(defun xwl-zenity (title message)
  (xwl-shell-command-asynchronously
   (format "zenity --info --text \"%s\"" message)))

;;; Misc

(defun xwl-strip-blank-lines-buffer ()
  "Strip all blank lines in current buffer."
  (interactive)
  (xwl-strip-blank-lines-region (point-min) (point-max)))

(defun xwl-strip-blank-lines-region (start end)
  "Strip all blank lines in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*\n" nil t)
        (replace-match "" t t))
      (widen))))

(defun xwl-shell-command-asynchronously (cmd)
  (start-process-shell-command cmd nil cmd))

;; insert line number before each line.
(defun xwl-numerate-lines ()
  "Insert line numbers into buffer"
  (interactive)
  (save-excursion
    (let ((max (count-lines (point-min) (point-max)))
	  (line 1))
      (goto-char (point-min))
      (while (<= line max)
	(insert (format "%4d " line))
	(beginning-of-line 2)
	(setq line (+ line 1))))))

;; a simple way of aligning columns
(defun his-align-cols (start end max-cols)
  "Align text between point and mark as columns.  Columns are separated by
whitespace characters.  Prefix arg means align that many columns. (default
is all)"
  (interactive "r\nP")
  (save-excursion
    (let ((p start)
	  pos
	  end-of-line
	  word
	  count
	  (max-cols (if (numberp max-cols) (max 0 (1- max-cols)) nil))
	  (pos-list nil)
	  (ref-list nil))

      ;; find the positions
      (goto-char start)
      (while (< p end)
	(beginning-of-line)
	(setq count 0)
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(re-search-forward "^\\s-*" end-of-line t)
	(setq pos (current-column))	;start of first word
	(if (null (car ref-list))
	    (setq pos-list (list pos))
	  (setq pos-list (list (max pos (car ref-list))))
	  (setq ref-list (cdr ref-list)))
	(while (and (if max-cols (< count max-cols) t)
		    (re-search-forward "\\s-+" end-of-line t))
	  (setq count (1+ count))
	  (setq word (- (current-column) pos))
	  ;; length of next word including following whitespaces
	  (setq pos (current-column))
	  (if (null (car ref-list))
	      (setq pos-list (cons word pos-list))
	    (setq pos-list (cons (max word (car ref-list)) pos-list))
	    (setq ref-list (cdr ref-list))))
	(while ref-list
	  (setq pos-list (cons (car ref-list) pos-list))
	  (setq ref-list (cdr ref-list)))
	(setq ref-list (nreverse pos-list))
	(forward-line)
	(setq p (point)))

      ;; align the cols starting with last row
      (setq pos-list (copy-sequence ref-list))
      (setq start
	    (save-excursion (goto-char start) (beginning-of-line) (point)))
      (goto-char end)
      (beginning-of-line)
      (while (>= p start)
	(beginning-of-line)
	(setq count 0)
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(re-search-forward "^\\s-*" end-of-line t)
	(goto-char (match-end 0))
	(setq pos (nth count pos-list))
	(while (< (current-column) pos)
	  (insert-char ?\040 1))
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(while (and (if max-cols (< count max-cols) t)
		    (re-search-forward "\\s-+" end-of-line t))
	  (setq count (1+ count))
	  (setq pos   (+  pos (nth count pos-list)))
	  (goto-char (match-end 0))
	  (while (< (current-column) pos)
	    (insert-char ?\040 1))
	  (setq end-of-line (save-excursion (end-of-line) (point))))
	(forward-line -1)
	(if (= p (point-min)) (setq p (1- p))
	  (setq p (point)))))))

;; count Chinese, English words
(defun xwl-count-ce-word (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
	(en-word 0)
	(total-word 0)
	(total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
	  total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (cn: %d, en: %d) words, %d bytes."
		     total-word cn-word en-word total-byte))))

;; xwl-word-count-analysis (how many times a word has appeared).
(defun xwl-word-count-analysis (start end)
  "Count how many times each word is used in the region.
    Punctuation is ignored."
  (interactive "r")
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
	(let* ((word (intern (match-string 0)))
	       (cell (assq word words)))
	  (if cell
	      (setcdr cell (1+ (cdr cell)))
	    (setq words (cons (cons word 1) words))))))
    (when (interactive-p)
      (message "%S" words))
    words))

(defun xwl-hide-buffer ()
  "Hide current buffer, and enlarge the other one if exists."
  (interactive)
  (xwl-highlight-changes-for-some-buffer)
  (delete-windows-on (buffer-name)))

(defun xwl-info (file)
  (interactive
   (list (read-file-name "info: ")))
  (info file))

;; dos <--> unix
(defun his-dos2unix ()
  "\r\n --> \r."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

(defun his-unix2dos ()
  "\n --> \r\n."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))

(defun xwl-delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete thru newline.
With prefix argument, delete that many lines from point.
Negative arguments delete lines backward.
With zero argument, deletes the text before point on the current line.

Note its difference between `xwl-delete-line' and `kill-line' is
that, the deleted contents won't be inserted to the `kill-ring'."
  (if arg
      (dotimes (i arg)
        (delete-region (point) (save-excursion (forward-line)
                                               (point))))
    (if (eolp)
        (delete-region (point) (save-excursion (forward-line)
                                               (point)))
      (delete-region (point) (save-excursion (end-of-line)
                                             (point))))))

(defun xwl-os-type ()
  "Return envrionment $OSTYPE."
  (interactive)
  (message (car (split-string (shell-command-to-string "echo $OSTYPE")))))

(defun xwl-fortune-laozi ()
  "Return a random chapter from laozi."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/etc/fortune/laozi")
    (let (beg title)
      (goto-char (point-min))
      (search-forward "老子第" nil t (random 81)) ; 81 chapters in total
      (setq title (buffer-substring-no-properties
                   (move-beginning-of-line 1)
                   (progn (move-end-of-line 1)
                          (point))))
      (setq beg (move-beginning-of-line 2))
      (search-forward "老子第" nil t 1)
      (format "%s
            ---- %s"
              (buffer-substring-no-properties
               beg (progn (move-end-of-line 0)
                          (point)))
              title))))

(setq xwl-fortune-favorites-length nil)

(defun xwl-fortune-favorites-vertically (&optional file)
  (interactive)
  (let ((ret (xwl-fortune-favorites file)))
    (setq ret
          (replace-regexp-in-string
           (regexp-opt '("，" "。" "！" "《" "》" "、" "？" "；"))
           "  "
           ret))
    (setq ret
          (shell-command-to-string
           (format
            "echo \"%s\" | iconv -f utf-8 -t gbk | pv | iconv -f gbk -t utf-8"
            ret)))))

(defun xwl-fortune-favorites (&optional file)
  "Return a random chapter from ~/notes/favorites."
  (interactive)
  (unless file
    (setq file "~/notes/favorites_en"))
  (with-temp-buffer
    (let ((flag "^\\%$")
          (ret ""))
      (insert-file-contents file)
      (goto-char (point-min))
      (re-search-forward flag nil t (random (count-matches flag)))
      (setq ret (buffer-substring-no-properties
                 (move-beginning-of-line 2)
                 (if (re-search-forward flag nil t 1)
                     (progn (move-end-of-line 0)
                            (point))
                   (point-max))))
      ret)))

(defun xwl-download-book (pre beg end post fmt subdir)
  "Download link formed of `PRE + index + POST', where `index' belongs to [BEG, END).
`index' is formated by FMT using `format'. BEG and END are integers. The
downloaded contents will be saved under \"~/Downloads/SUBDIR\".
e.g.,

  (xwl-get-book \"http://book.sina.com.cn/longbook/1071818529_qingcheng\"
                13
                24
                \".shtml\"
                \"%02d\"
                \"qczl\")"
  (let ((dst (concat "~/Downloads/" subdir)))
    (message "Start downloading at background...")
    (condition-case nil
        (make-directory dst)
      (error nil))
    (dotimes (i (- end beg))
      (xwl-shell-command-asynchronously
       (format "wget -c %s -P %s"
               (concat pre (format fmt (+ i beg)) post)
               dst)))))

(defun xwl-soft-kill-ring-save (beg end)
  "Same as `kill-ring-save' except it will convert hard newlines to soft newlines.
This could be useful for copying texts from Emacs and pasting it to blog
websites.  It also keeps original empty line for separating paragraphs."
  (interactive "r")
  (let ((content (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert content)
      (insert "\n")
      (goto-char (point-min))
      (move-beginning-of-line 2)
      (while (not (eobp))
        (if (looking-at "\n")
            (move-beginning-of-line 3)
          (if (looking-at "[[:ascii:]]")
              (progn (backward-delete-char-untabify 1)
                     (insert " "))
            (backward-delete-char-untabify 1))
          (move-beginning-of-line 2)))
      (copy-region-as-kill (point-min) (point-max)))))

;; Should be re-defun later.
(defun xds (any) "abcdefg")
(defun xes (any) "abcdefg")

(defun xwl-switch-or-create (buffer fun)
  "Switch to BUFFER when it exists, else create it with FUN."
  (let ((b (get-buffer buffer)))
    (if b
        (switch-to-buffer b)
      (call-interactively fun))))

(defun xwl-revert-buffer-with-sudo ()
  "Revert buffer using tramp sudo.
This will also reserve changes already made by a non-root user."
  (interactive)
  (let ((f (buffer-file-name)))
    (when f
      (let ((content (when (buffer-modified-p)
                       (widen)
                       (buffer-string))))
        (if (file-writable-p f)
            (revert-buffer)
          (kill-buffer (current-buffer))
          (if (file-remote-p f)
              (find-file
               (replace-regexp-in-string "^\\/[^:]+:" "/sudo:" f))
            (find-file (concat "/sudo::" f)))
          (when content
            (let ((buffer-read-only nil))
              (erase-buffer)
              (insert content))))))))

(global-set-key (kbd "C-c m R") 'xwl-revert-buffer-with-sudo)

(defun his-one-whitespace-between-ce (&optional start end)
  "Automatically insert a whitespace between Chinese and English,
Chinese and digits, which is useful when editing TeX files."
  (interactive)
  (save-excursion
    (unless start (setq start (point-min)))
    (unless end (setq end (point-max)))
    (goto-char start)
    (while (re-search-forward "\\(\\cc\\)\\([0-9a-zA-Z]\\)" end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char start)
    (while (re-search-forward "\\([0-9a-zA-Z]\\)\\(\\cc\\)" end t)
      (replace-match "\\1 \\2" nil nil))))

(global-set-key (kbd "C-c m o") 'his-one-whitespace-between-ce)

(defun his-transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(global-set-key (kbd "C-c m t") 'his-transpose-windows)

(defun xwl-check-holidays ()
  (calendar)
  (with-current-buffer "*Calendar*"
    (or (ignore-errors (diary-view-entries))
        (when (check-calendar-holidays (calendar-current-date))
          (calendar-cursor-holidays)))))

(defun xwl-running-daily ()
  "Staffs to run daily."
  (xwl-check-holidays)
  (plan))


;; ,----
;; | Date
;; `----

(defun xwl-insert-date ()
  (interactive)
  (insert (xwl-get-date)))

(defun xwl-get-date ()
  (format-time-string "%Y/%m/%d" (current-time)))
  ;; (format-time-string "%Y/%m/%d" (current-time)))

(defun xwl-update-date ()
  "Auto update '[Ll]ast [Uu]pdated:' part if exists when saving.
This should not affect `buffer-undo-list'."
  (interactive)
  (let ((old-list buffer-undo-list))
    (save-excursion
      (beginning-of-buffer)
      (when (search-forward-regexp "Last\\ updated:" nil t)
        (xwl-delete-line)
        (insert " ")
        (xwl-insert-date)))
    (setq buffer-undo-list old-list))
  nil)

(global-set-key (kbd "C-c m d") 'xwl-insert-date)

(add-hook 'before-save-hook 'xwl-update-date)

;; named-let, Thanks to Riastradh@#emacs
(defmacro his-named-let (name parameters &rest body)
  `(labels
       ((,name ,(mapcar 'car parameters) ,@body))
     (,name ,@(mapcar 'cadr parameters))))

(defun xwl-term (&optional create)
  (interactive)
  (cond
   (create
    (term "/bin/bash")
    (rename-buffer "*terminal*" t))
   ((not (get-buffer "xwl-term"))
    (term "/bin/bash")
    (rename-buffer "xwl-term" t))
   (t (switch-to-buffer "xwl-term"))))

(defun xwl-tty-p ()
  (string= (frame-parameter nil 'font) "tty"))

(defun xwl-generate-password (len)
  "[33, 126] is range for printable characters."
  (let ((min 33)
        (max 126)
        (ret ""))
    (dotimes (l len)
      (setq ret (concat ret (format "%c" (+ min (random (- max min)))))))
    ret))

(defun xwl-redirect-host ()
  (if xwl-w32-redirect-locally?
      "localhost"
    "172.28.206.207"))

;; Well, as Gnus and other components depend on this, we have to eval this much earlier.

(when (and xwl-at-company? xwl-w32?)
  (setq xwl-proxy-server "172.16.42.137"
        xwl-proxy-port 8080)

  (setq url-proxy-services
        `(("http" . ,(format "%s:%d" xwl-proxy-server xwl-proxy-port))))

  (setq xwl-w3m-arguments
        (list "-o" (format "http_proxy=http://%s:%d"
                           xwl-proxy-server
                           xwl-proxy-port))))


(setq xwl-timers-hook-started? nil)

(defun xwl-timers-hook ()
  "Timers to invoke on the fly.
Run it at an appropriate time, like when we twittering?"
  )

(defmacro xwl-shell-command-asynchronously-with-callback (cmd callback)
  "Run CMD asynchronously and apply CALLBACK in the output buffer.
Note: you are suggested to kill process buffer at the end of CALLBACK. "
  `(let* ((buf (generate-new-buffer (concat "*" (replace-regexp-in-string " .*" "" ,cmd) "*")))
          (p (start-process-shell-command ,cmd buf ,cmd)))
     (set-process-sentinel p
                           (lambda (process event)
                             (with-current-buffer (process-buffer process)
                               (when (eq (process-status  process) 'exit)
                                 (let ((inhibit-read-only t)
                                       (err (process-exit-status process)))
                                   (if (zerop err)
                                       (funcall ,callback)
                                     (error "`%s' failed: %d" ,cmd err)))))))))

(defun forward-ascii-symbol (arg)
  "New thing for `thingapt'."
  (interactive "p")
  (let ((re "\\([a-zA-Z0-9]\\(\\sw\\|\\s_\\)+[a-zA-Z0-9]\\)" ))
    (if (natnump arg)
        (re-search-forward re nil 'move arg)
      (while (< arg 0)
        (if (re-search-backward re nil 'move)
            (skip-syntax-backward "w_"))
        (setq arg (1+ arg))))))

(provide 'xwl-util)

;;; xwl-util.el ends here
