;;; xwl-extra-util.el --- non-essential, autoloaded utilities

;; Copyright (C) 2010  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;###autoload
(defun ascii-table-show ()
  "Print the ascii table, by crazycool@smth. "
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
      (goto-char (point-min)))
    (toggle-read-only 1)))

;;;###autoload
(defun xwl-delete-blank-lines-buffer ()
  "Strip all blank lines in current buffer."
  (interactive)
  (xwl-strip-blank-lines-region (point-min) (point-max)))

;;;###autoload
(defun xwl-delete-blank-lines-region (start end)
  "Strip all blank lines in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (flush-lines "^$"))))

;;;###autoload
(defun xwl-numerate-lines ()
  "Insert line number before each line."
  (interactive)
  (save-excursion
    (let ((max (count-lines (point-min) (point-max)))
	  (line 1))
      (goto-char (point-min))
      (while (<= line max)
	(insert (format "%4d " line))
	(beginning-of-line 2)
	(setq line (+ line 1))))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun xwl-generate-password (len)
  "[33, 126] is range for printable characters."
  (let ((min 33)
        (max 126)
        (ret ""))
    (dotimes (l len)
      (setq ret (concat ret (format "%c" (+ min (random (- max min)))))))
    ret))

;;;###autoload
(defmacro xwl-shell-command-asynchronously-with-callback (cmd callback)
  "Run CMD asynchronously and apply CALLBACK in the output buffer.
Note: you are suggested to kill process buffer at the end of CALLBACK. "
  `(let* ((buf (generate-new-buffer (concat "*" (replace-regexp-in-string " .*" "" ,cmd) "*")))
          (p (start-process-shell-command ,cmd buf ,cmd)))
     (set-process-sentinel
      p
      (lambda (process event)
        (with-current-buffer (process-buffer process)
          (when (eq (process-status  process) 'exit)
            (let ((inhibit-read-only t)
                  (err (process-exit-status process)))
              (if (zerop err)
                  (funcall ,callback)
                (error "`%s' failed: %d" ,cmd err)))))))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun his-dos2unix ()
  "\r\n --> \r."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

;;;###autoload
(defun his-unix2dos ()
  "\n --> \r\n."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))

;;;###autoload
(defun xwl-info (file)
  (interactive
   (list (read-file-name "info: ")))
  (info file))


(provide 'xwl-extra-util)
;;; xwl-extra-util.el ends here
