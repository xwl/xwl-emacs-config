;;; xwl-org.el --- configs for org-mode

;; Copyright (C) 2008, 2009, 2010, 2011, 2012 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

; (require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$\\|todo\\.org_archive" . org-mode))

(setq org-agenda-files '("~/.notes/todo.org"))

(setq org-todo-keywords
      '(;; (sequence "-" ">" "o" "|" "x" "|" "w" "o")
        (sequence "TODO" "ONGOING" "WAITING" "DELEGATED" "|" "DONE" "CANCELLED")
        ))

(setq org-agenda-todo-ignore-scheduled t)

(setq xwl-org-tag-alist '("@nokia" "@hacking" "@life" "@reading" "@watching"
                          "@travel" "@buy" "@study" "@misc"))

;; (setq org-agenda-custom-commands
;;       '(("h" "My Agenda & TODO" ((agenda "")
;;                                  (alltodo "")
;;                                  ))))

(setq org-agenda-ndays 15)

(eval-after-load 'org-agenda
  '(progn
     (org-defkey org-agenda-mode-map "m" 'org-agenda-month-view)
     (org-defkey org-agenda-mode-map "q" 'winner-undo) ; xwl-hide-buffer)

     (setq org-calendar-agenda-action-key [?K])
     (define-key calendar-mode-map [?k] 'calendar-backward-week)

     ))

(eval-after-load 'org
  '(progn
     (org-defkey org-mode-map (kbd "C-,") 'next-buffer)
     (org-defkey org-mode-map (kbd "C-.") 'previous-buffer)

     (defun org-image-file-name-regexp (&optional extensions)
       "Return regexp matching the file names of images.
If EXTENSIONS is given, only match these."
       ;;  (if (and (not extensions) (fboundp 'image-file-name-regexp))
       ;;      (image-file-name-regexp)
       (let ((image-file-name-extensions
              (or extensions
                  '("png" "jpeg" "jpg" "gif" "tiff" "tif"
                    "xbm" "xpm" "pbm" "pgm" "ppm"))))
         (concat "\\."
                 (regexp-opt (nconc (mapcar 'upcase
                                            image-file-name-extensions)
                                    image-file-name-extensions)
                             t)
                 "\\'")))

     ))

(require 'org-agenda)
(defadvice org-agenda-day-view (around leave-ndays-alone activate)
  "Do not touch `org-agenda-ndays' please!"
  ;; (interactive "P")
  (let ((old org-agenda-ndays))
    ad-do-it
    (setq org-agenda-ndays old)))

(defadvice org-agenda-week-view (around leave-ndays-alone activate)
  "Do not touch `org-agenda-ndays' please!"
  ;; (interactive "P")
  (let ((old org-agenda-ndays))
    ad-do-it
    (setq org-agenda-ndays old)))

(require 'org-archive)
(defun xwl-org-archive-all-done-item ()
  "Archive all items with prefix in `org-done-keywords'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((kwd-re (regexp-opt org-done-keywords))
           (match (concat "^" outline-regexp " *" kwd-re))
           (matches 0))
      (if (search-forward-regexp match nil t 1)
          (progn
            (setq matches (1+ matches))
            ;; (org-advertized-archive-subtree)
            (org-archive-set-tag)
            (forward-line 1)
            (while (search-forward-regexp match nil t 1)
              (setq matches (1+ matches))
              ;; (org-advertized-archive-subtree)
              (org-archive-set-tag)
              (forward-line 1))
            (message "%d items archived" matches))
        (message "Nothing to archive")))))

(setq org-agenda-format-date "%m-%d %a")

(setq org-agenda-show-all-dates nil)

(add-hook 'org-mode-hook
          (lambda ()
            (modify-syntax-entry ?- "w" org-mode-syntax-table)
            (modify-syntax-entry ?> "w" org-mode-syntax-table)))

(setq org-startup-with-inline-images t)

;; remember
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq remember-handler-functions '(org-remember-handler))

;; (global-set-key (kbd "C-c r") 'remember)

(setq org-remember-templates
      '((?t "* - %?\n  %u" "~/.notes/todo.org")
        ;; (?n "* %u %?" "~/..notes")
        ))

;; latex export

(eval-after-load 'org-exp
  '(progn
     (defadvice org-export (around disable-less activate)
       ;; (let ((orig global-less-minor-mode))
       ;;   (global-less-minor-mode -1)
       ;;   ad-do-it
       ;;   (global-less-minor-mode 1))
       (let ((inhibit-read-only t))
         ad-do-it)

       )
     ))

(setq org-export-latex-packages-alist
      '(("" "fontspec" t)
        ("" "xltxtra" t)
        ("" "xunicode" t)
        ("" "indentfirst" t)
        "\\setmainfont[Mapping=tex-text]{Hiragino Sans GB}
\\XeTeXlinebreaklocale ``zh''
\\XeTeXlinebreakskip = 0pt plus 1pt"))
;; \\setlength{\\parindent}{2.1em}

;; Run two times to generate `Contents' table.
(setq org-latex-to-pdf-process '("xelatex %s" "xelatex %s"))

;;; html export

(setq org-export-html-table-tag "<table border=\"2\" cellpadding=\"6\"")

(setq org-export-html-style-include-default nil
      org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"org.css\">")

(setq org-startup-with-inline-images t)

;; (when nil

;; (setq org-todo-keywords
;;       '(
;;         (sequence "-" ">" "o")
;;         (sequence "TODO" "DONE")
;;         (sequence "STARTED" "WAITING" "LATER" "CANCELLED")
;;         (sequence "x")
;;         ))

;; (defun xwl-org-set-tags (tag)
;;   "Insert TAG under point."
;;   (interactive
;;    (list (completing-read "Set tag: " xwl-org-tag-alist)))
;;   (let ((inhibit-read-only t))
;;     (save-excursion
;;       (move-end-of-line 1)
;;       (insert (format "  :%s:" tag)))))

;; (eval-after-load 'org
;;   '(progn
;;      (define-key org-mode-map (kbd "C-c C-g") 'xwl-org-set-tags)
;;      ))

;; (eval-after-load 'org-agenda
;;   '(progn

;;      (define-key org-agenda-mode-map (kbd "N") (lambda ()
;;                                                  (interactive)
;;                                                  (forward-char 1)
;;                                                  (re-search-forward "^[a-zA-Z]" nil t 1)
;;                                                  (backward-char 1)))

;;      (define-key org-agenda-mode-map (kbd "P") (lambda ()
;;                                                  (interactive)
;;                                                  (re-search-backward "^[a-zA-Z]" nil t 1)))

;;      (defadvice org-agenda-list (after maximize-window activate)
;;        (delete-other-windows))
;;      ))

;; ;; remember
;; (require 'remember)
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)

;; (setq remember-handler-functions '(org-remember-handler))

;; (global-set-key (kbd "C-c r") 'remember)

;; (setq org-remember-templates
;;       '((?t "* - %?\n  %u" "~/.notes/todo.org")
;;         ;; (?n "* %u %?" "~/..notes")
;;         ))

;; )

;;; iCalendar export

;; org-export-icalendar-this-file

(defun xwl-calendar-convert-locale-from-chinese (str)
  "Convert chinese locale in STR to english."
  (when (string-match "\
\\([[:space:]]*\\([0-9]+\\)[[:space:]]*年\\)?\
\\([[:space:]]*\\([0-9]+\\)[[:space:]]*月\\)?\
\[[:space:]]*\\([0-9]+\\)[[:space:]]*日\
\\([[:space:]]*周\\([一二三四五六日]\\)\\)?\
\\([[:space:]]*\\([0-9][0-9]?:[0-9][0-9]?\\)[[:space:]]*\\([apAP][mM]\\)?\\)?"
                      str)
    (let* ((matches (match-data))
           (year (if (nth 4 matches)
                     (substring str (nth 4 matches) (nth 5 matches))
                   (format-time-string "%Y" (current-time))))
           (month (if (nth 8 matches)
                      (substring str (nth 8 matches) (nth 9 matches))
                    (format-time-string "%m" (current-time))))                 
           (day (if (nth 10 matches)
                    (substring str (nth 10 matches) (nth 11 matches))
                  (format-time-string "%d" (current-time))))
           week-day
           (time (when (nth 18 matches)
                   (substring str (nth 18 matches) (nth 19 matches)))))
          
      (when (nth 20 matches) 
        (setq time (concat time " " (substring str (nth 20 matches) (nth 21 matches)))))

      (setq month (format "%02d" (string-to-number month)))
      (setq day (format "%02d" (string-to-number day)))
      (setq week-day (calendar-day-name
                      (mapcar 'string-to-number (list year month day))))

      (replace-match
       (concat " <"
               (mapconcat 'identity (list year month day) "-")
               " " week-day
               (if time (concat " " time) "")
               "> ")
       nil nil str))))

(defun xwl-calendar-convert-locale-from-chinese-in-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((str (buffer-substring (line-beginning-position) (line-end-position))))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (xwl-calendar-convert-locale-from-chinese str))
        (forward-line 1)))))


(provide 'xwl-org)

;;; xwl-org.el ends here
