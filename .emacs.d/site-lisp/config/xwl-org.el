;;; xwl-org.el --- configs for org-mode

;; Copyright (C) 2008, 2009, 2010 William Xu

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
     (org-defkey org-agenda-mode-map "q" 'xwl-hide-buffer)

     (setq org-calendar-agenda-action-key [?K])
     (define-key calendar-mode-map [?k] 'calendar-backward-week)

     (defadvice org-agenda-day-view (around leave-ndays-alone activate)
       "Do not touch `org-agenda-ndays' please!"
       (interactive "P")
       (let ((old org-agenda-ndays))
         ad-do-it
         (setq org-agenda-ndays old)))

     (defadvice org-agenda-week-view (around leave-ndays-alone activate)
       "Do not touch `org-agenda-ndays' please!"
       (interactive "P")
       (let ((old org-agenda-ndays))
         ad-do-it
         (setq org-agenda-ndays old)))

     ))

(eval-after-load 'org
  '(progn
     (org-defkey org-mode-map "\C-c," 'next-buffer)
     (org-defkey org-mode-map "\C-c." 'previous-buffer)))

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

;; remember
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq remember-handler-functions '(org-remember-handler))

;; (global-set-key (kbd "C-c r") 'remember)

(setq org-remember-templates
      '((?t "* - %?\n  %u" "~/.notes/todo.org")
        ;; (?n "* %u %?" "~/..notes")
        ))

(when nil

(setq org-todo-keywords
      '(
        (sequence "-" ">" "o")
        (sequence "TODO" "DONE")
        (sequence "STARTED" "WAITING" "LATER" "CANCELLED")
        (sequence "x")
        ))

(defun xwl-org-set-tags (tag)
  "Insert TAG under point."
  (interactive
   (list (ido-completing-read "Set tag: " xwl-org-tag-alist)))
  (let ((inhibit-read-only t))
    (save-excursion
      (move-end-of-line 1)
      (insert (format "  :%s:" tag)))))

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-c C-g") 'xwl-org-set-tags)
     ))

(eval-after-load 'org-agenda
  '(progn

     (define-key org-agenda-mode-map (kbd "N") (lambda ()
                                                 (interactive)
                                                 (forward-char 1)
                                                 (re-search-forward "^[a-zA-Z]" nil t 1)
                                                 (backward-char 1)))

     (define-key org-agenda-mode-map (kbd "P") (lambda ()
                                                 (interactive)
                                                 (re-search-backward "^[a-zA-Z]" nil t 1)))

     (defadvice org-agenda-list (after maximize-window activate)
       (delete-other-windows))
     ))

;; remember
(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq remember-handler-functions '(org-remember-handler))

(global-set-key (kbd "C-c r") 'remember)

(setq org-remember-templates
      '((?t "* - %?\n  %u" "~/.notes/todo.org")
        ;; (?n "* %u %?" "~/..notes")
        ))

)

(provide 'xwl-org)

;;; xwl-org.el ends here
