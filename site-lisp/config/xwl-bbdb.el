;;; xwl-bbdb.el --- bbdb config

;; Copyright (C) 2010, 2011 William Xu

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

(require 'bbdb)
(bbdb-initialize)

(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-user-mail-names
      (regexp-opt '("willam.xwl@gmail.com"
                    "willam.xwl@hotmail.com")))
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)

(setq bbdb-complete-name-allow-cycling t
      bbdb-use-pop-up nil
      bbdb-file-coding-system 'utf-8
      ;; bbdb-quiet-about-name-mismatches t
      )

(define-key bbdb-mode-map (kbd "TAB") 'bbdb-toggle-records-display-layout)

(defun xwl-bbdb ()
  (interactive)
  (if (get-buffer "*BBDB*")
      (switch-to-buffer "*BBDB*")
    (bbdb "" nil)
    (other-window 1)
    (delete-other-windows)))

(defun xwl-bbdb-search (field str)
  "Search records whose FIELD matches STR."
  (interactive
   (list
    (completing-read
     "Search: "
     (append (mapcar (lambda (el) (car el)) (bbdb-propnames))
             '("name")))
    (read-string "Match: ")))
  (if (string= field "name")
      (bbdb-name str nil)
    (let ((matches '())
          (case-fold-search bbdb-case-fold-search)
          (invert (bbdb-search-invert-p)))
      (when (stringp field)
        (setq field (intern field)))
      (mapc
       (lambda (record)
         (condition-case nil
             (let ((matchedp
                    (string-match
                     str
                     (cdr (assoc field (bbdb-record-raw-notes record))))))
               (when (or (and (not invert) matchedp)
                         (and invert (not matchedp)))
                 (setq matches (append matches (list record)))))
           (error nil)))
       (bbdb-records))
      (bbdb-display-records matches))))

(defun xwl-bbdb-create (name)
  "Add a new entry to the bbdb database.

This is different from `bbdb-create', where `xwl-bbdb-create' only
prompts for name field."
  (interactive "sName: ")
  (let ((record
         (vector name "" nil nil nil nil nil nil
                 (make-vector bbdb-cache-length nil))))
    (bbdb-invoke-hook 'bbdb-create-hook record)
    (bbdb-change-record record t)
    (bbdb-display-records (list record))))

(defun xwl-bbdb-display-all ()
  (interactive)
  (bbdb-display-records (bbdb-records)))

(define-key bbdb-mode-map (kbd "a") 'xwl-bbdb-display-all)
(define-key bbdb-mode-map (kbd "\/") 'xwl-bbdb-search)

(provide 'xwl-bbdb)

;;; xwl-bbdb.el ends here
