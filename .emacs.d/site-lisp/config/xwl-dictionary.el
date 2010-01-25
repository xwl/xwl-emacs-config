;;; xwl-dictionary.el --- dictionary config

;; Copyright (C) 2007, 2008, 2009 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; ,----
;; | dictionary.el - dictd/dict or online dict.org
;; `----

;; search online
;; (when xwl-at-company?
;;   (setq dictionary-use-http-proxy t
;;         dictionary-proxy-port xwl-proxy-port
;;         dictionary-proxy-server xwl-proxy-server))

(when xwl-at-company?
  (setq dictionary-server (xwl-redirect-host))
  (setq dictionary-port 12628))

(setq dictionary-default-dictionary "*") ;"wn"

(setq dictionary-tooltip-dictionary "wn"
      global-dictionary-tooltip-mode nil
      dictionary-tooltip-mode nil)

(defun xwl-dictionary-next-dictionary ()
  (interactive)
  (end-of-line)
  (search-forward-regexp "^From" nil t)
  (beginning-of-line))

(defun xwl-dictionary-prev-dictionary ()
  (interactive)
  (beginning-of-line)
  (search-backward-regexp "^From" nil t)
  (beginning-of-line))

(defun xwl-dictionary-mode-hook ()
  ;; faces
  (set-face-foreground 'dictionary-word-entry-face "magenta")

  (define-key dictionary-mode-map (kbd "<backtab>") 'dictionary-prev-link)
  (define-key dictionary-mode-map (kbd "n") 'xwl-dictionary-next-dictionary)
  (define-key dictionary-mode-map (kbd "p") 'xwl-dictionary-prev-dictionary))

(add-hook 'dictionary-mode-hook 'xwl-dictionary-mode-hook)

;; ,----
;; | wordnet
;; `----

(require 'wordnet)

;; ,----
;; | goo.ne.jp
;; `----

(defun xwl-search-jp (beg end)
  "Search marked word(utf8 coding) on goo.ne.jp in external browser."
  (interactive "r")
  (if mark-active
      (progn
        (deactivate-mark)
        (xwl-search-jp-word-at-point (buffer-substring-no-properties beg end)))
    (xwl-search-jp-word-at-point)))

(defun xwl-search-jp-word-at-point (&optional word)
  (interactive)
  (setq word (read-string "Search JP word: " (or word (current-word))))
  (let ((url (concat
              "http://dictionary.goo.ne.jp/search.php?MT="
              (emms-url-quote-plus (encode-coding-string word 'euc-jp))
              "&search_history=%CA%D8%CD%F8&kind=all&kwassist=0&all.x=31&all.y=8&all=%BC%AD%BD%F1%A4%B9%A4%D9%A4%C6&mode=0"
              ))
        (buf (get-buffer-create "*JP Dictionary*")))
    ;; (browse-url url)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (message "Querying %s..." word)
        (erase-buffer)
        (insert (shell-command-to-string
                 (format "w3m %s -dump \"%s\""
                         (mapconcat 'identity xwl-w3m-arguments " ")
                         url)))

        ;; Clean up
        (goto-char (point-min))         ; front
        (unless (search-forward "辞書すべての検索結果" nil t 1)
          (error "Oops, `%s' not found" word))
        (delete-region (point-min) (+ (line-end-position) 2))
        (while (search-forward "検索結果を見る" nil t 1) ; lines
          (delete-region (line-beginning-position) (1+ (line-end-position))))
        (delete-region (line-beginning-position) (point-max)) ; rear

        (insert url "\n")
        (goto-char (point-min))
        (xwl-jp-dictionary-setup)))

    ;; switch to jp buffer
    (unless (eq (current-buffer) buf)
      (switch-to-buffer-other-window buf))
    (message "Querying...done")))

(defun xwl-jp-dictionary-setup ()
  ;; bindings
  (local-set-key "n" (lambda ()
                       (interactive)
                       (end-of-line)
                       (search-forward-regexp "検索結果$" nil t)
                       (beginning-of-line)))
  (local-set-key "p" (lambda ()
                       (interactive)
                       (beginning-of-line)
                       (search-backward-regexp "検索結果$" nil t)
                       (beginning-of-line)))
  (local-set-key "q" 'xwl-hide-buffer)
  (local-set-key "s" 'xwl-search-jp-word-at-point)
  ;; faces
  (highlight-regexp ".*検索結果$" 'dictionary-word-entry-face)
  ;; hooks
  (less-minor-mode-on))


(provide 'xwl-dictionary)

;;; xwl-dictionary.el ends here
