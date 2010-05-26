;;; xwl-vim.el --- Some nifty vim emulations

;; Copyright (C) 2009, 2010 William Xu

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

;;; *, #

;; http://www.emacswiki.org/emacs/SearchAtPoint#toc2

(defun his-isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp."
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (isearch-search-and-update))

;;;###autoload
(defun his-isearch-yank-symbol (&optional partialp)
  "Put symbol at current point into search string.

  If PARTIALP is non-nil, find all partial matches."
  (interactive "P")
  (let* ((sym (find-tag-default))
         ;; Use call of `re-search-forward' by `find-tag-default' to
         ;; retrieve the end point of the symbol.
         (sym-end (match-end 0))
         (sym-start (- sym-end (length sym))))
    (if (null sym)
        (message "No symbol at point")
      (goto-char sym-start)
      ;; For consistent behavior, restart Isearch from starting point
      ;; (or end point if using `isearch-backward') of symbol.
      (isearch-search)
      (if partialp
          (isearch-yank-string sym)
        (his-isearch-yank-regexp
         (concat "\\_<" (regexp-quote sym) "\\_>"))))))

;;;###autoload
(defun his-isearch-current-symbol (&optional partialp)
  "Incremental search forward with symbol under point.

  Prefixed with \\[universal-argument] will find all partial
  matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-forward-regexp nil 1)
    (his-isearch-yank-symbol partialp)))

;;;###autoload
(defun his-isearch-backward-current-symbol (&optional partialp)
  "Incremental search backward with symbol under point.

  Prefixed with \\[universal-argument] will find all partial
  matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-backward-regexp nil 1)
    (his-isearch-yank-symbol partialp)))

;;; f, F

;;;###autoload
(defun xwl-forward-char (n char)
  (interactive "p\ncForward to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char) char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

;;;###autoload
(defun xwl-backward-char (n char)
  (interactive "p\ncBackward to char: ")
  (search-backward (string char) nil nil n)
  (while (char-equal (read-char) char)
    (search-backward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))


(provide 'xwl-vim)

;;; xwl-vim.el ends here
