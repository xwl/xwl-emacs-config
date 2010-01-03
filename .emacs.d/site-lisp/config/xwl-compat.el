;;; xwl-compat.el

;; Copyright (C) 2009 William Xu

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

(unless (featurep 'emms)
  (defalias 'emms-replace-regexp-in-string 'replace-regexp-in-string)

  (defun emms-url-quote (s &optional safe)
    "Replace special characters in S using the `%xx' escape.
This is useful for escaping parts of URLs, but not entire URLs.

Characters in [a-zA-Z_.-/] and SAFE(default is \"\") will never be
quoted.
e.g.,
    (emms-url-quote \"abc def\") => \"abc%20def\"."
    (if (not (stringp s))
        ""
      (or safe (setq safe ""))
      (save-match-data
        (let ((re (if (string-match "]" safe)
                      ;; `]' should be placed at the beginning inside []
                      (format "[]a-zA-Z_.-/%s]"
                              (emms-replace-regexp-in-string "]" "" safe))
                    (format "[a-zA-Z_.-/%s]" safe))))
          (mapconcat
           (lambda (c)
             (let ((s1 (char-to-string c)))
               (if (string-match re s1)
                   s1
                 (format "%%%02x" c))))
           (string-to-list (encode-coding-string s 'utf-8))
           "")))))

  (defun emms-url-quote-plus (s &optional safe)
    "Run (emms-url-quote s \" \"), then replace ` ' with `+'."
    (emms-replace-regexp-in-string
     " " "+" (emms-url-quote s (concat safe " "))))
  )


(provide 'xwl-compat)

;;; xwl-compat.el ends here
