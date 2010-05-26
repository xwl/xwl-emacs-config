;;; xwl-paste.el --- Paste to irc channels from emacs

;; Copyright (C) 2010  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: convenience

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

(setq xwl-paste-username "xwl")

(setq xwl-paste-ubuntu-cn-classes
      '("applescript" "actionscript-french" "ada" "apache" "asm" "asp" "autoit" "bash"
        "blitzbasic" "c" "c_mac" "caddcl" "cadlisp" "cfdg" "cpp" "csharp" "css" "d"
        "delphi" "diff" "div" "dos" "eiffel" "fortran" "freebasic" "gml" "html4strict"
        "inno" "java" "java5" "javascript" "lisp" "lua" "matlab" "mpasm" "mysql" "nsis"
        "objc" "ocaml" "ocaml-brief" "oobas" "oracle8" "pascal" "perl" "php" "php-brief"
        "python" "qbasic" "robots" "ruby" "sas" "scheme" "sdlbasic" "smarty" "sql"
        "tsql" "vb" "vbnet" "vhdl" "visualfoxpro" "xml"))

;;;###autoload
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

;;;###autoload
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

(provide 'xwl-paste)
;;; xwl-paste.el ends here
