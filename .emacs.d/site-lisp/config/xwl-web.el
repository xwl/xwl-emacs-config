;;; xwl-web.el --- w3m config

;; Copyright (C) 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

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

(require 'xwl-util)
(ignore-errors (require 'w3m))

;;; w3m

(when (featurep 'w3m)
  (setq w3m-default-display-inline-images t
        w3m-default-save-directory "~/download/"
        w3m-home-page "http://localhost/"
        w3m-init-file "~/.emacs.d/.emacs-w3m"
        ;; w3m-command-arguments
        ;;       (append w3m-command-arguments
        ;;               ;; '("-o" "http_proxy=http://222.43.34.94:3128/"))
        ;;               '("-o" "http_proxy="))
        ;;       w3m-no-proxy-domains '(".edu.cn,166.111.,162.105.,net9.org"))
        )

  (setq w3m-process-modeline-format " loaded: %s")

  (defun xwl-w3m-mode-hook ()
    (define-key w3m-mode-map (kbd "t") 'w3m-view-this-url-new-session)
    (define-key w3m-mode-map (kbd "b") 'w3m-view-previous-page)
    (define-key w3m-mode-map (kbd "f") 'w3m-view-next-page)
    (define-key w3m-mode-map (kbd "B") 'w3m-view-previous-page)
    (define-key w3m-mode-map (kbd "F") 'w3m-view-next-page)
    (define-key w3m-mode-map (kbd "o") 'w3m-goto-url)
    (define-key w3m-mode-map (kbd "O") 'w3m-goto-url-new-session))

  (add-hook 'w3m-mode-hook 'xwl-w3m-mode-hook)
  (add-hook 'w3m-mode-hook 'less-minor-mode-on)

  (setq 未临-看图-命令
        (xwl-compat-select-by-executable
         '(("zgv" "zgv")
           ("open" "open -a Preview"))))

  (setq w3m-content-type-alist
        `(("text/plain" "\\.\\(txt\\|tex\\|el\\)\\'" nil nil)
          ("text/html" "\\.s?html?\\'" browse-url-default-browser nil)

          ("image/jpeg"  "\\.jpe?g\\'" (,未临-看图-命令 file) nil)
          ("image/png"   "\\.png\\'"   (,未临-看图-命令 file) nil)
          ("image/gif"   "\\.gif\\'"   (,未临-看图-命令 file) nil)
          ("image/tiff"  "\\.tif?f\\'" (,未临-看图-命令 file) nil)
          ("image/x-xwd" "\\.xwd\\'"   (,未临-看图-命令 file) nil)
          ("image/x-xbm" "\\.xbm\\'"   (,未临-看图-命令 file) nil)
          ("image/x-xpm" "\\.xpm\\'"   (,未临-看图-命令 file) nil)
          ("image/x-bmp" "\\.bmp\\'"   (,未临-看图-命令 file) nil)

          ("video/mpeg" "\\.mpe?g\\'" nil nil)
          ("video/quicktime" "\\.mov\\'" nil nil)

          ("application/postscript" "\\.e?ps\\'" ("gv" file) nil)
          ("application/pdf" "\\.pdf\\'" ("xpdf" file) nil)
          ("application/xhtml+xml" nil nil "text/html")))

  )


;;; wget

(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
;; (load "w3m-wget")

(setq wget-download-directory "~/Downloads")


;;; External Browser

(defun xwl-browse-url-firefox-tab-only (url &optional new-window)
  "NEW-WINDOW is always nil."
  (case system-type
    ((darwin)
     (do-applescript
      ;; Flock, Firefox
      (format
       "tell application \"Finder\"
               open Location \"%s\"
end tell"
       url)))
    ((windows-nt)
     (w32-shell-execute "open" (concat"\"" url "\"")))
    (t
     (xwl-shell-command-asynchronously
      (concat "firefox -new-tab \"" url "\"")))))

(defun xwl-browse-url-camino (url &optional new-window)
  (do-applescript
   (format
    "tell application \"Camino\"
	open url \"%s\"
end tell"
    url)))

(defun xwl-browse-url-safari (url &optional new-window)
  (do-applescript
   (format
    "tell application \"Safari\"
	open location \"%s\"
end tell"
    url)))

(setq browse-url-browser-function
      (if window-system
;;;           (if (eq window-system 'mac)
;;;               'xwl-browse-url-camino
            'xwl-browse-url-firefox-tab-only
            ;; 'xwl-browse-url-safari
;;;         )
        'w3m-browse-url))

(global-set-key (kbd "C-c n b") 'browse-url)

;;; MIME

(if (featurep 'w3m)
    (setq mm-text-html-renderer 'w3m)
  (setq mm-text-html-renderer 'html2text))

(setq mm-inline-text-html-with-images t
      mm-w3m-safe-url-regexp nil)

(provide 'xwl-web)

;;; xwl-w3m.el ends here
