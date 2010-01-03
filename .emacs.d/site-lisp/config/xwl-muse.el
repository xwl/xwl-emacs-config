;;; xwl-muse.el --- muse config

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

;;; Muse - publishing html pages

(require 'muse)
(require 'muse-mode)
(require 'muse-colors)
(require 'muse-project)

(require 'muse-html)
(require 'muse-texinfo)
(require 'muse-latex)

(setq muse-project-alist
      '(("default"
	 ("~/studio/muse/default" :default "index")
	 (:base "html" :path "/home/web/net9")
;;	 (:base "html" :path "/williamxu@ftp.net9.org:/")
;; 	 (:base "texi" :path "~/info")
;; 	 (:base "info" :path "~/info"))
	 )
	("doc"
	 ("~/studio/muse/doc" :default "index")
	 (:base "html" :path "/home/web/doc")
;; 	 (:base "texi" :path "~/info")
;; 	 (:base "info" :path "~/info"))))
	 )))

(global-set-key (kbd "C-c m 9")
                (lambda () (interactive)
                  (find-file "/ftp:williamxu@ftp.net9.org:/")))

(setq muse-mode-auto-p t)

(setq muse-html-header "/home/william/studio/muse/style/header.html"
      muse-html-footer "/home/william/studio/muse/style/footer.html")

(defun xwl-muse-before-publish-hook ()
  (replace-regexp "" "")
  (replace-regexp "^<!-- .* -->\n" "")
  )

(add-hook 'muse-before-publish-hook 'xwl-muse-before-publish-hook)

(defun xwl-muse-resolve-path (base)
  "Return full path dir by looking at muse-current-project."
  (let ((path nil))
    (dolist (ls muse-current-project path)
      (if (and (listp ls) (member base ls))
	  (setq path (file-name-as-directory
		     (caddr (member base ls))))))))

(defun xwl-muse-preview (base &optional force)
  (interactive)
  (let* ((path (expand-file-name (xwl-muse-resolve-path base)))
	 (file (concat path (smart-compile-replace "%n.") base)))
;;     (when (or (file-newer-than-file-p buffer-file-name file)
;; 	      force)
      (muse-publish-this-file base path t);;)
    (cond ((equal base "html")
	   ;; (browse-url file)
	   (message "done"))
	  ((equal base "texi") 		; stupid here..<FIX ME>
	   (shell-command (concat "makeinfo " file))
	   (info (concat path (smart-compile-replace "%n.info")))
	   (shell-command (concat "makeinfo " file))
	   (Info-directory)
	   (Info-last)
	   (Info-last)))))

(defun xwl-muse-preview-html (&optional force)
  (interactive)
  (xwl-muse-preview "html" force))

(defun xwl-muse-preview-texi ()
  (interactive)
  (xwl-muse-preview "texi"))

(defun xwl-muse-mode-hook ()
  (local-unset-key (kbd "TAB"))
  (local-unset-key (kbd "*"))
  (define-key muse-mode-map (kbd "C-c DEL") 'xwl-muse-preview-html)
  (define-key muse-mode-map (kbd "C-c TAB") 'xwl-muse-preview-texi)

  (outline-minor-mode 1))

(add-hook 'muse-mode-hook 'xwl-muse-mode-hook)

(setq xwl-muse-tag-value
      '(("xwl-quotation"
	 "<div class=\"xwl-quotation\"><example>"
	 "</example></div>")
	("xwl-time"
	 "<div class=\"xwl-time\">"
	 "</div>")
	("xwl-code"
	 "<div class=\"xwl-code\"><example>"
	 "</example></div>")
	("xwl-note"
	 "<div class=\"xwl-note\"><example>"
	 "</example></div>")))

(defun xwl-muse-insert-tag (tag)
  "Support inserting user defined tags in a more flexiable style, by
looking at `xwl-muse-tag-value'. e.g.,

        (xwl-muse-insert-tag xwl-quotation)

Will result in,

<div class = \"xwl-quotation\"><pre>

</pre></div>"
  (interactive
   (list
    (ido-completing-read
     "Tag: "
     (mapcar 'car `(,@muse-publish-markup-tags
		    ,@xwl-muse-tag-value)))))
  (let ((tag-value (assoc tag xwl-muse-tag-value)))
    (if (not tag-value)
	(muse-insert-tag tag)
      (let ((begin (cadr tag-value))
	    (end (caddr tag-value)))
	(insert (concat begin "\n" "\n" end "\n"))
	(forward-line -2)))))

(define-key muse-mode-map (kbd "C-c <tab>") 'xwl-muse-insert-tag)

;;;; rss feed generation
;; ---------------------

(defun xwl-muse-rss-update (html-file)
  "e.g., blog.html -> blog.xml."
  (let ((buf (find-file-noselect html-file)))
    (with-current-buffer buf
      (let ((links '())
            (titles '())
            (descriptions '())
            (bound (point-max)))
        (goto-char (point-min))
        (while (re-search-forward "\"\\(blog.html#.*\\)\"" bound t 1)
          (setq links (append links (list (match-string-no-properties 1))))
          (re-search-forward ">\\(.*\\)<\\/a>" bound t 1)
          (setq titles (append titles (list (match-string-no-properties 1)))))
        ;; descriptions
        (while (search-forward "</h2>" bound t 1)
          (let ((start (point)))
            (if (search-forward "<h2>" bound t 1)
                (search-backward "<p>")
              ;; last entry
              (search-forward "</body>" bound t 1)
              (search-backward "<pre>"))
            (setq descriptions
                  (append descriptions
                          (list
                           (buffer-substring-no-properties
                            start (point)))))))
        (xwl-muse-rss-save (replace-regexp-in-string
                            "\\.htm[l]?$" ".xml" html-file)
                           titles
                           links
                           descriptions)))
    (kill-buffer buf)))

(defun xwl-muse-rss-save (rss-file titles links descriptions)
  (let ((buf (find-file-noselect rss-file)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-string "<?xml version=\"1.0\" ?>
<rss version=\"2.0\">
  <channel>
    <title>William Xu - 像风一样自由</title>
    <description>Have a nice dream.</description>
    <link>http://williamxu.net9.org</link>
")
        (mapcar*
         (lambda (title link description)
           (insert-string
            (format "
<item>
  <title>%s</title>
  <link>http://williamxu.net9.org/%s</link>
  <description>%s</description>
</item>"
                    title
                    link
                    (replace-regexp-in-string
                     ">" "&gt;" (replace-regexp-in-string
                                 "<" "&lt;" description)))))
         titles links descriptions)
        (insert-string "
</channel>
</rss>")
        (save-buffer)))
    (kill-buffer buf)))

(defun xwl-muse-update-blog-rss ()
  (interactive)
  (let ((filename "/home/web/net9/blog.html"))
    (xwl-muse-rss-update filename)
    (message "%s -> %s done"
             filename
             (replace-regexp-in-string "\\.html$" ".xml" filename))))

(define-key muse-mode-map (kbd "C-c C-r") 'xwl-muse-update-blog-rss)

(provide 'xwl-muse)

;;; xwl-muse.el ends here
