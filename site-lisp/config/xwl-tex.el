;;; xwl-tex.el --- TeX or LaTeX config

;; Copyright (C) 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; ,----
;; | auctex
;; `----

;; - 插入 section, chapter 等: C-c C-s
;; - 插入 envrionment 如 itemize 等: C-c C-e
;; - 插入 macro: C-c RET
;; - 插入各种字体, 或改变选区域字体: C-c C-f *
;; - 快速插入 \item: C-c C-j

(ignore-errors (load "auctex.el" nil t t))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(defun xwl-latex-mode-hook ()
  (smart-operator-mode 1)

  (setq ispell-parser 'tex)
  (flyspell-mode 1)

  ;; (set (make-local-variable 'outline-regexp) "\%\%\%+ ")
  ;; (outline-minor-mode -1)

  ;; (define-key LaTeX-mode-map (kbd "<C-up>") 'TeX-command-master)
  (define-key LaTeX-mode-map (kbd "<backtab>") 'read-completer-latex)
  (define-key LaTeX-mode-map (kbd "$") 'skeleton-pair-insert-maybe)
  (define-key LaTeX-mode-map (kbd "C-c C-j") (lambda ()
                                               (interactive)
                                               (insert "\\item\n\n")
                                               (indent-according-to-mode))))

(add-hook 'LaTeX-mode-hook 'xwl-latex-mode-hook)

;; (require 'latex)
;; (remove-hook 'LaTeX-mode-hook 'outline-minor-mode-on)

;; ,----
;; | preview latex
;; `----

;; Typical Error: 
;;   DviPS sentinel: Opening input file: no such file or directory

;; Solution: on Mac OS X, if you have installed MacTeX and fink, be
;; careful not messing with them!! You should always use commands(latex,
;; dvips, dvipng, etc) installed by MacTeX(under "/usr/texbin") ! Check
;; your `exec-path' and value of (getenv "PATH"), make sure
;; "/usr/texbin" is in front of "/sw/bin", you can run "$ which dvips"
;; to find out.
;;
;; However, I find that outputing to pdf with xelatex is a lot faster
;; than the preview process!!  So after all theses efforts for enabling
;; preview, I would rather use xelatex as usual.

;; (load "preview-latex.el" nil t t)

;; ,----
;; | metapost
;; `----

(defun xwl-metapost-preview (STR)
  (interactive "sPreview fig-")
  (let* ((file (file-name-sans-extension
                (file-name-nondirectory (buffer-file-name))))
         (file-pdf (concat file "-" STR ".pdf")))
    (shell-command (concat "xpdf " file-pdf))))

(defun xwl-metapost-pdftopng ()
  (interactive)
  (let* ((file (file-name-sans-extension
                (file-name-nondirectory (buffer-file-name))))
         (i 1)
         (file-pdf (concat file "-" (int-to-string i) ".pdf"))
         (file-png (concat file "-" (int-to-string i) ".png")))

    (while (file-exists-p file-pdf)
      (if (file-newer-than-file-p file-pdf file-png)
          (shell-command (concat "pdftoppm " file-pdf " tmp;\
                  convert tmp-000001.ppm " file-png ";\
                  rm -f tmp-000001.ppm")))
      (setq i (1+ i)
            file-pdf (concat file "-" (int-to-string i) ".pdf")
            file-png (concat file "-" (int-to-string i) ".png")))

    (message (concat file "-*.pdf have been converted to " file "-*.png."))))

(defun xwl-metapost-mode-hook ()
  (auto-revert-mode -1)
  (xwl-text-mode-hook)
  ;;   (local-set-key (kbd "<C-up>") 'xwl-metapost-mptopdf)
  ;; TODO: should be replaced by buffer-action.el.
  (local-set-key (kbd "<C-down>") 'xwl-metapost-preview)
  (local-set-key (kbd "<C-left>") 'xwl-metapost-pdftopng))

;; (require 'tex-info)
(defun xwl-texinfo-mode-compile ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (search-forward "@menu" nil t)
        (texinfo-make-menu))
      (save-buffer)
      (makeinfo-buffer)
      (delete-other-windows))))

;; metafont
(add-hook 'metapost-mode-hook 'xwl-metapost-mode-hook)

;; Texinfo
;; -------
(require 'texinfo)
(when (eq system-type 'darwin)
  (setq makeinfo-run-command
        "~/repo/cvs/texinfo/makeinfo/makeinfo"))

;; (define-key Texinfo-mode-map (kbd "C-c g c") 'makeinfo-buffer)
;; disable fill at all!
(setq makeinfo-options "--fill-column=999")
;; "--paragraph-indent=0")

(defun xwl-TeX-insert-macro (symbol)
  "A better `TeX-insert-macro'."
  (interactive (list (completing-read (concat "Macro (default "
					      TeX-default-macro
					      "): "
					      TeX-esc)
				      (TeX-symbol-list) nil nil nil
				      'TeX-macro-history)))
  (cond ((string-equal symbol "")
	 (setq symbol TeX-default-macro))
	((interactive-p)
	 (setq TeX-default-macro symbol)))
  ;; Generate @node automatically
  (if (member symbol '("chapter" "section" "subsection" "subsubsection"
                       
                       "unnumbered" "unnumberedsec" "unnumberedsubsec"
                       "unnumberedsubsubsec"))
      (let ((title (read-string "Chapter title: ")))
      (insert (format "@node %s\n@%s %s\n\n" title symbol title)))
    (insert "@" symbol)))

(defun xwl-TeX-insert-chapter ()
  (interactive)
  (xwl-TeX-insert-macro "chapter"))

(eval-after-load 'tex-info
  '(progn
     (define-key Texinfo-mode-map (kbd "C-c M-%") 'xwl-TeX-insert-macro)
     (define-key Texinfo-mode-map (kbd "C-c DEL") 'xwl-TeX-insert-chapter)
     ))


(provide 'xwl-tex)

;;; xwl-tex.el ends here
