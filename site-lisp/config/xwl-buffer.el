;;; xwl-buffer.el --- Buffer level operations like switching or listing

;; Copyright (C) 2009, 2010, 2011, 2012 William Xu

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

;;; Code:

;;; ibuffer --- "dired + gnus" for buffers

(setq ibuffer-formats
      `((mark
         modified
         read-only
         " " (name ,(if xwl-at-company? 40 25))
         " " (mode ,(if xwl-at-company? 30 20))
         ;; " " (size 6 -1 :right)
         " " filename)))

(eval-after-load 'ibuffer
  '(progn
     (define-key ibuffer-mode-map (kbd "q") 'xwl-hide-buffer)

     (define-key ibuffer-mode-map (kbd "C-o") nil)

     (define-key ibuffer-mode-map (kbd "N") '(lambda () (interactive)
                                               (end-of-line)
                                               (re-search-forward "^\\[" nil t 1)
                                               (beginning-of-line)
                                               ))

     (define-key ibuffer-mode-map (kbd "P") '(lambda () (interactive)
                                               (beginning-of-line)
                                               (re-search-backward "^\\[" nil t 1)
                                               (beginning-of-line)))

     (define-key ibuffer-mode-map (kbd "g") (lambda ()
                                              (interactive)
                                              (let ((pos (point)))
                                                (ibuffer-update nil)
                                                (goto-char pos)
                                                (recenter))))

     (define-key ibuffer-mode-map (kbd "C-x C-f") nil)))

;; gnus

(setq ibuffer-saved-filter-groups
      '(("default"
         ("elisp" (or (mode . emacs-lisp-mode)
                      (mode . lisp-interaction-mode)))
         ("c/c++" (or (mode . c-mode)
                      (mode . c++-mode)))
         ("text" (filename . ".*"))
         ("dired" (mode . dired-mode))
         ("erc" (mode . erc-mode))
         ("twittering" (mode . twittering-mode))
         ("gnus" (or
                  (mode . message-mode)
                  (mode . bbdb-mode)
                  (mode . mail-mode)
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)
                  (name . "^\\.bbdb$")
                  (name . "^\\.newsrc-dribble"))))))

(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; limit

(eval-after-load 'ibuf-ext
  '(progn
     (define-ibuffer-filter reference
       "Toggle current view to buffers that have references to filename matching QUALIFIER.
Such that we can rename or some other operations successfully for file at
point.  Especially useful for w32."
       (:description "reference"
        :reader (read-from-minibuffer "Filter by file reference (regexp): "))
       (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                          (and (eq (buffer-local-value 'major-mode buf) 'dired-mode)
                               (buffer-local-value 'default-directory buf)))
                      (string-match qualifier it)))

     (define-key ibuffer-mode-map (kbd "/ R") 'ibuffer-filter-by-reference)
     ))

;;; ido

(ido-mode 1)
(ido-everywhere 1)

(setq ido-create-new-buffer 'never
      ido-enable-regexp nil)

;; 1. if no visible match, will match against ignored buffers.
;; 2. one can also toggle this by C-a
(add-to-list 'ido-ignore-files "~")

(mapc (lambda (i)
       (add-to-list 'ido-ignore-buffers i))

     `(,(regexp-opt '(".diary" ".scratch"  "&bitlbee" ".newsrc"
                      ".bbdb" "todo.org" "172.28.206.207" ":6667"
                      ))
       "\\*.+\\*" "^#" "^localhost:" ;; "^:"
       ,@ido-ignore-files))

(defun xwl-update-frequent-directories ()
  (interactive)

  (when xwl-w32?
    (setq xwl-w32-drives (xwl-w32-get-drives)))

  (setq xwl-frequent-directories
        `("~/yapas/middleware/s40_sw"
          "~/"
          "/sudo::/"
          ,(concat xwl-emacs-top "/config")
          ,(concat xwl-emacs-top "/xwl")
          "~/.notes"

          ,@(when xwl-w32?
              (mapcar (lambda (d)
                        (concat (car d)
                                xwl-w32-drive-separator
                                (cdr d)))
                      xwl-w32-drives))
          )))

(xwl-update-frequent-directories)

(defadvice ido-find-file (around change-default-directory activate)
  "Set `default-directory' on the fly."
  (let ((d default-directory))
    (cond (current-prefix-arg
           (setq d (completing-read "let default-directory with: "
                                    xwl-frequent-directories))
           (when xwl-w32?
             (setq d (replace-regexp-in-string
                      (format "^\\([a-zA-Z]\\)%s.*"
                              (regexp-quote xwl-w32-drive-separator))
                      "\\1:"
                      d))))

          ;; Use shell's current dir as default-directory on w32.
          ;; ((and xwl-w32? (eq major-mode 'shell-mode))
          ;;  (save-excursion
          ;;    (goto-char (point-max))
          ;;    (let ((end (progn (search-backward ">" nil t 1)
          ;;                      (point))))
          ;;      (setq d (buffer-substring-no-properties
          ;;               (line-beginning-position) end)))))
          )
    (let ((default-directory (expand-file-name d)))
      ad-do-it)))

(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x C-d") 'ido-dired)

(provide 'xwl-buffer)

;; xwl-buffer.el ends here
