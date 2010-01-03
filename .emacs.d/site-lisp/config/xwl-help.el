;;; xwl-help.el --- Getting help

;; Copyright (C) 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;; common

(setq xwl-help-modes '(help-mode woman-mode Man-mode))

;;; describe-*

(eval-after-load "help-fns"
  '(progn
     (defun xwl-split-window-horizontally ()
       (when (< emacs-major-version 23)
         (split-window-horizontally)))

     (defadvice describe-function (around jump-to-help activate)
       (cond
        ((eq major-mode 'help-mode)
         ad-do-it)
        ((memq major-mode xwl-help-modes)
         (other-window 1)
         ad-do-it
         (other-window 1))
        ((>= (length (window-list)) 2)
         ad-do-it
         (other-window 1))
        (t
         (xwl-split-window-horizontally)
         ad-do-it
         (other-window 1))))

     (defadvice describe-variable (around jump-to-help activate)
       (cond
        ((eq major-mode 'help-mode)
         ad-do-it)
        ((memq major-mode xwl-help-modes)
         (other-window 1)
         ad-do-it
         (other-window 1))
        ((>= (length (window-list)) 2)
         ad-do-it
         (other-window 1))
        (t
         (xwl-split-window-horizontally)
         ad-do-it
         (other-window 1))))

     (defadvice describe-bindings (around jump-to-help activate)
       (cond
        ((eq major-mode 'help-mode)
         ad-do-it)
        ((memq major-mode xwl-help-modes)
         (other-window 1)
         ad-do-it
         (other-window 1))
        ((>= (length (window-list)) 2)
         ad-do-it
         (other-window 1))
        (t
         (xwl-split-window-horizontally)
         ad-do-it
         (other-window 1))))

     (defadvice describe-key (around jump-to-help activate)
       (cond
        ((eq major-mode 'help-mode)
         ad-do-it)
        ((memq major-mode xwl-help-modes)
         (other-window 1)
         ad-do-it
         (other-window 1))
        ((>= (length (window-list)) 2)
         ad-do-it
         (other-window 1))
        (t
         (xwl-split-window-horizontally)
         ad-do-it
         (other-window 1))))

     (defadvice describe-mode (around jump-to-help activate)
       (cond
        ((eq major-mode 'help-mode)
         ad-do-it)
        ((memq major-mode xwl-help-modes)
         (other-window 1)
         ad-do-it
         (other-window 1))
        ((>= (length (window-list)) 2)
         ad-do-it
         (other-window 1))
        (t
         (xwl-split-window-horizontally)
         ad-do-it
         (other-window 1))))
     ))

(eval-after-load "help-mode"
  '(progn
     (define-key help-mode-map (kbd "l") 'help-go-back)
     (define-key help-mode-map (kbd "L") 'locate)
     (define-key help-mode-map "q" 'delete-window)
     (define-key view-mode-map "q" 'delete-window)
     ))


;;; woman & man

(setq woman-fill-column default-fill-column
      woman-cache-level 3)   ; `C-u woman' sometimes to update manpages.


(eval-after-load "woman"
  '(progn
     (setq woman-manpath
           `("/Users/william/share/man"
             "/sw/share/man"
             ;; cygwin
             "c:/cygwin/usr/share/man"
             "c:/cygwin/usr/ssl/man"

             ,@woman-manpath))

     (define-key woman-mode-map "q" 'delete-window)

     ;; When woman fails, man stands up, please!
     (defadvice woman (around woman-and-man activate)
       (condition-case nil
           (cond
            ((memq major-mode xwl-help-modes)
             ad-do-it)
            ((>= (length (window-list)) 2)
             (other-window 1)
             ad-do-it)
            (t
             (split-window-horizontally)
             (other-window 1)
             ad-do-it))
         (error
          (progn
            (winner-undo)
            (man woman-last-file-name)))))
     ))

(setq Man-notify-method 'pushy)

(eval-after-load "man"
  '(progn
     (define-key Man-mode-map "q" 'delete-window)

     (defadvice man (around split-horizontally activate)
       (cond
        ((memq major-mode xwl-help-modes)
         ad-do-it)
        ((>= (length (window-list)) 2)
         (other-window 1)
         ad-do-it)
        (t
         (split-window-horizontally)
         (other-window 1)
         ad-do-it
         )))
     ))


;;; minibuffer completion

;; (defadvice minibuffer-complete (around split-horizontally activate)
;;   (save-window-excursion
;;     (cond
;;      ((>= (length (window-list)) 2)
;;       ad-do-it)
;;      (t
;;       (save-excursion
;;         (other-window 1)
;;         (split-window-horizontally))
;;       ad-do-it))))

;; (ad-deactivate 'minibuffer-complete)


(provide 'xwl-help)

;;; xwl-help.el ends here
