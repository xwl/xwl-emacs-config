;;; xwl-window.el --- GUI window related config

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 William Xu

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

(unless window-system
  (error "This config only makes sense for a window-system"))

;;; Fonts

;; (for linux) Add new fonts
;; 1. ~/.fonts
;; 2. fc-cache -rf

;; http://www.gringod.com/2006/02/24/return-of-monacottf/

(defun xwl-setup-font ()
  ;; fontforge
  ;; cn_font_size = (en_font_size * width_factor) * 2
  ;; monaco width factor is 0.6.
  (ignore-errors
    (let* ((hst (car (split-string (shell-command-to-string "hostname"))))

           (configs '(("ubuntu" 17 "DejaVu Sans Mono" "SimSun" "SimSun")
                      ("tokyolove.local" (if xwl-black-background? 15 14) "Monaco" "Hiragino Sans GB" "Hiragino Sans GB")
                      ))

           (en-font-size (if (assoc hst configs) (nth 1 (assoc hst configs)) 14))
           (cn-font-size
            (cond
             ((string-match "tokyolove.local" system-name) (if xwl-black-background? 18 16))
             (t
              (ceiling (* en-font-size 0.6 2)))))

           (fonts (if (assoc hst configs)
                      (cddr (assoc hst configs))
                    '("Monaco" "SimSun" "SimSun")))

           (en (format "%s:pixelsize=%d" (nth 0 fonts) en-font-size))
           (cn (nth 1 fonts))
           (jp (nth 2 fonts)))

      ;; This will decide default font size.
      (set-frame-font en)
      ;; ;; Fallback font
      ;; (set-fontset-font t 'unicode "Arial Unicode MS")
      ;; Font for chinese characters
      (mapc
       (lambda (range)
         (set-fontset-font
          t `(,(car range) . ,(cadr range)) (font-spec :family cn :size cn-font-size)))
       '((#x2E80 #x2EFF)                    ; CJK Radicals Supplement
         (#x3000 #x303F)                    ; CJK Symbols and Punctuation
         (#x31C0 #x31EF)                    ; CJK Strokes
         (#x3200 #x32FF)                    ; Enclosed CJK Letters and Months
         (#x3300 #x33FF)                    ; CJK Compatibility
         (#x3400 #x4DBF)                    ; CJK Unified Ideographs Extension A
         (#x4E00 #x9FFF)                    ; CJK Unified Ideographs
         (#xF900 #xFAFF)                    ; CJK Compatibility Ideographs
         (#xFE30 #xFE4F)                    ; CJK Compatibility Forms

         (#x2000 #x206f)                    ; General Punctuation
         )))))

(xwl-setup-font)

(add-hook 'color-theme-xwl-console-hook 'xwl-setup-font)

;; (when (string-match "be" system-name)
;;   (set-frame-font "-b&h-lucidatypewriter-medium-r-normal-sans-14-135-75-75-m-90-iso8859-1")
;;   )

;;; Misc

(global-set-key (kbd "C--") 'undo)

;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
(mouse-wheel-mode 1)

(setq woman-use-own-frame nil)

;; (global-set-key (kbd "<return>") (kbd "RET"))
;; (global-set-key (kbd "<tab>") (kbd "TAB"))
(keyboard-translate ?\C-m ?\C-@)
(keyboard-translate ?\C-i ?\M-%)

;; (when (eq window-system 'w32)
;;   (unless xwl-at-company?
;;     (setq explicit-shell-file-name "c:/cygwin/bin/bash.exe")
;;     (setq shell-file-name explicit-shell-file-name)))

;; control+鑻辨暟(jis_esiuu)+p is missing on my macbook! :(
(global-set-key (kbd "C-M-@") (kbd "C-M-p"))

(when (eq window-system 'ns)
  (setq ns-pop-up-frames nil)
  (setq ns-show-menu-bar-p t)

  ;; (let ((top (ns-get-resource nil "FrameTop"))
  ;;       (left (ns-get-resource nil "FrameLeft"))
  ;;       (f (selected-frame)))
  ;;   (when (and top left)
  ;;     (set-frame-parameter f 'top (string-to-number top))
  ;;     (set-frame-parameter f 'left (string-to-number left))))
  )

(setq xwl-session-cache-file "~/.emacs.d/session-cache.el")

(when (file-exists-p xwl-session-cache-file)
  (load xwl-session-cache-file))

(add-hook 'kill-emacs-hook
          (lambda ()
            ;; (when (eq system-type 'darwin)
            ;;   (let ((f (selected-frame)))
            ;;     (ns-set-resource nil "FrameTop" (number-to-string
            ;;                                      (frame-parameter f 'top)))
            ;;     (ns-set-resource nil "FrameLeft" (number-to-string
            ;;                                       (frame-parameter f 'left)))))
            (unless (and (eq system-type 'darwin)
                         (not (null (cdr (assq 'fullscreen (frame-parameters))))))
              (with-temp-file xwl-session-cache-file
                (let (print-length
                      print-level)
                  (pp `(modify-frame-parameters
                        (selected-frame)
                        ',(remove-if-not
                           (lambda (i)
                             ;; (memq (type-of (cdr i))
                             ;;       '(string symbol float integer))
                             (memq (car i) '(top left width height ;; font
                                                 ))
                             )
                           (frame-parameters)))
                      (current-buffer)))))
            ))

;; (setq-default cursor-color "Magenta")

(setq frame-inherited-parameters '(font))

(defun xwl-light-background ()
  (interactive)
  (set-background-color "#f8f8f8"))

(xwl-light-background)

(provide 'xwl-window)

;;; xwl-window.el ends here
