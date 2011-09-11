;;; xwl-window.el --- GUI window related config

;; Copyright (C) 2007, 2008, 2009, 2010, 2011 William Xu

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

(let* ((all-fonts
        `((mac . ("Monaco-14" "stheiti*" "hiragino maru gothic pro"))
          (ns  . ,(if (equal user-login-name "william")
                      '("Monaco-16"
                        "-apple-Hiragino_Sans_GB-medium-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                        "-apple-Hiragino_Sans_GB-medium-normal-normal-*-*-*-*-*-p-0-iso10646-1")
                    '("Monaco-14" "Hiragino Sans GB" "Hiragino_Kaku_Gothic_ProN")))
          (w32 . ("Monaco-10"
                  ;; "NSimSun" "NSimSun"
                  "SimSun" "Meiryo"
                  ;"微软雅黑" "微软雅黑"
                  ;; "-outline-SimSun-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1"
                  ;; "汉鼎繁中变" "汉鼎繁中变" "汉鼎繁中变"
                  ))
          (x   . ,(if (string= system-name "debian..xwl")
                      '("DejaVu Sans Mono-11" "wenquanyi" "wenquanyi")
                    '("DejaVu LGC Sans Mono-14" "wenquanyi" "wenquanyi")
                    ;;'("DejaVu LGC Sans Mono-13" "SimSun" "SimSun")
                    ))))
       (fonts (cdr (assoc window-system all-fonts)))
       (def (nth 0 fonts))
       (cn (nth 1 fonts))
       (jp (nth 2 fonts))
       (charset-fonts `((gb18030           . ,cn)
                        (chinese-gbk       . ,cn)
                        (chinese-gb2312    . ,cn)
                        (japanese-jisx0208 . ,jp)
                        (japanese-jisx0212 . ,jp)
                        )))
  (set-default-font def)       ; this will decide default font size.
  (mapc (lambda (cf)
          (set-fontset-font
           t (car cf) (font-spec :family (cdr cf) :size 16)))
        charset-fonts)
  (when (eq system-type 'windows-nt)
    ;; Mathematical Operators , SimSun only partial support.
    ;; http://en.wikipedia.org/wiki/Unicode_Mathematical_Operators#Mathematical_Operators
    (set-fontset-font t '(#x2200 . #x22ff) jp)))

;;; Misc

(global-set-key (kbd "C--") 'undo)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
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
                    (current-buffer))))
            ))

(set-cursor-color "Magenta")

(setq frame-inherited-parameters '(font))

(provide 'xwl-window)

;;; xwl-window.el ends here
