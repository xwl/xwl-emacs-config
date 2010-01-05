;;; xwl-window.el --- GUI window related config

;; Copyright (C) 2007, 2008, 2009 William Xu

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

(unless window-system 
  (error "This config only makes sense for a window-system"))

;;; Fonts

;; (for linux) Add new fonts
;; 1. ~/.fonts
;; 2. fc-cache -rf

;; http://www.gringod.com/2006/02/24/return-of-monacottf/
(setq xwl-window-fonts
      `((mac . ("Monaco-14" "stheiti*" "hiragino maru gothic pro"))
        (ns  . ("Monaco-14" "Hiragino Sans GB" "Hiragino_Kaku_Gothic_ProN"))
        (w32 . ("Monaco-10" "NSimSun" "NSimSun"))
        (x   . ,(if (string= system-name "debian..xwl")
                   '("DejaVu Sans Mono-11" "wenquanyi" "wenquanyi")
                 '("DejaVu LGC Sans Mono-14" "SimSun" "SimSun")))))

(let* ((fonts (cdr (assoc window-system xwl-window-fonts)))
       (default-font (nth 0 fonts))
       (cn-font (nth 1 fonts))
       (jp-font (nth 2 fonts))
       (charset-fonts `((japanese-jisx0208 . ,jp-font)
                        (chinese-gb2312    . ,cn-font)
                        (chinese-gbk       . ,cn-font)
                        (gb18030           . ,cn-font)
                        ;; (big5           . "Hei")
                        (japanese-jisx0208 . ,jp-font)
                        ;; (japanese-jisx0212 . ,jp-font)
                        )))
  (set-default-font default-font)       ; this will decide font size.
  (mapc (lambda (cf)
          (set-fontset-font (frame-parameter nil 'font)
                            (car cf)
                            (font-spec :family (cdr cf) :size 14)))
        charset-fonts))

;;; Misc

(global-set-key (kbd "C--") 'undo)
(global-unset-key (kbd "C-z"))

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
;;   (unless xwl-at-company-p
;;     (setq explicit-shell-file-name "c:/cygwin/bin/bash.exe")
;;     (setq shell-file-name explicit-shell-file-name)))

(setq ns-pop-up-frames nil)

;; control+英数(jis_esiuu)+p is missing on my macbook! :(
(global-set-key (kbd "C-M-@") (kbd "C-M-p"))

(provide 'xwl-window)

;;; xwl-window.el ends here
