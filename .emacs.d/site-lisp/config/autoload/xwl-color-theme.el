;;; xwl-color-theme.el --- color theme setup

;; Copyright (C) 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

;;; color-theme, `list-colors-display', `color-theme-print'.

;; (color-theme-xwl-console)
;; color-theme-robin-hood
;; color-theme-standard
;; color-theme-vim-colors

;;;###autoload
(defun color-theme-xwl-console ()
  "Color theme base on console colors with `color-theme-print'.
Created on 2005-05-13."
  (interactive)
  (unless window-system
    (error "Better not use color theme on console"))
  (require 'color-theme)
  (color-theme-install
   '(color-theme-xwl-console
     (
      (foreground-color  . "#c0c0c0")
      (background-color  .              ;"black"
                         ;; "#202020"
                         ;; "#2d2d2d"
                         ;; "#2e3436"
                         ;; "#111111"
                         "#222222"
                         )

      ;; (foreground-color  . "black")
      ;; (background-color  . "#f7f7f7")

      (mouse-color       . "black")
      (cursor-color      . "medium turquoise")
      (border-color      . "black")
      (background-mode   .  dark))

     ((Man-overstrike-face . bold)
      (Man-underline-face . underline)
      (browse-kill-ring-separator-face . bold)
      (display-time-mail-face . mode-line)
      (erc-button-face . bold)
      (erc-button-mouse-face . highlight)
      (gnus-article-button-face . bold)
      (gnus-article-mouse-face . highlight)
      (gnus-cite-attribution-face . gnus-cite-attribution-face)
      (gnus-mouse-face . highlight)
      (gnus-server-agent-face . gnus-server-agent-face)
      (gnus-server-closed-face . gnus-server-closed-face)
      (gnus-server-denied-face . gnus-server-denied-face)
      (gnus-server-offline-face . gnus-server-offline-face)
      (gnus-server-opened-face . gnus-server-opened-face)
      (gnus-signature-face . gnus-signature-face)
      (gnus-summary-selected-face . gnus-summary-selected-face)
      (gnus-treat-display-face . head)
      (help-highlight-face . underline)
      (hl-line-face . highlight)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face)
      (list-matching-lines-face . bold)
      (setnu-line-number-face . setnu-line-number-face)
      (view-highlight-face . highlight)
      (w3m-form-mouse-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "darkslategrey" :foreground
                            "wheat" :inverse-video nil :box nil
                            :strike-through nil :overline nil :underline
                            nil :slant normal :weight normal :width
                            normal :family "outline-andale mono"))))
     (Info-title-1-face ((t (:bold t :foreground "yellow" :weight bold))))
     (Info-title-2-face ((t (:bold t :foreground "lightblue" :weight bold))))
     (Info-title-3-face ((t (:bold t :weight bold))))
     (Info-title-4-face ((t (:bold t :weight bold))))

     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (nil))))
     (calendar-today-face ((t (:underline t))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))
     (cursor ((t (nil))))
     (emacs-wiki-bad-link-face ((t (:bold t :foreground "coral" :underline "coral" :weight bold))))
     (emacs-wiki-link-face ((t (:bold t :foreground "cyan" :underline "cyan" :weight bold))))
     (emacs-wiki-verbatim-face ((t (:foreground "gray"))))
     (emms-pbi-current-face ((t (:bold t :foreground "magenta" :weight bold))))
     (emms-pbi-song-face ((t (:foreground "green"))))

     (fixed-pitch ((t (:family "courier"))))
     (font-lock-builtin-face ((t (:foreground "blue" :weight light))))
     (font-lock-comment-face ((t (:foreground "red1"))))
     (font-lock-constant-face ((t (:foreground "magenta"))))
     (font-lock-doc-face ((t (:foreground "green"))))
     (font-lock-function-name-face ((t (:bold t :foreground "royal blue" :weight bold)))) ;
     (font-lock-keyword-face ((t (:bold t :foreground "cyan" :weight bold))))
     (font-lock-string-face ((t (:foreground "green"))))
     (font-lock-type-face ((t (:foreground "green"))))
     (font-lock-variable-name-face ((t (:foreground "yellow" :weight light))))
     (font-lock-warning-face ((t (:foreground "red"))))
     (fringe ((t (:background "grey10"))))
     (fvwm-button-conf-face ((t (:bold t :foreground "orange3" :weight bold :family "lucida"))))
     (fvwm-conf-face ((t (:bold t :foreground "brown" :weight bold :family "lucida"))))
     (fvwm-func-menu-face ((t (:bold t :foreground "palevioletred" :weight bold :family "lucida"))))
     (fvwm-module-face ((t (:bold t :foreground "black" :weight bold :family "lucida"))))
     (fvwm-mycomments-face ((t (:bold t :foreground "purple" :weight bold :family "lucida"))))
     (fvwm-piperead-face ((t (:bold t :foreground "#900090" :weight bold :family "lucida"))))
     (fvwm-special-face ((t (:bold t :foreground "#ff5555" :weight bold :family "lucida"))))

     ;; (gnus-cite-attribution-face ((t (:family "arial"))))
     (gnus-cite-face-10 ((t (nil))))
     (gnus-cite-face-11 ((t (nil))))
     (gnus-cite-face-1 ((t (:foreground "DarkGoldenrod3"))))
     ;; (gnus-cite-face-1 ((t (:foreground "ForestGreen"))))
     (gnus-cite-face-2 ((t (:foreground "IndianRed3"))))
     (gnus-cite-face-3 ((t (:foreground "tomato"))))
     (gnus-cite-face-4 ((t (:foreground "yellow green"))))
     (gnus-cite-face-5 ((t (:foreground "SteelBlue3"))))
     (gnus-cite-face-6 ((t (:foreground "Azure3"))))
     (gnus-cite-face-7 ((t (:foreground "Azure4"))))
     (gnus-cite-face-8 ((t (:foreground "SpringGreen4"))))
     (gnus-cite-face-9 ((t (:foreground "SlateGray4"))))
     (gnus-emphasis-bold ((t (:bold t :foreground "greenyellow" :weight bold :family "Arial"))))
     (gnus-emphasis-bold-italic
      ((t (:italic t :bold t :foreground "OrangeRed1" :slant italic :weight bold :family "arial"))))
     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "khaki"))))
     (gnus-emphasis-italic
      ((t (:italic t :bold t :foreground "orange" :slant italic :weight bold :family "Arial"))))
     (gnus-emphasis-underline ((t (:foreground "greenyellow" :underline t))))
     (gnus-emphasis-underline-bold
      ((t (:bold t :foreground "khaki" :underline t :weight bold :family "Arial"))))
     (gnus-emphasis-underline-bold-italic
      ((t (:italic t :bold t :underline t :slant italic :weight bold :family "Arial"))))
     (gnus-emphasis-underline-italic
      ((t (:italic t :foreground "orange" :underline t :slant italic :family "Arial"))))
     (gnus-group-mail-1-empty-face ((t (:foreground "Salmon4"))))
     (gnus-group-mail-1-face ((t (:bold t :foreground "firebrick1" :weight bold))))
     (gnus-group-mail-2-empty-face ((t (:foreground "turquoise4"))))
     (gnus-group-mail-2-face ((t (:bold t :foreground "turquoise" :weight bold))))
     (gnus-group-mail-3-empty-face ((t (:foreground "LightCyan4"))))
     (gnus-group-mail-3-face ((t (:bold t :foreground "LightCyan1" :weight bold))))
     (gnus-group-mail-low-empty-face ((t (:foreground "SteelBlue4"))))
     (gnus-group-mail-low-face ((t (:bold t :foreground "SteelBlue2" :weight bold))))
     (gnus-group-news-1-empty-face ((t (:foreground "Salmon4"))))
     (gnus-group-news-1-face ((t (:bold t :foreground "FireBrick1" :weight bold))))
     (gnus-group-news-2-empty-face ((t (:foreground "darkorange3"))))
     (gnus-group-news-2-face ((t (:bold t :foreground "dark orange" :weight bold))))
     (gnus-group-news-3-empty-face ((t (:foreground "turquoise4"))))
     (gnus-group-news-3-face ((t (:bold t :foreground "Aquamarine" :weight bold))))
     (gnus-group-news-4-empty-face ((t (:foreground "SpringGreen4"))))
     (gnus-group-news-4-face ((t (:bold t :foreground "SpringGreen2" :weight bold))))
     (gnus-group-news-5-empty-face ((t (:foreground "OliveDrab4"))))
     (gnus-group-news-5-face ((t (:bold t :foreground "OliveDrab2" :weight bold))))
     (gnus-group-news-6-empty-face ((t (:foreground "DarkGoldenrod4"))))
     (gnus-group-news-6-face ((t (:bold t :foreground "DarkGoldenrod3" :weight bold))))
     (gnus-group-news-low-empty-face ((t (:foreground "wheat4"))))
     (gnus-group-news-low-face ((t (:bold t :foreground "tan4" :weight bold))))

     (gnus-header-name-face ((t (:bold t :foreground "SeaGreen"))))
     (gnus-header-content-face ((t (:foreground "SeaGreen"  :italic t))))

     (gnus-header-from-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (gnus-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "SeaGreen" :slant italic))))

     (gnus-signature-face ((t (:italic t :foreground "yellow2" :slant italic))))
     (gnus-splash-face ((t (:foreground "Firebrick1"))))
     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
     (gnus-summary-high-ancient-face ((t (:bold t :foreground "MistyRose4" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "coral" :weight bold))))
     (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground
                                                 "red1" :slant italic
                                                 :weight bold))))
     (gnus-summary-low-ancient-face ((t (:italic t :foreground "DarkSeaGreen4" :slant italic))))
     (gnus-summary-low-read-face ((t (:foreground "SeaGreen4"))))
     (gnus-summary-low-ticked-face ((t (:italic t :foreground "Green4" :slant italic))))
     (gnus-summary-low-unread-face ((t (:italic t :foreground "green3" :slant italic))))
     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))
     (gnus-summary-normal-read-face ((t (:foreground "khaki4"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "khaki3"))))
     (gnus-summary-normal-unread-face ((t (:foreground "khaki"))))

     ;; highlight current post and posts related with me
     (gnus-summary-selected-face ((t (:foreground "yellow" :background "blue4"))))
     (gnus-summary-high-read-face ((t (:foreground "magenta"))))

     (header-line ((t (:underline nil))))
     (highlight ((t (:background "tomato3"))))
     (holiday-face ((t (:background "chocolate4"))))
     (ibuffer-deletion-face ((t (:foreground "red"))))
     (ibuffer-marked-face ((t (:foreground "green"))))
     (ido-first-match ((t (:foreground "magenta" :bold t :weight bold))))
     (ido-indicator ((t (:background "red" :foreground "yellow" :width condensed))))
     (ido-only-match ((t (:foreground "red"))))
     (ido-subdir ((t (:foreground "yellow"))))

     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "white"))))
     (info-header-xref ((t (:bold t :weight bold :foreground "cyan"))))
     (info-menu-5 ((t (:foreground "red1"))))
     (info-menu-header ((t (:bold t :underline t :weight bold))))
     (info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
     (info-xref ((t (:bold t :foreground "cyan" :weight bold))))

     (isearch ((t (:background "magenta4" :foreground "cyan1"))))
     (isearch-lazy-highlight-face ((t (:background "turquoise3"))))
     (italic ((t (:italic t :slant italic))))
     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "White"))))
     (message-header-cc-face ((t (:foreground "light cyan"))))
     (message-header-name-face ((t (:foreground "DodgerBlue1"))))
     (message-header-newsgroups-face ((t (:italic t :bold t :foreground
                                                  "LightSkyBlue3" :slant
                                                  italic :weight
                                                  bold))))
     (message-header-other-face ((t (:foreground "LightSkyBlue3"))))
     (message-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (message-header-to-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (message-header-xheader-face ((t (:foreground "DodgerBlue3"))))
     (message-mml-face ((t (:foreground "ForestGreen"))))
     (message-separator-face ((t (:background "cornflower blue" :foreground "chocolate"))))

     (mode-line ((t (:background "LightSlateGray" :foreground "black"))))
     (modeline ((t (:background "LightSlateGray" :foreground "black"))))
     (modeline-buffer-id ((t (:background "LightSlateGray" :foreground "blue4"))))
     (modeline-mousable ((t (:background "LightSlateGray" :foreground "firebrick"))))
     (modeline-mousable-minor-mode ((t (:background "LightSlateGray" :foreground "green4"))))

     (mouse ((t (nil))))
     (muse-bad-link-face ((t (:bold t :foreground "coral" :underline "coral" :weight bold))))
     (muse-header-1 ((t (:bold t :family "helv" :weight bold :height 1.4 :foreground "blue"))))
     (muse-header-2 ((t (:bold t :family "helv" :weight bold :height 1.3 :foreground "yellow"))))
     (muse-header-3 ((t (:bold t :family "helv" :weight bold :height 1.2 :foreground "cyan"))))
     (muse-header-4 ((t (:bold t :family "helv" :weight bold :height 1.1))))
     (muse-header-5 ((t (:bold t :family "helv" :weight bold :height 1.0))))
     (muse-header-6 ((t (:bold t :family "helv" :weight bold :height 0.9))))
     (muse-link-face ((t (:bold t :foreground "cyan" :underline "cyan" :weight bold))))
     (region ((t (:background "RoyalBlue3" :foreground "white")))) ; blue
     (scroll-bar ((t (nil))))
     (secondary-selection ((t (:background "cyan" :foreground "black"))))
     (setnu-line-number-face ((t (:bold t :weight bold))))
     (show-paren-match-face ((t (:background "yellow4")))) ;;yellow
     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
     (tabbar-button-face ((t (:background "gray72" :family "helv"
                                          :foreground "magenta" :box (:line-width 2 :color "white"
                                                                                  :style released-button) :height 0.8 :weight bold))))
     (template-message-face ((t (:bold t :weight bold))))
     (texinfo-heading-face ((t (:bold t :weight bold :foreground "blue"))))
     (tool-bar ((t (nil))))
     (trailing-whitespace ((t (:background "red"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "helv"))))
     (diary-face ((t (:background "yellow" :foreground "black"))))
     (w3m-anchor-face ((t (:foreground "cyan"))))
     (w3m-arrived-anchor-face ((t (:foreground "LightSkyBlue"))))
     (w3m-bold-face ((t (:bold t :weight bold))))
     (w3m-current-anchor-face ((t (:bold t :underline t :weight bold))))
     (w3m-form-button-face ((t (:foreground "red" :underline t))))
     (w3m-form-button-mouse-face ((t (:foreground "red" :underline t))))
     (w3m-form-button-pressed-face ((t (:foreground "red" :underline t))))
     (w3m-form-face ((t (:foreground "red" :underline t))))
     (w3m-header-line-location-content-face ((t (:background "Gray20"
                                                             :foreground
                                                             "LightGoldenrod"))))

     (w3m-header-line-location-title-face ((t (:background "Gray20" :foreground "Cyan"))))
     (w3m-history-current-url-face ((t (:background "cyan" :foreground "LightSkyBlue"))))
     (w3m-image-face ((t (:foreground "PaleGreen"))))
     (w3m-strike-through-face ((t (:strike-through t))))
     (w3m-tab-background-face ((t (:background "white" :foreground "black"))))
     (w3m-tab-selected-face ((t (:background "cyan" :foreground "black"))))
     (w3m-tab-selected-retrieving-face ((t (:background "cyan" :foreground "red"))))
     (w3m-tab-unselected-face ((t (:background "blue" :foreground "black"))))
     (w3m-tab-unselected-retrieving-face ((t (:background "blue" :foreground "OrangeRed"))))
     (w3m-underline-face ((t (:underline t))))
     (widget-button-face ((t (:bold t :weight bold))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-field-face ((t (:background "DarkCyan"))))
     (widget-inactive-face ((t (:foreground "light gray"))))
     (widget-single-line-field-face ((t (:background "green3"))))
     (woman-addition-face ((t (:foreground "orange"))))
     (woman-bold-face ((t (:bold t :foreground "green2" :weight bold))))
     (woman-italic-face ((t (:italic t :underline t :slant italic))))
     (woman-unknown-face ((t (:foreground "cyan"))))

     ))

  (setq xwl-black-background? t))


(provide 'xwl-color-theme)
;;; xwl-color-theme.el ends here
