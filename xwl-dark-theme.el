(deftheme xwl-dark
  "Created 2014-01-08.")

(custom-theme-set-faces
 'xwl-dark
 '(default ((t (:inherit nil :stipple nil :background "#111111" :foreground "#c5c5c5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 136 :width normal :foundry "misc" :family "fixed"))))
 '(cursor ((t (:background "medium turquoise"))))
 '(fixed-pitch ((t (:family "courier"))))
 '(variable-pitch ((t (:family "helv"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(highlight ((t (:background "tomato3"))))
 '(region ((t (:background "darkgoldenrod4"))))
 '(shadow ((t (:background "grey15"))))
 '(secondary-selection ((t (:background "cyan" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(font-lock-builtin-face ((t (:weight light :foreground "royal blue"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "red1"))))
 '(font-lock-constant-face ((t (:foreground "magenta"))))
 '(font-lock-doc-face ((t (:foreground "green"))))
 '(font-lock-function-name-face ((t (:foreground "royal blue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "cyan" :weight bold))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "RoyalBlue2"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :weight light))))
 '(font-lock-warning-face ((t (:foreground "red"))))
 '(button ((t (:inherit (link)))))
 '(link ((((class color) (min-colors 88) (background light)) (:foreground "RoyalBlue3" :underline (:color foreground-color :style line))) (((class color) (background light)) (:foreground "blue" :underline (:color foreground-color :style line))) (((class color) (min-colors 88) (background dark)) (:foreground "cyan1" :underline (:color foreground-color :style line))) (((class color) (background dark)) (:foreground "cyan" :underline (:color foreground-color :style line))) (t (:inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:background "grey10"))))
 '(header-line ((t (:underline nil))))
 '(tooltip ((((class color)) (:background "lightyellow" :foreground "black" :inherit (variable-pitch))) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:background "LightSlateGray" :foreground "black"))))
 '(mode-line-buffer-id ((t (:background "LightSlateGray" :foreground "blue4" :weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:weight light :box (:line-width -1 :color "grey75" :style nil) :foreground "grey20" :background "grey90")) (((class color) (min-colors 88) (background dark)) (:weight light :box (:line-width -1 :color "grey40" :style nil) :foreground "grey80" :background "grey30"))))
 '(isearch ((t (:background "magenta4" :foreground "cyan1"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:background "turquoise3"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:background "yellow" :foreground "black")) (((class color) (min-colors 8) (background dark)) (:background "blue" :foreground "white")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'xwl-dark)