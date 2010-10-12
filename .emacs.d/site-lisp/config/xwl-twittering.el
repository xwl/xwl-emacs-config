;;; xwl-twittering.el --- twittering-mode

;; Copyright (C) 2010  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Local variables, set this with caution. :)  Should be set before loading
;; twittering-mode.
(setq twittering-reverse-mode t
      twittering-icon-mode t)

(setq twittering-username "xwl"
      twittering-password pwtwitter)

(if xwl-at-company?
    (setq twittering-proxy-use t
          twittering-proxy-server "172.16.42.137"
          twittering-proxy-port 8080)
  (setq twittering-auth-method 'basic)

  ;; Also in `gtap', disable "secure: always".
  (setq twittering-use-ssl nil)

  (setq twittering-web-host (xds "\\?[jCOI*CdFnZ?EnY*HlP)0k")
        twittering-api-host (xds "\\?[jCOI*CdFnZ?EnY*HlP)0kC)FnXH==")
        twittering-api-search-host (xds "\\?[jCOI*CdFnZ?EnY*HlP)0kC*EcPOAaX8=="))

  ;; (setq twittering-web-host (xds "[?[g[?IcZ`,+[)nlPO9gQ)McCdEmYH==")
  ;;       twittering-api-host (xds "[?[g[?IcZ`(_Z>bl\\?[jCdFnXN[cQJ,aY)'=")
  ;;       twittering-api-search-host (xds "[?[g[?IcZ`(qQNFpP)^l\\?[jCdFnXN[cQJ,aY)'="))
  )

(setq xwl-twittering-padding-size 8)

(setq twittering-my-fill-column (- twittering-fill-column
                                   xwl-twittering-padding-size))

(setq twittering-status-format (concat "%i %g %s, from %f%L%r%R:\n%FILL["
                                       (make-string xwl-twittering-padding-size ? )
                                       "]{%T}\n")
      twittering-my-status-format "%g %s, from %f%L%r%R: %i\n%FILL[]{%T}\n")

(setq twittering-retweet-format "RT @%s: %t")

(setq twittering-url-show-status nil)

(setq twittering-new-tweets-count-excluding-me t
      twittering-new-tweets-count-excluding-replies-in-home t
      twittering-timer-interval 300
      twittering-cache-spec-strings
      '(":home" ":retweets_of_me" ":replies" ":direct_messages" "xwl/followers"
        ":search/twittering-mode/" ":search/emacs/" "xwl/tianxiashi" "xwl/hl")
      twittering-use-master-password t)

(setq twittering-use-native-retweet t)

(setq twittering-allow-insecure-server-cert t)

(add-hook 'twittering-edit-mode-hook (lambda ()
                                       (flyspell-mode 1)
                                       ;; (visual-line-mode 1)
                                       (save-excursion
                                         (fill-region (point-min) (point-max)))))

(add-hook 'twittering-mode-hook (lambda ()
                                  (setq cursor-type nil)
                                  ;; (hl-line-mode 1)
                                  ))
;; Disable URI handling in twittering, let's use goto-address-mode instead.
(setq twittering-regexp-uri "^^$")

(eval-after-load 'twittering-mode
  '(progn
     (define-key twittering-mode-map (kbd "c") 'twittering-current-timeline)

     (define-key twittering-mode-map (kbd "n") 'twittering-goto-next-status)
     (define-key twittering-mode-map (kbd "p") 'twittering-goto-previous-status)
     (define-key twittering-mode-map (kbd "N") 'twittering-goto-next-status-of-user)
     (define-key twittering-mode-map (kbd "P") 'twittering-goto-previous-status-of-user)

     (define-key twittering-mode-map (kbd "q") 'xwl-hide-buffer)

     (define-key twittering-mode-map (kbd "F") 'twittering-follow)
     (define-key twittering-mode-map (kbd "U") 'twittering-unfollow)
     (define-key twittering-mode-map (kbd "R") 'twittering-retweet)
     (define-key twittering-mode-map (kbd "@") 'twittering-reply-to-user)
     (define-key twittering-mode-map (kbd "D") 'twittering-direct-message)
     (define-key twittering-mode-map (kbd "C") 'twittering-erase-all)

     (define-key twittering-mode-map (kbd "C-c C-g") nil)
     (define-key twittering-mode-map (kbd "RET") nil)
     (define-key twittering-mode-map (kbd "d") nil)
     (define-key twittering-mode-map (kbd "i") nil)
     (define-key twittering-mode-map (kbd "t") nil)
     (define-key twittering-mode-map (kbd "s") 'twittering-search)
     (define-key twittering-mode-map (kbd "d") nil)

     (define-key twittering-mode-map (kbd "<S-tab>") 'twittering-goto-previous-thing)
     (define-key twittering-mode-map (kbd "C-c C-SPC") 'twittering-switch-to-unread-timeline)

     ;; (setq twittering-timeline-most-active-spec-strings
     ;;       (cons ":replies"
     ;;             twittering-timeline-most-active-spec-strings))

     (twittering-enable-unread-status-notifier)

     (when xwl-black-background?
       (set-face-background twittering-zebra-1-face "gray24")
       (set-face-background twittering-zebra-2-face "gray22"))

     ))

;; FIXME: in 23.2, who the hell autoload create-animated-image?? this exists in
;; 24 only.
(when (eq window-system 'mac)
  (defalias 'create-animated-image 'create-image))


(provide 'xwl-twittering)
;;; xwl-twittering.el ends here
