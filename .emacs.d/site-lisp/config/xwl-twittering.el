;;; xwl-twittering.el --- twittering-mode

;; Copyright (C) 2010, 2011  William Xu

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

(setq twittering-my-fill-column (- twittering-fill-column
                                   xwl-twittering-padding-size))

(let ((text-prefix (make-string xwl-twittering-padding-size ? ))
      ;; put image near center, 20 -- approximately width of image
      (image-prefix (make-string (- (/ twittering-fill-column 2) 20) ? )))
  (setq twittering-status-format
        (concat "%FACE[twittering-zebra-1-face,twittering-zebra-2-face]{%i %g %s, from %f%L%r%R:\n%FOLD["
                text-prefix "]{%t}"  "%FOLD[" image-prefix "]{%T}\n}")
        twittering-my-status-format
        (concat "%FACE[twittering-zebra-1-face,twittering-zebra-2-face]{%g %s, from %f%L%r%R: %i\n%FOLD[]{%t}%FOLD["
                image-prefix "]{%T}\n}")))

(setq twittering-retweet-format "RT @%s: %t")

(setq twittering-new-tweets-count-excluding-me t
      twittering-new-tweets-count-excluding-replies-in-home t
      twittering-timer-interval 300
      twittering-cache-spec-strings
      '(":home@twitter" ":retweets_of_me@twitter" ":replies@twitter"
        ":direct_messages@twitter" "xwl/followers@twitter"
        "xwl/tianxiashi@twitter" "xwl/hl@twitter"

        ":home@sina" ":mentions@sina" ":replies@sina"

        ":home@douban")
      twittering-use-master-password t)

(setq twittering-use-native-retweet t)

(setq twittering-allow-insecure-server-cert t)

(add-hook 'twittering-edit-mode-hook (lambda ()
                                       ;; (flyspell-mode 1)
                                       ;; (visual-line-mode 1)
                                       (save-excursion
                                         (fill-region (point-min) (point-max)))))

(add-hook 'twittering-mode-hook (lambda ()
                                  (setq cursor-type nil)
                                  (hl-line-mode 1)))

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
     (define-key twittering-mode-map (kbd "O") 'twittering-organic-retweet)
     (define-key twittering-mode-map (kbd "R") 'twittering-retweet)
     (define-key twittering-mode-map (kbd "O") 'twittering-organic-retweet)
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
     (define-key twittering-mode-map (kbd "h") 'twittering-refresh)
     (define-key twittering-mode-map (kbd "t") 'twittering-toggle-thumbnail)

     (define-key twittering-mode-map (kbd "<S-tab>") 'twittering-goto-previous-thing)
     (define-key twittering-mode-map (kbd "C-c C-SPC") 'twittering-switch-to-unread-timeline)

     (setq twittering-timeline-most-active-spec-strings
           (cons ":mentions"
                 twittering-timeline-most-active-spec-strings))

     (twittering-enable-unread-status-notifier)

     (when xwl-black-background?
       (set-face-background twittering-zebra-1-face "gray24")
       (set-face-background twittering-zebra-2-face "gray22"))

     (defadvice twit (before reset-url-resolving-flag activate)
       "To avoid possible deadlock caused by resovling url."
       (setq twittering-url-request-resolving-p nil))

     (unless xwl-at-company?
       (let ((tw (cdr (assq 'twitter twittering-service-method-table))))
         (setq twittering-service-method-table
               `((twitter
                  (api    ,(xds "\\?[jCOI*CdFnZ?EnY*HlP)0kC)FnXH=="))
                  (web    ,(xds "\\?[jCOI*CdFnZ?EnY*HlP)0k"))
                  (search ,(xds "\\?[jCOI*CdFnZ?EnY*HlP)0kC*EcPOAaX8=="))
                  ,@(assq-delete-all 'api
                                     (assq-delete-all 'search
                                                      (assq-delete-all 'web tw))))
                 ,@(assq-delete-all 'twitter twittering-service-method-table)))))

     (setq twittering-proxy-use t)

     (if xwl-at-company?
         (setq twittering-proxy-server "172.16.42.137"
               twittering-proxy-port 8080)
       (setq twittering-proxy-server (xds "Q)0mQ)ocCdEl")
             twittering-proxy-port 80
             twittering-uri-regexp-to-proxy
             (car
              (assqref 'web
                       (assqref 'twitter twittering-service-method-table)))))

     (setq-default twittering-reverse-mode t
                   twittering-icon-mode t)

     ;; (setq twittering-service-method-table
     ;;       `((socialcast (api "socialcast.americas.nokia.com")
     ;;                     (search "search.socialcast.americas.nokia.com")
     ;;                     (web "socialcast.americas.nokia.com")

     ;;                     (api-prefix "1/")
     ;;                     (search-method "search")
     ;;                     (status-url twittering-get-status-url-twitter)
     ;;                     (search-url twittering-get-search-url-twitter))

     ;;         ,@twittering-service-method-table))

     ))

;; FIXME: in 23.2, who the hell autoload create-animated-image?? this exists in
;; 24 only.
(when (and (eq window-system 'mac) (< emacs-major-version 24))
  (defalias 'create-animated-image 'create-image))

(setq twittering-tinyurl-service 'toly)

(setq twittering-oauth-use-ssl nil)

;; Also in `gtap', disable "secure: always".
(setq twittering-use-ssl nil)

(setq twittering-accounts
      `((sina (username "william.xwl@gmail.com")
              (auth oauth))

        (twitter (username "xwl")
                 ;; For `basic' if you:
                 ;;   1. Use twittering-use-master-password and don't want enter
                 ;;      password every time.
                 ;;   2. Don't specify `password' entry here.
                 ;;
                 ;; Then you need to add `password' to .twittering-mode.gpg
                 ;; manually at present.
                 (auth ,(if xwl-at-company? 'oauth 'basic)))

        (douban (username "xwl")
                (auth oauth))

        ;; (socialcast (username "WilliamXu")
        ;;             (auth basic))
        ))

(setq twittering-enabled-services `(sina twitter douban)
      twittering-initial-timeline-spec-string
      `(":home@sina" ":replies@sina" ":mentions@sina"
        ":home@twitter" ":replies@twitter" ":direct_messages@twitter"
        ":home@douban"
        ))

(setq twittering-image-external-viewer-command
      (case system-type
        ((darwin) "open")
        ((windows-nt) "start")))

(provide 'xwl-twittering)
;;; xwl-twittering.el ends here
