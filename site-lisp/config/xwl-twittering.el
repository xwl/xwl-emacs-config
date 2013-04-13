;;; xwl-twittering.el --- twittering-mode

;; Copyright (C) 2010, 2011, 2012, 2013  William Xu

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

(defvar xwl-twitter-direct-accessible?
 (zerop (shell-command "ping -n 1 twitter.com")))

;; (let ((col (max (round (/ (frame-width) 2)) 78))
;;       (padding 8))
;;   (setq twittering-fill-column col
;;         twittering-my-fill-column (- twittering-fill-column padding)))

(setq twittering-retweet-format "RT @%s: %t"
      twittering-use-native-retweet t)

(setq twittering-new-tweets-count-excluding-me t
      twittering-new-tweets-count-excluding-my-replies t
      twittering-timer-interval 300
      twittering-use-master-password t
      )

(setq twittering-allow-insecure-server-cert t)

;; Also in `gtap', don't set `secure' to "secure: always", use "optional" or
;; "disable", instead.

(add-hook 'twittering-edit-mode-hook (lambda ()
                                       ;; (flyspell-mode 1)
                                       ;; (visual-line-mode 1)
                                       (save-excursion
                                         (fill-region (point-min) (point-max)))))

(add-hook 'twittering-mode-hook (lambda ()
                                  (setq cursor-type nil)
                                  (hl-line-mode 1)
                                  (when (eq system-type 'windows-nt)
                                    (setq line-spacing 5))))

;; Disable URI handling in twittering, let's use goto-address-mode instead.
;; (setq twittering-regexp-uri "^^$")

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

     (define-key twittering-mode-map (kbd "A") 'twittering-open-all-thumbnails-externally)
     
     (setq twittering-timeline-most-active-spec-strings
           `(":mentions" ,@twittering-timeline-most-active-spec-strings))

     (twittering-enable-unread-status-notifier)

     (when xwl-black-background?
       (set-face-background 'twittering-zebra-1-face "gray15")
       (set-face-background 'twittering-zebra-2-face "gray11"))

     (when xwl-at-company?
       (let ((sc (assqref 'socialcast twittering-service-method-table)))
         (setq twittering-service-method-table
               `((socialcast
                  (api ,socialcast-api)
                  (web ,socialcast-web)
                  ,@(remove-if (lambda (i) (memq (car i) '(api web))) sc))

                 ,@(remove-if (lambda (i) (eq (car i) 'socialcast)) twittering-service-method-table)))))

     ;; (unless xwl-twitter-direct-accessible?
     ;;   (let ((tw (assqref 'twitter twittering-service-method-table)))
     ;;     (setq twittering-service-method-table
     ;;           `((twitter
     ;;              (api        ,(xds "\\?[jCOI*CdFnZ?EnY*HlP)0kC)FnXH=="))
     ;;              (web        ,(xds "\\?[jCOI*CdFnZ?EnY*HlP)0k"))
     ;;              (search     ,(xds "\\?[jCOI*CdFnZ?EnY*HlP)0kC*EcPOAaX8=="))
     ;;              (stream     ,(xds "\\?[jCOI*CdFnZ?EnY*HlP)0kC*E'ZdM_YH=="))
     ;;              (userstream ,(xds "\\?[jCOI*CdFnZ?EnY*HlP)0kC*MqQOAq[?AcPN'="))
     ;;              ,@(remove-if (lambda (i) (memq (car i) '(api web search stream userstream))) tw))

     ;;             ,@(remove-if (lambda (i) (eq (car i) 'twitter)) twittering-service-method-table)))))

     (when (and xwl-at-company?
                (not xwl-twitter-direct-accessible?))
       (setq twittering-proxy-use t)
       (setq twittering-proxy-server "172.16.42.42"
             twittering-proxy-port 8080))

       ;; (setq twittering-proxy-server "127.0.0.1"
       ;;       twittering-proxy-port 8087
       ;;       twittering-uri-regexp-to-proxy
       ;;       (car
       ;;        (assqref 'web
       ;;                 (assqref 'twitter twittering-service-method-table))))

     (setq-default twittering-reverse-mode t)

     (setq twittering-accounts
           (let* ((socialcast (assqref 'socialcast twittering-accounts))
                  (others (remove-if (lambda (i) (eq (car i) 'socialcast))
                                     twittering-accounts)))
             `((socialcast
                (auth basic)
                ,@(remove-if (lambda (i) (eq (car i) 'auth))
                             socialcast))
          
               ,@others)))

     (defadvice twittering-edit-post-status (after play-sound activate)
       (xwl-shell-command-asynchronously "mplayer ~/.emacs.d/sound/roar.au"))
     
       ))

;; FIXME: in 23.2, who the hell autoload create-animated-image?? this exists in
;; 24 only.
(when (and (eq window-system 'mac) (< emacs-major-version 24))
  (defalias 'create-animated-image 'create-image))

(setq twittering-tinyurl-service 'toly)

(setq twittering-initial-timeline-spec-string
      (sort 
       (remove-duplicates 
        `(":home@sina"
          ":replies@sina" ":mentions@sina"
          ,@(when (or (string-match "tokyo" system-name)
                      xwl-at-company? xwl-twitter-direct-accessible?)
              '(":home@twitter" ":replies@twitter" ":direct_messages@twitter"))
          ":home@douban"

          ,@(when xwl-at-company?
              '(":home@socialcast" ":public@socialcast"))

          ,@(when (boundp 'twittering-initial-timeline-spec-string)
              twittering-initial-timeline-spec-string))
        :test 'equal)
       '(lambda (a b) (string-match ":home" a))))

(setq twittering-image-external-viewer-command
      (case system-type
        ;; ((gnu/linux) "gnome-open")
        ((darwin) "open")
        ((windows-nt)
         (unless (equal system-name "3CNL12234")
           ""))
        ))

(setq twittering-status-filter 'xwl-twittering-status-filter)
(defun xwl-twittering-status-filter (status)
  (let ((spec-string (twittering-current-timeline-spec-string)))
    (not (or
          ;; Hide duplicated retweets
          (let ((rt (twittering-is-retweet? status))
                (table (twittering-current-timeline-referring-id-table)))
            (when (and rt table (not (string= spec-string ":mentions@sina")))
              (not (string= (gethash (assqref 'id rt) table)
                            (assqref 'id status)))))

          (cond
           ((string= spec-string ":public@socialcast")
            (member (assqref 'screen-name (assqref 'user status))
                    (mapcar 'xds
                            '("MdFp[N,HZdFiPOEf"
                              "Kd0iXN<^H)0l[dMpZ)F'XN0lZp8k@F9mZ*Iq"
                              "K*Qg@>A,@<,mX)c_"
                              "Q>F'PNclZ)ceX?H="))))

           ((string= spec-string ":home@sina")
            (or (string-match
                 (regexp-opt
                  (mapcar 'xds
                          '(",Y\\d,P)ONeMmKdFl"
                            ",X)],-Nc,X`?O'Ac[?I,"
                            ",f`I,g`m,P-]-CJ'"
                            "O)ogYec(PN+="
                            ",hB9,gXDZ>MlYeb="
                            )))
                 (assqref 'screen-name (assqref 'user status)))

                (string-match (regexp-opt
                               (mapcar 'xds
                                       '("DK^nEa^,E;Z,D8=="
                                         "DKP+E;HoD;Z*DH==")))
                              (assqref 'id (assqref 'user status)))))
           
           ((string= spec-string ":home@twitter")
            (member (assqref 'screen-name (assqref 'user status))
                    (mapcar 'xds
                            '("P*MjXOL="
                              "[eMiXN,e"
                              "[>0qXNj="
                              ))))
           )))))

(setq twittering-use-icon-storage t)


;; (define-key special-mode-map (kbd "q") 'bury-buffer)

(when (memq window-system '(ns mac))
  (setq twittering-curl-program "~/bin/curl-pac"))

(provide 'xwl-twittering)
;;; xwl-twittering.el ends here
