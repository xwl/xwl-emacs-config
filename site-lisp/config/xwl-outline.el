;;; xwl-outline.el --- outline mode config

;; Copyright (C) 2007, 2008, 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Last updated: 2010/12/04

(require 'outline)

(defadvice outline-mode (after hide-sublevels)
  "Enter overview after start up `outline-mode'."
  (hide-sublevels 1))

(defadvice outline-minor-mode (after hide-sublevels)
  "Enter overview after start up `outline-minor-mode'."
  (hide-sublevels 2))

(setq outline-font-lock-keywords
      '((eval list
              (concat "^\\(?:" outline-regexp "\\).+")
              0
              '(outline-font-lock-face)
              nil t)))

;; outline extra
(require 'foldout)

;; keys
(defun xwl-hide-body ()
  "Make `hide-body' take effects at any moment."
  (interactive)
  (show-all)
  (hide-body))

(defun xwl-outline-invisible-p ()
  "Are we inside a outline fold?"
  (interactive)
  (let ((overlays (overlays-at (line-end-position))))
    (and overlays
	 (eq (overlay-get (car overlays) 'invisible)
	     'outline))))

(defun xwl-foldout-exit-fold ()
  "Goto current folded line."
  (interactive)
  (call-interactively 'foldout-exit-fold) ; FIX ME
  (previous-line 1)
  (next-line 1))

(defun xwl-outline-toggle-enter-exit ()
  "Toggle entering and exiting fold."
  (interactive)
  (if (xwl-outline-invisible-p)
      (foldout-zoom-subtree)
    (xwl-foldout-exit-fold)))

(defun xwl-outline-toggle-show-hide ()
  "Toggle showing or hiding contents."
  (interactive)
  (if (xwl-outline-invisible-p)
      (show-subtree)
    (hide-subtree)))

(define-key outline-minor-mode-map (kbd "C-c C-i") 'hide-sublevels)
(define-key outline-minor-mode-map (kbd "C-c C-c") 'xwl-hide-body)
(define-key outline-minor-mode-map (kbd "C-c C-u") 'xwl-outline-toggle-enter-exit)
(define-key outline-minor-mode-map (kbd "C-c C-q") 'xwl-outline-toggle-show-hide)
(define-key outline-minor-mode-map (kbd "C-c C-n") (kbd "C-c @ C-n"))
(define-key outline-minor-mode-map (kbd "C-c C-p") (kbd "C-c @ C-p"))
(define-key outline-minor-mode-map (kbd "C-c C-a") (kbd "C-c @ C-a"))

(define-key outline-mode-map (kbd "C-c C-i") 'hide-sublevels)
(define-key outline-mode-map (kbd "C-c M-%") 'hide-sublevels)
(define-key outline-mode-map (kbd "C-c C-c") 'xwl-hide-body)
(define-key outline-mode-map (kbd "C-c C-u") 'xwl-outline-toggle-enter-exit)
(define-key outline-mode-map (kbd "C-c C-q") 'xwl-outline-toggle-show-hide)
(define-key outline-mode-map (kbd "C-c C-a") 'show-all)

(defun xwl-narrow-to-outline-level ()
  "Narrow to current outline level."
  (interactive)
  (save-excursion
    ;; (call-interactively 'outline-next-visible-heading)
    ;; (let ((end (point)))
    ;;   (call-interactively 'outline-previous-visible-heading)
    ;;   (narrow-to-region (point) end))

    (hide-subtree)
    (narrow-to-region (progn
                        (move-beginning-of-line 1)
                        (point))
                      (progn
                        (call-interactively 'outline-next-visible-heading)
                        (point)))
    (show-subtree)))


(global-set-key (kbd "C-x n o") 'xwl-narrow-to-outline-level)

(provide 'xwl-outline)

;;; xwl-outline.el ends here
