;;; xwl-gnus-agent.el --- gnus agent settings

;; Copyright (C) 2010 William Xu

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

;;; Commentary:

;; Load this only when running gnus agent.  We run gnus agent interactively,
;; such that when some error happens, we can notice it immediately, instead of
;; leaving lots of stalled process at the background.

;;; Code:

;; trunk temp fix
(setq package-enable-at-startup nil)

(require 'xwl-gnus)

(setq vc-follow-symlinks t)

(defun yes-or-no-p (p)
  t)

(defun y-or-n-p (p)
  t)

(suspend-frame)
(gnus-agent-batch)
(gnus-group-save-newsrc t)
(save-buffers-kill-terminal t)

(provide 'xwl-gnus-agent)

;;; xwl-gnus-agent.el ends here
