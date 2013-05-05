;;; xwl-algorithm.el --- Collection of algorithms

;; Copyright (C) 2013  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: data, tools

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

;;;###autoload
(defun xwl-algorithm-longest-common-substring-length (str1 str2)
  "http://en.wikipedia.org/wiki/Longest_common_substring_problem"
  (let ((lcs (make-vector (* (length str1) (length str2)) 0))
        (len (length str2)))
    (dotimes (i (length str1))
      (dotimes (j (length str2))
        (when (and (equal (substring str1 i (+ i 1))
                          (substring str2 j (+ j 1)))
                   (> i 0)
                   (> j 0))
          (aset lcs
                (+ (* i len) j)
                (1+ (aref lcs (+ (* (1- i) len) (1- j))))))))
    (apply 'max (append lcs nil))))

(provide 'xwl-algorithm)
;;; xwl-algorithm.el ends here
