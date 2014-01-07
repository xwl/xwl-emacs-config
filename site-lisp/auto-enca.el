;;; auto-enca.el --- automatically detect coding system with Enca

;; This code is public domain. Use it as is, at your own risk.
;; I tried it with Emacs 22, and it seems that it gives quite
;; satisfying results, but YMMV.
;;
;; The following function, enca-detect-coding, is meant to be placed at
;; file-coding-system-alist in order to detect coding system of files
;; when they are opened. This happens by running Enca on the file.
;;
;; Enca (Extremely Naive Charset Analyser) can be found at the address
;; http://trific.ath.cx/Ftp/enca/
;;
;; To get this function working, put the file at some place in your
;; load-path, compile, and add something like this into .emacs:
;;
;; (when (load "auto-enca" 'noerror)
;;   (modify-coding-system-alist 'file "" 'enca-detect-coding))
;;
;; With thanks to Kenichi Handa for nice suggestions.
;;
;; --
;; Dmitriyi Paduchikh

;;; Code:

;;;###autoload
(defun enca-detect-coding (arg)
  (if (not (and (eq (car arg) 'insert-file-contents) (nth 1 arg)))
      'undecided
    (let* ((target (nth 1 arg))
	  (enca-buffer (generate-new-buffer " *enca*"))
	  (charset nil)
	  (run-enca (if (atom target)
			(lambda ()
			  (if (file-exists-p target)
			      (call-process "enca" target
					    enca-buffer nil "-m")
			    1))
		      (lambda ()
			(with-current-buffer (cdr target)
			  (let ((coding-system-for-write 'no-conversion)
				(size (min 20480 (- (point-max)
						    (point-min)))))
			    (call-process-region
			     (point-min) (+ (point-min) size)
			     "enca" nil enca-buffer nil "-m")))))))
      (unwind-protect
	  (when (= 0 (funcall run-enca))
	    (with-current-buffer enca-buffer
	      (goto-char 1)
	      (downcase-region 1 (point-max))
	      (skip-chars-forward "\t\n\f\r ")
	      (delete-region 1 (point))
	      (when (looking-at "\\(ibm\\)[0-9]")
		(replace-match "cp" t t nil 1))
	      (skip-chars-forward "^\t\n\f\r ")
	      (setq charset (intern-soft (buffer-substring 1 (point))))))
	(kill-buffer enca-buffer))
      (or (and (not (memq charset '(nil unknown us-ascii)))
	       (coding-system-p charset)
	       charset)
	  'undecided))))

(provide 'auto-enca)
