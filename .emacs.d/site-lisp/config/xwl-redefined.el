;;; xwl-redefined.el --- Redefined functions from GNU Emacs

;; Copyright (C) 2007, 2008, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; Commentary:

;; Maybe usually this should be evaled very late in .emacs, right? Or
;; explicitly require them before redefinition.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (require 'xwl-redefined)

;;; Code:

(require 'xwl-util)

;;; If woman fails, call man.

(eval-after-load "man"
  '(defun Man-highlight-references0 (start-section regexp button-pos target type)
     ;; Based on `Man-build-references-alist'
     (when (or (null start-section)
               (Man-find-section start-section))
       (let ((end (if start-section
                      (progn
                        (forward-line 1)
                        (back-to-indentation)
                        (save-excursion
                          (Man-next-section 1)
                          (point)))
                    (goto-char (point-min))
                    (point-max))))
         ;; bug here?
         (while (re-search-forward regexp nil t) ;end t)
           (make-text-button
            (match-beginning button-pos)
            (match-end button-pos)
            'type type
            'Man-target-string (cond
                                ((numberp target)
                                 (match-string target))
                                ((functionp target)
                                 target)
                                (t nil))))))))


;;; Gnus

;; description -> "", disposition -> attached, and use ido
(require 'mml)

(defun mml-minibuffer-read-type (name &optional default)
  (mailcap-parse-mimetypes)
  (let* ((default (or default
		      (mm-default-file-encoding name)
		      ;; Perhaps here we should check what the file
		      ;; looks like, and offer text/plain if it looks
		      ;; like text/plain.
		      "application/octet-stream"))
	 (string ;; (completing-read
          (ido-completing-read
		  (format "Content type (default %s): " default)
		  ;; (mapcar 'list )
                  (mailcap-mime-types))))
    (if (not (equal string ""))
	string
      default)))

(defun mml-attach-file (file &optional type description disposition)
  (interactive
   (let* ((file (mml-minibuffer-read-file "Attach file: "))
          (type (mml-minibuffer-read-type file)))
     (list file type nil "attachment")))
  (save-excursion
    (unless (message-in-body-p) (goto-char (point-max)))
    (mml-insert-empty-tag 'part
			  'type type
			  'filename file
			  'disposition (or disposition "attachment")
			  'description description)))


(eval-after-load "gnus-group"
  '(progn
     (defun gnus-group-make-web-group (&optional solid)
       "Create an ephemeral nnweb group.
If SOLID (the prefix), create a solid group."
       (interactive "P")
       (require 'nnweb)
       (let* ((group
               (if solid (gnus-read-group "Group name: ")
                 (message-unique-id)))
              (default-type (or (car gnus-group-web-type-history)
                                (symbol-name (caar nnweb-type-definition))))
              (type
               (gnus-string-or
                (ido-completing-read
                 (format "Search engine type (default %s): " default-type)
                 (mapcar (lambda (elem) (list (symbol-name (car elem))))
                         nnweb-type-definition)
                 nil t nil 'gnus-group-web-type-history)
                default-type))
              (search
               (read-string
                "Search string: "
                (cons (or (car gnus-group-web-search-history) "") 0)
                'gnus-group-web-search-history))
              (method
               `(nnweb ,group (nnweb-search ,search)
                       (nnweb-type ,(intern type))
                       (nnweb-ephemeral-p t))))
         (if solid
             (progn
               (gnus-pull 'nnweb-ephemeral-p method)
               (gnus-group-make-group group method))
           (gnus-group-read-ephemeral-group
            group method t
            (cons (current-buffer)
                  (if (eq major-mode 'gnus-summary-mode) 'summary 'group))))))
     ))


;;; ispell

(require 'ispell)
(defun ispell-process-line (string shift)
  "Sends STRING, a line of text, to ispell and processes the result.
This will modify the buffer for spelling errors.
Requires variables START and END to be defined in its lexical scope.
Returns the sum SHIFT due to changes in word replacements."
  ;;(declare special start end)
  (let (poss accept-list)
    (if (not (numberp shift))
	(setq shift 0))
    ;; send string to spell process and get input.
    (ispell-send-string string)
    (while (progn
	     (ispell-accept-output)
	     ;; Last item of output contains a blank line.
	     (not (string= "" (car ispell-filter)))))
    ;; parse all inputs from the stream one word at a time.
    ;; Place in FIFO order and remove the blank item.
    (setq ispell-filter (nreverse (cdr ispell-filter)))
    (while (and (not ispell-quit) ispell-filter)
      ;; get next word, accounting for accepted words and start shifts
      (setq poss (ispell-parse-output (car ispell-filter)
				      accept-list shift))
      (if (and poss (listp poss))	; spelling error occurred.
	  ;; Whenever we have misspellings, we can change
	  ;; the buffer.  Keep boundaries as markers.
	  ;; Markers can move with highlighting!  This destroys
	  ;; end of region markers line-end and ispell-region-end
	  (let ((word-start
		 (copy-marker (+ start ispell-offset (car (cdr poss)))))
		(word-len (length (car poss)))
		(line-end (copy-marker end))
		(line-start (copy-marker start))
		recheck-region replace)
	    (goto-char word-start)
	    ;; Adjust the horizontal scroll & point
	    (ispell-horiz-scroll)
	    (goto-char (+ word-len word-start))
	    (ispell-horiz-scroll)
	    (goto-char word-start)
	    (ispell-horiz-scroll)

	    ;; Alignment cannot be tracked and this error will occur when
	    ;; `query-replace' makes multiple corrections on the starting line.
	    (when (ispell-looking-at (car poss))
		;; This occurs due to filter pipe problems
                ;; (error "")
;; 		(error (concat "Ispell misalignment: word "
;; 			       "`%s' point %d; probably incompatible versions")
;; 		       (car poss) (marker-position word-start))
;;                 )
	    ;; ispell-cmd-loop can go recursive & change buffer
	    (if ispell-keep-choices-win
		(setq replace (ispell-command-loop
			       (car (cdr (cdr poss)))
			       (car (cdr (cdr (cdr poss))))
			       (car poss) (marker-position word-start)
			       (+ word-len (marker-position word-start))))
	      (save-window-excursion
		(setq replace (ispell-command-loop
			       (car (cdr (cdr poss)))
			       (car (cdr (cdr (cdr poss))))
			       (car poss) (marker-position word-start)
			       (+ word-len (marker-position word-start))))))

	    (goto-char word-start)
	    ;; Recheck when query replace edit changes misspelled word.
	    ;; Error in tex mode when a potential math mode change exists.
	    (if (and replace (listp replace) (= 2 (length replace)))
		(if (and (eq ispell-parser 'tex)
			 (string-match "[\\\\][]()[]\\|\\\\begin\\|\\$"
				       (regexp-quote string)))
		    (error
		     "Don't start query replace on a line with math characters"
		     )
		  (set-marker line-end (point))
		  (setq ispell-filter nil
			recheck-region t)))

	    ;; insert correction if needed
	    (cond
	     ((or (null replace)
		  (equal 0 replace))	; ACCEPT/INSERT
	      (if (equal 0 replace)	; BUFFER-LOCAL DICT ADD
		  (ispell-add-per-file-word-list (car poss)))
	      ;; do not recheck accepted word on this line
	      (setq accept-list (cons (car poss) accept-list)))
	     (t				; replacement word selected or entered
	      (delete-region (point) (+ word-len (point)))
	      (if (not (listp replace))
		  (progn
		    (ispell-insert-word replace) ; insert dictionary word
		    (ispell-send-replacement (car poss) replace)
		    (setq accept-list (cons replace accept-list)))
		(let ((replace-word (car replace)))
		  ;; Recheck hand entered replacement word
		  (insert replace-word)
		  (ispell-send-replacement (car poss) replace-word)
		  (if (car (cdr replace))
		      (save-window-excursion
			(delete-other-windows) ; to correctly show help.
			;; Assume case-replace &
			;; case-fold-search correct?
			(query-replace (car poss) (car replace) t)))
		  (goto-char word-start)
		  ;; do not recheck if already accepted
		  (if (member replace-word accept-list)
		      (setq accept-list (cons replace-word accept-list)
			    replace replace-word)
		    (let ((region-end (copy-marker ispell-region-end)))
		      (setq recheck-region ispell-filter
			    ispell-filter nil ; save filter
			    shift 0	; already accounted
			    shift (ispell-region
				  word-start
				  (+ word-start (length replace-word))
				  t shift))
		      (if (null shift)	; quitting check.
			  (setq shift 0))
		      (set-marker ispell-region-end region-end)
		      (set-marker region-end nil)
		      (setq ispell-filter recheck-region
			    recheck-region nil
			    replace replace-word)))))

	      (setq shift (+ shift (- (length replace) word-len)))

	      ;; Move line-start across word...
	      ;; new shift function does this now...
	      ;;(set-marker line-start (+ line-start
	      ;;			(- (length replace)
	      ;;			   (length (car poss)))))
	      ))
	    (if (not ispell-quit)
		(let (message-log-max)
		  (message "Continuing spelling check using %s with %s dictionary..."
			   (file-name-nondirectory ispell-program-name)
			   (or ispell-current-dictionary "default"))))
	    (sit-for 0)
	    (setq start (marker-position line-start)
		  end (marker-position line-end))
	    ;; Adjust markers when end of region lost from highlighting.
	    (if (and (not recheck-region) (< end (+ word-start word-len)))
		(setq end (+ word-start word-len)))
	    (if (= word-start ispell-region-end)
		(set-marker ispell-region-end (+ word-start word-len)))
	    ;; going out of scope - unneeded
	    (set-marker line-start nil)
	    (set-marker word-start nil)
	    (set-marker line-end nil))))
      ;; finished with misspelling!
      (setq ispell-filter (cdr ispell-filter)))
    shift))

;;; (require 'perldoc)
;;; (defun perldoc (string)
;;;   "Run perldoc on the given STRING.
;;; If the string is a recognised function then we can call `perldoc-function',
;;; otherwise we call `perldoc-module'."
;;;   (interactive (list (ido-completing-read "Perl function or module: "
;;;                                           (mapcar (lambda (el) (car el))
;;;                                                   (perldoc-functions-alist))
;;;                                           nil nil)))
;;;   (perldoc-functions-alist)
;;;   (cond
;;;    ((assoc string perldoc-functions-alist)
;;;     (perldoc-function string))
;;;    ((stringp string)
;;;     (perldoc-module string))
;;;    (t
;;;     (message "Nothing to find."))))


(ignore-errors
  (progn

  (load "auctex.el" nil t t)

;; (eval-after-load "latex"
;; '(progn
(require 'latex)
(defun LaTeX-environment (arg)
  "Make LaTeX environment (\\begin{...}-\\end{...} pair).
With optional ARG, modify current environment.

It may be customized with the following variables:

`LaTeX-default-environment'       Your favorite environment.
`LaTeX-default-style'             Your favorite document class.
`LaTeX-default-options'           Your favorite document class options.
`LaTeX-float'                     Where you want figures and tables to float.
`LaTeX-table-label'               Your prefix to labels in tables.
`LaTeX-figure-label'              Your prefix to labels in figures.
`LaTeX-default-format'            Format for array and tabular.
`LaTeX-default-width'             Width for minipage and tabular*.
`LaTeX-default-position'          Position for array and tabular."

  (interactive "*P")
  (let ((environment (ido-completing-read (concat "Environment type: (default "
                                                  (if (TeX-near-bobp)
                                                      "document"
                                                    LaTeX-default-environment)
                                                  ") ")
                                          (mapcar (lambda (i) (car i))
                                                  (LaTeX-environment-list))
                                          nil nil nil
                                          'LaTeX-environment-history)))
    ;; Get default
    (cond ((and (zerop (length environment))
                (TeX-near-bobp))
           (setq environment "document"))
          ((zerop (length environment))
           (setq environment LaTeX-default-environment))
          (t
           (setq LaTeX-default-environment environment)))

    (let ((entry (assoc environment (LaTeX-environment-list))))
      (if (null entry)
          (LaTeX-add-environments (list environment)))

      (if arg
          (LaTeX-modify-environment environment)
        (LaTeX-environment-menu environment)))))


(defun LaTeX-section (arg)
  "Insert a template for a LaTeX section.
Determine the type of section to be inserted, by the argument ARG.

If ARG is nil or missing, use the current level.
If ARG is a list (selected by \\[universal-argument]), go downward one level.
If ARG is negative, go up that many levels.
If ARG is positive or zero, use absolute level:

  0 : part
  1 : chapter
  2 : section
  3 : subsection
  4 : subsubsection
  5 : paragraph
  6 : subparagraph

The following variables can be set to customize:

`LaTeX-section-hook'	Hooks to run when inserting a section.
`LaTeX-section-label'	Prefix to all section labels."

  (interactive "*P")
  (let* ((val (prefix-numeric-value arg))
         (level (cond ((null arg)
                       (LaTeX-current-section))
                      ((listp arg)
                       (LaTeX-down-section))
                      ((< val 0)
                       (LaTeX-up-section (- val)))
                      (t val)))
         (name (LaTeX-section-name level))
         (toc nil)
         (title (if (TeX-active-mark)
                    (buffer-substring (region-beginning)
                                      (region-end))
                  ""))
         (done-mark (make-marker)))
    (run-hooks 'LaTeX-section-hook)
    (LaTeX-newline)
    (if (marker-position done-mark)
        (goto-char (marker-position done-mark)))
    (set-marker done-mark nil)))

(defun LaTeX-section-heading ()
  "Hook to prompt for LaTeX section name.
Insert this hook into `LaTeX-section-hook' to allow the user to change
the name of the sectioning command inserted with `\\[LaTeX-section]'."
  (let ((string (ido-completing-read
                 (concat "Level: (default " name ") ")
                 (mapcar (lambda (i) (car i))
                         LaTeX-section-list)
                 nil nil nil)))
                                        ; Update name
    (if (not (zerop (length string)))
        (setq name string))
                                        ; Update level
    (setq level (LaTeX-section-level name))))

  ))

(eval-after-load "sql"
'(progn
   (defun sql-product-interactive (&optional product)
     "Run product interpreter as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
     (interactive)
     (setq product (or product sql-product))
     (when (sql-product-feature :sqli-connect product)
       (if (comint-check-proc "*SQL*")
           ;; (pop-to-buffer "*SQL*")
           (switch-to-buffer "*SQL*")   ;xwl
         ;; Get credentials.
         (apply 'sql-get-login (sql-product-feature :sqli-login product))
         ;; Connect to database.
         (message "Login...")
         (funcall (sql-product-feature :sqli-connect product))
         ;; Set SQLi mode.
         (setq sql-interactive-product product)
         (setq sql-buffer (current-buffer))
         (sql-interactive-mode)
         ;; All done.
         (message "Login...done")
         ;; (pop-to-buffer "*SQL*")
         (switch-to-buffer sql-buffer)  ;xwl
         )))
))


(when (eq system-type 'windows-nt)

  (eval-after-load 'message
    '(progn

       ;; FIXME; replace `call-process-region' with `shell-command-on-region' on
       ;; w32, or it won't work!!
       (defun message-send-mail-with-sendmail ()
         "Send off the prepared buffer with sendmail."
         (let ((errbuf (if message-interactive
                           (message-generate-new-buffer-clone-locals
                            " sendmail errors")
                         0))
               resend-to-addresses delimline to)
           (unwind-protect
               (progn
                 (let ((case-fold-search t))
                   (save-restriction
                     (message-narrow-to-headers)
                     (setq resend-to-addresses (message-fetch-field "resent-to")
                           to (message-fetch-field "to")))
                   ;; Change header-delimiter to be what sendmail expects.
                   (goto-char (point-min))
                   (re-search-forward
                    (concat "^" (regexp-quote mail-header-separator) "\n"))
                   (replace-match "\n")
                   (backward-char 1)
                   (setq delimline (point-marker))
                   (run-hooks 'message-send-mail-hook)
                   ;; Insert an extra newline if we need it to work around
                   ;; Sun's bug that swallows newlines.
                   (goto-char (1+ delimline))
                   (when (eval message-mailer-swallows-blank-line)
                     (newline))
                   (when message-interactive
                     (save-excursion
                       (set-buffer errbuf)
                       (erase-buffer))))
                 (let* ((default-directory "/")
                        (coding-system-for-write message-send-coding-system)
                        (cpr

                         ;;                       (apply
                         ;;                        'call-process-region
                         ;;                        (append
                         ;;                         (list (point-min) (point-max)
                         ;;                               (cond ((boundp 'sendmail-program)
                         ;;                                      sendmail-program)
                         ;;                                     ((file-exists-p "/usr/sbin/sendmail")
                         ;;                                      "/usr/sbin/sendmail")
                         ;;                                     ((file-exists-p "/usr/lib/sendmail")
                         ;;                                      "/usr/lib/sendmail")
                         ;;                                     ((file-exists-p "/usr/ucblib/sendmail")
                         ;;                                      "/usr/ucblib/sendmail")
                         ;;                                     (t "fakemail"))
                         ;;                               nil errbuf nil "-oi")
                         ;;                         ;; Always specify who from,
                         ;;                         ;; since some systems have broken sendmails.
                         ;;                         ;; But some systems are more broken with -f, so
                         ;;                         ;; we'll let users override this.
                         ;;                         (if (null message-sendmail-f-is-evil)
                         ;;                             (list "-f" (message-sendmail-envelope-from)))
                         ;;                         ;; These mean "report errors by mail"
                         ;;                         ;; and "deliver in background".
                         ;;                         (if (null message-interactive) '("-oem" "-odb"))
                         ;;                         ;; Get the addresses from the message
                         ;;                         ;; unless this is a resend.
                         ;;                         ;; We must not do that for a resend
                         ;;                         ;; because we would find the original addresses.
                         ;;                         ;; For a resend, include the specific addresses.
                         ;;                         (if resend-to-addresses
                         ;;                             (list resend-to-addresses)
                         ;;                           '("-t"))))

                         (shell-command-on-region (point-min)
                                                  (point-max)
                                                  (format "msmtp %s --file=c:/cygwin/home/william/.msmtprc" to))
                         ))
                   (unless (or (null cpr) (and (numberp cpr) (zerop cpr)))
                     (error "Sending...failed with exit value %d" cpr)))
                 (when message-interactive
                   (save-excursion
                     (set-buffer errbuf)
                     (goto-char (point-min))
                     (while (re-search-forward "\n\n* *" nil t)
                       (replace-match "; "))
                     (if (not (zerop (buffer-size)))
                         (error "Sending...failed to %s"
                                (buffer-string))))))
             (when (bufferp errbuf)
               (kill-buffer errbuf)))))

       )))

(eval-after-load 'ede-emacs
  '(progn
     (defadvice ede-emacs-version (around redefined activate)
       (let ((buff (get-buffer-create " *emacs-query*")))
         (save-excursion
           (set-buffer buff)
           (erase-buffer)
           (setq default-directory (file-name-as-directory dir))
           (call-process "egrep" nil buff nil "-n" "-e" "^version=" "Makefile")
           (goto-char (point-min))
           (prog1
               (if (re-search-forward "version=\\([0-9.]+\\)" nil t 1)
                   (match-string 1)
                 emacs-version)
             (kill-buffer buff)))))
     ))


(eval-after-load 'bbdb-com
  '(progn

     (defun bbdb-read-new-record ()     ;(around revise-prompts activate)
       (bbdb-records)                   ; make sure database is loaded
       (if bbdb-readonly-p
           (error "The Insidious Big Brother Database is read-only."))
       (let (firstname lastname)
         (bbdb-error-retry
          (progn
            (if current-prefix-arg
                (setq firstname (bbdb-read-string "First Name: ")
                      lastname (bbdb-read-string "Last Name: "))
              (let ((names (bbdb-divide-name (bbdb-read-string "Name: "))))
                (setq firstname (car names)
                      lastname (nth 1 names))))
            (if (string= firstname "") (setq firstname nil))
            (if (string= lastname "") (setq lastname nil))
            (if (and bbdb-no-duplicates-p
                     (bbdb-gethash (bbdb-build-name firstname lastname)))
                (error "%s %s is already in the database"
                       (or firstname "") (or lastname "")))))
         (let ((company (bbdb-read-string "Company: "))
               (net (bbdb-split (bbdb-read-string "Email: ") ","))
               ;; (addrs
               ;;  (let (L L-tail str addr)
               ;;    (while (not (string=
               ;;                 ""
               ;;                 (setq str
               ;;                       (bbdb-read-string
               ;;                        "Address Description [RET when no more]: "
               ;;                        ""
               ;;                        (mapcar (function (lambda(x) (list x)))
               ;;                                (bbdb-label-completion-list
               ;;                                 "addresses"))))))
               ;;      (setq addr (make-vector bbdb-address-length nil))
               ;;      (bbdb-record-edit-address addr str)
               ;;      (if L
               ;;          (progn (setcdr L-tail (cons addr nil))
               ;;                 (setq L-tail (cdr L-tail)))
               ;;        (setq L (cons addr nil)
               ;;              L-tail L)))
               ;;    L))
               ;; (phones
               ;;  (let (L L-tail str)
               ;;    (while (not (string=
               ;;                 ""
               ;;                 (setq str
               ;;                       (bbdb-read-string
               ;;                        "Phone Location [RET when no more]: "
               ;;                        ""
               ;;                        (mapcar (function (lambda(x) (list x)))
               ;;                                (bbdb-label-completion-list
               ;;                                 "phones"))))))
               ;;      (let* ((phonelist
               ;;              (bbdb-error-retry
               ;;               (bbdb-parse-phone-number
               ;;                (read-string "Phone: "
               ;;                             (and (integerp bbdb-default-area-code)
               ;;                                  (format "(%03d) "
               ;;                                          bbdb-default-area-code))))))
               ;;             (phone (apply 'vector str
               ;;                           (if (= 3 (length phonelist))
               ;;                               (nconc phonelist '(0))
               ;;                             phonelist))))
               ;;        (if L
               ;;            (progn (setcdr L-tail (cons phone nil))
               ;;                   (setq L-tail (cdr L-tail)))
               ;;          (setq L (cons phone nil)
               ;;                L-tail L))))
               ;;    L))
               ;; (notes (bbdb-read-string "Additional Comments: "))
               )
           (if (string= company "") (setq company nil))
           ;; (if (string= notes "") (setq notes nil))
           (let ((record
                  ;; (vector firstname lastname nil company phones addrs net notes
                  ;;         (make-vector bbdb-cache-length nil))))
                  (vector firstname lastname nil company nil nil net nil
                          (make-vector bbdb-cache-length nil))))

             record))))
     ))


(provide 'xwl-redefined)

;;; xwl-redefined.el ends here
