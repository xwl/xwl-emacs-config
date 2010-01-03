;; Author: Andreas Politz <politza@fh-trier.de>

;; Modified by: xwl
;; - (completion-read): Construct candicates table when `collection' is a
;;   function.

;;; Commentary

;; This file contains a couple of advices, mostly to make ido the
;; default `completing-read' function and some optimizations.

(require 'ido)

(defvar ido-hacks-orgin-completing-read-function (symbol-function 'completing-read))
(defvar ido-hacks-completing-read-recursive nil)
(defvar ido-hacks-flex-narrowed-matches-hash (make-hash-table :test 'equal))
;; Make compiler happy:
(defvar ido-directory-too-big nil)
(defvar ido-directory-nonreadable nil)
(defvar ido-directory-nonreadable nil)
(defvar ido-choice-list nil)
(defvar ido-temp-list nil)
(defvar ido-cur-item nil)

;; Use ido for `dired-do-rename', which is disabled in ido.el.  Also
;; make it possible to choose an empty string and such accepting direds
;; default.
(put 'dired-do-rename 'ido nil)
(put 'dired-do-rename 'ido-hacks-fix-default t)
(put 'dired-do-copy 'ido-hacks-fix-default t)
(put 'elp-instrument-package 'ido 'ignore)

(define-minor-mode ido-hacks-mode
  "Advices for ido-mode."
  nil nil nil :global t
  (if ido-hacks-mode
      (progn
	(ad-enable-regexp "^ido-hacks-")
	(global-set-key (kbd "M-x") 'ido-hacks-execute-extended-command))
    (global-set-key (kbd "M-x") 'execute-extended-command)
    (ad-disable-regexp "^ido-hacks-"))
  (ad-activate-regexp "^ido-hacks-"))

  
(defadvice completing-read (around ido-hacks-completing-read activate)
  "Advice `completing-read' to always use `ido-read-internal',
unless `this-command' has a (ido ignore) property or the
inherit-input-method argument is non-nil or the collection
argument is a function (which ido can't handle)."
  ;;(completing-read prompt collection &optional predicate require-match
  ;; initial-input hist def inherit-input-method)
  (if (or ido-hacks-completing-read-recursive
          ;; call from ido-read-internal
          (and (listp collection) (equal '("dummy" . 1) (car collection)))
          inherit-input-method
          (eq (get this-command 'ido) 'ignore))
      ad-do-it
    (when (functionp collection)          ; by xwl
      (setq collection (funcall collection "" nil t)))
    ;; copied from ido-completing-read        
    (let ((ido-hacks-completing-read-recursive t)
	  (ido-current-directory nil)
	  (ido-directory-nonreadable nil)
	  (ido-directory-too-big nil)
	  (ido-context-switch-command (or (get this-command 'ido-context-switch-command) 'ignore))
	  (ido-choice-list  (ido-hacks-make-completions collection predicate)))
      (setq ad-return-value
	    (ido-read-internal 'list prompt hist def require-match initial-input)))))

(defun ido-hacks-fix-default-hook ()
  (push "" ido-temp-list))

;; (defadvice ido-set-matches-1 (around ido-hacks-ido-set-matches-1)
;;   "Idos flex mechanism tends to be slow when confronted with lots of items.
;;  This advice makes it a good deal faster, by caching narrowed
;;  choices lists."
;;   ;; (defun ido-set-matches-1 (items &optional do-full)
;;   (cond
;; ;;    ((and (eq ido-cur-item 'list)
;; ;; 	 (equal ido-text ""))
;; ;;     (setq ad-return-value ido-choice-list))
;;    (t
;;     (cond
;;      ((not ido-enable-flex-matching)
;;       ad-do-it)
;;      (t
;;       (let (ido-enable-flex-matching) 
;; 	ad-do-it
;; 	(when  (and (null ad-return-value)
;; 		    (> (length ido-text) 1)
;; 		    (not ido-enable-regexp))
	
;; 	  (let* ((re (mapconcat #'regexp-quote (split-string ido-text "" t) ".*"))
;; 		 longest-prefix valid new-hash)
;; 	    (maphash
;; 	     #'(lambda (k v)
;; 		 (when (and (string-prefixp ido-text k)
;; 			    (> (length k) (length longest-prefix)))
;; 		   (setq longest-prefix k)
;; 		   (setq valid v)))
;; 	     ido-hacks-flex-narrowed-matches-hash)
	  
;; 	    (if ido-enable-prefix
;; 		(setq re (concat "\\`" re)))

;; 	    (if (and valid
;; 		     (not (eq ido-enable-prefix
;; 			      (cdr valid))))
;; 		(setq valid nil)
;; 	      (setq valid (car valid)))

;; 	    (setq new-hash (make-hash-table :test 'equal))
;; 	    ;; The order of the cached lists is not uptodate, can't
;; 	    ;; simply return them.
;; 	    (mapc
;; 	     #'(lambda (item)
;; 		 (let ((name (ido-name item)))
;; 		   (when (and (or (not valid)
;; 				(gethash name valid))
;; 			    (string-match re name))
;; 		       (puthash item t new-hash)
;; 		       (push item ad-return-value))))
;; 	     items)
	    
;; 	    (puthash ido-text
;; 		     (cons new-hash
;; 			   ido-enable-prefix) ;store current mode
;; 		     ido-hacks-flex-narrowed-matches-hash)))))))))

(defun string-prefixp (string prefix &optional ignore-case)
  "Return t if PREFIX is a prefix of STRING."
  (eq t
	(compare-strings string 0 (length prefix)
			 prefix 0 (length prefix)
			 ignore-case)))


(defun ido-hacks-make-completions (collection &optional predicate) ;funcs)
  (let ((completions
	 (all-completions "" collection predicate)))
    (if (or (hash-table-p collection)
    	    (arrayp collection))
	(ido-hacks-completions-sort completions)
      completions)))

(defun ido-hacks-completions-sort (completions)
  (sort completions
	#'(lambda (k1 k2)
	      (or (< (length k1) (length k2))
		  (and (= (length k1) (length k2))
		       (string< k1 k2))))))

(defun ido-hacks-execute-extended-command (&optional arg)
  (interactive "P")
  (let (old-message command)
    (let ((ido-enable-prefix nil))
      (setq command (intern
	 (completing-read
	  (concat
	   (cond
	    ((eq '- arg) "- ")
	    ((equal arg '(4)) "C-u ")
	    (arg (format "%d " (prefix-numeric-value arg))))
	   "M-x ")
	  obarray 'commandp t nil 'extended-command-history))))

    (call-interactively command)
    (when (and suggest-key-bindings
	       (not executing-kbd-macro)
	       (symbolp command))
      (let ((binding  (where-is-internal command overriding-local-map t))
	    (timeout (if (numberp suggest-key-bindings)
			 suggest-key-bindings
		       2))
	    binding-message waited)
	(when binding
	  (message "%s"
	   (concat "You can run the command "
		   (propertize (format "`%s'" (symbol-name command)) 'face 'font-lock-type-face)
		   "with <"
		   (propertize (ignore-errors
				 (substring (ido-hacks-get-keys (symbol-name command)) 1 -1))
			       'face 'font-lock-keyword-face)
		   ">")))))))

(defun ido-hacks-get-keys (func-name)
  "Return strings naming keys bound to `func-name', or nil if none.
Examines the prior, not current, buffer, presuming that current buffer
is minibuffer. (Stolen from icomplete.)"
  (if (commandp func-name)
      (save-excursion
	(let* ((sym (intern func-name))
	       (buf (other-buffer nil t))
	       (keys (with-current-buffer buf (where-is-internal sym))))
	  (if keys
	      (concat "<"
		      (mapconcat 'key-description
				 (sort keys
				       #'(lambda (x y)
					   (< (length x) (length y))))
				 ", ")
		      ">"))))))


;; (defun ido-completions (name candidates predicate require-match)
;;   ;; Return the string that is displayed after the user's text.
;;   ;; Modified from `icomplete-completions'.
;;   ;; Redefined for sake of performance by ido-hacks.
;;   (let* ((comps ido-matches)
;; 	 (ind (and (consp (car comps)) (> (length (cdr (car comps))) 1)
;; 		   ido-merged-indicator))
;; 	 first)

;;     (if (and ind ido-use-faces)
;; 	(put-text-property 0 1 'face 'ido-indicator ind))

;;     (if (and ido-use-faces comps)
;; 	(let* ((fn (ido-name (car comps)))
;; 	       (ln (length fn)))
;; 	  (setq first (format "%s" fn))
;; 	  (put-text-property 0 ln 'face
;; 			     (if (= (length comps) 1)
;;                                  (if ido-incomplete-regexp
;;                                      'ido-incomplete-regexp
;;                                    'ido-only-match)
;; 			       'ido-first-match)
;; 			     first)
;; 	  (if ind (setq first (concat first ind)))
;; 	  (setq comps (cons first (cdr comps)))))

;;     (cond ((null comps)
;; 	   (cond
;; 	    (ido-directory-nonreadable
;; 	     (or (nth 8 ido-decorations) " [Not readable]"))
;; 	    (ido-directory-too-big
;; 	     (or (nth 9 ido-decorations) " [Too big]"))
;; 	    (ido-report-no-match
;; 	     (nth 6 ido-decorations))  ;; [No match]
;; 	    (t "")))
;; 	  (ido-incomplete-regexp
;;            (concat " " (car comps)))
;; 	  ((null (cdr comps))		;one match
;; 	   (concat (if (if (not ido-enable-regexp)
;;                            (= (length (ido-name (car comps))) (length name))
;;                          ;; We can't rely on the length of the input
;;                          ;; for regexps, so explicitly check for a
;;                          ;; complete match
;;                          (string-match name (ido-name (car comps)))
;;                          (string-equal (match-string 0 (ido-name (car comps)))
;;                                        (ido-name (car comps))))
;;                        ""
;;                      ;; when there is one match, show the matching file name in full
;;                      (concat (nth 4 ido-decorations)  ;; [ ... ]
;;                              (ido-name (car comps))
;;                              (nth 5 ido-decorations)))
;; 		   (if (not ido-use-faces) (nth 7 ido-decorations))))  ;; [Matched]
;; 	  (t				;multiple matches
;; 	   (let ((items (if (> ido-max-prospects 0) (1+ ido-max-prospects) 999))
;; 		 alternatives)

;; 	     ;; ---------------------------
;; 	     (dotimes (i (min items (length comps)))
;; 	       (let* ((com (ido-name (nth i comps)))
;; 		      (str (copy-sequence com)))
;; 		 (if (= i (1- items))
;; 		     (push (nth 3 ido-decorations) alternatives)
;; 		   (push  (or ido-separator (nth 2 ido-decorations)) ; " | "
;; 			  alternatives)
;; 		   (if (and ido-use-faces
;; 			    (not (string= str first))
;; 			    (ido-final-slash str))
;; 		       (put-text-property 0 (length str) 'face 'ido-subdir str))
;; 		   (push str alternatives))))
;; 	     ;; ---------------------------
	     
;; 	     (concat
;; 	      ;; put in common completion item -- what you get by pressing tab
;; 	      (if (and (stringp ido-common-match-string)
;; 		       (> (length ido-common-match-string) (length name)))
;; 		  (concat (nth 4 ido-decorations) ;; [ ... ]
;; 			  (substring ido-common-match-string (length name))
;; 			  (nth 5 ido-decorations)))
;; 	      ;; list all alternatives
;; 	      (nth 0 ido-decorations) ;; { ... }
;; 	      (apply 'concat (cdr (nreverse alternatives)))
;; 	      (nth 1 ido-decorations)))))))


;; xwl

(defun ido-read-internal (item prompt history &optional default require-match initial)
  "Perform the `ido-read-buffer' and `ido-read-file-name' functions.
Return the name of a buffer or file selected.
PROMPT is the prompt to give to the user.
DEFAULT if given is the default item to start with.
If REQUIRE-MATCH is non-nil, an existing file must be selected.
If INITIAL is non-nil, it specifies the initial input string."
  (let
      ((ido-cur-item item)
       (ido-entry-buffer (current-buffer))
       (ido-process-ignore-lists t)
       (ido-process-ignore-lists-inhibit nil)
       (ido-set-default-item t)
       ido-default-item
       ido-selected
       ido-final-text
       (done nil)
       (icomplete-mode nil) ;; prevent icomplete starting up
       ;; Exported dynamic variables:
       ido-cur-list
       ido-ignored-list
       (ido-rotate-temp nil)
       (ido-keep-item-list nil)
       (ido-use-merged-list nil)
       (ido-try-merged-list t)
       (ido-pre-merge-state nil)
       (ido-case-fold ido-case-fold)
       (ido-enable-prefix ido-enable-prefix)
       (ido-enable-regexp ido-enable-regexp)
       (ido-show-confirm-message nil)
       )

    (ido-setup-completion-map)
    (setq ido-text-init initial)
    (setq ido-input-stack nil)

    (run-hooks 'ido-setup-hook)

    (while (not done)
      (ido-trace "\n_LOOP_" ido-text-init)
      (setq ido-exit nil)
      (setq ido-rescan t)
      (setq ido-rotate nil)
      (setq ido-text "")
      (when ido-set-default-item
	(setq ido-default-item
	      (cond
	       ((eq item 'buffer)
		(if (bufferp default) (buffer-name default) default))
	       ((stringp default)
		(if (memq item '(file dir))
		    (file-name-nondirectory default)
		  default))
	       ((eq item 'file)
		(and ido-enable-last-directory-history
		     (let ((d (assoc ido-current-directory ido-last-directory-list)))
		       (and d (cdr d)))))))
	(if (member ido-default-item ido-ignore-item-temp-list)
	    (setq ido-default-item nil))
	(ido-trace "new default" ido-default-item)
	(if ido-default-item
	    ;; (setq ido-initial-position 0))
            ;; (xwl) Set it to nil so that the cursor would not be placed at the
            ;; beginning when ido-hacks is enabled.  Why is this anyway? FIXME
            (setq ido-initial-position nil))
	(setq ido-set-default-item nil))

      (if ido-process-ignore-lists-inhibit
	  (setq ido-process-ignore-lists nil))

      (if (and ido-use-merged-list (memq ido-try-merged-list '(t wide)) (not ido-keep-item-list))
	  (let ((olist ido-cur-list)
		(oign ido-ignored-list)
		(omat ido-matches)
		(l (ido-make-merged-file-list ido-text-init
					      (eq ido-use-merged-list 'auto)
					      (eq ido-try-merged-list 'wide))))
	    (ido-trace "merged" l)
	    (cond
	     ((not l)
	      (if (eq ido-try-merged-list 'wide)
		  (setq ido-pre-merge-state
			(list "" ido-current-directory olist oign omat)
			ido-cur-list nil
			ido-ignored-list nil
			ido-matches nil
			ido-keep-item-list t
			ido-try-merged-list (if (eq ido-use-merged-list 'auto) 'auto nil)
			ido-use-merged-list nil)
		(setq ido-cur-list olist
		      ido-ignored-list oign
		      ido-matches omat
		      ido-keep-item-list t
		      ido-try-merged-list (if (eq ido-use-merged-list 'auto) 'auto nil)
		      ido-use-merged-list nil)))
	     ((eq l t)
	      (setq ido-use-merged-list nil))
	     ((eq l 'input-pending-p)
	      (setq ido-try-merged-list t
		    ido-use-merged-list nil))
	     (t
	      (setq ido-pre-merge-state
		    (list ido-text-init ido-current-directory olist oign omat))
	      (ido-set-current-directory (car (cdr (car l))))
	      (if (ido-final-slash ido-text-init)
		  (setq ido-text-init ""))
	      (setq ido-cur-list l
		    ido-ignored-list nil
		    ido-matches l
		    ido-rescan nil
		    ido-keep-item-list t
		    ido-use-merged-list t)
	      (ido-trace "Merged" t)
	      ))))

      (cond
       (ido-keep-item-list
	(setq ido-keep-item-list nil
	      ido-rescan nil))
       ((eq ido-cur-item 'file)
	(setq ido-ignored-list nil
	      ido-cur-list (and (not ido-directory-nonreadable)
				(not ido-directory-too-big)
				(ido-make-file-list ido-default-item))))
       ((eq ido-cur-item 'dir)
	(setq ido-ignored-list nil
	      ido-cur-list (and (not ido-directory-nonreadable)
				(not ido-directory-too-big)
				(ido-make-dir-list ido-default-item))))
       ((eq ido-cur-item 'buffer)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-buffer-list ido-default-item)))
       ((eq ido-cur-item 'list)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-choice-list ido-default-item)))
       (t nil))
      (setq ido-rotate-temp nil)

      (if ido-process-ignore-lists-inhibit
	  (setq ido-process-ignore-lists t
		ido-process-ignore-lists-inhibit nil))

      (ido-set-matches)
      (if (and ido-matches (eq ido-try-merged-list 'auto))
	  (setq ido-try-merged-list t))
      (let
	  ((minibuffer-local-completion-map
	    (if (memq ido-cur-item '(file dir))
		minibuffer-local-completion-map
	      ido-completion-map))
	   (minibuffer-local-filename-completion-map
	    (if (memq ido-cur-item '(file dir))
		ido-completion-map
	      minibuffer-local-filename-completion-map))
	   (max-mini-window-height (or ido-max-window-height
				       (and (boundp 'max-mini-window-height) max-mini-window-height)))
	   (ido-completing-read t)
	   (ido-require-match require-match)
	   (ido-use-mycompletion-depth (1+ (minibuffer-depth)))
	   (show-paren-mode nil))
	;; prompt the user for the file name
	(setq ido-exit nil)
	(setq ido-final-text
	      (catch 'ido
		(completing-read
		 (ido-make-prompt item prompt)
		 '(("dummy" . 1)) nil nil ; table predicate require-match
		 (prog1 ido-text-init (setq ido-text-init nil))	;initial-contents
		 history))))
      (ido-trace "completing-read" ido-final-text)
      (if (get-buffer ido-completion-buffer)
	  (kill-buffer ido-completion-buffer))

      (ido-trace "\n_EXIT_" ido-exit)

      (cond
       ((eq ido-exit 'refresh)
	(if (and (eq ido-use-merged-list 'auto)
		 (or (input-pending-p)))
	    (setq ido-use-merged-list nil
		  ido-keep-item-list t))
	nil)

       ((eq ido-exit 'done)
	(setq done t
	      ido-selected ido-text
	      ido-exit nil))

       ((memq ido-exit '(edit chdir))
	(cond
	 ((memq ido-cur-item '(file dir))
	  (let* ((read-file-name-function nil)
		 (edit (eq ido-exit 'edit))
		 (d ido-current-directory)
		 (f ido-text-init)
		 (new t))
	    (setq ido-text-init "")
	    (while new
	      (setq new (if edit
			    (condition-case nil
				(read-file-name (concat prompt "[EDIT] ")
						(expand-file-name d)
						(concat d f) nil f)
			      (quit (concat d f)))
			   f)
		    d (or (file-name-directory new) "/")
		    f (file-name-nondirectory new)
		    edit t)
	      (if (or
		   (file-directory-p d)
		   (and (yes-or-no-p (format "Create directory %s? " d))
			(condition-case nil
			    (progn (make-directory d t) t)
			  (error
			   (message "Could not create directory")
			   (sit-for 1)
			   nil))))
		  (progn
		    (ido-set-current-directory d nil (eq ido-exit 'chdir))
		    (setq ido-text-init f
			  new nil))))))
	 (t
	  (setq ido-text-init
		(condition-case nil
		    (read-string (concat prompt "[EDIT] ") ido-final-text)
		  (quit ido-final-text)))))

	nil)

       ((eq ido-exit 'keep)
	(setq ido-keep-item-list t))

       ((memq ido-exit '(dired fallback find-file switch-to-buffer insert-buffer insert-file))
	(setq done t))

       ((memq ido-exit '(updir push))
	;; cannot go up if already at the root-dir (Unix) or at the
	;; root-dir of a certain drive (Windows or MS-DOS).
        (if (ido-is-tramp-root)
	    (when (string-match "\\`\\(/\\([^/]+[:@]\\)*\\)\\([^/]+\\)[:@]\\'" ido-current-directory)
	      (setq ido-text-init (match-string 3 ido-current-directory))
	      (ido-set-current-directory (match-string 1 ido-current-directory))
	      (setq ido-set-default-item t))
	  (unless (ido-is-root-directory)
	    (when (eq ido-exit 'push)
	      (setq ido-input-stack (cons (cons ido-cur-item ido-text) ido-input-stack))
	      (setq ido-cur-item 'dir)
	      (setq ido-text-init (file-name-nondirectory (substring ido-current-directory 0 -1)))
	      (ido-trace "push" ido-input-stack))
	    (ido-set-current-directory (file-name-directory (substring ido-current-directory 0 -1)))
	    (setq ido-set-default-item t))))

       ((eq ido-exit 'pop)
	(ido-trace "pop" ido-input-stack)
	(let ((elt (car ido-input-stack)))
	  (setq ido-input-stack (cdr ido-input-stack))
	  (ido-set-current-directory (concat ido-current-directory ido-text))
	  (setq ido-cur-item (car elt))
	  (setq ido-text-init (cdr elt))))

       ((eq ido-exit 'pop-all)
	(ido-trace "pop-all" ido-input-stack)
	(while ido-input-stack
	  (let ((elt (car ido-input-stack)))
	    (setq ido-input-stack (cdr ido-input-stack))
	    (ido-set-current-directory (concat ido-current-directory ido-text))
	    (setq ido-cur-item (car elt))
	    (setq ido-text-init (cdr elt)))))

       ;; Handling the require-match must be done in a better way.
       ((and require-match
	     (not (memq require-match '(confirm confirm-after-completion)))
	     (not (if ido-directory-too-big
		      (file-exists-p (concat ido-current-directory ido-final-text))
		    (ido-existing-item-p))))
	(error "Must specify valid item"))

       (t
	(setq ido-selected
	      (if (or (eq ido-exit 'takeprompt)
		      (null ido-matches))
		  ido-final-text
		;; else take head of list
		(ido-name (car ido-matches))))

	(cond
	 ((memq item '(buffer list))
	  (setq done t))

	 ((string-equal "./" ido-selected)
	  nil)

	 ((string-equal "../" ido-selected)
	  ;; cannot go up if already at the root-dir (Unix) or at the
	  ;; root-dir of a certain drive (Windows or MS-DOS).
	  (or (ido-is-root-directory)
	      (ido-set-current-directory (file-name-directory (substring ido-current-directory 0 -1))))
	  (setq ido-set-default-item t))

	 ((and (string-match (if ido-enable-tramp-completion ".[:@]\\'" ".:\\'") ido-selected)
	       (ido-is-root-directory) ;; Ange-ftp or Tramp
	       (not (ido-local-file-exists-p ido-selected)))
	  (ido-set-current-directory ido-current-directory ido-selected)
	  (ido-trace "tramp prefix" ido-selected)
	  (if (ido-is-slow-ftp-host)
	      (setq ido-exit 'fallback
		    done t)
	    (setq ido-set-default-item t)))

	 ((or (string-match "[/\\][^/\\]" ido-selected)
	      (and (memq system-type '(windows-nt ms-dos))
		   (string-match "\\`[a-zA-Z]:" ido-selected)))
	  (ido-set-current-directory (file-name-directory ido-selected))
	  (setq ido-set-default-item t))

	 ((string-match "\\`~" ido-selected)
	  (ido-set-current-home ido-selected))

	 ((ido-final-slash ido-selected)
	  (if ido-enable-last-directory-history
	      (let ((x (assoc ido-current-directory ido-last-directory-list)))
		(if x
		    (setcdr x ido-selected)
		  (setq ido-last-directory-list
			(cons (cons ido-current-directory ido-selected) ido-last-directory-list)))))
	  (ido-set-current-directory ido-current-directory ido-selected)
	  (if ido-input-stack
	      ; automatically pop stack elements which match existing files or directories
	      (let (elt)
		(while (and (setq elt (car ido-input-stack))
			    (file-exists-p (concat ido-current-directory (cdr elt))))
		  (if (setq ido-input-stack (cdr ido-input-stack))
		      (ido-set-current-directory ido-current-directory (cdr elt))
		    (setq ido-text-init (cdr elt)))
		  (setq ido-cur-item (car elt))))
	    (setq ido-set-default-item t)))

	 (t
	  (setq done t))))))
    ido-selected))

(defadvice ido-read-internal (around ido-hacks-ido-read-internal)
  "This advice tries to fix idos handling of default values. When
a command has a (ido-hacks-fix-default t) property, it inserts an
empty string to the front of the choices list.

Additionally it inserts the complete selected item into the
history, instead of the incomplete input."
  ;;(defun ido-read-internal (item prompt history &optional default require-match initial)
  (let (history-add-new-input
	(hook  (intern (format "ido-make-%s-list-hook" item)))
	(fix-default (get this-command 'ido-hacks-fix-default)))
    
    (clrhash ido-hacks-flex-narrowed-matches-hash)
    (if (or fix-default
	    (and prompt
		 (or (not default)
		     (equal "" default))
		 (string-match "\\(.*\\)([ \t]*default[ \t]+\\(.*\\))\\([ \t]*:[ \t]*\\)\\'"
			       prompt)))
	(cond
	 ((eq item 'list)
	  (if fix-default
	      (push "" ido-choice-list)
	    (setq default (match-string 2 prompt))))
	 (t
	  (apply 'add-hook hook (list 'ido-hacks-fix-default-hook)))))
		 
    ad-do-it

    (apply 'remove-hook hook (list 'ido-hacks-fix-default-hook))
    (unless (eq history 'command-history)
      (setq history (if history
			(if (symbolp history)
			    history
			  (car history))
		      'minibuffer-history))
      (when (> (length ad-return-value) 0)
	(add-to-history history ad-return-value)))))


(provide 'ido-hacks)

;;; ido-hacks.el ends here
