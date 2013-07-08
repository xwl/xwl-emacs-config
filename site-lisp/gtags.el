;;; gtags.el --- gtags facility for Emacs

;;
;; Copyright (c) 1997, 1998, 1999, 2000, 2006, 2007, 2008, 2009, 2010
;;		2011, 2012, 2013
;;	Tama Communications Corporation
;;
;; This file is part of GNU GLOBAL.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;; GLOBAL home page is at: http://www.gnu.org/software/global/
;; Author: Tama Communications Corporation
;; Version: 3.2
;; Keywords: tools
;; Required version: GLOBAL 5.9.7 or later

;; Gtags-mode is implemented as a minor mode so that it can work with any
;; other major modes. Gtags-select mode is implemented as a major mode.
;;
;; Please copy this file into emacs lisp library directory or place it in
;; a directory (for example "~/.emacs.d") and write $HOME/.emacs like this.
;;
;;	(add-to-list 'load-path "~/.emacs.d")
;;	(autoload 'gtags-mode "gtags" "" t)
;;
;; If you hope gtags-mode is on in c-mode then please add c-mode-hook to your
;; $HOME/.emacs like this.
;;
;; (add-hook 'c-mode-hook
;;    '(lambda ()
;;       (gtags-mode 1)
;; ))
;;
;; There are two hooks, gtags-mode-hook and gtags-select-mode-hook.
;; The usage of the hook is shown as follows.
;;
;; [Setting to use vi style scroll key]
;;
;; (add-hook 'gtags-mode-hook
;;   '(lambda ()
;;      (define-key gtags-mode-map "\C-f" 'scroll-up)
;;      (define-key gtags-mode-map "\C-b" 'scroll-down)
;; ))
;;
;; [Setting to make 'Gtags select mode' easy to see]
;;
;; (add-hook 'gtags-select-mode-hook
;;   '(lambda ()
;;      (setq hl-line-face 'underline)
;;      (hl-line-mode 1)
;; ))
;;
;; (Policy of key mapping)
;; If 'gtags-suggested-key-mapping' is not set, any key mapping is not done.
;; If 'gtags-disable-pushy-mouse-mapping' is set, any mouse mapping is not done.
;;

;;; Code

(require 'thingatpt)

(defvar gtags-mode nil
  "Non-nil if Gtags mode is enabled.")
(make-variable-buffer-local 'gtags-mode)

;;;
;;; Customizing gtags-mode
;;;
(defgroup gtags nil
  "Minor mode for GLOBAL source code tag system."
  :group 'tools
  :prefix "gtags-")

(defcustom gtags-path-style 'root
  "*Controls the style of path in [GTAGS SELECT MODE]."
  :type '(choice (const :tag "Relative from the root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute" absolute))
  :group 'gtags)

(defcustom gtags-read-only nil
  "Gtags read only mode"
  :type 'boolean
  :group 'gtags)

(defcustom gtags-pop-delete nil
  "*If non-nil, gtags-pop will delete the buffer."
  :group 'gtags
  :type 'boolean)

(defcustom gtags-select-buffer-single nil
  "*If non-nil, gtags select buffer is single."
  :group 'gtags
  :type 'boolean)

(defcustom gtags-disable-pushy-mouse-mapping nil
  "*If non-nil, mouse key mapping is disabled."
  :group 'gtags
  :type 'boolean)

(defcustom gtags-suggested-key-mapping nil
  "*If non-nil, suggested key mapping is enabled."
  :group 'gtags
  :type 'boolean)

(defcustom gtags-use-old-key-map nil
  "*If non-nil, old key mapping is enabled."
  :group 'gtags
  :type 'boolean)

;; Variables
(defvar gtags-current-buffer nil
  "Current buffer.")
(defvar gtags-buffer-stack nil
  "Stack for tag browsing.")
(defvar gtags-point-stack nil
  "Stack for tag browsing.")
(defvar gtags-history-list nil
  "Gtags history list.")
(defconst gtags-symbol-regexp  "[A-Za-z_\"][A-Za-z_0-9.\"><]*"
  "Regexp matching tag name.")
(defconst gtags-definition-regexp "#[ \t]*define[ \t]+\\|ENTRY(\\|ALTENTRY("
  "Regexp matching tag definition name.")
(defvar gtags-mode-map (make-sparse-keymap)
  "Keymap used in gtags mode.")
(defvar gtags-select-mode-map (make-sparse-keymap)
  "Keymap used in gtags select mode.")
(defvar gtags-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Whether we are running XEmacs/Lucid Emacs")
(defvar gtags-rootdir nil
  "Root directory of source tree.")

;; Key mapping of gtags-mode.
(if gtags-suggested-key-mapping
    (progn
      ; Current key mapping.
      (define-key gtags-mode-map "\C-ch" 'gtags-display-browser)
      (define-key gtags-mode-map "\C-]" 'gtags-find-tag-from-here)
      (define-key gtags-mode-map "\C-t" 'gtags-pop-stack)
      (define-key gtags-mode-map "\C-cP" 'gtags-find-file)
      (define-key gtags-mode-map "\C-cf" 'gtags-parse-file)
      (define-key gtags-mode-map "\C-cg" 'gtags-find-with-grep)
      (define-key gtags-mode-map "\C-cI" 'gtags-find-with-idutils)
      (define-key gtags-mode-map "\C-cs" 'gtags-find-symbol)
      (define-key gtags-mode-map "\C-cr" 'gtags-find-rtag)
      (define-key gtags-mode-map "\C-ct" 'gtags-find-tag)
      (define-key gtags-mode-map "\C-cv" 'gtags-visit-rootdir)
      ; common
      (define-key gtags-mode-map "\e*" 'gtags-pop-stack)
      (define-key gtags-mode-map "\e." 'gtags-find-tag)
      (define-key gtags-mode-map "\C-x4." 'gtags-find-tag-other-window)
      (if gtags-disable-pushy-mouse-mapping nil
        (define-key gtags-mode-map [mouse-3] 'gtags-pop-stack)
        (define-key gtags-mode-map [mouse-2] 'gtags-find-tag-by-event)))
)
;; Key mapping of old gtags-mode (obsoleted)
(if (and gtags-suggested-key-mapping gtags-use-old-key-map)
    (progn
      ; Old key mapping
      (define-key gtags-mode-map "\eh" 'gtags-display-browser)
      (define-key gtags-mode-map "\C-]" 'gtags-find-tag-from-here)
      (define-key gtags-mode-map "\C-t" 'gtags-pop-stack)
      (define-key gtags-mode-map "\eP" 'gtags-find-file)
      (define-key gtags-mode-map "\ef" 'gtags-parse-file)
      (define-key gtags-mode-map "\eg" 'gtags-find-with-grep)
      (define-key gtags-mode-map "\eI" 'gtags-find-with-idutils)
      (define-key gtags-mode-map "\es" 'gtags-find-symbol)
      (define-key gtags-mode-map "\er" 'gtags-find-rtag)
      (define-key gtags-mode-map "\et" 'gtags-find-tag)
      (define-key gtags-mode-map "\ev" 'gtags-visit-rootdir)
      ; common
      (define-key gtags-mode-map "\e*" 'gtags-pop-stack)
      (define-key gtags-mode-map "\e." 'gtags-find-tag)
      (define-key gtags-mode-map "\C-x4." 'gtags-find-tag-other-window)
      (if gtags-disable-pushy-mouse-mapping nil
        (define-key gtags-mode-map [mouse-3] 'gtags-pop-stack)
        (define-key gtags-mode-map [mouse-2] 'gtags-find-tag-by-event)))
)

;; Key mapping of gtags-select-mode.
(if gtags-suggested-key-mapping
    (progn
      (define-key gtags-select-mode-map "\e*" 'gtags-pop-stack)
      (define-key gtags-select-mode-map "\^?" 'scroll-down)
      (define-key gtags-select-mode-map " " 'scroll-up)
      (define-key gtags-select-mode-map "\C-b" 'scroll-down)
      (define-key gtags-select-mode-map "\C-f" 'scroll-up)
      (define-key gtags-select-mode-map "k" 'previous-line)
      (define-key gtags-select-mode-map "j" 'next-line)
      (define-key gtags-select-mode-map "p" 'previous-line)
      (define-key gtags-select-mode-map "n" 'next-line)
      (define-key gtags-select-mode-map "q" 'gtags-pop-stack)
      (define-key gtags-select-mode-map "u" 'gtags-pop-stack)
      (define-key gtags-select-mode-map "\C-t" 'gtags-pop-stack)
      (define-key gtags-select-mode-map "\C-m" 'gtags-select-tag)
      (define-key gtags-select-mode-map "\C-o" 'gtags-select-tag-other-window)
      (define-key gtags-select-mode-map "\e." 'gtags-select-tag)
      (if gtags-disable-pushy-mouse-mapping nil
        (define-key gtags-select-mode-map [mouse-3] 'gtags-pop-stack)
        (define-key gtags-select-mode-map [mouse-2] 'gtags-select-tag-by-event)))
)
;; This is only one exception of the policy.

;;
;; utility
;;
(defun gtags-match-string (n)
  (buffer-substring (match-beginning n) (match-end n)))

(defun gtags-parse-token (token &optional dot-at-back)
  "Return (TYPE . VALUE) where TYPE could be 'filename or 'tagname.  "
  (if (and (equal (substring token 0 1) "\"")
           (equal (substring token -1) "\""))
      `(filename . ,(substring token 1 -1))
    (setq token (split-string token "[\.\"]" t))
    (if dot-at-back
        `(tagname . ,(cadr token))
      `(tagname . ,(car token)))))

(defun gtags-current-token ()
  (let ((allowed "[0-9A-Za-z_.\"><]")
        (spaces "[ \t]")
        dot-at-back)          ; where is the dot compared with current position.
    (save-excursion
      ;; Goto start or end of token.
      (unless (looking-at allowed)
        (skip-chars-forward spaces (line-end-position))
        (when (eolp)
          (skip-chars-backward spaces (line-beginning-position))
          (unless (bolp)
            (backward-char))))

      ;; Goto start of token and decide if dot is before or after point.
      (when (looking-at allowed)
        (while (and (not (bolp)) (looking-back allowed))
          (when (eq (char-before) ?.)
            (setq dot-at-back t))
          (backward-char)))

      ;; FIXME
      (when (and (bolp) (looking-at gtags-definition-regexp))
        (goto-char (match-end 0)))

      ;; Extract filename or tagname
      (when (looking-at gtags-symbol-regexp)
        (gtags-parse-token
         (substring-no-properties (gtags-match-string 0))
         dot-at-back)))))

;; push current context to stack
(defun gtags-push-context ()
  (setq gtags-buffer-stack (cons (current-buffer) gtags-buffer-stack))
  (setq gtags-point-stack (cons (point) gtags-point-stack)))

;; pop context from stack
(defun gtags-pop-context ()
  (if (not gtags-buffer-stack) nil
    (let (buffer point)
      (setq buffer (car gtags-buffer-stack))
      (setq gtags-buffer-stack (cdr gtags-buffer-stack))
      (setq point (car gtags-point-stack))
      (setq gtags-point-stack (cdr gtags-point-stack))
      (list buffer point))))

;; if the buffer exist in the stack
(defun gtags-exist-in-stack (buffer)
  (memq buffer gtags-buffer-stack))

;; get current line number
(defun gtags-current-lineno ()
  (if (= 0 (count-lines (point-min) (point-max)))
      0
    (save-excursion
      (end-of-line)
      (if (equal (point-min) (point))
          1
        (count-lines (point-min) (point))))))

;; completsion function for completing-read.
(defun gtags-completing-gtags (string predicate code)
  (gtags-completing 'gtags string predicate code))
(defun gtags-completing-gsyms (string predicate code)
  (gtags-completing 'gsyms string predicate code))
(defun gtags-completing-files (string predicate code)
  (gtags-completing 'files string predicate code))

(defun gtags-completing-any (string predicate code)
  ;; Can be partial completion string or completions table.  
  (let ((gtags (gtags-completing-gtags string predicate code))
        (gsyms (gtags-completing-gsyms string predicate code))
        (files (gtags-completing-files string predicate code)))
    (case code
      ((nil)
       (or gtags gsyms files))
      ((t)
       (remove nil (append gtags gsyms files))))))

(defun gtags-completing-idutils (string predicate code)
  (gtags-completing 'idutils string predicate code))
;; common part of completing-XXXX
;;   flag: 'gtags or 'gsyms or 'files
(defun gtags-completing (flag string predicate code)
  ; The purpose of using the -n option for the -P command is to exclude
  ; dependence on the execution directory.
  (let ((option (cond ((eq flag 'files)   "-cPo")
                      ((eq flag 'gsyms)   "-cs")
                      ((eq flag 'idutils) "-cI")
                      (t                  "-c")))
        (complete-list (make-vector 63 0))
        (prev-buffer (current-buffer)))
    ;; (if case-fold-search
    ;;     (setq option (concat option "i")))
    ; build completion list
    (set-buffer (generate-new-buffer "*Completions*"))
    (call-process "global" nil t nil option string)
    (goto-char (point-min))
    ;
    ; The specification of the completion for files is different from that for symbols.
    ; The completion for symbols matches only to the head of the symbol. But the completion
    ; for files matches any part of the path.
    ;
    (while (not (eobp))
      (looking-at ".*")
      (intern (gtags-match-string 0) complete-list)
      (forward-line))
    (kill-buffer (current-buffer))
    ; recover current buffer
    (set-buffer prev-buffer)
    ; execute completion
    (cond ((eq code nil)
           (try-completion string complete-list predicate))
          ((eq code t)
           (all-completions string complete-list predicate))
          ((eq code 'lambda)
           (if (intern-soft string complete-list) t nil)))))

;; get the path of gtags root directory.
(defun gtags-get-rootpath ()
  (let (path buffer)
    (save-excursion
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
      (set-buffer buffer)
      (setq n (call-process "global" nil t nil "-pr"))
      (if (= n 0)
        (setq path (file-name-as-directory (buffer-substring (point-min)(1- (point-max))))))
      (kill-buffer buffer))
    path))

;; decode path name
;; The path is encoded by global(1) with the --encode-path="..." option.
;; A blank is encoded to %20.
(defun gtags-decode-pathname (path)
  (let (start result)
    (while (setq start (string-match "%\\([0-9a-f][0-9a-f]\\)" path))
      (setq result (concat result
                     (substring path 0 start)
                     (format "%c" (string-to-number (substring path (match-beginning 1) (match-end 1)) 16))))
      (setq path (substring path (match-end 1))))
    (concat result path)))

;;
;; interactive command
;;
(defun gtags-visit-rootdir ()
  "Tell tags commands the root directory of source tree."
  (interactive)
  (let (path input n)
    (if gtags-rootdir
      (setq path gtags-rootdir)
     (setq path (gtags-get-rootpath))
     (if (equal path nil)
       (setq path default-directory)))
    (setq input (read-file-name "Visit root directory: " path path t))
    (if (equal "" input) nil
      (if (not (file-directory-p input))
        (message "%s is not directory." input)
       (setq gtags-rootdir (expand-file-name input))
       (setenv "GTAGSROOT" gtags-rootdir)))))

(defmacro gtags-read-token (prompt completions)
  "Assume local var `token' is defined.  "
  `(progn
     (unless token
       (unless current-prefix-arg
         (setq token (gtags-current-token)))

       (unless token
         (setq token
               (completing-read (format "Find %s: " ,prompt)
                                ,completions
                                nil nil nil gtags-history-list))))

     (when (stringp token)
       (if (string-match "\\." token)
           (setq token `(filename . ,token))
         (setq token (gtags-parse-token token))))))

(defmacro gtags-define-finder (proc prompt command completions)
  `(defun ,proc (&optional token)
     ,(format "Search %s.  " prompt)
     (interactive)
     (gtags-read-token ,prompt ,completions)
     (gtags-push-context)
     (gtags-goto-tag (cdr token) ,command ,prompt)))

(gtags-define-finder gtags-find-function "function" ""   'gtags-completing-gtags)
(gtags-define-finder gtags-find-symbol   "symbol"   "s"  'gtags-completing-gsyms)
(gtags-define-finder gtags-find-text     "text"     "go" nil)
(gtags-define-finder gtags-find-file     "file"     "Po" 'gtags-completing-files)

(gtags-define-finder gtags-find-function-reference "function reference" "r"  'gtags-completing-gtags)
(gtags-define-finder gtags-find-symbol-reference   "symbol reference"   "rs" 'gtags-completing-gsyms)

(defun gtags-find-any (&optional token)
  (interactive)
  (gtags-read-token "anything" 'gtags-completing-any)
  (if (eq (car token) 'filename)
      (gtags-find-file (cdr token))
    (or (gtags-find-function (cdr token))
        (gtags-find-symbol (cdr token))
        (gtags-find-text (cdr token)))))

(defun gtags-find-any-reference (&optional token)
  (interactive)
  (gtags-read-token "anything reference" 'gtags-completing-any)
  (if (eq (car token) 'filename)
      (gtags-find-text (cdr token))
    (or (gtags-find-function-reference (cdr token))
        (gtags-find-symbol-reference (cdr token))
        (gtags-find-text (cdr token)))))

(defun gtags-find-tag-other-window ()
  "Input tag name and move to the definition in other window."
  (interactive)
  ;; (gtags-find-tag t)
  (error "not supported"))

(defun gtags-find-with-idutils ()
  "Input pattern, search with idutils(1) and move to the locations."
  (interactive)
  (let (tagname prompt input)
    (setq tagname (gtags-current-token))
    (if tagname
        (setq prompt (concat "Find token: (default " tagname ") "))
      (setq prompt "Find token: "))
    (setq input (completing-read prompt 'gtags-completing-idutils
                  nil nil nil gtags-history-list))
    (if (not (equal "" input)) (setq tagname input))
    (gtags-push-context)
    (gtags-goto-tag tagname "I")))

(defun gtags-find-tag-from-here ()
  "Get the expression as a tagname around here and move there."
  (interactive)
  (let (tagname flag)
    (setq tagname (gtags-current-token))
    (if (not tagname)
        nil
      (gtags-push-context)
      (gtags-goto-tag tagname "C"))))

; This function doesn't work with mozilla.
; But I will support it in the near future.
(defun gtags-display-browser ()
  "Display current screen on hypertext browser."
  (interactive)
  (if (= (gtags-current-lineno) 0)
      (message "This is a null file.")
      (if (not buffer-file-name)
          (message "This buffer doesn't have the file name.")
          (call-process "gozilla"  nil nil nil (concat "+" (number-to-string (gtags-current-lineno))) buffer-file-name))))

; Private event-point
; (If there is no event-point then we use this version.
(eval-and-compile
  (if (not (fboundp 'event-point))
      (defun event-point (event)
	(posn-point (event-start event)))))

(defun gtags-find-tag-by-event (event)
  "Get the expression as a tagname around here and move there."
  (interactive "e")
  (let (tagname flag)
    (if (= 0 (count-lines (point-min) (point-max)))
        (progn (setq tagname "main")
               (setq flag ""))
      (if gtags-running-xemacs
          (goto-char (event-point event))
        (select-window (posn-window (event-end event)))
        (set-buffer (window-buffer (posn-window (event-end event))))
        (goto-char (posn-point (event-end event))))
      (setq tagname (gtags-current-token))
      (setq flag "C"))
    (if (not tagname)
        nil
      (gtags-push-context)
      (gtags-goto-tag tagname flag))))

(defun gtags-select-tag (&optional other-win)
  "Select a tag in [GTAGS SELECT MODE] and move there."
  (interactive)
  (gtags-push-context)
  (gtags-select-it nil other-win))

(defun gtags-select-tag-other-window ()
  "Select a tag in [GTAGS SELECT MODE] and move there in other window."
  (interactive)
  (gtags-select-tag t))

(defun gtags-select-tag-by-event (event)
  "Select a tag in [GTAGS SELECT MODE] and move there."
  (interactive "e")
  (if gtags-running-xemacs (goto-char (event-point event))
    (select-window (posn-window (event-end event)))
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event))))
  (gtags-push-context)
  (gtags-select-it nil))

(defun gtags-pop-stack ()
  "Move to previous point on the stack."
  (interactive)
  (let (delete context buffer)
    (if (and (not (equal gtags-current-buffer nil))
             (not (equal gtags-current-buffer (current-buffer))))
         (switch-to-buffer gtags-current-buffer)
         ; By default, the buffer of the referred file is left.
         ; If gtags-pop-delete is set to t, the file is deleted.
         ; Gtags select mode buffer is always deleted.
         (if (and (or gtags-pop-delete (equal mode-name "Gtags-Select"))
                  (not (gtags-exist-in-stack (current-buffer))))
	     (setq delete t))
      (setq context (gtags-pop-context))
      (if (not context)
	  (message "The tags stack is empty.")
        (if delete
	    (kill-buffer (current-buffer)))
        (switch-to-buffer (nth 0 context))
        (setq gtags-current-buffer (current-buffer))
        (goto-char (nth 1 context))))))


;;
;; common function
;;

;; goto tag's point
(defun gtags-goto-tag (tagname flag prompt &optional other-win)
  "Return t if tag was found.  "
  (setq tagname (car (split-string tagname)))
  (let (option context save prefix buffer lines flag-char)
    (setq save (current-buffer))
    (setq flag-char (string-to-char flag))
    ;; Use always ctags-x format.
    (setq option "-x")
    ;; (if case-fold-search
    ;;     (setq option (concat option "i")))
    (if (char-equal flag-char ?C)
        (setq context (concat "--from-here=" (number-to-string (gtags-current-lineno)) ":" buffer-file-name))
      (setq option (concat option flag)))
    (cond
     ((char-equal flag-char ?C)
      (setq prefix "(CONTEXT)"))
     ((char-equal flag-char ?P)
      (setq prefix "(P)"))
     ((char-equal flag-char ?f)
      (setq prefix "(F)")
      (setq option (concat option "q")))
     ((char-equal flag-char ?g)
      (setq prefix "(GREP)"))
     ((char-equal flag-char ?I)
      (setq prefix "(IDUTILS)"))
     ((char-equal flag-char ?s)
      (setq prefix "(S)"))
     ((char-equal flag-char ?r)
      (setq prefix "(R)"))
     (t (setq prefix "(D)")))
    ;; load tag
    (if gtags-select-buffer-single
        (progn
          ;; delete "*GTAGS SELECT*" buffer info from gtags-buffer-stack and gtags-point-stack
          (let (now-gtags-buffer-stack now-buffer now-gtags-point-stack now-point)
            (setq now-gtags-buffer-stack (reverse gtags-buffer-stack))
            (setq now-gtags-point-stack (reverse gtags-point-stack))
            (setq gtags-buffer-stack nil)
            (setq gtags-point-stack nil)
            (while now-gtags-buffer-stack
              (setq now-buffer (car now-gtags-buffer-stack))
              (setq now-point (car now-gtags-point-stack))
              (if (and (buffer-name now-buffer) (not (string-match "*GTAGS SELECT*" (buffer-name now-buffer))))
                  (progn
                    (setq gtags-buffer-stack (cons now-buffer gtags-buffer-stack))
                    (setq gtags-point-stack (cons now-point gtags-point-stack))))
              (setq now-gtags-buffer-stack (cdr now-gtags-buffer-stack))
              (setq now-gtags-point-stack (cdr now-gtags-point-stack))))
          ;; kill "*GTAGS SELECT*" buffer
          (let (now-buffer-list now-buffer)
            (setq now-buffer-list (buffer-list))
            (while now-buffer-list
              (setq now-buffer (car now-buffer-list))
              (if (string-match "*GTAGS SELECT*" (buffer-name now-buffer))
                  (kill-buffer now-buffer))
              (setq now-buffer-list (cdr now-buffer-list))))))
    (setq buffer (generate-new-buffer (generate-new-buffer-name (concat "*GTAGS SELECT* " prefix tagname))))
    (set-buffer buffer)
    ;;
    ;; Path style is defined in gtags-path-style:
    ;;   root: relative from the root of the project (Default)
    ;;   relative: relative from the current directory
    ;;	absolute: absolute (relative from the system root directory)
    ;;
    (cond
     ((equal gtags-path-style 'absolute)
      (setq option (concat option "a")))
     ((equal gtags-path-style 'root)
      (let (rootdir)
        (if gtags-rootdir
            (setq rootdir gtags-rootdir)
          (setq rootdir (gtags-get-rootpath)))
        (if rootdir (cd rootdir)))))
    (message "Searching %s `%s' ..." prompt tagname)
    (unless (= 0 (if (equal flag "C")
                     (call-process "global" nil t nil option "--encode-path=\" \t\"" context tagname)
                   (call-process "global" nil t nil option "--encode-path=\" \t\"" tagname)))
      (message "%s" (buffer-substring (save-excursion
                                        (goto-char (point-max))
                                        (forward-line -1)
                                        (line-beginning-position))
                                      (point-max))))

    (goto-char (point-min))
    (setq lines (count-lines (point-min) (point-max)))
    (case lines
      ((0)
       (message "%s: %s not found" tagname prompt)
       (gtags-pop-context)
       (kill-buffer buffer)
       (set-buffer save))
      ((1)
       (message "Searching %s `%s' ... Done" prompt tagname)
       (gtags-select-it t other-win))
      (t
       (if (null other-win)
           (switch-to-buffer buffer)
         (switch-to-buffer-other-window buffer))
       (gtags-select-mode)))

    (and lines (> lines 0))))

;; select a tag line from lines
(defun gtags-select-it (delete &optional other-win)
  (let (line file)
    ;; get context from current tag line
    (beginning-of-line)
    (if (not (looking-at "[^ \t]+[ \t]+\\([0-9]+\\)[ \t]\\([^ \t]+\\)[ \t]"))
        (gtags-pop-context)
      (setq line (string-to-number (gtags-match-string 1)))
      (setq file (gtags-decode-pathname (gtags-match-string 2)))
      ;;
      ;; Why should we load new file before killing current-buffer?
      ;;
      ;; If you kill current-buffer before loading new file, current directory
      ;; will be changed. This might cause loading error, if you use relative
      ;; path in [GTAGS SELECT MODE], because emacs's buffer has its own
      ;; current directory.
      ;;
      (let ((prev-buffer (current-buffer)))
        ;; move to the context
        (if gtags-read-only
            (if (null other-win) (find-file-read-only file)
              (find-file-read-only-other-window file))
          (if (null other-win) (find-file file)
            (find-file-other-window file)))
        (if delete (kill-buffer prev-buffer)))
      (setq gtags-current-buffer (current-buffer))
      (goto-char (point-min))
      (forward-line (1- line))
      (gtags-mode 1))))

;; make complete list (do nothing)
(defun gtags-make-complete-list ()
  "Make tag name list for completion."
  (interactive)
  (message "gtags-make-complete-list: Deprecated. You need not call this command any longer."))

;;;###autoload
(defun gtags-mode (&optional forces)
  "Toggle Gtags mode, a minor mode for browsing source code using GLOBAL.

Specify the root directory of project.
	\\[gtags-visit-rootdir]
Input tag name and move to the definition.
	\\[gtags-find-tag]
Input tag name and move to the definition in other window.
        \\[gtags-find-tag-other-window]
Input tag name and move to the referenced point.
	\\[gtags-find-rtag]
Input symbol and move to the locations.
	\\[gtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[gtags-find-with-grep]
Input pattern, search with idutils(1) and move to the locations.
	\\[gtags-find-with-idutils]
Input pattern and move to the top of the file.
	\\[gtags-find-file]
Input pattern and show the list of definitions of the file.
	\\[gtags-parse-file]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-from-here]
Display current screen on hypertext browser.
	\\[gtags-display-browser]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-by-event]
Move to previous point on the stack.
	\\[gtags-pop-stack]

Key definitions:
\\{gtags-mode-map}
Turning on Gtags mode calls the value of the variable `gtags-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (or (assq 'gtags-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(gtags-mode " Gtags") minor-mode-alist)))
  (or (assq 'gtags-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
      (cons (cons 'gtags-mode gtags-mode-map) minor-mode-map-alist)))
  (setq gtags-mode
      (if (null forces) (not gtags-mode)
        (> (prefix-numeric-value forces) 0)))
  (run-hooks 'gtags-mode-hook)
)

;; make gtags select-mode
(defun gtags-select-mode ()
  "Major mode for choosing a tag from tags list.

Select a tag in tags list and move there.
	\\[gtags-select-tag]
Move to previous point on the stack.
	\\[gtags-pop-stack]

Key definitions:
\\{gtags-select-mode-map}
Turning on Gtags-Select mode calls the value of the variable
`gtags-select-mode-hook' with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map gtags-select-mode-map)
  (setq buffer-read-only t
	truncate-lines t
        major-mode 'gtags-select-mode
        mode-name "Gtags-Select")
  (setq gtags-current-buffer (current-buffer))
  (goto-char (point-min))
  (message "[GTAGS SELECT MODE] %d lines" (count-lines (point-min) (point-max)))
  (run-hooks 'gtags-select-mode-hook)
)

(provide 'gtags)

;;; gtags.el ends here
;
