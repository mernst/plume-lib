;; bdiff.el --- compare the current buffer to a version on disk
;; Roger Crew <rfc@research.microsoft.com>

;; Emacs 19 version

;; bdiff:
;;
;;   M-x bdiff
;;     compare with visited file
;;   C-u M-x bdiff
;;     compare with most recent backup
;;   C-u C-u M-x bdiff
;;     compare with autosave file
;;
;;   M-- bdiff
;;     compare with file returned by bdiff---prefix-file
;;   M-digit bdiff
;;     compare with file returned by bdiff-digit-prefix-file
;;
;;   by default bdiff---prefix-file and bdiff-digit-prefix-file
;;   return the most recent backup, but these can be customized
;;   to make other sorts of comparisons.
;;
;; list-munged-buffers:
;;   generate listing of file buffers that have been
;;   modified out from under you
;;
;; list-unsaved-buffers:
;;   generate listing of unsaved file buffers

(defvar bdiff-context-lines 1
  "Number of lines of context to be given in bdiff listing.
If 'unidiff, then use unidiff output format instead.")

(defvar bdiff-ignore-whitespace nil "\
If t, bdiff totally ignores whitespace.
  E.g., ``if ( a == b )'' will compare equal to ``if(a==b)''.
If non-nil but not t, causes bdiff to compare strings of whitespace equal
  and ignore trailing whitespace.
If nil, whitespace is taken literally.")

(defvar bdiff-set-modified-if-different nil
  "If true, interactive bdiff sets the modified flag in the
case where you bdiff a buffer with its visited file and the respective
contents differ.  (This is the reverse sense of the t/nil argument
that you can supply to bdiff in a program.)")

(defvar bdiff-buffer-name "*Diff*output*"
  "Buffer for diff output.")

(defvar bdiff-text-output (not (member system-type '(windows-nt)))
  "If nil, RETURN characters need to be removed from diff output.")

(defvar bdiff-extra-flags "a"
  "Flags to pass to diff; for instance, \"a\" if you're using GNU diff
or \"t\" to expand tabs to spaces.")

(defun bdiff (&optional bfile quiet)
  "*diff the current buffer with the file being visited.
When called interactively:
With a ^U ^U prefix argument, diffs with the autosave file instead.
With any other prefix argument, diffs the current buffer with the
most recent backup file.

When called from a program:
This takes a single FILENAME argument
and returns T iff the current buffer differs from the specified file.
A nil filename argument specifies the visited file indicating
that the buffer should be marked as modified according as the buffer
differs from the file.
A T filename argument specifies the visited file and the the buffer
should be marked as unmodified if the buffer and file are the
same (modification mark doesn't change otherwise).

'quiet' argument means produce no output, just return a value."
  (interactive
   (bdiff-prefix-arg (current-buffer)))
  (if (stringp bfile)
      (setq bfile (expand-file-name bfile)))
  (let (temp-buffer-show-function)
    (with-output-to-temp-buffer bdiff-buffer-name
      (let (modified)
	(let ((real-bfile (or (and (not (eq bfile t)) bfile)
			      (buffer-file-name)
			      (error "no file associated with this buffer"))))
	  (if (not (file-exists-p (file-name-directory real-bfile)))
	      (princ (concat "Directory " (file-name-directory real-bfile)
			     " does not exist\n"))
	    (save-restriction
	      (widen)
	      (setq process-result
		    (call-process-region (point-min) (point-max)
					 diff-command nil
					 (and (not quiet) standard-output)
					 nil
					 (if (eq bdiff-context-lines 'unidiff)
					     "-u"
					   (format "-C %d" bdiff-context-lines))
					 (concat
					  (cond ((eq bdiff-ignore-whitespace t) "-w")
						(bdiff-ignore-whitespace "-b")
						;; If bdiff-extra-flags is empty,
						;; this causes trouble.
						(t "-"))
					  (if quiet "q" "")
					  bdiff-extra-flags)
					 real-bfile
					 "-"))
	      (setq modified (not (zerop process-result))))))
	;; old version
	;; (setq modified (save-excursion
	;;                  (set-buffer standard-output)
	;;                  (goto-char (point-min))
	;;                  (not (looking-at
	;;                        "\\'\\|\\`No differences encountered\n\\'"))))
	(if (or (null bfile)
		(eq bfile (not modified)))
	    (progn
	      (if (not modified)
		  ;; if file was modified out from under us,
		  ;; but not actually changed, fix modtime.
		  (set-visited-file-modtime))
	      (set-buffer-modified-p modified)))
	(if (and modified (not quiet))
	    (let ((orig-tab-width tab-width))
	      (save-excursion
		(set-buffer bdiff-buffer-name)
		(setq tab-width orig-tab-width)
		(if bdiff-text-output
		    (progn
		      (goto-char (point-min))
		      (while (re-search-forward "\r$" nil 1)
			(replace-match "" nil t))))))
	    (progn
	      (setq temp-buffer-show-function (function ignore))
	      (if (and (interactive-p) (not quiet))
		  (message "No differences encountered"))))
	(with-current-buffer bdiff-buffer-name
	  (diff-mode))
	modified))))

(defun bdiff-prefix-arg (buf)
  (list
   (cond ((not (buffer-file-name buf))
	  (read-file-name (format "diff buffer %s with: " buf) nil nil t))
	 ((not current-prefix-arg)
	  (not bdiff-set-modified-if-different))
	 ((and (consp current-prefix-arg)
	       (= (car current-prefix-arg) 16))
	  (save-excursion (set-buffer buf)
			  (make-auto-save-file-name)))
	 ((eq current-prefix-arg '-)
	  (bdiff---prefix-file (buffer-file-name buf)))
	 ((numberp current-prefix-arg)
	  (bdiff-digit-prefix-file (buffer-file-name buf)))
	 (t
	  (bdiff-backup-file (buffer-file-name buf))))))

(defun bdiff-backup-file (filename)
  "Find the most recent backup file for FILENAME."
  (let* ((default-directory (file-name-directory filename))
	 (fn (file-name-nondirectory filename))
	 (base-versions (concat fn ".~"))
	 (bv-length (length base-versions))
	 (highest 0)
	 (mname
	  (make-backup-file-name fn))
	 (mtime
	  (let ((attr (file-attributes mname)))
	    (if attr
		(nth 5 attr)
	      (setq mname nil)
	      '(0 0)))))
    (mapc
     (function
      (lambda (fn)
	(let ((ftime (nth 5 (file-attributes fn)))
	      (num   (backup-extract-version fn)))
	  (if (and (> num highest)
		   (or (> (car ftime) (car mtime))
		       (and (= (car ftime) (car mtime))
			    (> (nth 1 ftime) (nth 1 mtime)))))
	      (setq highest num
		    mname fn
		    mtime ftime)))))
     (file-name-all-completions base-versions default-directory))
    (if mname (expand-file-name mname)
      (error "no backup versions of %s" filename))))

;; fset these hooks if you want bdiff to do funky things
;; on M-- or M-digit prefix arguments
(or (fboundp 'bdiff---prefix-file)
    (fset 'bdiff---prefix-file 'bdiff-backup-file))
(or (fboundp 'bdiff-digit-prefix-file)
    (fset 'bdiff-digit-prefix-file 'bdiff-backup-file))

(defun Buffer-menu-bdiff (buf bdiff-arg)
  "Run bdiff on this line's buffer."
  (interactive (let ((buf (Buffer-menu-buffer t)))
		 (cons buf (bdiff-prefix-arg buf))))
  (let ((menu-buf (current-buffer)))
    (or (buffer-file-name buf)
	(error "Not a file buffer"))
    (switch-to-buffer buf)
    (delete-other-windows)
    (let ((different (bdiff bdiff-arg)))
      (switch-to-buffer menu-buf)
      (or different
	  (let ((buffer-read-only nil))
	    (forward-line 0)
	    (delete-region (point) (progn (forward-line 1) (point)))
	    (message "No differences encountered"))))))

(defun Buffer-menu-revert ()
  "Revert this line's buffer:  replace it with text of visited file on disk."
  (interactive)
  (let ((buf (Buffer-menu-buffer t)))
    (or (buffer-file-name buf)
	(error "Not a file buffer"))
    (save-excursion
      (set-buffer buf)
      (revert-buffer t t))
    ;; was (forward-line 1)
    (let ((buffer-read-only nil))
      (forward-line 0)
      (kill-line 1))))

(defun Buffer-menu-revert-select ()
  "Select this line's buffer, after replacing its text with disk file contents.
See also `Buffer-menu-revert' and `Buffer-menu-this-window'."
  (interactive)
  (let ((buf (Buffer-menu-buffer t)))
    (Buffer-menu-revert)
    (switch-to-buffer buf)))

(define-key Buffer-menu-mode-map "c" 'Buffer-menu-bdiff)
(define-key Buffer-menu-mode-map "=" 'Buffer-menu-bdiff)
(define-key Buffer-menu-mode-map "R" 'Buffer-menu-revert)
(define-key Buffer-menu-mode-map "r" 'Buffer-menu-revert-select)

(defvar list-munged-buffers-do-bdiff t
  "Non-nil if `list-munged-buffers' should always do a diff.
Otherwise, it first compares visited file modtimes.
Doing the diff is slower but avoids listing files that have been
identically regenerated, or touched but not modified.")

(defun list-munged-buffers (&optional arg)
  "Display a `list-buffers'-style listing of all file buffers that
have been modified behind our backs.
Prefix arg toggles interpretation of `list-munged-buffers-do-bdiff'."
  (interactive "P")
  (list-some-buffers
   (function (lambda (b)
	       (and (not (verify-visited-file-modtime b))
		    (or (if (not arg)
			    (not list-munged-buffers-do-bdiff)
			  list-munged-buffers-do-bdiff)
			(save-excursion
			  (set-buffer b)
			  (bdiff-p))))))
   "None of your buffer files has been altered on disk."))

(defun list-unsaved-buffers ()
  "Display a `list-buffers'-style listing of modified file buffers."
  (interactive)
  (list-some-buffers
   (function (lambda (b)
	       (and (buffer-file-name b) (buffer-modified-p b))))
   "No unsaved buffers."))

;; This implementation strategy is broken.  It would be better to call
;; list-buffers-noselect with the list of buffers to list, rather than
;; trying to delete the non-desired ones.  Purging doesn't work anyway for
;; buffers with long names that are truncated in the "*Buffer List*" buffer.
(defun list-some-buffers (predicate none-msg)
  (let ((munged nil))
    (mapc (function (lambda (b)
		      (if (funcall predicate b)
			  (setq munged (cons b munged)))))
	  (buffer-list))
    (if munged
	(progn
	  (switch-to-buffer (list-buffers-noselect nil munged))
	  ;; (message
	  ;;  "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help.")
	  )
      (progn
	(delete-windows-on (get-buffer "*Buffer List*"))
	(message none-msg)))))

;; This method silently fails to purge buffers with long names that are
;; truncated in the "*Buffer List*" buffer.  It's probably better to list
;; specific buffers rather than to purge after the fact (as this function does).
(defun purge-buffer-listing (unmunged)
  (list-buffers t)
  (set-window-point
   (get-buffer-window (get-buffer "*Buffer List*"))
   (save-excursion
     (set-buffer "*Buffer List*")
     (goto-char (point-min))
     (if (not (and (boundp 'Buffer-menu-use-header-line)
		   Buffer-menu-use-header-line))
	 ;; When using Buffer-menu-use-header-line, point-min is on first buffer
	 (forward-line 2))
     (save-restriction
       (let ((first-line (point))
	     buffer-read-only)
	 (narrow-to-region (point) (point-max))
	 (mapc (function
		(lambda (b)
		  (goto-char (point-max))
		  (if (re-search-backward
		       (concat "^...."
			       (regexp-quote (buffer-name b))
			       "[ \t]+")
		       nil t)
		      (delete-region
		       (point)
		       (save-excursion (forward-line 1)
				       (point))))))
	       unmunged)
	 first-line)))))


(defun verify-all-buffers ()
  "Query the user about all buffers that have been modified behind our backs."
  (interactive)
  (mapcar (function verify-buffer) (buffer-list)))

(defun verify-buffer (buf)
  "Determine if BUFFER has been modified on disk by another process.
If so, bdiff (see M-x bdiff) the buffer with the disk version; if there
are no differences, simply revert the buffer, otherwise display the
differences and query the user whether the buffer should be reverted."
  (interactive "bBuffer to verify: ")
  (setq buf (get-buffer buf))
  (if (verify-visited-file-modtime buf)
      (and (interactive-p)
	   (message "Buffer is up to date."))
    (let ((filename (buffer-file-name buf)))
      (if (not (file-exists-p filename))
	  (error "File %s no longer exists!" filename)
	(save-excursion
	  (set-buffer buf)
	  (if (save-window-excursion
		(delete-other-windows (display-buffer buf))
		(or (not (bdiff t))
		    (yes-or-no-p
		      (format "File %s changed on disk.  %s? "
		       filename
		       (if (buffer-modified-p buf)
			   "Flush your changes"
			 "Re-read from disk")))))
	      (let* ((w (get-buffer-window buf))
		     (p (and w (window-point w)))
		     (s (and w (window-start w))))
		(revert-buffer t t)
		(and p (progn (set-window-point w p)
			      (set-window-start w s))))))))))

(defun bdiff-p (&optional bfile)
  "Return t if the current buffer has been modified, nil otherwise.
A nil filename argument specifies the visited file."
  (setq bfile (or bfile
		  (buffer-file-name)
		  (error "no file associated with this buffer")))
  (let ((modified (or (not (file-exists-p (file-name-directory bfile)))
		      (not (file-exists-p bfile))
		      (not (= (buffer-size) (nth 7 (file-attributes bfile))))
		      (not (zerop (save-restriction
				    (widen)
				    (call-process-region
				     (point-min) (point-max) diff-command nil nil nil
				     "-q"
				     (concat
				      (cond ((eq bdiff-ignore-whitespace t) "-wt")
					    (bdiff-ignore-whitespace "-bt")
					    (t "-t"))
				      bdiff-extra-flags)
				     bfile
				     "-"))))
		      )))
    ;; This is important so that this doesn't run slowly every time.
    (if (not modified)
	;; if file was modified out from under us,
	;; but not actually changed, fix modtime.
	(set-visited-file-modtime))
    modified))

(provide 'bdiff)
