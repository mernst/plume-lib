;;; startup-functions-mde.el --- unconditionally defined functions

;;; Commentary:
;; (none yet)

;;; Code:

(add-hook 'before-save-hook 'time-stamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defined functions
;;;

;; Generally useful routine
(defun average (&rest args)
  "Return the average of the arguments."
  ;; multiplication by 1.0 ensures floating-point division
  (/ (apply '+ args) (* 1.0 (length args))))

(defun median (&rest args)
  "Return the median of the arguments."
  (let* ((len (length args))
         (sorted (sort args #'<))
         (firstmedian (nth (/ (- len 1) 2) sorted))
         (secondmedian (nth (/ len 2) sorted)))
    ;; division by 2.0 ensures floating-point division
    (/ (+ firstmedian secondmedian) 2.0)))
;; (assert (equal 4.5 (median 1 2 3 4 5 6 7 8)))
;; (assert (equal 5.0 (median 1 2 3 4 5 6 7 8 9)))
;; (assert (equal 4.5 (median 4 6 7 1 5 3 8 2)))
;; (assert (equal 5.0 (median 4 6 7 2 8 1 9 5 3)))

(defun geometric-mean (&rest args)
  "Return the geometric mean of the arguments."
  (expt (apply '* args) (/ 1.0 (funcall 'length args))))
;; (assert (equal 2.0 (geometric-mean 1 2 4))

(defun mde-split-window-vertically (arg)
  ;; checkdoc-params: (arg)
  "If called with no argument, change the buffer in the other window."
  (interactive "p")                     ; also makes this M-x accessible.
  (split-window-vertically)
  (if (= 1 arg)
      ;; Alternate implementation, in case the call to display-buffer in
      ;; switch-to-buffer-other-window changes window sizes.
      ;;       (progn
      ;;        (other-window 1)
      ;;        (switch-to-buffer (other-buffer)))
      (switch-to-buffer-other-window (other-buffer))
    ))
(defun mde-split-window-horizontally (arg)
  ;; checkdoc-params: (arg)
  "If called with no argument, change the buffer in the other window."
  (interactive "p")                     ; also makes this M-x accessible.
  (split-window-horizontally)
  (if (= 1 arg)
      ;; Alternate implementation, in case the call to display-buffer in
      ;; switch-to-buffer-other-window changes window sizes.
      ;;       (progn
      ;;        (other-window 1)
      ;;        (switch-to-buffer (other-buffer)))
      (switch-to-buffer-other-window (other-buffer))
    ))

(global-set-key "\C-x2" 'mde-split-window-vertically)
(global-set-key "\C-x3" 'mde-split-window-horizontally)


;; for default, could use something on the order of find-tag-tag instead.
(defun symbol-func (function)
  "Display the value of (symbol-function FUNCTION); for interactive use."
  ;; interactive spec snarfed from describe-function
  (interactive
   (let ((fn (function-called-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read (if fn
                                    (format "Symbol-function (default %s): " fn)
                                  "Symbol-function: ")
                                obarray 'fboundp t))
     (list (if (equal val "")
               fn (intern val)))))
  (message "%s" (if (fboundp function)
                    (symbol-function function))))


(defun dos-view ()
  "Hide/unhide carriage returns, for viewing DOS files."
  (interactive)
  (setq selective-display (not selective-display)
        selective-display-ellipses (not selective-display)
        ))

(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (replace-match ""))
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))

(defun jump-to-mark-and-pop ()
  "Call `set-mark-command' with an argument.
That is, \"jump to mark, and pop into mark off the mark ring.\""
  (interactive)
  (set-mark-command t))

;; Similar to Emacs 20's kill-buffer-and-window, but that always requires
;; confirmation.
(defun my-kill-buffer-and-window ()
  "Kill the buffer and delete the window it's displayed in."
  (interactive)
  (kill-buffer (current-buffer))
  (if (not (one-window-p t))
      (delete-window))
  ;; I hate seeing the "C-x -", which looks like it wants more input.
  (message ""))


(defun make-interactive (symbol &optional interactive-spec)
  "Make the function on SYMBOL be a command (make it interactive).
Optional INTERACTIVE-SPEC defaults to the list (interactive)."
  (let ((fn (symbol-function symbol)))
    (if (not (commandp fn))
        ;; it isn't already interative
        (setcdr (cdr fn)
                (cons (if interactive-spec
                          (list 'interactive interactive-spec)
                        '(interactive))
                      (cdr (cdr fn)))))))

(defun raise-buffer ()
  "Switch to the last buffer on the buffer list.
This one is likely to have been recently buried."
  (interactive)
  (let ((blist (nreverse (buffer-list))))
    (while (string-match "\\` " (buffer-name (car blist)))
      (setq blist (cdr blist)))
    (if blist
        (switch-to-buffer (car blist)))))

(defun bury-or-raise-buffer (arg)
  "Bury current buffer; with prefix arg, switch to last buffer in `buffer-list'.
Programmatically, non-nil argument ARG means raise; if nil, then bury."
  (interactive "P")
  (if arg
      (raise-buffer)
    (bury-buffer)))

;; (defun conor-display ()
;;   "Set up the X parameters for remote login from conor."
;;   (interactive)
;;   (x-set-font "7x13")
;;   (if (eq "white" (x-get-foreground-color))
;;       (x-flip-color)))

(defun usenet-address (fuzzy-string)
  "Find a random person's email address, if he has ever posted netnews.
Argument FUZZY-STRING is a string of space-separated names to be fuzzy-matched."
  (interactive "sSpace-separated names for fuzzy matching: ")
  (eval-when-compile (require 'sendmail))
  (mail)
  (mail-to) (insert "mail-server@pit-manager.mit.edu")
  (mail-text) (insert "send usenet-addresses/" fuzzy-string)
  (mail-bcc) (beginning-of-line 1) (kill-line 1)
  (mail-send-and-exit nil))


;; It is no good to use variable `confirm-kill-emacs', because I wish to be
;; informed of what I am doing before killing off processes, etc.
(defun save-buffers-kill-emacs-maybe (&optional save-silently)
  "Confirm that the user wishes to kill this Emacs process,
offer to save each buffer, then kill the process.
With prefix arg SAVE-SILENTLY, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (if (y-or-n-p "Kill Emacs? ")
      (progn
        (if (fboundp 'bbdb-save-db)
            (bbdb-save-db))
        (save-buffers-kill-emacs save-silently))))

;; (defadvice save-buffers-kill-emacs (before kill-ispell activate)
;;   "Kill Ispell before exiting Emacs (avoids questions about killing it)."
;;   (let ((ispell-process (get-process "ispell")))
;;     (if ispell-process
;;      (progn
;;        (emacs-21
;;          (process-kill-without-query ispell-process))
;;        (emacs-22+
;;          (set-process-query-on-exit-flag ispell-process nil))))))

;; Emacs22 or later version
(emacs-22+
 (defadvice save-buffers-kill-emacs (before kill-ispell activate)
   "Kill Ispell before exiting Emacs (avoids questions about killing it)."
   (let ((ispell-process (get-process "ispell")))
     (if ispell-process
         (set-process-query-on-exit-flag ispell-process nil)))))

;; Already defined in Emacs 22.
(if (not (fboundp 'process-query-on-exit-flag))
  (defun process-query-on-exit-flag (process)
    "Return the current value of query-on-exit flag for PROCESS."
    (let ((val (process-kill-without-query process)))
      ;; restore previous value for whether query needed
      (process-kill-without-query process val)
      val)))

;; Like the 21.2 version, but more specific question if only one process.
;; I should submit a patch.
(defvar confirm-kill-emacs nil)
(defun save-buffers-kill-emacs (&optional save-silently)
  "Offer to save each buffer, then kill this Emacs process.
With prefix arg, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers save-silently t)
  (and (or (not (memq t (mapcar (function
                                  (lambda (buf) (and (buffer-file-name buf)
                                                     (buffer-modified-p buf))))
                                (buffer-list))))
           (yes-or-no-p "Modified buffers exist; exit anyway? "))
       (or (not (fboundp 'process-list))
           ;; process-list is not defined on VMS.
           (let ((processes (process-list))
                 actives '())
             (while processes
               (and (memq (process-status (car processes)) '(run stop open))
                    (process-query-on-exit-flag (car processes))
                    (setq actives (cons (car processes) actives)))
               (setq processes (cdr processes)))
             (or (not actives)
                 (if (cdr actives)
                     (progn
                       (list-processes)
                       (yes-or-no-p "Active processes exist; kill them and exit anyway? "))
                   (yes-or-no-p (concat "Process " (process-name (car actives)) " is active; kill it and and exit anyway? "))))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
           (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))

;; ;; I probably did this because gnus was too slow.
;; ;; This should kill that pesky background buffer too.
;; (defun gnus-shell ()
;;   "Run a separate gnus process in the background."
;;   (interactive)
;;   (background "/usr/local/bin/emacs -wn gnus -rn gnus -e gnus"))
;; (fset 'gnus-background 'gnus-shell)


;;; If I use ack instead of grep, then this should be less necessary.

(defun grep-clean (&optional buffer)
  "Remove backup and uninteresting files from grep listing.
Arbitrary BUFFER may be supplied (defaults to *grep*)."
  (interactive)
  (if (not buffer)
      (setq buffer (get-buffer "*grep*")))
  (if (not buffer)
      (error "No *grep* buffer"))
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((buffer-read-only nil))
      (delete-matching-lines "^[-./a-z0-9_~]+~\\([0-9]+~\\)?:") ; backups
      (delete-matching-lines "^#[-./a-z0-9_]+#:")              ; autosave
      (delete-matching-lines "^\\([-./a-z0-9_]+/\\)?\\.#") ; CVS backups
      (delete-matching-lines "^\\.newsrc") ; dotfiles in home directory
      (delete-matching-lines "^[-./a-z0-9_]+\\.elc:")
      (delete-matching-lines "^[-./a-z0-9_]+\\.[gs]fasl42:")
      ;; maybe also "grep: Read error on project: Is a directory"

      ;; Now shorten file names
      (do-buffer-menu-replacements)
      )))
(autoload 'do-buffer-menu-replacements "buffer-menu-mde")

(defun vgrep (command-args)
  ;; checkdoc-params: (command-args)
  "Run `grep' (which see), but use a special command line for Vortex."
  (interactive
   (list (let ((grep-command '("grep -n -i  $links/*.{c,h,cecil,rtl}" . 12))
               (default-directory (substitute-in-file-name "$links")))
           (read-from-minibuffer "Run grep (like this): "
                                 grep-command nil nil 'grep-history))))
  (grep command-args))

(add-hook 'compilation-finish-functions 'do-grep-clean-maybe)

;; It would be good for a prefix argument to M-X grep to toggle the
;; value/sense of this variable.
(defvar grep-clean-p t
  "Whether `do-grep-clean-maybe' should do anything.")

(defun do-grep-clean-maybe (buffer finish-description)
  "Run `grep-clean'.  Intended for use in variable `compilation-finish-functions'."
  (if (and (equal "*grep*" (buffer-name buffer))
           grep-clean-p)
      (grep-clean buffer)))


;; This form follows (in this file) other advices of grep because by
;; default each advice is placed first in the list of advices so far.
(defadvice grep (before check-grep-args first activate)
  "Check the arguments; avoid calling grep without a pattern or without any files."
  (let ((args (ad-get-args 0)))
    (if (= 1 (length args))
        (progn
          (setq args (car args))
          (if (string-match "^.?grep[ \t]+" args)
              (progn
                (setq args (substring args (match-end 0)))
                (while (string-match "^-[^ \t]+[ \t]+" args)
                  (setq args (substring args (match-end 0))))
                (if (equal args "")
                    (error "No pattern or files supplied to grep: %s" (ad-get-args 0)))
                (if (or (not (string-match "[ \t]" args))
                        (string-match "^\\('[^']*'\\|\"[^\"]*\"\\)$" args))
                    ;; (error "Insufficient arguments to grep: %s" (ad-get-args 0))
                    (let ((files (read-string "grep in files (default *): ")))
                      (if (equal files "")
                          (setq files "*"))
                      (ad-set-arg 0 (concat (ad-get-arg 0) " " files))))))))))



(eval-when-compile '(require 'man))     ; for Man-fontify-manpage-flag

;;; Was "man-format", but that conflicts with "manual-entry" for completion.
;; This seems to work, but point is left in the Man buffer and the messages
;; speak of nil, not what Man-arguments really ought to be.
(defun format-man ()
  "Format the current buffer as a raw nroff man page."
  (interactive)
  (require 'man)
  (let* ((Man-arguments
          (let ((file-name (buffer-file-name)))
           (if file-name
               (file-name-sans-extension
                (file-name-nondirectory file-name))
             (file-name-sans-extension (buffer-name)))))
         (buffer (get-buffer-create (concat "*Man " Man-arguments "*"))))
    (save-excursion
      (save-excursion
        (set-buffer buffer)
        (erase-buffer))
      (shell-command-on-region (point-min) (point-max) "nroff -man" buffer)
      (pop-to-buffer buffer)
      (if Man-fontify-manpage-flag
          (Man-fontify-manpage)
        (Man-cleanup-manpage))
      (run-hooks 'Man-cooked-hook)
      (Man-mode)
      (set-buffer-modified-p nil))))

(defadvice find-file (before delete-trailing-newline activate)
  "Remove any trailing newline in filename (can be caused by cut and paste)."
  (if (and (interactive-p)
           (string-match "\n$" (ad-get-arg 0)))
      (ad-set-arg 0 (substring (ad-get-arg 0) 0 (match-beginning 0)))))

;; ;; originally from Greg Badros
;; (defun vc-update ()
;;   "Refresh and then update buffers."
;;   (interactive)
;;   (vc-clear-context)
;;   (let ((dir default-directory)          ; pop-to-buffer might change current buffer
;;      (output-buffer (generate-new-buffer "*cvs update output*")))
;;     (pop-to-buffer output-buffer)
;;     (shell-command (concat "cd " (shell-quote-argument dir) "; cvs update") t))
;;   (update-all-buffers))

;; originally from Greg Badros
(defun update-all-buffers ()
  "Revert all unmodified buffers from disk."
  (interactive)
  (save-excursion
    (mapcar (function (lambda (b)
                        (if (and (not (buffer-modified-p))
                                 (buffer-file-name b)
                                 (file-readable-p (buffer-file-name b))
                                 (not (verify-visited-file-modtime b)))
                            (progn
                              (set-buffer b)
                              (revert-buffer 'IGNORE-AUTO 'NOCONFIRM)))))
            (buffer-list))))

(defun delete-netscape-lock ()
  "Delete Netscape lock file."
  (interactive)
  (delete-file-noerr "~/.netscape/lock")
  ;; Need to generalize for any *.default directory, not jus fnlfahkw
  (delete-file-noerr "~/.mozilla/firefox/fnlfahkw.default/.parentlock")
  )

(defun delete-file-noerr (file)
  "Try to delete FILE, but throw no error if it cannot be deleted."
  ;; I can't wrap this in
  ;;   (if (file-exists-p (expand-file-name file)) ...)
  ;; because that returns nil if the file exists but is a symlink to a
  ;; non-existent file.  In that case, the delete-file call does not err.
  (condition-case err
      (delete-file (substitute-in-file-name file))
    (error nil)))



;;; As of 11/2009, ack is broken (ignores some files it shouldn't).
;;; Also, it doesn't search inside compressed files.
;; (autoload 'ack "ack" "Run ack to search through files" t)
;; (if (eq system-site 'laptop)
;;     (setq ack-command "ack-grep"))
;; (setq ack-default-flags "-i")


(require 'ag nil 'noerror)
(setq ag-regexp-default t)              ; default to regexp search
;; Some VCS ignore files (such as .gitignore) indicate generated files that
;; should be ignored, but others should be searched; ag's default of ignoring
;; everything mentioned in an ignore file is too extreme.
;; TODO: What is an example of what should be searched?
;; (eval-after-load "ag"
;;   '(setq ag-arguments (cons "--skip-vcs-ignores" ag-arguments)))


;; In general, use the "ack" program instead.  But, it doesn't search
;; compressed files and has other problems, so fall back to "search" on
;; occasion.
;; For the "search" Perl program; the Emacs function was called `search'.
;; Fixes submitted to jfriedl@omron.co.jp 8/31/97
(defun sgrep (what dir)
  "Run search with all grep goodies.
Find WHAT in any file in or under DIR."
  (interactive "sSearch for: \nDSearch under: ")
  (if (equal "" what)
      (error "Empty string passed as argument to sgrep"))
  (let ((default-directory (file-name-as-directory dir)))
    ;;not necessary any more? (require 'compile)
    (let* ((quoted-what (if (string-match "^'.*'$" what)
                        what
                      (if (string-match "'" what)
                          (concat "\"" what "\"")
                        (concat "'" what "'"))))
        (command (concat "search -i -n "
                            (if (string-match "^-" what) "-e " "")
                         quoted-what)))
      ;; Old version
      ;; (compile-internal command
      ;;                   "No more search hits" "grep" nil grep-regexp-alist)
      (compilation-start command 'grep-mode)
      )))

(defun strip-line-numbers ()
  "Remove line numbers from error messages in current buffer, for easier comparison."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([^ :]+:\\)[0-9]+" nil t)
      (replace-match "\\1"))))

(defun delete-long-lines (&optional arg)
  "Delete lines longer than 80 (or prefix argument ARG) characters.
Applies to lines after point, but does not move point."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 80))
  (save-excursion
    (while (not (eobp))
      (if (> (- (progn (end-of-line) (point))
                (progn (beginning-of-line) (point)))
             arg)
          (kill-line 1)
        (forward-line)))))

(defun delete-short-lines (&optional arg)
  "Delete lines shorter than 80 (or prefix argument ARG) characters.
Applies to lines after point, but does not move point."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 80))
  (save-excursion
    (while (not (eobp))
      (if (< (- (progn (end-of-line) (point))
                (progn (beginning-of-line) (point)))
             arg)
          (kill-line 1)
        (forward-line)))))

;; should extend this to work only over the current region
(defun remove-tex-comments ()
  "Remove TeX/LaTeX comment from the document."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[^\\]%" nil t)
      (delete-region (1+ (match-beginning 0)) (progn (end-of-line) (point))))))

(defun find-repeated-words ()
  "Find duplicated/repeated/doubled words, such as \"the the\"."
  (interactive)
  (tags-search "\\b\\(\\w+\\)\\W\\1\\b"))

(defmacro no-err (form)
  "Execute FORM, suppressing errors.
If an error occurs, the result value is nil."
  `(condition-case nil
       ,form
     (error nil)))
;; (macroexpand '(no-err (foo bar baz)))


(defun orphaned-elc-files ()
  "List .elc files on `load-path' for which no .el file exists in the directory."
  (let ((dirs (remove-duplicates load-path :test 'equal))
        (result '()))
    (while dirs
      (let* ((all-files (and (file-readable-p (car dirs))
                             (directory-files (car dirs) nil "\\.elc?")))
             (files all-files))
        (while files
          (let ((file (car files)))
            (if (string-match "\\.elc$" file)
                (if (not (member (substring file 0 -1) all-files))
                    (setq result (cons (concat (car dirs) "/" file) result)))))
          (setq files (cdr files))))
      (setq dirs (cdr dirs)))
  result))
;; (orphaned-elc-files)


;; From: bjaspan@athena.mit.edu (Barr3y Jaspan)
(defun mapline (beg end f &rest args)
  "With the point set to the beginning of each line between BEGIN and
END, apply FUNCTION to ARGS and return a list of the result."
  (let* ((p (point-marker))
        (mlist (list 'mlist))
        (mtail (last mlist)))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (beginning-of-line)
      (while (< (point) (point-max))
        (save-excursion
          (setcdr mtail (list (apply f args)))
          (setq mtail (cdr mtail)))
        (forward-line 1))
      )
    (goto-char p)
    (cdr mlist)))


(autoload 'time-less-p "time-date")

(autoload 'file-contents "util-mde")


(defvar ical-business-hours "8:30am-5pm")
(defvar ical-business-hours-phone ical-business-hours)
;; To permit evening calls.
;; (setq ical-business-hours-phone "8:30am-5pm,7:30pm-9:30pm")

;; Implementation that uses the iCal format
(defun ical-available (&optional days start-date timezone2)
  "Insert a summary of my available times from ical.
Optional prefix argument DAYS is days how many days to show (default 8).
With just C-u prefix argument, prompt for starting date and days."
  (interactive "P")
  (let* ((ical-args
          (progn
            (if (equal days '(4))
                (setq start-date (read-from-minibuffer "Start date: " nil nil nil nil "today")
                      days (read-number "Days: " 8)
                      timezone2 (read-from-minibuffer "Timezone2: ")))
            (if (not (numberp days))
                (setq days 8))
            (if (equal start-date "")
                (setq start-date nil))
            (if (equal timezone2 "")
                (setq timezone2 nil))
            (append
             (apply #'append
             (mapcar #'(lambda (fname)
                      (let ((f (expand-file-name fname)))
                        (if (file-exists-p f)
                            (list "--iCal-URL" (bbdb-string-trim (file-contents f))))))
                  '("~/private/iCal-url1" "~/private/iCal-url2" "~/private/iCal-url3")))
             (list "--days" (format "%s" days))
             (if start-date
                 (list "--date" start-date))
             (if timezone2
                 (list "--timezone2" timezone2))
             (list "--business-hours" (if timezone2
                                          ical-business-hours-phone
                                        ical-business-hours))))))
    (let ((old-point (point)))
      ;; (message "java %s" (cons "plume.ICalAvailable" ical-args))
      (insert (apply #'call-process "java" nil t nil (append (list "-cp" (substitute-in-file-name "$HOME/bin/src/plume-lib/java/plume.jar") "-Dical4j.parsing.relaxed=true" "-Dical4j.parsing.relaxed=true" "plume.ICalAvailable") ical-args)))
      (if (or (= (char-before) 0) (= (char-before) 1) (= (char-before) 255))
          (delete-backward-char 1))
      ;; Clean up an irritating warning message.
      (save-excursion
        (goto-char old-point)
        (if (looking-at "[A-Z][a-z][a-z] [0123]?[0-9], 20[0-9][0-9] .* net.fortuna.ical4j.util.Configurator <clinit>\nINFO: ical4j.properties not found.\n")
            (replace-match ""))))))


;; (defvar ical-available-start-hour "08:30")
;; (defvar ical-available-end-hour "17:00")
;;
;; ;; Implementation that uses the Unix ical program.
;; (defun ical-available-old (&optional days start-date)
;;   "Insert a summary of my available times from ical.
;; Optional prefix argument DAYS is days how many days to show (default 8).
;; With just C-u prefix argument, prompt for starting date and days."
;;   (interactive "P")
;;   ;; (error "Should rewrite to grab ical file from Google via the web, then query that.")
;;   (let* ((ical-args
;;        (progn
;;          (if (equal days '(4))
;;              (setq start-date (read-from-minibuffer "Start date: ")
;;                    days (read-from-minibuffer "Days: ")))
;;          (if (or (not days) (equal days ""))
;;              (setq days 8))
;;          (if (or (not start-date) (equal start-date ""))
;;              (setq start-date "today"))
;;          (list "--date" start-date
;;                "--days" (format "+%s" days)
;;                "--starthour" ical-available-start-hour
;;                "--endhour" ical-available-end-hour))))
;;     (insert (apply #'call-process "ical-available" nil t nil ical-args))
;;     (if (= (char-before) 0)
;;      (delete-backward-char 1))))

;; ;; ical-summarize is deprecated:  use ical-available instead.
;; (defun ical-summarize (&optional days start-date)
;;   "Insert a summary of my appointments from ical.
;; Optional prefix argument DAYS is days how many days to show (default 7).
;; With just C-u prefix argument, prompt for starting date and days."
;;   (interactive "P")
;;   (let* ((ical-args
;;        (progn
;;          (if (equal days '(4))
;;              (setq start-date (read-from-minibuffer "Start date: ")
;;                    days (read-from-minibuffer "Days: ")))
;;          (if (or (not days) (equal days ""))
;;              (setq days 7))
;;          (append (list "-show" (format "+%s" days))
;;                  (if (or (not start-date) (equal start-date ""))
;;                      '()
;;                    (list "-date" start-date)))))
;;       (result (with-current-buffer (get-buffer-create " *ical*")
;;                 (erase-buffer)
;;                 (apply #'call-process "ical" nil (current-buffer) nil ical-args)
;;                 (goto-char (point-min))
;;                 (ical-cleanup)
;;                 (buffer-string))))
;;     (cond ((string-equal result "")
;;         (message "No appointments"))
;;        ((string-equal "Usage: ical [options]" (substring result 0 21))
;;         (error "Bad ical command line: ical %s" ical-args))
;;        (t
;;         (insert result)))))
;;
;; (defun ical-cleanup ()
;;   "Compress output of ical to list only times unavailable.
;; Does not show what the commitments are, and merges adjacent times."
;;   (interactive)
;;   (error "Should rewrite to process ical file from Google via the web, then query that.")
;;   (save-excursion
;;     ;; Mark all times by prefixing with "\22ICAL_TIMES: "
;;     (goto-char (point-min))
;;     (while (re-search-forward "^\\( \\* \\)?\\(.*[ap]m to .*[ap]m\n\\)" nil t)
;;       (replace-match "\22ICAL_TIMES: \\2"))
;;     ;; Get rid of content of appointments (text following a marker)
;;     (goto-char (point-min))
;;     (while (re-search-forward "\\(^\22ICAL_TIMES: .*[ap]m to .*[ap]m\n\\)\\([^\22\n].*\\(\n\\|\\'\\)\\)+\\(\n\\|\22\\|\\'\\)" nil t)
;;       (replace-match "\\1\\4")
;;       (goto-char (match-beginning 0)))
;;     (goto-char (point-min))
;;     (while (re-search-forward "^\\(\\(Mon\\|Tues\\|Wednes\\|Thurs\\|Fri\\|Satur\\|Sun\\)day \\(January\\|February\\|March\\|April\\|May\\|June\\|July\\|August\\|September\\|October\\|November\\|December\\) [0-9]+, [12][0-9][0-9][0-9]\n\\)\\([^\22\n].*\n\\)+\\(^\n?\22ICAL_TIMES: .*[ap]m to .*[ap]m\n\\)" nil t)
;;       (replace-match "\\1\\5")
;;       (goto-char (match-beginning 0)))
;;     ;; Merge overlapping or adjacent ranges (possibly with one intervening
;;     ;; line; should generalize that test)
;;     (goto-char (point-min))
;;     (while (re-search-forward
;;          (concat "^\22ICAL_TIMES: \\(.*[ap]m\\) to \\(.*[ap]m\\)\n\n?"
;;                  "\22ICAL_TIMES: \\(.*[ap]m\\) to \\(.*[ap]m\\)\n")
;;          nil t)
;;       (let* ((match-begin (match-beginning 0))
;;           (start1-string (match-string 1))
;;           (start1 (apply 'encode-time (parse-time-string-ampm start1-string)))
;;           (end1-string (match-string 2))
;;           (end1 (apply 'encode-time (parse-time-string-ampm end1-string)))
;;           (start2-string (match-string 3))
;;           (start2 (apply 'encode-time (parse-time-string-ampm start2-string)))
;;           (end2-string (match-string 4))
;;           (end2 (apply 'encode-time (parse-time-string-ampm end2-string))))
;;      (assert (not (time-less-p start2 start1)))
;;      (if (not (time-less-p end1 start2))
;;          (let ((end-string (if (time-less-p end1 end2) end2-string end1-string)))
;;            (replace-match (concat "\22ICAL_TIMES: "
;;                                   start1-string " to "
;;                                   end-string "\n"))
;;            (goto-char match-begin))
;;        (goto-char (1+ match-begin)))))
;;     ;; Remove item marks
;;     (goto-char (point-min))
;;     (while (re-search-forward "\\(^\n?\22ICAL_TIMES: \\)" nil t)
;;       (replace-match ""))
;;     ))
;;
;; (autoload 'parse-time-string "parse-time")
;;
;; (autoload 'firstn "util-mde")
;;
;; (defun parse-time-string-ampm (string)
;;   "Call `parse-time-string' on STRING, which is in the form \"HH:MM[ap]m\"."
;;   (let ((ampm (substring string -2))
;;      (result (append (firstn 3 (parse-time-string (substring string 0 -2)))
;;                      (cdddr (decode-time)))))
;;     (if (= 12 (nth 2 result))
;;      (setf (nth 2 result) 0))
;;     (cond ((equal "am" ampm)
;;         result)
;;        ((equal "pm" ampm)
;;         (setf (nth 2 result)
;;               (+ 12 (nth 2 result)))
;;         result)
;;        (t
;;         (error "Bad time %s" string)))))
;; ;; (equal (parse-time-string-ampm "11:16am") (parse-time-string "11:16"))
;; ;; (equal (parse-time-string-ampm "1:16pm") (parse-time-string "13:16"))


(defun swap-backspace-and-delete ()
  "Swap the backspace and delete keys, via `keyboard-translate'.
This is particularly useful when they are incorrectly set, as on a TTY.
Also consider `normal-erase-is-backspace' variable (Emacs 21)."
  (interactive)
  (keyboard-translate ?\C-h ?\C-?)  ; translate `C-h' to DEL
  (keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'.
  )
(if (and (equal (getenv "DISPLAY") "localhost:10.0")
         (equal (getenv "TERM") "vt100"))
    (swap-backspace-and-delete))


(defalias 'ediff-regions 'ediff-regions-wordwise)


;; Manipulation of the other window.

(defun kill-buffer-other-window (arg)
  "Kills the buffer in ARG'th next window."
  (interactive "p")
  (let ((current-window (selected-window)))
    (other-window arg)
    (kill-buffer (current-buffer))
    (select-window current-window)))

(defun kill-other-buffer-and-window (arg)
  "Kills the ARG'th next buffer and window."
  (interactive "p")
  (let ((current-window (selected-window)))
    (other-window arg)
    (kill-buffer (current-buffer))
    (delete-window)
    (select-window current-window)))

; [?] ; These are already set.
(global-set-key "\C-x4k" 'kill-buffer-other-window)      ; C-x 4 k
(global-set-key "\C-x4w" 'kill-other-buffer-and-window)  ; C-x 4 w

(defun insert-other-window ()
  "Insert other window at point."
  (interactive)
  (insert-buffer-substring (window-buffer (next-window)))
  (exchange-point-and-mark))

(defun remove-text-properties-region (begin end)
  "Remove all text properties from the region."
  (interactive "r")
  (set-text-properties begin end nil (current-buffer)))


(defun infer-tab-width (&optional omit-first-column)
  "Set tab-width so that columns line up.
The first column is omitted if the optional argument is specified."
  (interactive "P")
  (let ((max-width -1)
        (max-width-text "")
        (column-regexp (concat (if omit-first-column
                                   "\t"
                                 "[\t\n]")
                               "\\(.*?\\)\t")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward column-regexp nil t)
        (if (< max-width (- (match-end 1) (match-beginning 1)))
            (progn
              (setq max-width (- (match-end 1) (match-beginning 1))
                    max-width-text (match-string 1))))
        (backward-char 1)))
    (if (= max-width -1)
        (message "no tab-separated columns found")
      (progn
        (setq tab-width (+ max-width 1))
        (message "tab-width set to %d because of %s" tab-width max-width-text)))))


(defun pdf-fixup-region (beg end)
  "Fix ligatures that resulted from cutting PDF text and pasting into Emacs."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (search-forward "¯" nil t)
      (replace-match "fi" nil t))
    (goto-char beg)
    (while (search-forward "®" nil t)
      (replace-match "ff" nil t))
    (goto-char beg)
    (while (search-forward "±" nil t)
      (replace-match "ffi" nil t))
    (goto-char beg)
    (while (search-forward "|" nil t)
      (replace-match " -- " nil t))
    (goto-char beg)
    (while (re-search-forward "\\([a-z]-\\) \\([a-z]\\)" nil t)
      (replace-match "\\1\\2"))
    (goto-char beg)
    (while (re-search-forward "\\([ \n]\\)\\\\\\([a-z]+\"\\)" nil t)
      (replace-match "\\1\"\\2"))
    ))


;; This is intended for us in ~/random/addresses.tex
;; Example use:
;:   (make-local-variable 'after-change-functions)
;;   (add-hook 'after-change-functions 'latex-timestamp-paragraph)
;; MINOR PROBLEM: Edits between paragraphs get recorded in the next one.
;; MAJOR PROBLEM: This screws up undo information (and even revert-buffer).
;; Thus, it may not be worth using.
;; (Unfortunately, I don't have CVS info for ~/random/addresses.tex.)
(defun latex-timestamp-paragraph (beg end pre-change-length)
  "Add/update, after a paragraph, a LaTeX comment containins the current date.
This is good for indicating when the paragraph was last edited.
You can add this function to `after-change-hooks'."
  (save-excursion
    (end-of-paragraph-text)
    (beginning-of-line)
    (if (looking-at "% [01]?[0-9]/[0-9][0-9][0-9]?[0-9]?")
        (delete-region (match-beginning 0) (match-end 0))
      (progn
        (forward-line 1)
        (insert "\n")
        (backward-char 1)))
    (insert "% ")
    (insert (format-time-string "%m/%Y"))))

(defun insert-timestamp ()
  "Insert the current time at point."
  (interactive)
  (insert (current-time-string)))

(defun replace-string-noninteractive (from-string to-string)
  "A non-interactive version of `replace-string', which see."
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))


(defun regexp-remove-alternative (alternative whole-regexp)
  "Remove regexp ALTERNATIVE from regexp WHOLE-REGEXP.
Not guaranteed to work in all cases."
  (flet ((grouped (re) (concat "\\(" re "\\)")))
    (let* ((qalternative (regexp-quote alternative))
           (qopen (regexp-quote "\\("))
           (gqopen (grouped (concat qopen "\\|^")))
           (qclose (regexp-quote "\\)"))
           (gqclose (grouped (concat qclose "\\|$")))
           (qalt (regexp-quote "\\|"))
           (gqalt (grouped qalt))
           del-start del-end)
      (cond  ((string-match (concat gqalt qalternative qalt) whole-regexp)
             (setq del-start (match-end 1)
                   del-end (match-end 0)))
             ((string-match (concat gqopen qalternative qalt) whole-regexp)
             (setq del-start (match-end 1)
                   del-end (match-end 0)))
            ((string-match (concat (grouped (concat qalt qalternative)) gqclose) whole-regexp)
             (setq del-start (match-beginning 0)
                   del-end (match-end 0))))
      (if del-start
          (concat (substring whole-regexp 0 del-start)
                  (substring whole-regexp del-end))
        whole-regexp))))
;; Testing:
;; (regexp-remove-alternative "Return-Path:" mail-yank-ignored-headers)
;; (regexp-remove-alternative "Sender:" mail-yank-ignored-headers)
;; (regexp-remove-alternative "^references:" mail-yank-ignored-headers)


;; For documentatino:  C-h I METHOD <RET>
;; "!/" -> "¡"     "?/" -> "¿"
(defun spanish-postfix-input-method ()
  "Set input method to \"spanish-postfix\"."
  (interactive)
  (set-input-method "spanish-postfix"))


(defun offer-to-change-if-read-only ()
  (if buffer-read-only
      (progn
        (if (y-or-n-p "Buffer is read-only.  Make buffer modifiable? ")
            (setq buffer-read-only nil))))
  (barf-if-buffer-read-only))


(defadvice flush-lines (before make-buffer-modifiable activate)
  (interactive
   (progn
     (offer-to-change-if-read-only)
     (keep-lines-read-args "Flush lines containing match for regexp"))))

(defadvice keep-lines (before make-buffer-modifiable activate)
  (interactive
   (progn
     (offer-to-change-if-read-only)
     (keep-lines-read-args "Keep lines containing match for regexp"))))


;; The defaliases and defun are copied verbatim from flush-lines, then
;; replace each instance of "(forward-line 0)" by "(backward-paragraph)"
;; and then "line" by "paragraph".  Not well tested.  One known bug:
;; keep-paragraphs always seems to keep the first paragraph even if it
;; doesn't contain the regexp.
;; An alternate technique that works but is ugly:
;;   (goto-char (point-min))
;;   (replace-string "\C-j\C-j" "<<<PARBREAK>>>")
;;   (goto-char (point-min))
;;   (replace-string "\C-j" "<<<LINEBREAK>>>")
;;   (goto-char (point-min))
;;   (replace-string "<<<PARBREAK>>>" "\C-j")
;;   (goto-char (point-min))
;;   (delete-non-matching-lines regexp)
;;   (goto-char (point-min))
;;   (replace-string "\C-j" "\C-j\C-j")
;;   (goto-char (point-min))
;;   (replace-string "<<<LINEBREAK>>>" "\C-j")
(defalias 'delete-non-matching-paragraphs 'keep-paragraphs)
(defalias 'delete-matching-paragraphs 'flush-paragraphs)
(defun keep-paragraphs (regexp &optional rstart rend interactive)
  "Delete all paragraphs except those containing matches for REGEXP.
A match split across paragraphs preserves all the paragraphs it lies in.
When called from Lisp (and usually interactively as well, see below)
applies to all paragraphs starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
This command operates on (the accessible part of) all paragraphs whose
accessible part is entirely contained in the region determined by RSTART
and REND.  (A newparagraph ending a paragraph counts as part of that paragraph.)

Interactively, in Transient Mark mode when the mark is active, operate
on all paragraphs whose accessible part is entirely contained in the region.
Otherwise, the command applies to all paragraphs starting after point.
When calling this function from Lisp, you can pretend that it was
called interactively by passing a non-nil INTERACTIVE argument.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."

  (interactive
   (progn
     (offer-to-change-if-read-only)
     (keep-lines-read-args "Keep paragraphs (containing match for regexp)")))
  (if rstart
      (progn
        (goto-char (min rstart rend))
        (setq rend
              (progn
                (save-excursion
                  (goto-char (max rstart rend))
                  (unless (or (bolp) (eobp))
                    (backward-paragraph))
                  (point-marker)))))
    (if (and interactive transient-mark-mode mark-active)
        (setq rstart (region-beginning)
              rend (progn
                     (goto-char (region-end))
                     (unless (or (bolp) (eobp))
                       (backward-paragraph))
                     (point-marker)))
      (setq rstart (point)
            rend (point-max-marker)))
    (goto-char rstart))
  (save-excursion
    ;; MDE CHANGE:  was "(or (bolp)"
    (or (bobp) (forward-paragraph 1))
    (let ((start (point))
          (case-fold-search  (and case-fold-search
                                  (isearch-no-upper-case-p regexp t))))
      (while (< (point) rend)
        ;; Start is first char not preserved by previous match.
        (if (not (re-search-forward regexp rend 'move))
            (delete-region start rend)
          (let ((end (save-excursion (goto-char (match-beginning 0))
                                     (backward-paragraph)
                                     (point))))
            ;; Now end is first char preserved by the new match.
            (if (< start end)
                (delete-region start end))))

        (setq start (save-excursion (forward-paragraph 1) (point)))
        ;; If the match was empty, avoid matching again at same place.
        (and (< (point) rend)
             (= (match-beginning 0) (match-end 0))
             (forward-char 1)))))
  (set-marker rend nil)
  nil)
(defun flush-paragraphs (regexp &optional rstart rend interactive)
 "Delete paragraphs containing matches for REGEXP.
When called from Lisp (and usually when called interactively as
well, see below), applies to the part of the buffer after point.
The paragraph point is in is deleted if and only if it contains a
match for regexp starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
Paragraphs partially contained in this region are deleted if and only if
they contain a match entirely contained in it.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.  When calling this function
from Lisp, you can pretend that it was called interactively by passing
a non-nil INTERACTIVE argument.

If a match is split across paragraphs, all the paragraphs it lies in are deleted.
They are deleted _before_ looking for the next match.  Hence, a match
starting on the same paragraph at which another match ended is ignored."

  (interactive
   (progn
     (offer-to-change-if-read-only)
     (keep-lines-read-args "Flush paragraphs containing match for regexp")))
  (if rstart
      (progn
        (goto-char (min rstart rend))
        (setq rend (copy-marker (max rstart rend))))
    (if (and interactive transient-mark-mode mark-active)
        (setq rstart (region-beginning)
              rend (copy-marker (region-end)))
      (setq rstart (point)
            rend (point-max-marker)))
    (goto-char rstart))
  (let ((case-fold-search (and case-fold-search
                               (isearch-no-upper-case-p regexp t))))
    (save-excursion
      (while (and (< (point) rend)
                  (re-search-forward regexp rend t))
        (delete-region (save-excursion (goto-char (match-beginning 0))
                                       (backward-paragraph)
                                       (point))
                       (progn (forward-paragraph 1) (point))))))
  (set-marker rend nil)
  nil)


;; TODO:  handle multiple flights on a single day.
;; TODO:  handle hotel & car
;; TODO:  retain "operated by", which is actually important
(defun travel-summarize ()
  "Summarize an itinerary such as that sent to me by Jerry Constantino."
  (interactive)
  (while (re-search-forward
          (concat "^ *\\([0-9]+ [A-Z][A-Z][A-Z] [0-9][0-9]\\) - \\([A-Z]*DAY\\) *\n\n?"
                  " *\\(.*?\\) * \\([0-9]+\\).*\n\n?"
                  "\\( *\\(DEPART TERMINAL\\).*\n\n?\\)*"
                  " *LV: \\(.*?\\) *  \\([0-9]+[AP]\\).*\n\n?"
                  " *AR: \\(.*?\\) *  \\([0-9]+[AP]\\).*\n\n?"
                  "\\( *\\(ARRIVAL TERMINAL\\|DINNER\\|FOOD TO PURCHASE\\|FREQ FLYER\\|OPERATED BY\\|REFRESHMENTS\\|SEAT\\).*\n\n?\\)*"
                  " *AIRLINE LOCATORS: .*-\\([A-Z0-9]+\\) *\n"
                  )
          nil t)
  (replace-match (concat
                  (capitalize (substring (match-string 2) 0 3))
                  " "
                  (capitalize (match-string 1))
                  " "
                  (capitalize (match-string 3))
                  " "
                  (match-string 4)
                  " "
                  (capitalize (match-string 7))
                  " "
                  (match-string 8)
                  " -> "
                  (capitalize (match-string 9))
                  " "
                  (match-string 10)
                  " [locator: "
                  (match-string 13)
                  "]"
                  )
                 'fixedcase)))


;; Create drop-in replacements for all-completions and try-completion that
;; use partial completion.  Surprisingly, no drop-in replacements are
;; defined, either in complete.el (defines partial-completion, but mostly
;; operates directly on the minibuffer) or in minibuffer.el (defines
;; completion-pcm-*).
;;
;; These still are not quite right (don't put cursor in right place, deal
;; badly when no completions exist, insert extra hyphens), but they are
;; already an improvement.
(defun all-completions-partial (string collection &optional predicate hide-spaces)
  (let ((result
         (completion-pcm--find-all-completions string collection predicate (length string))))
    ;; (message "all-completions-partial: %s %s %s %s => %s" string collection predicate hide-spaces result)
    (second result)))
(defun try-completion-partial (string collection &optional predicate)
  (let ((result
         (completion-pcm-try-completion string collection predicate (length string))))
    ;; (message "try-completion-partial %s %s %s => %s" string collection predicate result)
    (if (eq t result)
        string
      (car result))))

;; Permit partial completion versions to override original when called from
;; certain functions, for all-completions and try-completion.

(defvar partial-completion-context-functions nil
  "A list of functions (symbols) in which `all-completions' and
`try-completion' should do partial completion.")
(defvar partial-completion-context-depth 15
  "The number of stack frames to examine looking for a match to
`partial-completion-context-functions'.  nil means no limit.")

(defadvice all-completions (around maybe-partial activate)
  "Use partial completion, depending on the backtrace."
  ;; (message "") (backtrace)
  (if (and (in-backtrace partial-completion-context-functions
                         partial-completion-context-depth)
           (not (in-backtrace '(all-completions-partial))))
      (setq ad-return-value (apply 'all-completions-partial (ad-get-args 0)))
    ad-do-it))
(defadvice try-completion (around maybe-partial activate)
  "Use partial completion, depending on the backtrace."
  ;; (message "") (backtrace)
  (if (and (in-backtrace partial-completion-context-functions
                         partial-completion-context-depth)
           (not (in-backtrace '(try-completion-partial))))
      (setq ad-return-value (apply 'try-completion-partial (ad-get-args 0)))
    ad-do-it))

(defun try-completion-tester ()
  "Does partial completion."
  (let ((partial-completion-context-functions '(try-completion-tester)))
    (try-completion "mich ern" '("michael ernst"))))
;; (assert (string-equal (try-completion-tester) "michael ernst"))
(defun try-completion-tester2 ()
  "Does not do partial completion."
  (try-completion "mich ern" '("michael ernst")))
;; (assert (not (try-completion-tester2)))

(defun in-backtrace (functions &optional depth)
  "Return t if a function in `functions' is on the backtrace
at depth less than `depth'.  `functions' is a list of symbols."
  (if (null functions)
      nil
    (let ((i 0)
          (result nil)
          (done nil))
      (while (and (or (not depth) (< i depth))
                  (not done))
        (let ((frame (backtrace-frame i)))
          (if (not frame)
              (setq done t)
            (progn
              (setq result (member (second frame) functions))
              (setq done result))))
        (setq i (1+ i)))
      result)))
;; (assert (null (in-backtrace '(foo) 100)))
;; (assert (in-backtrace '(eval-last-sexp) 15))


(setq partial-completion-context-functions
      '(bbdb-complete-name
        mew-draft-addrbook-expand
        mew-complete
        mew-complete-2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advices
;;;

(defadvice scroll-up (around end-of-buffer-maybe activate)
  "If on the last screenful of a buffer but not at the end, go to the end.
By default, `scroll-up' raises an error in that circumstance."
  (condition-case err
      ad-do-it
    (end-of-buffer (if (eobp)
                       (signal (car err) (cdr err))
                     (goto-char (point-max))))))

(defadvice scroll-down (around beginning-of-buffer-maybe activate)
  "If on the first screenful of a buffer but not at the beginning,
go to the beginning.
By default, `scroll-down' raises an error in that circumstance."
  (condition-case err
      ad-do-it
    (beginning-of-buffer (if (bobp)
                             (signal (car err) (cdr err))
                           (goto-char (point-min))))))


;; Should be made to work for obarrays (which less frequently have duplicates).
;; In general, this completely the wrong thing to do, however:  fix the caller.
(defadvice completing-read (before prune-duplicates activate)
  "Remove duplicates from completion table."
  (if (and (listp (ad-get-arg 1))
           ;; Test needed for, for example, fi::clman-big-oblist in fi-clman.el
           (< (length (ad-get-arg 1)) 1000))
      ;; not string=
      (progn
        (require 'cl)
        (eval-when-compile (require 'cl))
        (ad-set-arg 1 (remove-duplicates (ad-get-arg 1) ':test (function equal))))))


(defun emacs-source-file-p (filename)
  "Return t if FILENAME is an Emacs source file."
  (or (string-match "/emacs/x?lisp/" filename)
      (string-match "emacs[-/][0-9]+\.[0-9]+\\(\.[0-9]+\\)?/\\(lisp\\|src\\)/" filename)
      ;; (string-match "local/src/emacs-19" (buffer-file-name))
      ;; (string-match "lib/emacs/local-lisp/w3" (buffer-file-name)))
      ))


(defadvice bdiff (around emacs-diff-context activate)
  "For Emacs patches, use context diff format rather than unidiff format."
  (let* ((filename (if (stringp (ad-get-arg 0))
                       (expand-file-name (ad-get-arg 0))
                     (buffer-file-name)))
         (patch-p (and filename
                       (eq bdiff-context-lines 'unidiff)
                       (emacs-source-file-p filename)
                       (y-or-n-p "Use context diff format (for patch submission)? ")))
         (bdiff-context-lines (if patch-p
                                  2
                                bdiff-context-lines))
         (bdiff-ignore-whitespace (if patch-p
                                      nil
                                    bdiff-ignore-whitespace)))
    ad-do-it))

(defadvice diff (around emacs-diff-context activate)
  "For Emacs patches, use context diff format rather than unidiff format."
  (let* ((filename (if (stringp (ad-get-arg 0))
                       (expand-file-name (ad-get-arg 0))
                     (buffer-file-name)))
         (patch-p (and filename
                       (not (equal diff-switches "-c"))
                       (emacs-source-file-p filename)
                       (y-or-n-p "Use context diff format (for patch submission)? ")))
         (diff-switches (if patch-p "-c" diff-switches)))
    ad-do-it))

;; Emacs 20.2.99 introduced `compare-with-file', so I need another way to type
;; `compare-windows' conveniently.
(defalias 'wdiff 'compare-windows)


;; (defadvice add-change-log-entry (around emacs-change-log activate)
;;   "For Emacs changes, use my permanent personal email address."
;;   (let* ((filename (buffer-file-name))
;;       (add-log-mailing-address
;;        (if (and filename
;;                 (or (string-match "/emacs/lisp/" filename)
;;                     (string-match "emacs/\d+\.\d+/lisp/" filename)))
;;            "mernst@alum.mit.edu"
;;          add-log-mailing-address)))
;;     ad-do-it))


(defadvice compilation-find-file (before substitute-env-vars activate)
  "Call `substitute-in-file-name' if first character of argument is `$'."
  (if (= ?$ (elt (ad-get-arg 1) 0))
      (ad-set-arg 1 (substitute-in-file-name (ad-get-arg 1)))))


;;; File groups

;; (Is this still necessary as of Emacs 21.2?)

;; I need to advise write-region as well, because it may be called directly
;; rather than through basic-save-buffer.

;; To disable: (ad-disable-advice 'basic-save-buffer-2 'around 'preserve-group-id)
(defadvice basic-save-buffer-2 (around preserve-group-id activate)
  "Try to set the group of the file to what it used to be on disk."
  (let ((old-group (and (file-exists-p buffer-file-name)
                        (nth 3 (file-attributes buffer-file-name)))))
    ad-do-it
    (if (and old-group
             (not (= old-group (nth 3 (file-attributes buffer-file-name)))))
        (call-process "chgrp" nil 0 nil
                      (int-to-string old-group) buffer-file-name))))


;;; Three failed implementations of preserve-group-id.

;; This doesn't work:  when write-region is called by basic-save-buffer-2,
;; the original disk file no longer exists, having been moved aside.
;; (defadvice write-region (around preserve-group-id activate)
;;   "Try to set the group of the file to what it used to be on disk."
;;   (let ((old-group (and (file-exists-p (ad-get-arg 2))
;;                      (nth 3 (file-attributes (ad-get-arg 2))))))
;;     ad-do-it
;;     (if old-group
;;      (call-process "chgrp" nil 0 nil
;;                    (int-to-string old-group) (ad-get-arg 2)))))

;; This can't be an advice to basic-save-buffer because we may not know the
;; file name until midway through the function, when we certainly call
;; write-contents-hook and possibly call basic-save-buffer-1.  So it could
;; be an advice to basic-save-buffer-[12] or, better, to write-region.
;; Another possibility would be to have write-contents-hook remember the
;; old group and after-save-hook set the group.
;; (defadvice basic-save-buffer-2 (around preserve-group-id activate)
;;   "Try to set the group of the file to what it used to be on disk."
;;   (let* ((filename buffer-file-name)
;;          (old-group (and (file-exists-p filename)
;;                      (nth 3 (file-attributes filename)))))
;;     ad-do-it
;;     (if old-group
;;      (call-process "chgrp" nil 0 nil
;;                    (int-to-string old-group) filename))))

;; This works, but it took forever to debug, mostly stupid errors masked by
;; the fact that hooks don't let errors through.
;; (defvar old-group nil
;;   "Cons of (filename . group) for disk version of a file.")
;;
;; (defun remember-old-group ()
;;   "A `write-contents-hook' which saves the `buffer-file-name' and file group id."
;;   (setq old-group (and (file-exists-p buffer-file-name)
;;                     (cons buffer-file-name
;;                           (nth 3 (file-attributes buffer-file-name)))))
;;   (message "called remember-old-group")
;;   ;; Return nil: we didn't save the file
;;   nil)
;;
;; (defun use-old-group ()
;;   "An `after-save-hook', to reset the file's group to what it used to be.
;; Function `remember-old-group' should have been called before saving."
;;   (if (and old-group
;;         (eq buffer-file-name (car old-group)))
;;       (progn
;;      (call-process "chgrp" nil 0 nil
;;                    (int-to-string (cdr old-group)) buffer-file-name)
;;      (setq old-group nil))))
;;
;; (add-hook 'after-save-hook 'use-old-group)
;; ;; Can't use write-contents-hooks because it's buffer-local.
;; (add-hook 'write-file-hooks 'remember-old-group)


;; It would be better to surround the variable name by braces, not parens.
;; `substitute-in-file-name' deals correctly with braces, and so does the
;; shell; neither handles parens.  But the paren version is still common
;; in Makefiles (even though they, too, support braces), so retain this.
(defadvice substitute-in-file-name (before drop-parens activate)
  "Substitute variables whose names are surrounded by parentheses."
  (let ((fname (ad-get-arg 0)))
    (while (string-match "\\$(\\([A-Za-z_]+\\))" fname)
      (setq fname (concat
                   (substring fname 0 (match-beginning 0))
                   (or (getenv (match-string 1 fname))
                       (error (concat "Substituting nonexistent environment variable \""
                                      (match-string 1 fname)
                                      "\"")))
                   (substring fname (match-end 0)))))
    (ad-set-arg 0 fname)))
;; Testing
;; (substitute-in-file-name "$HOME/.emacs")
;; (substitute-in-file-name "${HOME}/.emacs")
;; (substitute-in-file-name "$(HOME)/.emacs")

(defadvice outline-next-preface (after dont-go-backward activate)
  "Don't print an extra blank line above outline entries that have
one in the source code."
  (if (and (not (looking-at (concat "\n\\(" outline-regexp "\\)")))
           (looking-at (concat "\n\n\\(" outline-regexp "\\)")))
      (forward-char 1)))


;;; Enabled experimentally, 2/23/2005, to see whether it is helpful or annoying.
;;; Jeff Perkins says this causes the "revert buffer" menu item to crash Emacs!
;;; It would be nicer to say:
;;;   file <bla> changed on disk. Reread from disk? (yes or no)
;;; rather than the more obscure:
;;;   Revert buffer from file <bla> (yes or no)
;; Offer to revert the buffer when changing to a buffer that has been
;; modified on disk.
;; (There doesn't seem to be a hook in Emacs that gets called on every buffer
;; switch, which is a shame; I hope I have advised all the relevant functions.)
(defvar disable-maybe-revert-buffer nil)
(defun maybe-revert-buffer ()
  "If file has changed on disk, offer to revert the buffer."
  (if (and (not disable-maybe-revert-buffer)
           (not (buffer-modified-p))
           (not (verify-visited-file-modtime (current-buffer))))
      (revert-buffer)))
(defadvice switch-to-buffer (after maybe-revert-buffer activate)
  "If file has changed on disk, offer to revert the buffer."
  (maybe-revert-buffer))
(defadvice pop-to-buffer (after maybe-revert-buffer activate)
  "If file has changed on disk, offer to revert the buffer."
  (maybe-revert-buffer))
(defadvice other-window (after maybe-revert-buffer activate)
  "If file has changed on disk, offer to revert the buffer."
  (maybe-revert-buffer))
(defadvice next-window (after maybe-revert-buffer activate)
  "If file has changed on disk, offer to revert the buffer."
  (maybe-revert-buffer))
(defadvice previous-window (after maybe-revert-buffer activate)
  "If file has changed on disk, offer to revert the buffer."
  (maybe-revert-buffer))

(defadvice Buffer-menu-bdiff (around disable-maybe-revert-buffer activate)
  (let ((disable-maybe-revert-buffer t))
    ad-do-it))

(defadvice recover-session (around disable-dired-omit activate)
  "Don't omit the very files that we wish to display."
  (require 'dired-x)
  (let ((dired-omit-mode nil))
    ad-do-it))

(defadvice sort-lines (before not-certain-files activate)
  "Don't sort lines in certain files."
  (if (member buffer-file-name
              (list
               (expand-file-name "~/random/addresses.tex")
               (expand-file-name "~/private/to-do")
               (expand-file-name "~/to-do")
               ))
      (error "Sort this file by paragraphs, not by lines")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bibliographies
;;;

;; Add
;;   (save-excursion
;;     (set-buffer "*Shell Command Output*")
;;     (setq buffer-read-only t))
;; to each of these when my buffer-read-only hack to simple.el appears in Emacs.

;; Some non-FSF versions of bibtex.el don't (provide 'bibtex).
;; (eval-when-compile (require 'bibtex))

(defun rolo (string)
  "Find address book entries matching words in STRING."
  (interactive "sArguments to rolo: ")
  ;; This doesn't work for remote Emacses started directly via ssh.
  ;; (But expanding the alias doesn't work in that circumstance either.)
  (shell-command (concat "rolo " (quote-for-shell-command string))))

(defun quote-for-shell-command (arguments)
  ;; Should split arguments then call quote-word-for-shell-command on each,
  ;; but only if the string contains zero or one single-quote and zero or
  ;; one double-quote character.
  ;; The reason is to respect any quotation marks that the user inserted
  ;; intentionally.
  arguments
  )

;;
(defun quote-word-for-shell-command (string)
  (cond ((and (string-match "'" string)
           (not (string-match "\"" string)))
         (setq string (concat "\"" string "\"")))
        ((and (string-match "\"" string)
              (not (string-match "\'" string)))
         (setq string (concat "'" string "'")))
        ((and (string-match "\"" string)
              (not (string-match "\'" string)))
         (error (concat "cannot quote argument for shell command because string contains both single and double quotes: " string)))
        (t
         string)))

(defun quotefind (string)
  "Find quotations matching words in STRING."
  (interactive "sArguments to quotefind: ")
  (shell-command (concat "quotefind " (quote-for-shell-command string))))

;; Advise shell-command-on-region rather than shell-command, because this
;; single advice gets both.
(defadvice shell-command-on-region (around not-in-echo-area activate)
  "Display *Shell Command Output* buffer, don't just show output in echo area.
This is because I usually want to cut-and-paste output from the shell
command, so it is convenient to have that buffer displayed."
  (let ((resize-mini-windows nil))
    ad-do-it))

(eval-when-compile '(require 'thingatpt))

(defun bibfind (string)
  "Find bibliography entries matching words in STRING."
  (interactive
   (let* ((default (word-at-point))
          (user-input (read-string (format "Arguments to bibfind (default %s): "
                                           default))))
     (list (if (string= user-input "") default user-input))))
  (shell-command (concat "bibfind " (quote-for-shell-command string))))


(defun bib-show-properties ()
  "Show the PROPERTIES: paragraph of each BibTeX entry that has one."
  (interactive)
  (save-excursion
    (save-excursion
      (set-buffer (get-buffer-create "bib-properties"))
      (erase-buffer))
    (goto-char (point-min))
    (let (properties prop-end bibentry)
      (while (re-search-forward "^[ \t]*PROPERTIES:" nil t)
        (setq properties (buffer-substring (match-beginning 0)
                                           (progn (search-forward "\n\n")
                                                  (point)))
              prop-end (point)
              bibentry (buffer-substring (progn ;; (beginning-of-bibtex-entry)
                                           (bibtex-beginning-of-entry)
                                           (search-forward "{")
                                           (point))
                                         (progn (end-of-line) (point))))
        (goto-char prop-end)
        (save-excursion
          (set-buffer (get-buffer "bib-properties"))
          (insert bibentry "\n" properties)))))
  (switch-to-buffer "bib-properties")
  (set-buffer-modified-p nil))

(defun bibtex-comments-to-field ()
  "Place comments following BibTeX entries into \"comments\" fields in the entries.

The BibTeX entry must end with } at the beginning of a line, and the
comment must not contain blank lines.  The trailing comment should be
flush left, except for lines which begin paragraphs, which should be
indented with space(s) or tab(s).

This is crude, but works for our files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s *\n}\\s *\n\\<" nil t)
      (replace-match ",\n  comments = \"")
      (let ((comment-start (point)))
        (re-search-forward "\n\\(\\s *\n\\|\\'\\)")
        (replace-match "\"\n}\n\n")
        (save-restriction
          (narrow-to-region comment-start (match-beginning 0))
          (goto-char (point-min))
          ;; Precede quotation marks by a backslash, if there isn't one already.
          ;; (replace-string-noninteractive "\"" "\\\"")
          (while (re-search-forward "\\([^\\]\\)\"" nil t)
            (replace-match "\\1\\\\\""))
          (goto-char (point-min))
          ;; Add paragraph breaks
          (while (re-search-forward "\n[ \t]+" nil t)
            (replace-match "\n\n"))))))
  ;; Protect from accidentally overwriting the original.
  (setq buffer-file-name (concat buffer-file-name "-comments"))
  (message "Done.  Now write it to a new file."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting variable values
;;;

;; These two should take an optional arg to set the variable one way or
;; another.  But that's overkill.

(defun debug-on-error ()
  "Toggle the value of variable `debug-on-error'."
  (interactive)
  (require 'edebug)          ; needed if I'm not running the default edebug
  (setq debug-on-error (not debug-on-error))
  (message (if debug-on-error
               "Debugging on errors enabled."
             "Debugging on errors disabled.")))

(defun truncate-lines ()
  "Toggle the value of variable `truncate-lines'."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (message (if truncate-lines
               "Truncating lines enabled."
             "Truncating lines disabled.")))

(defun case-fold-search ()
  "Toggle the value of variable `case-fold-search'."
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "case-fold-search is %s." case-fold-search))

(defun visible-bell ()
  "Toggle the value of variable `visible-bell'."
  (interactive)
  (setq visible-bell (not visible-bell))
  (message "visible-bell is %s." visible-bell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tags tables
;;;

(defun emacs-tags-table ()
  "Use the Emacs TAGS table."
  (interactive)
  (visit-tags-table "~/emacs/lisp/"))

(defun mde-emacs-tags-table ()
  "Use my Emacs Lisp TAGS table."
  (interactive)
  (visit-tags-table "~/emacs/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading Emacs Lisp
;;;

;; Still needed in Emacs 21.2.  (Maybe the library-complete part is now
;; built in, perhaps under some other name?)
(emacs-fsf               ; built into XEmacs, I think
  ;; Add completing-read to load-library.
  ;; From: hallvard@IFI.UIO.NO (Hallvard B Furuseth)

  (defun load-library (library)
    "Load the library named LIBRARY.
This is an interface to the function `load'."
    (interactive
     (list (completing-read "Load Library: " 'library-complete)))
    (load library))

  (defun library-complete (lib pred-unused all)
    "Search load-path for LIBRARY.  Returns competion list if 3rd arg ALL is t,
otherwise completion of LIBRARY (t if complete, nil if no completions).
2nd arg is unused."
    (let* ((subdir (file-name-directory lib)) ; nil if no directory
           (match (concat "^" (regexp-quote (file-name-nondirectory lib))
                          ".*\\.elc?$"))
           (files (mapcar
                   (function
                    (lambda (n)
                      (substring n 0 (if (string-match "c$" n) -4 -3))))
                   (apply 'nconc
                          (mapcar (function
                                   (lambda (l)
                                     (if subdir
                                         (setq l (expand-file-name subdir l))
                                       (if (null l)
                                           (setq l default-directory)))
                                     (and (file-directory-p l)
                                          (directory-files l nil match))))
                                  load-path)))))
      (and all (cdr load-path)
           (setq files (sort files 'string-lessp)))
      (let ((ptr files)
            (this files))
        (while (setq ptr (cdr ptr))
          (if (equal (car this) (car ptr))
              (setcar ptr nil)
            (setq this ptr)))
        (setq files (delq nil files)))
      (if subdir
          (setq files (mapcar (function (lambda (f)
                                          (expand-file-name f subdir)))
                              files)))
      (if all
          files
        (try-completion lib (mapcar 'list files))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Info
;;;

;;; Replacement for Info:  try to be smart about what manual section to read.
;;; (Emacs 19 has an info-mode-hook, so put this on that.)

(eval-when-compile (require 'info))

(defun Info-goto-index (&optional nodename)
  "Go to info index named NODENAME.
If called interactively, prompt for which index."
  (interactive)
  (let ((cursor-in-echo-area t)
        char)
    (while (not nodename)
      (message "Which index? [i,c,d,f,k,p,v,?] ")
      (setq char (downcase (read-char)))
      (setq nodename (cdr (assoc char '((?i . ("index" "concept index"))
                                        (?c . ("concept index" "index"))
                                        (?d . "data type index")
                                        (?f . "function index")
                                        (?k . "keystroke index")
                                        (?p . ("program index" "procedure index"))
                                        (?v . "variable index")))))
      (if (not nodename)
          (with-output-to-temp-buffer "*Help*"
            (princ "Type the first character of one of the following indices:\n\n")
            (princ "  index  or  concept index\n")
            (princ "  concept index  or  index\n")
            (princ "  data type index\n")
            (princ "  function index\n")
            (princ "  keystroke index\n")
            (princ "  program index  or  procedure index\n")
            (princ "  variable index\n")))))
  (if (listp nodename)
      (let (success)
        (while nodename
          (setq success (car nodename)
                nodename (cdr nodename))
          (condition-case nil
              (progn
                (Info-goto-node success)
                (setq nodename nil))
            (error (setq success nil))))
        (if (not success)
            (error "No such index")))
    (Info-goto-node nodename)))

;;; Info-guess-node occasionally causes a problem.
;; (defadvice info (around guess-node activate)
;;   "Try to guess which node the user wants to read.
;; Does nothing if an argument was given to info."
;;   (let ((old-major-mode major-mode))
;;     ad-do-it
;;     (if (not (ad-get-arg 0))
;;      (Info-guess-node old-major-mode))))
;;
;;
;; (defun Info-guess-node (old-major-mode)
;;   "Guess the name of the Info index node for OLD-MAJOR-MODE."
;;   (cond ((eq old-major-mode 'emacs-lisp-mode)
;;       (if (not (string-match "elisp$" Info-current-file))
;;           (progn
;;             (Info-directory)
;;             (Info-menu "elisp")
;;             ;; do I want this?
;;             (Info-goto-index "index"))))
;;      ((or (eq old-major-mode 'scheme-mode)
;;           (eq old-major-mode 'scheme-interaction-mode))
;;       (if (not (string-match "scheme$" Info-current-file))
;;           (progn
;;             (Info-directory)
;;             (Info-menu "Scheme Reference"))))
;;      ((eq old-major-mode 'python-mode)
;;       (if (not (string-match "python" Info-current-file))
;;           (progn
;;             (Info-directory)
;;             (Info-menu "Python-lib"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File mode setting
;;;

;; ;; This doesn't work in Emacs 19.24+, because basic-save-buffer-1 (called
;; ;; after the hooks) resets the setmodes variable.
;; ;;; automagic new file mode setting from craig, see files.el
;;  (defvar setmodes)
;;  (defun set-modes-from-dir ()
;;    ;; Set new modes from file .filemode only if (backup-buffer) and
;;    ;; other hooks have not already decided the mode:
;;    (if (not setmodes)
;;        (setq setmodes
;;           (file-modes
;;            (concat
;;             (file-name-directory buffer-file-name)
;;             ".filemode"))))
;;    ;; Return nil since we did not write the file:
;;    nil)
;;  (add-hook 'write-file-hooks 'set-modes-from-dir)

; This was the old code.
; ;; This causes any files created with emacs to have the same protection
; ;; bits as has a file named ".filemode" in the directory in which the new
; ;; file is being created.  That is, this allows you to have a "default
; ;; protection" set for each directory, rather than just the default
; ;; protection you get with your "umask" setting.
; (defun set-modes-from-dir ()
;     (let ((fm-name (concat (file-name-directory buffer-file-name)
;                          ".filemode")))
;       (if (file-exists-p fm-name)
;           (setq setmodes (logior (file-modes fm-name)
;                                  (if (integerp setmodes)
;                                      (logand setmodes 73)
;                                      0))))))


;; Sudish Joseph <joseph@cis.ohio-state.edu> says:
;;
;; Here's one way of doing it w/o touching files.el, but I'm rather hesitant to
;; use it.  The way I look at it we could safely do the writing of the file
;; ourself and then set the mode, provided we knew that we were the last hook,
;; hence the following code.
;;
;; The only thing that bugs me is that I have neither the time (nor the patience)
;; to understand every check occuring in basic-save-buffer-1 to decide the file
;; mode, and theres a good possibility that blindly setting the mode could lead
;; to all sorts of problems.  I tested the hook out on some dummy files and it
;; works just fine, but I'm not using it for everyday work until someone with
;; experience (you ;-) says that this is safe.
;;
;; [I guess I really should be checking if (listp hooks)]
;;
;; (defvar setmodes)
;; (defvar hooks)
;; ;; KLUGE ALERT!
;; (defun set-modes-from-dir ()
;;   (let (tmpsetmodes)
;;     ;; we prefer being the last item on the hooks, thankyouverymuch
;;     (if (cdr hooks)
;;         ;; put us last on the list
;;         (setq hooks (append (cdr hooks) (cons (car hooks) nil)))
;;       ;; Set new modes from file .filemode only if (backup-buffer) and
;;       ;; other hooks have not already decided the mode:
;;       (if (not setmodes)
;;           (setq setmodes
;;                 (file-modes
;;                  (concat
;;                   (file-name-directory buffer-file-name)
;;                   ".filemode"))))
;;       (setq tmpsetmodes (basic-save-buffer-1))
;;       (or (equal setmodes tmpsetmodes)
;;           (message "Overriding %d, using %d, for perms."
;;                    (or tmpsetmodes 0) setmodes))
;;       ;; uh, is this, like, a safe way to live?
;;       (if setmodes
;;           (condition-case ()
;;               (set-file-modes buffer-file-name setmodes)
;;             (error nil)))))
;;     ;; Return t only when we actually write the file
;;   (if (cdr hooks) nil t))
;; ;(add-hook 'write-file-hooks 'set-modes-from-dir)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Executed statements
;;;


;; Tags
(eval-after-load "etags" '(load "etags-mde"))
(setq tags-find-related-names-functions '(mit-scheme-tags-find-related-names))

;; Fiddle with window title, in particular shorten system name
(let ((frame-name (cdr (assoc 'name (frame-parameters (selected-frame))))))
  (if (string-match "^emacs-?\\(19\\|20\\)\\(\\.[23][0-9]\\)@" frame-name)
      (setq frame-name (concat "emacs"
                               (substring frame-name (1- (match-end 0))))))
  (if (string-match "\\(\\.cs\\.rice\\.edu\\|\\.cs\\.washington\\.edu\\|\\.csail\\.mit\\.edu\\|\\.lcs\\.mit\\.edu\\|\\.ds\\.mpi-sws\\.mpg\\.de\\)" frame-name)
      (setq frame-name (substring frame-name 0 (match-beginning 0))))
  (if (string-match "@cs\\.rice\\.edu" frame-name)
      (setq frame-name (concat (substring frame-name 0 (match-beginning 0))
                               "@titan")))
  (if (string-match "@cs\\.washington\\.edu" frame-name)
      (setq frame-name (concat (substring frame-name 0 (match-beginning 0))
                               "@june")))
  (if (equal user-login-name "root")
      (setq frame-name (concat "root " frame-name)))
  (modify-frame-parameters (selected-frame) (list (cons 'name frame-name))))

;; Use  M-x list-colors-display  to see other options
(if (equal user-login-name "root")
    ;;  "misty rose" is a bit dark, but the point is to make this unmissable...
    (modify-frame-parameters (selected-frame) (list (cons 'background-color "misty rose"))))

;; Don't use this!!  It disables partial completion.
;; ;;; Make multiple TABs scroll completions
;; ;;; Written by Morten Welinder (terra@diku.dk)
;; (defun minibuf-tab ()
;;   "Like `minibuffer-complete', but if you use this repeatedly it will scroll
;; the window showing completions."
;;   (interactive)
;;   (or (eq last-command this-command) (setq minibuffer-scroll-window nil))
;;   (if minibuffer-scroll-window
;;       (save-excursion
;;      (set-buffer (window-buffer minibuffer-scroll-window))
;;      (if (pos-visible-in-window-p (point-max) minibuffer-scroll-window)
;;          (set-window-start minibuffer-scroll-window (point-min))
;;        (scroll-other-window)))
;;     (minibuffer-complete)))
;;
;; (define-key minibuffer-local-must-match-map "\t" 'minibuf-tab)
;; (define-key minibuffer-local-completion-map "\t" 'minibuf-tab)


;; This is most important for systems like Athena where my quota is tight.
;; (Probably javadoc-index shouldn't be under revision control anyway...)
(defvar non-byte-compiled-files
  '("~/emacs/javadoc-index.el"))        ; no longer exists, actually
(defun purge-undesired-elc-files ()
  "Remove .elc files that should not have been made in the first place."
  (let ((els non-byte-compiled-files))
    (while els
      (let ((elc (concat (car els) "c")))
        (if (file-exists-p elc)
            (delete-file elc)))
      (setq els (cdr els)))))
(run-with-idle-timer 2 nil 'purge-undesired-elc-files)

;; Too gaudy for me; maybe try again later.
;; (iswitchb-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs bugs
;;;


;;; This seems not necessary, since ediff-version-control-package is a symbol.
;;; It would be a valid patch if the variable were a string.
;; ;; These two ediff functions differ from that in Emacs 21.2 through Emacs 24.3 only by
;; ;; lowercasing the "%S" in "ediff-%S-...".  That is required for compatibility
;; ;; with custom-print (which inserts double-quote marks around "%S" arguments),
;; ;; and shouldn't affect non-users of custom-print.
;; ;; I should submit a bug report against Emacs 21.2.
;; ;;
;; (eval-after-load "ediff"
;;   '(progn
;;      (defun ediff-merge-revisions-with-ancestor (&optional
;;                                               file startup-hooks
;;                                               ;; MERGE-BUFFER-FILE is the file to
;;                                               ;; be associated with the merge
;;                                               ;; buffer
;;                                               merge-buffer-file)
;;        "Run Ediff by merging two revisions of a file with a common ancestor.
;; The file is the optional FILE argument or the file visited by the current
;; buffer."
;;        (interactive)
;;        (if (stringp file) (find-file file))
;;        (let (rev1 rev2 ancestor-rev)
;;       (setq rev1
;;             (read-string
;;              (format
;;               "Version 1 to merge (default %s's working version): "
;;               (if (stringp file)
;;                   (file-name-nondirectory file) "current buffer")))
;;             rev2
;;             (read-string
;;              (format
;;               "Version 2 to merge (default %s): "
;;               (if (stringp file)
;;                   (file-name-nondirectory file) "current buffer")))
;;             ancestor-rev
;;             (read-string
;;              (format
;;               "Ancestor version (default %s's base revision): "
;;               (if (stringp file)
;;                   (file-name-nondirectory file) "current buffer"))))
;;       (ediff-load-version-control)
;;       (funcall
;;        (intern (format "ediff-%s-merge-internal" ediff-version-control-package))
;;        rev1 rev2 ancestor-rev startup-hooks merge-buffer-file)))
;;
;;      (defun ediff-revision (&optional file startup-hooks)
;;        "Run Ediff by comparing versions of a file.
;; The file is an optional FILE argument or the file entered at the prompt.
;; Default: the file visited by the current buffer.
;; Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'."
;;        ;; if buffer is non-nil, use that buffer instead of the current buffer
;;        (interactive "P")
;;        (if (not (stringp file))
;;         (setq file
;;               (ediff-read-file-name "Compare revisions for file"
;;                                     (if ediff-use-last-dir
;;                                         ediff-last-dir-A
;;                                       default-directory)
;;                                     (ediff-get-default-file-name)
;;                                     'no-dirs)))
;;        (find-file file)
;;        (if (and (buffer-modified-p)
;;              (y-or-n-p (format "Buffer %s is modified. Save buffer? "
;;                                (buffer-name))))
;;         (save-buffer (current-buffer)))
;;        (let (rev1 rev2)
;;       (setq rev1
;;             (read-string
;;              (format "Revision 1 to compare (default %s's latest revision): "
;;                      (file-name-nondirectory file)))
;;             rev2
;;             (read-string
;;              (format "Revision 2 to compare (default %s's current state): "
;;                      (file-name-nondirectory file))))
;;       (ediff-load-version-control)
;;       (funcall
;;        (intern (format "ediff-%s-internal" ediff-version-control-package))
;;        rev1 rev2 startup-hooks)
;;       ))))


(provide 'startup-functions-mde)

;;; startup-functions-mde.el ends here
