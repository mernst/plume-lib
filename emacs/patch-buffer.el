;; patch.el --- apply a patch to a buffer
;; Roger Crew <rfc@research.microsoft.com>

(defvar patch-start-regexp "^\\*\\*\\* \\([^ \t\n]+\\).*[^*\n]$"
  "Regexp indicating the start of a patch.  \\(\\) brackets a filename")
(defvar patch-message-buffer "*Patch*Output*"
  "Buffer to which patch diagnostics are written")
(defvar patch-program "patch"
  "Where Larry Wall's patch program lives.")
(defvar patch-tmp-directory "/usr/tmp/"
  "Where to put temporary files.")
(defvar patch-fuzz nil
  "Fuzz factor for patch (nil for default)")


(defun patch-buffer (buffer beg reverse)
  "Applies the context-diff patch around point to a specified buffer.
With a prefix argument, this applies the reversed patch."
  (interactive
   (save-excursion
     (end-of-line 1)
     (re-search-backward patch-start-regexp)
     (list (completing-read
	    ;; damn, why can't read-buffer have an initial-input argument
	    "Buffer to patch: "
	    (mapcar (function (lambda (b) (cons (buffer-name b) b)))
		    (buffer-list))
	    nil
	    t
	    (let ((default (get-file-buffer
			    (buffer-substring (match-beginning 1) (match-end 1)))))
	      (and default (buffer-name default))))
	   (point)
	   (if current-prefix-arg t nil))))
  (setq patch-tmp-directory (file-name-as-directory patch-tmp-directory))
  (let* ((n (random 10000))
	 (end (save-excursion
		(goto-char beg)
		(forward-line 1)
		(and (re-search-forward patch-start-regexp nil 1)
		     (forward-line 0))
		(point)))
	 (iname (expand-file-name (format "ipatch%d" n) patch-tmp-directory))
	 (oname (expand-file-name (format "opatch%d" n) patch-tmp-directory))
	 (rname (expand-file-name (format "rpatch%d" n) patch-tmp-directory))
	 (patch-buffer (current-buffer))
	 (temp-buffer-show-function (function ignore)))
    (while (or (file-exists-p iname)
	       (file-exists-p oname)
	       (file-exists-p rname))
      (setq iname (concat iname "x")
	    oname (concat oname "x")
	    rname (concat rname "x")))
    (unwind-protect
	(with-output-to-temp-buffer patch-message-buffer
	  (switch-to-buffer buffer)
	  (save-restriction
	    (widen)
	    (write-region (point-min) (point-max) iname nil 1))
	  (let ((save-point (point))
		(status
		 (save-excursion
		   (set-buffer patch-buffer)
		   (call-process-region
		    beg end patch-program nil standard-output nil
		    "-d" patch-tmp-directory
		    "-o" oname
		    "-r" rname
		    (if reverse "--reverse" "--normal")
		    "-F" (number-to-string (or patch-fuzz 2))
		    "--force"
		    "--context"
		    "--ignore-whitespace"
		    iname
		    "-"
		    ))))
	    (cond ((eq 0 status)
		   (erase-buffer)
		   (insert-file-contents oname)
		   (goto-char save-point))
		  (t
		   (setq temp-buffer-show-function nil)
		   (message "Patch failed:  %s" status)
		   (ding)))))
      (condition-case e
	  (delete-file iname)
	(file-error e))
      (condition-case e
	  (delete-file oname)
	(file-error e))
      (condition-case e
	  (delete-file rname)
	(file-error e)))))
