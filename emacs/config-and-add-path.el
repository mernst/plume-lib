;; config-and-add-path.el

;;; Don't byte-compile this file, because it is supposed to work in all
;;; Emacs versions, but (we think) no byte-compilation format does.


;;; {{{ Configuration management macros and variables

(provide 'config-and-add-path)

(defvar running-emacs-22 (= 22 emacs-major-version))
(defvar running-emacs-23 (= 23 emacs-major-version))
(defvar running-xemacs (featurep 'xemacs))
(defvar running-emacs-fsf (not (featurep 'xemacs)))

(defmacro emacs-22 (&rest body)
  "Execute BODY if running Emacs 22."
  `(if running-emacs-22
         (progn ,@body)))
(defmacro emacs-23 (&rest body)
  "Execute BODY if running Emacs 23."
  `(if running-emacs-23
         (progn ,@body)))
(defmacro emacs-fsf (&rest body)
  "Execute BODY if running (FSF) Emacs."
  `(if running-emacs-fsf
         (progn ,@body)))
(defmacro xemacs (&rest body)
  "Execute BODY if running XEmacs."
  `(if running-xemacs
         (progn ,@body)))

(xemacs
 ;; Put xemacs byte-compiled files in "xemacs" subdir!
 (defvar byte-compile-use-xemacs-subdir t)
 (defun byte-compile-dest-file (filename)
   "Convert an Emacs Lisp source file name to a compiled file name.
GJB: this version adds the directory \"xemacs/\" as a prefix so
that xemacs byte-compiled files can coexist with GNU Emacs ones"
   (setq filename (byte-compiler-base-file-name filename))
   (setq filename (file-name-sans-versions filename))
   (let ((newname
          (cond ((eq system-type 'vax-vms)
                 (concat (substring filename 0 (string-match ";" filename)) "c"))
                ((string-match emacs-lisp-file-regexp filename)
                 (concat (substring filename 0 (match-beginning 0)) ".elc"))
                (t (concat filename ".elc")))))
     (concat (file-name-directory newname)
             (if byte-compile-use-xemacs-subdir "xemacs/" "")
             (file-name-nondirectory newname)))))


;;; }}}

;;; {{{ Load path manipulation functions and settings

;;; This is from Geoffroy Ville and Jerry Quin
(defun add-path (oldpath newpath &optional append)
  "Add to the value of OLDPATH the path NEWPATH (similar to add-hook).
NEWPATH is not added if already present.
NEWPATH is added (if necessary) at the beginning of the path unless
the optional argument APPEND is non-nil, in which case NEWPATH is
added at the end.

OLDPATH should be a symbol, and NEWPATH may be any valid path (it will
be expanded).  If OLDPATH is void, it is first set to nil.  If OLDPATH's
value is a single path (string), it is changed to a list of path.

A prefix arg causes the interactive version of the command to append
instead of prepend."
  (interactive "vPath variable: \nDPath to add: \nP")
  (or (boundp oldpath) (set oldpath nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value oldpath)))
    (if (or (not (listp old)) (stringp old))
        (set oldpath (list old))))
  (let ((new (expand-file-name newpath)))
    (or (if (sequencep new); consp
            (member new (symbol-value oldpath))
          (memq new (symbol-value oldpath)))
        (set oldpath
             (if append
                 (nconc (symbol-value oldpath) (list new))
               (cons new (symbol-value oldpath)))))))

;; Typical usage:
;;  (add-path-maybe-xemacs 'load-path "/uns/share/emacs/site-lisp/elib")
(defun add-path-maybe-xemacs (oldpath newpath &optional append)
  "Add to the value of OLDPATH the path NEWPATH (similar to add-hook).
If we're using xemacs, also add NEWPATH/xemacs.  Uses `add-path'."
  (if append
      (progn
        ;; add xemacs first if we're appending
        (xemacs (add-path oldpath (concat newpath "/xemacs") 'APPEND))
        (add-path oldpath newpath 'APPEND))
    ;; add xemacs second if we're prepending
    (add-path oldpath newpath)
    (xemacs (add-path oldpath (concat newpath "/xemacs")))))


;; This prevents things from being added to load-path, but doesn't remove
;; them from load-path.
(defvar load-path-prune-regexps '())
(defun load-path-prune (path)
  "Return t if PATH shouldn't appear on load-path."
  (let ((result nil)
        (regexps load-path-prune-regexps))
    (while regexps
      (if (string-match (car regexps) path)
          (setq result t
                regexps nil)
        (setq regexps (cdr regexps))))
    result))

(defvar load-path-prune-noisy t)

;; Defined in startup.el, but I got an error anyway, so define it here...
(defvar normal-top-level-add-subdirs-inode-list nil)

;; Lifted from similar function in Emacs 20.4 startup.el.
;; Emacs 20 and XEmacs 20 already recursively add subdirectories
;; to load-path; here we make Emacs 19 act that way, too; but we
;; make them all prune.
;; Uses a list of regular expressions to omit certain file names.
(defun normal-top-level-add-subdirs-to-load-path-pruning ()
  "Add all subdirectories of current directory to `load-path'.
More precisely, this uses only the subdirectories whose names
start with letters or digits; it excludes any subdirectory named `RCS'
or `CVS', and any subdirectory that contains a file named `.nosearch'.
Also omits directories for which `load-path-prune' returns non-nil."
  (let (dirs
        attrs
        (pending (list default-directory)))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      (setq dirs (cons (car pending) dirs))
      (setq pending (cdr pending))
      (setq attrs (nthcdr 10 (file-attributes (car dirs))))
      (let ((contents (directory-files (car dirs)))
            (default-directory (car dirs)))
        (unless (member attrs normal-top-level-add-subdirs-inode-list)
          (setq normal-top-level-add-subdirs-inode-list
                (cons attrs normal-top-level-add-subdirs-inode-list))
          (while contents
            (let ((candidate (car contents)))
              (unless (member candidate '("." ".." "RCS" "CVS"))
                (when (and (string-match "\\`[a-zA-Z0-9]" candidate)
                           ;; Avoid doing a `stat' when it isn't necessary
                           ;; because that can cause trouble when an NFS server
                           ;; is down.
                           (not (string-match "\\.elc?\\'" candidate))
                           ;; First, call on unexpanded dirname (may be cheaper).
                           (not (load-path-prune candidate))
                           (file-directory-p candidate))
                  ;; Under Windows, the extra argument to expand-file-name
                  ;; seems necessary.
                  (let ((expanded (expand-file-name candidate default-directory)))
                    (unless (or
                             ;; Then, call on expanded dirname.
                             (load-path-prune expanded)
                             (file-exists-p (expand-file-name ".nosearch"
                                                              expanded)))
                      (if load-path-prune-noisy
                          (message "Add to load-path: %s = %s" candidate expanded))
                      (setq pending (nconc pending (list expanded)))))))
              (setq contents (cdr contents)))))))
    (normal-top-level-add-to-load-path (cdr (nreverse dirs)))))

;; normal-top-level-add-subdirs-to-load-path expects the default directory
;; to already be in the load-path
(if (not (fboundp 'normal-top-level-add-to-load-path))
    ;; This function is called from a subdirs.el file.
    ;; It assumes that default-directory is the directory
    ;; in which the subdirs.el file exists,
    ;; and it adds to load-path the subdirs of that directory
    ;; as specified in DIRS.  Normally the elements of DIRS are relative.
    (defun normal-top-level-add-to-load-path (dirs)
      (let ((tail load-path)
            (thisdir (directory-file-name default-directory)))
        (while (and tail
                    (not (equal thisdir (car tail)))
                    (not (and (memq system-type '(ms-dos windows-nt))
                              (equal (downcase thisdir) (downcase (car tail))))))
          (setq tail (cdr tail)))
        ;; TEST ADDED BY MDE (I think)
        (if tail
            (setcdr tail (append (mapcar 'expand-file-name dirs) (and tail (cdr tail))))
          (setq load-path (nconc load-path (mapcar 'expand-file-name dirs)))))))

(defun remove-matching-strings (regexp list)
  "Return a copy of LIST with strings matching REGEXP removed."
  (delq nil
        (mapcar (function (lambda (elt)
                    (if (string-match regexp elt)
                        nil
                      elt)))
                list)))

(defun move-subdirs-before-parents-by-copying (subdir-name path-list)
  "Return a copy of PATH-LIST such that any directory ending in SUBDIR-NAME
precedes its parent in the list (if the parent also appears in the list)."
  (let ((result nil)
        (pl path-list)
        (subdir-regexp (concat "/" subdir-name "/?$")))
    (while pl
      (let ((this-dir (car pl)))
        (if (string-match "/xemacs/?$" this-dir)
            (let* ((parent (substring this-dir 0 (match-beginning 0)))
                   (parent-tail (or (member parent result)
                                    (member (concat parent "/") result))))
              (if parent-tail
                  ;; The parent is already in result.  We should insert this
                  ;; directory after the parent (because result is reversed).
                  (setcdr parent-tail (cons this-dir (cdr parent-tail)))
                (setq result (cons this-dir result))))
          (setq result (cons this-dir result))))
      (setq pl (cdr pl)))
    (setq load-path (reverse result))))

(defun move-subdirs-before-parents-by-side-effect (subdir-name path-list)
  "Side effect PATH-LIST such that any directory ending in SUBDIR-NAME
precedes its parent in the list (if the parent also appears in the list).
Returns the original PATH-LIST (guaranteed to have the same first cons)."
  (let ((child-dirs '()))               ; list of (parent . child) conses
    (let ((dirs path-list)
          (subdir-regexp (concat "/" subdir-name "/?$")))
      (while dirs
        (let ((this-dir (car dirs)))
          (if (string-match subdir-regexp this-dir)
              (setq child-dirs (cons (cons (substring this-dir
                                                      0 (match-beginning 0))
                                           this-dir)
                                     child-dirs))))
        (setq dirs (cdr dirs))))
    (while child-dirs
      (let* ((parent-dir (car (car child-dirs)))
             (child-dir (cdr (car child-dirs)))
             (parent-member (member parent-dir path-list))
             (child-member (member child-dir parent-member)))
        (if child-member
            ;; both are in the list, with parent first
            (progn
              ;; insert the child ahead of the parent
              (setcar parent-member child-dir)
              (setcdr parent-member (cons parent-dir (cdr parent-member)))
              ;; delete the child (but leave it if it's the last element)
              (if (cdr child-member)
                  (progn
                    (setcar child-member (car (cdr child-member)))
                    (setcdr child-member (cdr child-member)))))))
      (setq child-dirs (cdr child-dirs))))
  ;; return original argument so interface to this
  ;; and to the -by-copying versions are identical
  path-list)

(defun move-subdirs-before-parents (str dirs)
  (move-subdirs-before-parents-by-side-effect str dirs))

(defun config-time-less (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (nth 1 t1) (nth 1 t2)))))

(defun modification-time (file)
  "Modification time of FILE."
  (nth 5 (file-attributes file)))

;;; End of function definitions.
;;; Now actually modify load-path.

(defvar load-path-assoc-list nil
  "Assoc list of (emacs-version . load-path).")
(defvar load-path-assoc-list-date nil
  "File time for file containing `load-path-assoc-list'.")

(defvar load-path-assoc-list-file (expand-file-name "~/.path.el"))

;; Extra site-lisp directories, for example.
(defvar load-path-extra-load-dirs nil)

(if (file-exists-p load-path-assoc-list-file)
    (progn
      (setq load-path-assoc-list-date (modification-time load-path-assoc-list-file))
      (load-file load-path-assoc-list-file)))

(defun load-path-normal-add (dir)
  (if (file-exists-p (expand-file-name dir))
      (let ((default-directory dir))
        (add-path-maybe-xemacs 'load-path default-directory)
        (normal-top-level-add-subdirs-to-load-path-pruning))))


;; This is the main entry point for this file.
(defun set-load-path ()
  (let ((dirs load-path-extra-load-dirs))
    (while dirs
      (load-path-normal-add (car dirs))
      (setq dirs (cdr dirs))))
  (load-path-normal-add "~/emacs")

  ;; Remove inappropriate elements from load-path
  (emacs-fsf
    (setq load-path (remove-matching-strings "/xemacs\\($\\|/\\)" load-path)))
  (xemacs
    (setq load-path (remove-matching-strings "/emacs-fsf\\($\\|/\\)" load-path)))
  (emacs-22
    (setq load-path (remove-matching-strings "/emacs-\\(20\\|NOT21\\)\\($\\|/\\)" load-path)))

  ;; backup directories in w3
  (setq load-path (remove-matching-strings "/bak\\($\\|/\\)" load-path))
  (setq load-path (remove-matching-strings "/DELETED\\($\\|/\\)" load-path))

  ;; Remove duplicates (due to symbolic links)
  (let ((true-load-path (mapcar #'(lambda (elt) (cons (file-truename elt) elt))
                                load-path)))
    (while true-load-path
      (let ((dup (assoc (car (car true-load-path)) (cdr true-load-path))))
        (if dup
            ;; no need to setq:  we know it's not the first element of load-path
            (delete (cdr dup) load-path)))
      (setq true-load-path (cdr true-load-path))))

  ;; Make sure more specific subdirectories precede their parents.
  (xemacs
    (setq load-path (move-subdirs-before-parents "xemacs" load-path)))
  (emacs-22
    (setq load-path (move-subdirs-before-parents "emacs-22" load-path)))
  (emacs-23
    (setq load-path (move-subdirs-before-parents "emacs-23" load-path)))
  )


;; Try to look up the load-path in .path.el;
;; if not up to date, then compute and save in that file.
(let ((candidate-load-path (cdr (assoc (emacs-version) load-path-assoc-list))))
  (setq normal-top-level-add-subdirs-inode-list nil)
  (if (not (and candidate-load-path
                (let ((dir-changed-p nil)
                      (changed-dir nil)
                      (dirs candidate-load-path))
                  (while (and dirs (not dir-changed-p))
                    (let ((dir (car dirs)))
                      (setq dir-changed-p
                            (or (let ((dir-mod-time (modification-time dir)))
                                  (and dir-mod-time
                                       (config-time-less load-path-assoc-list-date
                                                         dir-mod-time)))
                                (or (not (file-exists-p dir))
                                    (config-time-less load-path-assoc-list-date
                                                      (modification-time
                                                       ;; Used to use
                                                       ;; (directory-file-name
                                                       ;;  (file-name-as-directory
                                                       ;;   dir)).  Why?
                                                       dir))))
                            changed-dir (and dir-changed-p dir)
                            dirs (cdr dirs))))
                  (if dir-changed-p (message "changed directory: %s" changed-dir))
                  (not dir-changed-p))))
      (progn
        (set-load-path)
        (let ((lp-assoc (assoc (emacs-version) load-path-assoc-list)))
          (if lp-assoc
              (setcdr lp-assoc load-path)
            (setq load-path-assoc-list (cons (cons (emacs-version) load-path)
                                             load-path-assoc-list))))
        (let ((buf (find-file-noselect load-path-assoc-list-file)))
          (set-buffer buf)
          (erase-buffer)
          ;; This line avoids errors if "-*-" appears in load-path-assoc-list.
          (insert ";; -*- Emacs-Lisp -*-\n");
          (insert "(setq load-path-assoc-list '")
          (prin1 load-path-assoc-list buf)
          (insert ")\n")
          (let ((delete-old-versions t))
            (save-buffer))
          (kill-buffer buf)))
    ;; Is this the right thing?
    (setq load-path candidate-load-path)))


;; }}}
