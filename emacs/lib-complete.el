;;; lib-complete.el --- Completion on the lisp search path

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) Mike Williams <mike-w@cs.aukuni.ac.nz> 1991

;; Author: Mike Williams <mike-w@cs.aukuni.ac.nz>
;; Maintainer: XEmacs Development Team
;; Keywords: lisp, extensions
;; Created: Sat Apr 20 17:47:21 1991

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; ========================================================================
;; lib-complete.el --  Completion on a search path
;; Author          : Mike Williams <mike-w@cs.aukuni.ac.nz>
;; Created On      : Sat Apr 20 17:47:21 1991
;; Last Modified By: Heiko M|nkel <muenkel@tnt.uni-hannover.de>
;; Additional XEmacs integration By: Chuck Thompson <cthomp@cs.uiuc.edu>
;; Last Modified On: Thu Jul 1 14:23:00 1994
;; RCS Info        : $Revision: 1.2 $ $Locker:  $
;; ========================================================================
;; NOTE: XEmacs must be redumped if this file is changed.
;;
;; Copyright (C) Mike Williams <mike-w@cs.aukuni.ac.nz> 1991
;;
;; Keywords: utility, lisp

;; Many thanks to Hallvard Furuseth <hallvard@ifi.uio.no> for his
;; helpful suggestions.

;; The function locate-file is removed, because of its incompatibility
;; with the buildin function of the lemacs 19.10 (Heiko M|nkel).

;; There is now the new function find-library in this package.

;;; ChangeLog:

;; 4/26/97: sb Mule-ize.

;;; Code:

;;=== Determine completions for filename in search path ===================

(defun library-all-completions (FILE SEARCH-PATH &optional FULL FAST)
  "Return all completions for FILE in any directory on SEARCH-PATH.
If optional third argument FULL is non-nil, returned pathnames should be
  absolute rather than relative to some directory on the SEARCH-PATH.
If optional fourth argument FAST is non-nil, don't sort the completions,
  or remove duplicates."
  (setq FILE (or FILE ""))
  (if (file-name-absolute-p FILE)
      ;; It's an absolute file name, so don't need SEARCH-PATH
      (progn
	(setq FILE (expand-file-name FILE))
	(file-name-all-completions
	 (file-name-nondirectory FILE) (file-name-directory FILE)))
    (let ((subdir (file-name-directory FILE))
	  (file (file-name-nondirectory FILE))
	  all-completions)
      ;; Make list of completions in each directory on SEARCH-PATH
      (while SEARCH-PATH
	(let* ((dir (concat (file-name-as-directory
			     (expand-file-name (car SEARCH-PATH)))
			    subdir))
	       (dir-prefix (if FULL dir subdir)))
	  (if (file-directory-p dir)
	      (let ((subdir-completions
		     (file-name-all-completions file dir)))
		(while subdir-completions
		  (setq all-completions
			(cons (concat dir-prefix (car subdir-completions))
			      all-completions))
		  (setq subdir-completions (cdr subdir-completions))))))
	(setq SEARCH-PATH (cdr SEARCH-PATH)))
      (if FAST all-completions
	(let ((sorted (nreverse (sort all-completions 'string<)))
	      compressed)
	  (while sorted
	    (if (equal (car sorted) (car compressed)) nil
	      (setq compressed (cons (car sorted) compressed)))
	    (setq sorted (cdr sorted)))
	  compressed)))))

;;=== Utilities ===========================================================

(defmacro progn-with-message (MESSAGE &rest FORMS)
  "(progn-with-message MESSAGE FORMS ...)
Display MESSAGE and evaluate FORMS, returning value of the last one."
  ;; based on Hallvard Furuseth's funcall-with-message
  `(if (eq (selected-window) (minibuffer-window))
       (save-excursion
	 (goto-char (point-max))
	 (let ((orig-pmax (point-max)))
	   (unwind-protect
	       (progn
		 (insert " " (, MESSAGE)) (goto-char orig-pmax)
		 (sit-for 0)		; Redisplay
		 ,@FORMS)
	     (delete-region orig-pmax (point-max)))))
     (prog2
	 (message "%s" (, MESSAGE))
	 (progn ,@FORMS)
       (message ""))))
;; #+infodock (defalias 'lib-funcall-with-msg 'progn-with-message)

(put 'progn-with-message 'lisp-indent-hook 1)
;; #+infodock (put 'lib-funcall-with-message 'lisp-indent-hook 1)

;;=== Completion caching ==================================================

(defconst lib-complete:cache nil
  "Used within read-library and read-library-internal to prevent
costly repeated calls to library-all-completions.
Format is a list of lists of the form

    ([<path> <subdir>] <cache-record> <cache-record> ...)

where each <cache-record> has the form

   (<root> <modtimes> <completion-table>)")
;; #+infodock (defvaralias 'lib-completions 'lib-complete:cache)

(defun lib-complete:better-root (ROOT1 ROOT2)
  "Return non-nil if ROOT1 is a superset of ROOT2."
  (and (equal (file-name-directory ROOT1) (file-name-directory ROOT2))
       (string-match
	(concat "^" (regexp-quote (file-name-nondirectory ROOT1)))
	ROOT2)))

(defun lib-complete:get-completion-table (FILE PATH FILTER)
  (let* ((subdir (file-name-directory FILE))
	 (root (file-name-nondirectory FILE))
	 (PATH
	  (mapcar
	   (function (lambda (dir) (file-name-as-directory
				    (expand-file-name (or dir "")))))
	   PATH))
	 (key (vector PATH subdir FILTER))
	 (real-dirs
	  (if subdir
	      (mapcar (function (lambda (dir) (concat dir subdir))) PATH)
	    PATH))
	 (path-modtimes
	  (mapcar
	   (function (lambda (fn) (if fn (nth 5 (file-attributes fn)))))
	   real-dirs))
	 (cache-entry (assoc key lib-complete:cache))
	 (cache-records (cdr cache-entry)))
    ;; Look for cached entry
    (catch 'table
      (while cache-records
	(if (and
	     (lib-complete:better-root (nth 0 (car cache-records)) root)
	     (equal (nth 1 (car cache-records)) path-modtimes))
	    (throw 'table (nth 2 (car cache-records))))
	(setq cache-records (cdr cache-records)))
      ;; Otherwise build completions
      (let ((completion-list
	     (progn-with-message "(building completion table...)"
	       (library-all-completions FILE PATH nil 'fast)))
	    (completion-table (make-vector 127 0)))
	(while completion-list
	  (let ((completion
		 (if (or (not FILTER)
			 (file-directory-p (car completion-list)))
		     (car completion-list)
		   (funcall FILTER (car completion-list)))))
	    (if completion
		(intern completion completion-table)))
	  (setq completion-list (cdr completion-list)))
	;; Cache the completions
	(lib-complete:cache-completions key root
					path-modtimes completion-table)
	completion-table))))

(defvar lib-complete:max-cache-size 40
  "*Maximum number of search paths which are cached.")

(defun lib-complete:cache-completions (key root modtimes table)
  (let* ((cache-entry (assoc key lib-complete:cache))
	 (cache-records (cdr cache-entry))
	 (new-cache-records (list (list root modtimes table))))
    (if (not cache-entry) nil
      ;; Remove old cache entry
      (setq lib-complete:cache (delq cache-entry lib-complete:cache))
      ;; Copy non-redundant entries from old cache entry
      (while cache-records
	(if (or (equal root (nth 0 (car cache-records)))
		(lib-complete:better-root root (nth 0 (car cache-records))))
	    nil
	  (setq new-cache-records
		(cons (car cache-records) new-cache-records)))
	(setq cache-records (cdr cache-records))))
    ;; Add entry to front of cache
    (setq lib-complete:cache
	  (cons (cons key (nreverse new-cache-records)) lib-complete:cache))
    ;; Trim cache
    (let ((tail (nthcdr lib-complete:max-cache-size lib-complete:cache)))
      (if tail (setcdr tail nil)))))

;;=== Read a filename, with completion in a search path ===================

(defun read-library-internal (FILE FILTER FLAG)
  "Don't call this."
  ;; Relies on read-library-internal-search-path being let-bound
  (let ((completion-table
	 (lib-complete:get-completion-table
	  FILE read-library-internal-search-path FILTER)))
    (cond
     ((not completion-table) nil)
     ;; Completion table is filtered before use, so the PREDICATE
     ;; argument is redundant.
     ((eq FLAG nil) (try-completion FILE completion-table nil))
     ((eq FLAG t) (all-completions FILE completion-table nil))
     ((eq FLAG 'lambda) (and (intern-soft FILE completion-table) t))
     )))

(defun read-library (PROMPT SEARCH-PATH &optional DEFAULT MUST-MATCH
			    FULL FILTER)
  "Read library name, prompting with PROMPT and completing in directories
from SEARCH-PATH.  A nil in the search path represents the current
directory.  Completions for a given search-path are cached, with the
cache being invalidated whenever one of the directories on the path changes.
Default to DEFAULT if user enters a null string.
Optional fourth arg MUST-MATCH non-nil means require existing file's name.
  Non-nil and non-t means also require confirmation after completion.
Optional fifth argument FULL non-nil causes a full pathname, rather than a
  relative pathname, to be returned.  Note that FULL implies MUST-MATCH.
Optional sixth argument FILTER can be used to provide a function to
  filter the completions.  This function is passed the filename, and should
  return a transformed filename (possibly a null transformation) or nil,
  indicating that the filename should not be included in the completions."
  (let* ((read-library-internal-search-path SEARCH-PATH)
	 (library (completing-read PROMPT 'read-library-internal
				   FILTER (or MUST-MATCH FULL) nil)))
    (cond
     ((equal library "") DEFAULT)
     (FULL (locate-file library read-library-internal-search-path
			;; decompression doesn't work with Mule -slb
			(if (featurep 'mule)
			    ".el:.elc"
			  ".el:.el.gz:.elc")))
     (t library))))

;; NOTE: as a special case, read-library may be used to read a filename
;; relative to the current directory, returning a *relative* pathname
;; (read-file-name returns a full pathname).
;;
;; eg. (read-library "Local header: " '(nil) nil)

(defun get-library-path ()
  "Front end to read-library"
  (read-library "Find Library file: " load-path nil t t
		  (function (lambda (fn)
			      (cond
			       ;; decompression doesn't work with mule -slb
			       ((string-match (if (featurep 'mule)
						  "\\.el$"
						"\\.el\\(\\.gz\\)?$") fn)
				(substring fn 0 (match-beginning 0))))))
		  ))

;; FSF Emacs version
(defun get-library-path ()
  (let* ((short-lib (completing-read "Find Library file: " 'library-complete))
	 (full-path (locate-library short-lib)))
    (if (string-match "\\.elc$" full-path)
	(let ((non-elc (substring full-path 0 (1- (length full-path)))))
	  (if (file-exists-p non-elc)
	      non-elc
	    full-path))
      full-path)))


;;=== Replacement for load-library with completion ========================

(defun load-library (library)
  "Load the library named LIBRARY.
This is an interface to the function `load'."
  (interactive
   (list (read-library "Load Library: " load-path nil nil nil
		  (function (lambda (fn)
			      (cond
			       ((string-match "\\.elc?$" fn)
				(substring fn 0 (match-beginning 0))))))
		  )))
  (load library))

;;=== find-library with completion (Author: Heiko Muenkel) ===================

(defun find-library (library &optional codesys)
  "Find and edit the source for the library named LIBRARY.
The extension of the LIBRARY must be omitted.
Under XEmacs/Mule, the optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive
   (list (get-library-path)
	 (if current-prefix-arg
	     (read-coding-system "Coding System: "))))
  ;; (find-file library codesys)
  (find-file library)
  )

(defun find-library-other-window (library &optional codesys)
  "Load the library named LIBRARY in another window.
Under XEmacs/Mule, the optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive
   (list (get-library-path)
	 (if current-prefix-arg
	   (read-coding-system "Coding System: "))))
  ;; (find-file-other-window library codesys)
  (find-file-other-window library)
  )
;; #+infodock (defalias 'lib-edit-other-window 'find-library-other-window)

(defun find-library-other-frame (library &optional codesys)
  "Load the library named LIBRARY in a newly-created frame.
Under XEmacs/Mule, the optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive
   (list (get-library-path)
	 (if current-prefix-arg
	     (read-coding-system "Coding System: "))))
  ;; (find-file-other-frame library codesys)
  (find-file-other-frame library)
  )

; This conflicts with an existing binding
;(define-key global-map "\C-xl" 'find-library)
(define-key global-map "\C-x4l" 'find-library-other-window)
(define-key global-map "\C-x5l" 'find-library-other-frame)

;; #+infodock (defalias 'lib-where-is 'locate-library)

;; #+infodock (provide 'lib)
(provide 'lib-complete)
;;; lib-complete.el ends here
