;;; javadoc-lookup.el
;;; Displays (in a separate browser window) Javadoc documentation for a Java
;;; package, class, method, or field, whose name is specified with completion.
;;; Originally jdk-goto-ref.el by Greg J. Badros -- 11/3/97.
;;; Substantially rewritten by Michael D. Ernst, 11/21/97, 4/22/99
;;; Also see script javadoc-index-to-alist,
;;; which creates the file that this Emacs Lisp code uses.

(defvar javadoc-ignored-prefixes
  ;; no need for a default; the .javadoc-index.el file sets this variable
  nil
  "*Directories in which Java documentation is found.
These prefixes are stripped from filenames and URLs when determining
what Java entities are documented by a particular HTML file.")

(defvar javadoc-index-filename (expand-file-name "~/.javadoc-index.el")
  "File mapping Java identifiers to HTML documentation files.
The mapping is created by the javadoc-index-to-alist program.")
(defvar javadoc-html-refs nil
  "Alist of (id . list-of-refs), read from file `javadoc-index-filename'.")
(if (not javadoc-html-refs)
    (load-file javadoc-index-filename))

(defvar java-keywords
  '("abstract" "boolean" "break" "byte" "case" "catch" "char" "class"
    "const" "continue" "default" "do" "double" "else" "extends" "final"
    "finally" "float" "for" "goto" "if" "implements" "import" "instanceof"
    "int" "interface" "long" "native" "new" "package" "private" "protected"
    "public" "return" "short" "static" "super" "switch" "synchronized"
    "throw" "throws" "transient" "try" "void" "volatile" "while"))

(defun javadoc-read-id ()
  "Read a string identifier for a Java class or method."
   ;; Setting completion-ignore-case to t is tempting, because Java code
   ;; has strange capitalization.  When using an unpatched version of
   ;; partial completion (from the Emacs 22 pre-test as of June, 2006),
   ;; there are negative consequences, such as typing "Iterato" and having
   ;; it complete to "iterator".  But my fixes correct that.
  (let ((completion-ignore-case t))
    (completing-read "Javadoc for: " javadoc-html-refs nil
		     t			; require match
		     (let* ((raw-guess (current-word))
			    (guess (if (or (null raw-guess)
					   (member raw-guess java-keywords))
				       ""
				     raw-guess))
			    (try (try-completion guess javadoc-html-refs)))
		       (if (eq try t) guess try)))))

;; Map from Java class/method name to fully-qualified class name, both as strings
(defvar javadoc-get-url-hashtable (make-hash-table :test 'equal))

;; TODO: add a cache to avoid repeated queries about the same class
(defun javadoc-get-url (id)
  "Determine a URL for Javadoc documentation for a Java class or method."
  (or
   (gethash id javadoc-get-url-hashtable)
   (let* ((refs (cdr (or (assoc id javadoc-html-refs)
			 ;; If exact match failed, try case-insensitive
			 (assoc-string id javadoc-html-refs t))))
	  (ref (if (= 1 (length refs))
		   (car refs)
		 (let ((refs-as-lists (mapcar #'(lambda (ref)
						  (cons (ref-to-class ref) ref))
					      refs))
		       (completion-ignore-case t)
		       (choice nil))
		   ;; loop because completing-read can return null
		   (while (or (not choice) (equal choice ""))
		     (setq choice
			   (completing-read "Select an index entry: "
					    refs-as-lists
					    nil t
					    (try-completion "" refs-as-lists))))
		   (cdr (or (assoc choice refs-as-lists)
			    (assoc-string choice refs-as-lists t)))))))
     (puthash id ref javadoc-get-url-hashtable)
     ref)))

(defun javadoc-lookup (id)
  "Visit, via WWW browser, Javadoc documentation for a Java class or method."
  (interactive (list (javadoc-read-id)))
  (let ((ref (javadoc-get-url id)))
    (funcall browse-url-browser-function ref)))
(fset 'jlookup `javadoc-lookup)

(defun java-insert-import (id)
  "Insert an import statement for a Java class."
  (interactive (list (javadoc-read-id)))
  (let* ((ref (javadoc-get-url id))
	 (class (ref-to-class ref))
	 (insertion (concat "import " class ";\n")))
    (save-excursion
      (goto-char (point-min))
      (if (not (search-forward insertion nil t))
	  (progn
	    (or (re-search-forward "^import\\b\\|^class\\b\\|^public\\b\\|^static\\b\\|^@SuppressWarnings\\b" nil t)
		(re-search-forward "^/\\*" nil t))
	    (beginning-of-line)
	    (if (looking-back "\\*/\n*")
		(search-backward "\n/*")) ; note non-regexp search
	    (if (looking-back "^package .*;\n")
		(insert "\n"))
	    (insert insertion)
	    (if (not (looking-at "\n\\|import"))
		(insert "\n")))))))
(fset 'jimport 'java-insert-import)

(defun compilation-java-insert-imports ()
  "Insert an import for every class mentioned in the *compilation* buffer."
  (interactive)
  (with-current-buffer "*compilation*"
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\(?:\\[\\(?:ERROR\\|WARNING\\)\\] \\|:compileJava\\)?\\(.*\\.java\\):\\(?:[0-9]+:\\|\\[[0-9]+,[0-9]+\\]\\) error: cannot find symbol.*\n\\(?:.*\n.*\n\\)?  symbol: +\\(?:variable\\|class\\) \\([A-Za-z0-9_]+\\)\n"
	    nil t)
      (let ((filename (match-string 1))
	    (class-to-import (match-string 2)))
	(save-excursion
	  ;; Requires that compilation is run at top level; makefile must not do "cd", for example.
	  (find-file filename)
	  (if (not buffer-read-only) ;; silently ignore read-only buffers
	      (java-insert-import class-to-import)
	    ))))))


;; ;; I cannot get this to work; the current buffer stays at *compilation* after compile-goto-error.
;; (defun compilation-java-insert-imports ()
;;   "Insert an import for every class mentioned in the *compilation* buffer."
;;   (interactive)
;;   (with-current-buffer "*compilation*"
;;     (goto-char (point-min))
;;     (while (re-search-forward
;; 	    "error: cannot find symbol.*\n.*\n.*\n  symbol:   variable \\([A-Za-z0-9_]+\\)"
;; 	    nil t)
;;       (goto-char (match-beginning 0))
;;       (compile-goto-error)
;;       (message "after compile-goto-error, (current-buffer)=%s" (current-buffer))
;;       (jimport (match-string 1))
;;       (set-buffer "*compilation*")
;;       (forward-line 1)
;;       )))



(defun ref-to-class (str)
  "Given \"java/math/BigInteger.html#abs()\", return \"java.math.BigInteger.abs()\"."
  (let ((prefixes javadoc-ignored-prefixes))
    (while prefixes
      (if (string-match (car prefixes) str)
          (setq str (substring str (match-end 0))
                prefixes nil)
        (setq prefixes (cdr prefixes)))))
  (if (string-match "\\.html#" str)
      (setq str (replace-match "." t t str)))
  (if (string-match "\\.html$" str)
      (setq str (replace-match "" t t str)))
  (while (string-match "/" str)
    (setq str (replace-match "." t t str)))
  str)
;; Testing
;; (equal (ref-to-class "java/math/BigInteger.html#abs()") "java.math.BigInteger.abs()")


(eval-after-load "browse-url"
  ;; add open-paren to list of quoted characters
  '(defun browse-url-netscape (url &optional new-window)
  "Ask the Netscape WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new Netscape window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'."
  (interactive (browse-url-interactive-arg "Netscape URL: "))
  ;; URL encode any `confusing' characters in the URL.  This needs to
  ;; include at least commas; presumably also close parens.
  (while (string-match "[,()]" url)
    (setq url (replace-match
               (format "%%%x" (string-to-char (match-string 0 url))) t t url)))
  (let* ((process-environment (browse-url-process-environment))
         (process (apply 'start-process
                         (concat "netscape " url) nil
                         browse-url-netscape-program
                         (append
                          browse-url-netscape-arguments
                          (if (eq window-system 'w32)
                              (list url)
                            (append
                             (if new-window '("-noraise"))
                             (list "-remote"
                                   (concat "openURL(" url
                                           (if new-window ",new-window")
                                           ")"))))))))
    (set-process-sentinel process
                          (list 'lambda '(process change)
                                (list 'browse-url-netscape-sentinel 'process url))))))
