;;; prog-modes-mde.el --- Michael Ernst's Emacs mode hooks for programming languages

;;; Commentary:

;; Much of the hook stuff should use add-hook instead of just setq.


;;; Code:

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(defvar check-parens-previous-try nil
  "A buffer name if the previous call to check-parens failed.
Nil if the previous call to check-parens succeeded.
There might or might not have been edits between the two attempts.")
;; This should probably check that the buffer was not edited in between...
(defun check-parens-ignore-on-retry ()
  "Like 'check-parens' (which see), but a second retry in a row causes success.
This is good for modes like Perl, where the parser can get confused."
  (if (not (equal check-parens-previous-try (buffer-name)))
      (progn
        (setq check-parens-previous-try (buffer-name))
        (check-parens)
	;; If check-parens finds a problem, it throws an exception
	;; and check-parens-previous-try does not get set to nil.
        (setq check-parens-previous-try nil))))


;; This causes asynchronous behavior.  I need to decide whether I like that.
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)


;; To debug slowness in parsing compilation errors (due to inefficient
;; regexes in compilation-error-regexp-alist), edit
;; `compilation-parse-errors' to add these at the top of the dolist loop:
;;    (message "%s compilation-parse-errors: working on %s" (current-time-string) item)
;;    (message "%s compilation-parse-errors: done with %s" (current-time-string) item)
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (delete 'maven compilation-error-regexp-alist)))
;; Disabled by default because the regexp is slow.  If I am using Maven, run:
;; (use-maven-compilation-error-regexp)
(defun use-maven-compilation-error-regexp ()
  (interactive)
  (add-to-list 'compilation-error-regexp-alist 'maven))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key maps
;;;

(defun swap-return-and-linefeed ()
  "Swap the return and linefeed keys."
  ;; This doesn't use keyboard-translate because it should apply only in
  ;; certain situations.
  ;; Values are in octal.
  (local-set-key "\15" 'newline-and-indent)
  (local-set-key "\12" 'newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C and C++
;;;

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq-default c-recognize-knr-p nil)

(defun mde-c-mode-hook ()
  "Michael Ernst's C mode hook."
  (if (featurep 'elide-head)
      (elide-head))
  (swap-return-and-linefeed)
  (local-set-key "\C-c\C-c" 'compile)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter
        (concat "^\f\\|"
                (regexp-quote "/* ***************************************************************************")
                "\\|///////////////////////////////////////////////////////////////////////////"))
  (c-set-compile-command)

  (dtrt-indent-mode t)

  (setq indent-tabs-mode nil)		; never insert tab characters

  (turn-on-font-lock)
  )
(add-hook 'c-mode-hook 'mde-c-mode-hook)


;; dtrt-indent is the successor to guess-offset.
;; Homepage:  https://github.com/jscheid/dtrt-indent
;; To debug dtrt-indent, execute this in the buffer with the bad guess:
;;   (dtrt-indent-diagnosis)

(if (not (locate-library "dtrt-indent"))
    (message "Could not find dtrt-indent")
  (progn
    (require 'dtrt-indent)
    (dtrt-indent-mode 1)
    (setq dtrt-indent-min-indent-superiority 50.0) ; default 100.0
    (setq dtrt-indent-max-merge-deviation 30.0) ; default 20.0; 40.0 didn't work for me
    ;; (setq dtrt-indent-min-indent-superiority-double 40.0) ; default 100.0
    ))

(if (not (locate-library "yaml-mode"))
    (message "Could not find yaml-mode")
  (progn
    (require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
    (add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)
        (make-local-variable 'inleft-string)
        (setq inleft-string "# ")))
    ))


;; I was afraid this would blow away prefix argument info; maybe it doesn't.
(defadvice c-electric-slash (around not-at-left-margin activate)
  "Don't indent if at left column."
  (interactive "P")
  (if (and (eq ?/ (char-after (- (point) 1)))
           (eq ?\n (char-after (- (point) 2))))
      (self-insert-command (prefix-numeric-value arg))
    ad-do-it))

(defun mde-c++-mode-hook ()
  "Michael Ernst's C++ mode hook."
  (mde-c-mode-hook)
  (make-local-variable 'inleft-string)
  (setq inleft-string "// ")
  (c++-set-compile-command))
(add-hook 'c++-mode-hook 'mde-c++-mode-hook)

;; Infinite loop in c-fill-paragraph if fill-prefix is empty string.
(defadvice c-fill-paragraph (before avoid-empty-string-fill-prefix activate)
  (if (equal fill-prefix "")
      (setq fill-prefix nil)))


;; This is generally useful, but not currently used.

;; Arguments to the c-*-of-* functions seem to be required by Emacs 20.
(defun c-name-of-enclosing-function ()
  "Return the name of the function containing point, or nil
if point is not in a function."
  (save-excursion
    (beginning-of-line)
    (let ((orig-point (point)))
      (c-end-of-defun 1)
      (c-beginning-of-defun 1)
      (if (= (point) (point-min))
          nil
        (let ((bod (point)))            ; beginning of defun
          (c-beginning-of-statement 1)
          (if (< orig-point (point))
              nil
            (if (re-search-forward "\\b\\(\\w+\\)\\s-*(" bod t)
                (match-string 1)
              (progn
                (message "c-name-of-enclosing-function got confused")
                nil))))))))
;; Here is some test code.
(defun message-c-name-of-enclosing-function ()
  (if (eq major-mode 'c-mode)
      (message "%s" (c-name-of-enclosing-function))))
;; (add-hook 'post-command-hook 'message-c-name-of-enclosing-function)
;; ;; To undo:
;; (remove-hook 'post-command-hook 'message-c-name-of-enclosing-function)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java
;;;

(setq auto-mode-alist
      (append '(("\\.javax\\'" . java-mode) ; ConstJava uses ".javax" extension
                ("\\.jpp\\'" . java-mode) ; for preprocessed files; can't specify ".java.jpp"
		("\\.astub\\'" . java-mode) ; Checker Framework annotated libraries
		("\\.java-ORIG\\'" . java-mode)
		("\\.java-SAVE\\'" . java-mode)
		)
              auto-mode-alist))
(defun java-beginning-of-defun (&optional arg)
  "See `c-beginning-of-defun'.
With prefix arg, goes to beginning of class; otherwise to beginning of method."
  (interactive "P")
  (let ((c-beginning-of-defun-prefer-second t))
    (if (equal arg '(4))
        (setq arg 1
              c-beginning-of-defun-prefer-second nil))
    (c-beginning-of-defun (prefix-numeric-value arg))))

(defun java-end-of-defun (&optional arg)
  "See `c-end-of-defun'.
With prefix arg, goes to end of class; otherwise to end of method."
  (interactive "P")
  (let ((c-beginning-of-defun-prefer-second t))
    (if (equal arg '(4))
        (setq arg 1
              c-beginning-of-defun-prefer-second nil))
    (c-end-of-defun (prefix-numeric-value arg))))

;; Appears to mean go to beginning of class rather than method.
(defvar c-beginning-of-defun-prefer-second nil)

;; Problem with c-beginning-of-defun:
;;   in Java, it goes to the beginning of a class.
;;   So enable it to ignore the first result it gets and go to the second,
;;   which is the containing class.
;;   Alternately, I could arrange that it set the mark so I can get back
;;   to where I want to be.

(eval-after-load "cc-cmds"
  '(if (string-equal c-version "5.25")
       (progn
  ;; Changes from Martin Stjernholm <mast@lysator.liu.se>
  ;; to make return value respect documentation and to correct some
  ;; other problems with c-{beg|end}-of-defun.

  (defun c-beginning-of-defun (&optional arg)
    "Move backward to the beginning of a defun.
  With argument, do it that many times.  Negative arg -N
  means move forward to Nth following beginning of defun.
  Returns t unless search stops due to beginning or end of buffer.

  Unlike the built-in `beginning-of-defun' this tries to be smarter
  about finding the char with open-parenthesis syntax that starts the
  defun."
    (interactive "p")
    (unless arg (setq arg 1))
    (if (< arg 0)
        (c-end-of-defun (- arg))
      (while (> arg 0)
        (let ((state (nreverse (c-parse-state)))
              prevbod bod)
          (while (and state (not bod))
            (setq bod (car state)
                  state (cdr state))
            (if (consp bod)
                (setq prevbod (car bod)
                      bod nil)))
          (cond
           (bod (goto-char bod))
           (prevbod (goto-char prevbod))
           (t (goto-char (point-min))
              (setq arg 0)))
          (setq arg (1- arg))))
      (c-keep-region-active)
      (= arg 0)))

  (defun c-end-of-defun (&optional arg)
    "Move forward to next end of defun.  With argument, do it that many times.
  Negative argument -N means move back to Nth preceding end of defun.
  Returns t unless search stops due to beginning or end of buffer.

  An end of a defun occurs right after the close-parenthesis that matches
  the open-parenthesis that starts a defun; see `beginning-of-defun'."
    (interactive "p")
    (if (not arg)
        (setq arg 1))
    (if (< arg 0)
        (c-beginning-of-defun (- arg))
      (while (> arg 0)
        (let ((pos (point))
              eol)
          (while (and (c-safe (down-list 1) t)
                      (not (eq (char-before) ?{)))
            ;; skip down into the next defun-block
            (forward-char -1)
            (c-forward-sexp))
          (c-beginning-of-defun 1)
          (setq eol (c-point 'eol))
          (c-forward-sexp)
          (if (< eol (point))
              ;; Don't move to next line for one line defuns.
              (forward-line 1))
          (when (<= (point) pos)
            (goto-char (point-max))
            (setq arg 0))
          (setq arg (1- arg))))
      (c-keep-region-active)
      (= arg 0)))
  )))

;; This is my enhancement
(eval-after-load "cc-cmds"
  '(defun c-beginning-of-defun (&optional arg)
    "Move backward to the beginning of a defun.
  With argument, do it that many times.  Negative arg -N
  means move forward to Nth following beginning of defun.
  Returns t unless search stops due to beginning or end of buffer.

  Unlike the built-in `beginning-of-defun' this tries to be smarter
  about finding the char with open-parenthesis syntax that starts the
  defun.

  This also respects `c-beginning-of-defun-prefer-second'."
    (interactive "p")
    (unless arg (setq arg 1))
    (if (< arg 0)
        (c-end-of-defun (- arg))
      (while (> arg 0)
        (let ((state (nreverse (c-parse-state)))
              prevbod bod
              prevbod2 bod2)
          (while (and state (not bod))
            (setq bod (car state)
                  state (cdr state))
            (if (consp bod)
                (setq prevbod (car bod)
                      bod nil)))
          (if c-beginning-of-defun-prefer-second
              (while (and state (not bod2))
                (setq bod2 (car state)
                      state (cdr state))
                (if (consp bod2)
                    (setq prevbod2 (car bod2)
                          bod2 nil))))
          (cond
           (bod2 (goto-char bod2))
           (prevbod2 (goto-char prevbod2))
           (bod (goto-char bod))
           (prevbod (goto-char prevbod))
           (t (goto-char (point-min))
              (setq arg 0)))
          (setq arg (1- arg))))
      (c-keep-region-active)
      (= arg 0))))



;; google-java-format.el defines these commands but doesn't affect
;; Emacs's own formatting.  Maybe file google-c-style.el does so?
;; https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el
;; Or maybe dtrt-indent will do the right thing?
(autoload 'google-java-format-region "google-java-format")
(autoload 'google-java-format-buffer "google-java-format")

(defun mde-java-mode-hook ()
  "Michael Ernst's Java mode hook."
  (eval-when-compile (require 'cc-mode)) ; defines java-mode
  (save-match-data
    (mde-c-mode-hook)
    (make-local-variable 'inleft-string)
    (setq inleft-string "// ")
    (setq paragraph-start (concat " */* *<p>\\|" paragraph-separate))
    (setq paragraph-separate (concat ".*<p>\\|" paragraph-separate))
    (define-key java-mode-map "\C-hf" 'javadoc-lookup)
    (define-key java-mode-map [C-M-tab] 'google-java-format-region)
    (make-local-variable 'write-contents-hooks)
    (if (string-match "/\\(checker-framework\\|plume-lib\\|randoop\\)/"
		      (directory-file-name default-directory))
	(progn
	  (make-variable-buffer-local 'before-save-hook)
	  (add-hook 'before-save-hook 'delete-trailing-whitespace)))
    (if (and (buffer-file-name (current-buffer))
             (not (string-match "\.jpp$" (buffer-file-name (current-buffer)))))
        (add-hook 'write-contents-hooks 'check-parens-ignore-on-retry))
    (add-hook 'write-contents-hooks 'check-for-string-equality)
    (java-set-compile-command)

    ;; (subword-mode t)     ; handle CamelCase
    ;; (hs-minor-mode 1) ; hide/show code and comment blocks

    ;; Fill column
    (let ((buf-file-name (buffer-file-name (current-buffer))))
      (if (and buf-file-name
               (string-match "/checker-framework\\|/randoop\\|/daikon" buf-file-name))
          (progn
            ;; Google Java style sets fill column to 100
            (setq fill-column 100)
	    (emacs-25
	      (require 'fill-column-indicator)
	      (fci-mode t)
	      )				; show fill-column indicator
            )))
    ))

(add-hook 'java-mode-hook 'mde-java-mode-hook)


(defun set-tab-stop-list-width (n)
  "Set tab width in current buffer to N.
Interactively, it's probably better to just set variable `tab-width'."
  (interactive "P")
  (if (not (numberp n))
      (error "Supply explicit numeric prefix argument to `set-tab-stop-list-width'"))
  (setq tab-stop-list '())
  ;; was 120, not big enough
  (let ((i (* (/ 500 n) n)))
    (while (> i 0)
      (setq tab-stop-list (cons i tab-stop-list)
            i (- i n)))))

(defun set-tab-width (n)
  (interactive "P")
  (while (or (not (numberp n)) (zerop n))
    (setq n (string-to-number (read-string "Set tab width: "))))
  (setq tab-width n))

(defun java-equals-method-template ()
  "Insert template for a Java `equals' method."
  (interactive)
  (if (not (and buffer-file-name
                (string-match "/\\([^/]+\\)\\.java$" buffer-file-name)))
      (error "Not editing a Java file"))
  (let ((class-name (match-string 1 buffer-file-name)))
    (if (not (bolp))
        (insert "\n"))
    (insert "  public boolean equals(Object obj)
    {
      if (!(other instanceof " class-name ")) {
        return false;
      }
      " class-name " o = (" class-name ") other;
      ")))


(defun check-for-string-equality ()
  "Complain if Java strings are being compared for pointer equality.
This is disabled on lines with a comment containing the string \"interned\"."
  (let ((error-point nil))
    (save-excursion
      (goto-char (point-min))
      ;; Look for `=="' or `"==' (likewise for `!=')
      ;; The "[^+]" is to avoid complaining about:   foo + " != " + bar
      (while
          ;; (re-search-forward "[^=][=!]= *\".[^+]\\|[^+].\" *[=!]=[^=]" nil t)
          (condition-case err
              (re-search-forward (concat "[^=\n][=!]= *\"\\(.?\"\\|.[^+\n].*\"\\)"
                                         "\\|"
                                         "\\(\".?\\|\".*[^+\n].\\)\" *[=!]=[^=\n].*\"")
                                 nil t)
            (error
             (let ((error-message (second err)))
               (if (equal error-message "Stack overflow in regexp matcher")
                   nil
                 (throw 'error error-message)))))
        (if (let ((bol-point (beginning-of-line-point)))
	      (not (or (looking-at ".*//.*interned")
		       ;; line ends with string ending with "=="
		       (and (looking-back "=?= *\"" bol-point)
			    (looking-at ";\n"))
		       ;; if already in comment, suppress warning
		       (looking-back "/[/*].*" bol-point)
		       (looking-back "^[ \t]*\\*.*" bol-point) ; Javadoc comment
		       ;; entire string appears to be "==" or "!=" (as an arg)
		       (looking-back "\(\"[=!]=\"\).*" bol-point)
		       )))
            (progn
              (sit-for 0)               ; perform redisplay
              (if (not (y-or-n-p "Strings being compared with pointer equality; save anyway? "))
                  (progn
                    (message "\"// interned\" comment suppresses warning")
                    (setq error-point (point))
                    (goto-char (point-max)))
                (message "\"// interned\" comment suppresses warning"))))))
    (if error-point
        (progn
          (goto-char error-point)
          (error "Strings being compared with pointer equality"))))
  ;; return nil so this can be used as a write-{file,contents}-hook
  nil)


;; Java stack trace, as printed by a program
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (append
          (list '("\\(?:^[ ][ ]\\|; Stack trace: \\)[A-Za-z0-9_.]+(\\([A-Za-z0-9_.]+\\):\\([0-9]+\\))$" 1 2))
          compilation-error-regexp-alist)))


(autoload 'bdiff-revert-buffer-maybe "bdiff")

(defun update-java-mode-hook-for-gjf ()
  (add-hook 'after-save-hook 'run-google-java-format nil 'local))
(add-hook 'java-mode-hook 'update-java-mode-hook-for-gjf)

(defun run-google-java-format ()
  "Run external program run-google-java-format.py on the file,
if it matches a hard-coded list of directories."
  (interactive)
  (let ((cmd
	 (cond
	  ((or (and (string-match-p "/\\(randoop\\)" (buffer-file-name))
		    (not (string-match-p "CloneVisitor\\.java$" (buffer-file-name))))
	       (and (string-match-p "/daikon" (buffer-file-name))
		    (not (string-match-p "\\.jpp$" (buffer-file-name))))
	       (and (string-match-p "/toradocu" (buffer-file-name))
		    (not (string-match-p "/src/test/resources/" (buffer-file-name))))
	       (and (string-match-p "/plume-lib" (buffer-file-name))
		    (not (string-match-p "WeakHasherMap.java$\\|WeakIdentityHashMap.java$"
					 (buffer-file-name))))
	       (string-match-p "/org/plumelib/" (buffer-file-name)))
	   ;; normal formatting
	   "run-google-java-format.py ")
	  ((and (string-match-p "/checker-framework" (buffer-file-name))
		(not (string-match-p "/checker-framework-inference" (buffer-file-name)))
		(not (string-match-p "/checker/jdk/" (buffer-file-name)))
		(not (string-match-p "\.astub$" (buffer-file-name)))
		)
	   ;; non-standard cammand-line arguments
	   "run-google-java-format.py -a ")
	  (t
	   ;; for all other projects, don't automatically reformat
	   nil))))
    (if cmd
	(progn
	  ;; I would like to avoid the "(Shell command succeeded with no output)"
	  ;; message.
	  (shell-command (concat cmd (buffer-file-name)) "*run-google-java-format*")
	  (bdiff-revert-buffer-maybe)))))

(custom-set-variables
 '(jdee-server-dir (expand-file-name "~/.emacs.d/jdee-server"))
 )


(defadvice jdb (after set-gud-jdb-sourcepath activate)
  "Hard-code some directories whose bin/jar is on my classpath."
  (setq gud-jdb-sourcepath
	(append
	 gud-jdb-sourcepath
	 (mapcar #'expand-file-name
		 '(
		   "~/research/types/annotation-tools/annotation-file-utilities/src"
		   "~/research/types/annotation-tools/scene-lib/src"
		   "~/research/types/annotation-tools/scene-lib/src-devel"
		   "~/research/types/annotation-tools/asmx/src"
		   "~/research/types/checker-framework/checker/src"
		   "~/research/types/jsr308-langtools/src/share/classes"
		   "~/java/java-6-src"
		   "~/java/junit-4.5-src"
		   "~/java/iCal4j/source"
		   )))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Makefiles and shell scripts
;;;

(defun mde-makefile-mode-hook ()
  "Michael Ernst's Makefile mode hook."
  (local-set-key "\C-c\C-c" 'compile)
  (setq inleft-string "# "))
(add-hook 'makefile-mode-hook 'mde-makefile-mode-hook)

(defun mde-xml-mode-hook ()
  "Michael Ernst's XML mode hook."
  (if (and (buffer-file-name)
           (string-equal "build.xml" (file-name-nondirectory (buffer-file-name))))
      (local-set-key "\C-c\C-c" 'compile)))
(add-hook 'sgml-mode-hook 'mde-xml-mode-hook)



(eval-when-compile (require 'sh-script))
(defun mde-sh-mode-hook ()
  "Michael Ernst's shell mode hook."
  (setq inleft-string "# ")
  )
(add-hook 'sh-mode-hook 'mde-sh-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl
;;;

(defvar perl-major-modes '(perl cperl))

(eval-when-compile (require 'perl-mode))

(defun mde-perl-mode-hook ()
  "Michael Ernst's Perl mode hook."
  (swap-return-and-linefeed)
  (setq inleft-string "# ")
  ;; GNU indentation style for Perl
  (setq perl-indent-level                2
        perl-continued-statement-offset  2
        perl-continued-brace-offset      0
        perl-brace-offset                0
        perl-brace-imaginary-offset      0
        perl-label-offset               -2)
  ;; Why is this necessary?
  (setq perl-brace-offset -2)
  (make-local-variable 'compile-command)
  (if buffer-file-name
      (if (looking-at ".* -[^ ]T")
          ;; If shebang line has -T, command line must also
          (setq compile-command (concat "perl -cT " buffer-file-name))
        (setq compile-command (concat "perl -c " buffer-file-name))))
  (local-set-key "\C-c\C-c" 'compile)
  ;; (local-set-key "\C-hf" 'cperl-info-on-command)
  (local-set-key "\C-hf" 'perldoc)
  (make-local-variable 'write-contents-hooks)
  ;; (add-hook 'write-contents-hooks 'maybe-delete-trailing-whitespace)
  (add-hook 'write-contents-hooks 'shadowed-variables-perl-write-hook)
  (add-hook 'write-contents-hooks 'perl-backup-file-check)
  (add-hook 'write-contents-hooks 'check-parens-ignore-on-retry)
  )
;; In Emacs 19.34, this doesn't appear to work.  But once I do (run-hooks
;; 'perl-mode-hook) by hand, it appears to take for future Perl buffers.
(add-hook 'perl-mode-hook 'mde-perl-mode-hook)

(autoload 'perldoc "perldoc" "Run perldoc on the given STRING." t)
(defadvice perldoc (before supply-default activate)
  "Provide a default of the thing at point."
  (interactive
   (list (let* ((default (or (thing-at-point 'word)
                             (thing-at-point 'filename)))
                (default-prompt (and default (concat " (default " default ")"))))
           (completing-read (concat "Perl function or module" default-prompt ": ")
                            (perldoc-functions-alist) nil nil
                            nil nil default)))))
(defadvice perldoc-start-process (after bind-perldoc activate)
  "Set `C-h f' key to run `perldoc'."
  (local-set-key "\C-hf" 'perldoc))

;; Parse Perl error messages
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (append
          (list '("\\bat \\([^ ]+\\) line \\([0-9]+\\)\\($\\|[\\.,]\\)" 1 2)
                '("^syntax error in file \\([^ ]*\\) at line \\([0-9]+\\)," 1 2)
                '("\\bfile \\([^ ]*\\) line \\([0-9]+\\)$" 1 2))
          compilation-error-regexp-alist)))

(defadvice indent-perl-exp (after unspace-brace-hash activate)
  "Insert no space before a hash (#) immediately following an open brace."
  (let* ((eol (save-excursion (end-of-line) (point)))
	 (end (save-excursion
		(while (<= (point) eol)
		  (forward-sexp 1))
		(point))))
    (save-excursion
      (while (re-search-forward "[\{\(]\\(\\s-+\\)#" end t)
	;; replace first submatch by a single space
	(replace-match " " t t nil 1)))))

(defun perl-in-comment ()
  "Return non-nil if in a Perl comment."
  (save-excursion
    (let ((here (point)))
      (beginning-of-line 1)
      (save-match-data
        (re-search-forward "\\(^\\|[^\\\$]\\)#" here t)))))


(defun perl-backup-file-check ()
  "Check that Perl in-place editing isn't done without a backup."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at ".*perl.*-[^ \n]*i[^.]")
        (if (not (y-or-n-p "Perl script does in-place editing with no backup file; save anyway? "))
            (error "Perl script does in-place editing with no backup file")))))


(defun shadowed-variables-perl-write-hook ()
  "When in Perl mode, check for shadowed variables before writing file."
  (if (member major-mode '(perl-mode cperl-mode))
      (let ((result (shadowed-variables-perl t)))
        (while result
          (if (not (y-or-n-p (format "%s; save anyway? " (car result))))
              (error "%s" (car result))
            ;; put it in the *Messages* buffer for later reference
            (message "%s" (car result)))
          (setq result (cdr result))))))

(defun shadowed-variables-perl (&optional no-err)
  "Look for Perl variables with nested \(\"my\"\) scope.
If NO-ERR is non-nil return a list of error messages;
otherwise, raise an error after the first problem is encountered."
  (interactive)
  (let ((opoint (point))
        (result '()))
    (goto-char (point-min))
    (search-forward "\n=cut\n" nil t)
    (while (re-search-forward "\\b\\(for\\(each\\)?\\s-*\\(\(\\s-*\\)?\\)?\\bmy\\b\\|use\\s-+vars\\s-+\\('\\|qw\(\\)" nil t)
      (if (perl-in-comment)
          (forward-line 1)
        (let* ((is-my (equal "my" (buffer-substring (- (point) 2) (point))))
               (is-for (equal "for" (buffer-substring (match-beginning 0)
                                                      (+ (match-beginning 0) 3))))
               (vars-string (buffer-substring
                             (point)
                             (progn
                               ;; "foreach my $arg (@args) doesn't declare "@args"
                               (save-match-data
                                 ;; The re-search-forward forms can fail if
                                 ;; there is a partial statement near file end.
                                 (or (if (looking-at "\\s-*\(")
                                         (re-search-forward "\)" nil t)
                                       (re-search-forward "[;=(]" nil t))
                                     (error "Partial statement"))
                                 (point)))))
               (decl-end (point))
               (scope-end (let ((end (1- decl-end)))
                            (if is-for
                                (let ((for-open-paren-pos
                                       (cond ((= (char-after end) ?\()
                                              end)
                                             ((let ((str-3 (match-string 3)))
                                                (and str-3
                                                     (= ?\( (elt str-3 0))))
                                              (1- (match-end 3))))))
                                  (if for-open-paren-pos
                                      (setq end
                                            (save-excursion
                                              (goto-char for-open-paren-pos)
                                              (no-err (forward-sexp 2))
                                              (point))))))
                            (while (<= end decl-end)
                              (setq end
                                    (if (re-search-backward "{" nil t)
                                        (if (and (not (bobp))
                                                 (or (= ?\\ (char-after (1- (point))))
                                                     (= ?$ (char-after (1- (point))))
                                                     (perl-in-comment)))
                                            end ; don't change value of end
                                          (condition-case nil
                                              (save-excursion
                                                (forward-sexp 1)
                                                (point))
                                            (error (point-max))))
                                      (point-max))))
                            end)))
          (goto-char decl-end)
          ;; we're now at the end of this declaration
          ;; not \\w+\\b because '\w' doesn't include _
          (while (string-match "[$@%][a-zA-Z_][a-zA-Z_0-9]*" vars-string)
            (let* ((this-var (match-string 0 vars-string))
                   ;; look for "${i}" as well as "$i"
                   (this-var-regexp (concat
                                     (regexp-quote (substring this-var 0 1))
                                     "{?"
                                     (regexp-quote (substring this-var 1))))
                   (this-var-regexp (concat this-var-regexp
                                            "\\($\\|[^a-zA-Z0-9_]\\)")))
              (setq vars-string (substring vars-string (match-end 0)))
              (goto-char decl-end)
              ;; These tests aren't quite right; if they find something in
              ;; a comment, they should iterate and look again instead of
              ;; just giving up, as they do now.
              (let ((msg
                     (if (and is-my
                              (save-excursion
                                (and
                                 (not (re-search-forward this-var-regexp scope-end t))
                                 (not (and (= ?\@ (elt this-var 0))
                                           (or
                                            (re-search-forward
                                             (concat "\\$" (substring this-var 1) "\\[")
                                             nil t)
                                            (re-search-forward
                                             (concat "\\$#" (substring this-var 1) "\\b")
                                             nil t))))
                                 (not (and (= ?\% (elt this-var 0))
                                           (re-search-forward
                                            (concat "\\$" (substring this-var 1) "{")
                                            nil t))))))
                         (format "Local variable %s at line %d is never used"
                                 this-var (count-lines (point-min) decl-end))
                       (if (let ((case-fold-search nil))
                             (and (re-search-forward
                                   ;; "{" is for body of "for my ..."
                                   ;; Problem: "# My hack\n}elsif ($tag eq "p")"
                                   ;; looks like a definition of $tag to this.
                                   (concat "\\bmy\\b\\s-*\\(\([^;={)]*\\|[^;={]*\\)"
                                           this-var-regexp)
                                   scope-end t)
                                  (not (perl-in-comment))
                                  ;; Make sure the variable doesn't appear in the ()
                                  ;; section of  foreach my $var (...)
                                  (not (string-match (concat "^my\\s-*[$@%][^;=]*\([^;=()]*"
                                                             this-var-regexp)
                                                     (match-string 0)))))
                           (format "Redefinition of variable %s at lines %d and %d"
                                   this-var
                                   (count-lines (point-min) decl-end)
                                   (count-lines (point-min) (point)))))))
                (if msg
                    (if no-err
                        (setq result (cons msg result))
                      (error "%s" msg)))))))))
    (goto-char opoint)
    (if (called-interactively-p 'interactive)
        (message "No shadowed variables.")
      (nreverse result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python
;;;

;; There are two modes for editing Python code in Emacs:
;;  * python.el is from the Emacs community
;;    Its varables/routines start with "python-".
;;  * python-mode.el is from the Python community
;;    Its varables/routines start with "py-".
;; As of Emacs 23 (and even more so as of Emacs 24), python.el is better:
;; it comes with Emacs, has a few extra features, and works out of the box.


;; Avoid errors if various Python support is not available.
(eval-when-compile (if (locate-library "python-mode") (require 'python-mode)))

(autoload 'python-shell "python" "Start an interactive Python interpreter" t)

(defun mde-python-mode-hook ()
  "Michael Ernst's Python mode hook."
  (swap-return-and-linefeed)
  (setq inleft-string "# ")
  (setq comment-indent-function 'python-comment-indent)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter (mde-page-delimiter ?#))
  ;; Not needed if my patch is accepted.
  ;; (setq comint-prompt-regexp "^\\(>>>\\|(pdb)\\) ")
  ;; This variable only ever honors comments starting with exactly one #,
  ;; never those starting with "##".  I hate that behavior, so I hacked
  ;; my version of python-mode.el.
  (setq python-honour-comment-indentation t)
  (define-key python-mode-map "\C-hf" 'pylookup-lookup)
  (define-key python-mode-map "\C-x-" 'kill-buffer-and-window)
  (make-local-variable 'write-contents-hooks)
  ;; (add-hook 'write-contents-hooks 'maybe-delete-trailing-whitespace)
  ;; (add-hook 'write-contents-hooks 'pyflakes-this-file)
  (if (featurep 'filladapt)
      (filladapt-mode 1))
  (setq indent-tabs-mode nil)
  )
(add-hook 'python-mode-hook 'mde-python-mode-hook)

(defadvice beginning-of-defun (around python-bod activate)
  "Extension to work in Python mode."
  (if (eq major-mode 'python-mode)
      (beginning-of-python-def-or-class 'either (ad-get-arg 0))
    ad-do-it))

(defadvice end-of-defun (around python-eod activate)
  "Extension to work in Python mode."
  (if (eq major-mode 'python-mode)
      (end-of-python-def-or-class 'either (ad-get-arg 0))
    ad-do-it))

;; It would be cleaner to do this kill-buffer-and-window hacking with advice instead.
(defun python-override-kill-buffer-and-window ()
  "Avoid accidental killing of Python shell buffers."
  (interactive)
  (if (string-match "python" (buffer-name))
      (error "You probably meant to hit \"C-c -\", not \"C-x -\"")
    (kill-buffer-and-window)))
(defun shell-override-kill-buffer-and-window ()
  "Avoid accidental killing of shell buffers."
  (interactive)
  (error "Kill shell buffers with C-x k  (M-x kill-buffer)"))
;; Doing this in all shell buffers seems overkill; but on the other hand,
;; I do hate to lose a lot of work in a shell buffer.
;; (defadvice shell (after set-keys activate)
;;   ;; It's too easy to kill a shell buffer, especially a python-shell
;;   ;; in which "C-c -" is bound to a useful keystroke.
;;   (local-set-key "\C-x-" 'shell-override-kill-buffer-and-window))



(defun python-symbol-around-point ()
  "Return a string consisting of the symbol that point is within (or near)."
  (or (symbol-at-point)                 ; defined in "thingatpt.el"
      (save-excursion
        ;; skip backward over open-parentheses and spaces, then try again
        (while (and (not (bobp))
                    (memq (char-syntax (preceding-char)) '(?\( ?\ )))
          (forward-char -1))
        ;; also, (symbol-at-point) fails if just after final character.
        (if (and (not (bobp))
                 (eq ?\w (char-syntax (preceding-char))))
            (forward-char -1))
        (symbol-at-point))))

(defun python-comment-indent ()
  "Choose comment column for Python comments.  Lifted from `lisp-comment-indent'."
  (if (looking-at "\\s<\\s<\\s<")
      (current-column)
    (progn
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defadvice indent-for-tab-command (around move-to-text activate)
  "In Python mode, if at first column, then move to first non-space character."
  (if (and (eq major-mode 'python-mode)
           (zerop (current-column))
           (looking-at "[ \t]+[^ \t\n]"))
      (goto-char (1- (match-end 0)))
    ad-do-it))

;; From https://github.com/tsgates/pylookup
;; I need to have done:
;;   cd ~/emacs && git clone https://github.com/tsgates/pylookup.git && cd pylookup/ && make download
(eval-when-compile (require 'pylookup nil 'noerror))
(if (locate-library "pylookup")
    (progn
(setq pylookup-dir "~/emacs/pylookup")
(add-to-list 'load-path pylookup-dir)
;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))
;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
))

;; To fix later.  Executing this causes the error
;;   "Info-insert-dir: Can't find the Info directory node"
;; but is that because the directory doesn't exist?
;; (eval-after-load "info"
;;   '(progn
;;      (setq Info-directory-list
;;         (cons (substitute-in-file-name "$HOME/emacs/auctex-11.85/doc")
;;               Info-directory-list))))

;; Python error messages
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (append '(("^ *File \"\\(.*\\)\", line \\([0-9]+\\)" 1 2)
                   ("^SyntaxError: ('invalid syntax', ('\\(.*\\)', \\([0-9]+\\), " 1 2))
                 compilation-error-regexp-alist)))


(defun pyflakes-this-file () (interactive)
  (compile (format "pyflakes %s" (buffer-file-name)))
  )

;; (add-hook 'python-mode-hook (lambda () (pyflakes-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp/Scheme programming
;;;

(defvar lisp-major-modes
  '(emacs-lisp-mode lisp-mode fi:common-lisp-mode scheme-mode))

;;;
;;; Lisp
;;;

;; Should also deal with fi:emacs-lisp-mode (which replaces emacs-lisp-mode
;; when ACL extensions are loaded).


(defun mde-lisp-mode-hook ()
  "Michael Ernst's Lisp mode hook."
  (if (featurep 'elide-head)
      (elide-head))
  (swap-return-and-linefeed)
  (make-local-variable 'inleft-string)
  (setq inleft-string ";; ")
  (make-local-variable 'page-delimiter)
  (setq page-delimiter (mde-page-delimiter ?\;))
  ;; Permit fill-paragraph to work
  (setq paragraph-ignore-fill-prefix nil)
  (if (featurep 'filladapt)
      (filladapt-mode 1))
  ;; Not write-file-hooks, as this is about the contents, not the file.
  (make-local-variable 'write-contents-hooks)
  ;; Is this what I really want?
  ;; (add-hook 'write-contents-hooks 'maybe-delete-trailing-whitespace)
  (add-hook 'write-contents-hooks 'check-parens-ignore-on-retry)

  (if (string-match "/plume-lib/"
		    (directory-file-name default-directory))
      (progn
	(make-variable-buffer-local 'before-save-hook)
	(add-hook 'before-save-hook 'delete-trailing-whitespace)))
  (if (eq major-mode 'lisp-mode)
      ;; Not emacs-lisp-mode, fi::*-mode, etc.
      (progn
        (if (fboundp 'fi:clman)
            (progn
              (define-key lisp-mode-map "\C-hf" 'fi:clman)
              (define-key lisp-mode-map "\C-ha" 'fi:clman-apropos)))
        (define-key lisp-mode-map "\C-cl" 'run-lisp))))
(add-hook 'lisp-mode-hook 'mde-lisp-mode-hook)

(put 'with-open-file 'lisp-indent-function 1)
(put 'with-input-from-string 'lisp-indent-function 1)


;;;
;;; Emacs Lisp
;;;

;; Checkdoc is already autoloaded in Emacs 20.

;;; Documentation checking
;; Change this when I trust it more.
(setq checkdoc-autofix-flag 'query)     ; default 'semiautomatic
;; May need to do ispell-check-ispell
(setq checkdoc-spellcheck-documentation-flag 'buffer) ; default nil
;; not useful for variable docstrings, though good for function docstrings
(setq checkdoc-verb-check-experimental-flag nil)
(setq checkdoc-permit-comma-termination-flag t)
(setq checkdoc-triple-semi-comment-check-flag nil)
(setq checkdoc-force-history-flag nil)

;; It's a bit too annoying for this to happen every time.
;; (Maybe t would be less annoying than 'ask.)
(setq maybe-checkdoc-flag nil)

(defvar maybe-checkdoc-flag 'ask
  "Non-nil means `maybe-checkdoc-current-buffer' checks for style;
nil means don't check.
If the value is neither nil nor t, then the user is queried first.")
(make-variable-buffer-local 'maybe-checkdoc-flag)
(defun maybe-checkdoc-current-buffer ()
  "Check documentation/style for current buffer, if not in distribution directory."
  (if (and (or (not (buffer-file-name))
               (not (string-match "/emacs[-0-9./]*/lisp" (buffer-file-name))))
           (or (eq t maybe-checkdoc-flag)
               (and maybe-checkdoc-flag
                    (y-or-n-p "Check style of buffer? "))))
      (let ((checkdoc-spellcheck-documentation-flag nil)
            (checkdoc-autofix-flag 'never))
        ;; Don't save-window-excursion, as checkdoc will change it when it
        ;; shows the "*Style Warnings*" buffer.
        (pop-to-buffer (current-buffer))
        (checkdoc-current-buffer 'take-notes)))
  ;; Always return nil; returning t means the buffer has been written
  nil)


(add-hook 'lisp-mode-hook 'mde-lisp-mode-hook)

(setq auto-mode-alist (append '(("\\.elc$"  . emacs-lisp-mode))
                              auto-mode-alist))
(defun mde-emacs-lisp-mode-hook ()
  "Michael Ernst's Emacs Lisp mode hook."
  (run-hooks 'lisp-mode-hook)
  (add-hook 'write-contents-hooks 'maybe-checkdoc-current-buffer 'append) ; execute last
  )
(add-hook 'emacs-lisp-mode-hook 'mde-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'mde-emacs-lisp-mode-hook)
;; These define-key's needn't be in the hook, as the map is already defined.
;; (Uh, why is it already defined?)
(define-key emacs-lisp-mode-map "\M-\C-x" 'eval-or-compile-defun) ; was eval-defun
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)

;; This is useful enough to define everywhere.
;; (define-key emacs-lisp-mode-map "\C-cf" 'find-function)      ; like M-. (find-tag)
(global-set-key "\C-cf" 'find-function) ; like M-. (find-tag)


(eval-after-load "edebug"
  '(progn
     ;; largely lifted from "let*"
     (def-edebug-spec elib-set-buffer-bind-dll-let*
       (form
        (&rest
         &or symbolp (gate symbolp &optional form))
        body))
     (def-edebug-spec crypt-save-point
       (body))))


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


;; This is most important for systems like Athena where my quota is tight.
;; (Probably javadoc-index shouldn't be under revision control anyway...)
(defvar non-byte-compiled-files
  '("~/.javadoc-index.el"))
(defun purge-undesired-elc-files ()
  "Remove .elc files that should not have been made in the first place."
  (let ((els non-byte-compiled-files))
    (while els
      (let ((elc (concat (car els) "c")))
        (if (file-exists-p elc)
            (delete-file elc)))
      (setq els (cdr els)))))
(run-with-idle-timer 10 nil 'purge-undesired-elc-files)


;;;
;;; Scheme
;;;

(add-hook 'scheme-mode-hook (function (lambda ()
                                        (run-hooks 'lisp-mode-hook)
                                        (require 'scheme-mde))))
(setq auto-mode-alist (append '(("\\.ss$"  . scheme-mode))
                              auto-mode-alist))
;; Emacs 19 and 20 use 'scheme-indent-function, not -hook
(put 'local 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'make-class 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(add-hook 'xscheme-start-hook 'mde-xscheme-start-hook)
;; For some reason, this doesn't seem to work.
(add-hook 'chez-scheme-mode-hook (function (lambda ()
                                             (run-hooks 'scheme-mode-hook))))

;;;
;;; Common Lisp
;;;

;; At UW as of 7/27/99, ACL doesn't support versions of Emacs newer than 19.
;; ;; Allegro Common Lisp (C-c l  starts ACL)
;; (cse
;;  (if (and (or (file-exists-p "/projects/ai/emacs/standard.emacs-4.2.el")
;;            (file-exists-p "/projects/ai/emacs/standard.emacs-4.2.elc"))
;;        (file-exists-p "/usr/local/AllegroCL-4.2/lib/emacs/fi"))
;;      (if (eq system-type 'irix)
;;       ;; ACL doesn't work under Solaris (does work under SunOS 4)
;;       (load (expand-file-name "/projects/ai/emacs/standard.emacs-4.2"))
;;        ;; At least get the documentation advantages.
;;        ;; Don't reload if already loaded.
;;        (if (not (fboundp 'fi::file-contents))
;;         ;; lisp-mode.el doesn't seem to get loaded, or eval-after-load
;;         ;; doesn't work, so do this unconditionally (it's fast anyway).
;;         (if (fboundp 'lisp-mode)
;;             (let ((fi-dir "/usr/local/AllegroCL-4.2/lib/emacs/fi/"))
;;               (require 'cl)
;;               (setq fi::clman-prog (concat fi-dir "clman"))
;;               (setq fi::clman-data (concat fi-dir "clman.data"))
;;               (setq fi::manual-dir (concat fi-dir "manual/"))
;;               (load (concat fi-dir "fi-version"))
;;               (load (concat fi-dir "fi-utils"))
;;               (load (concat fi-dir "fi-clman")))
;;           (eval-after-load "lisp-mode"
;;             '(let ((fi-dir "/usr/local/AllegroCL-4.2/lib/emacs/fi/"))
;;                (require 'cl)
;;                (setq fi::clman-prog (concat fi-dir "clman"))
;;                (setq fi::clman-data (concat fi-dir "clman.data"))
;;                (setq fi::manual-dir (concat fi-dir "manual/"))
;;                (load (concat fi-dir "fi-version"))
;;                (load (concat fi-dir "fi-utils"))
;;                (load (concat fi-dir "fi-clman")))))))))

(defadvice fi::get-lisp-interactive-arguments (before dont-ask activate)
  "Set `first-time' to nil, so that I'm not prompted for ACL arguments."
  (ad-set-arg 0 nil))
(defun mde-fi:common-lisp-mode-hook ()
  "Michael Ernst's \"fi:\" Common Lisp mode hook."
  (run-hooks 'lisp-mode-hook)
  (setq inleft-string ";; "))


(eval-after-load "fi-clman"
  '(define-key fi:clman-mode-map "\C-hf" 'fi:clman))
(eval-after-load "fi-site-init"
  '(progn
     (global-set-key "\C-c\C-m" 'fi:clman)
     (fset 'clman 'fi:clman)
     (fset 'clman-apropos 'fi:clman-apropos)
     ;; this add-hook wasn't working outside the eval-after-load; how about now?
     (add-hook 'fi:common-lisp-mode-hook 'mde-fi:common-lisp-mode-hook)
     ;; I don't just do the define-key in (eval-after-load "fi-modes.elc" ...)
     ;; because that file defines the variables but leaves them set to nil
     (add-hook
      'fi:inferior-common-lisp-mode-hook
      #'(lambda ()
         (define-key fi:inferior-common-lisp-mode-map "\C-hf" 'fi:clman)
         (define-key fi:inferior-common-lisp-mode-map "\ep" 'fi:pop-input)
         (define-key fi:inferior-common-lisp-mode-map "\en" 'fi:push-input)
         (add-hook 'fi::subprocess-filter-output-preprocess-hook
                   'mde-fi::subprocess-filter-output-preprocess-hook)
         (setq comint-prompt-regexp "^\\(\\[[0-9]+\\] \\)?[-a-zA-Z]+([0-9]+): ")))
     (add-hook
      'fi:common-lisp-mode-hook
      #'(lambda ()
         (define-key fi:common-lisp-mode-map "\C-hf" 'fi:clman)
         (define-key fi:common-lisp-mode-map "\C-ha" 'fi:clman-apropos)
         ;; Not eval-or-compile-last-sexp, because I want to see the result.
         (define-key fi:common-lisp-mode-map "\C-x\C-e" 'fi:lisp-eval-last-sexp)))))


(defvar mde-fi::subprocess-redefinition-regexp
  (mapconcat (function identity)
             '("\\(^\\|\n\\)Warning: .*"
               "was defined in"
               ".*\\.lisp"
               "and is now" "being"
               "defined \\(at the top level\\|in" ".*\\.lisp\\)\n")
             "\\( \\|\n +\\)")
  "A separate variable to avoid the `mapconcat' on every execution.
Output that matches this is swallowed by the filter.")

(defun mde-fi::subprocess-filter-output-preprocess-hook (string)
  "Filter out uninteresting errors out of STRING, then return the result."
  ;; not concat, as string might contain "%".
  ;; (message "fi:output[1] = <<%s>>" string)
  (while (string-match mde-fi::subprocess-redefinition-regexp string)
    (setq string (concat (substring string 0 (match-end 1))
                         (substring string (match-end 0)))))
  (if (string-match "\\(^\\|\n\\)Warning: .* was defined in" string)
      (message "Why didn't fi::... match?\n<<<%s>>>" string))
  (if (string-match "was defined in" string)
      (message "WHY didn't fi::... match?\n<<<%s>>>" string))
  (while (string-match "\\(^\\|\n\\);  Note: doing tail merge\n" string)
    (setq string (concat (substring string 0 (match-end 1))
                         (substring string (match-end 0)))))
  (if (string-match "\\(^\\|\n\\); Autoloading for EXCL::COMPLEX-LOOP-EXPANDER:\n" string)
      (setq string (concat (substring string 0 (match-end 1))
                           (substring string (match-end 0)))))
  (if (string-match "\\(^\\|\n\\); Fast loading /projects/null/ai.IRIX/acl4.2/lib/code/loop.fasl.\n" string)
    (setq string (concat (substring string 0 (match-end 1))
                         (substring string (match-end 0)))))
  ;; not concat, as string might contain "%".
  ;; (message "fi:output[2] = <<%s>>" string)
  string)

;; Defaults to eval to maintain emacs bindings and because usually when I'm
;; evaluating code in the buffer I'm willing to put up with it being
;; slightly slower since that makes it that much easier to debug; next
;; emacs session it will be compiled for me, and it gets compiled when I
;; write the buffer anyway.
(defun eval-or-compile-defun (&optional compilep)
  "With prefix argument, compile defun; otherwise, evaluate it.
Interactively, supply optional flag argument COMPILEP."
  (interactive "P")
  ;; The nil arguments mean don't insert the value in the buffer.
  (if compilep
      (compile-defun nil)
    (eval-defun nil)))

(defun mde-inferior-lisp-mode-hook ()
  "CMU Common Lisp Inferior Lisp mode (ie, created via `run-lisp') hook."
  (setq comint-prompt-regexp "^\\(\*\\|[0-9]+\\]+\\) ")
  (if (fboundp 'fi:clman)
      (progn
        (eval-when-compile (require 'inf-lisp))
        (define-key inferior-lisp-mode-map "\C-hf" 'fi:clman)
        (define-key inferior-lisp-mode-map "\C-ha" 'fi:clman-apropos))))
(add-hook 'inferior-lisp-mode-hook 'mde-inferior-lisp-mode-hook)

;; Parse Python (CMUCL) error messages.
;; Problem: these are character numbers, not line numbers.
;;    Reader error at 47821 on #<Stream for file "...">:
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons '("^Reader error at \\([0-9]+\\) on #<Stream for file \"\\(.*\\)\">:" 2 1)
               compilation-error-regexp-alist)))


;; Perhaps I can't use
;;   (modify-syntax-entry ?# ". 14" c-mode-syntax-table)
;;   (modify-syntax-entry ?| ". 23" c-mode-syntax-table)
;; because # currently has syntax quote: "'".

(defadvice forward-comment (after lisp-mode-forward-comment activate)
  "Partially cope with Lisp #|| ... ||# comments.
This skips over them, but the return value isn't sensible."
  (if (and (memq major-mode lisp-major-modes)
           (not ad-return-value)
           (looking-at "#||"))
      (let ((depth 1))
        (forward-char 3)
        (while (and (> depth 0)
                    (re-search-forward "||#\\|#||" nil t))
          (cond ((equal "||#" (match-string 0))
                 (setq depth (1- depth)))
                ((equal "#||" (match-string 0))
                 (setq depth (1+ depth)))
                (t
                 (error "What match? %s" (match-string 0)))))
        (if (zerop depth)
            (progn
              (skip-chars-forward " \t\n\r\f")
              0)
          (error "No closing ||# for #|| comment starter at depth %s" depth)))))


;; Reduce indentation in structure definitions.

(defvar defstruct-comment-column 20
  "*Column to indent comments to in structure definitions.
Nil means use `comment-column'.")

;; \\s< is comment-starter
(defun lisp-comment-indent ()
  "Indent appropriately for Lisp mode.
How does this differ from whatever is built in?"
  (if (looking-at "\\s<\\s<\\s<")
      (current-column)
    (if (looking-at "\\s<\\s<")
        (let ((tem (calculate-lisp-indent)))
          (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           (save-excursion
             (beginning-of-defun)
             (if (looking-at "(defstruct")
                  defstruct-comment-column
               comment-column))))))


;; For planning problems
;; This "99" approximates the current indentation in problem files
(put ':operator 'lisp-indent-function 99)
(put ':operator 'fi:lisp-indent-hook 99)
(put 'define 'lisp-indent-function 1)
(put 'define 'fi:lisp-indent-hook 1)
(put 'forall 'lisp-indent-function 1)
(put 'forall 'fi:lisp-indent-hook 1)

;; For invariant checking
(put 'with-invariants-check 'lisp-indent-function 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation
;;;

(defadvice compile (before save-before-compile activate)
  "Save current buffer before performing compilation.
This avoids a question, the answer to which would surely be \"Yes\"."
  (save-buffer-if-modified))

(defadvice compile (before check-for-bad-regexps activate)
  "Check that elements of compilation-error-regexp-alist do not start with \".*\".
Such regexps have very bad performance, especially for long lines
in compilation output."
  (dolist (cer compilation-error-regexp-alist)
    (if (listp cer)
        (let ((regexp (car cer)))
          (if (string-equal ".*" (substring regexp 0 2))
              (error "Element of compilation-error-regexp-alist starts with \".*\": %s" cer))))))

(defadvice compile (before recompute-compile-command activate)
  "In certain modes, re-compute `compile-command'."
  (if (memq major-mode '(shell-mode))
      (set-compile-command-for-directory)))


(defun next-error-recenter ()
  "Move point to top of screen, if point is so close to bottom that some
explanatory text is probably off-screen."
  (let ((error-win (get-buffer-window next-error-last-buffer)))
    (if error-win
        (with-selected-window error-win
          (if (not (pos-visible-in-window-p (point-max) (selected-window)))
              (let ((lines-from-bottom (count-lines (point) (window-end))))
                (if (< lines-from-bottom 4)
                    (recenter 2))))))))

(add-hook 'next-error-hook 'next-error-recenter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set compilation command
;;;

(defun file-in-super-directory (filename &optional dir)
  "Return a filename if the file exists in this directory or a parent,
or null if it does not exist."
  (if (not dir)
      (setq dir default-directory))
  (setq global default-directory)
  (let ((expanded (expand-file-name filename dir)))
    (if (file-exists-p expanded)
        expanded
      (let ((parent-dir (file-name-directory (directory-file-name dir))))
        (if (null parent-dir)
            ;; If dir = "~/", then directory-file-name = ~ and parent-dir = nil
            nil
          (if (equal dir parent-dir)
              nil
            (file-in-super-directory filename parent-dir)))))))
;; (file-in-super-directory "prog-modes-mde.el")
;; (file-in-super-directory ".emacs")
;; (file-in-super-directory "foobarbazunlikely")
;; (file-in-super-directory "build.xml" nil)

(defun should-set-compile-command ()
  "Return non-nil if the default \"make\" compilation command is inappropriate."
  (and
   ;; editing a file or directory
   (or buffer-file-name
       (memq major-mode '(compilation-mode cvs-mode dired-mode magit-status-mode shell-mode svn-status-mode)))
   ;; Makefile doesn't exist, so we need a different command
   (not (or (file-exists-p (expand-file-name "Makefile"))
            (file-exists-p (expand-file-name "makefile"))
            (file-exists-p (expand-file-name "GNUmakefile"))))))

;; I would like this to work for shell mode, but I would need to make it run
;; as part of M-x compile instead of when the mode is set.
(defun set-compile-command-for-directory ()
  "Returns true if it set the `compile-command' variable.
Sets the variable to an invocation of \"ant\", \"gradle\", \"mvn\", etc.
depending on whether a build.xml, build.gradle, or pom.xml file exists
in this directory or some superdirectory."
  (if (should-set-compile-command)
      (cond ((file-readable-p "build.xml")
             (make-local-variable 'compile-command)
             (setq compile-command "ant -e "))
            ((let ((buildfile (file-in-super-directory
                               "build.xml" default-directory)))
               (and buildfile
                    ;; hack to account for mysterious directory named build.xml
                    (not (file-directory-p buildfile))))
             (make-local-variable 'compile-command)
             (setq compile-command "ant -e -find build.xml "))
            ((file-readable-p "build.gradle")
             (make-local-variable 'compile-command)
             (let ((gradle-command (if (file-readable-p "gradlew")
                                       (setq compile-command "./gradlew")
                                     (setq compile-command "gradle"))))
               (setq compile-command (concat gradle-command " build"))))
            ((file-in-super-directory "build.gradle" default-directory)
             (let* ((buildfile (file-in-super-directory
                                "build.gradle" default-directory))
                    (gradle-command
                     (let ((gradlew (concat (file-name-directory buildfile)
                                            "gradlew")))
                       (if (file-readable-p gradlew)
                           gradlew
                         "gradle"))))
               (make-local-variable 'compile-command)
               (setq compile-command
                     (concat gradle-command " -b " buildfile " build"))))
            ((file-readable-p "pom.xml")
             (make-local-variable 'compile-command)
             (setq compile-command "mvn -B package"))
            ((file-in-super-directory "pom.xml" default-directory)
             (let* ((buildfile (file-in-super-directory
                                "pom.xml" default-directory)))

               (make-local-variable 'compile-command)
               (setq compile-command
                     (concat "mvn -B" " -f " buildfile " package"))))
	    ((file-readable-p "Rakefile")
             (make-local-variable 'compile-command)
             (setq compile-command "rake"))
            )))
(add-hook 'find-file-hooks 'set-compile-command-for-directory)
(add-hook 'dired-mode-hook 'set-compile-command-for-directory)
(add-hook 'compilation-mode-hook 'set-compile-command-for-directory)
(add-hook 'cvs-mode-hook 'set-compile-command-for-directory)
(add-hook 'svn-status-mode-hook 'set-compile-command-for-directory)
(add-hook 'magit-status-mode-hook 'set-compile-command-for-directory)

;; Below are for modes that have a default to use if there is no makefile
;; or build.xml file.

(defun c-set-compile-command ()
  "Set `compile-command' appropriately for C files.
Use as a hook, like so:
  (add-hook 'c-mode-hook 'c-set-compile-command)"
  (if (should-set-compile-command)
      (let ((file-name (file-name-nondirectory buffer-file-name)))
        (make-local-variable 'compile-command)
        (setq compile-command
              (concat "gcc -Wall -g -o "
                      (file-name-sans-extension file-name)
                      " " file-name)))))

(defun c++-set-compile-command ()
  "Set `compile-command' appropriately for C++ files.
Use as a hook, like so:
  (add-hook 'c++-mode-hook 'c++-set-compile-command)"
  (if (should-set-compile-command)
      (let ((file-name (file-name-nondirectory buffer-file-name)))
        (make-local-variable 'compile-command)
        (setq compile-command
              (concat "g++ -Wall -g -o "
                      (file-name-sans-extension file-name)
                      " " file-name)))))

(defun java-set-compile-command ()
  "Set `compile-command' appropriately for Java files.
Use as a hook, like so:
  (add-hook 'java-mode-hook 'java-set-compile-command)"
  (if (and (should-set-compile-command)
           (not (set-compile-command-for-directory)))
      (let ((file-name (file-name-nondirectory buffer-file-name)))
        (make-local-variable 'compile-command)
        (setq compile-command (concat "javac -g " file-name)))))

;; To do: abstract this out to use a variable
(defun special-case-set-compile-command ()
  "Override default, to set `compile-command' properly for specific projects."
  (cond ((string-match "/eclat/" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml compile-no-eclipse"))
        ((string-match "/pastry/" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml -Djsr308.checker.dir=$ch -Djsr308.javac=$anno/langtools/dist/bin/javac -Dorg.checkerframework.checker.interning.InterningChecker checker"))
        ((string-match "/\\(checker\\|framework\\)/tests/\\([^/]*\\)/" default-directory)
         (let ((dir (match-string 2 default-directory)))
           (if (equal dir "src")
               (setq dir "all"))
           (setq dir (replace-regexp-in-string "_" "-" dir))
	   (setq dir (replace-regexp-in-string "flow2" "flow" dir))
           (make-local-variable 'compile-command)
           (setq compile-command (concat "ant -e -find build.xml " dir "-tests"))))
        ;; Checker Framework demos
;;      ((string-match "/annotations/demos/nonnull-interned-demo/checker/" default-directory)
;;       (make-local-variable 'compile-command)
;;       (setq compile-command "cd $anno/demos/nonnull-interned-demo/checker/; ant -e framework"))
;;      ((string-match "/annotations/demos/nonnull-interned-demo/personalblog-demo/" default-directory)
;;       (make-local-variable 'compile-command)
;;       (setq compile-command "cd $anno/demos/nonnull-interned-demo/personalblog-demo/; ant -e"))
;;      ((string-match "/annotations/demos/nonnull-interned-demo/junit/" default-directory)
;;       (make-local-variable 'compile-command)
;;       (setq compile-command "cd $anno/demos/nonnull-interned-demo/junit/; ant -e"))
        ((and buffer-file-name (string-match "demos/nonnull-interned-demo/IGJChecker/src/checkers/types/AnnotationLocation.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml location"))
        ((and buffer-file-name (string-match "demos/nonnull-interned-demo/IGJChecker/src/checkers/types/AnnotatedTypeFactory.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml factory-old"))
        ((and buffer-file-name (string-match "demos/nonnull-interned-demo/IGJChecker/src/org/checkerframework/framework/type/AnnotatedTypeFactory.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml factory"))
        ((and buffer-file-name (string-match "demos/nonnull-interned-demo/junit/tests/JUnitTests.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml test-assert"))
        ;; This only works if you delete the buffer and re-visit the file,
        ;; because compile-command is set when the file is first visited.
        ((and buffer-file-name
              (string-match "demos/nonnull-interned-demo/personalblog-demo/src/net/eyde/personalblog/service/PersonalBlogService.java" buffer-file-name)
              (save-excursion
                (goto-char (point-min))
                (not (search-forward "executeQuery(constructQuery" nil t))))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml pblog-tainting"))
        ((and buffer-file-name
              (string-match "plume-lib-for-demo/java/src/plume/ICalAvailable.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "make typecheck-only"))
        ;; end of Checker Framework demos

        ((string-match "/bzr/.*/doc/en/user-guide/" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "make -C ../../.. doc/en/user-guide/index.html"))
        ((equal (substitute-in-file-name "$HOME/java/plume/") default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "make -C $HOME/bin/src/plume-lib/java"))
	((string-match "^\\(.*commons-io-fork-nikshinde1996[^/]*/\\)" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command (concat "cd " (match-string 1 default-directory) " && mvn -B install")))
        ))
(add-hook 'find-file-hooks 'special-case-set-compile-command 'append)
(add-hook 'dired-mode-hook 'special-case-set-compile-command 'append)
(add-hook 'compilation-mode-hook 'special-case-set-compile-command 'append)
(add-hook 'shell-mode-hook 'special-case-set-compile-command 'append)
(add-hook 'magit-status-mode-hook 'special-case-set-compile-command 'append)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation error regexps
;;;

;; omake pattern is said to be inefficient (http://emacs.1067599.n5.nabble.com/bug-13369-24-1-compile-message-parsing-slow-because-of-omake-hack-td274585.html) and I don't know what it is needed for.
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (delete 'omake compilation-error-regexp-alist)))
;; ... but an equally serious problem is maven, which is very slow on long
;; lines, such as those created when building the Daikon manual.
;; (I tried re-enabling this in March 2016 and it still made Emacs unusable.)
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (delete 'maven compilation-error-regexp-alist)))

;; What language is this for??
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons '("at line \\([0-9]+\\) of file \"\\([^\"]*\\)\"" 2 1)
               compilation-error-regexp-alist)))
;; For dmalloc's ra_info output
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons '("^Line \\([0-9]+\\) of \"\\([^\"]*\\)\"" 2 1)
               compilation-error-regexp-alist)))
;; For linkchecker
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons '("^Parent URL file:\\(.*\\), line \\([0-9]+\\)" 1 2)
               compilation-error-regexp-alist)))
;; For html5validator
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons '("^\\(?::validate\\)?\\(?:WARNING:html5validator.validator:\\)?\"file:\\(.*\\)\":\\([0-9]+\\).\\([0-9]+\\)" 1 2 3)
               compilation-error-regexp-alist)))

;; JUnit under ant, such as
;;    [junit] 	at org.checkerframework.framework.stub.StubParser.processCompilationUnit(StubParser.java:447)
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons (list
                (concat "    \\[junit\\] 	at [^ \n]+"
                        "("
                        "\\([a-zA-Z][a-zA-Z._0-9]+.java\\):\\([0-9,]+\\)"
                        ")$")
                1 2)
               compilation-error-regexp-alist)))

;; I suspect this regexp is extremely inefficient, and I don't understand it.
;; ;; ant output, such as
;; ;; "    [javac] /afs/athena.mit.edu/user/m/e/mernst/6.170/ps0/src/ps0/Ball.java:18: cannot find symbol"
;; (eval-after-load "compile"
;;   '(setq compilation-error-regexp-alist
;;       (cons (list
;;              (concat "^ *\\[[a-z]+\\] "
;;                      "\\([a-zA-Z][-a-zA-Z._0-9]+: ?\\)?" ;; what is this for?
;;                      "\\([a-zA-Z]?:?[^:( \t\n]*[^:( \t\n0-9][^:( \t\n]*\\)[:(][ \t]*\\([0-9]+\\)"
;;                      "\\([) \t]\\|:\\(\\([0-9]+:\\)\\|[0-9]*[^:0-9]\\)\\)")
;;              2 3 6)
;;             compilation-error-regexp-alist)))

;; jdb output, such as
;; "  [4] daikon.VarInfo$1GuardingVisitor.visitSlice (VarInfo.java:1,690)"
;; Notice the comma!!  Yuck...   [Does that actually mean line 1690?]
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons (list
                (concat "  \\[[0-9]+\\] [^ \n]+ "
                        "("
                        "\\([a-zA-Z][a-zA-Z._0-9]+.java\\):\\([0-9,]+\\)"
                        ")$")
                1 2)
               compilation-error-regexp-alist)))


;; JJTree (?): "In file daikon/PrintInvariants.javax: Encountered "const vi =" at line 399, column 13."
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons '("^In file \\([a-zA-Z0-9_$]+\\)\\.[a-zA-Z0-9_$]+: .* at line \\([0-9]+\\), column \\([0-9]+\\)" 1 2 3)
               compilation-error-regexp-alist)))

;; gradle leaves text in front of error message.
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons '("^\\(?::[a-zA-Z]+\\)\\(/.*\\):\\([0-9]+\\): " 1 2)
               compilation-error-regexp-alist)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic compilation of changed files
;;;

;; Emacs 19's local-write-file-hooks (or Emacs 22's write-file-functions)
;; isn't the right thing, since it runs before the file is written.
;; Perhaps rather than this code, I should just add the hooks
;; unconditionally, but have them check major-mode and perhaps do nothing.
;; That would be a bit less efficient, but would be less code.


;;; Ask to compile .el and .scm files after saving them.
;; There might be a cleaner way to do this with local hook variables.

(add-hook 'after-save-hook
          'run-mode-specific-after-save-buffer-hooks)

(defvar mode-specific-after-save-buffer-hooks nil "\
Alist (MAJOR-MODE . HOOK) of after-save-buffer hooks specific to major modes.")

(defun run-mode-specific-after-save-buffer-hooks ()
  "Run hooks in that match the current buffer's major mode.
The hooks are found in `mode-specific-after-save-buffer-hooks'.
A call to this should be put in `after-save-buffer-hooks'."
  (let ((hooks mode-specific-after-save-buffer-hooks))
    (while hooks
      (let ((hook (car hooks)))
        (if (eq (car hook) major-mode)
            (funcall (cdr hook))))
      (setq hooks (cdr hooks)))))

(setq mode-specific-after-save-buffer-hooks
      '((emacs-lisp-mode . ask-to-byte-compile)
        (fi:emacs-lisp-mode . ask-to-byte-compile) ; yuck, Franz changes name
        (scheme-mode . ask-to-scheme-compile)))

(defun ask-to-byte-compile ()
  "Ask the user whether to byte compile the current buffer,
if its name ends in `.el' and the `.elc' file also exists."
  (let ((name (buffer-file-name)))
    (and name (string-match "\\.el$" name)
         (file-exists-p (concat name "c"))
         (if (y-or-n-p (format "Byte-compile %s? " name))
             (byte-compile-file name)
           (message ""))
         )))

(defun ask-to-scheme-compile ()
  "Ask the user whether to compile the current buffer,
if its name ends in `.scm' and the `.bin' or `.com' file also exists."
  (let* ((name (buffer-file-name))
         (root-name (and name
                         (string-match "\\(.*\\)\\.scm$" name)
                         (substring name (match-beginning 1) (match-end 1)))))
    (if root-name
        (cond ((file-exists-p (concat root-name ".com"))
               (if (y-or-n-p (format "Compile (via `cf') %s? " name))
                   (progn
                     (xscheme-send-string (format "(cf \"%s\")" name))
                     (message "Producing %s.com...continue at will" root-name))
                 (message "")))
              ((file-exists-p (concat root-name ".bin"))
               (if (y-or-n-p (format "Compile (via `sf') %s? " name))
                   (progn
                     (xscheme-send-string (format "(sf \"%s\")" name))
                     (message "Producing %s.bin...continue at will" root-name))
                 (message "")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging
;;;

;; Is there not a way to do this in GDB itself?
;; Did I want this only for Vortex, or is there some other reason?
(defvar gud-filter-sigsegv-gcfindlimit-state nil)
(make-variable-buffer-local 'gud-filter-sigsegv-gcfindlimit-state)
(defadvice gud-filter (before skip-sigsegv-in-gcfindlimit activate)
  "Continue past segmentation faults in procedure GC_find_limit."
  (let ((str (ad-get-arg 1)))           ; not "string" as that's the formal name
    ;; Leave this in for debugging, for the time being.
    ;; (message "filter (state %s) got <<%s>>" gud-filter-sigsegv-gcfindlimit-state str)
    (cond
     ((and (not gud-filter-sigsegv-gcfindlimit-state)
           (equal str "Program received signal SIGSEGV, Segmentation fault.\n"))
      (setq gud-filter-sigsegv-gcfindlimit-state 'saw-sigsegv))
     ((and (eq gud-filter-sigsegv-gcfindlimit-state 'saw-sigsegv)
           (string-match "^0x[0-9a-f]* in GC_find_limit ()\n$" str))
      (setq gud-filter-sigsegv-gcfindlimit-state 'saw-sigsegv-in-gcfindlimit))
     ((or (and (eq gud-filter-sigsegv-gcfindlimit-state 'saw-sigsegv-in-gcfindlimit)
               (equal str "(gdb) "))
          (and (not gud-filter-sigsegv-gcfindlimit-state)
               (string-match "\\(^\\|\n\\)Program received signal SIGSEGV, Segmentation fault.\n0x[0-9a-f]* in GC_find_limit ()\n(gdb) $" str)))
      (setq gud-filter-sigsegv-gcfindlimit-state nil)
      ;; (message "Calling gud-cont")
      (gud-cont nil))
     (t
      (setq gud-filter-sigsegv-gcfindlimit-state nil))))
  ;; (message (format "filter ends in state %s" gud-filter-sigsegv-gcfindlimit-state))
  )

(eval-after-load "gud"
  '(progn
     ;; compile defines compilation-error-regexp-alist
     (require 'compile)
     (setq compilation-error-regexp-alist
           (cons '("\\bat \\([^ \n]+\\):\\([0-9]+\\)$" 1 2)
                 compilation-error-regexp-alist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The shell
;;;

(setq comint-scroll-to-bottom-on-input nil)
(setq comint-scroll-to-bottom-on-output nil)
(setq comint-scroll-show-maximum-output nil)

(eval-after-load "shell"
  '(require 'honorary-compile))

;; When I call this, if Info isn't loaded, I get an error
;;  "** reference to free variable Info-current-file"
;; because the byte-compiler appears not to compile advices, but leave them
;; in source form in the .elc file (with or without Emacs 19 compatibility on).
;; This has nothing to do with whether the variable is referenced when the
;; function is actually run.
;; (eval-when-compile (require 'info)) does no good.
;; (defvar Info-current-file) also does no good.
;; But putting eval-when-compile *in the defadvice body* does the trick.

(defadvice shell (around choose-alternate-shell activate)
  "Select the appropriate shell.
If in Python mode, look for a buffer associated with a python process, etc."
  ;; This causes an error from M-x edebug-defun:
  ;;   While compiling toplevel forms:
  ;;     !! Wrong type argument ((consp nil))
  (eval-when-compile (defvar Info-current-file))
  (cond ((and (or (eq major-mode 'cecil-mode)
                  ;; I ought to know which mode I should be in when this
                  ;; search will succeed.
                  (save-excursion
                    (goto-char (point-min))
                    (or (looking-at "#include \"vortex-defs-C\\+\\+\\.h\"\n")
                        (looking-at "#include \"vortex-defs-Cecil\\.h\"\n")
                        (search-forward "\n(** language(\"C++\") **)"
                                        ;; not just 200 if buffer is narrowed
                                        (+ (point-min) 200) t))))
              (get-buffer "*vortex*"))
         (switch-to-buffer (get-buffer "*vortex*")))
        ((and (memq major-mode '(lisp-mode
                                 fi:common-lisp-mode fi:clman-mode
                                 fi:lisp-listener-mode))
              (or (get-buffer "*allegro*")
                  (get-buffer "*inferior-lisp*")))
         (switch-to-buffer (or (get-buffer "*allegro*")
                               (get-buffer "*inferior-lisp*"))))
        ((and (or (memq major-mode '(python-mode))
                  (and (eq major-mode 'Info-mode)
                       (string-match "python" Info-current-file)))
              (get-buffer "*Python*"))
         (switch-to-buffer (get-buffer "*Python*")))
        ((and (memq major-mode '(comint-mode shell-mode inferior-lisp-mode
                                             fi:inferior-common-lisp-mode))
              (not (eobp)))
         (goto-char (point-max)))
        (t
         (let ((processes (process-list))
               (dir default-directory)
               (result nil))
           (while processes
             (let* ((process (car processes))
                    (pbuffer (process-buffer process)))
               (if (and pbuffer
                        (buffer-live-p pbuffer)
                        (not (eq pbuffer (current-buffer)))
                        (eq 'run (process-status process))
                        (equal default-directory
                               (with-current-buffer pbuffer
                                 default-directory))
                        (with-current-buffer pbuffer
                          (and
                           (memq major-mode '(comint-mode shell-mode inferior-lisp-mode
                                             fi:inferior-common-lisp-mode))
                           (not (string-equal (buffer-name) "*Async Shell Command*")))))
                   (setq result pbuffer
                         processes nil)
                 (setq processes (cdr processes)))))
           (if result
               (switch-to-buffer result)
             ad-do-it)))))

;; (defadvice shell-directory-tracker (before handle-back activate)
;;   "Convert \"back\" into \"cd -\", which `shell-directory-tracker' understands."
;;   (if (and shell-dirtrackp
;;            (string-match "^\\s-*back\\s-*$" (ad-get-arg 0)))
;;       (ad-set-arg 0 "cd -")))

;; This does not work; "(dirs)" is executed before the command is executed,
;; even if this command sleeps first.  I should create a new funtion and
;; install it on `comint-output-filter-functions' rather than modify
;; shell-directory-tracker, which is on comint-input-filter-functions.

;; (defadvice shell-directory-tracker (after handle-gcb-gnb activate)
;;   "Handle gcb and gnb commands that checkout or create a branch."
;;   (if (string-match "^\\s-*g[cn]b\\s-+" (ad-get-arg 0))
;;       (shell-resync-dirs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commenting
;;;

(defadvice comment-region (around strip-whitespace activate)
  "If `comment-start' begins with a space and `comment-padding' is 1,
then set `comment-padding' to nil."
  (let ((comment-padding
         (if (and (or (and (numberp comment-padding) (= 1 comment-padding))
                      (and (stringp comment-padding) (equal " " comment-padding)))
                  comment-start
                  (= ?\  (aref comment-start (1- (length comment-start)))))
             nil
           comment-padding)))
    ad-do-it))



(provide 'prog-modes-mde)

;;; prog-modes-mde.el ends here
