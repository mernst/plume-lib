;;; prog-modes-mde.el --- Michael Ernst's Emacs mode hooks for programming languages

;;; Commentary:

;; Much of the hook stuff should use add-hook instead of just setq.


;;; Code:

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(defvar check-parens-previous-try nil)
;; This should probably check that the buffer was not edited in between...
(defun check-parens-ignore-on-retry ()
  "Like 'check-parens' (which see), but a second retry in a row causes success.
This is good for modes like Perl, where the parser can get confused."
  (if (not (equal check-parens-previous-try (buffer-name)))
      (progn
        (setq check-parens-previous-try (buffer-name))
        (check-parens)
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

;; cc-mode is the default as of Emacs 19.30
;; ;; Use cc-mode.el instead of c-mode.el
;; (fmakunbound 'c-mode)
;; (makunbound 'c-mode-map)
;; (fmakunbound 'c++-mode)
;; (makunbound 'c++-mode-map)
;; (makunbound 'c-style-alist)
;; (autoload 'c++-mode  "cc-mode" "C++ editing mode" t)
;; (autoload 'c-mode    "cc-mode" "C editing mode" t)
;; (autoload 'objc-mode "cc-mode" "Objective-C editing mode" t)
;; (autoload 'java-mode "cc-mode" "Java editing mode" t)
;; ;; (setq auto-mode-alist (append '(("\\.C$"  . c++-mode)
;; ;;                           ("\\.cc$" . c++-mode)
;; ;;                           ("\\.c$"  . c-mode)
;; ;;                           ("\\.h$"  . c-mode)
;; ;;                           ("\\.m$"  . objc-mode))
;; ;;                         auto-mode-alist))

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
  ;; yuck, I don't like this.
  ;; (setq c-tab-always-indent 'not-in-literals)

  ;;; Use guess-offset package instead
  ;; Indentation
  ;; (c-set-basic-offset)

  ;; Tab width
  (let ((buf-file-name (buffer-file-name (current-buffer))))
    (if (and buf-file-name
             ;; (string-match "/\\(frikqcc\\)/" buf-file-name)
             )
        (progn
          (setq tab-width 2)
          (make-local-variable 'tab-stop-list)
          (set-tab-stop-list-width 2)))
    (if (and buf-file-name
             (or (string-match "/valgrind/fjalar/dwarf.c" buf-file-name)
                 (string-match "/valgrind/fjalar/readelf.c" buf-file-name))
             )
        (progn
          (setq tab-width 8)
          (make-local-variable 'tab-stop-list)
          (set-tab-stop-list-width 8)))
    )

  (setq indent-tabs-mode nil)

  ;; experimental, 3/26/2002
  (turn-on-font-lock)
  )
(add-hook 'c-mode-hook 'mde-c-mode-hook)


;; dtrt-indent is the successor to guess-offset.
;; For the latest version (wget doesn't work...): http://git.savannah.gnu.org/gitweb/?p=dtrt-indent.git;a=blob_plain;f=dtrt-indent.el;hb=HEAD
;; To debug, execute this in the buffer with the bad guess:
;;   (dtrt-indent-diagnosis)
;; Homepage:  http://savannah.nongnu.org/git/?group=dtrt-indent
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

;; (require 'guess-offset)
;; ;; Eliminated in favor of http://www.emacswiki.org/elisp/guess-offset.el
;; ;; also see (less relevant) http://cc-mode.sourceforge.net/src/cc-guess.el
;; ;; (defvar c-basic-offset-default 2
;; ;;   "Default value used by `c-set-basic-offset'.
;; ;; If nil, use the current value of `c-basic-offset' as the default.")
;; ;; (defun c-set-basic-offset ()
;; ;;   "If possible set `c-basic-offset' to correspond to text in the buffer.
;; ;; `c-basic-offset' is used by C, C++, Java, and related modes.
;; ;; To use, put in your .emacs file:  \(add-hook 'c-mode-hook 'c-set-basic-offset\)"
;; ;;   (make-local-variable 'c-basic-offset)
;; ;;   (if c-basic-offset-default
;; ;;       (setq c-basic-offset c-basic-offset-default))
;; ;;   (save-excursion
;; ;;     (goto-char (point-min))
;; ;;     (while (forward-comment 1)
;; ;;       ;; nothing to do
;; ;;       )
;; ;;     ;; This isn't quite right:  I might find indentation in a C++ initializer
;; ;;     ;;   Foo::Foo(int arg1) : slot1(arg1),
;; ;;     ;;                      : slot2(NULL)
;; ;;     ;; so I should insist that the indentation be found in the body starting with the "{".
;; ;;     ;; Insist on space around brace to avoid finding @link{...} in a comment.
;; ;;     (if (re-search-forward "\\([ \t\n\r]{[ \t\n\r]\\|{$\\)" nil t)
;; ;;   (progn
;; ;;     (while (forward-comment 1)
;; ;;       ;; nothing to do
;; ;;       )
;; ;;     ;; forward-comment actually brings us all the way to non-whitespace
;; ;;     (beginning-of-line)
;; ;;     ;; This isn't quite right:  it could match in comments.  Perhaps demand
;; ;;     ;; a match for c-Java-defun-prompt-regexp or some other keywords.
;; ;;     ;; Forbid a trailing colon to avoid matching labels, which have special
;; ;;     ;; indentation.
;; ;;     (if (re-search-forward "^\\([ \t]+\\)[^ \t\n\r][^\n\r/*].*[^:\n\r]$" nil t)
;; ;;         (progn
;; ;;           (goto-char (match-end 1))
;; ;;           (if (looking-back "^\t+")
;; ;;               (progn
;; ;;                 (setq tab-width 2)
;; ;;                 (make-local-variable 'tab-stop-list)
;; ;;                 (set-tab-stop-list-width 2)))
;; ;;           ;; sanity check
;; ;;           (if (<= (current-column) 8)
;; ;;               (setq c-basic-offset (current-column))))))))
;; ;;   (message "Set c-basic-offset to %d" c-basic-offset))


;;; This comment seems obsolete as of Eamcs 22.1.
;;; ;;; This doesn't work, apparently due to a bug.
;;; ;;; (c-get-syntactic-indentation '((substatement-open 1))) => 2
;;; ;;; because
;;; ;;; (c-calc-offset '(substatement-open 1)) => 2
;;; ;;; because c-basic-common-init calls c-set-style with second argument
;;; ;;; DONT-OVERRIDE t, so the variables are reversed and my changes are at
;;; ;;; the front. It seems like a bug that
;;; ;;; (c-get-style-variables "java2") puts the base styles first (or else
;;; ;;; it's a bug that DONT-OVERRIDE is t).
;;; ;;; I should report this, or else just directly change the "java" style.
;;; ;;; 4/2/2006.
;; Like "java" style, but defaults to 2 rather than 4 spaces of indentation.
(c-add-style "java2" '("java" (c-basic-offset . 2) (substatement-open . 0)))
(setq c-default-style (cons '(java-mode . "java2") c-default-style))


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
                ("\\.jpp\\'" . java-mode)) ; for preprocessed files; can't specify ".java.jpp"
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
    (make-local-variable 'write-contents-hooks)
    ;; (add-hook 'write-contents-hooks 'maybe-delete-trailing-whitespace)
    ;; (add-hook 'write-contents-hooks 'check-for-unbalanced-paren)
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
            (fci-mode t)                ; show fill-column indicator
            )))

    ;; This is orthogonal to dtrt-indent.el, which doesn't set tab-width.
    ;; Really, it shouldn't be necessary:  tabs do not belong in source code files.
    ;; Tab width
    (let ((buf-file-name (buffer-file-name (current-buffer))))
      ;; Dubious, gud: Craig Kaplan
      ;; joie: Geoff Cohen
      (if (and buf-file-name
               (string-match "/\\(Dubious\\|gud\\|joie\\|junit\\)/\\|/joie-" buf-file-name))
          (progn
            (setq tab-width 2)
            (make-local-variable 'tab-stop-list)
            (set-tab-stop-list-width 2)
            (setq indent-tabs-mode t))))
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
    (insert "  public boolean equals( Object other )
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
        (if (not (or (looking-at ".*//.*interned")
                     ;; line ends with string ending with "=="
                     (and (looking-back "=?= *\"") (looking-at ";\n"))
                     ;; if already in comment, suppress warning
                     (looking-back "/[/*].*")
                     (looking-back "^[ \t]*\\*.*") ; Javadoc comment
                     ;; entire string appears to be "==" or "!=" (as an arg)
                     (looking-back "\(\"[=!]=\"\).*")
                     ))
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


;; The individual lines in this function work, but the function doesn't:
;; the lines have to be executed one at a time,
;; because `tags-query-replace' terminates by throwing an error.
;; This is irrelevant if using a code formatter such as google-java-format.
(defun tags-cleanup-java-whitespace ()
  "Clean up whitespace in Java code."
  (interactive)
  ;; avoid matching urls (http://...) and strings ("//")
  (tags-query-replace "\\([^:\"]//\\)\\([^ /\n\t]\\)" "\\1 \\2" nil nil)
  ;; omit "switch" from this regexp
  (tags-query-replace "\\([^_]\\)\\b\\(for\\|if\\|return\\)(" "\\1\\2 (" nil nil)
  (tags-query-replace "){" ") {" nil nil)
  (tags-query-replace "}else{" "} else {" nil nil)
  (tags-query-replace "}else" "} else" nil nil)
  (tags-query-replace "else{" "else {" nil nil)
  (tags-query-replace ";}" "; }" nil nil)
  (tags-query-replace "\\b\\(for (.*;\\)\\([^[:space:]\n].*;\\)\\([^[:space:]\n]\\)" "\\1 \\2 \\3" nil nil)
  (tags-query-replace "\\b\\(for (.*[^;];\\)\\([^ \t\n;]\\)" "\\1 \\2" nil nil)
  (tags-query-replace "\\b\\(throws.*[a-z]\\){" "\\1 {" nil nil)
  ;; avoid matching "else if"; should search for it separately.
  (tags-query-replace "} else \\([^\ni][^\n{]*;\\)$" "} else { \\1 }" nil nil)

  (tags-replace "return (\\(-?[][A-Za-z0-9_.]+\\));$" "return \\1;" nil nil)
  (tags-replace "return (\\(\\\"[^\\\"]*\\\"\\));$" "return \\1;" nil nil)
  ;; This isn't fruitful; the two tags-replace forms above handle all cases.
  ;; (tags-query-replace "return (\\([^()\n]*\\));$" "return \\1;" nil nil)
  )


;; TODO BUG: This fails if a file was not already read into a buffer,
;; perhaps because reading a file into a buffer changes the match-data.
;; So, before running this do: (tags-search "\\`\\(.\\|\n\\)")
;; TODO: Handle adding curly braces when an if or for has a body consisting
;; of a single statement, on the same line as the if or for.
(defun add-curly-braces ()
  "Add curly braces around body of if/for statements whose body is a single
statement.  Does replacement in any file in a currently-visited tags table."
  (interactive)
  ;; To find if/for statements that don't have a curly brace:
  ;; (tags-search "^ *\\b\\(if\\|for\\) (.*) .*[^{\n]$")

  ;; Could also find if/for statements that don't end with an open curly or
  ;; a semicolon, which suggests that the body is a single statement that
  ;; is broken across lines.

  ;; Find if/for statements that end with a close paren, which suggests the
  ;; body is on the next line.  Also else statements that end a line.
  (let ((tags-regex
         "^ *\\(?:}? else *\\)?\\(\\(if\\|for\\) (.*)\\|}? else\\( //.*\\)?\\)\\(.*;\\)?$"))
    (tags-search tags-regex)
    (message "match-data after tags-search: %s" (match-data))
    (while t
      (let ((start-of-if (match-beginning 1)))
      (message "match-data at top of loop: %s" (match-data))
      ;; (message "Before and after point:\n %s\n %s"
      ;;          (buffer-substring (max (- (point) 45) (point-min))
      ;;                            (point))
      ;;          (buffer-substring (point)
      ;;                            (min (+ (point) 45) (point-max))))
      ;; Are tags-search and tags-loop-continue guaranteed to leave the
      ;; match-data set??  Do looking-at to re-set match-data.
      (beginning-of-line)
      ;; Match might not have started at beginning of line
      ;; (if (not (looking-at tags-regex))
      ;;          (error "This can't happen"))

      ;; (message "Looking at (2): %s"
      ;;          (buffer-substring (point)
      ;;                            (min (+ (point) 45) (point-max))))
      ;; (message "1 %s" (match-data))
      (goto-char (match-beginning 1))
      ;; (message "Looking at (3): %s"
      ;;          (buffer-substring (point)
      ;;                            (min (+ (point) 45) (point-max))))
      (let* ((line (buffer-substring (point) (save-excursion (end-of-line) (point))))
             (semicolon-terminated (equal ";" (substring line -1))))
        (if (and (= (how-many-in-string "(" line) (how-many-in-string ")" line))
                 (= 0 (how-many-in-string "{" line))
                 (= 0 (how-many-in-string "//" line))
                 (let ((leading-spaces (progn (string-match "^ *" line)
                                              (match-string 0 line))))
                   (if semicolon-terminated
                       (looking-at (concat leading-spaces "[^ \n].*\n+"
                                           leading-spaces "[^ ]"))
                     (and (looking-at (concat leading-spaces "[^ \n].*\n"
                                              leading-spaces "[ ]"))
                          (not (looking-at (concat leading-spaces "[^ \n].*\n"
                                                   leading-spaces "[ ]*\\(for\\|if\\|try\\)\\b")))))))
            ;; Parens are balanced on the if/for line, and either:
            ;;  * line ends with ";" and next line is equally indented, or
            ;;  * line does not end with ";" and next line is indented more.
            (progn
              (cond ((looking-at " *\\(if\\|for\\)")
                     (forward-sexp 2))
                    ((looking-at " *\\(}? else\\)")
                     ;; (message "2 %s" (match-data))
                     (goto-char (match-end 0)))
                    (t
                     (error "This can't happen.  Looking at: %s"
                            (buffer-substring (point)
                                              (min (+ (point) 45) (point-max))))))
              (insert " \{")
              (if semicolon-terminated
                  (newline-and-indent))
              (re-search-forward ";\\( *//.*\\)?$")
              (while (looking-back "^[^;\n]*//[^\n]*$")
                (re-search-forward ";\\( *//.*\\)?$"))
              (if (looking-at "\n *\\(else\\)")
                  (progn
                    ;; (message "3 %s" (match-data))
                    (goto-char (match-beginning 1))
                    (insert "} "))
                (progn
                  (newline-and-indent)
                  (insert "}")
                  (c-indent-line-or-region))))
          (next-line)))
      (tags-loop-continue)))))


(defun examine-and-cleanup-curly-braces ()
  "Investigate Java code that does not use curly braces for compound statements.
Works over the currently-visited tags table."
  (interactive)
  
  ;; Clean up formatting of curly braces.
  ;; For example, don't put curly braces before if or else on their own line.
  (tags-query-replace "^\\( *\\)}\n *else" "\\1} else")
  (tags-query-replace "\\([{;]\\) *}\n\\( *\\)else" "\\1\n\\2} else")
  (tags-query-replace "}\n *else" "} else")
  (tags-query-replace " else\n *{" " else {")
  (tags-query-replace "\\( *\\)\\(.*) \\|else \\)\\(return\\b[^;\n]*;\\)$" "\\1\\2{\n\\1  \\3\n\\1}")
  (tags-search "\\(if (.*\\|else\\)\n *{")
  (tags-search "[^/] else [^i{/]")
  (tags-search "[^/] else { [^/\n]")


  (tags-search "[^/] else\\( *//.*\\)?\n")
  (tags-search "^ *\\(}? else *\\)?\\(\\(if\\|for\\) (.*)\\|}? else\\( //.*\\)?\\)\n *[^ \n&|]")
  )


(defun downcase-previous-character ()
  "Downcase the character before point."
  (let* ((prev-char (char-before (point)))
         (replacement (downcase prev-char)))
    (if (not (equal prev-char replacement))
        (progn
          (delete-backward-char 1)
          (insert (downcase prev-char))))))

;; Be sure to check the changes; occasionally, the first word of a Javadoc
;; comment is a proper noun.
(defun improve-javadoc-tag-style ()
  "Improve style for Javadoc @param and @return, for files in the current TAGS tables."
  (interactive)
  ;; "End the phrase with a period only if another phrase or sentence follows it."
  ;; Do it twice because matches may overlap.
  (tags-replace "\\(@\\(?:param +[A-Za-z0-9_]+\\|return\\) +[^@./]*\\)\\.\\(\n *\\*/\\|\n *\\\* *@\\)" "\\1\\2")
  (tags-replace "\\(@\\(?:param +[A-Za-z0-9_]+\\|return\\) +[^@./]*\\)\\.\\(\n *\\*/\\|\n *\\\* *@\\)" "\\1\\2")
  (tags-replace "\\(@\\(?:param +[A-Za-z0-9_]+\\|return\\)\\) +- +" "\\1 ")
  ;; Start descriptive text with lowercase letter.
  (let ((case-fold-search nil))
    ;; Emacs can convert case when doing {query-}replace-regexp, but it doesn't
    ;; seem to work with tags-query-replace, so call downcase-previous-character.
    ;; We only do so if the capital letter is at the beginning of a word
    ;; whose other characters are lowercase.
    (tags-search "\\(?:@\\(?:param +[A-Za-z0-9_]+\\|return\\)\\ +\\(?:\n +\* +\\)?\\)\\([A-Z]\\)[a-z]*\\b")
    (goto-char (match-end 1))
    (downcase-previous-character)
    (while t
      (tags-loop-continue)
      (goto-char (match-end 1))
      (downcase-previous-character)))
  ;; PROBLEM: the final tags-loop-continue terminates the whole function so
  ;; nothing here or beyond will be executed.

  ;; TODO:

  ;; To detect incorrect end-of-clause punctuation for @param, @return, @throws, @exception:
  ;; (Run each until it finds no more issues)
  (tags-query-replace "\\(^ *\\* @[^.@/]*\\)\\.\\([ \n]*\\([* \n]* @\\|[* \n]*\\*/\\)\\)" "\\1\\2")
  (tags-search "^ *\\* @[^.@/]*\\.[ \n][^.@/]*\\(\\*/\\|@\\)")
  ;; Missing period at the end of the main part of the Javadoc:
  (tags-search "/\\*\\*[^@/]*\\. [^@/]*[^. \n][ \n]*\\*/")

  )
   
(defun improve-javadoc-code-style ()
  "Improve style for inline code in Javadoc comments, for files in the current TAGS table."

  ;; TODO: as I run these, I may need to convert
  ;;   <code>...</code>
  ;; to
  ;;   {@code ...}
  ;; .  I don't yet have automation for just changing the ones I want --
  ;; I did it in a hackish way the last time.

  (tags-query-replace "&lt;--?&gt;" "&rarr;")
  (tags-query-replace "&lt;--?" "&rarr;")
  (tags-query-replace "--?&gt;" "&rarr;")
  (tags-query-replace "&lt;==?&gt;" "&hArr;")
  (tags-query-replace "&lt;==" "&hArr;")
  (tags-query-replace "==?&gt;" "&rArr;")

  (tags-query-replace "\\({@code[^}]?*\\)&lt;" "\\1<")
  (tags-query-replace "\\({@code[^}]?*\\)&gt;" "\\1>")

  (tags-query-replace "&lt;" "<")
  (tags-query-replace "&gt;" ">")
  )

;; TODO: More Javadoc fixup, for /** and */ not on their own line when they should be:
;; (tags-search "/\\*\\* [A-Z].*\n *\\*")
;; (tags-search "/\\*\\*\\(\n[ \t]*\\*.*\\)+.*\\*/")



(defun declaration-annotations-to-their-own-line ()
  "Move commented declaration annotations to their own line, for files in the current TAGS tables."
  (tags-query-replace "^\\( *\\)/\\*\\(@SideEffectFree\\|@Pure\\|@Deterministic\\)\\*/ \\(public\\|private\\|protected\\|boolean\\|int\\|static\\)" "\\1/*\\2*/\n\\1\\3")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java debugging
;;;


(defadvice jdb (after set-gud-jdb-sourcepath activate)
  "Hard-code some directories whose bin/jar is on my classpath."
  (setq gud-jdb-sourcepath
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
                  ))))



;; ;;; Using the full classpath makes jdb take forever to start.
;; ;;; I could instead advise gud-jdb-find-source-file.
;; ;; (setq gud-jdb-directories
;; ;;       (let* ((classpath-elts (split-string (getenv "CLASSPATH") ":"))
;; ;;        (dirs-on-classpath nil))
;; ;;   (while classpath-elts
;; ;;     (if (file-directory-p (car classpath-elts))
;; ;;         (setq dirs-on-classpath (cons (car classpath-elts) dirs-on-classpath)))
;; ;;     (setq classpath-elts (cdr classpath-elts)))
;; ;;   (nreverse dirs-on-classpath)))
;;
;;
;; ;; As of JDE 2.2.5, I want to use JDEbug, the JDE's own debugger.  But it
;; ;; requires use of M-x customize, and worse, I have to set
;; ;; jde-db-source-directories for each project and must set
;; ;; jde-bug-jpda-directory to something containing jpdi.jar, which seems to
;; ;; no longer exist.
;;
;;
;; ;;; All this is for JDE 2.2.1.
;; ;;; I'll try JDE 2.2.5 without these changes and see how it works.
;; (defvar jdb-use-jde-db t
;;   "*Non-nil if invoking `jdb' should instead call `jde-db'.
;; This doesn't guarantee that JDEbug is used, just that JDE rather than
;; gud runs jdb.")
;;
;; (defun jdb-daikon ()
;;   (interactive)
;;   (if jdb-use-jde-db
;;       (progn
;;      (require 'jde)
;;      (jde-db "daikon.Daikon"))
;;     (error "Implement jdb-daikon for (not jdb-use-jde-db)")))
;;
;; (defadvice jde-db (around interactive-spec (app-class) activate)
;;   "Call `jde-db' interactively."
;;   (interactive
;;    (list
;;     (let ((default (or (and jde-run-application-class
;;                          (not (string= jde-run-application-class ""))
;;                          jde-run-application-class)
;;                     (and (buffer-file-name)
;;                          (concat (jde-db-get-package)
;;                                  (file-name-sans-extension
;;                                   (file-name-nondirectory (buffer-file-name))))))))
;;       (read-from-minibuffer "Java class to debug: "
;;                          default nil nil nil))))
;;   ;; (let ((jde-run-application-class (ad-get-arg 0)))
;;   ;;   ;; This setting keeps getting wiped out for reasons I don't understand.
;;   ;;   (add-hook 'jde-db-mode-hook 'mde-jde-db-mode-hook)
;;   ;;   ad-do-it)
;;   (setq jde-run-application-class (ad-get-arg 0))
;;   ;; This setting keeps getting wiped out for reasons I don't understand.
;;   (add-hook 'jde-db-mode-hook 'mde-jde-db-mode-hook)
;;   ad-do-it
;;   )
;;
;;
;; (defadvice jdb (around use-jde-db (&optional arg) activate)
;;   (interactive)
;;   (let ((orig-arg (ad-get-arg 0)))
;;     (if jdb-use-jde-db
;;      (progn
;;        (require 'jde)
;;        (if orig-arg
;;            (jde-db orig-arg)
;;          (call-interactively 'jde-db)))
;;       (progn
;;      ;; using original jdb, not jde-db
;;      (if (equal gud-jdb-directories (list "."))
;;          (progn
;;            (message "Consider setting `gud-jdb-directories'.")
;;            (sit-for 1)))
;;      (if (not orig-arg)
;;          ;; original jdb `interactive' specification
;;          (setq orig-arg
;;                (read-from-minibuffer "Run jdb (like this): "
;;                                      (if (consp gud-jdb-history)
;;                                          (car gud-jdb-history)
;;                                        (concat gud-jdb-command-name " "))
;;                                      nil nil
;;                                      '(gud-jdb-history . 1))))
;;      ad-do-it))))
;;
;;
;;
;; ;;; Provide missing jdb commands, and permit completion of names.
;; ;;; I should expand this to ordinary jdb as well as jde-db.
;;
;; (defvar jdb-commands-alist
;;   (append
;;    (mapcar #'list
;;         '("threads" "thread" "suspend" "resume" "where" "wherei" "threadgroups"
;;           "threadgroup" "print" "dump" "locals" "classes" "methods" "stop" "stop"
;;           "up" "down" "clear" "step" "stepi" "next" "cont" "catch" "ignore"
;;           "list" "use" "memory" "gc" "load" "run" "!!"  "help" "exit" "quit"))
;;    ;; My abbreviations
;;    '(("finish" . "step up")
;;      ("java" . "run"))))
;;
;; (defun wait-for-process-output ()
;;   "Wait until output appears."
;;   ;; This code is very bad if no output at all.
;;   (while (= bsize (buffer-size))
;;     ;; Edebug barfs on "(sleep-for .1)"
;;     (sleep-for 0 100))
;;   )
;;
;; (defun wait-for-all-process-output ()
;;   "Wait until all process output has (apparently) appeared."
;;   (wait-for-process-output)
;;   (while (let ((new-bsize (buffer-size)))
;;         ;; Edebug barfs on "(sleep-for .2)"
;;         (sleep-for 0 200)
;;         (not (= new-bsize (buffer-size))))
;;     ;; do nothing
;;     )
;;   )
;;
;; (defun jdb-input-sender (proc string)
;;   "Send to PROC a massaged version of STRING."
;;   (message "jdb-input-sender: %s" string)
;;   (let ((first-word (car (split-string string))))
;;     (if first-word
;;      (let* ((expanded (try-completion first-word jdb-commands-alist))
;;             (exp-assoc (assoc (if (eq expanded t) first-word expanded)
;;                               jdb-commands-alist))
;;             (substitution (and exp-assoc
;;                                (or (cdr exp-assoc) (car exp-assoc)))))
;;        (if substitution
;;            (progn
;;              (setq string (concat substitution
;;                                   (substring string (length first-word))))
;;              (setq first-word substitution)
;;              (message "Expanded \"%s\" to \"%s\"" first-word substitution)))))
;;     ;; Expand environment variables
;;     (if (member first-word '("run"))
;;      ;; not the very most efficient implementation (I could keep an
;;      ;; index instead of modifying remaining), but probably fine.
;;      (let ((remaining string)
;;            (new ""))
;;        (while (string-match "\\$\\([a-zA-Z_0-9]+\\)" remaining)
;;          (let* ((var (match-string 1 remaining))
;;                 (varval (getenv var)))
;;            (if varval
;;                (setq new (concat new
;;                                  (substring remaining 0 (match-beginning 0))
;;                                  (or varval ""))
;;                      remaining (substring remaining (match-end 0))))))
;;        (setq new (concat new remaining))
;;        (if (not (equal new string))
;;            (progn
;;              (message "Expanded \"%s\" to \"%s\"" string new)
;;              (setq string new)))))
;;
;;     ;; Expand "*" shell wildcards
;;     (if (member first-word '("run"))
;;      ;; not the very most efficient implementation (I could keep an
;;      ;; index instead of modifying remaining), but probably fine.
;;      (let ((remaining string)
;;            (new ""))
;;        (while (string-match "[ \t]\\(\\([^ \t]*\\)/\\([^ \t/]*\\*[^ \t]*\\)\\)\\b" remaining)
;;          (let* ((exp-beginning (match-beginning 1))
;;                 (exp-end (match-end 1))
;;                 (unexpanded (match-string 1 remaining))
;;                 (expanded (file-expand-wildcards unexpanded)))
;;            (if (null expanded)
;;                (error "Wildcard matches no files: %s" unexpanded))
;;            (setq new (concat new
;;                              (substring remaining 0 exp-beginning)
;;                              (join expanded " "))
;;                      remaining (substring remaining exp-end))))
;;        (setq new (concat new remaining))
;;        (if (not (equal new string))
;;            (progn
;;              (message "Expanded \"%s\" to \"%s\"" string new)
;;              (setq string new)))))
;;
;;     (let ((bsize (buffer-size))
;;        (old-jde-db-stack-depth jde-db-stack-depth))
;;       (comint-simple-send proc string)
;;       (if (member first-word '("up" "down" "print" "dump" "run"))
;;        (progn
;;          (wait-for-all-process-output)
;;          (cond ((member first-word '("up" "down"))
;;                 (if (looking-back comint-prompt-regexp)
;;                     (delete-region (match-beginning 0) (match-end 0)))
;;                 (comint-simple-send proc "where"))
;;                ((member first-word '("print" "dump"))
;;                 (if (not (equal "1" old-jde-db-stack-depth))
;;                     (progn
;;                       (if (looking-back comint-prompt-regexp)
;;                           (delete-region (match-beginning 0) (match-end 0)))
;;                       (comint-simple-send proc (concat "up " (int-to-string (1- (string-to-number old-jde-db-stack-depth))))))))
;;                ((member first-word '("run"))
;;                 (if (looking-back (concat "VM already running. Use 'cont' to continue after events.\n"
;;                                              comint-prompt-regexp))
;;                     (progn
;;                       ;; (delete-region (match-beginning 0) (match-end 0))
;;                       (comint-simple-send proc "exit")
;;                       (wait-for-all-process-output)
;;                       ;; call jdb again
;;                       (if jdb-use-jde-db
;;                           (let* ((bname (buffer-name))
;;                                  (app-class (and bname
;;                                                  (string-match "^\\*debug-?\\(.*\\)\\*" bname)
;;                                                  (match-string 1 bname))))
;;                             (jde-db app-class))
;;                         (call-interactively 'jdb))
;;                       (while (not (get-buffer-process (current-buffer)))
;;                         (message "No process yet.")
;;                         (sit-for 1))
;;                       (comint-simple-send (get-buffer-process (current-buffer)) string))))
;;                (t
;;                 (error "What first-word?")))))))
;;   )
;;
;;
;; ;; As of JDE 2.1.5, this can't be (eval-after-load "jde-db" ...) because it
;; ;; requires jde-classpath-separator to be defined; that is defined in
;; ;; jde.el *after* jde.el does (require 'jde-db).  Terrible style!
;; (eval-after-load "jde"
;;   '(jde-db-set-source-paths (getenv "CLASSPATH")))
;;
;; ;; (setq jde-db-read-app-args t)
;; (setq jde-db-set-initial-breakpoint nil)
;;
;; (defun mde-jde-db-mode-hook ()
;;   (message "Called mde-jde-db-mode-hook")
;;   (make-local-variable 'comint-input-sender)
;;   (setq comint-input-sender (function jdb-input-sender))
;;
;;   ;; Instead of (setq jde-db-set-initial-breakpoint nil) set jde-db-read-app-args.
;;   ;; (Actually, that seems to sometimes hang, so dont...)
;;   ;; ;; Temporary, yuck.  Needed for new jdb, which doesn't like to restart
;;   ;; ;; (ignores "run" commands after first one).
;;   (setq jde-db-set-initial-breakpoint nil)
;;   (message "Finished mde-jde-db-mode-hook")
;;   )
;; ;; This doesn't seem to be working...
;; (add-hook 'jde-db-mode-hook 'mde-jde-db-mode-hook)
;; ;; so try this
;; (eval-after-load "jde"
;;   '(add-hook 'jde-db-mode-hook 'mde-jde-db-mode-hook))
;;
;;
;; ;; I get errors from the way JDE invokes font lock.
;; ;; JDE shouldn't do (require 'font-lock) or use font lock by default.
;; ;; Amazingly, this setq doesn't work because of evilness in
;; ;; `jde-set-variables-init-value'
;; (setq jde-use-font-lock nil)
;; (defadvice jde-set-variables-init-value (after mde-values activate)
;;   "Ignoring current values in favor of custom values is evil!"
;;   (setq jde-use-font-lock nil)
;;   (jde-db-set-source-paths (getenv "CLASSPATH"))
;;   )
;;
;; ;; end of JDE 2.2.1
;;
;; ;;; Compatibility code yanked from 2.2.1, to make the above work
;;
;; ;;;###autoload
;; (defun jde-db-set-source-paths (paths)
;;  "Set the source path list used by JDE to locate Java source files
;; visited by the debugger. PATHS is a list of source paths separated by
;; colons or semicolons, depending on the operating system. Note that
;; for packages, you need enter only the directory containing the
;; top-level package. For example, including '../jdk1.1/src/' enables the
;; JDE to locate all source files provided with JDK1.1. Note also that
;; the paths must end in a slash."
;;  (interactive
;;   "sEnter source paths: ")
;;  (let ((m 0)
;;        (n (string-match jde-classpath-separator paths)))
;;    (setq jde-db-source-directories (list))
;;    (while n
;;      (let ((path (check-source-path (substring paths m n))))
;;        (if path
;;         (setq jde-db-source-directories
;;               (cons path jde-db-source-directories)))
;;        (setq m (+ n 1))
;;        (setq n (string-match jde-classpath-separator paths m))))
;;    (setq n (length paths))
;;    (if (and (> n 0) (< m n))
;;        (let ((path (check-source-path (substring paths m n))))
;;       (if path
;;           (setq jde-db-source-directories
;;                 (cons path jde-db-source-directories)))))
;;    (setq jde-db-source-directories (nreverse jde-db-source-directories))))



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
  (emacs-22+ (setq perl-brace-offset -2))
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
  ;; (add-hook 'write-contents-hooks 'check-for-unbalanced-paren)
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
  (save-excursion
    (let ((end (save-excursion
                 (let ((eol (save-excursion (end-of-line) (point))))
                   (while (<= (point) eol)
                     (forward-sexp 1))
                   (point)))))
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
    (if (interactive-p)
        (message "No shadowed variables.")
      (nreverse result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python
;;;

;; There are two modes for editing Python code in Emacs:
;;  * python-mode.el is from the Python community
;;    Its varables/routines start with "py-".
;;  * python.el is from the Emacs community
;;    Its varables/routines start with "python-".
;; As of Emacs 23, python.el is generally recommended:  it comes with
;; Emacs, has a few extra features, and works out of the box.
;; (Maybe python-mode.el supports ipython better??)
;;
;; The below was originally for python-mode.el, but I'm now switching to
;; python.el and some of the below might be out of date?


;; Does this only work with python-mode.el, not python.el?
;; (require 'ipython nil 'noerror)

;; Avoid errors if various Python support is not available.
(eval-when-compile (if (locate-library "python-mode") (require 'python-mode)))

(autoload 'python-shell "python" "Start an interactive Python interpreter" t)
(defalias 'run-python 'py-shell)

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
  (define-key python-mode-map "\C-c\C-c"  'py-execute-import-or-reload) ; was py-execute-buffer
  (define-key python-mode-map "\C-cb" 'py-execute-buffer) ; was unbound
  (define-key python-mode-map "\C-hf" 'pylookup-lookup)
  (define-key python-mode-map "\C-x-" 'python-override-my-kill-buffer-and-window) ; too easy to hit when I intend "C-c -"
  (make-local-variable 'write-contents-hooks)
  ;; (add-hook 'write-contents-hooks 'maybe-delete-trailing-whitespace)
  ;; (add-hook 'write-contents-hooks 'pyflakes-this-file)
  ;; It isn't enough to rebind M-f and M-b, because I want completion to
  ;; consider _ to split words, too.
  (modify-syntax-entry ?\_ "_"  py-mode-syntax-table)
  ;; This is wrong, because then the end of the defun is considered to be the
  ;; close paren that matches the beginning of the defun open paren (which
  ;; is the open paren for the parameter list).
  ;; (setq defun-prompt-regexp "\ndef [A-Za-z_]+[ \t]*") ; so beginning-of-defun works
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

;; I could do this my-kill-buffer-and-window hacking with advice instead.

(defun python-override-my-kill-buffer-and-window ()
  "Avoid accidental killing of Python shell buffers."
  (interactive)
  (if (string-match "python" (buffer-name))
      (error "You probably meant to hit \"C-c -\", not \"C-x -\"")
    (my-kill-buffer-and-window)))

;; Problem:  this sets the shell-mode-map, not just the map for python shells.
(defadvice py-shell (after set-keys activate)
  "Unset \"\C-x-\", which is easy to type accidentally in Python mode."
  (local-set-key "\C-x-" 'python-override-my-kill-buffer-and-window))

(defun shell-override-my-kill-buffer-and-window ()
  "Avoid accidental killing of shell buffers."
  (interactive)
  (error "Kill shell buffers with C-x k  (M-x kill-buffer)"))

;; Doing this in all shell buffers seems overkill; but on the other hand,
;; I do hate to lose a lot of work in a shell buffer.
;; (defadvice shell (after set-keys activate)
;;   ;; It's too easy to kill a shell buffer, especially a python-shell
;;   ;; in which "C-c -" is bound to a useful keystroke.
;;   (local-set-key "\C-x-" 'shell-override-my-kill-buffer-and-window))


;;; Don't do this; just set variable py-jump-on-exception!
;; ;; Add this to end of py-process-filter; see defadvice below.
;; (defun py-postprocess-process-filter ()
;;   "If a Python error occurs, jump to the source location.
;; If variable `py-jump-on-exception' is nil, do nothing."
;;   ;; It doesn't work to wrap this whole body in save-excursion.
;;   (if (and py-jump-on-exception
;;         (looking-back "\n>>> ")
;;         (save-excursion
;;           (forward-line -1)
;;           (looking-at "[A-Za-z]*Error\\b")))
;;       (if (save-excursion
;;          (re-search-backward py-traceback-line-re (- (point) 300) 'no-error))
;;        (let ((file (match-string 1))
;;              (lineno (string-to-number (match-string 2))))
;;          (if (not (equal file "<stdin>"))
;;              (py-jump-to-exception file lineno))))))
;;
;; This is gratuitous; I can just use C-c - instead.  (Maybe add that
;; binding to next-error, or advise it to sometimes do that instead.)
;; (defadvice py-process-filter (after jump-to-exception activate)
;;   (py-postprocess-process-filter))


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

;; Superseded by the below.
;; ;; Lifted from scheme-describe-function; they should be re-merged (better,
;; ;; use the Emacs 20 functionality for this).
;; (defun python-describe-function (function)
;;   "Display manual entry regarding a FUNCTION (a string or symbol).
;; When called interactively, prompts for the symbol (defaults to the function
;; point is currently near)."
;;   (interactive (list (let* ((default (python-symbol-around-point))
;;                          (fn (read-string (format "Describe Python function (default %s): " default))))
;;                     (if (string= fn "") default fn))))
;;   (if (symbolp function) (setq function (symbol-name function)))
;;   (let (message)
;;     (save-window-excursion
;;       (info)
;;       ;; was (Info-guess-node 'python-mode); we've partial-evaluated it.
;;       (eval-when-compile (require 'info))
;;       (if (not (string-match "python-lib" Info-current-file))
;;        (progn
;;          (Info-directory)
;;          (Info-menu "Python-lib")))
;;       (setq message (condition-case nil
;;                      (Info-index function)
;;                    (error nil))))
;;     (if message
;;      (progn
;;        (switch-to-buffer-other-window "*info*")
;;        (recenter)
;;        (message "%s" message))
;;       (error (format "No \"%s\" in index." function)))))

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

(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (append '(("^ *File \"\\(.*\\)\", line \\([0-9]+\\)" 1 2)
                   ("^SyntaxError: ('invalid syntax', ('\\(.*\\)', \\([0-9]+\\), " 1 2))
                 compilation-error-regexp-alist)))

(defadvice py-execute-import-or-reload (before save-first activate)
  "Save current buffer first."
  ;; test of buffer-modified-p prevents "(No changes need to be saved)" message
  (if (and buffer-file-name (buffer-modified-p))
      (save-buffer)))

;; Problem:  after doing this, Python input is queued until the next
;; comint-send-input (RET) typed in the process.  That's OK if I actually
;; do some Python work in between reloading new versions of a file.  The
;; two calls to comint-send-input in this function appear to be in vain; I
;; could probably eliminate them and turn this back into a before advice.
(defadvice py-execute-import-or-reload (around exit-debugger activate)
  "If in Python debugger, offer to quit before importing/reloading a file."
  (let* ((py-process (get-process "Python"))
         (py-buffer (and py-process (process-buffer py-process)))
         (exit-debugger-p nil))
    (if (and py-buffer
             (with-current-buffer py-buffer
               (equal "(Pdb) "
                      (buffer-substring (max 1 (- (point-max) 6)) (point-max)))))
        (if (y-or-n-p "Exit Python debugger first? ")
            (with-current-buffer py-buffer
              (setq exit-debugger-p t)
              (goto-char (point-max))
              (process-send-string py-process "q\n")
              (comint-send-input))
          (message "Warning: in Python debugger, effects may be transient")))
    ad-do-it
    (if exit-debugger-p
        ;; This seems to be necessary to keep the file evaluation from
        ;; merely being buffered up until the next comint-send-input.
        ;; This is not so great if there is partial input, but since
        ;; we saw the debugger prompt at end of buffer, there isn't.
        (with-current-buffer py-buffer
          (comint-send-input)))
    ))

(defun pyflakes-this-file () (interactive)
  (compile (format "pyflakes %s" (buffer-file-name)))
  )

;; (add-hook 'python-mode-hook (lambda () (pyflakes-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp/Scheme programming
;;;

(defvar lisp-major-modes '(emacs-lisp-mode lisp-mode fi:common-lisp-mode
                                           scheme-mode))

;;;
;;; Lisp
;;;

;; Should also deal with fi:emacs-lisp-mode (which replaces emacs-lisp-mode
;; when ACL extensions are loaded).

;; XEmacs disables this in favor of ilisp.
(xemacs (autoload 'run-lisp "inf-lisp" nil t))


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
  ;; (add-hook 'write-contents-hooks 'check-for-unbalanced-paren)
  (add-hook 'write-contents-hooks 'check-parens-ignore-on-retry)

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

(defun save-if-modified ()
  "Save current buffer if it is modified."
  ;; test of buffer-modified-p prevents "(No changes need to be saved)" message
  (if (and buffer-file-name (buffer-modified-p))
      (save-buffer)))

(defadvice compile (before save-before-compile activate)
  "Save current buffer before performing compilation.
This avoids a question, the answer to which would surely be \"Yes\"."
  (save-if-modified))

(defadvice compile (before check-for-bad-regexps activate)
  "Check that elements of compilation-error-regexp-alist do not start with \".*\".
Such regexps have very bad performance, especially for long lines
in compilation output."
  (dolist (cer compilation-error-regexp-alist)
    (if (listp cer)
        (let ((regexp (car cer)))
          (if (string-equal ".*" (substring regexp 0 2))
              (error "Element of compilation-error-regexp-alist starts with \".*\": %s" cer))))))


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
       (memq major-mode '(compilation-mode cvs-mode dired-mode svn-status-mode)))
   ;; Makefile doesn't exist, so we need a different command
   (not (or (file-exists-p (expand-file-name "Makefile"))
            (file-exists-p (expand-file-name "makefile"))
            (file-exists-p (expand-file-name "GNUmakefile"))))))

(defun ant-set-compile-command ()
  "Returns true if it set the `compile-command' variable.
Sets the variable to an invocation of \"ant\" if a build.xml file exists
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
             (if (file-readable-p "gradlew")
                 (setq compile-command "./gradlew ")
               (setq compile-command "gradle ")))
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
               (setq compile-command (concat gradle-command " -b " buildfile " build"))))
            ((file-readable-p "pom.xml")
             (make-local-variable 'compile-command)
             (setq compile-command "mvn ")))))
(add-hook 'find-file-hooks 'ant-set-compile-command)
(add-hook 'dired-mode-hook 'ant-set-compile-command)
(add-hook 'compilation-mode-hook 'ant-set-compile-command)
(add-hook 'cvs-mode-hook 'ant-set-compile-command)
(add-hook 'svn-status-mode-hook 'ant-set-compile-command)
;; There was no svn-status-mode-hook before "psvn.el 23079 2007-01-17".
;; (defadvice svn-status-mode (after ant-set-compile-command activate)
;;   (ant-set-compile-command))

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
           (not (ant-set-compile-command)))
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
           (make-local-variable 'compile-command)
           (setq compile-command (concat "ant -e -find build.xml " dir "-tests"))))
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
        ((string-match "/bzr/.*/doc/en/user-guide/" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "make -C ../../.. doc/en/user-guide/index.html"))
        ((equal (substitute-in-file-name "$HOME/java/plume/") default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "make -C $HOME/bin/src/plume-lib/java"))
        ))
(add-hook 'find-file-hooks 'special-case-set-compile-command 'append)
(add-hook 'dired-mode-hook 'special-case-set-compile-command 'append)
(add-hook 'compilation-mode-hook 'special-case-set-compile-command 'append)


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

;; gradle leaves text in front of error message
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (cons '("^\\(?::compileTestJava\\)\\(/.*\\):\\([0-9]+\\): " 1 2)
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

(defadvice shell-directory-tracker (before handle-back activate)
  "Convert \"back\" into \"cd -\", which `shell-directory-tracker' understands."
  (if (and shell-dirtrackp
           (string-match "^\\s-*back\\s-*$" (ad-get-arg 0)))
      (ad-set-arg 0 "cd -")))


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

;;; Comment indentation

(defun region-indentation (beg end)
  "Return the minimum indentation of any line in the current region."
  (save-excursion
    (let ((result nil))
      (goto-char (min beg end))
      (beginning-of-line)
      (setq end (max beg end))
      (while (< (point) end)
        (skip-chars-forward " \t")
        (if (not (= ?\n (char-after)))
            (if result
                (setq result (min result (current-column)))
              (setq result (current-column))))
        (forward-line 1))
      result)))

;; Implementation 1
(defadvice comment-region (around indent-comment activate)
  "Place comment characters as far to the right as possible (not in column 0)."
  (let* ((my-beg (ad-get-arg 0))
         (my-end (ad-get-arg 1))
         (indent (region-indentation my-beg my-end))
         (beg-marker (make-marker))
         (end-marker (make-marker)))
    (set-marker beg-marker (min beg end))
    (set-marker end-marker (max beg end))
    (indent-rigidly beg end (- indent))
    ;; indent-rigidly can change leading space to tabs; when we
    ;; indent-rigidly a second time, this can make things look screwy.
    (untabify beg-marker end-marker)
    (ad-set-arg 0 beg-marker)
    (ad-set-arg 1 end-marker)
    ad-do-it
    (indent-rigidly beg-marker end-marker indent)))

;; ;; Implementation 2
;; ;; Needs to be fixed to remove the indentation first.
;; (defadvice comment-region (around indent-comment activate)
;;   (or comment-start (error "No comment syntax is defined"))
;;   (let ((indent (region-indentation (ad-get-arg 0) (ad-get-arg 1)))
;;      ;; "my-" prefix to avoid advice probs.
;;      (my-arg (ad-get-arg 2))
;;      (my-cs comment-start)
;;      (my-ce comment-end))
;;     ;; Lifted directly from comment-region
;;     (if (not (consp my-arg))
;;      (progn
;;        (setq my-arg (prefix-numeric-value my-arg))
;;        ;; For positive arg > 1, replicate the comment delims now,
;;        ;; then insert the replicated strings just once.
;;        (while (> my-arg 1)
;;          (setq my-cs (concat my-cs comment-start)
;;                my-ce (concat my-ce comment-end))
;;          (setq my-arg (1- my-arg)))
;;        (setq my-cs (concat (make-string indent ? ) my-cs))
;;        (ad-set-arg 2 1)))
;;     (let ((comment-start my-cs)
;;        (comment-end my-ce))
;;       ad-do-it)))


;; ;; Implementation 3; requires modification of comment-region
;;
;; (defadvice comment-region (around indent-comment activate)
;;   (let ((comment-indentation (region-indentation (ad-get-arg 0) (ad-get-arg 1))))
;;     ad-do-it))
;;
;;
;; ;; Taken from simple.el of Emacs 20.4
;;
;; (defvar comment-indentation 0
;;   "Number of spaces `comment-region' puts between left margin and comment chars.")
;;
;; (defun comment-region (beg end &optional arg)
;;   "Comment or uncomment each line in the region.
;; With just C-u prefix arg, uncomment each line in region.
;; Numeric prefix arg ARG means use ARG comment characters.
;; If ARG is negative, delete that many comment characters instead.
;; Comments are terminated on each line, even for syntax in which newline does
;; not end the comment.  Blank lines do not get comments."
;;   ;; if someone wants it to only put a comment-start at the beginning and
;;   ;; comment-end at the end then typing it, C-x C-x, closing it, C-x C-x
;;   ;; is easy enough.  No option is made here for other than commenting
;;   ;; every line.
;;   (interactive "r\nP")
;;   (or comment-start (error "No comment syntax is defined"))
;;   (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
;;   (save-excursion
;;     (save-restriction
;;       (let ((cs comment-start) (ce comment-end)
;;          (cp (when comment-padding
;;                (make-string comment-padding ? )))
;;          numarg)
;;      (if (consp arg) (setq numarg t)
;;        (setq numarg (prefix-numeric-value arg))
;;        ;; For positive arg > 1, replicate the comment delims now,
;;        ;; then insert the replicated strings just once.
;;        (while (> numarg 1)
;;          (setq cs (concat cs comment-start)
;;                ce (concat ce comment-end))
;;          (setq numarg (1- numarg))))
;;      ;; Loop over all lines from BEG to END.
;;      (narrow-to-region beg end)
;;      (goto-char beg)
;;      (if (or (eq numarg t) (< numarg 0))
;;          (while (not (eobp))
;;            (let (found-comment)
;;              ;; Delete comment start from beginning of line.
;;              (if (eq numarg t)
;;                  (while (looking-at (regexp-quote cs))
;;                    (setq found-comment t)
;;                    (delete-char (length cs)))
;;                (let ((count numarg))
;;                  (while (and (> 1 (setq count (1+ count)))
;;                              (looking-at (regexp-quote cs)))
;;                    (setq found-comment t)
;;                    (delete-char (length cs)))))
;;              ;; Delete comment padding from beginning of line
;;              (when (and found-comment comment-padding
;;                         (looking-at (regexp-quote cp)))
;;                (delete-char comment-padding))
;;              ;; Delete comment end from end of line.
;;              (if (string= "" ce)
;;                  nil
;;                (if (eq numarg t)
;;                    (progn
;;                      (end-of-line)
;;                      ;; This is questionable if comment-end ends in
;;                      ;; whitespace.  That is pretty brain-damaged,
;;                      ;; though.
;;                      (while (progn (skip-chars-backward " \t")
;;                                    (and (>= (- (point) (point-min)) (length ce))
;;                                         (save-excursion
;;                                           (backward-char (length ce))
;;                                           (looking-at (regexp-quote ce)))))
;;                          (delete-char (- (length ce)))))
;;                  (let ((count numarg))
;;                    (while (> 1 (setq count (1+ count)))
;;                      (end-of-line)
;;                      ;; this is questionable if comment-end ends in whitespace
;;                      ;; that is pretty brain-damaged though
;;                      (skip-chars-backward " \t")
;;                      (if (>= (- (point) (point-min)) (length ce))
;;                          (save-excursion
;;                            (backward-char (length ce))
;;                            (if (looking-at (regexp-quote ce))
;;                                (delete-char (length ce)))))))))
;;              (forward-line 1)))
;;
;;        (when comment-padding
;;          (setq cs (concat cs cp)))
;;        (while (not (eobp))
;;          ;; Insert at beginning and at end.
;;          (if (looking-at "[ \t]*$") ()
;;            ;; test added by MDE
;;            (if (and (boundp 'comment-indentation) comment-indentation
;;                (move-to-column-force comment-indentation))
;;            (insert cs)
;;            (if (string= "" ce) ()
;;              (end-of-line)
;;              (insert ce)))
;;          (search-forward "\n" nil 'move)))))))


(provide 'prog-modes-mde)

;;; prog-modes-mde.el ends here
