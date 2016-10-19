;;; util-mde.el --- Emacs Lisp utilities

;; Copyright (C) 1990-1993 Michael D. Ernst <mernst@theory.lcs.mit.edu>

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords:

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug fixes
;;;

;; String-equal permits symbols, whose print names are used instead.  One
;; would hope that it would err when passed nil, or at least that it
;; wouldn't return t for (string-equal () "nil").  Therefore, it should be
;; avoided.
;; However, sometimes it's really the right thing to use, if we know that one
;; or both arguments aren't symbols.  How does the byte-compiler treat it,
;; compared to equal?  (That is, which is more efficient?  Probably equal.)
(make-obsolete 'string-equal 'equal)


;; You usually want forward-line, not next-line.
(make-obsolete
 'next-line "forward-line is easier to use and more reliable than next-line")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbers
;;;

(defmacro %-base-1 (x y)
  "Like %, but if (% x y) = 0, (%-base-1 x y) = y."
  `(let* ((y-value ,y)
            (result (% ,x y-value)))
       (if (zerop result) y-value result)))
(defalias 'mod-base-1 '%-base-1)

(defun power (x y)
  "Simplistic x^y for integral x, y > 0.  Also consider using built-in `expt'."
  (let ((result 1))
    (while (> y 0)
      (setq result (* x result)
            y (1- y)))
    result))

;; This checks the more likely divisors first.
(defun primep (x)
  (or (= 2 x)
      (and (oddp x)
           (let ((limit (floor (sqrt x)))
                 (divisor 3)
                 (result t))
             (while (and result (<= divisor limit))
               (let ((div-result (/ x divisor)))
                 (setq result (not (= x (* divisor div-result)))
                       divisor (+ 2 divisor))))
             result))))
;; Testing
;; (and (primep 97) (primep 2) (primep 7) (not (or (primep 6) (primep 88))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;

;; (defun empty-string-p (string)
;;   "Return t if its argument (a string) has length 0, nil otherwise.
;; Don't apply this to nil, as the result will be nil."
;;   (string-equal "" string))
(defmacro empty-string-p (string)
  "Return t if STRING has length 0, nil otherwise.
Don't apply this to nil, as the result will be nil."
  `(equal "" ,string))

(defun empty-string-or-nil-p (string-or-nil)
  "Return t if its argument is nil or a zero-length string, nil otherwise."
  (or (not string-or-nil)
      (equal "" string-or-nil)))

(defmacro string-shorter-p (string1 string2)
  "Return t if STRING1 is shorter than STRING2, nil otherwise."
  `(< (length (, string1)) (length (, string2))))

;; This needs to remain a function so it can be passed as an argument.
(defun string-shorter-p-function (string1 string2)
  "Return t if STRING1 is shorter than STRING2, nil otherwise.
Good for passing to `sort' in order to sort strings by length."
  (< (length string1) (length string2)))

(defmacro string-longer-p (string1 string2)
  "Return t if STRING1 is longer than STRING2, nil otherwise."
  `(> (length (, string1)) (length (, string2))))

;; This needs to remain a function so it can be passed as an argument.
(defun string-longer-p-function (string1 string2)
  "Return t if STRING1 is longer than STRING2, nil otherwise.
Good for passing to sort in order to sort strings by length."
  (> (length string1) (length string2)))

(defun starts-with (string prefix)
  "Return t if STRING starts with PREFIX."
  (let ((string-len (length string))
        (prefix-len (length prefix)))
    (and (>= string-len prefix-len)
         (equal prefix (substring string 0 prefix-len)))))
(defun test-starts-with ()
  "Test the `starts-with' routine."
  (assert (starts-with "a" "a"))
  (assert (starts-with "a" ""))
  (assert (not (starts-with "a" "b")))
  (assert (not (starts-with "a" "abc")))
  (assert (starts-with "abc" "a"))
  (assert (starts-with "abc" ""))
  (assert (not (starts-with "abc" "b")))
  (assert (starts-with "abc" "abc"))
  )
;; (test-starts-with)


(defun right-justify (string fieldwidth)
  (format (format "%-%%ds" fieldwidth) string))
;; (right-justify "foo" 2)
;; (right-justify "foo" 3)
;; (right-justify "foo" 4)
;; (right-justify "foo" 12)

;; Is it necessary to return the argument at the end?  I think that the
;; side-effects will propagate back in any case.
(defun string-substitute-substring-same-length (new old-regexp string)
  (let ((i 0)
        j
        max-j)
    (while (setq i (string-match old-regexp string i))
      (setq j 0
            max-j (length new))
      (while (< j max-j)
        (aset string i (aref new j))
        (setq i (1+ i)
              j (1+ j))))))


;;; Old version.
;; ;; This could be more efficient with while loops instead of recursion.  Maybe.
;;
;; ;; Returns a value; does not act by side effect.
;;
;; ;; Dies a horrible death if passed a very long string, which is why we use
;; ;; string-replace-regexp-2 instead.
;; (defun string-substitute-substring-general-case-1 (new old-regexp string)
;;   (if (string-match old-regexp string)
;;       (concat (substring string 0 (match-beginning 0))
;;            new
;;            (string-substitute-substring-general-case
;;             new old-regexp (substring string (match-end 0))))
;;     string))
;;
;; (defun string-substitute-substring-general-case (new old-regexp string)
;;   "Call `string-replace-regexp-2'.  Beware special meaning of \\!."
;;   (string-replace-regexp-2 string old-regexp new))
;;
;; ;; If much replacement is going to happen, this is more efficient.
;; ;; Original version from gaynor@brushfire.rutgers.edu (Silver).
;; (defun string-replace-regexp-2 (string regexp replacement)
;;   "Return the string resulting by replacing all of STRING's instances of REGEXP
;; with REPLACEMENT."
;;   (save-excursion
;;     (set-buffer (get-buffer-create " *Temporary*"))
;;     (buffer-disable-undo (current-buffer))
;;     (erase-buffer)
;;     (insert string)
;;     (goto-char (point-min))
;;     (while (re-search-forward regexp nil t)
;;       (replace-match replacement))
;;     (buffer-string)
;;     ))

(defun string-replace-regexps (string regexps replacements)
  (save-excursion
    (set-buffer (get-buffer-create " *Temporary*"))
    (erase-buffer)
    (save-excursion (insert string))
    (buffer-replace-regexps regexps replacements)
    (buffer-string)))

(defun buffer-replace-regexps (regexps replacements)
  (while regexps
    (let ((regexp (car regexps))
          (replacement (car replacements)))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match replacement))
      (setq regexps (cdr regexps)
            replacements (cdr replacements)))))

(defun join (strings separator)
  (let ((with-delims))
    (while strings
      (setq with-delims (cons separator (cons (car strings) with-delims))
            strings (cdr strings)))
    (apply #'concat (nreverse (cdr with-delims)))))
;;; All these should return nil (rather than signalling an error).
;; (assert (equal (join '("foo" "bar") " ") "foo bar"))
;; (assert (equal (join '("foo" "bar" "baz") " ") "foo bar baz"))
;; (assert (equal (join '("foo") " ") "foo"))
;; (assert (equal (join '("foo" "bar" "baz") "") "foobarbaz"))
;; (assert (equal (join '() " ") ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversion
;;;

(defun string->integer-default (string &optional default)
  "If STRING represents an integer, return it; otherwise return DEFAULT."
  (let ((result (condition-case nil
                    (car (read-from-string string))
                  (error nil))))
    (if (integerp result)
        result
      default)))
(defalias 'string->number-default 'string->integer-default)

(defun string->integer-or-nil (string)
  (string->integer-default string nil))
(defalias 'string->number-or-nil 'string->integer-or-nil)

(defun string->integer (string)
  "Return the integer represented by STRING, or err.
See also `string->integer-default'."
  (or (string->integer-or-nil string)
      (error "string->integer:  `%s' doesn't look like an integer." string)))
(defalias 'string->number 'string->integer)

;; Backward compatibility
(defalias 'string->integer-no-check 'string->integer)
(defalias 'string->number-no-check 'string->integer-no-check)

;; The conversions in the other direction are already written.
(defalias 'integer->string 'int-to-string)
(defalias 'number->string 'int-to-string)

;; Number or nil
(defun number-or-nil->string (number)
  (if (numberp number)
      (number-to-string number)
    ""))
(make-obsolete 'integer-or-nil->string 'number-or-nil->string)

(defun string-or-nil->number-or-nil (string-or-nil)
  (and string-or-nil
       (string->number-or-nil string-or-nil)))
(make-obsolete 'string-or-nil->integer-or-nil 'string-or-nil->number-or-nil)

(defun string->boolean (string)
  (not (string= string "nil")))

;; ;; This is unacceptable because of version 18 bug:  can't have a marker at 0.
;; (defun string-is-integer-p-save-match (string)
;;   "Return t if string looks like the representation of an integer, nil otherwise."
;;   (let ((md (match-data)))
;;     (prog1
;;      (string-match "^[       ]*-?[0-9]+" string)
;;       (store-match-data md))))
;; (fset 'string-is-number-p-save-match
;;       (symbol-function 'string-is-integer-p-save-match))
;;
;; (defun string->integer-save-match (integer-string)
;;   (if (string-is-integer-p-save-match integer-string)
;;       (car (read-from-string integer-string))
;;     (error "`%s' doesn't look like an integer." integer-string)))
;; (defun string->number-save-match (symbol-function 'string->integer-save-match))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Atoms (symbols)
;;;

(defun symbol-append (&rest symbols)
  (intern (apply (function concat)
                 (mapcar (function symbol-name)
                         symbols))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
;;;

(defun flatten (list)
  "Return a list of all of the elements of LIST's elements.
For instance, ((a b) (c) (d (e f) g))) => (a b c d (e f) g)."
  (apply (function append) list))

(defun last-element (list)
  "Return last element of LIST."
  (car (last list)))

;; Which should come first, N or LIST?
(defun firstn (n list)
  "Return a copy of the first N elements of LIST."
  (let ((result '()))
    (while (and list (> n 0))
      (setq result (cons (car list) result)
            n (1- n)
            list (cdr list)))
    (nreverse result)))

(defun string-assoc-ci (elt list)
  "Return non-nil if ELT is the car of an element of LIST.  Comparison is done
with `string-equal', and case is ignored.
The value is actually the element of LIST whose car is (`string-equal' to) ELT."
  (setq elt (downcase elt))
  (let (result)
    (while list
      (if (string-equal elt (downcase (cdr (car list))))
          (setq result (car list)
                list nil)
        (setq list (cdr list))))
    result))


;;; Find patterns in lists.

;; Could have an optional argument which returns non-nil only if partial = 0.
(defun find-pattern (list)
  "Check for a repeated pattern in LIST.
Return a list of (length repeatcount partial) if one is found; nil otherwise.
The pattern is length elements long and occurs fully repeatcount times;
after that, its first partial elements appear.  The length of LIST is
\(+ (* length repeatcount) partial).  Tests are done with `equal'."
  (let* ((len (length list))
         (pattern-len 1)
         (max-pattern-len (/ len 2))
         result)
    (while (<= pattern-len max-pattern-len)
      (if (repeated-pattern-p pattern-len list)
          (setq result (list pattern-len (/ len pattern-len) (% len pattern-len))
                pattern-len len)
        (setq pattern-len (+ pattern-len 1))))
    result))

;; It is *not* necessary to check the last (- (length list) pattern-length)
;; elements against the first ones; because of transitivity, they will
;; match if this returns t.
(defun repeated-pattern-p (pattern-length list)
  (let ((leader (nthcdr pattern-length list))
        (result t))
    (while leader
      (if (not (equal (car leader) (car list)))
          (setq result nil
                leader nil)
        (setq leader (cdr leader)
              list (cdr list))))
    result))

(defun random-nth (list)
  "Return a random element of LIST."
  (nth (% (abs (random)) (length list)) list))


(defun swap (LIST el1 el2)
  "In LIST, swap elements at indices EL1 and EL2 in place."
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))

(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1))))
             (swap LIST i j)))
  LIST)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;

;; was funcall-maybe
(defsubst funcall-or-arg (fun &rest args)
  "If FUN is non-nil, apply it to ARGS.  Otherwise return second argument,
which would have been the first argument to which FUN was applied.
FUN should be a funcallable object or nil.  Compare to `funcall-or-nil'."
  (if fun
      (apply fun args)
    (if args
        (car args))))
(put 'funcall-or-arg 'edebug-form-spec '(function &rest form))

;; was funcall-maybe-default
;; Perhaps I should change the order of arguments.
(defsubst funcall-or-default (default fun &rest args)
  "If FUN is non-nil, apply it to ARGS.  Otherwise return DEFAULT.
FUN should be a funcallable object or nil."
  (if fun
      (apply fun args)
    default))
(put 'funcall-or-default 'edebug-form-spec '(form function &rest form))

;; was maybe-funcall
(defmacro funcall-or-nil (fun &rest args)
  "If FUN is non-nil, apply it to ARGS.  Otherwise return nil.
FUN should be a funcallable object or nil.  Compare to `funcall-maybe'."
  `(funcall-maybe-default nil ,fun ,@args))
(put 'funcall-or-nil 'edebug-form-spec '(function &rest form))

;; Obviously this could be (easily) generalized to take a list of integers
;; and to try all of those numbers of arguments; but why would I want that?
(defmacro vararg-call (func noargs1 noargs2 &rest args)
  "Apply FUNC to NOARGS1 (an integer), then (if that fails), to NOARGS2
of the ARGS.  -1 means all arguments.  This macro lets you deal with functions
expecting different numbers of arguments in a uniform way.  Since this is a
macro, don't supply something of the form (function foo) as its first argument;
just supply foo itself."
  (let ((noargs (length args))
        nocommon-args
        common-vars
        common-bindings
        thisvar
        (thisargno 0))

    (if (< noargs1 0) (setq noargs1 (- noargs)))
    (if (< noargs2 0) (setq noargs2 (- noargs)))
    (if (not (= (max noargs1 noargs2) noargs))
        (progn
          (byte-compile-warn "`%s' was vararg-called with a maximum of %d arguments, but you supplied %d."
                           func (max noargs1 noargs2) noargs)
          (setq args (firstn (max noargs1 noargs2) args))))
    (setq nocommon-args (min noargs1 noargs2))
    (if (= noargs1 noargs2)
        ;; aka `(,func ,@args)
        (cons func args)
      (while (< thisargno nocommon-args)
        (setq thisargno (1+ thisargno)
              thisvar (make-symbol (concat "vararg-common-"
                                           (int-to-string thisargno)))
              common-vars (cons thisvar common-vars)
              common-bindings (cons (list thisvar (car args)) common-bindings)
              args (cdr args)))
      (setq common-vars (nreverse common-vars)
            common-bindings (nreverse common-bindings))
      `(let (, common-bindings)
           (condition-case err
               ;; Try calling it with first number of arguments.
               (,func (,@ common-vars)
                (,@ (if (< nocommon-args noargs1) args)))
             (wrong-number-of-arguments
              ;; Call it with second number of arguments.
              (,func (,@ common-vars)
               (,@ (if (< nocommon-args noargs2) args))))
             (error
              ;; Otherwise resignal; "while t" makes this work under the
              ;; debugger (see, eg, the code for the "error" function).
              (while t
                (signal (car err) (cdr err)))))))))

;; Test cases:
;; (macroexpand '(vararg-call foo 3 1 bar baz bum))
;; (macroexpand '(vararg-call foo 3 5 bar baz bum quux quux2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
;;;

(defconst writability-mask 146
  "Mask for `set-file-modes'; indicates writable by user, group, and others.")
(defconst group-other-writability-mask 18
  "Mask for `set-file-modes'; indicates writable by group and others.")
(defconst readability-mask 292
  "Mask for `set-file-modes'; indicates readable by user, group, and others.")

(defun check-file-readable (filename &optional description)
  (if (not (file-readable-p filename))
      (error "Can't read %s `%s'." (or description "file") filename)))
;; Problem in Emacs 19.34
;; (proclaim-inline check-file-readable)

;; Syntactic sugar.
(defun if-file-exists-p (filename)
  "Return FILENAME if the file exists, nil otherwise."
  (if (file-exists-p filename)
      filename))
(defun if-file-readable-p (filename)
  "Return FILENAME if the file is readable, nil otherwise."
  (if (file-readable-p filename)
      filename))

(defun file-contents (filename)
  "Return the contents of FILENAME as a string."
  (with-current-buffer (get-buffer-create " file contents")
    (erase-buffer)
    (insert-file-contents filename)
    (prog1
        (buffer-substring (point-min) (point-max))
      (erase-buffer))))

(defun locate-file-with-extensions (filename extensions)
  "Return the name of a readable file starting with FILENAME
or FILENAME's basename and ending with a string in EXTENSIONS, which is a list.
EXTENSIONS may be nil, in which case FILENAME is searched for as is."
  (if extensions
      (let (result)
        (while (and extensions (not result))
          (setq result (or (if-file-readable-p (concat filename (car extensions)))
                           (if-file-readable-p (concat (file-name-sans-extension
                                                        filename)
                                                       (car extensions))))
                extensions (cdr extensions)))
        result)
    (if-file-readable-p filename)))

(defun locate-file-with-extensions-on-path (filename extensions path)
  "Return the name of a readable file starting with FILENAME
or FILENAME's basename and ending with a string in EXTENSIONS, which is a list.
PATH is a list of strings representing directories to be searched in
order after the current one; they may be relative directories.
Nil means the current directory."
  (or (locate-file-with-extensions filename extensions)
      (let ((filename-directory (file-name-directory filename))
            (filename-nondirectory (file-name-nondirectory filename))
            result candidate-directory)
        (while (and path (not result))
          (setq candidate-directory (if (car path)
                                        (file-name-as-directory (car path))
                                      default-directory)
                path (cdr path)
                result (locate-file-with-extensions
                        ;; This check is so we return something reasonable,
                        ;; not because the code requires the simpler form.
                        (if (file-name-absolute-p candidate-directory)
                            (concat candidate-directory filename-nondirectory)
                          ;; This probably only works on Unix.
                          (concat filename-directory candidate-directory
                                  filename-nondirectory))
                        extensions)))
        result)))

(defun locate-file-on-path (filename path)
  "Return the full path of a file named FILENAME located
in the current directory or on PATH, which is a list of directories (strings)
or nil for the current directory."
  (locate-file-with-extensions-on-path filename nil path))


(defun load-file-maybe (file)
  "Load the file FILE of Lisp code, or do nothing if FILE is nil."
  (if file
      (load-file file)))

(defun delete-file-maybe (file)
  "Delete FILE if it exists; do nothing otherwise."
 (if (and file (file-exists-p file))
     (delete-file file)))

;; ;; Isn't this built into Emacs 19 somewhere?
;; (defun same-file-p (file1 file2)
;;   "Return t if FILE1 and FILE2 are names for the same file."
;;   (or
;;    ;; Resolve all symbolic links
;;    (equal (file-truename file1) (file-truename file2))
;;    ;; Works for hard links.  If neither file exists, attributes are nil
;;    ;; and so trivially equal.
;;    (and (file-exists-p file1) (file-exists-p file2)
;;      (equal (file-attributes file1)
;;             (file-attributes file2)))))


;; Joe Wells <jbw@cs.bu.edu> points out some unpleasant interactions of the
;; following procedure with automounters:
;; If you resolve pathnames to contain no symbolic links, i.e. each component
;; of the path names a hard link, and the pathname points into an automounted
;; area, then the automounter may unmount the file system while you are
;; working with it.  It will only remount the file system if you refer to it
;; through the special automounter symbolic link.  The solution is to remove
;; all symbolic links but the special ones that are used by the automounter,
;; but this is non-trivial to do properly.  Also, if somehow Emacs were fed a
;; non-symbolic-link-containing path to an automounted file, the path should
;; be altered to contain the special automounter symbolic link.  This is
;; really hard.

;; Version by David Jones <dmjones@theory.lcs.mit.edu>
;; It will probably die if you hit a directory that you cannot read.
(defun journal-resolve-symlink (filename)
  "Return a full pathname for FILE that contains no symbolic links.
This gets in an infinite loop if FILE points into a circular list of symlinks.
Does not handle hard links."
  (and (stringp filename)
       (file-readable-p filename)
       (let ((components (journal-split-path (expand-file-name filename)))
             (result ""))
         (while components
           (setq result (expand-file-name (car components) result))
           (setq components (cdr components))
           (if (file-symlink-p result)
               (setq result (journal-resolve-symlink
                             (expand-file-name (car (file-attributes result))
                                               (file-name-directory result))))))
         result)))

(defun journal-split-path (string)
  (journal-split-string string "/"))

(defun journal-split-string (string pattern &optional regexp)
  (or regexp (setq pattern (regexp-quote pattern)))
  (let (result)
    (while (string-match pattern string)
      (setq result
            (append result (list (substring string 0 (match-beginning 0)))))
      (setq string (substring string (match-end 0))))
    (append result (list string))))

;; Use built-in substitute-in-file-name for "environment-var-expand-file-name".


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Messages
;;;

(defvar use-electric-help-p nil
  "*Non-nil if Emacs programs should use electric help where possible.
Don't set this to a non-nil value unless the ehelp package is available.")

;; Regrettably, this produces big code by including its argument twice.
(defmacro with-electric-help-maybe (&rest body)
  "Similar to `with-electric-help' if `use-electric-help-p' is non-nil;
otherwise like `with-output-to-temp-buffer' with the \"*Help*\" buffer.
Ehelp is loaded if necessary.
BODY is not a thunk (a function of no arguments) but simply a set of forms."
  `(if use-electric-help-p
         (progn
           (require 'ehelp)
           (with-electric-help
            (function (lambda ()
                        ,@body))))
       (with-output-to-temp-buffer "*Help*"
         ,@body)))

;; Originally by Joe Wells <jbw@cs.bu.edu>
(defun best-fit-message (text &optional buffer)
  "Show TEXT in echo area if it fits or in optional BUFFER (default *Message*)."
  (or buffer (setq buffer "*Message*"))
  (save-excursion
    (set-buffer (get-buffer-create " temp printing buffer"))
    (erase-buffer)
    (buffer-disable-undo (current-buffer))
    (insert text)
    (delete-region (point)
                   (progn
                     (skip-chars-backward " \t\n")
                     (point)))
    (cond ((and (< (current-column) (frame-width))
                (progn
                  (beginning-of-line 1)
                  (bobp)))
           ;; This can't be just buffer, even though that's non-nil,
           ;; because it might not be an existing buffer.
           (delete-windows-on (get-buffer-create buffer))
           (message "%s" (buffer-substring (point-min) (point-max))))
          (t
           (with-electric-help-maybe
            (princ text))))))

;; Originally by Roland B Roberts <ROBERTS@hahn.nsrl.rochester.edu>
(defmacro silently (&rest body)
  "Evaluate BODY with no messages to the minibuffer.
This will not disable any messages from built-in C subroutines."
  `(let ((f (symbol-function 'message)))
       (unwind-protect
           (progn (fset 'message (symbol-function 'ignore))
                  ,@body)
         (fset 'message f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor movement
;;;

;; Written so as to avoid the use of backquote,
;; which invokes a (byte-compiler?) bug.
(defmacro point-after (&rest commands)
  "Return the value of point after executing the COMMANDS.  Doesn't move point."
  (cons 'save-excursion
        (append
         commands
         '((point)))))
(put 'point-after 'edebug-form-spec '(&rest form))

(defun forward-line-wrapping (arg)
  "Like forward-line, but wrap around to the beginning of the buffer if
it encounters the end."
  (interactive "p")
  (let ((to-go (forward-line arg)))
    (cond ((or (plusp to-go) (not (bolp)))
           (goto-char (point-min))
           (forward-line-wrapping to-go))
          ((minusp to-go)
           (goto-char (point-max))
           (forward-line-wrapping (1+ to-go))))))

;; Renamed from current-line to avoid conflict with an example function in
;; the Emacs Lisp manual.
(defun current-buffer-line ()
  "Return the line number of the line containing point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text properties
;;;

(defun replace-text-property (start end prop old-value new-value &optional object)
  "Change a property of text from START to END.
Wherever PROP has OLD-VALUE, set it to NEW-VALUE instead.
If NEW-VALUE is nil, the property is removed instead.
Optional fifth argument OBJECT is the string or buffer containing the text."
  (let (prev-value next-change)
    (while (< start end)
      (setq prev-value (get-text-property start prop object))
      (setq next-change (next-single-property-change start prop object))
      (if (not next-change)
          (setq next-change end)
        (setq next-change (min next-change end)))
      (if (eq prev-value old-value)
          (if new-value
              (put-text-property start next-change prop new-value object)
            (remove-text-properties start next-change '(prop 'ignore) object)))
      (setq start next-change))))

;; This could easily be made to use a real stack.
(defun push-text-property (start end prop value &optional object)
  "Change a property of text from START to END:  set PROP to VALUE.
The old property can be retrieved via `pop-text-property'.
Optional fifth argument OBJECT is the string or buffer containing the text.
Multiple uses of `push-text-property' without an intervening
`pop-text-property' will not work; this does not actually use a stack."
  (let ((pushed-prop (intern (concat "pushed-" (symbol-name prop))))
        prev-value next-change)
    (while (< start end)
      (setq prev-value (get-text-property start prop object))
      (setq next-change (next-single-property-change start prop object))
      (if (not next-change)
          (setq next-change end)
        (setq next-change (min next-change end)))
      (put-text-property start next-change pushed-prop prev-value object)
      (put-text-property start next-change prop value object)
      (setq start next-change))))

(defun pop-text-property (start end prop &optional object)
  "Change a property of text from START to END:
restore a previous value of PROP which was modified via `pop-text-property'.
Optional fourth argument OBJECT is the string or buffer containing the text."
  (let ((orig-start start)
        (pushed-prop (intern (concat "pushed-" (symbol-name prop))))
        pushed-value next-change)
    (while (< start end)
      (setq pushed-value (get-text-property start pushed-prop object))
      (setq next-change (next-single-property-change start prop object))
      (if (not next-change)
          (setq next-change end)
        (setq next-change (min next-change end)))
      (if pushed-value
          (put-text-property start next-change prop pushed-value object)
        (remove-text-properties start next-change (list prop 'ignore) object))
      (setq start next-change))
  (remove-text-properties orig-start end (list pushed-prop 'ignore) object)))

(defun add-list-text-properties (pos prop new-elts &optional object)
  "Add, at position POS, to the value of the PROP text-proprty.
That value should be a list; add each of NEW-ELTS to it.
Optional OBJECT holds the text.
The old property is returned."
  (let* ((old-list (get-text-property pos prop object))
         (new-list old-list))
    (if (not (eq old-list t))
        (progn
          (while new-elts
            (if (not (or (eq old-list t)
                         (memq prop old-list)))
                (setq new-list (cons (car new-elts) new-list)))
            (setq new-elts (cdr new-elts)))
          (put-text-property pos (1+ pos) prop new-list object)))
    old-list))

(defun remove-list-text-properties (pos prop old-elts &optional object)
  "Reduce, at position POS, the value of the PROP text-proprty.
That value should be a list; remove each of OLD-ELTS from it.
Optional OBJECT holds the text.
The old property is returned."
  (let* ((old-list (get-text-property pos prop object))
         (new-list (if (eq t old-list)
                       (error "get all properties")
                     (copy-sequence old-list))))
    (while old-elts
      (setq new-list (delq (car old-elts) new-list)
            old-elts (cdr old-elts)))
    (put-text-property pos (1+ pos) prop new-list object)
    old-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
;;;

;;; Use built-in `with-current-buffer' (as of at least Emacs 20.7) rather
;;; than writing my own `in-buffer'.
;; ;;; This macro duplicates BODY.  This should be fixed.
;; ;; This version, which works when body moves point in a buffer displayed in
;; ;; a window other than the selected window, is from Joe Wells
;; ;; <jbw@cs.bu.edu>.  (If Lisp code moves point in a buffer displayed in a
;; ;; window other than the selected window, Emacs kindly restores point in
;; ;; the buffer to its window's version of point.)
;; (defmacro in-buffer (buffer &rest body)
;;   "Execute, in BUFFER, forms of BODY."
;;   ;; Need get-buffer-create because BUFFER might be a string.
;;   `(let ((target-buffer (get-buffer-create ,buffer))
;;         (this-buffer (current-buffer)))
;;        (if (eq target-buffer this-buffer)
;;         (progn
;;           ,@body)
;;       ;; Can't use save-excursion here because we only want to save the
;;       ;; current buffer, not its value for point.
;;       ;; (The Emacs Lisp manual, section "Introduction to Buffer-Local
;;       ;; Variables", says to use save-excursion for sanity, however.)
;;       (unwind-protect
;;           (progn
;;             (set-buffer target-buffer)
;;             (let* ((target-window (get-buffer-window target-buffer))
;;                    (track-window-point-p
;;                     (and (not (eq target-window (selected-window)))
;;                          (eq (window-point target-window) (point)))))
;;               (prog1
;;                   (progn
;;                     ,@body)
;;                 (if (and track-window-point-p
;;                          ;; *** Do I need this check?
;;                          (eq (current-buffer) target-buffer)
;;                          (eq target-window (get-buffer-window target-buffer))
;;                          (not (eq target-window (selected-window))))
;;                     (set-window-point target-window (point))))))
;;         (if (non-killed-buffer-p this-buffer)
;;             (set-buffer this-buffer))))))
;; (put 'in-buffer 'lisp-indent-hook 1)
;; (put 'in-buffer 'edebug-form-spec '(&rest form))

;;; [In what way did this screw up point?]
;;; This did more than I needed, and it screwed up point.
;; (defmacro in-buffer (buffer &rest body)
;;   "Executes, in BUFFER, forms of BODY."
;;   `(save-window-excursion
;;        (set-buffer ,buffer)
;;        ,@body))

;; ;; Why not just use save-excursion for this?
;; (defmacro in-buffer-simple (buffer &rest body)
;;   "Execute, in BUFFER, forms of BODY.
;; BODY shouldn't move point in a buffer displayed in a non-selected window."
;;   `(save-excursion
;;        (set-buffer ,buffer)
;;        ,@body))
;; (put 'in-buffer-simple 'lisp-indent-hook 1)

;; (defmacro in-buffer-simple (buffer &rest body)
;;   "Execute, in BUFFER, forms of BODY.
;; BODY shouldn't move point in a buffer displayed in a non-selected window."
;;   `(let ((this-buffer (current-buffer)))
;;        (set-buffer ,buffer)
;;        (unwind-protect
;;         (progn ,@body)
;;       (set-buffer this-buffer))))


;; Similar tricks can be done with syntax-table and current-local-map.
;; Adapted from code by Joe Wells.
(defun copy-buffer-local-variables (buffer)
  "Copy the values of all of BUFFER's local variables into the current buffer."
  (let ((blv (with-current-buffer buffer (buffer-local-variables)))
        pair symbol)
    (while (consp blv)
      (setq pair (car blv))
      (setq symbol (car pair))
      (if (memq symbol '(0 buffer-undo-list)) ; 18.57 brain-damage!!!!!
          nil
        (progn
          (if (not (and symbol (symbolp symbol))) (error "impossible"))
          (make-local-variable symbol)
          (set symbol (cdr pair))))
      (setq blv (cdr blv)))))

(defun non-killed-buffer-p (object)
  "Non-nil if OBJECT is a non-killed buffer."
  (and (bufferp object)
       (buffer-name object)))

(defun buffer-flush-undo-history (&optional buffer)
  "Make BUFFER keep undo info but have an empty undo history.
Default is current buffer."
  (buffer-disable-undo (or buffer (current-buffer)))
  (buffer-enable-undo buffer))

(defun longest-line-in-region (beg end)
  "Return the length of the widest line in the region.
Actually return the number of characters in its on-screen representation.
When called non-interactively, requires two arguments, the region's beginning and end."
  (interactive "r")
  (save-excursion
    (goto-char (min beg end))
    (if (< end beg) (setq end beg))
    (end-of-line 1)
    (let ((result (current-column)))
      (while (< (point) end)
        (end-of-line 2)
        (if (> (current-column) result)
            (setq result (current-column))))
      result)))

(defun line-lengths-histogram (beg end)
  "Return a histogram of the lengths of lines in the region.
The result is a hash table of the form ((len . count) ...)."
  (let ((table (make-hash-table)))
    (save-excursion
      (goto-char (min beg end))
      (if (< end beg) (setq end beg))
      (end-of-line 1)
      (let ((len (current-column)))
        (puthash len (1+ (gethash len table 0)) table))
      (while (< (point) end)
        (end-of-line 2)
        (let ((len (current-column)))
          (puthash len (1+ (gethash len table 0)) table))))
    table))

;; Modified from insert-buffer.
(defun buffer-contents (buffer)
  "Return the entire contents of BUFFER as a string.
Differs from `buffer-string', which returns only the visible part if
narrowing is in effect."
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (with-current-buffer buffer
    (buffer-substring (point-min) (point-max))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows
;;;

(defmacro in-window (window &rest body)
  "Executes, in WINDOW, forms of BODY.
This is more useful than `with-curent-buffer' for window manipulation,
as by `scroll-up'."
  `(let ((this-window (selected-window)))
       (unwind-protect
           (progn
             (select-window ,window)
             ,@body)
         (select-window this-window))))
(put 'in-window 'lisp-indent-hook 1)
(put 'in-window 'edebug-form-spec '(&rest form))


;; These respect narrowing.
(defun bob-visible-p ()
  (= (point-min) (window-start)))

(defun eob-visible-p ()
  (= (point-max) (window-end)))

;;; Old, less efficient implementation.
;; ;; I'm not sure whether this works if the last line is wrapped.
;; ;; Likewise for bob-visible-p and wrapped first line (is that possible?).
;; (defun eob-visible-p ()
;;   (save-excursion
;;     (let ((ht (window-height (selected-window))))
;;       (move-to-window-line (- ht 2))
;;       (end-of-line)
;;       (eobp))))
;;
;; (defun bob-visible-p ()
;;   (save-excursion
;;     (move-to-window-line 0)
;;     (beginning-of-line)
;;     (bobp)))

;; Return number of screen lines between START and END; return a negative
;; number if END precedes START.
(defun count-screen-lines-signed (start end)
  (let ((lines (count-screen-lines start end)))
    (if (< start end)
        lines
      (- lines))))

(defun count-lines-signed (start end)
  (let ((lines (count-lines start end)))
    (if (< start end)
        lines
      (- lines))))

(make-obsolete 'buffer-visible-p 'get-buffer-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keys
;;;

;;; Semi-transparent way to meta-ify a key.  This still isn't quite what we
;;; want.  The problem is that some patches to permit 8-bit character sets
;;; to be displayed change meta sequences to escape sequences, even without
;;; changing meta-flag to nil.

;;; This does't work, because meta-flag is t in the implementations I'm
;;; targetting.  (Besides the syntax being wrong for a macro.)
;; (defmacro meta-prefix-ify (keys)
;;   "Set meta bit of first character of KEYS or prepend  meta-prefix-char,
;; depending on  meta-flag."
;;   (if meta-flag
;;       (let ((first-char (aref keys 0)))
;;      (if (zerop (logand meta-bit first-char))
;;          (progn
;;            (aset keys 0 (logior meta-bit first-char))
;;            keys)
;;        (concat (list meta-prefix-char) keys)))
;;     (concat (list meta-prefix-char) keys)))

(defmacro meta-prefix-ify (keys)
  "Prepend  meta-prefix-char  to KEYS, a string."
  `(concat (list meta-prefix-char) ,keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching and matching
;;;

;;; No-error versions

(defmacro re-search-forward-no-err (regexp &optional limit)
  "Expands to (re-search-forward ,REGEXP ,LIMIT t)."
  `(re-search-forward ,regexp ,limit t))

(defmacro re-search-backward-no-err (regexp &optional limit)
  "Expands to (re-search-backward ,REGEXP ,LIMIT t)."
  `(re-search-backward ,regexp ,limit t))

(defmacro search-forward-no-err (string &optional limit)
  "Expands to (search-forward ,STRING ,LIMIT t)."
  `(search-forward ,string ,limit t))

(defmacro search-backward-no-err (string &optional limit)
  "Expands to (search-backward ,STRING ,LIMIT t)."
  `(search-backward ,string ,limit t))

;;; Other functions

(defmacro re-search-forward-maybe (regexp &rest args)
  "Like re-search-forward, but if regexp is nil, then return nil."
  `(let ((re ,regexp))
       (if re
           (re-search-forward re ,@args))))
(put 're-search-forward-maybe 'edebug-form-spec '(&rest form))

(defun regexps->regexp (&rest regexps)
  (mapconcat (function identity)
             regexps
             "\\|"))

(defmacro re-search-forward-many (&rest regexps)
  "Like `re-search-forward', but search for any of REGEXPS.
No optional arguments are permitted."
  `(re-search-forward (regexps->regexp ,@regexps)))

(defmacro re-search-forward-many-no-err (&rest regexps)
  "Like `re-search-forward-no-err', but search for any of REGEXPS.
No optional arguments are permitted."
  `(re-search-forward-no-err (regexps->regexp ,@regexps)))

(defmacro looking-at-string (string)
  "t if text after point is STRING."
  `(looking-at (regexp-quote ,string)))

;;; Skipping regexps

(defmacro skip-regexp-forward (regexp &optional match-no)
  "If point is at REGEXP, move past it and return point;
otherwise return nil.
Point is left at the end of match MATCH-NO if it is specified."
  `(if (looking-at ,regexp)
         (goto-char (match-end (or (, match-no) 0)))))

(defmacro skip-regexp-forward-maybe (regexp &optional match-no)
  "Return nil if REGEXP is nil; otherwise like skip-regexp-forward."
  `(let ((re ,regexp))
       (if re
           (skip-regexp-forward re))))

(defmacro skip-regexp-backward (regexp &optional match-no)
  "If point is after REGEXP, move past it and return point;
otherwise return nil."
  `(let ((here (point)))
       (if (re-search-backward ,regexp nil t)
           (if (= here (match-end 0))
               t
             (progn
               (goto-char here)
               nil)))))

(defmacro skip-regexp-backward-maybe (regexp &optional match-no)
  "Return nil if REGEXP is nil; otherwise like skip-regexp-backward."
  `(let ((re ,regexp))
       (if re
           (skip-regexp-backward re))))

;;; Skipping strings

;; Is this more efficient than regexp-quote and skip-regexp-forward
;; (which equals looking-at-string and goto-char)?
(defmacro skip-string-forward (string)
  "If point is at STRING, move past it and return non-nil;
otherwise return nil."
  `(let ((s ,string))
       (if (empty-string-p s)
           t
         (if (search-forward s (+ (point) (length s)) t)
             (goto-char (match-end 0))))))

(defmacro skip-string-backward (string)
  "If point is after STRING, move back past it and return t;
otherwise return nil."
  `(let ((s ,string))
       (if (empty-string-p s)
           t
         (search-backward s (- (point) (length s)) t))))

;;; Extracting matches

(defun match-string-maybe (n &optional source)
  "Like match-string, but return nil if there was no match for parenthesis N."
  (and (match-beginning n)
       (match-string n source)))

;; Mostly a debugging function.
(defun show-match-data (&optional source)
  (interactive)
  (list (match-string-maybe 0 source)
        (match-string-maybe 1 source)
        (match-string-maybe 2 source)
        (match-string-maybe 3 source)
        (match-string-maybe 4 source)
        (match-string-maybe 5 source)
        (match-string-maybe 6 source)
        (match-string-maybe 7 source)
        (match-string-maybe 8 source)
        (match-string-maybe 9 source)))

;;; How-many

;; Lifted from how-many (aka count-matches), but leaner and meaner
(defun how-many-regexp (regexp)
  "Return number of matches for REGEXP following point.
REGEXP should not match the empty string."
  (let ((count 0))
    (save-excursion
     (while (re-search-forward regexp nil t)
       (setq count (1+ count))))
    count))
(fset 'count-matches-regexp 'how-many-regexp)

(defun how-many-string (string)
  "Return number of matches for STRING following point."
  (let ((count 0))
    (save-excursion
     (while (search-forward string nil t)
       (setq count (1+ count))))
    count))
(fset 'count-matches-string 'how-many-string)

(defun how-many-regexp-overlapping (regexp)
  "Return number of matches for REGEXP following point, including overlapping ones.
REGEXP should not match the empty string."
  (let ((count 0))
    (save-excursion
     (while (re-search-forward regexp nil t)
       (goto-char (1+ (match-beginning 0)))
       (setq count (1+ count))))
    count))
(fset 'count-matches-regexp-overlapping 'how-many-regexp-overlapping)

;; Instead of using match-beginning, I could compute the length of the
;; string and use (backward-char (1- string-length)).
(defun how-many-string-overlapping (string)
  "Return number of matches for STRING following point, including overlapping ones."
  (let ((count 0))
    (save-excursion
     (while (search-forward string nil t)
       (goto-char (1+ (match-beginning 0)))
       (setq count (1+ count))))
    count))
(fset 'count-matches-string-overlapping 'how-many-string-overlapping)

(defun how-many-in-string (regexp target)
  "Return number of matches for REGEXP in TARGET."
  (let ((count 0)
        (start -1))
    (while (setq start (string-match regexp target (1+ start)))
      (setq count (1+ count)))
    count))
;; Testing:  (how-many-in-string "(" "((()())()())")

(defun how-many-substring-overlapping (substring target)
  "Return number of matches for SUBSTRING in TARGET, including overlapping ones."
  (let ((ss-regexp (regexp-quote substring))
        (count 0)
        (start -1))
    (while (setq start (string-match ss-regexp target (1+ start)))
      (setq count (1+ count)))
    count))
(fset 'count-matches-substring-overlapping 'how-many-substring-overlapping)

;;; Find-char

(defun find-char (char string &optional count)
  "Look for CHAR in STRING; return first index in STRING whose element is CHAR.
If optional arg COUNT is specified, return the COUNTth occurrance."
  (if (not count)
      (setq count 1))
  (let ((index 0)
        (string-length (length string))
        (result nil))
    (while (and (< index string-length) (not result))
      (if (char-equal char (aref string index))
          (if (= count 1)
              (setq result index)
            (setq count (1- count))))
      (setq index (1+ index)))
    result))

(defun find-char-from-end (char string &optional count)
  "Look for CHAR in STRING; return last index in STRING whose element is CHAR.
If optional arg COUNT is specified, return the COUNTth occurrance from the end."
  (if (not count)
      (setq count 1))
  (let ((index (1- (length string)))
        (string-length )
        (result nil))
    (while (and (> index -1) (not result))
      (if (char-equal char (aref string index))
          (if (= count 1)
              (setq result index)
            (setq count (1- count))))
      (setq index (1- index)))
    result))

;;; Replace-regexp

(defsubst replace-regexp-noninteractive (regexp replacement &optional delimited)
  "Like `replace-regexp', but doesn't modify mark or the mark ring."
  (if delimited
      (setq regexp (concat "\\\\<" regexp "\\\\>")))
  (while (re-search-forward regexp nil t)
    (replace-match replacement)))
(make-obsolete 'replace-regexp 'replace-regexp-noninteractive)

(defun delete-all-matching-lines (regexp)
  (interactive "sFlush all lines (containing match for regexp): ")
  (save-excursion
    (goto-char (point-min))
    (delete-matching-lines regexp)))

(defun delete-all-non-matching-lines (regexp)
  (interactive "sKeep only lines (containing match for regexp): ")
  (save-excursion
    (goto-char (point-min))
    (delete-non-matching-lines regexp)))

(defun unused-char-in-buffer ()
  "Return a character not used in the current buffer, or nil.
This function attempts to return a character that can be displayed in a single
screen column."
  (save-excursion
    (let ((candidate ?\ )
          (result t))
      (while (eq result t)
        (goto-char (point-min))
        (if (not (search-forward (char-to-string candidate) nil t))
            (setq result candidate)
          (progn
            (setq candidate (% (1+ candidate) 256))
            (if (eq candidate ?\ )
                (setq result nil)))))
      result)))

(defun unused-char-in-string (string)
  "Return a character not used in STRING, or nil.
This function attempts to return a character that can be displayed in a single
screen column."
  (save-excursion
    (set-buffer (get-buffer-create " *Temporary*"))
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (insert string)
    (unused-char-in-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insertion
;;;

;; The interior delq is to catch some nils at compile time.
(defmacro insert-maybe (&rest args)
   "Like insert, but ignores any arguments which are nil."
   `(apply (function insert) (delq nil (, (cons 'list (delq nil args))))))
(put 'insert-maybe 'edebug-form-spec '(&rest form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer substitution
;;;

(defun buffer-substitute (substitutions backward check)
  "Make replacements in the current buffer according to SUBSTITUTIONS.
  SUBSTITUTIONS is list of pairs of strings; the cdr of each pair will be
substituted for the car, in order, unless optional argument BACKWARD is
non-nil, in which case the car is substituted for the cdr and the
substitutions are done in reverse order.
  If optional third argument CHECK is non-nil, the user is warned if any of
the substituted-in strings already appears in the buffer; such a situation
would make substitution, then unsubstitution, not yield a result identical
to the original buffer, since all instances of the substituted-in string
will be assumed on the reverse substitution to have been the result of
replacing a substituted-for string.
  Return nil if CHECK is nil or there were no ambiguities; otherwise
return a list of replacements creating ambiguity."

  (if backward
      (setq substitutions (mapcar (function (lambda (sub-cons)
                                     (cons (cdr sub-cons) (car sub-cons))))
                                  (reverse substitutions))))
    ;;; Much too tricky, and modified the argument besides.
    ;; (let ((subs (reverse substitutions))
    ;;      temp)
    ;;  (while subs
    ;;    (setq temp (caar subs))
    ;;    (setcar (car subs) (cdar subs))
    ;;    (setcdr (car subs) temp)
    ;;    (setq subs (cdr subs)))
    ;;  (setq substitutions (nreverse substitutions)))

  ;; (message "buffer-substitute:  substitutions = %s" substitutions)

  ;; Should do all checking before any substitutions are done.
  ;; Bad:
  ;;  * any to-string appears in text, unless it's an earlier from-string.
  ;;  * any to-string appears in previous to-string without intervening
  ;;    from-string.  (but then it's just stupidly inefficient)

  ;; Perhaps be able to override checks of the substitutions pairs.  Such
  ;; checks will be hairy anyway because we may create an ambiguity by
  ;; replacing part of a match such that the other part is still in the
  ;; buffer unchanged.  With one-character stuff this is obviously much
  ;; easier.
  ;; Perhaps do the checks by character...?

  ;; Don't want to do checks as we do the substitutions because that leaves
  ;; us in a bad state:  the work is partially done.  We want to let the
  ;; guy know before we start.

  ;; If, in the case of an ambiguity, we're just going to give up anyway,
  ;; then perhaps it isn't so bad to do the checks after part of the work
  ;; is done (except that the work already done would have been a waste of
  ;; time).  So maybe make the check of the pairs a preliminary one and do
  ;; the real check as we go.  But in some cases such checks won't be
  ;; necessary.

  ;; Perhaps if we want checks on the substitution strings themselves, then
  ;; do that separately beforehand and call this with check = nil.

  ;; And hey, searching for one instance of a string is pretty cheap, after
  ;; all.  And I don't expect to be calling this with a truly enormous list
  ;; of substitutions anyway.

  ;; I think I'm being too paranoid here.  In many cases I'm not even going
  ;; to call this with check = t.

  (let (from-string to-string ambiguity ambiguities)
    (while substitutions
      (setq from-string (caar substitutions)
            to-string (cdar substitutions))
      ;; (message "Substituting %s for %s." to-string from-string)
      (goto-char (point-min))

      (if (and check (search-forward to-string nil t))
          (progn
            (setq ambiguity (car substitutions))
            (goto-char (point-min))))

      (replace-string-noninteractive from-string to-string)

      ;; Don't complain if we didn't actually do any substitution.
      (if ambiguity
          (progn
            (if (not (= (point) (point-min)))
                (setq ambiguities (cons ambiguity ambiguities)))
            (setq ambiguity nil)))

      (setq substitutions (cdr substitutions)))
    ambiguities))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

;; Ought to have a way to set the default value, too.
(defmacro deflocalvar (&rest args)
  "Like defvar, but defines a buffer-local variable."
  `(progn
       (defvar ,@args)
       (make-variable-buffer-local (quote (, (car args))))))
(put 'deflocalvar 'edebug-form-spec '(&rest form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constant functions
;;;

(defun t-function (&rest args)
  t)

(defun nil-function (&rest args)
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Current date
;;;

(defun current-date ()
  "Return the current date, as a human-readable string."
  (let ((now (current-time-string)))
    (concat (substring now 4 8)         ; month
            (substring now 8 11)        ; day
            (substring now 20)          ; year
            )))
;; (current-date)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp programming
;;;

;;;
;;; Proclaim-inline
;;;

(defun check-proclaim-inline ()
  "Make sure all arguments to `proclaim-inline' are defined as functions.
Uses the current tags table."
  (interactive)
  (let (f fbegin)
    (tags-search "\(proclaim-inline ")
    (while t
      (setq fbegin (point))
      (forward-sexp)
      (setq f (intern (buffer-substring fbegin (point))))
      (if (not (fboundp f))
          (error "%s not fboundp." f))
      (if (looking-at "\)")
          (tags-loop-continue)
        (skip-chars-forward " \t\n")))))

;;;
;;; Documentation strings
;;;

(defconst non-quote-regexp
  "\\([^\"\\]\\|\\\\.\\|\n\\)*"
  "Regular expression matching strings not containing unbackquoted quotation marks.")

(defun for-all-docstrings (function)
  "Call FUNCTION once for each documentation string.
Its three arguments are the name being defined \(a string\) and the beginning
and end of the documentation string \(as buffer positions\)."
  (let (defname begin end)
    (tags-search "^\(def")
    (while t
      (setq begin nil)
      (cond ((skip-regexp-forward "un\\|macro\\|subst")
             (skip-regexp-forward "\\s *")
             (setq defname (point))
             (forward-sexp 1)
             (setq defname (buffer-substring defname (point)))
             (if (skip-regexp-forward " \(.*\)\n  \"")
                 (setq begin (point))))
            ((skip-regexp-forward "var\\|const\\|localvar")
             (skip-regexp-forward "\\s *")
             (setq defname (point))
             (forward-sexp 1)
             (setq defname (buffer-substring defname (point)))
             (if (not (looking-at "\)"))
                 (progn
                   (forward-sexp 1)     ; value
                   (if (skip-regexp-forward "[ \t\n]*\"")
                       (setq begin (point)))))))
      (if begin
          (progn
            (looking-at non-quote-regexp)
            (setq end (match-end 0))
            (funcall function defname begin end)))
      (tags-loop-continue))))

(defun princ-docstring-region (defname beg end)
  (princ "\n--- ")
  (princ (upcase defname))
  (princ "\n")
  (princ (buffer-substring beg end)))

;; (with-output-to-temp-buffer "*Docstrings*" (for-all-docstrings 'princ-docstring-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File local variables
;;;

(defun operate-on-local-variables (region-function)
  "Apply REGION-FUNCTION to the local-variables region of the buffer.
Return t if a local-variables region was found; REGION-FUNCTION should act
by side effect."
  (goto-char (point-max))
  (search-backward "\n\^L"
                   (max (- (point-max) 3000) (point-min)) 'move)
  (if (search-forward "Local Variables:" nil t)
      (progn
        (beginning-of-line 1)
        (funcall region-function (point) (point-max))
        t)))

(defun really-hack-local-variables ()
  "Call `hack-local-variables', ignoring variables that limit it."
  ;; Bah!  Hulk not impressed by puny attempts to thwart him!
  (let ((enable-local-eval t)
        (enable-local-variables t))
    (hack-local-variables)))


;; This page feed is to defeat local variables processing.


(provide 'util-mde)

;;; util-mde.el ends here
