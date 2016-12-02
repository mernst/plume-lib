;;; util-cl-mde.el
;;; Michael Ernst <mernst@csail.mit.edu>

;;; Implementations of
;;;  * Common Lisp functions not in cl.el
;;;  * Functions that use those
;;;  * A few basic cl.el functions, to forego loading the entire package

;; This file is intended to be lean, not to provide full Common Lisp
;; compatibility; efforts have been made to exclude the extraneous.
;; This file is self-contained; it does not require that you load cl.el
;; also.  An effort has been made, however, to make it compatible with
;; cl.el, so that either can be loaded after the other.
;; The following cl.el functions are identically redefined here, but made
;; inlineable:
;;   first second third fourth
;;   caar cadr cdar cddr
;;   plusp minusp oddp evenp abs
;; The following cl.el functions are compatibly redefined here:
;;   [none]
;; The following Emacs built-in functions are compatibly redefined here:
;;   sort


(provide 'util-clmde)
(provide 'util-cl-mde)

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug fixes
;;;

;; cl.el uses copy-vector but doesn't define it.
(defun copy-vector (vector)
  (copy-sequence vector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12. Numbers
;;;

;;; 5.2 Trigonometric and related functions

(defun signum (x)
  (cond ((plusp x)
         1)
        ((zerop x)
         0)
        ((minusp x)
         -1)
        (t
         (error "What kind of number is this in signum?"))))
;; ;; Actually correct implementation (which is probably too expensive) is:
;; (defun signum (x)
;;   (if (zerop x)
;;       x
;;     (/ x (abs x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14. Sequences
;;;

;;; 1. Simple sequence functions

;;; 2. Concatenating, mapping, and reducing sequences

;; This seems to trounce the cl.el definition; I assume that's intentional.

;; Which is more efficient:  calling this with first argument nil or
;; calling mapcar and throwing away the result?
(defun map-one-sequence (result-type function sequence)
  "Return a sequence of type RESULT-TYPE whose elements are the results of
applying FUNCTION to each element of SEQUENCE.  The length of the returned
sequence is the same as that of SEQUENCE, unless RESULT-TYPE is nil, in
which case nil is returned."
  (let ((index 0)
        (maxindex (length sequence))
        result-list)
    (if (not result-type)
        ;; If the sequence is a list, the "elt" calls may be expensive; is
        ;; it more efficient to do this or to call mapcar and throw away
        ;; the result?
        (progn
          (while (< index maxindex)
            (funcall function (elt sequence index))
            (setq index (1+ index)))
          nil)
      (progn
        (if (listp sequence)
            (setq result-list (mapcar function sequence))
          (progn
            (while (< index maxindex)
              (setq result-list
                    (cons (funcall function (elt sequence index)) result-list))
              (setq index (1+ index)))
            (setq result-list (nreverse result-list))))
        (cond ((eq 'list result-type)
               result-list)
              ((eq 'vector result-type)
               (vconcat result-list))
              ((eq 'string result-type)
               (concat result-list))
              (t
               (error "Unrecognized result-type %s" result-type)))))))

;;; 3. Modifying sequences

;;; Incompatible with cl.el's version, which accepts optional arguments.
;; ;; I should generalize this.
;; (defun substitute (newitem olditem sequence)
;;   "Substitute NEWITEM for OLDITEM in SEQUENCE.  Only works on strings for now."
;;   (if (and (stringp sequence) (characterp newitem) (characterp olditem))
;;       (string-substitute newitem olditem sequence)
;;     (error "substitute isn't that good yet.")))

(defmacro string-substitute (newchar oldchar string)
  "Substitute NEWCHAR for instances of OLDCHAR in STRING.
NEWCHAR and OLDCHAR are characters."
  `(string-substitute-opt ,newchar
                            (regexp-quote (char-to-string ,oldchar))
                            ,string))

;; Optimized version.  oldchar-regexp should only match one-character strings.
(defun string-substitute-opt (newchar oldchar-regexp string)
  (let ((i -1)
        (case-fold-search nil))
    (while (setq i (string-match oldchar-regexp string (1+ i)))
      (aset string i newchar))))


;;; 4.  Searching sequences for items

;;; Incompatible with cl.el's version, which accepts optional arguments.
;; ;; Actually could use cl.el's member-if for this.
;; (defun find-if (test sequence)
;;   "Return the first element satisfying TEST in SEQUENCE, or nil if none do."
;;   (let ((limit (length sequence))
;;      result
;;      (index 0))
;;     (while (< index limit)
;;       (if (funcall test (elt sequence index))
;;        (setq result (elt sequence index)
;;              index limit)
;;      (setq index (1+ index))))
;;     result))

(defun mdecl-position (item sequence)
  "Return the first index of ITEM in SEQUENCE, or nil; tests with `equal'."
  (let ((limit (length sequence))
        result
        (index 0))
    (while (< index limit)
      (if (equal item (elt sequence index))
          (setq result index
                index limit)
        (setq index (1+ index))))
    result))

;;; Incompatible with cl.el's version, which accepts optional arguments.
;; (defun count (item sequence)
;;   "Return the number of times that ITEM appears in SEQUENCE; test with `equal'."
;;   (let ((limit (length sequence))
;;      (result 0)
;;      (index 0))
;;     (while (< index limit)
;;       (if (equal item (elt sequence index))
;;        (setq result (1+ result)))
;;       (setq index (1+ index)))
;;     result))



;;; 5. Sorting and merging

;;; Both versions of this cause rmail-sort-by-date to err after (require
;;; 'util-clmde), even though it is sorting a list and the new code
;;; shouldn't even be invoked.  I don't understand why; for now, comment it
;;; out (and find out where I depend on this functionality...)  1/22/99.

;; (if (not (fboundp 'mdecl-old-sort))
;;     (fset 'mdecl-old-sort (symbol-function 'sort)))
;; (defun sort (sequence predicate)
;;   "Sort SEQUENCE, stably, comparing elements using PREDICATE.
;; Return the sorted sequence, which is modified by side effects.
;; PREDICATE is called with two elements of LIST, and should return T
;; if the first element is \"less\" than the second."
;;   (if (listp sequence)
;;       (mdecl-old-sort sequence predicate)
;;     (let ((sorted-seq-as-list (mdecl-old-sort (map-one-sequence
;;                                       'list (function identity) sequence)
;;                                      predicate))
;;        (index 0)
;;        (maxindex (length sequence)))
;;       (while (< index maxindex)
;;      (aset sequence index (car sorted-seq-as-list))
;;      (setq sorted-seq-as-list (cdr sorted-seq-as-list)))
;;       sequence)))

;; (defadvice sort (around any-sequence activate)
;;   "Permit sorting of any sequence, not just lists."
;;   (if (listp (ad-get-arg 0))
;;       ad-do-it
;;     (let* ((orig-seq (ad-get-arg 0))
;;         (orig-seq-as-list (map-one-sequence
;;                            'list (function identity) orig-seq))
;;         (result-as-list (progn
;;                           (ad-set-arg 0 orig-seq-as-list)
;;                           ad-do-it)))
;;       ;; set the elements of the original sequence
;;       (let ((index 0)
;;          (maxindex (length sequence)))
;;      (while (< index maxindex)
;;        (aset sequence index (car sorted-seq-as-list))
;;        (setq sorted-seq-as-list (cdr sorted-seq-as-list)))
;;      orig-seq))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15. Lists
;;;

;;; 2. Lists

(defun butlast (list &optional n)
  "Return a list with the same elements as LIST, excepting the last N elements.
N defaults to 1.  If LIST has fewer than N elements, NIL is returned."
  (let ((copied-elts (- (length list) (or n 1)))
        result)
    (while (and list (plusp copied-elts))
      (setq result (cons (car list) result)
            copied-elts (1- copied-elts)
            list (cdr list)))
    (nreverse result)))

;;; 5. Using lists as sets

;; Originally lifted from gnus.

;;; Incompatible with cl.el's version, which accepts optional arguments.
;; (defun set-difference (list1 list2)
;;   "Return an in-order list of elements of LIST1 that do not appear in LIST2."
;;   (let ((result (copy-sequence list1)))
;;     (while list2
;;       (setq result (delq (car list2) result))
;;       (setq list2 (cdr list2)))
;;     result
;;     ))

;;; Incompatible with cl.el's version, which accepts optional arguments.
;; (defun intersection (list1 list2)
;;   "Return a list of elements that appear in both LIST1 and LIST2."
;;   (let (result)
;;     (while list2
;;       (if (memq (car list2) list1)
;;        (setq result (cons (car list2) result)))
;;       (setq list2 (cdr list2)))
;;     result
;;     ))

(defun set-difference-equal (list1 list2)
  "Return an in-order list of elements of LIST1 that do not appear in LIST2.
Uses equal for the comparison."
  (let (result)
    (while list1
      (if (not (member (car list1) list2))
          (setq result (cons (car list1) result)))
      (setq list1 (cdr list1)))
    (nreverse result)
    ))

(defun intersection-equal (list1 list2)
  "Return a list of elements that appear in both LIST1 and LIST2.
Uses equal for the comparison."
  (let (result)
    (while list2
      (if (member (car list2) list1)
          (setq result (cons (car list2) result)))
      (setq list2 (cdr list2)))
    result
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 18. Strings
;;;

(defsubst characterp (char)
  "Return t if its argument is a character, nil otherwise."
  (and (integerp char)
       (not (minusp char))
       (< char 256)))

;; Return a regexp that matches zero or more characters in charseq, a
;; sequence of characters appropriate for inclusion in brackets in a regexp.
;; If some characters in charseq have special meaning (eg ^,-,[), they will
;; not necessarily be literally matched.
(defun chars->regexp (charseq)
  (if (listp charseq)
      (setq charseq (funcall (function concat) charseq)))
  (concat "[" charseq "]*"))

;;; These functions clobber the match-data.
;; Their comments about CHARSEQ should be more explicit.

;; defsubsts should be defined before they are used, to permit them to be
;; inlined.

(defsubst string-left-trim-regexp (regexp string)
  (if (string-match (concat "^" regexp) string)
      (substring string (match-end 0))
    string))

(defsubst string-left-trim (charseq string)
  "Return a substring, with characters in CHARSEQ removed from the
beginning, of STRING."
  (string-left-trim-regexp (chars->regexp charseq) string))

(defsubst string-right-trim-regexp (regexp string)
  (if (string-match (concat regexp "$") string)
      (substring string 0 (match-beginning 0))
    string))

(defsubst string-right-trim (charseq string)
  "Return a substring, with characters in CHARSEQ removed from the
end, of STRING."
  (string-right-trim-regexp (chars->regexp charseq) string))

(defsubst string-trim (charseq string)
  "Return a substring, with characters in CHARSEQ removed from the
beginning and end, of STRING."
  (let ((regexp (chars->regexp charseq)))
    (string-left-trim-regexp regexp (string-right-trim-regexp regexp string))))

;; Perhaps all the string-trim functions should be defined more like this one.
(defsubst string-trim-whitespace (string)
  (if (string-match "^\\s *\\(.*[^ \t\n]\\)\\s *$" string)
      (match-string 1 string)
    ""))

(defun string-remove (char string)
  "Return a string with CHAR removed, but otherwise like STRING."
  (string-remove-regexp (concat "[" char "]") string))

(defun string-remove-regexp (regexp string)
  "Return a string with (nonoverlapping) matches for REGEXP removed from STRING."
  (let ((result ""))
    (while (string-match regexp string)
      (setq result (concat result (substring string 0 (match-beginning 0)))
            string (substring string (match-end 0))))
    (concat result string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Non-CL utilities which depend on these functions follow.
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;

(defsubst string-lines (string)
  "Return the number of lines in the printed representation of STRING."
  (1+ (count ?\n string)))

(defsubst blank-string-p (string)
  "Return non-nil if STRING contains any non-whitespace characters."
  (string-match "^[ \t\n]*$" string))

(defsubst blank-string-or-nil-p (string-or-nil)
  "Return non-nil if STRING is nil or contains any non-whitespace characters."
  (or (not string-or-nil)
      (blank-string-p string-or-nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structures
;;;

;; This works with the cl.el structures

;; Return an index that can be used by aset and aref.  SLOTNAME is a
;; string or a symbol, with or without a leading colon.
(defun slotname->index (structurename slotname)
  (let ((index (cdr (assoc (intern-soft
                            ;; string with colon prepended
                            (concat ":"
                                    ;; remove leading colon, if any
                                    (string-left-trim ":"
                                                      ;; make slotname a string
                                                      (format "%s" slotname))))
                           (get structurename ':structure-indices)))))
    (if index
        (1+ index))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structures redux
;;;

;;; Simple defstruct originally from BBDB, by Jamie Zawinski <jwz@lucid.com>.

;;; Use it like this:
;;; (def-mdecl-struct bbdb-phone
;;;   location area exchange suffix extension)
;;; (setq this-phone-record (make-vector bbdb-phone-length nil))
;;; (bbdb-phone-set-exchange this-phone-record 617)
;;; (bbdb-phone-exchange this-phone-record) ==> 617

;;; Changes by Michael Ernst <mernst@theory.lcs.mit.edu>, March 19, 1992:
;;;  * added an extra first slot which holds the struct name; this is good
;;;    for determining a structure's type
;;;  * added def-mdecl-struct-concatenator to permit greater flexibility in
;;;    names of accessor functions
;;;  * added make-foo and foo-p functions

;;; A make- constructor, with default values and overrides, would be nice.
;;; It might just be best to have the programmer define one by hand, but
;;; I'm not convinced by that.

;;; Why does it have such a strange name?  Because I want it to start with
;;; "def", so it shows up in TAGS files, and to end with "struct", so I can
;;; do M-. struct foo to go to the definition of the foo-bar-baz structure.

(defvar def-mdecl-struct-concatenator "-"
  "Inserted between the struct and slot names in slot accessors and setters.
Typical values are \"-\" and \"\".")

;; NAME is a symbol or a list of (symbol (option-name option-value) ...).
(defmacro def-mdecl-struct (name &rest slots)
  "Define NAME as a structure type with a slot for each additional argument.
NAME is a symbol, the name of the new structure, and each slotname is a symbol.
This macro defines functions `make-NAME', `NAME-p', and `copy-NAME' for the
structure, and functions `NAME-SLOTNAME' and `NAME-set-SLOTNAME' to access and
set slots.  It also sets variable  NAME-length  to the number of slots.

NAME may also be a list (struct-name (option-name option-value) ...), where
each option-name is a keyword symbol in \{:constructor :predicate :copier\}
and option-value is a symbol, the name that should be used for that
function instead of the defaults listed above."

  (let ((body '())
        (i 1)
        (L (length slots))
        conc-name options
        name1 name2 makename predname copyname)
    (if (listp name)
        (setq options (cdr name)
              name (car name)))
    (setq conc-name (concat (symbol-name name)
                            def-mdecl-struct-concatenator))
    (while slots
      (setq name1 (intern (concat conc-name (symbol-name (car slots))))
            name2 (intern (concat conc-name "set-" (symbol-name (car slots))))
            body (nconc body
                        (list
                         (list 'defmacro name1 '(vector)
                               (list 'list ''aref 'vector i))
                         (list 'defmacro name2 '(vector value)
                               (list 'list ''aset 'vector i 'value))
                         (list 'put (list 'quote name1)
                               ''edebug-form-hook ''(form))
                         (list 'put (list 'quote name2)
                               ''edebug-form-hook ''(form form))
                         ))
            slots (cdr slots)
            i (1+ i)))
    (setq makename (or (car (cdr (assoc ':constructor options)))
                       (intern (concat "make" def-mdecl-struct-concatenator
                                       (symbol-name name))))
          predname (or (car (cdr (assoc ':predicate options)))
                       (intern (concat conc-name "p")))
          copyname (or (car (cdr (assoc ':copier options)))
                       (intern (concat "copy" def-mdecl-struct-concatenator
                                       (symbol-name name)))))
    (setq body (nconc body (list (list 'defconst
                                       (intern (concat conc-name "length"))
                                       L)
                                 (list 'defun makename '()
                                       (list 'let (list (list 'result (list 'make-vector (1+ L) nil)))
                                             (list 'aset 'result 0
                                                   (list 'quote name))
                                             'result))
                                 (list 'put (list 'quote makename)
                                       ''edebug-form-hook ''())
                                 (list 'defmacro copyname '(struct)
                                       '(list 'copy-sequence struct))
                                 (list 'put (list 'quote copyname)
                                       ''edebug-form-hook ''(form))
                                 (list 'defun predname '(object)
                                       (concat "T if OBJECT is a "
                                               (symbol-name name) ".")
                                       (list 'and
                                        '(vectorp object)
                                        (list '= '(length object) (1+ L))
                                        (list 'eq '(aref object 0)
                                              (list 'quote name)))))))
    (cons 'progn body)))
(put 'def-mdecl-struct 'edebug-form-spec '(&rest form))
