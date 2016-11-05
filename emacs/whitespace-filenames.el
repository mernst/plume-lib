;; This file augments the whitespace package to be able to
;; conveniently ignore files whose name matches certain regular expressions.

;; Typical usage:

;; (require 'whitespace-filenames)
;; (setq trailing-whitespace-ignore-filename-regexps
;;       (list
;;        ;; Emacs source files
;;        "mernst/emacs/x?lisp/"
;;        "emacs[-/][0-9]+\.[0-9]+\\(\.[0-9]+\\)?/\\(lisp\\|src\\)/"
;;        "viewmail/lisp/"
;;        ;; Javac compiler
;;        "annotations/\\(vendor-\\)?compiler/"
;;        ;; ASM bytecode manipulation library
;;        "annotations/asmx?/"
;;        "org/objectweb/asm/"
;;        ;; FreePastry
;;        "pastry/src/"
;;        ;; Valgrind (part of Kvasir)
;;        "valgrind-3/"
;;        ))


(defvar whitespace-ignore-filename-regexps
  nil
  "List of regular expressions.  If any of them match a file name, then
whitespace is not cleaned up in the file.
These are typically source files whose style I shouldn't modify, because
they are maintained by someone else, and I wish to minimize differences/patches."
)


(defun whitespace-ignored-filename-p (filename)
  "Return t if FILENAME should not have trailing whitespace removed."
  (let ((match nil)
        (regexps whitespace-ignore-filename-regexps))
    (while regexps
      (let ((regexp (car regexps)))
        (setq regexps (cdr regexps))
        (if (string-match regexp filename)
            (setq match t
                  regexps nil))))
    match))

(provide 'whitespace-filenames)
