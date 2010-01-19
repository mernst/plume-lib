;; This file augments the nuke-trailing-whitespace package to be able to
;; conveniently ignore files whose name matches certain regular expressions.

;; Typical usage:

;; (require 'nuke-whitespace-filenames)
;; (setq-default nuke-trailing-whitespace-p 'nuke-trailing-whitespace-check-filename)
;; (setq nuke-trailing-whitespace-ignore-filename-regexps
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


(defvar nuke-trailing-whitespace-ignore-filename-regexps
  nil
  "List of regular expressions.  If any of them match a file name, then
trailing whitespace is not removed from the file.
These are typically source files whose style I shouldn't modify, because
they are maintained by someone else, and I wish to minimize differences/patches."
)


(defun nuke-trailing-whitespace-ignored-filename-p (filename)
  "Return t if FILENAME should not have trailing whitespace removed."
  (let ((match nil)
	(regexps nuke-trailing-whitespace-ignore-filename-regexps))
    (while regexps
      (let ((regexp (car regexps)))
	(setq regexps (cdr regexps))
	(if (string-match regexp filename)
	    (setq match t
		  regexps nil))))
    match))

(defun nuke-trailing-whitespace-check-filename ()
  "A value for `nuke-trailing-whitespace-p'.
Returns false if filename should be ignored, else delegates to
`nuke-trailing-whitespace-check-mode'."
  (if (and (buffer-file-name)
	   (nuke-trailing-whitespace-ignored-filename-p (buffer-file-name)))
      nil
    (nuke-trailing-whitespace-check-mode)))

(provide 'nuke-whitespace-filenames)