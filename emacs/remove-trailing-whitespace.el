;; When you edit source code, trailing whitespace serves no purpose.
;; This file causes such whitespace to be removed when the file is written.

;; TODO: See Emacs function delete-trailing-whitespace instead.
;; Also see library whitespace.el, which is part of the Emacs distribution and contains `whitespace-cleanup'.



(defun install-whitespace-hooks ()
  (add-hook 'write-contents-hooks 'maybe-remove-trailing-whitespace))

(add-hook 'c-mode-hook 'install-whitespace-hooks)
(add-hook 'java-mode-hook 'install-whitespace-hooks)
(add-hook 'makefile-mode-hook 'install-whitespace-hooks)

;; Customize if desired.
(defvar remove-trailing-whitespace-ignore-regexps
  nil
  "List of regular expressions.  If any of them match a file name, then
trailing whitespace is not removed from the file.
These are typically source files whose style I shouldn't modify, because
they are maintained by someone else, and I wish to minimize differences/patches."
)

(defun remove-trailing-whitespace-ignored-filename (filename)
  "Return t if FILENAME should not have trailing whitespace removed."
  (let ((match nil)
	(regexps remove-trailing-whitespace-ignore-regexps))
    (while regexps
      (let ((regexp (car regexps)))
	(setq regexps (cdr regexps))
	(if (string-match regexp filename)
	    (setq match t
		  regexps nil))))
    match))

(defun maybe-remove-trailing-whitespace ()
  "Remove trailing whitespace and newlines from all lines in the file,
unless the file is maintained by someone else."
  (if (not (and (buffer-file-name)
		(remove-trailing-whitespace-ignored-filename (buffer-file-name))))
      (progn
	(delete-trailing-whitespace)
	;; I'm not sure where this is defined.  Re-enable when I figure
	;; that out, or when I re-implement.
	;; (remove-trailing-newlines)
	)))


(provide 'remove-trailing-whitespace)
