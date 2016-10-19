;;; unbalanced-paren.el

;; Emacs 21.2 has `check-parens'.  Consider merging the two or eliminating
;; this one.  I'm currently experimenting with using `check-paren' instad
;; of this.

(defvar lisp-major-modes '(emacs-lisp-mode lisp-mode fi:common-lisp-mode
                                           scheme-mode))

(defvar c-major-modes '(c-mode c++-mode objc-mode java-mode))


;; Another way to find an extra open paren is to use backward-up-list from
;; the end of the buffer.
(defun unbalanced-paren (&optional no-err)
  "Find an unbalanced parenthesis in the current buffer.
If optional argument NO-ERR is supplied, then if there's an unbalanced
parenthesis, return a string (an error message) and put point at the problem;
if all parens are balanced, return nil."
  (interactive)
  (catch 'found-unbalanced-paren
    (let ((opoint (point))
          (error-fun (function (lambda (string)
                                 (if no-err ; test inside fn:  fn usually isn't called
                                     (throw 'found-unbalanced-paren string)
                                   (error string)))))
          ;; Problem:  as of cc-mode 5.25 (released December 1998,
          ;; distributed with Emacs 20.4, and current as of August 1999),
          ;; c-beginning-of-defun doesn't obey its documentation and
          ;; doesn't return a meaningful result.
          (beginning-of-defun-fn (cond
                                  ((memq major-mode lisp-major-modes)
                                   (function beginning-of-defun))
                                  ((memq major-mode c-major-modes)
                                   (function c-beginning-of-defun))
                                  (t
                                   (function beginning-of-defun)))))
      (goto-char (point-min))
      (while (condition-case nil
                 (funcall beginning-of-defun-fn -1)
               (scan-error t))
        (let ((end-of-fn-pos (point)))
          ;; This errs if there is an extra open paren (missing close paren).
          (if no-err
              (condition-case forward-sexp-err
                  (forward-sexp 1)
                (error (funcall error-fun (car (cdr forward-sexp-err)))))
            (forward-sexp 1))
          ;; This errs if there is an extra close paren (missing open paren).
          (if (looking-at "[ \t\n\r]*)")
              (progn
                (goto-char (- (match-end 0) 1))
                (funcall error-fun "Extra close parenthesis")))
          ;; This was "(match-beginning 0)", which (only?) worked in Lisp.
          (goto-char end-of-fn-pos)))

      (cond ((memq major-mode lisp-major-modes)
             (lisp-unbalanced-paren-heuristics error-fun)))

      (goto-char opoint)
      (if (interactive-p)
          (message "Couldn't find unbalanced parenthesis")
        ;; return nil if no problems
        nil))))

(defun lisp-unbalanced-paren-heuristics (error-fun)
  "ERROR-FUN takes a single string argument."
  (goto-char (point-min))
  (while (forward-comment 9999) nil)    ; skip whitespace, comments
  (while (not (eobp))
    ;; We should be at the beginning of a defun
    (if (not (= (current-column) 0))
        (funcall error-fun "Possible missing open or extra close parenthesis"))
    (if (not (or (looking-at "(")
                 ;; For Common Lisp
                 (looking-at "#[-+]")
                 (looking-at "#||")
                 ;; Some losers uses quotes for beginning-of-file comments
                 (and (looking-at "\"") (bobp))))
        (funcall error-fun "Character besides open paren starts a top-level form"))
    ;; This errs if there is an extra open paren (missing close paren).
    (condition-case forward-sexp-err
        (if (looking-at "#[-+]")
            (forward-sexp 2)
          (forward-sexp 1))
      (error (funcall error-fun (car (cdr forward-sexp-err)))))
    ;; This errs if there is an extra close paren (missing open paren).
    (if (not (looking-at "[ \t\n\r]"))  ; Perhaps permit semicolon too.
        (funcall error-fun "No whitespace at end of definition"))
    (if (not (looking-at "[ \t]*[;\n]"))
        (funcall error-fun "No whitespace at end of definition"))
    (while (forward-comment 9999) nil)  ; skip whitespace, comments
    ))

;; To do:  I ought to give the name of the buffer in the error message.
;; Unlike unbalanced-paren, this is appropriate as a write-{file,contents}-hook
(defun check-for-unbalanced-paren ()
  ;; I can't wrap this in save-excursion because in that case, even if the
  ;; error is signalled, we return to the original point.  I want to move
  ;; point iff the error is signaled.
  (let ((message (unbalanced-paren 'no-err)))
    (if (and message
             (progn
               (sit-for 0)              ; perform redisplay
               (not (y-or-n-p (concat message "; save anyway? ")))))
        ;; Since basic-save-buffer has a save-excursion, we don't leave
        ;; point at the problem, unfortunately.
        (error message)))
  ;; return nil so this can be used as a write-{file,contents}-hook
  nil)
