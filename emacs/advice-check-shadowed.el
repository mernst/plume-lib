;; advice-check-shadowed.el -- fixes to advice.el

;; Add check for shadowed variables.

;; I submitted a patch on 8/21/97 for Emacs 19.34, but the defadvice
;; maintainer was strongly opposed.  He suspects that there would be a lot
;; of false positives (I found none in hundreds of files) and contrived
;; just one example that he admitted is bad code.  He said that he expected
;; Lisp programmers who use defadvice to be aware of its dangers and
;; subtleties, so they don't need warnings.

(eval-when-compile
  (require 'advice))

(defun ad-insert-argument-access-forms (definition arglist)
  ;;"Expands arg-access text macros in DEFINITION according to ARGLIST."
  (ad-substitute-tree
   (function
    (lambda (form)
      (if (memq (car-safe form) '(let let*))
          (let* ((bindings (car-safe (cdr form)))
                 (vars (mapcar (function (lambda (binding)
                                           (if (symbolp binding)
                                               binding
                                             (car-safe binding))))
                               bindings)))
            (while vars
              (if (memq (car vars) arglist)
                  ;; Problem: not easy to report function name.
                  ;; Workaround: set debug-on-error and try again...
                  (error "Binding of %s shadows formal parameter -- choose another name (to debug, set debug-on-error to true)" (car vars)))
              (setq vars (cdr vars)))))
      (or (eq form 'ad-arg-bindings)
          (and (memq (car-safe form)
                     '(ad-get-arg ad-get-args ad-set-arg ad-set-args))
               (integerp (car-safe (cdr form)))))))
   (function
    (lambda (form)
      (if (eq form 'ad-arg-bindings)
          (ad-retrieve-args-form arglist)
        (let ((accessor (car form))
              (index (car (cdr form)))
              (val (car (cdr (ad-insert-argument-access-forms
                              (cdr form) arglist)))))
          (cond ((eq accessor 'ad-get-arg)
                 (ad-get-argument arglist index))
                ((eq accessor 'ad-set-arg)
                 (ad-set-argument arglist index val))
                ((eq accessor 'ad-get-args)
                 (ad-get-arguments arglist index))
                ((eq accessor 'ad-set-args)
                 (ad-set-arguments arglist index val)))))))
   definition))
