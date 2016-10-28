;; headers.el -- show first line of all definitions in a buffer
;; Michael Ernst <mernst@alum.mit.edu>
;; 5/20/93
;; Thanks to David Glasser <glasser@mit.edu> for improvements.

;;; Commentary:

;; Function `headers' (bound to C-x C-p C-h) shows the first line of all
;; definitions in a buffer; from the definitions buffer, the user may jump
;; to one of them via "g" or "j" or "C-c C-c".
;;
;; It is easy to add support for new languages.
;;
;; This borrows ideas from page-ext.el.  I would have preferred to use
;; page-ext.el, but it doesn't display the page separator, only what
;; follows it on that line.
;;
;; However, this should probably be rewritten to use page-ext.el; they are
;; quite duplicative.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User-visible variables
;;;

(defvar headers-regexp nil
  "Regexp to use when searching for definition headers.
Overrides all other defaults.  If nil, a default is used.")
(make-variable-buffer-local 'headers-regexp)

(defvar headers-omit-regexp nil
  "Regexp to ignore when searching for definition headers.
Overrides all other defaults.  If nil, a default is used.")
(make-variable-buffer-local 'headers-omit-regexp)

(defvar headers-scheme-regexp nil
  "If non-nil, use this regexp instead of the default in Scheme mode.")

(defvar headers-internal-definitions-p nil
  "If non-nil, internal definitions are included in the headers buffer.
If you set this, then you may want to set `headers-scheme-regexp' or
adjust `scheme-defform-regexp' to include named-lambda.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other variables
;;;

(defvar headers-code-buffer nil
  "The buffer containing the code for this headers buffer.")
(make-variable-buffer-local 'headers-code-buffer)

;; This requires ANSI C, or nearly; for instance, all functions must have
;; a return type.
;; This could use a bit more work, but it catches a lot as it is.
;; It doesn't cope with user-defined types at all.
;; (defvar c-type-regexp "\\(char\\|inline\\|int\\|void\\)\\s *\\**")
(defvar c-type-regexp "\\(\\w\\|:\\)+\\s *\\**")
;; what is the colon doing here?
(defvar c-symbol-regexp "\\(\\w\\|:\\)+")
(defvar c-type-and-symbol-regexp
  (concat c-type-regexp
          "\\("
          "\\s *"
          c-symbol-regexp
          "\\)?")
  "Actually the type is optional.")
(defvar c-args-regexp
  (concat "("
          "\\("
          c-type-and-symbol-regexp
          ",\\s *"
          "\\)"
          c-type-and-symbol-regexp
          ")"))

(defvar c-definition-regexp
  (concat "^"
          c-type-and-symbol-regexp
          "\\("
          "\\s *"
          c-args-regexp
          "\\)?"))

;; not \\w+, as that omits "_.".
(defvar java-word-regexp "\\b[A-Za-z0-9_]\\([A-Za-z0-9_.$<]*[A-Za-z0-9_]\\)?\\b>?")
;; Empty square brackets with leading space.
(defvar square-brackets-regexp "\\(\\s *\\[\\s *\\]\\)")
;; Like type-and-symbol; maybe just 1 symbol
(defvar java-symbols-regexp
  (concat java-word-regexp square-brackets-regexp "*"
          "\\(\\s +" java-word-regexp square-brackets-regexp "*" "\\)*"
          ))
(defvar java-args-regexp
  (concat "(\\s *"
          "\\("
          java-symbols-regexp
          "\\("
          "\\s *,\\s *"
          java-symbols-regexp
          "\\)*"
          "\\s *"
          "\\)?"
          ")"
          "[ \t]*\\($\\|[^ \t.]\\)"     ; aviod "foo()" in "foo().bar();"
          ))
;; Unfortunately, this doesn't get member variables, only methods.
;; I don't see how to get the member variables without catching too
;; many local variables.
(defvar java-definition-regexp
  (concat "\\("
          "^[ \t]*"
          java-symbols-regexp
          "\\("
          "\\s *"
          java-args-regexp
          "\\)"
          "\\|"
          "^[ \t]*\\(\\(public\\|private\\|protected\\|final\\|abstract\\)\\b.*\\)?\\bclass\\b"
          "\\)"))
(defvar java-nondefinition-regexp
  (concat
   "^[ \t]*\\(catch\\|if\\|return\\|switch\\|while\\)\\b" ; these aren't (typeless) methods
   "\\|"
   "^[ \t]*[^a].*\;$"                 ; ends w/semicolon, but isn't abstract
   ))


(defvar headers-map (make-sparse-keymap)
  "Keymap for the headers buffer.")

(define-key headers-map "\C-c\C-c" 'headers-goto)
(define-key headers-map "g" 'headers-goto)
(define-key headers-map "j" 'headers-goto)
(define-key headers-map "\C-m" 'headers-goto)
(define-key headers-map [mouse-2] 'headers-mouse-goto)
(define-key headers-map " " 'next-line)
(define-key headers-map "n" 'next-line)
(define-key headers-map "p" 'previous-line)

(if (not (boundp 'ctl-x-ctl-p-map))
    (progn
      (defvar ctl-x-ctl-p-map (make-sparse-keymap)
        "Keymap for subcommands of C-x C-p, which are for page handling.")
      (fset 'ctl-x-ctl-p-prefix ctl-x-ctl-p-map)
      (global-unset-key "\C-x\C-p")
      (define-key ctl-x-map "\C-p" 'ctl-x-ctl-p-prefix)
      (define-key ctl-x-ctl-p-map "\C-m" 'mark-page)
      (fset 'ctl-x-ctl-p-prefix ctl-x-ctl-p-map)))
(define-key ctl-x-ctl-p-map "\C-h" 'headers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Computing headers
;;;

;; This ought to go to a nearby definition, not stay at the beginning of
;; the buffer.
(defun headers ()
  "Display a buffer of definition \(function\) headers for this code buffer."
  (interactive)
  (let ((code-buffer (current-buffer))
        (code-tab-width tab-width)
        (header-buffer (get-buffer-create
                          (concat "*Definitions in: " (buffer-name))))
        (current-header (save-excursion
                          (end-of-defun 1) ; associate comments w/following def
                          (beginning-of-defun 1)
                          (concat "\n" (buffer-line) "\n")))
        (regexp (headers-regexp))
        (omit-regexp (headers-omit-regexp))
        (page-regexp (or (headers-page-delimiter) page-delimiter)))

    (pop-to-buffer header-buffer)
    (buffer-disable-undo header-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq tab-width code-tab-width)
    (use-local-map headers-map)
    (setq headers-code-buffer code-buffer)

    (insert-buffer-substring headers-code-buffer)
    (goto-char (point-min))
    (delete-non-matching-lines (concat "\\(" regexp "\\|" page-regexp "\\)"))
    (if omit-regexp
        (delete-matching-lines omit-regexp))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n" nil nil))
    (goto-char (point-min))
    (if (search-forward current-header nil t)
        (forward-line -1))
    (shrink-window-if-larger-than-buffer)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(defun headers-goto ()
  "Go to the definition whose header is under point."
  (interactive)
  (let* ((string (concat "\n" (buffer-line) "\n")))
    (pop-to-buffer headers-code-buffer)
    (push-mark)
    (goto-char (point-min))
    (search-forward string)
    (forward-line -1)))

(defun headers-mouse-goto (event)
  "Go to the definition whose header is clicked upon."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event)))
    (headers-goto)))

(defun buffer-line ()
  "Return the text of the current line of the buffer."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun headers-regexp ()
  "Return a regular expression for finding headers in the current major mode."
  (or headers-regexp
      (cond ((or (eq major-mode 'scheme-mode)
                 (eq major-mode 'chez-scheme-mode))
             (or headers-scheme-regexp
                 (concat (if headers-internal-definitions-p "^[\t ]*(" "^\(")
                         (substring (or scheme-defform-regexp "^def") 1))))
            ((or (eq major-mode 'lisp-mode)
                 (eq major-mode 'emacs-lisp-mode)
                 (eq major-mode 'fi:common-lisp-mode))
             "^\(def")
            ((or (eq major-mode 'c-mode)
                 (eq major-mode 'c++-mode))
             c-definition-regexp)
            ((or (eq major-mode 'java-mode)
                 (eq major-mode 'jde-mode))
             java-definition-regexp)
            ((eq major-mode 'perl-mode)
             "^\\(sub\\|my\\)\\b")
            ((eq major-mode 'python-mode)
             "^[ \t]*\\(def\\|class\\)\\b")
            ((eq major-mode 'texinfo-mode)
             "^@\\(chapter\\|\\(sub\\)*section\\)[ \t]")
            ((eq major-mode 'cecil-mode)
             ;; This is giving me trouble: doesn't always include everything.
             (let ((top-alternatives
                    (list
                     (concat ;; (regexp-opt
                             ;;  '("abstract" "template" "concrete" "dynamic")
                             ;;  'paren)
                             "\\(abstract\\|concrete\\|dynamic\\|template\\)"
                             "?"
                             "[ \t]*"
                             ;; (regexp-opt
                             ;;  '("representation" "object") 'paren)
                             "\\(object\\|representation\\)")
                     ;; (regexp-opt
                     ;;  '("type" "predicate" "signature" "method" "module") 'paren)
                     "\\(m\\(ethod\\|odule\\)\\|predicate\\|signature\\|type\\)"
                     (concat "extend[ \t]+"
                             ;; (regexp-opt
                             ;; '("type" "representation" "object") 'paren)
                             ;; "?"
                             )
                     ;; "\\(method[ \t]+\\)?implementation"
                     (concat
                      "\\(shared[ \t]+\\)?"
                      "\\(var[ \t]+\\)?"
                      "field[ \t]+"
                      ;; (regexp-opt
                      ;; '("signature" "field implementation") 'paren)
                      ;; "?"
                      ))))
               (concat
                "^[ \t]*\\(public\\|private\\|protected\\)?[ \t]*"
                (regexp-opt top-alternatives 'paren)
                "\\b")))
            (t
             (error "headers.el doesn't understand %s; please teach it"
                    major-mode)))))

(defun headers-omit-regexp ()
  "Return regexp for omitted headers in current major mode."
  (or headers-omit-regexp
      (cond
       ((or (eq major-mode 'java-mode)
            (eq major-mode 'jde-mode))
        java-nondefinition-regexp))))

(defun headers-page-delimiter ()
  "Return page delimiter for current major mode."
  nil)

(provide 'headers)

;;; headers.el ends here
