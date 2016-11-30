;;; etags-mde.el -- etags.el enhancements:  defaults, related matches, continue

;; Author: Michael Ernst <mernst@alum.mit.edu>
;; Created: 24 Jun 1993
;; Keywords: tools

;;; Commentary:

;; This code improves Emacs 19's etags.el in the following ways.

;; * Provide defaults for tags-search.

;; * Bind function mde-tags-loop-continue to M-, in place of
;;   tags-loop-continue.  When the immediately preceding command was find-tag
;;   (M-.), it continues that search; otherwise, it continues the last
;;   tags-search or tags-query-replace command, like tags-loop-continue.

;; * Permit finding related tags after the original find-tag fails.
;;   For instance, (find-tag "mystruct-slota"), after failing to find this
;;   string explicitly in the TAGS file, would look for related entries and
;;   eventually position point on the (defstruct mystruct ...) form which
;;   defined that function.

;; * Permit ?: characters in tags in etags-tags-completion-table.

;; * Add tags-replace, like tags-query-replace.

;; * Make tags-verify-table never query the user, always use latest TAGS table.

;; Use this code by placing
;;   (eval-after-load "etags" '(load "etags-mde" nil t))
;; in your .emacs file.


;;; Code:

;; Probably a bad idea, since loading etags.el loads etags-mde.el
;; (eval-when-compile (require 'etags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defaults for tags-search
;;;

(defvar tags-search-edit-search-string nil
  "If non-nil, `tags-search' initially edits the default regexp.
Otherwise, the default is displayed but no initial input is supplied.")

(defun tags-search-tag (prompt)
  "Like `find-tag-tag' of etags.el, but regexp-quotes and doesn't complete.
The two functions are sufficiently different that it probably doesn't
pay to try to merge them."
  (let* ((default (funcall (tags-search-tag-default-function)))
         (default-re (and default (regexp-quote default)))
         (spec (if tags-search-edit-search-string
                   (read-string (concat prompt ": ") default-re)
                 (read-string
                  (if default-re
                      (format "%s (default %s): " prompt default-re)
                    (concat prompt ": "))))))
    (if (equal spec "")
        (or default-re (error "There is no default tag"))
      spec)))

(emacs-fsf
 (defun tags-search-tag-default-function ()
   (or find-tag-default-function
       (get major-mode 'find-tag-default-function)
       'find-tag-default)))
(xemacs
 (defun tags-search-tag-default-function ()
   (or find-tag-default-hook
       (get major-mode 'find-tag-default-hook)
       'find-tag-default)))

(defadvice tags-search (before interactive-enhancement activate)
  "Use `tags-search-tag' to read interactive argument."
  (interactive (list (tags-search-tag "Tags search regexp"))))

(defadvice tags-query-replace (before interactive-enhancement activate)
  "Use `tags-search-tag' to read interactive argument."
  (interactive
   (let ((search (tags-search-tag "Tags query replace regexp")))
     (list search
           (read-string (format "Tags query replace %s by: "
                                search)
                        (and tags-search-edit-search-string
                             search))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding related tags
;;;

;; I should also try setting search-caps-disable-folding to nil, in case
;; the tag wasn't found because of a case mismatch (a la Lisp).

(defvar tags-find-related-names-functions
  '()
  "A list of related-names functions.
Each function takes one argument, a TAGNAME, and returns a list
of tag regexps to try if that search fails.")

;; Could I perhaps have done something similar simply by setting
;; find-tag-regexp-tag-order and find-tag-tag-order?

;;; (Very) simplified version of below patch; just sets variable this-command.
;; I need this because when we read from the minibuffer, then last-command
;; becomes minibuffer-exit.
;; Maybe Emacs should do this by default.
(defadvice find-tag (after set-this-command activate)
  "Set `this-command' if called interactively."
  (if (interactive-p) (setq this-command 'find-tag)))

;; ;; This needs to be modified for XEmacs.
;; ;; And possibly for FSF Emacs 20?
;; (emacs-19
;;  (defun find-tag (tagname &optional next-p regexp-p)
;;    "Find tag (in current tags table) whose name contains TAGNAME.
;; Select the buffer containing the tag's definition, and move point there.
;; The default for TAGNAME is the expression in the buffer around or before point.
;;
;; If second arg NEXT-P is t (interactively, with prefix arg), search for
;; another tag that matches the last tagname or regexp used.  When there are
;; multiple matches for a tag, more exact matches are found first.  If NEXT-P
;; is the atom `-' (interactively, with prefix arg that is a negative number
;; or just \\[negative-argument]), pop back to the previous tag gone to.
;;
;; See documentation of variable `tags-file-name'.
;;
;; Modified by MDE:
;;  If the search fails, try related names before erring.
;;  Set `this-command' if called interactively."
;;    (interactive (progn (require 'etags) (find-tag-interactive "Find tag: ")))
;;    ;; Watch out; in Emacs 19.34, if a function is advised, it isn't considered
;;    ;; to be called interactively, even if the advice is called interactively.
;;    ;; That's why I have inlined find-tag-noselect here.
;;    ;; If my patch makes it to Emacs 19.35, this won't be a problem any more.
;;    (if (interactive-p) (setq this-command 'find-tag))
;;    (if next-p
;;        (switch-to-buffer (find-tag-noselect tagname next-p regexp-p))
;;      (condition-case err
;;       (switch-to-buffer (find-tag-noselect tagname next-p regexp-p))
;;        (error
;;      (if (not (err-no-tag-p err))
;;          ;; I'd prefer to reraise/pass the error along instead of
;;          ;; creating a new one, so it doesn't seem to come from here.
;;          (signal (car err) (cdr err)))
;;      (let ((mod-tagnames (apply (function append)
;;                                 (mapcar (function (lambda (f) (funcall f tagname)))
;;                                         tags-find-related-names-functions)))
;;            (found nil))
;;        (while (and mod-tagnames (not found))
;;          (condition-case err
;;              (setq found
;;                    (find-tag-noselect (car mod-tagnames) next-p t))
;;            (error
;;             (if (err-no-tag-p err)
;;                 ;; I'd prefer to reraise/pass the error along instead of
;;                 ;; creating a new one, so it doesn't seem to come from here.
;;                 (signal (car err) (cdr err))
;;               (setq mod-tagnames (cdr mod-tagnames))))))
;;        (if found
;;            (switch-to-buffer found)
;;          (error "No tags containing %s" tagname))))))))

;; Perhaps inline for speed?
;; This returns false in particular for "rerun etags" and such.
(defun err-no-tag-p (err)
  "Return t if error ERR has to do with tag not found in tags table, nil otherwise."
  (and (eq (car err) 'error)
       (let ((err-text (car (cdr err))))
         (and (> (length err-text) 17)
              (let ((first-seventeen (substring err-text 0 17)))
                ;; perhaps more efficient than string-match,
                ;; and doesn't clobber match-data
                (or (equal first-seventeen "No more tags matc")
                    (equal first-seventeen "No more tags cont")
                    (equal first-seventeen "No tags matching ")
                    (equal first-seventeen "No tags containin")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding related tags
;;;

;; Here is an example tags-find-related-names function.
;; You might put this form in your .emacs:
;;   (setq tags-find-related-names-functions
;;         '(mit-scheme-tags-find-related-names))
;; I need to separate it into independent parts:  MIT Scheme, and Zaphod.


;; These functions assume (possibly incorrectly) that the tag name is a
;; string, not a regexp.
(setq tags-find-related-names-functions
      '(lisp-tags-find-related-names
        mit-scheme-tags-find-related-names
        perl-tags-find-related-names))

(defun perl-tags-find-related-names (tagname)
  "Find `@TAGNAME' or `%TAGNAME' definition based on a `$TAGNAME' use."
  (if (string-match "^\\$" tagname)
      (list (substring tagname 1))))

(defun lisp-tags-find-related-names (tagname)
  (cond ((or (string-match "^\\([^:]+::?\\)?set-\\(.+\\)-[^-]+!$" tagname)
             (string-match "^\\([^:]+::?\\)?\\(.+\\)-[^>][^-]+$" tagname))
         (let ((max-name (substring tagname
                                    (match-beginning 2) (match-end 2)))
               (result ()))
           (while max-name
             ;; largest names come first in result list
             (setq result (nconc result
                                 (structure-name->defstruct-line max-name))
                   max-name (if (string-match "-[^>][^-]*$" max-name)
                                (substring max-name 0 (match-beginning 0)))))
           result))))
;; Testing
;; (lisp-tags-find-related-names "foo-bar-baz")
;; (lisp-tags-find-related-names "foo-set-bar-baz")


;; Returns a list of tag regexps to try if tagname isn't found verbatim.
(defun mit-scheme-tags-find-related-names (tagname)
  (cond ((string-match "\\(^\\|:\\)\\(make\\|copy\\)-" tagname)
         (structure-name->define-structure-line
          (substring tagname (match-end 0))
          (substring tagname 0 (match-beginning 0))))
        ((string-match "^vdg:construct-\\(.*\\)-\\(node\\|ports?\\)$" tagname)
         (list (concat "(define-vdg-constructors "
                       (substring tagname
                                  (match-beginning 1)
                                  (match-end 1)))))
        (;; (string-match "\\(^\\|:\\)\\(.+\\)\\?$" tagname)
         (string-match "^\\([^:]+:\\)?\\(.+\\)\\?$" tagname)
         (structure-name->define-structure-line
          (substring tagname (match-beginning 2) (match-end 2))
          (and (match-beginning 1)
               (substring tagname (match-beginning 1) (1- (match-end 1))))))
        ((or (string-match "^\\([^:]+:\\)?set-\\(.+\\)-[^-]+!$" tagname)
             (string-match "^\\([^:]+:\\)?\\(.+\\)-[^>][^-]+$" tagname))
         (append
          (save-match-data
            (if (string-match "^vdg:node-" tagname)
                (list
                 (concat "(define-vdg-attribute-and-accessors "
                         (substring tagname (match-end 0))))
              '()))
          (let ((prefix (and (match-beginning 1)
                             (substring tagname
                                        (match-beginning 1) (1- (match-end 1)))))
                (max-name (substring tagname
                                     (match-beginning 2) (match-end 2)))
                (result ()))
            (while max-name
              ;; largest names come first in result list
              (setq result (nconc result
                                  (structure-name->define-structure-line
                                   max-name prefix))
                    max-name (if (string-match "-[^>][^-]*$" max-name)
                                 (substring max-name 0 (match-beginning 0)))))
            result)))))

(defvar tag-end-re "[ \)]")

(defun structure-name->defstruct-line (structure-name)
  ;; This will pick up some false hits; I can't add punctuation after
  ;; structure-name because the DEL character immediately follows it.
  ;; Or, I could use the regexp-p option of `find-tag-noselect'.
  (list
   (concat "(defstruct (?" (regexp-quote structure-name) tag-end-re)))

;; Fullname is nil unless prefix is set.
(defun structure-name->define-structure-line (structure-name &optional prefix fullname)
  (if (equal prefix "")
      (setq prefix nil))
  (if (and (not fullname) prefix)
      (setq fullname (concat prefix ":" structure-name)))
  (let* ((structure-name-re (regexp-quote structure-name))
         (fullname-re (and fullname (regexp-quote fullname)))
         (structure-names
          ;; This will pick up some false hits; I can't add punctuation after
          ;; structure-name because the DEL character immediately follows it.
          ;; Or, I could use the regexp-p option of `find-tag-noselect'.
          (if prefix
              (list
               (concat "(define-structure\\(-prefixed\\)? (?" fullname-re tag-end-re))
            (list
             (concat "(define-structure (?" structure-name-re tag-end-re))))
         ;; For MSR
         (zaphod-node-names
           (append
            (if prefix
                (cond ((string= prefix "vdg")
                       (list
                        (concat "(define-vdg-\\(call-\\|lambda-\\)?node (?"
                                fullname-re tag-end-re)
                        (concat "(define-vdg:primop-accessors "
                                structure-name-re tag-end-re)))
                      ((string= prefix "pdg")
                       (list
                        (concat "(define-pdg-structure (?" fullname-re tag-end-re)))
                      ((string= prefix "cfg")
                       (list
                        (concat "(define-cfg-structure (?" fullname-re tag-end-re)))
                      ((string= prefix "vtype")
                       (list
                        (concat "(define-vtype (?" fullname-re tag-end-re)))
                      ((string= prefix "source")
                       (list
                        (concat "(define-source-node (?" fullname-re tag-end-re)
                        (concat "(define-vdg:primop-accessors "
                                structure-name-re tag-end-re)))
                      ;; ((or (string= prefix "etext")
                      ;;      (string= prefix "source")
                      ;;      (string= prefix "portinst"))
                      ;;  (list
                      ;;   (concat "(define-etext-accessors " structure-name-re)))
                      (t
                       '()))
              '())
            (list
             (concat "(define-disjoint-type (?" structure-name-re)))))
    ;; Put zaphod-node-names first as they're more specific
    (nconc zaphod-node-names structure-names)))

;; (structure-name->define-structure-line "foo" "vdg")
;; (structure-name->define-structure-line "if" "etext")

;; (mit-scheme-tags-find-related-names "make-foo-bar")
;; (mit-scheme-tags-find-related-names "copy-foo-bar")
;; (mit-scheme-tags-find-related-names "foo-bar?")
;; (mit-scheme-tags-find-related-names "set-foo-bar-baz-bum!")
;; (mit-scheme-tags-find-related-names "foo-bar-baz-bum")

;; (mit-scheme-tags-find-related-names "pref:foo?")
;; (mit-scheme-tags-find-related-names "vdg:port?")
;; (mit-scheme-tags-find-related-names "vdg:port?")

;; (mit-scheme-tags-find-related-names "vdg:construct-foo-node")

;; (mit-scheme-tags-find-related-names "vdg:node-color")

;; (mit-scheme-tags-find-related-names "vdg:make-formal")
;; (mit-scheme-tags-find-related-names "source:make-formal")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tags-loop-continue
;;;

(define-key esc-map "," 'mde-tags-loop-continue) ; was tags-loop-continue

;; This modification, plus the setq of this-command in find-tag, makes M-,
;; continue a M-. as it did in Emacs 18, but only if the M-. was the
;; immediately preceding command.
(defun mde-tags-loop-continue (&optional first-time)
  "Continue last \\[tags-search], \\[tags-query-replace], or \\[find-tag] command.
A \\[find-tag] command is continued only if it was the previous command.
Used noninteractively with non-nil argument to begin such a command.
Two variables control the processing we do on each file:
the value of `tags-loop-scan' is a form to be executed on each file
to see if it is interesting (it returns non-nil if so)
and `tags-loop-operate' is a form to execute to operate on an interesting file
If the latter returns non-nil, we exit; otherwise we scan the next file."
  (interactive)
  (if (and (eq last-command 'find-tag) (not first-time))
      (progn
        (setq this-command 'find-tag)
        (find-tag nil t))
    (progn
      (setq this-command 'tags-loop-continue)
      (tags-loop-continue first-time))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; etags-tags-completion-table
;;;

(defun etags-tags-completion-table ()
  (let ((table (make-vector 511 0)))
    (save-excursion
      (goto-char (point-min))
      ;; This monster regexp matches an etags tag line.
      ;;   \1 is the string to match;
      ;;   \2 is not interesting;
      ;;   \3 is the guessed tag name; XXX guess should be better eg DEFUN
      ;;   \4 is not interesting;
      ;;   \5 is the explicitly-specified tag name.
      ;;   \6 is the line to start searching at;
      ;;   \7 is the char to start searching at.
      (while (re-search-forward
              "^\\(\\(.+[^-a-zA-Z0-9_$]+\\)?\\([-a-zA-Z0-9_$?:]+\\)\
\[^-a-zA-Z0-9_$?:]*\\)\177\\(\\([^\n\001]+\\)\001\\)?\
\\([0-9]+\\)?,\\([0-9]+\\)?\n"
              nil t)
        (intern (if (match-beginning 5)
                    ;; There is an explicit tag name.
                    (buffer-substring (match-beginning 5) (match-end 5))
                  ;; No explicit tag name.  Best guess.
                  (buffer-substring (match-beginning 3) (match-end 3)))
                table)))
    table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tags-replace
;;;

;; This function may not be a good idea if you have multiple active TAGS
;; tables.

(emacs-fsf
 (defun tags-replace (from to &optional delimited file-list-form ignore)
   "Replace-regexp FROM with TO through all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.

See documentation of variable `tags-file-name'."
   ;; This returns 5 forms, which is the reason for the "ignore" argument.
   (interactive (query-replace-read-args "Tags replace (regexp)" t))
   (setq tags-loop-scan (list 'prog1
                              (list 'if (list 're-search-forward
                                              (list 'quote from) nil t)
                                    ;; When we find a match, move back
                                    ;; to the beginning of it so perform-replace
                                    ;; will see it.
                                    '(goto-char (match-beginning 0))))
         tags-loop-operate (list 'progn
                                 (list 'replace-regexp
                                       (list 'quote from) (list 'quote to)
                                       (list 'quote delimited))
                                 ;; the loop is exited if nil is returned
                                 t))
   (tags-loop-continue (or file-list-form t))))

(xemacs
 (defun tags-replace (from to &optional delimited file-list-form)
  "Query-replace-regexp FROM with TO through all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.

See documentation of variable `tag-table-alist'."
  (interactive
   "sTags replace (regexp): \nsTags replace %s by: \nP")
  (setq tags-loop-scan `(with-caps-disable-folding ,from
                          (if (re-search-forward ,from nil t)
                              ;; When we find a match, move back
                              ;; to the beginning of it so perform-replace
                              ;; will see it.
                              (progn (goto-char (match-beginning 0)) t)))
        tags-loop-operate (list 'progn
                                 (list 'replace-regexp
                                       from to (not (null delimited)))
                                 ;; the loop is exited if nil is returned
                                 t))
   (tags-loop-continue (or file-list-form t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tags-verify-table
;;;

(xemacs
 (setq tags-auto-read-changed-tag-files t))

;; This is lifted out of Emacs 19.34, but the yes-or-no-p always returns true.
(defun tags-verify-table (file)
  "Read FILE into a buffer and verify that it is a valid tags table.
Sets the current buffer to one visiting FILE (if it exists).
Returns non-nil iff it is a valid table."
  (if (get-file-buffer file)
      ;; The file is already in a buffer.  Check for the visited file
      ;; having changed since we last used it.
      (let (win)
        (set-buffer (get-file-buffer file))
        (setq win (or verify-tags-table-function (initialize-new-tags-table)))
        (if (or (verify-visited-file-modtime (current-buffer))
                (and nil ;; added by MDE
                (not (yes-or-no-p
                      (format "Tags file %s has changed, read new contents? "
                              file)))))
            (and win (funcall verify-tags-table-function))
          (revert-buffer t t)
          (initialize-new-tags-table)))
    (and (file-exists-p file)
         (progn
           (set-buffer (find-file-noselect file))
           (or (string= file buffer-file-name)
               ;; find-file-noselect has changed the file name.
               ;; Propagate the change to tags-file-name and tags-table-list.
               (let ((tail (member file tags-table-list)))
                 (if tail
                     (setcar tail buffer-file-name))
                 (if (eq file tags-file-name)
                     (setq tags-file-name buffer-file-name))))
           (initialize-new-tags-table)))))

(provide 'etags-mde)

;;; etags-mde.el ends here
