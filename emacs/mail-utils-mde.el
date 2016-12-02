;;; Header-manipulation functions useful both in sending and receiving mail.

(eval-when-compile '(require 'sendmail))

;;;
;;; Headers
;;;

(defun mail-field-extent (field)
  "Return nil or a cons of (start . end) for the field.
Start is at the beginning of a line and end is at the end of a line."
  (save-excursion
    (and (mail-position-on-field field t)
         (let ((end (point)))
           (re-search-backward "^[^ \t]" nil 'move)
           (cons (point) end)))))

(defun mail-delete-field (field)
  (let ((region (mail-field-extent field)))
    (if region
        (delete-region (car region) (1+ (cdr region))))))

(defun mail-make-field-invisible (field)
  (let ((region (mail-field-extent field)))
    (if region
        (put-text-property (car region) (1+ (cdr region)) 'invisible t))))


;; Could use mail-position-on-field in the following two functions, but it
;; does extra work when we know the field exists and don't need to find end
;; of mail headers, etc.  Also, mail-position-on-field requires that
;; mail-header-separator be found, so it isn't useful on messages being
;; read, only those being composed.
(defun mail-remove-field (field)
  (save-match-data
    (if (let ((case-fold-search t)
              (header-end (rmail-header-end)))
          (goto-char (point-min))
          (re-search-forward (concat "^" (regexp-quote field) ":") header-end t))
        (progn
          (beginning-of-line)
          (delete-region (point)
                         (progn (re-search-forward "\n[^ \t]")
                                (forward-char -1)
                                (point)))))))

(defun mail-set-field (field text &optional add)
  "If optional argument ADD is non-nil, the field is added if it did not
previously exist.  Otherwise, if it did not exist, no action is taken."
  (save-match-data
    (if (or (let ((case-fold-search t)
                  (header-end (rmail-header-end)))
              (goto-char (point-min))
              (re-search-forward (concat "^" (regexp-quote field) ":") header-end t))
            (and add
                 (progn
                   (goto-char (point-min))
                   (insert field ": \n")
                   (backward-char 2)
                   t)))
        ;; point is not just after the colon and before a space or tab
        (progn
          (forward-char 1)
          (delete-region (point)
                         (progn (re-search-forward "\n[^ \t]")
                                (forward-char -2)
                                (point)))
          (while (string-match "\n[^ \t]" text)
            (setq text (concat (substring text 0 (1+ (match-beginning 0)))
                               "\t"
                               (substring text (1- (match-end 0))))))
          (insert text)))))

;; `mail-header-end' doesn't work in an RMAIL buffer, because it widens and
;; then goes to the beginning of the buffer.  It does work well in a *mail*
;; buffer, however.  But this works in that case, too.
(defun rmail-header-end ()
  (save-excursion
    (goto-char (point-min))
    (rfc822-goto-eoh)
    (point)))

;;;

(provide 'mail-utils-mde)
