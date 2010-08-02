;;;; mailto-compose-mail.el (2009-08-08)

;; From http://www.emacswiki.org/emacs/MailtoHandler , 31 July 2010

;;;###autoload
(defun mailto-compose-mail (mailto-url)
  "Parse MAILTO-URL and start composing mail."
  (if (and (stringp mailto-url)
           (string-match "\\`mailto:" mailto-url))
      (progn
        (require 'rfc2368)
        (require 'rfc2047)
        (require 'mailheader)

        (let ((hdr-alist (rfc2368-parse-mailto-url mailto-url))
              (allowed-xtra-hdrs '(cc bcc in-reply-to))
              to subject body)

          (with-temp-buffer
            (dolist (hdr hdr-alist)
              (insert (concat (car hdr) ": " (cdr hdr) "\n")))
            (rfc2047-decode-region (point-min) (point-max))
            (goto-char (point-min))
            (setq hdr-alist (mail-header-extract-no-properties)))

          (setq to (or (cdr (assq 'to hdr-alist)) "")
                subject (or (cdr (assq 'subject hdr-alist)) "")
                body (or (cdr (assq 'body hdr-alist)) "")
                hdr-alist
                (remove nil (mapcar
                             (lambda (item)
                               (when (memq (car item) allowed-xtra-hdrs)
                                 (cons (capitalize (symbol-name (car item)))
                                       (cdr item))))
                             hdr-alist)))

          (compose-mail to subject hdr-alist nil nil
                        (list (lambda (&rest args)
                                (insert (format "%s\n" (car args))))
                              body))))
    (compose-mail)))
