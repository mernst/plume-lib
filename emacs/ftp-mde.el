;;; ftp-mde.el --- utility functions for accessing ftp and www sites

(defadvice browse-url-url-at-point (around maybe-file activate)
  "Replacement for stock `browse-url-url-at-point': perhaps return file: URL."
  (let ((url (let ((file (thing-at-point 'filename)))
               ;; symbolic links in dired buffers:  "index.html -> j3TOC.html"
               (if (and file (string-match "^\\(.*\\) -> \\(.*\\)$" file))
                   (setq file (match-string 1 file)))
               (if (and file
                        (not (zerop (length file)))
                        (file-exists-p file))
                   (concat "file:" (expand-file-name file))
                 (thing-at-point 'url)))))
    (set-text-properties 0 (length url) nil url)
    (setq ad-return-value url)))

(eval-when-compile
  (require 'thingatpt))

(defun thing-at-point-file-name-forward ()
  (if (equal major-mode 'dired-mode)
      (end-of-line)
    ;; original definition
    (skip-chars-forward thing-at-point-file-name-chars)))
(defun thing-at-point-file-name-backward ()
  (if (equal major-mode 'dired-mode)
      (dired-next-line 0)
    ;; original definition
    (skip-chars-backward thing-at-point-file-name-chars)))

(eval-after-load "thingatpt"
  '(put 'filename 'end-op 'thing-at-point-file-name-forward))
(eval-after-load "thingatpt"
  '(put 'filename 'beginning-op 'thing-at-point-file-name-backward))


(defun quote-url (url)
  "Return a HTML-quoted version of URL."
  (while (string-match ":" url)
    (setq url (replace-match "%3A" t t url)))
  (while (string-match "/" url)
    (setq url (replace-match "%2F" t t url)))
  url)
;; Testing
;; (quote-url "http://cf-web.mit.edu/cystic-l/94q3")

(eval-when-compile (require 'browse-url))

(setq browse-url-netscape-program "firefox")

;; "foo bar \"baz bum\"" turns into:
;; http://www.altavista.com/cgi-bin/query?pg=q&text=yes&kl=XX&q=foo+bar+%22baz+bum%22&act=search
;; "foo bar \"baz b+um\"" turns into:
;; http://www.altavista.com/cgi-bin/query?pg=q&text=yes&kl=XX&q=foo+bar+%22baz+b%2Bum%22&act=search
(defun altavista-quote (phrase)
  (require 'dired)                      ; for dired-replace-in-string
  (setq phrase (dired-replace-in-string "\"" "%22" phrase))
  (setq phrase (dired-replace-in-string "#" "%23" phrase))
  (setq phrase (dired-replace-in-string "&" "%26" phrase))
  (setq phrase (dired-replace-in-string "\\+" "%2B" phrase))

  ;; The ordering of the below needs to avoid double-substitution.
  ;; Not required for Google, though it was required for Altavista
  ;; (setq phrase (dired-replace-in-string " " "+" phrase))
  (setq phrase (dired-replace-in-string "\n" " " phrase))
  phrase)

;; "foo bar \"baz bum\"" turns into:
;; http://scholar.google.com/scholar?q=foo+bar+%22baz+bum%22&ie=UTF-8&oe=UTF-8&hl=en&btnG=Search
;; "foo bar \"baz b+um\"" turns into:
;; http://scholar.google.com/scholar?hl=en&lr=&q=foo+bar+%22baz+b%2Bum%22&btnG=Search
(defun google-search (phrase &optional searchtype)
  "Search using Google."
  (interactive "sSearch via Google: ")
  (setq searchtype (or searchtype "search"))
  (require 'browse-url)
  (browse-url
   (concat "http://www.google.com/" searchtype "?q="
           (altavista-quote phrase)
           "&lr=lang_en&num=100"
           (if current-prefix-arg
               "&btnI=Lucky"
             ""))))

(defun wikipedia-lookup (phrase &optional searchtype)
  "Search using Wikipedia."
  (interactive "sSearch via Wikipedia: ")
  (require 'browse-url)
  (browse-url
   (concat "http://en.wikipedia.org/wiki/"
           (altavista-quote phrase))))

(defun wayback-machine (url)
  "Look up a URL in the Internet Archive Wayback Machine."
  (interactive "sWayback Machine: ")
  (require 'browse-url)
  (browse-url (concat "http://waybackmachine.org/*/"
                      (altavista-quote url))))

(defun scholar-google (phrase)
  "Search using Google Scholar."
  (interactive "sSearch via Google Scholar: ")
  (google-search phrase "scholar"))

(autoload 'browse-url-interactive-arg "browse-url")

(defun wget (url &optional x)
  "Call wget on the URL, downloading the file to the current directory.
Second argument is ignored but permits use of `browse-url-interactive-arg'."
  (interactive (browse-url-interactive-arg "wget URL: "))
  (shell-command (concat "wget '" url "'")))


(provide 'ftp-mde)

;;; ftp-mde.el ends here
