;; ffap-mde
;; Enhancements to ffap (find-file-at-point).

;; "If you use ange-ftp, browse-url, complete, efs, or w3, it is best to load
;; or autoload them before ffap.  If you use ff-paths, load it afterwards."
(require 'ffap)
(ffap-bindings)
;; ffap-url seems to only give me a menu of ways to fetch a url
;; (require 'ffap-url)
(defun set-ffap-require-prefix-true-locally ()
  "Set variable `ffap-require-prefix' true in this buffer only."
  (make-local-variable 'ffap-require-prefix)
  (setq ffap-require-prefix t))
(add-hook 'buffer-menu-mode-hook 'set-ffap-require-prefix-true-locally)
;; Actually I'd like ffap to work when I'm after the "->" of a symbolic link.
(add-hook 'dired-mode-hook 'set-ffap-require-prefix-true-locally)
(defadvice ffap-url-at-point (after no-mailto activate)
  "Don't return mailto: URLs."
  (if (and ad-return-value
           (string-match "^mailto:" ad-return-value))
      (setq ad-return-value nil)))
(defadvice ffap-machine-at-point (around disable activate)
  "Never give an ange-ftp pathname when on a machine name."
  nil)
(defadvice ffap-newsgroup-p (around disable activate)
  "Never try to read news."
  nil)
;; (setq ffap-machine-p-known 'accept)
;; Promote the "netscape" method, so it becomes the default for "http:":
(if (boundp 'ffap-url-methods)
    (let ((method (assoc "netscape" ffap-url-methods)))
      (and method
           (setq ffap-url-methods
                 (cons method (delete method ffap-url-methods))))))

;; Actually, just don't return URLs; I typically use a different keystroke.
;; This doesn't do what I want: http://www.cs.kun.nl is interpreted as a file!
;; (That is the fault of ffap-file-at-point.)
(setq ffap-url-regexp nil)              ; disable URL features in ffap

;; Redefine in order to recoginze "${HOME}/research"; see "ADDED BY MDE" comments.
(defun ffap-string-at-point (&optional mode)
  "Return a string of characters from around point.
MODE (defaults to value of `major-mode') is a symbol used to look up string
syntax parameters in `ffap-string-at-point-mode-alist'.
If MODE is not found, we use `file' instead of MODE.
If the region is active, return a string from the region.
Sets `ffap-string-at-point' and `ffap-string-at-point-region'."
  (let* ((args
          (cdr
           (or (assq (or mode major-mode) ffap-string-at-point-mode-alist)
               (assq 'file ffap-string-at-point-mode-alist))))
         (pt (point))
         (str
          (if (and transient-mark-mode mark-active)
              (buffer-substring
               (setcar ffap-string-at-point-region (region-beginning))
               (setcar (cdr ffap-string-at-point-region) (region-end)))
            (buffer-substring
             (save-excursion
               (skip-chars-backward (car args))
               ;; ADDED BY MDE
               (while (looking-back "\\${[a-zA-Z0-9_]+}/?")
                 (goto-char (match-beginning 0))
                 (skip-chars-backward (car args)))
               (skip-chars-forward (nth 1 args) pt)
               (setcar ffap-string-at-point-region (point)))
             (save-excursion
               (skip-chars-forward (car args))
               ;; ADDED BY MDE
               (while (and (looking-back "\\$")
                           (looking-at "{[a-zA-Z0-9_]+}"))
                 (goto-char (match-end 0))
                 (skip-chars-forward (car args)))
               (skip-chars-backward (nth 2 args) pt)
               (setcar (cdr ffap-string-at-point-region) (point)))))))
    (set-text-properties 0 (length str) nil str)
    (setq ffap-string-at-point str)))

;; This is a bit gross, because it should also adjust the variables that
;; ffap-string-at-point sets.
(defadvice ffap-string-at-point (after remove-html-markup activate)
  "Remove HTML markup."
  (if ad-return-value
      (progn
        (if (string-match "^tt>" ad-return-value)
            (setq ad-return-value (substring ad-return-value 3)))
        (if (string-match "</tt$" ad-return-value)
            (setq ad-return-value (substring ad-return-value 0 -4))))))

(setq ffap-string-at-point-mode-alist
      (cons
       ;; default:  (file "--:$+<>@-Z_a-z~" "<@" "@>;.,!?:")  I have
       ;; removed the colons.  This is good for finding specific elements
       ;; of paths, and also for compilation error messages like "file:lineno".
       '(file "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
       ffap-string-at-point-mode-alist))

;; I'll submit a bugfix for this someday.
(if (not (assq 'makefile-mode ffap-string-at-point-mode-alist))
    (setq ffap-string-at-point-mode-alist
          (cons
           ;; Makefiles:  allow environment variables in parentheses
           '(file "--:$+<>@-Z_[:lower:]~*?()" "<@(" "@>);.,!:")
           ffap-string-at-point-mode-alist)))

;; Redefine to move gopher after file, and to disallow URLs.
;; I'd rather have no guess than a guessed URL.
(eval-after-load 'ffap
  '(defun ffap-guesser nil
     "Return file or URL or nil, guessed from text around point."
     (or (and ffap-url-regexp
              (ffap-fixup-url (ffap-url-at-point)))
         (let ((file (ffap-file-at-point)))             ; may yield url!
           (if (and file (string-match "^/?https?:" file))
               nil
             file))
         (and ffap-url-regexp
              (ffap-fixup-url (ffap-gopher-at-point)))
         (ffap-fixup-machine (ffap-machine-at-point)))))


;; This doesn't work as of 19.34, because find-file-at-point checks whether
;; it was called interactively, but advice always makes a non-interactive call.
;; (defadvice find-file-at-point (around ffap-require-prefix-for-modes activate)
;;   "Set `ffap-require-prefix' to t for certain modes
;; \(currently Buffer-menu-mode and dired-mode\)."
;;   (let ((ffap-require-prefix (or (member major-mode '(Buffer-menu-mode
;;                                                    dired-mode))
;;                               ffap-require-prefix)))
;;     ad-do-it))


(setq ffap-alist
      (append ffap-alist
              '(("" . ffap-expand-semi-env-vars))))

(defvar ffap-semi-env-vars
  '()
  "At the beginning of a file name, these words act like environment variables;
   for example, \"annotations/README.txt\" could act like \"$anno/README.txt\".")

(defun ffap-expand-semi-env-vars (name)
  (let ((sevs ffap-semi-env-vars))
    (while sevs
      (let ((sev (car sevs)))
        (if (string-match (concat "^\\(?:\./\\)?" (car sev) "\\($\\|/.*\\)") name)
            (setq name (substitute-in-file-name (concat (cdr sev) (match-string 1 name)))
                  sevs nil)))
      (setq sevs (cdr sevs))))
  name)
;; (ffap-expand-semi-env-vars "annotations/README.txt")
;; (ffap-expand-semi-env-vars "annotations")

;;; Handle lines like these, by ignoring the leading "a/" or "b/":
;;; --- a/java/daikon/AnnotateNullable.java
;;; +++ b/java/daikon/AnnotateNullable.java
(defun ffap-strip-diff-prefix (name)
  (if (string-match "^[ab]/" name)
      (substring name 2)))
(setq ffap-alist
      (append ffap-alist
              '(("" . ffap-strip-diff-prefix))))


(require 'ff-paths)
