;; ffap-mde
;; Enhancements to ffap (find-file-at-point).

;; "If you use ange-ftp, browse-url, complete, efs, or w3, it is best to load
;; or autoload them before ffap.  If you use ff-paths, load it afterwards."
(require 'ffap)
(ffap-bindings)

(setq ffap-url-regexp nil)              ; don't match URLs

(defun set-ffap-require-prefix-true-locally ()
  "Set variable `ffap-require-prefix' true in this buffer only."
  (make-local-variable 'ffap-require-prefix)
  (setq ffap-require-prefix t))
(add-hook 'buffer-menu-mode-hook 'set-ffap-require-prefix-true-locally)
;; Actually I'd like ffap to work when I'm after the "->" of a symbolic link.
(add-hook 'dired-mode-hook 'set-ffap-require-prefix-true-locally)

;; Disable ffap for some things.
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
;; Don't return URLs; I typically use a different keystroke.
;; This doesn't do what I want: http://www.cs.kun.nl is interpreted as a file!
;; (That is the fault of ffap-file-at-point.)
(setq ffap-url-regexp nil)              ; disable URL features in ffap


;;; Ignore the leading "a/" or "b/" in lines like these:
;;;   --- a/java/daikon/AnnotateNullable.java
;;;   +++ b/java/daikon/AnnotateNullable.java
(defun ffap-strip-diff-prefix (name)
  (if (string-match "^[ab]/" name)
      (substring name 2)))
(setq ffap-alist
      (append ffap-alist
              '(("" . ffap-strip-diff-prefix))))


;;;
;;; String-at-point enhancements
;;;

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


;;;
;;; Semi-environment-variables
;;;

(setq ffap-alist
      (append ffap-alist
              '(("" . ffap-expand-semi-env-vars))))

(defvar ffap-semi-env-vars
  '()
  "At the beginning of a file name, these words act like environment variables;
   for example, \"annotations/README.txt\" could act like \"$anno/README.txt\".")

(defun ffap-expand-semi-env-vars (name)
  "Treat some prefixes like environment variables."
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



(require 'ff-paths)
