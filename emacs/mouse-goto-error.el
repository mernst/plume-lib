;;; mouse-goto-error.el --- Make middle-click on error go to that error

;; Defines mouse-yank-or-goto-error, and binds it to the middle mouse button.
;; Causes the middle mouse button to have the following behavior:
;;  * If an error message is under point, go to the error.
;;    Otherwise, call `mouse-yank-at-click'.


;;;
;;; Cross-version compatibility
;;;

(defvar running-xemacs (featurep 'xemacs))
(defvar running-emacs-fsf (not (featurep 'xemacs)))
(defmacro emacs-fsf (&rest body)
  "Execute BODY if running (FSF) Emacs."
  `(if running-emacs-fsf
         (progn ,@body)))
(defmacro xemacs (&rest body)
  "Execute BODY if running XEmacs."
  `(if running-xemacs
         (progn ,@body)))


;;;
;;; Installation in shell mode
;;;


(emacs-fsf
 ;; In XEmacs, this attaches itself to button2up, not button2.  Thus, both
 ;; button2 (bound to mouse-yank) and this get run.
 (eval-after-load "shell"
   '(define-key shell-mode-map [mouse-2] 'mouse-yank-or-goto-error))
 (eval-after-load "gud"
   '(define-key gud-mode-map [mouse-2] 'mouse-yank-or-goto-error))
 (eval-after-load "compile"
   '(progn
      (define-key compilation-mode-map [mouse-2] 'mouse-yank-or-goto-error)
      (if (boundp 'compilation-button-map)
          (define-key compilation-button-map [mouse-2] 'mouse-yank-or-goto-error))))
 ;; There is no fundamental-mode-map; want this for "*Shell Command Output*" buf
 (define-key global-map [mouse-2] 'mouse-yank-or-goto-error))
(xemacs
 (eval-after-load "shell"
   '(define-key shell-mode-map [button2] 'mouse-yank-or-goto-error))
 (eval-after-load "gud"
   '(define-key gud-mode-map [button2] 'mouse-yank-or-goto-error))
 (eval-after-load "compile"
   '(progn
      (define-key compilation-mode-map [button2] 'mouse-yank-or-goto-error)
      (define-key compilation-button-map [button2] 'mouse-yank-or-goto-error)))
 ;; There is no fundamental-mode-map; want this for "*Shell Command Output*" buf
 (define-key global-map [button2] 'mouse-yank-or-goto-error))


;;;
;;; Code
;;;

;;; Should abstract out more of this.
;; As of Emacs 22, this refactoring seems to have occurred.
;; ;; Parts are lifted from compile-[mouse-]goto-error (should try to merge
;; ;; some of this back in eventually; in any event, there should be a
;; ;; separate goto-error-at-point function).
(defun mouse-yank-or-goto-error (click arg)
  "If an error message is under point, go to the error.  Otherwise,
call `mouse-yank-at-click'.
Arguments CLICK and ARG are as for `mouse-yank-at-click'.
In many cases -- for instance, in compilation buffers -- using `next-error'
is easier than clicking on the error."
  (interactive "@e\nP")
  (let ((click-point (or (emacs-fsf (posn-point (event-end click)))
                         (xemacs (event-point click))
                         (error "Where is the click?")))
        file line)
    (if (save-excursion
          (goto-char click-point)
          (beginning-of-line)
          ;; Hack; should generalize
          (if (looking-at "[-.a-zA-Z0-9]*: In function `")
              (forward-line 1))
          (require 'compile)  ; for variable compilation-error-regexp-alist
          (let ((end-pos (line-end-position))
                (regexp-list compilation-error-regexp-alist))
            (while regexp-list
              (let ((regexp-info (car regexp-list)))
                (if (symbolp regexp-info)
                    (setq regexp-info
                          (cdr (assq regexp-info
                                     compilation-error-regexp-alist-alist))))
                (if (or (looking-at (car regexp-info))
                        ;; Check at preceding char, too:  many regexps start with "\n".
                        (and (not (bobp))
                             (progn (backward-char 1)
                                    (prog1 (looking-at (car regexp-info))
                                      (forward-char 1))))
                        ;; not all regexps start match at beginning of line,
                        ;; but don't only search: let those anchored span
                        ;; multiple lines
                        (re-search-forward (car regexp-info) end-pos t))
                    (progn
                      ;; (message "%s" (match-data))
                      (setq file (let ((raw (match-string (nth 1 regexp-info)))
                                       format)
                                   (if (>= emacs-major-version 22)
                                       (if (listp raw)
                                           (setq format (cadr raw)
                                                 ;; bug:  we only try the first format
                                                 raw (car raw)))
                                     (setq format (nth 4 regexp-info)))
                                   (if format
                                       (format format raw)
                                     raw))
                            line (let ((match-num (nth 2 regexp-info)))
                                   (if (consp match-num)
                                       (setq match-num (car match-num)))
                                   (let ((raw (match-string match-num)))
                                     (string-to-number
                                      ;; Sun's JDB line numbers have a comma!
                                      ;; (Or a period, depending on the locale.)
                                      (if (string-match "[.,]" raw)
                                          (replace-match "" nil nil raw)
                                        raw))))
                            ;; short-circuit testing
                            regexp-list nil))
                  (setq regexp-list (cdr regexp-list)))))
            ;; return non-nil if matched
            file))
      ;; Click was on an error/warning
      (let ((buffer (or (let ((abs-file (expand-file-name file)))
                          (and (file-exists-p abs-file)
                               (find-file-noselect abs-file)))
                        (get-buffer file)
                        (let ((completed (file-name-completion file default-directory)))
                          (and completed
                               (file-exists-p completed)
                               (find-file-noselect completed))))))
        ;; If we didn't find it yet, try looking in TAGS tables and directories.
        (if (not buffer)
            (setq buffer
                  (or (and tags-file-name
                           (find-in-tags-table file tags-file-name))
                      (let ((tags-files tags-table-list)
                            (result nil))
                        (while (and (not result) tags-files)
                          (setq result (find-in-tags-table file (car tags-files))
                                tags-files (cdr tags-files)))
                        result)
                      (find-in-tags-table file (concat default-directory "TAGS"))
                      ;; for Java
                      (save-excursion
                        (goto-char click-point)
                        (beginning-of-line)
                        ;; eg, "        at joie.code.BranchInstruction.getString(BranchInstruction.java:64)"

                        (and (looking-at "      at \\([^()]+\\.\\)?[^.]+\\.[^.]+([^()]+.java:[0-9]+)$")
                             ;; We dropped the last two components from the dotted operation name.
                             (let ((dir (or (match-string 1) "")))
                               (while (string-match "\\." dir)
                                 (setq dir (replace-match "/" t t dir)))
                               (require 'dired)
                               (let ((dir-plus-file (concat dir file))
                                     (java-dirs (and (getenv "CLASSPATH")
                                                     (split-string (getenv "CLASSPATH") ":")))
                                     (result nil))
                                 (while java-dirs
                                   (if (file-directory-p (car java-dirs))
                                       (let ((maybe-absname (concat
                                                             (file-name-as-directory (car java-dirs))
                                                             dir-plus-file)))
                                         (if (file-exists-p maybe-absname)
                                             (setq result
                                                   (find-file-noselect maybe-absname)
                                                   java-dirs nil))))
                                   (setq java-dirs (cdr java-dirs)))
                                 result)))))))
        (if (not buffer)
            (error "Can't find file %s" file))
        (pop-to-buffer buffer)
        (if (= line 0)
            (message "unknown line; not moving point")
          (goto-line line)))
      ;; Click was not on an error/warning
      (mouse-yank-at-click click arg))))


(eval-when-compile (require 'etags))

(defun find-in-tags-table (file tags-file)
  (if (file-exists-p tags-file)
      (let ((tags-buf (find-file-noselect tags-file)))
        (save-excursion
          (set-buffer tags-buf)
          (if (or (not (boundp 'tags-included-tables-function))
                  (not tags-included-tables-function))
              (progn
                (require 'etags)
                ;; Necessary lest we get errors if we ever
                ;; use this buffer as a tags table
                ;; (etags-recognize-tags-table)
                (initialize-new-tags-table)))
          (goto-char (point-min))
          (if (re-search-forward (concat "\f\n\\(.*/\\)?" (regexp-quote file)) nil t)
              ;; return the buffer
              (find-file-noselect
               (concat default-directory
                       (buffer-substring (point) (line-beginning-position)))))))))


;;;
;;; Java
;;;


;; A *very* primitive Java inspector.
;; I can't believe I haven't already implemented something like this.
;; Note that not all numbers following "@" are valid arguments to "dump",
;; so stick with those starting with "0x" for now.

(defvar hex-re "\\b0x[0-9a-fA-F]+\\b")

(defadvice mouse-yank-or-goto-error (around jdb-dump activate)
  "If click is on a hex number, dump a Java object.
Note the usual Emacs problem with the buffer being the wrong one when this
is invoked the first time; the jdb buffer needs to be current before using
this mouseclick."
  ;; arguments: (click arg)
  (let* ((click-advice (ad-get-arg 0))
         (click-point (or (emacs-fsf (posn-point (event-end click-advice)))
                          (xemacs (event-point click-advice))
                          (error "Where is the click?")))
         (hex-at-point
          (and (eq major-mode 'gud-mode)
               (boundp 'gud-find-file)
               (eq gud-find-file 'jde-db-find-file)
               (or (save-excursion
                     (goto-char click-point)
                     (if (re-search-backward "\\s-" nil t)
                         (progn
                           (forward-char 1)
                           (if (looking-at hex-re)
                               (match-string 0)))))
                   (save-excursion
                     (goto-char click-point)
                     (if (re-search-forward "\\s-" nil t)
                         (progn
                           (forward-char -1)
                           (if (looking-back hex-re)
                               (match-string 0)))))))))
    (if hex-at-point
        (let* ((command (concat "dump " hex-at-point))
               (prev-output
                (save-excursion
                  (goto-char (point-min))
                  (search-forward (concat "] " command "\n") nil t))))
          ;; If we already printed this value, go to that point rather
          ;; than cluttering the buffer with duplicate output.
          ;; (This could be a problem if some slot values have changed...)
          (if prev-output
              (goto-char prev-output)
            (progn
              (goto-char (point-max))
              (insert command)
              (comint-send-input))))
      ad-do-it)))


(provide 'mouse-goto-error)
