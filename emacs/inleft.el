;;; *********************************************
;;; INLEFT.EL
;;; Originally from Bard Bloom.
;;; Last changed by Michael Ernst, mernst@theory.lcs.mit.edu, 2/6/91.
;;; Next-line changed to forward-line, 10/6/94.

;; This file defines two commands, `inleft' and `uncomment-region'.

;; Another way to do inleft is C-x n M-x replace-regexp RET ^ RET > RET C-x w
;; (narrow to region, replace beginning of line by >, widen), but I can never
;; remember that.

;; To use this file, add the following lines to your .emacs file:
;;   (make-variable-buffer-local 'inleft-string)
;;   (autoload 'inleft "~/emacs/inleft" "Comment-out-like utility." t)
;;   (autoload 'inleft-internal "~/emacs/inleft" "Comment-out-like utility." t)
;;   (autoload 'uncomment-region "~/emacs/inleft" "Uncomment-out-like utility." t)

;; You may want to set inleft-string in TeX-mode-hook, LaTeX-mode-hook,
;; texinfo-mode-hook, lisp-mode-hook, and elsewhere that "> " is not the
;; best default.

;; This first form may be unnecessary (or maybe the third one is).
(defvar inleft-string "> " "Default string for inleft.")
(make-variable-buffer-local 'inleft-string)
(setq-default inleft-string "> ")

(provide 'inleft)

(global-set-key "\C-c>" 'inleft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implementation 1

(defun inleft (&optional beg end)
  "Inserts a string at the beginning of each line in the region, and
moves to end of region.  Prompts for string, and remembers it in
the buffer-local variable `inleft-string'."
  (interactive "r")
  (setq inleft-string (read-string "Inleft String:" inleft-string))
  (inleft-internal inleft-string beg end))

(defun inleft-internal (left-string &optional beg end)
  "Inserts LEFT-STRING at the beginning of each line in the region, and
moves to end of region.  Not to be called interactively."
  (save-excursion
    (save-restriction
      ;; Should I instead use the beginning of the line containing beg,
      ;; and the end of the one containing end, or the beginning of the next?
      (narrow-to-region (or beg (point-marker))
                        (or end (mark-marker)))
      (goto-char (point-min))
      (while (and (<= (point-marker) (point-max)) (not (eobp)))
        (beginning-of-line 1)
        (insert-before-markers left-string)
        (forward-line 1)))))

;; Perhaps outleft would be a better name.
;; This is nicer than kill-rectangle in that it checks what it's deleting.
;; Bug: this advances point by one line.
(defun uncomment-region ()
  "Deletes a string from the beginning of each line in the region.
Prompts for string, and remembers it in the variable `inleft-string'."
  (interactive)
  (setq inleft-string (read-string "Uncomment String:" inleft-string))
  (let ((inleft-string-len (length inleft-string))
        (p (point-marker))
        (m (mark-marker)))
    (if (< m p) (let ((thrip m))  ; swap m and p
                  (setq m p p thrip)))
    (goto-char p)
    (while (and (<= (point-marker) m) (not (eobp)))
      (beginning-of-line 1)
      (if (string= inleft-string
                   (buffer-substring (point) (+ (point) inleft-string-len)))
          (delete-char inleft-string-len))
      (forward-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Implementation 2
;;
;; ;; This implementation doesn't respect undo-boundaries, which is Bad.
;;
;; ;; The problem with using an interactive argument to get the inleft-string
;; ;; is that no default is provided with interactive "s".
;;
;; (defmacro point-after (&rest commands)
;;   "Returns the value of point after executing the COMMANDS.  Doesn't move
;; point.  (Expands to (save-excursion COMMANDS (point)))."
;;   `(save-excursion
;;        ,@commands
;;        (point)))
;;
;; (defun inleft (beg end)
;;   "Inserts a string at the beginning of each line in the region.
;; Prompts for string, remembers it in the buffer-local variable inleft-string."
;;   (interactive "r")
;;   (setq inleft-string (read-string "Inleft String:" inleft-string))
;;   (inleft-internal inleft-string beg end))
;;
;; (defun inleft-internal (inleft-string &optional beg end)
;;   "Inserts INLEFT-STRING string at the beginning of each line in the region.
;; If BEG and END are specified, they delimit the region to be inlefted.
;; For use by programs that don't want to see the prompt for inleft-string."
;;   (if (not (and beg end))
;;       (setq beg (min (point-marker) (mark-marker))
;;          end (min (point-marker) (mark-marker))))
;;   (save-restriction
;;     (narrow-to-region (point-after (goto-char beg) (beginning-of-line 1))
;;                       end)
;;     (goto-char (point-min))
;;     (perform-replace "^" inleft-string nil t nil)))
;;
;; (defun uncomment-region (beg end)
;;    "Deletes a string from the beginning of each line in the region.
;; Prompts for string, remembers it in the buffer-local variable inleft-string."
;;   (interactive "r")
;;   (setq inleft-string (read-string "Uncomment String:" inleft-string))
;;
;;   (save-restriction
;;     (narrow-to-region (point-after (goto-char beg) (beginning-of-line 1))
;;                       end)
;;     (goto-char (point-min))
;;     (perform-replace (concat "^" (regexp-quote inleft-string)) "" nil t nil)))
