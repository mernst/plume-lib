;; darken-lighten-face.el


;; The default colors for font lock, and some other Emacs features, are too
;; bright and distracting.  This code lets you tone it down a bit.  Example
;; use:
;;
;; (require 'darken-lighten-face)
;;
;; (eval-after-load "font-lock"
;;   '(progn
;;      (darken-face-foreground 'font-lock-comment-face)
;;      (darken-face-foreground 'font-lock-string-face)
;;      (darken-face-foreground 'font-lock-keyword-face)
;;      (darken-face-foreground 'font-lock-type-face)
;;      (darken-face-foreground 'font-lock-constant-face)
;;      (darken-face-foreground 'font-lock-variable-name-face)
;;      ;; This darkens it too much; I would like more subtle darkening.
;;      ;; (darken-face-foreground 'font-lock-function-name-face)
;;      ;; Leave font-lock-warning-face as is; I want it bright.
;;      ))
;;
;; (eval-after-load "compile"
;;   '(darken-face-foreground 'compilation-warning)))
;;
;; (eval-after-load "font-latex"
;;   '(progn
;;      (darken-face-foreground 'font-latex-bold-face)
;;      (darken-face-foreground 'font-latex-italic-face)
;;      (darken-face-foreground 'font-latex-math-face)
;;      (darken-face-foreground 'font-latex-sedate-face)
;;      (darken-face-foreground 'font-latex-string-face)
;;      (darken-face-foreground 'font-latex-warning-face)
;;      (darken-face-foreground 'font-latex-verbatim-face)
;;      (darken-face-foreground 'font-latex-superscript-face)
;;      (darken-face-foreground 'font-latex-subscript-face)
;;      ;; (darken-face-foreground 'font-latex-slide-title-face)
;;      (darken-face-foreground 'font-latex-doctex-preprocessor-face)
;;      (darken-face-foreground 'font-latex-doctex-documentation-face)))



;;; Colors

;; If a face is defined simply as inheriting from some other face, then it
;; cannot be directly lightened or darkened.  For an example, see
;; dired-ignored in dired.el.

(defun lighten-face-foreground (face-symbol)
  "Lighten the RGB values for the foreground of the face."
  (if (not (facep face-symbol))
      (error "No known face %s" face-symbol))
  (let ((old-color (face-foreground face-symbol)))
    (if (not old-color)
        ;; This can happen with "emacs -nw"; I don't know why.
        ;; (error "No foreground color for face %s" face-symbol)
        nil
      (set-face-foreground face-symbol (lighten-rgb old-color)))))

(defun lighten-rgb (string)
  "Lighten each of the RGB values for the color."
  (let* ((rgb (color-values string))
         (lighter-rgb (mapcar #'lighten-color rgb))
         (lighter-string (rgb-to-hex-string lighter-rgb)))
    lighter-string))
;; (lighten-rgb "#FA5")
;; (lighten-rgb "purple")

(defun lighten-color (color-val)
  "Double the RGB value, effectively lightening it."
  ;; Max color-val is 65535 (= 2^16-1)
  (/ (+ color-val 65535) 2))

(defun darken-face-foreground (face-symbol)
  "Darken the RGB values for the foreground of the face."
  (if (not (facep face-symbol))
      (error "No known face %s" face-symbol))
  (let ((old-color (face-foreground face-symbol)))
    (if (not old-color)
        ;; This can happen with "emacs -nw"; I don't know why.
        ;; (error "No foreground color for face %s" face-symbol)
        nil
      (set-face-foreground face-symbol (darken-rgb old-color)))))

(defun darken-rgb (string)
  "Darken each of the RGB values for the color."
  (let* ((rgb (color-values string))
         (darker-rgb (mapcar #'darken-color rgb))
         (darker-string (rgb-to-hex-string darker-rgb)))
    darker-string))
;; (darken-rgb "#FA5")
;; (darken-rgb "purple")

(defun darken-color (color-val)
  "Halve the RGB value, effectively darkening it."
  ;; Max color-val is 65535 (= 2^16-1)
  (/ color-val 2))

(defun rgb-to-hex-string (clist)
  (format "#%04x%04x%04x"
          (round (first clist)) (round (second clist)) (round (third clist))))
;; (rgb-to-hex-string (color-values "purple"))
;; (rgb-to-hex-string (color-values "white"))
;; (rgb-to-hex-string (color-values "black"))



(provide 'darken-lighten-face)
