;;; -*- Mode:Emacs-Lisp -*-

;;; "hide-copyleft.el" by Jamie Zawinski <jwz@lucid.com>, 19-jan-91.
;;; Last modified 16-mar-91.
;;;
;;; I sometimes find it tiresome to have fifteen lines of copyright notice at
;;; the beginning of each file.  Meta-< does not take you to the beginning of
;;; the code, it takes you a windowfull or two away, which can be tedious on
;;; slow terminal lines.
;;;
;;; I know what the copyright notice says; so this code makes all but the first
;;; line of it be invisible, by using Emacs's selective-display feature.  The
;;; text is still present and unmodified, but it is invisible.
;;;
;;; Elide the copyright notice with "Meta-X hide-copyleft-region".  Make it
;;; visible again with "Control-U Meta-X hide-copyleft-region".  Or, if you're
;;; sure you're not gonna get sued, you can do something like this in your
;;; .emacs file:
;;;
;;;            (autoload 'hide-copyleft-region "hide-copyleft" nil t)
;;;            (autoload 'unhide-copyleft-region "hide-copyleft" nil t)
;;;            (add-hook 'emacs-lisp-mode-hook 'hide-copyleft-region)
;;;            (add-hook 'c-mode-hook 'hide-copyleft-region)
;;;
;;; This code (obviously) has quite specific knowledge of the wording of the
;;; various copyrights I've run across.  Let me know if you find one on which
;;; it fails.

(defvar copylefts-to-hide
  ;; There are some extra backslashes in these strings to prevent this code
  ;; from matching the definition of this list as the copyright notice!
  '(;; GNU
    ("free software\; you can redistribute it" .
     "notice must be preserved on all")
    ("free software\; you can redistribute it" .
     "copy of the GNU General Public License.*\n?.*\n?.*\n?.*\n?.*02139,")
    ("free software\; you can redistribute it" .   ;; added by MDE, 3/1/97
     "copy of the GNU Library General Public.*\n?.*\n?.*\n?.*\n?.*02139,")
    ("free software\; you can redistribute it" .   ;; added by MDE, 3/13/97
     "copy of the GNU General Public.*\n?.*\n?.*\n?.*02111-1307, USA.")
    ("distributed in the hope that it will be useful\," .
     "notice must be preserved on all")
    ("free software\; you can redistribute it" .
     "General Public License for more details\\.")
    ;; X11
    ("Permission to use\, copy, modify," .
     "CONNECTION WITH THE USE OR PERFORMANCE")
    ("Permission to use\, copy, modify," .
     "without express or implied warranty")
    ;; Motif
    ("Copyright.*OPEN\ SOFTWARE FOUNDATION" .
     "X Window System is a trademark of the")
    ("THIS SOFTWARE\ IS FURNISHED UNDER A LICENSE" .
     "X Window System is a trademark of the")
    ;; UPenn
    ("Permission to use\, copy, and distribute" .
     " provided \"as is\" without")
    ;; Evans & Sutherland, Solbourne.
    ("Copyright 19[0-9][0-9] by " .
     "OR PERFORMANCE OF THIS SOFTWARE\\.")
    ("^%%BeginDocumentation" . "^%%EndDocumentation")
    )
  "An alist of pairs of regexps which delimit copyright notices to hide.
The first one found is hidden, so order is significant.")


(defun hide-copyleft-region (&optional arg)
  "Make the legal drivel at the front of this file invisible.  Unhide it again
with C-u \\[hide-copyleft-region]."
  (interactive "P")
  (if arg
      (unhide-copyleft-region)
    (save-excursion
     (save-restriction
      (if selective-display (error "selective-display is already on."))
      (catch 'Abort
	(let ((mod-p (buffer-modified-p))
	      (buffer-read-only nil)
	      (rest copylefts-to-hide)
	      pair start end max)
	  (widen)
	  (goto-char (point-min))
	  (while (and rest (not pair))
	    (save-excursion
	      (and (re-search-forward (car (car rest)) nil t)
		   (setq start (point))
		   ;; Set limit to avoid expensive searches, but mostly
		   ;; in case there are two copyleft notices here:  don't
		   ;; hide everything between the start of the first and
		   ;; the end of the second.
		   (re-search-forward (cdr (car rest)) (+ start 1000) t)
		   (setq end (point)
			 pair (car rest))))
	    (setq rest (cdr rest)))
	  (or pair
	      (if (interactive-p)
		  (error "Couldn't find a CopyLeft to hide.")
		(throw 'Abort nil)))
	  (goto-char end)
	  (forward-line 1)
	  ;; If the last line of the notice closes a C comment, don't
	  ;; hide that line (to avoid confusion...)
	  (if (save-excursion (forward-char -3) (looking-at "\\*/"))
	      (forward-line -1))
	  (setq end (point))
	  (goto-char start)
	  (forward-line 1)
	  (while (< (point) end)
	    (delete-char -1)
	    (insert "\^M")
	    (forward-line 1))
	  (setq selective-display t)
	  (set-buffer-modified-p mod-p)))))))

(defun unhide-copyleft-region ()
  "If the legal nonsense at the top of this file is elided, make it visible again."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((mod-p (buffer-modified-p))
	    (buffer-read-only nil)
	    end)
	(or (search-forward "\^M" nil t) (error "Nothing hidden here, dude."))
	(end-of-line)
	(setq end (point))
	(beginning-of-line)
	(while (search-forward "\^M" end t)
	  (delete-char -1)
	  (insert "\^J"))
	(set-buffer-modified-p mod-p)
	(setq selective-display nil)))))
