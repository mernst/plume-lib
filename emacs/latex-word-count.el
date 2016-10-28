;;; conf-wc.el -- Word counting for conference submissions.
;;; Michael Ernst <mernst@research.microsoft.com>
;;; Last modified 10/94


(defun conference-wc ()
  "Count words in conference submission.
Strips off TeX commands, comments, figures, etc."
  (interactive)
  (let ((full-buffer (buffer-string)))
    (set-buffer (get-buffer-create "conference-wc"))
    (erase-buffer)
    (insert full-buffer)

    ;; Kill comment
    (delete-tex-comments)

    ;; Kill figure
    (goto-char (point-min))
    (while (search-forward "\\begin{figure}" nil t)
      (let ((fig-begin (match-beginning 0)))
        (search-forward "\\end{figure}")
        (delete-region fig-begin (point))))

    ;; Kill \def
    (goto-char (point-min))
    (while (search-forward "\\def\\" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (kill-sexp 1)
      (if (= ?# (char-after (point)))
          (kill-sexp 1))
      (kill-sexp 1))

    ;; Kill TeX command and argument
    (delete-all-tex-command-string "\\newcommand" 3)
    (delete-all-tex-command-string "\\renewcommand" 2)
    (delete-all-tex-command-string "\\comment" 1)
    (delete-all-tex-command-regexp "~?\\\\\\(page\\|fig\\|sec\\)?ref" 1)
    (delete-all-tex-command-regexp "~?\\\\cite" 1)
    (delete-all-tex-command-regexp "~?\\\\cite\\[^]]\\]" 1)
    (delete-all-tex-command-string "\\setlength" 2)
    (delete-all-tex-command-string "\\begin" 1)
    (delete-all-tex-command-string "\\end" 1)
    (delete-all-tex-command-string "\\title" 1)
    (delete-all-tex-command-string "\\author" 1)
    (delete-all-tex-command-string "\\typeout" 1)
    (delete-all-tex-command-string "\\label" 1)
    (delete-all-tex-command-regexp "\\\\documentstyle\\(\\[[^]]*\\]\\)?" 1)
    (delete-all-tex-command-string "\\bibliographystyle" 1)
    (delete-all-tex-command-string "\\bibliography" 1)
    (delete-all-tex-command-string "\\input" 1)
    (delete-all-tex-command-string "\\hyphenation" 1)
    (delete-all-tex-command-string "\\newif" 2)


    ;; Kill TeX command
    (delete-all-tex-command-string "\\makeatletter" nil)
    (delete-all-tex-command-string "\\makeatother" nil)
    (delete-all-tex-command-string "\\maketitle" nil)
    (delete-all-tex-command-regexp "\\\\\\(sub\\)*section\\*?" nil)
    (delete-all-tex-command-string "\\tt" nil)
    (delete-all-tex-command-string "\\emergencystretch" nil)
    (delete-all-tex-command-string "\\em" nil)
    (delete-all-tex-command-string "\\/" nil)
    (delete-all-tex-command-string "\\boldcomments" nil)
    (delete-all-tex-command-string "\\footnotesize" nil)
    (delete-all-tex-command-string "$\\!$" nil) ; $\!$
    (delete-all-tex-command-string "\\parskip" nil)
    (delete-all-tex-command-string "\\itemsep" nil)
    (delete-all-tex-command-string "\\item" nil)
    (delete-all-tex-command-string "\\it" nil)
    (delete-all-tex-command-string "\\rm" nil)
    (delete-all-tex-command-string "\\sf" nil)
    (delete-all-tex-command-string "\\smallcaptions" nil)
    (delete-all-tex-command-string "\\small" nil)
    (delete-all-tex-command-string "\\linewidth" nil)
    (delete-all-tex-command-string "\\divide" nil) ; should kill 3 arguments
    (delete-all-tex-command-string "\\verb" nil)
    (delete-all-tex-command-string "\\@" nil)
    (delete-all-tex-command-string "\\footnote" nil)
    (delete-all-tex-command-string "\\langle" nil)
    (delete-all-tex-command-string "\\rangle" nil)


    ;; Get rid of extra begin groups left by deletion of \em, \tt, etc.
    ;; wc counts these as words!
    (goto-char (point-min))
    (while (search-forward "{ " nil t)
      (delete-backward-char 2))
    (goto-char (point-min))
    (while (search-forward "{\n" nil t)
      (delete-backward-char 2))

    (goto-char (point-min))
    (while (re-search-forward "\\s ,\\s " nil t)
      (delete-backward-char 2)
      (backward-char 1))

    ;; Kill comment, again (for those that used to follow a TeX command).
    (delete-tex-comments)

    (shell-command-on-region (point-min) (point-max) "wc")
    (kill-buffer (current-buffer))
    ))

(defun delete-all-tex-command-string (string &optional argp)
  (goto-char (point-min))
  (if (and argp (> argp 0))
      (let ((full-string (concat string "{")))
        (while (search-forward full-string nil t)
          (delete-region (match-beginning 0) (1- (match-end 0)))
          (backward-char 1)
          (kill-sexp argp)))
    (while (search-forward string nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun delete-all-tex-command-regexp (regexp &optional argp)
  (goto-char (point-min))
  (if (and argp (> argp 0))
      (let ((full-regexp (concat regexp "{")))
        (while (re-search-forward full-regexp nil t)
          (delete-region (match-beginning 0) (1- (match-end 0)))
          (backward-char 1)
          (kill-sexp argp)))
    (while (re-search-forward regexp nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun delete-tex-comments ()
  (goto-char (point-min))
  (while (= (char-after (point-min)) ?%)
    (kill-line 1))
  ;; Faster non-regexp search
  (while (search-forward "\n%" nil t)
    (backward-char 1)
    (kill-line 1)
    (backward-char 1))
  (goto-char (point-min))
  (while (re-search-forward "^\\s \\s *%" nil t)
    (beginning-of-line nil)
    (kill-line 1)))
