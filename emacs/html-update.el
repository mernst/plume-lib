;; To make Emacs automatically run html-update-toc on HTML files as you
;; save them, add to your .emacs file:
;;   (load-library "html-update")

(autoload 'bdiff-revert-buffer-maybe "bdiff")

(defun update-html-mode-hook ()
  (add-hook 'after-save-hook 'run-html-update-toc nil 'local))
(add-hook 'html-mode-hook 'update-html-mode-hook)

(defun run-html-update-toc ()
  "Run external program html-update-toc on the file."
  (interactive)
  (if (save-excursion
        (goto-char (point-min))
        (re-search-forward "<!-- start \\(toc\\|contents\\)" nil t))
      (progn
        ;; I would like to avoid the "(Shell command succeeded with no output)"
        ;; message.
        (shell-command (concat "html-update-toc --quiet " (buffer-file-name)))
	(bdiff-revert-buffer-maybe))))

