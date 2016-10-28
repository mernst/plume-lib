;; To make Emacs automatically run html-update-toc on HTML files as you
;; save them, add to your .emacs file:
;;   (load-library "html-update")

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
        (require 'bdiff)
        ;; It's not enough to check verify-visited-file-modtime, because we
        ;; ran html-update-toc as soon as the file was written:  even if it
        ;; changes the file, it is unlikely to change the file's modtime.
        (set-buffer-modified-p t)       ; set buffer as modified
        (if (bdiff t t)                 ; possibly unset it
            (revert-buffer nil t)))))
