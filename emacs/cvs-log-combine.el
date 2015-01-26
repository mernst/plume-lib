;; Filter the result of "cvs log" to get just one copy of each checkin,
;; in chronological order.

;; Get the log in the current buffer.
;; (cvs-combine-log-part1)
;; Save the file as   log-trimmed
;; Now do     cat log-trimmed | sort | uniq > log-uniq
;; Then edit   log-uniq
;; (cvs-combine-log-part2)


(require 'util-mde)			; for replace-regexp-noninteractive
(eval-when-compile
  (require 'util-mde))

(defun cvs-combine-log-part1 ()
  (goto-char (point-min))
  (replace-string-noninteractive "\n" "\r")
  (goto-char (point-min))
  (replace-string-noninteractive "\r----------------------------\r" "\n")
  (goto-char (point-min))
  (replace-string-noninteractive
   "\r=============================================================================\r"
   "\n")
  (goto-char (point-min))
  (replace-string-noninteractive "\rRCS file: " "\nRCS file: ")
  (goto-char (point-min))
  (replace-string-noninteractive "\rWorking file: " "\nWorking file: ")
  (goto-char (point-min))
  (replace-regexp-noninteractive "^revision [0-9]+\.[0-9]+\r" "")
  (goto-char (point-min))
  (replace-regexp-noninteractive ";  lines: \\+[0-9]+ -[0-9]+\r" ";\r")
  (goto-char (point-min))
  ;; seconds lead to gratuitous differences
  (replace-regexp-noninteractive "^\\(date: [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]\\):[0-9][0-9]" "\\1")
  (goto-char (point-min))
  (delete-matching-lines "^RCS file: ")
  (goto-char (point-min))
  (delete-matching-lines "^Working file: ")
  (goto-char (point-min))
  (delete-matching-lines (regexp-quote "\r*** empty log message ***\r")))

(defun cvs-combine-log-part2 ()
  (goto-char (point-min))
  (replace-string-noninteractive "\n" "\n===========================================================================\n")
  (goto-char (point-min))
  (replace-string-noninteractive "\r" "\n"))
