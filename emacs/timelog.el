;;; Timelog

;; This is a very primitive time logging system; it works for me, but is
;; not guaranteed to work for you.  It should probably be generalized,
;; better documented, etc.
;; (In Emacs 21.2, timeclock.el gave me errors, and isn't really what I want.)

;; There are two commands:
;;  M-x timelog   causes you to edit your timelog file
;;  M-x timelog-summarize   summarizes the timelog entries in the current region

;; The timelog file contains newline-separated entries of the following form:
;;
;;  1/5
;;  0800-1015 S review paper for EMSE
;;  1015-1145 R work on bug in MergeESC; fixed problem with VarInfoName lookup
;;  1145-1230 R fix problem with ESC output (equalities all on one line)
;;  1300-1330 R update to JUnit 3.8.1: "assert()" becomes "assertTrue()"
;;  1430-1445 A Stephen McCamant
;;  1530-1600 R testing of changes
;;
;; A date starts the paragraph.
;; The first column is a range of times, in military (24-hour) style.
;;   These times must be rounded to the nearest quarter hour.
;; The second column is a single letter indicating what I spent that time on:
;;   research, advising, education, service, logistics, or miscellaneous.
;; The final column is what I spent the time on.
;; The timelog-summarize command adds up all the time for each category, so
;;   you can see how much time you spent on (say) service in the last (say) week.

(defvar timelog-file "~/prof/timelog"
  "The file that records the time log.")

(defun timelog ()
  "Edit the timelog."
  (interactive)
  (find-file timelog-file)
  (goto-char (point-max))
  (if (not (equal (timelog-get-date) (date-string)))
      (progn
        (insert "\n")
        (delete-blank-lines)
        (insert "\n" (date-string) "\n")))
  (if (looking-back "-[0-9][0-9][0-9][0-9]\n?")
      (delete-region (match-beginning 0) (point)))
  (if (looking-back "[0-9][0-9][0-9][0-9]\n")
      (backward-char))
  (if (not (looking-back (rounded-current-time-string)))
      (progn
        (if (looking-back "\n[0-9][0-9][0-9][0-9]")
            (insert "-"))
        (if (not (looking-back "[-\n]"))
            (insert "\n"))
        (insert (rounded-current-time-string))))
  (message (current-time-string)))

(defun timelog-get-date ()
  (find-file timelog-file)
  (save-excursion
    (goto-char (point-max))
    (re-search-backward "\n\n\\([0-9]+/[0-9]+\\)" nil t)
    (match-string 1)))

(defun date-string ()
  (let* ((decoded (decode-time (current-time))))
    (concat (int-to-string (nth 4 decoded))
            "/"
            (int-to-string (nth 3 decoded)))))

(defun rounded-current-time-string ()
  (let* ((decoded (decode-time (current-time)))
         (minutes (second decoded))
         (minutes-rounded (* 15 (round minutes 15)))
         (hour (third decoded)))
    (if (= minutes-rounded 60)
        (setq minutes-rounded 0
              hour (mod (1+ hour) 24)))
    (format "%02d%02d" hour minutes-rounded)))
;; (rounded-current-time-string)

(defun timelog-summarize (beg end)
  "Add up the times in the current region; insert the result at point."
  (interactive "r")
  (goto-char (min beg end))
  (beginning-of-line)
  (setq end (max beg end))
  (let ((first-day nil)
        (last-day nil)
        (research-time 0)
        (advising-time 0)
        (education-time 0)
        (service-time 0)
        (logistics-time 0)
        (misc-time 0)
        (research-lines nil)
        (advising-lines nil)
        (education-lines nil)
        (service-lines nil)
        (logistics-lines nil)
        (misc-lines nil))
    (while (< (point) end)
      (cond
       ((looking-at "$")
        nil)
       ((looking-at "\\([01]?[0-9]/[0-3]?[0-9]\\):?$")
        (if (not first-day)
            (setq first-day (match-string 1)))
        (setq last-day (match-string 1)))
       ((looking-at "\\([012][0-9]\\)\\([0-5][05]\\)-\\([012][0-9]\\)\\([0-5][05]\\) \\([RAESLM]\\) \\(.*\n\\)")
        (let* ((start-hour (+ (string-to-number (match-string 1))
                              (/ (string-to-number (match-string 2)) 60.0)))
               (end-hour (+ (string-to-number (match-string 3))
                            (/ (string-to-number (match-string 4)) 60.0)))
               (category (match-string 5))
               ;; Mod in case end time is after midnight
               (duration (mod (- end-hour start-hour) 24))
               (detail (concat category " " (match-string 6))))
          (cond ((equal category "R")
                 ;; research
                 (incf research-time duration)
                 (push detail research-lines))
                ((equal category "A")
                 ;; advising (for research students)
                 (incf advising-time duration)
                 (push detail advising-lines))
                ((equal category "E")
                 ;; education
                 (incf education-time duration)
                 (push detail education-lines))
                ((equal category "S")
                 ;; service; includes undergraduate advising
                 (incf service-time duration)
                 (push detail service-lines))
                ((equal category "L")
                 ;; logistics
                 (incf logistics-time duration)
                 (push detail logistics-lines))
                ((equal category "M")
                 ;; miscellaneous:  non-work
                 (incf misc-time duration)
                 (push detail misc-lines))
                (t
                 (error "Bad category for time: %s" category)))))
       ((looking-at "\\([012][0-9]\\)\\([0-5][05]\\)-\\([012][0-9]\\)\\([0-5][05]\\) \\(.\\)[^ ]")
        (error "No category (or no space after category) on line %d: %s"
               (1+ (count-lines 1 (point)))
               (buffer-substring (line-beginning-position) (line-end-position))))
       (t
        (error "Can't parse line %d: %s"
               (1+ (count-lines 1 (point)))
               (buffer-substring (line-beginning-position) (line-end-position)))))
      (forward-line 1))
    (if (not first-day)
        (error "Didn't find any days in region"))
    (if (not (looking-back "\n\n"))
        (insert "\n"))
    (insert (format (concat
                     "Week of %s-%s:\n"
                     "  research: %.2f\n"
                     "  advising: %.2f\n"
                     "  education: %.2f\n"
                     "  service: %.2f\n"
                     "  logistics: %.2f\n"
                     "  misc: %.2f\n")
                    first-day last-day
                    research-time advising-time
                    education-time service-time
                    logistics-time
                    misc-time))
    (let ((after-summary (point)))
      (insert "\n")
      (if research-lines
          (apply #'insert "\nResearch:\n" (sort research-lines #'string-lessp)))
      (if advising-lines
          (apply #'insert "\nAdvising:\n" (sort advising-lines #'string-lessp)))
      (if education-lines
          (apply #'insert "\nEducation:\n" (sort education-lines #'string-lessp)))
      (if service-lines
          (apply #'insert "\nService:\n" (sort service-lines #'string-lessp)))
      (if logistics-lines
          (apply #'insert "\nLogistics:\n" (sort logistics-lines #'string-lessp)))
      (if misc-lines
          (apply #'insert "\nMiscellaneous:\n" (sort misc-lines #'string-lessp)))
      (goto-char after-summary))
    ))
