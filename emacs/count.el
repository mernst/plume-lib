;; From: sk@thp.uni-koeln.de
;; Subject: More useful C-x = and ESC = commands
;; Date: 2 Nov 90 12:22:10 GMT
;; Organization: GNUs Not Usenet
;; 
;; C-x = (what-cursor-position) should also display the line number, and
;; ESC = (count-lines-region) should not only count lines, but words and
;; characters as well:

(define-key esc-map "=" 'count-region)
(defun count-region (start end)
  "Count lines, words and characters in region."
  (interactive "r")
  (let ((l (count-lines start end))
	(w (count-words start end))
	(c (- end start)))
    (message "Region has %d line%s, %d word%s, and %d character%s."
	     l (if (= 1 l) "" "s")
	     w (if (= 1 w) "" "s")
	     c (if (= 1 c) "" "s"))))

(defun count-words (start end)
  "Return number of words between START and END."
  (let ((count 0))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (forward-word 1)
	  (setq count (1+ count)))))
    count))

(define-key ctl-x-map "=" 'what-cursor-position-and-line)
(defun what-cursor-position-and-line ()
  ;; So you don't need what-line any longer.
  "Print info on cursor position (on screen and within buffer)."
  (interactive)
  (let* ((char (following-char))
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format " Hscroll=%d" (window-hscroll))))
	 (col (current-column))
	 (line (save-restriction
		 (widen)
		 (save-excursion
		   (beginning-of-line)
		   (1+ (count-lines 1 (point)))))))
    (if (= pos end)
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "point=%d of %d(%d%%) <%d - %d>  line %d column %d %s"
		     pos total percent beg end line col hscroll)
	  (message "point=%d of %d(%d%%)  line %d column %d %s"
		   pos total percent line col hscroll))
      (if (or (/= beg 1) (/= end (1+ total)))
	  (message "Char: %s (0%o)  point=%d of %d(%d%%) <%d - %d>  line %d column %d %s"
		   (single-key-description char) char pos total percent beg end line col hscroll)
	(message "Char: %s (0%o)  point=%d of %d(%d%%)  line %d column %d %s"
		 (single-key-description char) char pos total percent line col hscroll)))))
