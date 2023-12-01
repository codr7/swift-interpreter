(load (merge-pathnames "utils.lisp" *load-truename*))

(defun find-string (in strings &key (from-end nil))
  (let ((ri (if from-end -1 (length in))) rj)
    (dolist (s strings)
      (let ((i (search (first s) in :from-end from-end)))
	(when (and i (or (and from-end (> i ri))
			 (and (not from-end) (< i ri))))
	  (setf ri i rj (rest s)))))
    rj))

(defun enumerate (&rest in)
  (mapcar #'cons in (iota 1 (1+ (length in)) 1)))
	    
(defun find-digit (in &key (from-end nil))
  (find-string in
	       (append (enumerate "1" "2" "3" "4" "5" "6" "7" "8" "9")
		       (enumerate "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
	       :from-end from-end))

(defun decode-line (line)
  (parse-integer (format nil "~a~a" (find-digit line) (find-digit line :from-end t))))

(defun calibrate (path)
  (reduce #'+ (mapcar #'decode-line (read-lines path))))
