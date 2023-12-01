(defun read-lines (path)
  (with-open-file (f path)
    (labels ((rec (out)
	       (let ((line (read-line f nil)))
		 (if line
		     (rec (cons line out))
		     (nreverse out)))))
      (rec nil))))

(defun find-string (in strings &key (from-end nil))
  (let ((ri (if from-end -1 (length in))) rj)
    (dolist (s strings)
      (let ((i (search (first s) in :from-end from-end)))
	(when (and i (or (and from-end (> i ri))
			 (and (not from-end) (< i ri))))
	  (setf ri i rj (rest s)))))
    rj))

(defun enumerate (&rest in)
  (labels ((rec (in out i)
	     (if in
		 (rec (rest in) (cons (cons (first in) i) out) (1+ i))
		 (nreverse out))))
    (rec in nil 1)))
	    
(defun find-digit (in &key (from-end nil))
  (find-string in
	       (append (enumerate "1" "2" "3" "4" "5" "6" "7" "8" "9")
		       (enumerate "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
	       :from-end from-end))

(defun decode-line (line)
  (parse-integer (format nil "~a~a" (find-digit line) (find-digit line :from-end t))))

(defun calibrate (path)
  (reduce #'+ (mapcar #'decode-line (read-lines path))))
