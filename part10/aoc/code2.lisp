(defun read-lines (path)
  (with-open-file (f path)
    (labels ((rec (out)
	       (let ((line (read-line f nil)))
		 (if line
		     (rec (cons line out))
		     (nreverse out)))))
      (rec nil))))

(defun find-string (in strings &key (from-end nil))
  (let ((ri (if from-end -1 (length in))) rj (j 1))
    (dolist (s strings)
      (let ((i (search s in :from-end from-end)))
	(when (and i (or (and from-end (> i ri))
			 (and (not from-end) (< i ri))))
	  (setf ri i rj j)))
      (incf j))
    (values ri rj)))

(defun find-char-digit (in &key (from-end nil))
  (find-string in '("1" "2" "3" "4" "5" "6" "7" "8" "9") :from-end from-end))

(defun find-word-digit (in &key (from-end nil))
  (find-string in '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine") :from-end from-end))

(defun find-digit (in &key (from-end nil))
  (multiple-value-bind (ci cj) (find-char-digit in :from-end from-end)
    (multiple-value-bind (wi wj) (find-word-digit in :from-end from-end)
      (if from-end
	  (if (> ci wi) cj wj)
	  (if (< ci wi) cj wj)))))

(defun decode-line (line)
  (parse-integer (format nil "~a~a" (find-digit line) (find-digit line :from-end t))))

(defun calibrate (path)
  (reduce #'+ (mapcar #'decode-line (read-lines path))))
