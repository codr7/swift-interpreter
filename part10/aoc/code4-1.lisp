(load (merge-pathnames "utils.lisp" *load-truename*))

(defun parse-numbers (in)
  (let ((i 0) out)
    (tagbody
     next
       (when (< i (length in))
	 (multiple-value-bind (n j) (parse-integer in :start i :junk-allowed t)
	   (when n
	     (push n out)
	     (setf i (1+ j))
	     (go next)))))
    (nreverse out)))
  
(defun parse-card (in)
  (let* ((i (position #\: in))
	 (j (position #\| in :start i))
	 (wns (parse-numbers (subseq in (+ 2 i) j)))
	 (ns (parse-numbers (subseq in (+ 2 j)))))
    (cons wns ns)))

(defun score-card (in)
  (let ((ns (intersection (first in) (rest in))) (s 0))
    (dotimes (i (length ns))
      (setf s (if (zerop s) 1 (* s 2))))
    s))

(defun parse-cards (path)
  (mapcar #'parse-card (read-lines path)))

(defun score-cards (path)
  (mapcar #'score-card (parse-cards path)))

(defun sum-cards (path)
  (reduce #'+ (score-cards path)))
