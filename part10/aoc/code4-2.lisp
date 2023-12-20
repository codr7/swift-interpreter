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

(defun count-winners (in)
  (length (intersection (first in) (rest in))))

(defun parse-cards (path)
  (mapcar #'parse-card (read-lines path)))

(defun count-winning-cards (path)
  (let ((in (parse-cards path)) out (k 0))
    (tagbody
     next
       (setf in (append in out) out nil)
    
       (dotimes (i (- (length in) k))
	 (incf k)
	 
	 (dotimes (j (count-winners (nth (+ i k) in)))
	   (when (< (+ i j k) (length in))
	     (push (nth (+ i j k 1) in) out))))

       (unless (null out) (go next)))

    in))
