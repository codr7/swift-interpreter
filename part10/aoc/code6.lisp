(load (merge-pathnames "utils.lisp" *load-truename*))

(defvar parts (make-hash-table :test 'equal))

(defun decode-row (in row)
  (dotimes (col (length in))
    (let ((c (char in col)))
      (if (digit-char-p c)
	  (multiple-value-bind (n i) (parse-integer in :start col :junk-allowed t)
	    (dotimes (j (- i col))
	      (setf (gethash (cons row (+ col j)) parts) n))
	    (setf col (1- i)))
	  (when (char= c #\*)
	    (setf (gethash (cons row col) parts) :gear))))))

(defun get-gear-ratio (row col)
  (let ((gns))
    (dotimes (ri 3)
      (dotimes (ci 3)
	(let ((gn (gethash (cons (+ row (1- ri)) (+ col (1- ci))) parts)))
	  (when (and (numberp gn) (not (member gn gns)))
	    (push gn gns)))))
    (when (= (length gns) 2)
      (reduce #'* gns))))
  
(defun sum-gears (path)
  (setf parts (make-hash-table :test 'equal))
  
  (let ((i 0) out)
    (dolist (in (read-lines path))
      (decode-row in i)
      (incf i))
    
    (do-hash (k v parts)
      (when (eq v :gear)
	(let ((gr (get-gear-ratio (first k) (rest k))))
	  (when gr
	    (push gr out)))))
    
    (reduce #'+ out)))
