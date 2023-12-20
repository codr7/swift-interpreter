(load (merge-pathnames "utils.lisp" *load-truename*))

(define-symbol-macro card-count 14)

(defparameter card-names #(#\T #\J #\Q #\K #\A))

(defun partition-hand (h)
  (let ((acc (make-array (1+ card-count) :initial-element 0)))
    (do-vector (c h)
      (incf (aref acc c)))
    
    (let (out)
      (dotimes (c (length acc))
	(let ((n (aref acc c)))
	  (unless (zerop n)
	    (push (cons c n) out))))

      (coerce (sort out (lambda (c1 c2) (> (rest c1) (rest c2)))) 'vector))))

(defun compare-hands (hb1 hb2)
  (let ((h1 (first hb1)) (h2 (first hb2)))
    (let ((ph1 (partition-hand h1))
	  (ph2 (partition-hand h2)))
      (dotimes (i (min (length ph1) (length ph2)))
	(let ((cs1 (aref ph1 i))
	      (cs2 (aref ph2 i)))
	  (when (< (rest cs1) (rest cs2))
	    (return-from compare-hands t))
	  (when (> (rest cs1) (rest cs2))
	    (return-from compare-hands)))))

    (dotimes (i (min (length h1) (length h2)))
      (when (< (aref h1 i) (aref h2 i))
	(return-from compare-hands t))
      (when (> (aref h1 i) (aref h2 i))
	(return-from compare-hands)))
    
    (error "Equal hands ~a ~a" h1 h2)))

(defun parse-hand (s)
  (coerce (mapcar (lambda (c)
		    (ecase c
		      ((#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (char-digit c))
		      ((#\T #\J #\Q #\K #\A) (+ 10 (position c card-names)))))
		  (coerce s 'list))
	  'vector))
	      
(defun parse-line (s)
  (let* ((ps (split-sep s #\space))
	 (h (parse-hand (first ps)))
	 (b (parse-integer (second ps))))
    (cons h b)))

(defun parse-file (path)
  (sort (mapcar #'parse-line (read-lines path)) #'compare-hands))

(defun total-win (path)
  (let ((hs (coerce (decode-file path) 'vector))
	(tw 0))
    (dotimes (i (length hs))
      (incf tw (* (1+ i) (rest (aref hs i)))))
    tw))
