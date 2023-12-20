(defun char-digit (c)
  (- (char-int c) (char-int #\0)))

(defun delete-at (seq i)
  (let ((elt (aref seq i))
	(slide (subseq seq (1+ i)))
	(count (1- (fill-pointer seq))))
    (replace seq slide :start1 i)
    (adjust-array seq count :fill-pointer count)
    elt))

(defmacro do-hash ((k v tbl) &body body)
  (let (($i (gensym)) ($k (gensym)) ($next (gensym)) ($ok (gensym)))
    `(with-hash-table-iterator (,$i ,tbl)
       (tagbody
	  ,$next
	  (multiple-value-bind (,$ok ,$k ,v) (,$i)
	    (when ,$ok
	      (let ((,k ,$k))
		,@body
		(go ,$next))))))))

(defmacro do-vector ((val vec) &body body)
  (let ((i (gensym)))
    `(dotimes (,i (length ,vec))
       (let ((,val (aref ,vec ,i)))
	 ,@body))))

(defmacro do-while (cnd &body body)
  (let (($next (gensym)))
    `(block nil
       (tagbody
	  ,$next
	  (when ,cnd
	    ,@body
	    (go ,$next))))))

(defun every? (seq pred)
  (not (find-if-not pred seq)))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun flatten (lst)
  (labels ((rec (in out)
	     (if in
		 (let ((x (pop in)))
		   (if (listp x)
		       (rec in (append (rec x nil) out))
		       (rec in (cons x out))))
		 out)))
    (nreverse (rec lst nil))))

(defun iota (start end step)
  (labels ((rec (i out)
	     (if (< i end)
		 (rec (+ i step) (cons i out))
		 (nreverse out))))
    (rec start nil)))

(defun join (sep &rest args)
  (with-output-to-string (out)
    (dolist (a args)
      (unless (zerop (file-position out))
	(princ sep out))
      (princ a out))))

(defmethod len (x)
  (length x))

(defmethod len ((x hash-table))
  (hash-table-count x))

(defun reduce-times (fn n out)
  (labels ((rec (i out)
	     (if (< i n)
		 (rec (1+ i) (funcall fn i out))
		 out)))
    (rec 0 out)))

(defmacro nor (&rest args)
  `(not (or ,@args)))

(defun read-lines (path)
  (with-open-file (f path)
    (labels ((rec (out)
	       (let ((line (read-line f nil)))
		 (if line
		     (rec (cons line out))
		     (nreverse out)))))
      (rec nil))))

(defun replace-all (in old new)
  (with-output-to-string (out)
    (labels ((rec (j)
	       (let ((i (search old in :start2 j)))
		 (write-string in out :start j :end (or i (length in)))
		 (when i
		   (write-string new out)
		   (rec (+ i (length old)))))))
      (rec 0))))

(defun replace-char (in old new)
  (map 'string (lambda (c) (if (char= c old) new c)) in))

(defun reverse-vector (vec &optional (start 0))
  (dotimes (i (floor (- (length vec) start) 2))
    (let ((tmp (aref vec (+ start i))))
      (setf (aref vec (+ start i)) (aref vec (- (length vec) i 1))
	    (aref vec (- (length vec) i 1)) tmp))))

(defun set-hash (key tbl val)
  (setf (gethash key tbl) val))

(defun slurp (path)
  (with-open-file (f path)
    (let ((out (make-string (file-length f))))
      (read-sequence out f)
      out)))

(defun split-at (in i)
  (if (zerop i)
      (values nil in)
      (let* ((prev (nthcdr (1- i) in))
             (tail (rest prev)))
        (rplacd prev nil)
        (values in tail))))

(defun split-sep (seq sep)
  (labels ((rec (i out)
	     (let ((j (position sep seq :start i)))
	       (if j
		   (rec (1+ j) (cons (subseq seq i j) out))
		   (nreverse (cons (subseq seq i) out))))))
    (rec 0 nil)))
   
(defun write-symbols (&rest args)
  (with-output-to-string (out)
    (dolist (a args)
      (etypecase a
	(symbol (princ a out))
	(string (princ (string-upcase a) out))))))

(defun kw (&rest args)
  (intern (apply #'write-symbols args) 'keyword))

(defun sym (&rest args)
  (intern (apply #'write-symbols args)))

(defmacro when-let (var form &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defun ws? (c)
  (when (or (char= c #\space) (char= c #\tab) (char= c #\newline))
    c))
