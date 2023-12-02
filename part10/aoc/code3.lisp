(load (merge-pathnames "utils.lisp" *load-truename*))

(defun read-count (in)
  (multiple-value-bind (n i) (parse-integer in :junk-allowed t)
    (cons (kw (subseq in (1+ i))) n)))

(defun read-game (in)
  (mapcar #'read-count (split-sequence in #\,)))
	  
(defun read-games (in)
  (let ((i (position #\: in)))
    (mapcar #'read-game (split-sequence (subseq in (1+ i)) #\;))))

(defun get-count (in c)
  (let ((found (assoc c in)))
    (if found (rest found) 0)))

(defun enumerate (in)
  (mapcar #'cons (iota 1 (1+ (length in)) 1) in))    

(defun count-games (path &key (red 12) (green 13) (blue 14))
  (reduce #'+ (mapcar #'first
		      (remove-if (lambda (it)
				   (member-if (lambda (g)
						(or (> (get-count g :red) red)
						    (> (get-count g :green) green)
						    (> (get-count g :blue) blue)))
					      (rest it)))
				 (enumerate (mapcar #'read-games
						    (read-lines path)))))))
