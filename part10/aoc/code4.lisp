(load (merge-pathnames "utils.lisp" *load-truename*))

(defun read-count (in)
  (multiple-value-bind (n i) (parse-integer in :junk-allowed t)
    (cons (kw (subseq in (1+ i))) n)))

(defun read-game (in)
  (mapcar #'read-count (split-sep in #\,)))
	  
(defun read-games (in)
  (let ((i (position #\: in)))
    (mapcar #'read-game (split-sep (subseq in (1+ i)) #\;))))

(defun get-count (in c)
  (let ((found (assoc c in)))
    (if found (rest found) 0)))

(defun count-games (path)
  (reduce #'+ (mapcar (lambda (gs)
			(let ((gr 0) (gg 0) (gb 0))
			  (dolist (g gs)
			    (setf gr (max gr (get-count g :red)))
			    (setf gg (max gg (get-count g :green)))
			    (setf gb (max gb (get-count g :blue))))
			  (* gr gg gb)))
		      (mapcar #'read-games (read-lines path)))))
    
