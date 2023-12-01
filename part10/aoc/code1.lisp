(load (merge-pathnames "utils.lisp" *load-truename*))

(defun find-digit (in &key (from-end nil))
  (let ((c (find-if #'digit-char-p in :from-end from-end)) )
    (- (char-code c) (char-code #\0))))
   
(defun decode-line (line)
  (parse-integer (format nil "~a~a" (find-digit line) (find-digit line :from-end t))))
   
(defun calibrate (path)
  (reduce #'+ (mapcar #'decode-line (read-lines path))))
