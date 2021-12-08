#+sbcl

(defparameter *starting-position1* '(0 0))
(defparameter *instruction-alist1* `(("forward" . ,(lambda (x) (lambda (pos) (list (+ x (first pos)) (second pos)))))
                                     ("down" . ,(lambda (y) (lambda (pos) (list (first pos) (+ (second pos) y)))))
                                     ("up" . ,(lambda (y) (lambda (pos) (list (first pos) (- (second pos) y)))))))

(defparameter *starting-position2* '(0 0 0))
(defparameter *instruction-alist2* `(("forward" . ,(lambda (x) (lambda (pos) (list (+ x (first pos)) (+ (second pos) (* (third pos) x)) (third pos)))))
                                     ("down" . ,(lambda (y) (lambda (pos) (list (first pos) (second pos) (+ (third pos) y)))))
                                     ("up" . ,(lambda (y) (lambda (pos) (list (first pos) (second pos) (- (third pos) y)))))))


(defun read-file (filename instruction-alist)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-instruction line instruction-alist))))

(defun parse-instruction (line instruction-alist)
  (destructuring-bind (instruction _ value) (sb-unicode:words line)
    (let ((func (cdr (assoc instruction instruction-alist :test #'string-equal))))
      (funcall func (parse-integer value)))))

(defun part-1 ()
  (let ((instructions (read-file "input.txt" *instruction-alist1*)))
    (reduce (lambda (pos instruction) (funcall instruction pos)) instructions :initial-value *starting-position1*)))

(defun part-2 ()
  (let ((instructions (read-file "input.txt" *instruction-alist2*)))
    (reduce (lambda (pos instruction) (funcall instruction pos)) instructions :initial-value *starting-position2*)))

(part-1)
(part-2)
