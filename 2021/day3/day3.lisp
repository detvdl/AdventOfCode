(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-binary line))))

(defun parse-binary (line)
  (map 'list #'digit-char-p line))

(defun bin-lst-to-int (lst)
  (parse-integer (format nil "~{~A~}" lst) :radix 2))

(defun part-1 ()
  (let* ((input (read-file "input.txt"))
         (len (length input))
         (total (reduce (lambda (acc el) (mapcar #'+ acc el)) input :initial-value '(0 0 0 0 0 0 0 0 0 0 0 0)))
         (gamma (mapcar (lambda (x) (if (< x (/ len 2)) 0 1)) total))
         (epsilon (mapcar (lambda (bit) (logxor bit 1)) gamma)))
    (*  (bin-lst-to-int gamma) (bin-lst-to-int epsilon))))

(print (part-1))
