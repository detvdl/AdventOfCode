(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

(defun part-1 ()
  (let* ((numbers (read-file "input.txt"))
         (n0 (pop numbers)))
    (loop for previous = n0 then next
          and next in numbers
          sum (if (> next previous) 1 0) into total
          finally (print total))))

(defun part-2 ()
  (let* ((numbers (read-file "input.txt"))
         (n0 (pop numbers))
         (n1 (pop numbers))
         (n2 (pop numbers)))
    (loop for first = n0 then second
          and second = n1 then third
          and third = n2 then fourth
          and fourth in numbers
          sum (if (> (+ second third fourth) (+ first second third)) 1 0) into total
          finally (print total))
    ))

(part-1)
(part-2)
