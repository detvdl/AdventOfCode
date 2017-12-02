(defun read-day2-input (path)
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((contents (split-string (buffer-string) "\n" t)))
      (mapcar (lambda (el)
                (let ((line (split-string el)))
                  (mapcar #'string-to-number line)))
              contents))))

(defconst input-matrix (read-day2-input "./input"))

;; Puzzle 1
(let ((sum 0))
  (dolist (element input-matrix)
    (let* ((min-val (nth 0 element))
           (max-val min-val)
           (diff))
      (dolist (el element)
        (setq min-val (min min-val el))
        (setq max-val (max max-val el)))
      (setq sum (+ sum (- max-val min-val)))))
  (print (format "sum is %d" sum)))

;; Puzzle 2
(let ((sum 0))
  (dolist (element input-matrix)
    (catch 'break-loop
      (dolist (el1 element)
        (dolist (el2 element)
          (let ((min-val (min el1 el2))
                (max-val (max el1 el2)))
            (when (and (not (= el1 el2))
                       (= (mod max-val min-val) 0))
              (setq sum (+ sum (/ max-val min-val)))
              (throw 'break-loop sum)))))))
  (print (format "sum is %d" sum)))
