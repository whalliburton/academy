(in-package :academy)

(defun random-element (sequence)
  (elt sequence (random (length sequence))))

(defun random-booleans (num &optional (probability 2))
  (loop for i from 1 to num
        collect (zerop (random probability))))

(defmacro random-do (&rest statements)
  (let ((length (length statements)))
    `(case (random ,length)
       ,@(loop for x from 0 to length
               for statement in statements
               collect `(,x ,statement)))))

(defun random-word ()
  (unless *words* (load-words))
  (aref *words* (random (length *words*))))
