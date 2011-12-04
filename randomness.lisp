(in-package :academy)

(defun random-element (sequence)
  (elt sequence (random (length sequence))))

(defun random-booleans (num)
  (loop for i from 1 to num
        collect (zerop (random 2))))

(defmacro random-do (&rest statements)
  (let ((length (length statements)))
    `(case (random ,length)
       ,@(loop for x from 0 to length
               for statement in statements
               collect `(,x ,statement)))))
