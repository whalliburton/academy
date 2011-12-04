(in-package :academy)

(defun random-element (sequence)
  (elt sequence (random (length sequence))))

(defmacro random-do (&rest statements)
  (let ((length (length statements)))
    `(case (random ,length)
       ,@(loop for x from 0 to length
               for statement in statements
               collect `(,x ,statement)))))
