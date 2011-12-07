(in-package :academy)

(defmacro collect (number &rest body)
  "Return a list of NUMBER of the results of independently executing BODY."
  (let ((counter (gensym)))
    `(loop for ,counter from 1 to ,number
           collect (progn ,@body))))
