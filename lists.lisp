(in-package :academy)

(defmacro collect (number &rest body)
  "Return a list of NUMBER of the results of independently executing BODY."
  (let ((counter (gensym)))
    `(loop for ,counter from 1 to ,number
           collect (progn ,@body))))

(defun count-unique (list &optional (test #'equal))
  "Return as list of (NAME COUNT) of unique names from LIST and the number of occurances
in the list of this unique name."
  (let ((hash (make-hash-table :test test)))
    (loop for element in list
          do (if (gethash element hash)
               (incf (gethash element hash))
               (setf (gethash element hash) 1)))
    (loop for name being each hash-key of hash
          for count being each hash-value of hash
          collect (list name count))))
