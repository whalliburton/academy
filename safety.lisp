(in-package :academy)

;; Safety third!

(defun safe-read (&rest args)
  (let ((*read-eval* nil))
    (apply #'read args)))
