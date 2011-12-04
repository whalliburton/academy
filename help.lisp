(in-package :academy)

(defun help ()
  "Lisp helps those who help themselves."
  (multiple-value-bind (commands max-size)
      ;; Figure out the size of the left column and sort the entries.
      (loop for var being each external-symbol in :academy
            collect var into commands
            maximizing (length (symbol-name var)) into max-size
            finally (return (values
                             (sort commands #'string<)
                             max-size)))
    (loop for command in commands
          ;; !!! Nested formats to set the column size in the control string.
          do (format t (format nil "~~~DA ~~A~~%" (+ max-size 1))
                     command (documentation command 'function)))))
