(in-package :academy)

(defun list-all-common-lisp-symbols ()
  (loop for symbol being each external-symbol in :cl
        for is-function = (handler-case (symbol-function symbol) (undefined-function () ()))
        when is-function
        collect symbol into functions
        when (not is-function)
        collect symbol into non-functions
        finally (return (values functions non-functions))))

(defun show-common-lisp-symbols ()
  (multiple-value-bind (functions non-functions) (list-all-common-lisp-symbols)
    (print-heading "functions")
    (print-table (group (sort functions #'string<) 2))
    (print-heading "everything else")
    (print-table (group (sort non-functions #'string<) 2))))
