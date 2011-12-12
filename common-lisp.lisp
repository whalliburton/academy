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

(defun list-all-used-common-lisp-symbols (system-name)
  (let ((all-files
          (remove-duplicates
           (flatten
            (loop for filename in (asdf-lisp-files system-name)
                  collect (read-lisp-file filename))))))
    (multiple-value-bind (functions non-functions) (list-all-common-lisp-symbols)
      (values
       (sort (intersection all-files functions) #'string<)
       (sort (intersection all-files non-functions) #'string<)))))

(defun show-all-used-common-lisp-symbols (system-name)
  "Shows all the symbols used in the system named SYSTEM-NAME that come from the CL package."
  (multiple-value-bind (functions non-functions) (list-all-used-common-lisp-symbols system-name)
    (print-heading "functions")
    (print-table (group functions 4))
    (print-heading "everything else")
    (print-table (group non-functions 4))))
