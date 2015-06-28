(in-package :academy)

(defun split-string-into-lines (string)
  "Return a list containing the lines from STRING split on the end of each line"
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-string (delimiter string)
  "Return a list containing the strings from STRING split on DELIMITER."
  (loop with rtn
        with acc
        for char across string
        if (char= delimiter char)
        do (push (coerce (nreverse acc) 'string) rtn)
           (setf acc nil)
        else do (push char acc)
        finally (return (nreverse (cons (coerce (nreverse acc) 'string) rtn)))))

(defun print-list-delimited (list &optional (stream *standard-output*) (printer #'prin1) (delimiter ","))
  "Output LIST to STREAM as a comma separated listing."
  (loop for els on list
        do (funcall printer (car els) stream)
           (unless (null (cdr els))
             (princ delimiter stream))))
