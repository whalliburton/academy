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
