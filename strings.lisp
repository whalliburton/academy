(in-package :academy)

(defun split-string-into-lines (string)
  "Return a list containing the lines from STRING split on the end of each line."
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil )
          while line
          collect line)))
