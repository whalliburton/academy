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

(defun string-starts-with (string prefix &key (test #'char=))
  "Returns true if STRING starts with PREFIX."
  (let ((mismatch (mismatch prefix string :test test)))
    (or (not mismatch) (= mismatch (length prefix)))))

(defun string-ends-with (string suffix &key (test #'char=))
  "Returns true if STRING ends with PREFIX."
  (let ((mm 0))
    (loop for end1 from (1- (length string)) downto 0
          for end2 from (1- (length suffix)) downto 0
          while (funcall test (aref string end1) (aref suffix end2))
          do (incf mm))
    (= mm (length suffix))))
