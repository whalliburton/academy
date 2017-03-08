(in-package :academy)

(defparameter *cities* (load-cities))

(defun load-cities ()
  (with-open-file (stream (data-path "Cities.dat") :direction :input :if-does-not-exist :error
                                                   :external-format :utf-8)
    (loop
      collect
      (let ((row (handler-case
                     (read-line stream)
                   (end-of-file () (return rows)))))
        (parse-cities-line row))
        into rows
      finally (return rows))))

(defun dms-to-float (d m s mult)
  (float (* mult (/ (+ (* d 60 60) (* m 60) s)
                    (* 60 60)))))

(defun parse-cities-line (line)
  (flet ((trim (string) (string-right-trim '(#\space) string)))
    (let ((name (trim (subseq line 0 33)))
          (region (trim (subseq line 35 57)))
          (country (trim (subseq line 59 81)))
          (latd (parse-integer (trim (subseq line 83 85))))
          (latm (parse-integer (trim (subseq line 88 90))))
          (lats (parse-integer (trim (subseq line 93 95))))
          (latdir (trim (subseq line 98 99)))
          (lond (parse-integer (trim (subseq line 102 105))))
          (lonm (parse-integer (trim (subseq line 108 110))))
          (lons (parse-integer (trim (subseq line 113 115))))
          (londir (trim (subseq line 118 119))))
      (list name region country
            (dms-to-float latd latm lats (if (string= latdir "N") 1 -1))
            (dms-to-float lond lonm lons (if (string= londir "E") 1 -1))))))

(defparameter *cities* (load-cities))

(defun cities (&key search (count 20) raw)
  (let* ((index 1)
         (rows
           (loop for (name region country lat lon) in *cities*
                 when (or (null count) (< index count))
                   when (or (null search)
                            (search search name :test #'char-equal)
                            (search search region :test #'char-equal))
                     collect (list name region country lat lon)
                     and do (incf index))))
    (if raw rows (print-table rows))))
