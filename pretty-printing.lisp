(in-package :academy)

(defun rotate-rows-to-columns (rows)
  (loop for remaining = rows then (mapcar #'cdr remaining)
        while (not (every #'null remaining))
        collect (mapcar #'car remaining)))

(defun maximize-length (list &key (key #'identity))
  (loop for element in list maximizing (length (funcall key element))))

(defun pad-list (list length &optional (pad-element nil))
  (loop for el on list
        for x from 1
        do (when (null (cdr el))
             (setf (cdr el) (make-list (- length x) :initial-element pad-element))
             (return list))))

(defun print-table (rows &key (gap "  ") (align :left))
  (loop
    with max-row-length = (apply #'max (mapcar #'length rows))
    with control-string =
       (format nil
               (concatenate
                'string "~{~~~D" (ecase align (:right "@") (:left "")) "A~^" gap "~}~%")
               (mapcar (lambda (row) (maximize-length row :key #'princ-to-string))
                       (rotate-rows-to-columns rows)))
    for row in (mapcar (lambda (row) (pad-list row max-row-length "")) rows)
    do (apply #'format t control-string row)))

(defun print-heading (text &key (underline "▀"))
  (terpri)
  (write-string text)
  (fresh-line)
  (dotimes (i (length text)) (write-string underline))
  (fresh-line)
  (terpri))
