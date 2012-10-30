(in-package :academy)

;; Computer programmers need to get out into nature also. You sould not spend your entire
;; waking life sitting down and typing symbols into the great machine!

;; Learning plants by family is the best way to start seeing many patterns in
;; nature. Mustards, legums, parslies, asters, etc.

;; I highly recommend -- "Botony in a Day" by Thomas J. Elpel.


(defun read-csv-line (line)
  (with-input-from-string (stream line)
    (loop
      collect (read stream) into data
      do (handler-case
             (read-char stream)
           (end-of-file () (return data))))))

(defun plant-row-scientific-name (row) (third row))
(defun plant-row-common-name (row) (fourth row))
(defun plant-row-family (row) (fifth row))
(defun plant-row-genus (row) (sixth row))
(defun plant-row-species (row) (seventh row))

(defun parse-scientific-name (long)
  (subseq (split-string #\space long) 0 2))

(defun load-plant-table ()
  (with-open-file (stream (data-path "plants.txt") :direction :input :if-does-not-exist :error
                                                   :external-format :latin1)
    (sort
     (loop
       collect
          (let ((row (handler-case
                         (read-csv-line (read-line stream))
                       (end-of-file () (return rows)))))
            (nconc row (parse-scientific-name (plant-row-scientific-name row))))
       into rows)
     #'string< :key #'plant-row-scientific-name)))

(defparameter *plant-table* (load-plant-table))

(defun list-plant-families (&optional count)
  (sort
   (let ((families (make-hash-table :test 'equal)))
     (loop for row in *plant-table*
           do (if (gethash (plant-row-family row) families)
                (incf (gethash (plant-row-family row) families))
                (setf (gethash (plant-row-family row) families) 1)))
     (loop for name being each hash-key of families
           for count being each hash-value of families
           collect (list name count)))
   (if count #'> #'string<) :key (if count #'second #'first)))

(defparameter *plant-families* (list-plant-families))
(defparameter *plant-families-by-size* (list-plant-families t))

(defun plant-families (&optional by-size)
  (print-table (if by-size *plant-families-by-size* *plant-families*)))

(defun plant-row-search (row search)
  (loop for column in row
        do (when (search search column :test #'char-equal)
             (return t))))

(defun remove-subspecies (rows)
  (loop
    with genus and species
    for row in rows
    unless (and (equal genus (plant-row-genus row))
                (equal species (plant-row-species row)))
    collect row
    and do (setf genus (plant-row-genus row)
                 species (plant-row-species row))))

(defun plants (&key (maximum 20) start end search (subspecies t))
  "List and search the plants database."
  (let* (total
         (rows
           (mapcar
            (lambda (row)
              (list
               (subseq (plant-row-scientific-name row) 0
                       (min 60 (length (plant-row-scientific-name row))))
               (subseq (plant-row-common-name row) 0
                       (min 26 (length (plant-row-common-name row))))
               (plant-row-family row)))
            (let ((results
                    (funcall
                     (if subspecies #'identity #'remove-subspecies)
                     (cond
                       (search
                        (remove-if-not (lambda (row) (plant-row-search row search)) *plant-table*))
                       ((and start end) (subseq *plant-table* start end))
                       (t *plant-table*)))))
              (if maximum
                (if (> (length results) maximum)
                  (progn
                    (setf total (length results))
                    (subseq results 0 maximum))
                  results)
                results)))))
    (if rows
      (progn
        (print-table rows)
        (when (and total (< (length rows) total))
          (format t "...and ~A more~%" (- total (length rows)))))
      (format t "none.~%"))))