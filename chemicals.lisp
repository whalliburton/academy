(in-package :academy)

;; A parse for SDF chemical compound files.

;; http://pubchem.ncbi.nlm.nih.gov/summary/summary.cgi?cid=190&disopt=SaveSDF

(defparameter *sdf-directory* (format nil "~Achemical-structures/" (asdf-system-base-path :academy))
  "The directory that holds all the sdf files.")
(defparameter *sample-sdf-file* (pathname (format nil "~ACID_190_adenine.sdf" *sdf-directory*)))

(defstruct chemical-description name info comment num-atoms num-bonds atoms bonds data)

(defun choose-sdf ()
  (choose-file *sdf-directory* "*.sdf" :prompt "Choose a chemical:" :default *sample-sdf-file*))

(defun load-sdf-file (&optional (filename (choose-sdf)))
  (with-open-file (stream filename)
    (flet ((line () (read-line stream))
           (element () (safe-read stream)))
      (let (natoms nbonds)
        (values
         (make-chemical-description
          :name (line)
          :info (line)
          :comment (line)
          :num-atoms (setf natoms (element))
          :num-bonds (prog1 (setf nbonds (element)) (line))
          :atoms (loop for index from 1 to natoms collect (decode-atom-line (line)))
          :bonds (prog1 (loop for index from 1 to nbonds collect (decode-bond-line (line)))
                   (assert (string= (line) "M  END")))
          :data (read-associated-data stream))
         filename)))))

(defun decode-atom-line (line)
  (with-input-from-string (stream line)
    (list
     (safe-read stream) ; x
     (safe-read stream) ; y
     (safe-read stream) ; z
     (safe-read stream) ; element
     )))

(defun decode-bond-line (line)
  (with-input-from-string (stream line)
    (list
     (safe-read stream) ; first atom
     (safe-read stream) ; second atom
     (safe-read stream) ; type
     )))

(defun read-associated-data (stream)
  (loop for header = (read-line stream nil)
        while (and header (string/= header "$$$$"))
        collect (nconc (list (string-trim '(#\space #\< #\>) header))
                       (loop for line = (read-line stream)
                             while (string/= line "")
                             collect line))))

(defun chemical ()
  "Describe one of the chemicals in the chemical database."
  (multiple-value-bind (element filename) (load-sdf-file)
    (print-heading (format nil "chemical - ~A" (pathname-name filename)))
    (with-slots (name info num-atoms num-bonds atoms bonds data) element
      (format t "  name: ~A~%  info: ~A~%  atoms: ~A~%  bonds: ~A~%" name (string-trim '(#\space) info) num-atoms num-bonds)
      (format t "~%  atoms:~%")
      (print-table (loop for atom in atoms collect (cons "" atom)))
      (format t "~%  bonds:~%")
      (print-table (loop for bond in bonds collect (cons "" bond)))
      (format t "~%  data:~%")
      (loop for (name . vals) in data
            do (format t "  ~22A  ~A~%"
                       (if (string-starts-with name "PUBCHEM_") (subseq name 8) name)
                       (with-output-to-string (stream) (print-list-delimited vals stream #'princ ", "))))))  )
