(in-package :academy)

;;; I Ching
;;; Book of Changes

;;; Calculated using the 4 coin method described at
;;; http://en.wikipedia.org/wiki/I_Ching_divination
;;; which has the same probabilities as the original yarrow stalk method.

(defun i-ching (&optional short)
  "Consult the Book of Changes."
  (load-i-ching-text)
  (let* ((roll (loop for line from 0 to 5
                     collect (let ((index (reduce #'+
                                                  (mapcar #'*
                                                          (mapcar (lambda (el) (if el 1 0))
                                                                  (random-booleans 4))
                                                          '(8 4 2 1)))))
                               (loop for (list symbol) in
                                        '(((0) :old-yin)
                                          ((1 2 3) :old-yang)
                                          ((4 5 6 7 8) :young-yang)
                                          ((9 10 11 12 13 14 15) :young-yin))
                                     do (when (find index list :test #'=)
                                          (return symbol))))))
         (aged-roll (age-i-ching roll)))
    (labels ((print-trigram (top bottom)
               (let ((hexagram (trigrams-to-hexagram top bottom)))
                 (princ (hexagram-symbol hexagram))
                 (princ "  ")
                 (if short
                   (format t "~2A ~A~%" hexagram (hexagram-title hexagram))
                   (hexagram-text hexagram))))
             (print-roll (roll)
               (apply #'print-trigram
                      (loop for (char index) in
                               (reverse
                                (loop for (bottom middle top) on roll by #'cdddr
                                      collect (i-ching-trigram-character bottom middle top)))
                            do (format t "~C  ~A~%" char
                                       (let ((name (char-name char)))
                                         (string-downcase
                                          (subseq name (1+ (position #\_ name :from-end t))))))
                            collect index))))
      (print-roll roll)
      (terpri)
      (print-roll aged-roll))))

(defun i-ching-trigram-character (bottom middle top)
  (flet ((yang (el) (if (member el '(:young-yang :old-yang)) 1 0)))
    (let ((index (+ (yang bottom) (* (yang middle) 2) (* (yang top) 4))))
      (list
       (case index
         (0 #\TRIGRAM_FOR_EARTH)
         (1 #\TRIGRAM_FOR_THUNDER)
         (2 #\TRIGRAM_FOR_WATER)
         (3 #\TRIGRAM_FOR_LAKE)
         (4 #\TRIGRAM_FOR_MOUNTAIN)
         (5 #\TRIGRAM_FOR_FIRE)
         (6 #\TRIGRAM_FOR_WIND)
         (7 #\TRIGRAM_FOR_HEAVEN))
       index))))

(defparameter *hexagrams*
  '((7 1 2 4 0 6 5 3)
    ((1 34 5 26 11 9 14 43)
     (25 51 3 27 24 42 21 17)
     (6 40 29 4 7 59 64 47)
     (33 62 39 52 15 53 56 31)
     (12 16 8 23 2 20 35 45)
     (44 32 48 18 46 57 50 28)
     (13 55 63 22 36 37 30 49)
     (10 54 60 41 19 61 38 58))))

(defun trigrams-to-hexagram (top bottom)
  (destructuring-bind (map grid) *hexagrams*
    (let ((row (position bottom map :test #'=))
          (column (position top map :test #'=)))
      (nth column (nth row grid)))))

(defun age-i-ching (roll)
  (loop for el in roll
        collect
           (case el
             (:young-yin :old-yin)
             (:young-yang :old-yang)
             (:old-yin :young-yang)
             (:old-yang :young-yin))))

(defparameter *i-ching-text* nil)
(defparameter *i-ching-index* nil)
(defparameter *i-ching-titles* nil)

(defun i-ching-text ()
  (unless *i-ching-text*
    (multiple-value-bind (text index ) (load-i-ching-text)
      (setf *i-ching-text* text
            *i-ching-index* index)))
  (values *i-ching-text* *i-ching-index*))

(defun load-i-ching-text ()
  (unless *i-ching-text*
    (let ((file (slurp-file (data-path "ching.txt"))))
      (loop
        with index
        with on-digit
        for x from 0 to (length file)
        for char across file
        do (if (and (null on-digit) (digit-char-p char))
             (progn
               (push x index)
               (setf on-digit t))
             (when (and on-digit (not (digit-char-p char)))
               (setf on-digit nil)))
        finally (setf *i-ching-text* file
                      *i-ching-index* (nreverse index))))
    (setf *i-ching-titles*
          (with-input-from-string (stream *i-ching-text*)
            (loop for i in *i-ching-index*
                  for x from 1
                  do (file-position stream i)
                  collect (list x (subseq (read-line stream) 5)))))))

(defun hexagram-title (number)
  (second (assoc number *i-ching-titles* :test #'=)))

(defun hexagram-symbol (number)
  (code-char (+ (1- number) (char-code #\HEXAGRAM_FOR_THE_CREATIVE_HEAVEN))))

(defun hexagram-text (number)
  "Print the text for hexagram number NUMBER."
  (princ
   (subseq *i-ching-text*
           (nth (1- number) *i-ching-index*)
           (nth number *i-ching-index*)))
  (values))
