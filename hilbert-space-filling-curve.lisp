(in-package :academy)

;;; Algorithm 781

(defun hilbert-points (width)
  (unless (power-of-two-p width)
    (error "The specified width ~A is not a power of two." width))
  (let (points)
    (labels ((recur (x y lg i1 i2)
               (if (= lg 1)
                 (push (cons x y) points)
                 (let ((lg (floor lg 2)))
                   (recur (+ x (* i1 lg))       (+ y (* i1 lg))       lg i1       (- 1 i2))
                   (recur (+ x (* i2 lg))       (+ y (* (- 1 i2) lg)) lg i1       i2)
                   (recur (+ x (* (- 1 i1) lg)) (+ y (* (- 1 i1) lg)) lg i1       i2)
                   (recur (+ x (* (- 1 i2) lg)) (+ y (* i2 lg))       lg (- 1 i1) i2)))))
      (recur 0 0 width 0 0))
    (nreverse points)))

(defun hilbert-space-filling-curve (&optional (width 64))
  "Draw one of Hilbert's continuous fractal space-filling curves."
  (let ((points (hilbert-points width))
        (grid (make-array (list width width))))
    (let ((start (calculate-box-graphic (first points) (second points) (third points) t)))
      (setf (aref grid 0 0) start
            (aref grid (1- width) 0) start))
    (loop for (from to next) on points
          while next
          do (setf (aref grid (car to) (cdr to))
                   (calculate-box-graphic from to next)))
    (loop for y from 0 to (1- width)
          do (loop for x from 0 to (1- width)
                   do (princ (aref grid x y)))
             (fresh-line))))

(defun calculate-box-graphic (from to next &optional start)
  (flet ((direction (from to)
           (flet ((x (loc) (car loc))
                  (y (loc) (cdr loc)))
             (cond
               ((< (x from) (x to)) :left)
               ((> (x from) (x to)) :right)
               ((< (y from) (y to)) :up)
               ((> (y from) (y to)) :down)))))
    (let ((in (direction from to))
          (out (direction next to)))
      (if start
        (ecase in
          (:up #\BOX_DRAWINGS_LIGHT_VERTICAL)
          (:left #\BOX_DRAWINGS_LIGHT_HORIZONTAL))
        (second (assoc-if (lambda (el)
                            (or (and (eq (first el) in)
                                     (eq (second el) out))
                                (and (eq (first el) out)
                                     (eq (second el) in))))
                          '(((:up    :right) #\BOX_DRAWINGS_LIGHT_UP_AND_RIGHT)
                            ((:up    :left)  #\BOX_DRAWINGS_LIGHT_UP_AND_LEFT )
                            ((:down  :right) #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT)
                            ((:down  :left)  #\BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT)
                            ((:left  :right) #\BOX_DRAWINGS_LIGHT_HORIZONTAL)
                            ((:up    :down)  #\BOX_DRAWINGS_LIGHT_VERTICAL))))))))
