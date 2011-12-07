(in-package :academy)

(defvar *turtle*)

(defstruct turtle x y heading bitmap pen)

(defmacro with-turtle ((&optional (bitmap '*bitmap*)) &body body)
  `(destructuring-bind (height width) (array-dimensions ,bitmap)
     (let ((*turtle* (make-turtle :x (floor width 2) :y (floor height 2) :heading 0
                                  :bitmap ,bitmap :pen :down)))
       ,@body)))

(defun pen-up (&optional (turtle *turtle*))
  (setf (turtle-pen turtle) :up))

(defun pen-is-up (&optional (turtle *turtle*))
  (eq (turtle-pen turtle) :up))

(defun pen-down (&optional (turtle *turtle*))
  (setf (turtle-pen turtle) :down))

(defun pen-is-down (&optional (turtle *turtle*))
  (eq (turtle-pen turtle) :down))

(defun left (degrees &optional (turtle *turtle*))
  (setf (turtle-heading turtle)
        (mod (- (turtle-heading turtle) degrees) 360)))

(defun right (degrees &optional (turtle *turtle*))
  (setf (turtle-heading turtle)
        (mod (+ (turtle-heading turtle) degrees) 360)))

(defmacro repeat (times &body body)
  `(dotimes (,(gensym) ,times)
     ,@body))

(defun move-to (x y &optional (turtle *turtle*))
  (when (pen-is-down turtle)
    (draw-line (round (turtle-x turtle)) (round (turtle-y turtle)) (round x) (round y)
               (turtle-bitmap turtle)))
  (setf (turtle-x turtle) x
        (turtle-y turtle) y))

(defun degrees-to-radians (degrees)
  (/ (* degrees pi) 180))

(defun forward (steps &optional (turtle *turtle*))
  (let ((x (turtle-x turtle))
        (y (turtle-y turtle))
        (heading (- (turtle-heading turtle) 90)))
    (let* ((dx (* steps (cos (degrees-to-radians heading))))
           (dy (* steps (sin (degrees-to-radians heading)))))
      (move-to (+ x dx) (+ y dy) turtle))))

(defun backward (steps &optional (turtle *turtle*))
  (forward (- steps) turtle))

(defmacro turtle-graphics ((&key (width 32) (height 32) x y) &body body)
  `(with-bitmap (,width ,height)
     (with-turtle ()
       ,@(when x `((setf (turtle-x *turtle*) ,x)))
       ,@(when y `((setf (turtle-y *turtle*) ,y)))
       ,@body
       (draw))))

(defun turtle-race ()
  "Sure and steady wins the race."
  (macrolet ((races (&rest name-bodies)
               `(progn
                  ,@(loop for (name arguments . body) in name-bodies
                          collect `(format t "~(~A~)~%" ',name)
                          collect `(turtle-graphics ,arguments ,@body)
                          collect `(terpri)))))
    (races
     (squares
      (:x 2 :y 30)
      (loop for width from 2 to 24 by 4
            do (repeat 4
                 (forward width)
                 (right 90))))
     (squares-rotated
      (:x 2 :y 16)
      (right 45)
      (loop for width from 2 to 20 by 4
            do (repeat 4
                 (forward width)
                 (right 90))))
     (triangle
      (:x 2 :y 30)
      (repeat 3
        (forward 24)
        (right 120)))
     (circle
      (:x 2 :y 20)
      (repeat 26
        (forward 3)
        (right (/ 360 26))))
     (spiral
      (:x 13 :y 16)
      (loop for size from 2 to 10 by 0.3
            do (forward size)
               (right 35)))
     (box-spiral
      (:width 64 :height 64)
      (loop for a from 3 to 50 by 2
            do (forward a)
               (right 91)))
     (star
      (:width 64 :height 64 :x 20 :y 60)
      (repeat 7
        (forward 50)
        (right (- 180 (/ 180 7))))))))
