(in-package :academy)

(defun make-bitmap (width height &optional contents)
  (if contents
    (make-array (list height width) :initial-contents contents)
    (make-array (list height width) :initial-element nil)))

(defvar *bitmap*)

(defmacro with-bitmap ((width height) &body body)
  `(let ((*bitmap* (make-bitmap ,width ,height)))
     ,@body))

(defun set-pixel (x y &optional (bitmap *bitmap*))
  (destructuring-bind (height width) (array-dimensions bitmap)
    (unless (or (< x 0) (< y 0) (>= x width) (>= y height))
      (setf (aref bitmap y x) t))))

(defun draw (&optional (bitmap *bitmap*))
  (destructuring-bind (height width) (array-dimensions bitmap)
    (loop for y from 0 to (1- height) by 2
          do (loop for x from 0 to (1- width)
                   do (princ
                       (let ((top (aref bitmap y x))
                             (bottom (when (< y (1- height)) (aref bitmap (1+ y) x))))
                         (cond
                           ((and top bottom) #\FULL_BLOCK)
                           (top              #\UPPER_HALF_BLOCK)
                           (bottom           #\LOWER_HALF_BLOCK )
                           (t                #\space)))))
             (fresh-line))))

(defun draw-from-list (bit-list width)
  (let ((rows (group bit-list width)))
    (draw (make-array (list (length rows) width) :initial-contents rows))))

(defun copy-onto-bitmap (bitmap pattern x y)
  (loop for row in pattern
        for yi from y
        do (loop for character across row
                 for xi from x
                 do (setf (aref bitmap yi xi) (not (eq character #\space))))))

(defun center-on-bitmap (bitmap pattern)
  (destructuring-bind (height width) (array-dimensions bitmap)
    (copy-onto-bitmap bitmap pattern
                      (- (floor width 2) (floor (length (car pattern)) 2))
                      (- (floor height 2) (floor (length pattern) 2)))))

(defun pattern-to-bitmap (pattern)
  (let ((bitmap (make-bitmap (length (car pattern)) (length pattern))))
    (loop for row in pattern
          for y from 0
          do (loop for character in (coerce row 'list)
                   for x from 0
                   do (when (not (char= character #\space))
                        (setf (aref bitmap y x) t))))
    bitmap))

(defun smile ()
  "When you're smiling, the whole world smiles with you."
  (draw (pattern-to-bitmap '("  ****  "
                             " *    * "
                             "* *  * *"
                             "*      *"
                             "* *  * *"
                             "*  **  *"
                             " *    * "
                             "  ****  "))))

(defun draw-border (&optional (bitmap *bitmap*))
  (destructuring-bind (height width) (array-dimensions bitmap)
    (loop for x from 0 to (1- width)
          do (setf (aref bitmap 0 x) t
                   (aref bitmap (1- height) x) t))
    (loop for y from 1 to (- height 2)
          do (setf (aref bitmap y 0) t
                   (aref bitmap y (1- width)) t))))

;;; Described in
;;; Computer Graphics - Principles and Practice by Donald Hearn and M. Pauline Baker

(defun draw-circle (x-center y-center radius &optional (bitmap *bitmap*))
  (labels ((pixel (x y) (set-pixel (+ x-center x) (+ y-center y) bitmap))
           (draw-points (x y)
             (pixel x     y)
             (pixel (- x) y)
             (pixel x     (- y))
             (pixel (- x) (- y))
             (pixel y     x)
             (pixel (- y) x)
             (pixel y     (- x))
             (pixel (- y) (- x))))
    (loop with x = 0
          with y = radius
          with p = (- 1 radius)
          initially (draw-points x y)
          while (< x y)
          do (incf x)
             (if (< p 0)
               (incf p (+ (* 2 x) 1))
               (progn
                 (decf y)
                 (incf p (+ (* 2 (- x y)) 1))))
             (draw-points x y))))

(defun bullseye (&optional (size 64) (step 4))
  "Draw a bullseye."
  (with-bitmap (size size)
    (let ((mid (floor size 2)))
      (loop for radius from 2 to mid by step
            do (draw-circle mid mid radius))
      (draw))))

(defun draw-line (xa ya xb yb &optional (bitmap *bitmap*))
  (let* ((dx (- xb xa))
         (dy (- yb ya))
         (steps (if (> (abs dx) (abs dy)) (abs dx) (abs dy)))
         (xi (/ dx steps))
         (yi (/ dy steps)))
    (set-pixel xa ya bitmap)
    (loop with x = xa
          with y = ya
          for k from 0 to (1- steps)
          do (incf x xi)
             (incf y yi)
             (set-pixel (floor x) (floor y) bitmap))))

(defun sunbeam (&key (step 8) (size 64))
  "Draw a sunbeam."
  (with-bitmap (size size)
    (loop for x from 0 to size by step
          do (draw-line 0 (1- size) x 0)
             (draw-line 0 (1- size) (1- size) x))
    (draw)))
