(in-package :academy)

(defun make-bitmap (width height &optional contents)
  (if contents
    (make-array (list height width) :initial-contents contents)
    (make-array (list height width) :initial-element nil)))

(defvar *bitmap*)
(defvar *bitmap-overwrite* nil)

(defmacro with-bitmap ((width height) &body body)
  `(let ((*bitmap* (or *bitmap-overwrite* (make-bitmap ,width ,height))))
     ,@body))

(defun outside-bounds (x y &optional (bitmap *bitmap*))
  (destructuring-bind (height width) (array-dimensions bitmap)
    (or (< x 0) (< y 0) (>= x width) (>= y height))))

(defun set-pixel (x y &optional (bitmap *bitmap*) (value t))
  (unless (outside-bounds x y bitmap)
    (setf (aref bitmap y x) value)))

(defvar *comic-strip*)
(defvar *save-drawing-name* nil)

(defun draw (&optional (bitmap *bitmap*))
  (cond
    ((and (boundp '*comic-strip*) (not (eq bitmap (comic-strip-bitmap *comic-strip*))))
     (draw-on-comic-strip *comic-strip* bitmap))
    (*save-drawing-name* (save-bitmap *save-drawing-name*))
    (t (destructuring-bind (height width) (array-dimensions bitmap)
         (loop for y from 0 to (1- height) by 2
               do (loop for x from 0 to (1- width)
                        do (princ
                            (let ((top (aref bitmap y x))
                                  (bottom (when (< y (1- height)) (aref bitmap (1+ y) x))))
                              (cond
                                ((or (stringp top) (stringp bottom))
                                 (incf x (length (or top bottom)))
                                 (or top bottom))
                                ((and top bottom) #\FULL_BLOCK)
                                (top              #\UPPER_HALF_BLOCK)
                                (bottom           #\LOWER_HALF_BLOCK )
                                (t                #\space)))))
                  (fresh-line))))))

(defun draw-from-list (bit-list width)
  (let ((rows (group bit-list width)))
    (draw (make-array (list (length rows) width) :initial-contents rows))))

(defun copy-onto-bitmap (bitmap pattern x y)
  (loop for row in pattern
        for yi from y
        do (loop for character across row
                 for xi from x
                 do (setf (aref bitmap yi xi) (not (eq character #\space)))))
  bitmap)

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

(defun bullseye (&key (size 64) (step 4) filled (draw t))
  "Draw a bullseye."
  (with-bitmap (size size)
    (let ((mid (floor size 2)))
      (loop for radius from 2 to mid by step
            do (draw-circle mid mid radius))
      (when filled
        (loop for x from 2 to mid by (* 2 step)
              do (fill-bitmap (+ mid x 1) mid)))
      (if draw
        (draw)
        *bitmap*))))

(defun moiré (&key (size 64) (step 4) (filled t) (offset 16))
  "Draw a Moiré pattern."
  (let ((*bitmap* (make-bitmap (+ size offset) size))
        (one (bullseye :size size :step step :filled filled :draw nil)))
    (copy-bitmap-onto-bitmap one *bitmap* 0 0)
    (copy-bitmap-onto-bitmap one *bitmap* offset 0)
    (draw *bitmap*)))

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

(defun fill-bitmap (x y &optional (bitmap *bitmap*))
  (unless (outside-bounds x y bitmap)
    (unless (aref bitmap y x)
      (setf (aref bitmap y x) t)
      (fill-bitmap (+ x 1) y bitmap)
      (fill-bitmap (- x 1) y bitmap)
      (fill-bitmap x (+ y 1) bitmap)
      (fill-bitmap x (- y 1) bitmap))))

(defun draw-filled-circle (x-center y-center radius &optional (bitmap *bitmap*))
  (draw-circle x-center y-center radius bitmap)
  (fill-bitmap x-center y-center bitmap))

(defun sun (&key (size 64))
  "Draw a sun."
  (with-bitmap (size size)
    (let ((mid (floor size 2)))
      (draw-filled-circle mid mid (1- mid))
      (draw))))

;;; Attention Hackers! Exercises are good for the soul.
;;;
;;;    Someone with the desire could expand PEACE to draw peace symbols of any size.

(defun peace ()
  "Peace on Earth."
  (with-bitmap (12 12)
    (draw-circle 6 6 5)
    (draw-line 6 10 6 1)
    (draw-line 6 6 3 9)
    (draw-line 6 6 9 9)
    (draw)))

(defun copy-bitmap-onto-bitmap (from-bitmap to-bitmap x y &key (fn (lambda (a b) (or a b))))
  (destructuring-bind (height width) (array-dimensions from-bitmap)
    (loop for yi from 0 to (1- height)
          do (loop for xi from 0 to (1- width)
                   do (let ((from (aref from-bitmap yi xi))
                            (to (aref to-bitmap (+ y yi) (+ x xi))))
                        (set-pixel (+ x xi) (+ y yi)
                                   to-bitmap
                                   (funcall fn from to)))))))

(defun center-bitmap-onto-bitmap (from-bitmap to-bitmap)
  (destructuring-bind (fh fw) (array-dimensions from-bitmap)
    (destructuring-bind (th tw) (array-dimensions to-bitmap)
      (copy-bitmap-onto-bitmap from-bitmap to-bitmap
                               (floor (- tw fw) 2)
                               (floor (- th fh) 2)))))

(defstruct comic-strip bitmap width height columns rows column)

(defmacro with-comic-strip ((&key (width 32) (height 32) (columns 3) (action 'draw)) &body body)
  `(let ((*comic-strip* (make-comic-strip :bitmap (make-bitmap (* ,width ,columns) 0)
                                          :width ,width :height ,height :columns ,columns
                                          :rows 0 :column 0)))
     ,@body
     (,action (comic-strip-bitmap *comic-strip*))))

(defun draw-on-comic-strip (strip cell-bitmap)
  (with-slots (rows column width height columns bitmap) strip
    (when (= column 0)
      (incf rows)
      (setf bitmap
            (adjust-array bitmap (list (* rows height) (* columns width)) :initial-element nil)))
    (copy-bitmap-onto-bitmap cell-bitmap bitmap (* column width) (* (1- rows) height))
    (setf column (mod (1+ column) columns))))

(defparameter *image-save-directory* "images")
(defparameter *image-save-inverse* nil)
(defparameter *image-save-directory-overwrite* nil)
(defparameter *image-save-scale* 1)

(defun save-bitmap (name &key (bitmap *bitmap*) (overwrite *image-save-directory-overwrite*)
                              (directory *image-save-directory*) (inverse *image-save-inverse*)
                              (scale *image-save-scale*))
  (ensure-directories-exist (data-path "" :subdirectory directory :verify nil))
  (let ((filename (data-path (format nil "~A.pbm" name) :subdirectory directory :verify nil)))
    (when (and (not overwrite) (probe-file filename))
      (error "An image named ~S already exists." name))
    (format t "saving ~S~%" filename)
    (with-output-to-file (filename)
      (destructuring-bind (height width) (array-dimensions bitmap)
        (format t "P1~%# ~A~%~A ~A~%" name (* width scale) (* scale height))
        (loop for y from 0 to (1- height)
              do (dotimes (i scale)
                   (loop for x from 0 to (1- width)
                         do (dotimes (i scale)
                              (format t "~A " (if (aref bitmap y x)
                                                (if inverse "0" "1")
                                                (if inverse "1" "0")))))
                   (fresh-line)))))))
