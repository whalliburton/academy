(in-package :academy)

(defun age-bitmap (bitmap)
  (destructuring-bind (height width) (array-dimensions bitmap)
    (let ((next-generation (make-bitmap width height)))
      (loop for y from 0 to (1- height)
            do (loop for x from 0 to (1- width)
                     do (let ((neighbors
                                (let ((total 0))
                                  (loop for (dx dy) in '((-1 -1) (0 -1) (1 -1)
                                                         (-1 0)         (1 0)
                                                         (-1 1)  (0 1)  (1 1))
                                        do (let ((xi (+ dx x))
                                                 (yi (+ dy y)))
                                             (unless (or (minusp xi) (minusp yi)
                                                         (= xi width)
                                                         (= yi height))
                                               (when (aref bitmap yi xi)
                                                 (incf total)))))
                                  total)))
                          (setf (aref next-generation y x)
                                (if (aref bitmap y x) ; we got a live one
                                  (cond
                                    ((< neighbors 2) nil)  ; lonliness
                                    ((<= neighbors 3) t)   ; party on dude!
                                    ((> neighbors 3) nil)) ; overcrowding
                                  (when (= neighbors 3) ; kinky!
                                    t))))))
      next-generation)))

(defun life (&key (pattern '("  ***"
                             " *  *"
                             "*   *"))
                  (steps 32)
                  (size 32))
  "Play Conway's Game of Life."
  (let ((grid (make-bitmap size size)))
    (when (or (null pattern) (stringp pattern))
      (setf pattern (load-life-pattern pattern)))
    (cond
      ((consp pattern)
       (center-on-bitmap grid pattern))
      ((arrayp pattern)
       (center-bitmap-onto-bitmap pattern grid)))
    (draw grid)
    (dotimes (x steps)
      (setf grid (age-bitmap grid))
      (draw grid))))

(defun load-life-pattern (&optional name)
  (let ((name
          (or (and (stringp name)
                   (data-path (format nil "~A.RLE" name)
                              :subdirectory "life-patterns" :verify nil))
              (choose-file (data-path "" :subdirectory "life-patterns" :verify nil) "*.RLE"))))
    (with-input-from-file (stream name)
      (let ((dimensions
              (loop for line = (read-line stream)
                    while (string-starts-with line "#C")
                    finally (return line))))
        (assert (string-starts-with dimensions "x ="))
        (destructuring-bind (x y)
            (mapcar (lambda (string)
                      (parse-integer string :start (1+ (position #\= string))))
                    (split-string #\, dimensions))
          (copy-onto-bitmap
           (make-bitmap (1+ x) (1+ y))
           (loop for row from 0
                 for string in
                    (split-string #\$ (remove #\newline (slurp-stream stream)))
                 collect (convert-rle-to-pattern string))
           0 0))))))

(defun convert-rle-to-pattern (rle)
  (with-output-to-string (stream)
    (loop with acc
          for char across rle
          if (digit-char-p char)
          do (push char acc)
          else do (dotimes (x (if acc
                                (parse-integer (coerce (nreverse acc) 'string))
                           1))
                    (write-char (cond
                                  ((char= char #\b) #\space)
                                  ((char= char #\o) #\*)
                                  (t #\space))
                                stream))
                  (setf acc nil))))

(defun create-life-pattern-images ()
  "Create images of all the life patterns."
  (loop for file in (list-files (data-path "" :subdirectory "life-patterns" :verify nil) "*.RLE")
        do (save-bitmap (pathname-name file) :bitmap (load-life-pattern (pathname-name file))
                             :overwrite t :directory "life-images" :inverse t
                             :scale 8)))
