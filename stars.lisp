(in-package :academy)

(defun load-stars (&optional count)
  (with-open-file (stream (data-path "yale.star") :direction :input :if-does-not-exist :error
                                                  :external-format :latin1)
    (remove nil
            (loop
              for x from 1 to (or count #xFFFFFF)
              collect
              (let ((row (handler-case
                             (read-line stream)
                           (end-of-file () (return rows)))))
                (parse-yale-star-line row))
                into rows
              finally (return rows)))))

;; File format for yale.star (reduced Yale star catalog, version 2)

;; Name    Col     Len    Type     Desc
;; -------------------------------------------------
;; ra	1	6	i6	ra  2000 hhmmss
;; dec	7	5	+-i4	dec 2000 -mmss
;; mag	12	3	{-}i	magnitude*100 -dd or -ddd
;; type	15	2	c2	object type
;; type1	15	1	c1	object type 1	/ star,ga,cl,neb,etc
;; type2	16	1	c1	object type 2	/ dbl,open,cl,etc
;; spec2	17	2	c1d1	spectral 2 'G2'
;; letter	19	2	c2	greek Bayer letter(s), or Flamsteed number
;; const	21	3	c3	constellation (I.A.U. designation)
;; name	24	30	c*	name,descrip.
;; newline	54	1	c1	newline (max final loc)

(defvar *line*)

(defun ra-to-deg (ra)
  (float
   (/ (* (+ (* (parse-integer ra :end 2) 60 60)
            (* (parse-integer ra :start 2 :end 4) 60)
            (parse-integer ra :start 4 :end 6))
         360)
      (* 60 60 24))))

(defun dec-to-deg (dec)
  (float
   (/
    (* (if (char= (char dec 0) #\-) -1 1)
       (+ (* (parse-integer dec :start 1 :end 3) 60)
          (parse-integer dec :start 3 :end 5)))
    60)))

(defun parse-yale-star-line (line)
  (unless (< (length line) 19)
    (let ((ra (subseq line 0 6))
          (dec (subseq line 6 11))
          (mag (subseq line 11 14))
          (type (subseq line 14 16))
          (spec2 (subseq line 16 18))
          (letter (subseq line 18 20))
          (const (subseq line 20 23))
          (name (subseq line 23)))
      (list (ra-to-deg ra) (dec-to-deg dec) mag type spec2 letter const name))))

(defparameter *stars* (load-stars))

;; http://www2.arnes.si/~gljsentvid10/starmap.html
;;
;; x' = cos (L)* sin (W)
;; y' = sin (L)
;; z' = cos (L) *cos (W)

;; These are the cartesian coordinates of the star if
;; the North Pole has coordinates (0,1,0) and the projection
;; point has coordinates (0,0,1), and the South Pole has
;; projection points (0,-1,0). The star is at a distance of
;; 1 from the centre.

;; The coordinates in the plane of the projection are given by

;; X = x' / (1 + z')
;; Y = y' / (1 + z')

;; Worked case - projection of Deneb with projection point 0h
;; 0 Dec

;; Deneb has J2000.0 coordinates of about RA 20h 41m 26s and
;; DEC 45 deg 16' 49 "

;; This gives (converting to degrees)

;; W = -49.6421  L = 45.2803

;; x' = -0.536183
;; y' =  0.710558
;; z' =  0.455649

;; X = -0.3683
;; Y =  0.4881

;; You might check the X, Y coordinates of the following W (longitude),
;; L (latitude) values used for the 'frame' of the star chart.

;; ( W, L)    -->  (X, Y)
;; (-40, 60)   --> (-0.232385, 0.626183)
;; ( 40, 60)   --> ( 0.232385, 0.626183)
;; (-40, -30)  --> (-0.334655,-0.300587)
;; ( 40, -30)  --> ( 0.334655,-0.300587)

(defun steriographic-projection (ra dec)
  (let* ((w (degrees-to-radians ra))
         (l (degrees-to-radians dec))
         (x (* (cos l) (sin w)))
         (y (sin l))
         (z (* (cos l) (cos w)))
         (x2 (/ x (+ 1 z)))
         (y2 (/ y (+ 1 z))))
    (values (- x2) (- y2))))

(defun scale (x y w h xl xh yl yh)
  (let ((dx (/ w (- xh xl)))
        (dy (/ h (- yh yl))))
    (values
     (float (- (* x dx) (* dx xl)))
     (float (- (* y dy) (* dy yl))))))

(defun draw-scale (bitmap xl xh yl yh)
  (destructuring-bind (height width) (array-dimensions bitmap)
    (set-pixel 0 0 bitmap (format nil "⇱ ~A,~A" xl yh))
    (let ((str (format nil "(~A,~A)⇲ " xh yl)))
      (set-pixel (- width (length str)) (1- height) bitmap str))))

(defun stars (&key (count 40) named table draw-scale (scale '(0 360 -90 90))
                (width 80) (height 46) project)
  (if table
      (print-table (if named
                       (remove-if (lambda (el) (zerop (length (eighth el)))) *stars*)
                       *stars*))
      (with-bitmap (width height)
        (when project (setf scale '(-1.5 1.5 -2 2)))
        (when draw-scale
          (apply #'draw-scale *bitmap* scale))
        (loop for star in *stars*
              for index from 1 to count
              do
                 (destructuring-bind (ra dec mag type spec2 letter const name) star
                   (multiple-value-bind (x y)
                       (multiple-value-bind (ux uy)
                           (if project
                               (steriographic-projection ra dec)
                               (values ra dec))
                         (apply #'scale ux uy width height scale))
                     (set-pixel (round x) (round y) *bitmap*
                                (if named
                                    (let ((str (or (unless (string= name "") name)
                                                   (format nil "~A~A" const letter))))
                                      (format nil "⋆~A"
                                              (if (and (string-equal named :short)
                                                       (< 4 (length str)))
                                                  (subseq str 0 4)
                                                  str)))
                                    "⋆")))))
        (draw *bitmap*))))
