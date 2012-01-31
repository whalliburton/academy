(in-package :academy)

;;; A little tribute to where it all began for many....

;;; An apple DOS 3.3 disk image reader and applesoft decoder.

(defparameter *apple-disk*
  (format nil "~Avintage/prisoner.dsk" (asdf-system-base-path :academy)))

;; 35 tracks per each 5 1/4 floppy disk side
(defun split-disk-image-into-tracks (disk-image)
  (let* ((length (length disk-image))
         (track-size (/ length 35)))
    (loop for i = 0 then (+ i track-size)
          while (<= (+ i track-size) length)
          collect (subseq disk-image i (+ i track-size)))))

;; ASCII text data in the images has the high bit set, so we chop it off before
;; converting it to a native string.
(defun bytes-to-string (array)
  (sb-ext:octets-to-string (map-into array (lambda (el) (max 0 (- el 128))) array)
                           :external-format :ascii))

(defun extract-sector (index track)
  (subseq track (* index 256) (min (length track) (* (1+ index) 256))))

(defun extract-apple-disk-image (disk-image)
  (unless (= (length disk-image) (* 16        ; sectors
                                    35        ; tracks
                                    256))     ; bytes
    (error "Invalid size for apple disk image. ~A" (length disk-image)))
  (loop
    with tracks = (split-disk-image-into-tracks disk-image)
    with vtoc = (extract-sector 0 (nth 17 tracks))
    with track = (aref vtoc 1)   ; first catalog sector track
    and sector = (aref vtoc 2)   ; first catalog sector sector
    while track
    nconc
       (let* ((sector (extract-sector sector (nth track tracks)))
              (next-track (aref sector 1))
              (next-sector (aref sector 2)))
         (if (zerop next-track)
           (setf track nil)
           (setf track next-track sector next-sector))
         (read-apple-dos-file-descriptions sector tracks))))

(defparameter *list-only* nil)
(defparameter *list-file* nil)

(defun read-apple-dos-file-descriptions (catalog tracks)
  (loop for entry in (loop for i = #x0b then (+ i 35)
                           while (< (+ i 34) (length catalog))
                           collect (subseq catalog i (+ i 34)))
        when (not (zerop (aref entry 0)))
        nconcing
           (let* ((file-code
                    (ecase (logand #x7f (aref entry 2))
                      (0 :text)
                      (1 :integer-basic)
                      (2 :applesoft-basic)
                      (4 :binary)
                      (8 :S)
                      (10 :relocatable)
                      (20 :a)
                      (40 :b)))
                  (file-name  (string-right-trim '(#\Space)
                                                 (bytes-to-string (subseq entry 3 20)))))
             (when (or (null *list-file*)
                       (equal *list-file* file-name))
               (list
                (nconc
                 (list file-name file-code)
                 (unless *list-only*
                     (list (collect-file-tracks
                            file-code
                            tracks
                            (extract-apple-file tracks
                                                (aref entry 0) ; first track
                                                (aref entry 1) ; first sector
                                                ))))))))))

(defun extract-apple-file (tracks first-track first-sector)
  (let* ((track (nth first-track tracks))
         (sector (and track (extract-sector first-sector track))))
    (when sector
      (when (or (plusp (aref sector 1))  ; next T/S track
                (plusp (aref sector 2))) ; next T/S sector
        (error "Secondary T/S not yet handled."))
      (loop for i = #x0c then (+ i 2)
            while (< i #xFF)
            when (not (zerop (aref sector i)))
            collect (cons (aref sector i)
                          (aref sector (1+ i)))))))

(defmethod collect-file-tracks ((file-code (eql :text)) tracks sectors)
  (apply #'concatenate 'string
         (loop for sector-data
                 in (loop for sector in sectors
                          collect (extract-sector (cdr sector) (nth (car sector) tracks)))
               collect (bytes-to-string sector-data))))

(defmethod collect-file-tracks ((file-code (eql :binary)) tracks sectors)
  (let ((raw
         (apply #'concatenate 'vector
                (loop for sector-data
                        in (loop for sector in sectors
                                 collect (extract-sector (cdr sector)
                                                         (nth (car sector) tracks)))
                      collect sector-data))))
  (subseq raw 4 (logior (ash (aref raw 3) 8) (aref raw 2)))))

(defun collect-basic-tracks (tracks sectors)
  (let ((raw
         (apply #'concatenate 'vector
                (loop for sector-data
                        in (loop for sector in sectors
                                 collect (extract-sector (cdr sector)
                                                         (nth (car sector) tracks)))
                      collect sector-data))))
    (subseq raw 2 (logior (ash (aref raw 1) 8) (aref raw 0)))))

(defun decode-applesoft (data)
  (let ((index 0)
        (length (length data)))
    (flet ((next-byte ()
             (if (= index length)
               (throw 'end nil)
               (prog1
                   (aref data index)
                 (incf index)))))
      (loop
        for line
           =
           (catch 'end
             (list (logior (next-byte) (ash (next-byte) 8))  ; offset
                   (logior (next-byte) (ash (next-byte) 8))  ; size
                   (loop for el = (next-byte)
                         while (not (zerop el))
                         collect el)))
        while line
        collect line))))

(defparameter *applesoft-tokens*
  #("END"       "FOR"   "NEXT"  "DATA"
   "INPUT"      "DEL"   "DIM"   "READ"
   "GR"         "TEXT"  "PR#"           "IN#"
   "CALL"       "PLOT"  "HLIN"  "VLIN"
   "HGR2"       "HGR"   "HCOLOR="       "HPLOT"
   "DRAW"       "XDRAW" "HTAB"  "HOME"
   "ROT="       "SCALE="        "SHLOAD"        "TRACE"
   "NOTRACE"    "NORMAL"        "INVERSE"       "FLASH"
   "COLOR="     "POP"   "VTAB"  "HIMEM:"
   "LOMEM:"     "ONERR" "RESUME"        "RECALL"
   "STORE"      "SPEED="        "LET"   "GOTO"
   "RUN"        "IF"            "RESTORE"       "&"
   "GOSUB"      "RETURN"        "REM"   "STOP"
   "ON"         "WAIT"  "LOAD"  "SAVE"
   "DEF"        "POKE"  "PRINT" "CONT"
   "LIST"       "CLEAR" "GET"   "NEW"
   "TAB("       "TO"            "FN"            "SPC("
   "THEN"       "AT"            "NOT"   "STEP"
   "+"          "-"             "*"             "/"
   "^"          "AND"   "OR"            ">"
   "="          "<"             "SGN"   "INT"
   "ABS"        "USR"   "FRE"   "SCRN("
   "PDL"        "POS"   "SQR"   "RND"
   "LOG"        "EXP"   "COS"   "SIN"
   "TAN"        "ATN"   "PEEK"  "LEN"
   "STR$"       "VAL"   "ASC"   "CHR$"
   "LEFT$"      "RIGHT$"        "MID$"  ""
   "SYNTAX"                     "RETURN WITHOUT GOSUB"
   "OUT OF DATA"                "ILLEGAL QUANTITY"
   "OVERFLOW"                   "OUT OF MEMORY"
   "UNDEF'D STATEMENT"          "BAD SUBSCRIPT"
   "REDIM'D ARRAY"              "DIVISION BY ZERO"
   "ILLEGAL DIRECT"             "TYPE MISMATCH"
   "STRING TOO LONG"            "FORMULA TOO COMPLEX"
   "CAN'T CONTINUE"             "UNDEF'D FUNCTION"
   "ERROR \a"   ""              ""              ""))

(defun tokenify-applesoft-lines (lines)
  (loop for (offset line-number data) in lines
        collect (list line-number
                      (loop for el in data
                            if (> el #x7f)
                            collect (aref *applesoft-tokens* (logand el #x7f))
                            else collect (code-char el)))))

(defun print-applesoft-lines (lines &optional (stream t))
  (loop for (line-number tokens) in lines
        do (format stream "~A  " line-number)
           (let ((on-space t))
             (flet ((out (token) (princ token stream) (setf on-space nil))
                    (space () (princ " " stream) (setf on-space t)))
               (loop for token in tokens
                     do (cond
                          ((and (stringp token)
                                (not (member token '("CHR$" "=" "-" "+" "/"
                                                     ">" "<" "RND" "SGN"
                                                     "INT" "NOT" "LEN"
                                                     "ASC" "MID$" "SPC("
                                                     "PEEK" "VAL" "ABS"
                                                     "FRE") :test 'string=)))
                           (when (member token '("THEN" "TO"
                                                 "GOTO" "GOSUB"
                                                 "OR" "AND") :test 'string=)
                             (unless on-space (space)))
                           (out token) (space))
                          ((and (characterp token) (position token ";:"))
                           (when (position token ";:") (space))
                           (out token) (space))
                          (t (out token))))))
           (fresh-line stream)))

(defmethod collect-file-tracks ((file-code (eql :applesoft-basic)) tracks sectors)
  (with-output-to-string (stream)
    (print-applesoft-lines
     (tokenify-applesoft-lines
      (decode-applesoft (collect-basic-tracks tracks sectors)))
     stream)))

(defmethod collect-file-tracks ((file-code (eql :integer-basic)) tracks sectors)
  (collect-basic-tracks tracks sectors))

(defmethod collect-file-tracks (file-code tracks sectors)
  (error "Unhandled apple file type ~A." file-code))

(defun show-apple-disk-catalog (&optional (disk *apple-disk*))
  (format t "Catalog of DOS disk ~S.~%" disk)
  (let ((*list-only* t))
    (print-table (extract-apple-data (slurp-binary-file disk)))))

(defun show-apple-disk-file (filename &optional (disk *apple-disk*))
  (let ((*list-file* filename))
    (let ((data (first (extract-apple-data (slurp-binary-file disk)))))
      (if (null data)
        (warn "No file named ~S found in ~S." filename disk)
        (format t "~A ~A~%~A~%" (first data) (second data) (third data))))))
