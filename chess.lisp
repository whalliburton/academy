(in-package :academy)

;; http://diovo.com/2012/01/chess-programming-the-0x88-board-representation

(defvar board)

(defmacro with-chess-board (&body body)
  `(let ((board (make-chess-board)))
     ,@body))

(defparameter *initial-chess-board-layout*
  '(((:W :R) (:W :N) (:W :B) (:W :Q) (:W :K) (:W :B) (:W :N) (:W :R))
    ((:W :p) (:W :p) (:W :p) (:W :p) (:W :p) (:W :p) (:W :p) (:W :p))
    (nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil)
    ((:B :p) (:B :p) (:B :p) (:B :p) (:B :p) (:B :p) (:B :p) (:B :p))
    ((:B :R) (:B :N) (:B :B) (:B :Q) (:B :K) (:B :B) (:B :N) (:B :R))))

(defun make-chess-board (&optional (contents *initial-chess-board-layout*))
  (make-array '(8 8) :initial-contents contents))

(defun piece-color (piece) (first piece))
(defun piece-type (piece) (second piece))

(defun chess-piece-character (piece)
  (let ((white (eq (piece-color piece) :W)))
    (ecase (piece-type piece)
      (:K (if white #\white_chess_king #\black_chess_king))
      (:Q (if white #\white_chess_queen #\black_chess_queen))
      (:R (if white #\white_chess_rook #\black_chess_rook))
      (:B (if white #\white_chess_bishop #\black_chess_bishop))
      (:N (if white #\white_chess_knight #\black_chess_knight))
      (:p (if white #\white_chess_pawn #\black_chess_pawn))
      ((nil) #\space))))

(defun print-chess-board (&key with-coordinates with-borders)
  (let ((pad (if with-coordinates "  " "")))
    (when with-borders (format t "~A┌──┬──┬──┬──┬──┬──┬──┬──┐~%" pad))
    (loop for row from 0 to 7
          do (when with-coordinates (format t "~A " (- 8 row)))
             (when with-borders (princ "│"))
             (loop for column from 0 to 7
                   do (princ (chess-piece-character (aref board row column)))
                      (if with-borders
                        (princ " │")
                        (princ #\space)))
             (fresh-line)
             (when (and with-borders (not (= row 7))) (format t "~A├──┼──┼──┼──┼──┼──┼──┼──┤~%" pad)))
    (when with-borders (format t "~A└──┴──┴──┴──┴──┴──┴──┴──┘~%" pad))
    (when with-coordinates
      (princ "  ")
      (loop for column from (char-code #\a) to (char-code #\h)
            do (format t "~A~A " (if with-borders " " "") (code-char column)
                       (if with-borders " " "")))
      (fresh-line))))

(defun expand-move (move)
  (destructuring-bind (piece . movement) move
    (let ((raw (mapcar (lambda (el)
                         (if (characterp el)
                           (if (digit-char-p el)
                             (- (char-code el) (char-code #\0))
                             (symb (char-upcase el)))
                           el)) (remove :takes movement))))
      (cons
       piece
       (ecase (length raw)
         (1 raw)
         (2 (list '? raw))
         (3 (let* ((from (car raw))
                   (digit (numberp from)))
              (list (list (if digit '? from) (if digit from '?)) (cdr raw)))))))))

(defun expand-game (game)
  (loop for (number white black) in game
        for count from 1
        do (when (/= number count)
             (error "Move number mismatch,"))
        collect (list (expand-move white) (expand-move black))))
