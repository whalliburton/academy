(in-package :academy)

;;; Here we implement a simple single character lookahead parser for the Portable Game
;;; Notation used to record chess positions and games.

;;; http://en.wikipedia.org/wiki/Portable_Game_Notation

(defparameter *pgn-directory* (format nil "~Apgn/" (asdf-system-base-path :academy))
  "The directory that holds all the pgn files.")
(defparameter *sample-chess-game*
  (pathname (format nil "~Afischer-spassky-1972-round-6.pgn" *pgn-directory*)))

(defun choose-pgn ()
  (choose-file *pgn-directory* "*.pgn" :prompt "Choose a chess file:" :default *sample-chess-game*))

(defun parse-pgn-file (&key (pgn (choose-pgn)))
  (with-open-file (stream pgn)
    (parse-pgn-stream stream)))

(defun parse-pgn-stream (stream)
  (loop
    for game = (parse-pgn-game stream)
    while game
    collect game))

(defun parse-pgn-game (stream)
  (let ((tokens
          (loop
            for token = (parse-pgn-token stream)
            while token
            when (not (eq token :ignore))
            collect token))
        (movements
          (loop
            for movement = (parse-pgn-movement stream)
            while movement
            collect movement)))
    (when tokens
      (append tokens (when movements `((movements ,movements)))))))

(defun whitespacep (char)
  (member char '(#\space #\tab #\newline #\return)))

(defun read-whitespace (stream)
  (loop
    while (whitespacep (peek-char nil stream nil))
    do (read-char stream)))

(defmacro consume (&rest chars)
  `(if (not (member (peek-char nil stream nil) ',chars :test #'equal))
     nil
     (read-char stream)))

(defun parse-pgn-token (stream)
  (read-whitespace stream)
  (or (consume #\[) (return-from parse-pgn-token))
  (let ((token (read stream))
        (value (read stream)))
    (read-line stream)
    (if (equal value "?")
      :ignore
      (list
       token
       (case token
         (fen (parse-fen-string value))
         (t value))))))

(defun parse-pgn-movement (stream)
  (read-whitespace stream)
  (if (consume #\*)
    (return-from parse-pgn-movement)
    (let ((chars
            (loop for next = (peek-char nil stream nil)
                  while (and next (digit-char-p next))
                  collect (read-char stream))))
      (unless chars
        (return-from parse-pgn-movement))
      (loop while (consume #\. #\space))
      (list
       (parse-integer (coerce chars 'string))
       (parse-move stream)
       (parse-move stream)))))

(defun parse-move (stream)
  (read-whitespace stream)
  (let* ((first (let ((char (peek-char nil stream nil)))
                  (when (and char (char= char #\[))
                    (return-from parse-move))
                  char))
         (piece
           (let ((el
                   (case first
                     (#\P :pawn)
                     (#\N :knight)
                     (#\B :bishop)
                     (#\R :rook)
                     (#\Q :queen)
                     (#\K :king))))
             (cond
               (el (read-char stream) el)
               (t (case first
                    (#\O :castling)
                    ((#\1 #\0) :end)
                    (#\* :in-progress)
                    (t :pawn))))))
         (rest
           (loop
             for next = (peek-char nil stream nil)
             while (and next (not (member next '(#\space #\newline #\return))))
             collect (read-char stream)))
         (string (coerce rest 'string)))
    (cons piece (case piece
                  (:castling (cond
                               ((equal string "O-O") '(:kingside))
                               ((equal string "O-O-O") '(:queenside))))
                  (:end (cond
                          ((equal string "1-0") '(:white-wins))
                          ((equal string "0-1") '(:black-wins))
                          ((equal string "1/2-1/2") '(:draw))))
                  (t (subst :takes #\x rest))))))

;;; http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation

(defun parse-fen-string (string)
  (destructuring-bind
      (placement active-color castling en-passant halfmove-clock fullmove-number)
      (split-string #\space string)
    (list :placement (chess-board-from-fen-placement placement)
          :active-color active-color
          :castling castling
          :en-passant en-passant
          :halfmove-clock halfmove-clock
          :fullmove-number fullmove-number)))

(defun chess-board-from-fen-placement (placement)
  (make-chess-board
   (let ((rows (split-string #\/ placement)))
     (loop for row in rows
           for i = 0
           collect
              (loop for el across row
                    nconcing (if (digit-char-p el)
                               (loop for x from 1 to (- (char-code el) (char-code #\0))
                                     collect nil)
                               (list
                                (list
                                 (if (upper-case-p el) :W :B)
                                 (ecase (char-downcase el)
                                   (#\p :p)
                                   (#\n :N)
                                   (#\b :N)
                                   (#\r :R)
                                   (#\q :Q)
                                   (#\k :K))))))))))

(defun piece-to-char (piece)
  (case piece
    (:knight "N")
    (:bishop "B")
    (:rook "R")
    (:queen "Q")
    (:king "K")
    (t "")))

(defun movement-to-string (movement)
  (flet ((wtakes (move) (subst #\x :takes move)))
    (destructuring-bind (count white black) movement
      (list
       (format nil "~A." count)
       (format nil "~A~{~A~} ~A~{~A~}"
               (piece-to-char (car white)) (wtakes (cdr white))
               (piece-to-char (car black)) (wtakes (cdr black)))))))

(defun show-chess-file (&key (pgn (choose-pgn)))
  "Show the contents of a chess game file."
  (let ((pgn (parse-pgn-file :pgn pgn))
        board)
    (loop for match in pgn
          do (format t "~2%")
             (let ((movements (assoc 'movements match))
                   (fen (assoc 'fen match)))
               (print-table
                (loop for row in match
                      when (not (member row (list movements fen)))
                      collect row))
               (when fen
                 (format t "~%")
                 (setf board (getf (second fen) :placement))
                 (print-chess-board))
               (when movements
                 (format t "~%")
                 (let ((board (or board (make-chess-board))))
                   (print-table (group (flatten
                                        (mapcar #'movement-to-string (second movements))) 8))))))))
