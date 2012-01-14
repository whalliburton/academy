(in-package :academy)

(defun create-deck ()
  (loop for suit in '(#\black_heart_suit #\black_diamond_suit #\black_spade_suit #\black_club_suit)
        nconc
        (loop for value in '(1 2 3 4 5 6 7 8 9 J Q K A)
              collect (format nil "~A~A" value suit))))

(defun print-deck (card-deck &optional (row-size 13))
  "Print out a playing card deck."
  (print-table (group card-deck row-size)))

(defun play-cards ()
  "Play a game of cards. (incomplete)"
  (print-deck (create-deck)))

