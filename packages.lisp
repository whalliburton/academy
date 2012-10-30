(defpackage academy
  (:use common-lisp)
  (:shadow if push)
  (:export help
           petals-around-the-rose
           hilbert-space-filling-curve
           show-unicode-characters
           war
           life
           smile
           visualize-one-in-chance
           visualize-chance
           cellular-automata
           visualize-math-functions
           i-ching
           bullseye
           sunbeam
           remember forget
           sun
           moiré
           multiplication-table
           peace
           turtle-race
           save-log
           hall-of-programmers
           calculator
           calculate
           play-cards
           chemical
           ))

(in-package :academy)

;;; !!! A hack to get around package locks to allow for FLETing a new PUSH.
(defmacro push (&rest args)
  `(cl:push ,@args))

