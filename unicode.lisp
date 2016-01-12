(in-package :academy)

;; ☮ ☘ ☁ ❄ ☃ ✈ ☳ ☠ ✝ ✡ ☯ ☥ ♁ ♛ ⚅ ⚛ ⌚ ☄ ䷊

(defparameter *sample-symbols*
  '(#\PEACE_SYMBOL
    #\SHAMROCK
    #\CLOUD
    #\SNOWFLAKE
    #\SNOWMAN
    #\AIRPLANE
    #\TRIGRAM_FOR_THUNDER
    #\SKULL_AND_CROSSBONES
    #\LATIN_CROSS
    #\STAR_OF_DAVID
    #\YIN_YANG
    #\ANKH
    #\EARTH
    #\BLACK_CHESS_QUEEN
    #\DIE_FACE-6
    #\ATOM_SYMBOL
    #\WATCH
    #\COMET
    #\HEXAGRAM_FOR_PEACE))

;;; Hackers! Suggestions for additional unicode sets are wholeheartedly welcome!

(defparameter *sample-unicode-sets*
  `((:arrows                  #x2190 #x21ff)
    (:dingbats                #x2700 #x27FF)
    (:supplemental-arrows-a   #x27F0 #x27FF)
    (:supplemental-arrows-b   #x2900 #x297F)
    (:misc-symbols-and-arrows #x2B00 #x2B2F)
    (:mathematical-operators  #x2200 #x22FF)
    (:box-drawing             #x2500 #x257F)
    (:block-elements          #x2580 #x259F)
    (:geometric-shapes        #x25A0 #x25FF)
    (:miscellaneous-symbols   #x2600 #x26FF)
    (:miscellaneous-technical #x2300 #x23FF)
    (:number-forms            #x2150 #x218F)
    (:i-ching-trigrams        #x2630 #x2637)
    (:i-ching-symbols         #x4DC0 #x4DFF)
    (:braille-patterns        #x2800 #x28FF)
    (:mahjong                 #x1F000 #x1F02B)
    (:dominos                 #x1F030 #x1F093)
    (:playing-cards           #x1F0A0 #x1F0F5)
    (:emoticons               #x1F601 #x1F64F)
    (:transportation          #x1F680 #x1F6F3)
    (:misc-3                  #x1F300 #x1F5FF)))

(defun show-unicode-characters (&optional which)
  "Show many sets of unicode arrows, dingbats, boxes, i-ching, etc."
  (if which
    (loop for set in (if (eq which :all) (mapcar #'car *sample-unicode-sets*) (ensure-list which))
          do (destructuring-bind (name start end) (or (assoc set *sample-unicode-sets*)
                                                      (error "unicode set ~A not found" set))
               (format t "~%~A~%~%" name)
               (loop for x from start to end
                     do (format t "~X : ~C   ~S~%" x (code-char x) (code-char x)))))
    (progn
      (format t "Please select a unicode set to view from the following.~%~%  :all~%")
      (loop for (name) in *sample-unicode-sets*
            do (format t "  ~(~S~)~%" name)))))

(defparameter *ascii-to-braille-map*
  " A1B'K2L@CIF/MSP\"E3H9O6R^DJG>NTQ,*5<-U8V.%[$+X!&;:4\\0Z7(_?W]#Y)=")

(defun print-in-braille (string)
  (loop for character across string
        do (let ((pos (position (char-upcase character) *ascii-to-braille-map*)))
             (if pos
               (write-char (code-char (+ #x2800 pos)))
               (warn "Character ~S is not a valid braille character." character)))))
