(defsystem :academy
  :serial t
  :components ((:static-file "academy.asd")
               (:file "packages")
               (:file "classic-utilities")
               (:file "unicode")
               (:file "randomness")
               (:file "welcome")))

