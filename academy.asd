(defsystem :academy
  :serial t
  :components ((:static-file "academy.asd")
               (:file "packages")
               (:file "classic-utilities")
               (:file "randomness")
               (:file "welcome")))

