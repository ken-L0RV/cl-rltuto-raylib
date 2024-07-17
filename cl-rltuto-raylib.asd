;;;; cl-rltuto-raylib.asd

(asdf:defsystem #:cl-rltuto-raylib
  :description "2024 r/roguelikedev annual tuto in common lisp with raylib"
  :author "ken-L0RV"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:beast #:cl-raylib)
  :components ((:file "src/package")
               (:file "src/components")
               (:file "src/entities")
               (:file "src/systems")
               (:file "src/scene")
               (:file "src/display")
               (:file "src/display-raylib")
               (:file "src/cl-rltuto-raylib")
               ))
