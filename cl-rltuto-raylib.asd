;;;; cl-rltuto-raylib.asd

(asdf:defsystem #:cl-rltuto-raylib
  :description "2024 r/roguelikedev annual tuto in common lisp with raylib"
  :author "ken-L0RV"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:beast #:cl-raylib #:3d-vectors)
  :components ((:file "src/package")
               (:file "src/components")
               (:file "src/entities")
               (:file "src/systems")
               (:file "src/scene")
               (:file "src/fov-raycasting")
               (:file "src/ai")
               (:file "src/procgen")
               (:file "src/display-raylib")
               (:file "src/ui-raylib")
               (:file "src/cl-rltuto-raylib")))
