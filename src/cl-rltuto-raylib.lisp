;;;; cl-rltuto-raylib.lisp

(in-package #:cl-rltuto-raylib)

;;;; instantiate main game scene
(defparameter *scene-width* 34)
(defparameter *scene-height* 34)

(defparameter *main-scene* (make-scene *scene-width* *scene-height*))
(initialise-cells *main-scene*) ; init main scene cells

(defparameter *player* (create-entity 'creature
                               :location/x (/ *scene-width* 2)
                               :location/y (/ *scene-height* 2)
                               :avatar/visual "@"
                               :avatar/color :white))
(defparameter *npc* (create-entity 'creature
                                   :location/x (+ (/ *scene-width* 2) 1)
                                   :location/y (+ (/ *scene-height* 2) 1)
                                   :avatar/visual "@"
                                   :avatar/color :yellow))

(defun main ()
  (let ((player *player*))
    "init scene & start draw loop"
    ;(init-scene *main-scene*)
    (start-draw player *main-scene*)
    ;; destructor calls
    (destroy-entity player)))
