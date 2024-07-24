;;;; cl-rltuto-raylib.lisp

(in-package #:cl-rltuto-raylib)

;;;; main game scene parameters
(defparameter *scene-width* 45)
(defparameter *scene-height* 34)
;;;; procgen parameters
(defparameter *room-max-size* 12)
(defparameter *room-min-size* 5)
(defparameter *max-rooms* 34)
;;;; main game scene
(defparameter *main-scene* nil)
;;;; player creature instance
(defparameter *player* nil)

(deftype terrain-kind () '(member :ground :wall :water))

(defun terrain-kind-p (terrain)
  (typep t 'terrain-kind))

(defmethod spawn-player ((player creature) (spawn rectangle-room))
  (let ((spawn-coords))
    (setf spawn-coords (find-center-rectangle-room spawn))
    (move player (car spawn-coords) (cadr spawn-coords))))

(defmethod init-scene-cells ((s scene))
  (let ((spawn-cell))
    (setf spawn-cell (procgen-scene-basic s *max-rooms* *room-min-size* *room-max-size* *scene-width* *scene-height*))
    spawn-cell))

(defmethod init-scene-creatures ((s scene))
  "initalise scene w/ player & procgen creatures"
  (let ((creatures))
    (setf creatures (generate-creatures s))
    ;(format t "creatures ~a~%" creatures)
    ))

;(defmethod init-scene-items ((s scene))
;  "initialise scene w/ procgen items"
;  (generate-items s))

(defun start-drawing (player scene)
  (draw-screen player scene))

(defun main ()
  "main game function"
  ;; game initialisation
  (setf *main-scene* (make-scene *scene-width* *scene-height*))
  (setf *player* (create-entity 'creature :visual/visual "@" :visual/color :white))
  (let ((starting-room (init-scene-cells *main-scene*)))
    (spawn-player *player* starting-room)
    ;(init-scene-items *main-scene*)
    (init-scene-creatures *main-scene*))
  ;; game loop
  (start-drawing *player* *main-scene*)
  ;; cleanup
  (clear-entities))
