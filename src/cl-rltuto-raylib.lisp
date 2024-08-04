;;;; cl-rltuto-raylib.lisp

(in-package #:cl-rltuto-raylib)

;;;; main game scene parameters
(defparameter *scene-width* 45)
(defparameter *scene-height* 34)
;;;; procgen parameters
(defparameter *room-max-size* 12)
(defparameter *room-min-size* 5)
(defparameter *max-rooms* 34)
(defparameter *max-creatures-per-room* 3)
;;;; main game scene
(defparameter *main-scene* nil)
;;;; player creature instance
(defparameter *player* nil)
;;;; other entities instances list
(defparameter *entities* nil)

(deftype terrain-kind () '(member :ground :wall :water))

(defun terrain-kind-p (terrain)
  (typep t 'terrain-kind))

(defmethod spawn-player ((player creature) (spawn rectangle-room))
  (let ((spawn-coords))
    (setf spawn-coords (find-center-rectangle-room spawn))
    (spawn-creature player (car spawn-coords) (cadr spawn-coords))))

(defmethod init-scene-cells ((s scene))
  (let ((spawn-cell) (rooms) (walls) (creatures (list nil)))
    (setf rooms (procgen-scene-basic s *max-rooms* *room-min-size* *room-max-size* *scene-width* *scene-height*))
    ;(setf rooms (procgen-scene-basic s 1 (- *scene-height* 2) (- *scene-height* 1) *scene-width* *scene-height*)) ; one big room
    ;(format t "rooms ~a~%" rooms)
    (setf spawn-cell (nth 0 rooms))
    ;(format t "spawn-cell ~a~%" spawn-cell)
    ;; spawn walls & other terrain features
    (setf walls (init-scene-walls s))
    ;; spawn creatures
    (setf creatures (populate-rooms rooms creatures *max-creatures-per-room*))
    ;(format t "creatures ~a~%" creatures)
    (setf *entities* (cdr creatures)) ; everything after first nil element
    ;(format t "*entities* ~a~%" *entities*)
    spawn-cell))

(defmethod init-scene-walls ((s scene))
  (let ((walls))
    (setf walls (gen-walls s))))

(defun start-drawing (player scene)
  (draw-screen player scene))

(defun main ()
  "main game function"
  ;; game initialisation
  (setf *main-scene* (make-scene *scene-width* *scene-height*))
  (setf *player* (make-player))
  (let ((starting-room (init-scene-cells *main-scene*)))
    (spawn-player *player* starting-room))
  (cast-light *main-scene* (list (slot-value *player* 'location/x) (slot-value *player* 'location/y)))
  ;; start ui
  (start-drawing *player* *main-scene*)
  ;; cleanup
  (clear-entities))
