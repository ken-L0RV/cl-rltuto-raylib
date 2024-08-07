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

(deftype game-states () '(member :player-turn :ai-turn :exit))

(deftype terrain-kind () '(member :ground :wall :water))

(defun terrain-kind-p (terrain)
  (typep t 'terrain-kind))

(defmethod spawn-player ((player creature) (spawn rectangle-room))
  (let ((spawn-coords))
    (setf spawn-coords (find-center-rectangle-room spawn))
    (push player (cdr (last *entities*)))
    (spawn-creature player (car spawn-coords) (cadr spawn-coords))))

(defmethod init-scene-cells ((s scene))
  (let ((spawn-cell) (rooms) (walls) (creatures (list nil)))
    (setf rooms (procgen-scene-basic s *max-rooms* *room-min-size* *room-max-size* *scene-width* *scene-height*))
    ;(setf rooms (procgen-scene-basic s 1 (- *scene-height* 2) (- *scene-height* 1) *scene-width* *scene-height*)) ; one big room
    ;(format t "rooms ~a~%" rooms)
    (setf spawn-cell (nth 0 rooms))
    ;(format t "spawn-cell ~a~%" spawn-cell)
    ;; spawn walls & other terrain features
    ;(setf walls (init-scene-walls s))
    ;; spawn creatures
    (setf creatures (populate-rooms rooms creatures *max-creatures-per-room*)); populates all rooms except the spawn
    ;(format t "creatures ~a~%" creatures)
    (setf *entities* (cdr creatures)) ; everything after first nil element
    ;(format t "*entities* ~a~%" *entities*)
    spawn-cell))

(defmethod init-scene-walls ((s scene))
  (let ((walls))
    (setf walls (gen-walls s))))

(defun wait-action (creature)
  (format t "The ~A idles.~%" creature))

(defun move-action (creature movement)
  (move creature (car movement) (cdr movement)))

(defun melee-action (attacker defender)
  (let* ((a-power (slot-value attacker 'power/power))
         (d-defense (slot-value defender 'defense/defense))
         (d-vit (slot-value defender 'vitality/current))
         (damage (- a-power d-defense))
         (new-d-vit (- d-vit damage)))
    (format t "The ~A attacks the ~A in melee.~%" attacker defender)
    (format t "The ~A has ~a vitality and takes ~a damage.~%" defender d-vit damage)
    (if (<= new-d-vit 0)
        (progn
          (setf (slot-value defender 'vitality/current) new-d-vit)
          (format t "The ~a dies.~%" defender)
          (kill-creature defender))
        (progn
          (setf (slot-value defender 'vitality/current) new-d-vit)
          (format t "The ~A has ~a vitality left.~%" defender new-d-vit)))))

(defun check-movement (creature entities scene movement)
  (let* ((creature-x (slot-value creature 'location/x))
         (creature-y (slot-value creature 'location/y))
         (dx (+ creature-x (car movement)))
         (dy (+ creature-y (cdr movement)))
         (cell (get-cell scene (list dx dy)))
         (target (location-occupied-p entities dx dy)))
    ;(format t "chk target ~a~%" target)
    ;(format t "chk impassable ~a~%" (slot-value cell 'impassable/impassable))
    (cond (target (when (creature? target)
                    (when (slot-value target 'impassable/impassable)
                      target)))
          ((not (null (slot-value cell 'impassable/impassable))) T)
          (T nil))))

(defun enact-action (creature entities scene action)
  ;(format t "enact action ~a~%" action)
  (cond ((getf action :movement)
         (let* ((movement (getf action :movement))
                (target (check-movement creature entities scene movement)))
           ;(format t "e target ~a~%" target)
           (cond ((null target) (move-action creature movement))
                 ((creature? target) (melee-action creature target))
                 (T nil))))
        ((getf action :melee-attack)
         (let ((melee-attack (getf action :melee-attack)))
           (melee-action (car melee-attack) (cdr melee-attack))))
        ((getf action :wait)
         (wait-action creature))
        (T
         (progn
           (format t "unrecognized action ~a~%" action)
           nil))))

(defun game-tick (player entities scene game-state)
  (let ((player-action (handle-keys)))
    (when (and (eql game-state :player-turn) player-action)
      (when (enact-action player entities scene player-action)
        (progn
          (update-fov scene player) ; TODO: replace w/ indiv fov for all entities
          (setf game-state :ai-turn))))
    (when (getf player-action :reveal-scene)
      (reveal-scene scene)))
  (when (eql game-state :ai-turn)
    (dolist (entity entities)
      (when (and (not (eql player entity))
                 (slot-value entity 'ai/ai))
          (enact-action entity entities scene (chose-action entity scene entities))))
    (setf game-state :player-turn))
  (when (creature-dead-p player)
    (setf game-state :exit))
  game-state)

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

