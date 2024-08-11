;;;; cl-rltuto-raylib.lisp

(in-package #:cl-rltuto-raylib)

;;;; main game scene parameters
(defparameter *scene-width* 60)
(defparameter *scene-height* 60)
;;;; procgen parameters
(defparameter *room-max-size* (round (/ (+ *scene-width* *scene-height*) 8)))
(defparameter *room-min-size* (round (/ (+ *scene-width* *scene-height*) 12)))
(defparameter *max-rooms* (+ *scene-width* *scene-height*))
(defparameter *max-creatures-per-room* (round (/ (+ *scene-width* *scene-height*) 20)))
;;;; main game scene
(defparameter *main-scene* nil)
;;;; player creature instance
(defparameter *player* nil)
;;;; other entities instances list
(defparameter *entities* nil)
;;;; system messages log
(defparameter *messages* nil)
(defparameter *msg-idx* 0)
;;;; items instances
(defparameter *items* nil)
(defparameter *max-items-per-room* 2)
;(defparameter *max-items-per-room* (round (/ (+ *scene-width* *scene-height*) 100)))

(deftype game-states () '(member :player-turn :ai-turn :exit))

(deftype terrain-kind () '(member :ground :wall :water))

(defun terrain-kind-p (terrain)
  (typep t 'terrain-kind))

(defmethod spawn-player ((player creature) (spawn rectangle-room))
  (let ((spawn-coords))
    (setf *messages* (list (list :message (format nil "~A spawned." (slot-value *player* 'name/name)))))
    (setf spawn-coords (find-center-rectangle-room spawn))
    (push player (cdr (last *entities*)))
    (push (spawn-item (make-potion 1) (1+ (car spawn-coords)) (cadr spawn-coords)) (cdr (last *items*)))
    (push (spawn-item (make-potion 1) (1+ (1+ (car spawn-coords))) (cadr spawn-coords)) (cdr (last *items*)))
    (spawn-creature player (car spawn-coords) (cadr spawn-coords))))

(defmethod init-scene-cells ((s scene))
  (let ((spawn-cell) (rooms) (walls) (creatures (list nil)) (items (list nil)))
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
    (setf items (furnish-rooms rooms items *max-items-per-room*))
    ;(format t "items ~a~%" items)
    (setf *items* (cdr items))
    ;(format t "*items* ~a~%" *items*)
    spawn-cell))

(defmethod init-scene-walls ((s scene))
  (let ((walls))
    (setf walls (gen-walls s))))

(defun wait-action (creature)
  (let ((results (list T)))
    ;(push (list :message (format nil "The ~A idles." (name/name creature))) (cdr (last results)))
    results))

(defun move-action (creature movement)
  (let ((results (list T)))
    (move creature (car movement) (cdr movement))
    ;(push (list :message (format nil "The ~A moves toward (~2d,~2d)." (name/name creature) (car movement) (cdr movement))) (cdr (last results)))
    results))

(defun melee-action (attacker defender)
  (let ((results (list T)))
    (push (list :message (attack-creature attacker defender)) (cdr (last results)))
    (if (creature-dead-p defender)
        (progn
          (push (list :message (format nil "The ~a dies." (name/name defender))) (cdr (last results)))
          (kill-creature defender))
        (progn
          (push (list :message (format nil "The ~A has ~a vitality left." (name/name defender) (vitality/current defender))) (cdr (last results)))))
    results))

;((push (list :message (format nil "The ~A inventory is full." (name/name creature))) (cdr (last results))))

(defun pickup-action (creature)
  ;(format t "inventory ~a~%" (inventory/slots creature))
  (let ((results (list T))
        (free-slot (inventory-free-slot creature)))
    ;(format t "free slot ~a~%" (inventory-free-slot creature))
    (if (null free-slot) ; inventory full
        (push (list :message (format nil "The ~A inventory is full." (name/name creature))) (cdr (last results)))
        (let ((target (location-item *items* (location/x creature) (location/y creature))))
        ;(format t "target ~a~%" target)
        (when (item? target)
          (progn
            (setf (aref (inventory/slots creature) free-slot) target)
            (setf (location/x target) nil)
            (setf (location/y target) nil)
            (push (list :message (format nil "The ~A picked up ~A." (name/name creature) (name/name target))) (cdr (last results)))))))
    ;(format t "updt inventory ~a~%" (inventory/slots creature))
    results))

(defun drop-item-action (creature slot)
  ;(format t "inventory ~a~%" (inventory/slots creature))
  (let ((results (list T))
        (target-item (aref (inventory/slots creature) slot)))
    (if (item? target-item)
        (let ((x (location/x creature))
              (y (location/y creature)))
          (setf (location/x target-item) x)
          (setf (location/y target-item) y)
          (setf (aref (inventory/slots creature) slot) 0)
          (push (list :message (format nil "The ~A dropped ~A." (name/name creature) (name/name target-item))) (cdr (last results)))
          )
        (push (list :message (format nil "Nothing to drop.")) (cdr (last results)))
        )
    ;(format t "updt inventory ~a~%" (inventory/slots creature))
    results))

(defun use-item-action (creature slot)
  ;(format t "inventory ~a~%" (inventory/slots creature))
  (let* ((results (list T))
        (target-item (aref (inventory/slots creature) slot)))
    (if (item? target-item)
        (let ((result (use-potion target-item 1 creature)))
          (push (list :message (cadr result)) (cdr (last results)))
          (when (<= (car result) 0)
            (progn
              (push (list :message (format nil "The ~A is consumed." (name/name target-item))) (cdr (last results)))
              (setf (aref (inventory/slots creature) slot) 0)
              (destroy-entity target-item)))
          ))
    ;(format t "updt inventory ~a~%" (inventory/slots creature))
    results))

(defun enact-action (creature entities scene action)
  (let ((results))
    ;(format t "enact action ~a~%" action)
    (cond ((getf action :movement)
           (let* ((movement (getf action :movement))
                  (target (check-movement creature entities scene movement)))
             ;(format t "e target ~a~%" target)
             (cond ((null target) (setf results (move-action creature movement)))
                   ((creature? target) (setf results (melee-action creature target)))
                   (T nil))))
          ((getf action :melee-attack)
           (let ((melee-attack (getf action :melee-attack)))
             (setf results (melee-action (car melee-attack) (cdr melee-attack)))))
          ((getf action :pickup)
           (setf results (pickup-action creature)))
          ((getf action :drop-item)
           (let ((target-slot (getf action :drop-item)))
             (setf results (drop-item-action creature target-slot))))
          ((getf action :use-item)
           (let ((target-slot (getf action :use-item)))
             (setf results (use-item-action creature target-slot))))
          ((getf action :wait)
           (setf results (wait-action creature)))
          ((getf action :reveal-scene)
           (reveal-scene scene))
          (T
           (progn
             (format t "unrecognized action ~a~%" action)
             nil)))))

(defun game-tick (player entities scene game-state)
  (let ((player-action (handle-keys))
        (results))
    (when (and (eql game-state :player-turn) player-action)
      (setf results (enact-action player entities scene player-action))
      (when results
        (progn
          (update-fov scene player) ; TODO: replace w/ indiv fov for all entities
          (update-message-log results)
          ;(update-message-ui)
          (update-drawn-messages)
          (setf game-state :ai-turn))))
    (when (eql game-state :ai-turn)
      (dolist (entity entities)
        (when (and (not (eql player entity))
                   (slot-value entity 'ai/ai))
          (setf results (enact-action entity entities scene (chose-action entity scene entities)))
          (update-message-log results))))
    (setf game-state :player-turn)
    (when (creature-dead-p player)
      (setf game-state :exit))
    (update-drawn-messages)
    game-state))

(defun update-message-log (messages)
  (dolist (message (cdr messages))
    (when (getf message :message)
      (push message (cdr (last *messages*))))))

(defun dump-message-log ()
  (dolist (msg *messages*)
    (format t "~a~%" (getf msg :message))))

(defun start-drawing (player scene)
  (draw-visible-screen player scene))

(defun main ()
  "main game function"
  ;; game initialisation
  (setf *main-scene* (make-scene *scene-width* *scene-height*))
  (setf *player* (make-player))
  (let ((starting-room (init-scene-cells *main-scene*)))
    (spawn-player *player* starting-room))
  (cast-light *main-scene* (list (slot-value *player* 'location/x) (slot-value *player* 'location/y)) (slot-value *player* 'perceptive/radius))
  ;; start ui
  (start-drawing *player* *main-scene*)
  ;; cleanup
  (clear-entities)
  ;; debug
  (dump-message-log))

