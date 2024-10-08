;;;; ai.lisp

(in-package #:cl-rltuto-raylib)

(defun get-distance (x0 y0 x1 y1)
  (let ((dx (- x0 x1))
        (dy (- y0 y1)))
    (sqrt (+ (expt dx 2) (expt dy 2)))))

(defun check-movement (creature entities scene movement)
  (let* ((creature-x (slot-value creature 'location/x))
         (creature-y (slot-value creature 'location/y))
         (dx (+ creature-x (car movement)))
         (dy (+ creature-y (cdr movement)))
         (cell (get-cell scene (list dx dy)))
         (target (location-occupied-p entities dx dy)))
    (cond (target (when (creature? target)
                    (when (slot-value target 'impassable/impassable)
                      target)))
          ((not (null (slot-value cell 'impassable/impassable))) T)
          (T nil))))

(defmethod move-towards ((c creature) tx ty scene entities)
  (with-slots ((x location/x) (y location/y)) c
    (let* ((dx (- tx x))
           (dy (- ty y))
           (distance (sqrt (+ (expt dx 2) (+ (expt dy 2))))))
      (setf dx (round (/ dx distance)))
      (setf dy (round (/ dy distance)))
      (unless (= (+ (abs dx) (abs dy)) 1) (setf dy 0)) ; manage occasional diagonal movement
      (let ((is-blocked (check-movement c entities scene (cons dx dy))))
        (unless is-blocked
          (list :movement (cons dx dy)))))))

(defmethod chose-action ((c creature) scene entities)
  "take action only when inside the player fov"
  ;(format t "creature ~a~%" c)
  (with-slots ((x location/x) (y location/y)) c
    (let ((action nil)
          (in-sight (slot-value (get-cell scene (list x y)) 'visible/visible)))
      (if in-sight
        (let* ((target *player*)
               (tx (slot-value target 'location/x))
               (ty (slot-value target 'location/y))
               (distance (ceiling (get-distance x y tx ty))))
          (cond ((>= distance 2)
                 (progn
                   ;(format t "The ~A sees that the ~A is out of range at distance ~a.~%" c target distance)
                   (setf action (move-towards c tx ty scene entities))))
                ((< distance 2)
                 (progn
                   ;(format t "The ~A sees that the ~A is in range at distance ~a.~%" c target distance)
                   (setf action (list :melee-attack (cons c target)))))))
        (setf action (list :wait T)))
      ;(format t "~A action ~a~%" c action)
      action)))

