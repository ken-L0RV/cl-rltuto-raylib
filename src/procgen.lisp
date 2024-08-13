;;;; proc-gen.lisp

;;;; procedural generation functions

(in-package #:cl-rltuto-raylib)

(defmethod build-rect-room ((s scene) (r rectangle-room))
  (loop :for y :from (1+ (rectangle-y1 r)) :below (rectangle-y2 r)
        :do
        (loop :for x :from (1+ (rectangle-x1 r)) :below (rectangle-x2 r)
              :do
              (let ((cell (aref (scene/cells s) x y)))
                (update-terrain cell :ground)))))

(defmethod build-horizontal-corridor ((s scene) (c corridor))
  (let ((start (corridor-start c))
        (stop (corridor-end c)))
    (when (> start stop) (rotatef start stop))
    (loop :for x :from start :below (1+ stop)
          :do
          (let ((cell (aref (scene/cells s) x (corridor-baseline c))))
            (update-terrain cell :ground)))))

(defmethod build-vertical-corridor ((s scene) (c corridor))
  (let ((start (corridor-start c))
        (stop (corridor-end c)))
    (when (> start stop) (rotatef start stop))
    (loop :for y :from start :below (1+ stop)
          :do
          (let ((cell (aref (scene/cells s) (corridor-baseline c) y)))
            (update-terrain cell :ground)))))

(defmethod procgen-scene-basic ((s scene) max-rooms room-min-size room-max-size scene-width scene-height)
  (let ((num-rooms 0) (w 0) (h 0) (x 0) (y 0) (rooms) (new-room) (invalid-location))
    (dotimes (r max-rooms)
      (setf invalid-location nil)
      (setf w (+ (random (- room-max-size room-min-size)) room-min-size))
      (setf h (+ (random (- room-max-size room-min-size)) room-min-size))
      (setf x (random (- scene-width w)))
      (setf y (random (- scene-height h)))
      (setf new-room (create-entity 'rectangle-room :x x :y y :w w :h h))
      (unless (null rooms)
        (dotimes (other-room num-rooms)
          (setf invalid-location (or invalid-location
                                     (intersects-rectangle-room-p new-room (nth other-room rooms))))))
      (unless invalid-location
        (if (null rooms) ; TODO: fix rooms elem 1 being nil
          (setf rooms (list new-room))
          (push new-room (cdr (last rooms))))
        (build-rect-room s new-room)
        (setf num-rooms (1+ num-rooms))
        (when (>= num-rooms 2)
          ;(format t "rooms ~a n-1 ~a n-2 ~a~%" rooms (nth (- num-rooms 1) rooms) (nth (- num-rooms 2) rooms))
          (let ((prev-x (car (find-center-rectangle-room (nth (- num-rooms 2) rooms))))
                (prev-y (cadr (find-center-rectangle-room (nth (- num-rooms 2) rooms))))
                (curr-x (car (find-center-rectangle-room (nth (- num-rooms 1) rooms))))
                (curr-y (cadr (find-center-rectangle-room (nth (- num-rooms 1) rooms)))))
            (if (= (random 2) 1)
                (progn
                  (build-horizontal-corridor s (create-entity 'corridor :baseline prev-y :s prev-x :e curr-x))
                  (build-vertical-corridor s (create-entity 'corridor :baseline curr-x :s prev-y :e curr-y)))
                (progn
                  (build-vertical-corridor s (create-entity 'corridor :baseline prev-x :s prev-y :e curr-y))
                  (build-horizontal-corridor s (create-entity 'corridor :baseline curr-y :s prev-x :e curr-x))))))))
    rooms))

(defmethod gen-walls ((s scene))
  (with-slots ((w width) (h height)) s
    (let ((walls) (new-wall))
      (dotimes (y h)
        (dotimes (x w)
          (let ((cell (aref (scene/cells s) x y)))
            (when (match-cell-terrain-p cell :wall)
              (progn
                (setf new-wall (spawn-creature (make-wall) x y))
                (if (null walls)
                    (setf walls (list new-wall))
                    (push new-wall (cdr (last walls)))))))))
      walls)))

(defmethod gen-room-creatures ((room rectangle-room) creatures max-creatures-per-room)
  (let ((num-creatures (random max-creatures-per-room)))
    (dotimes (i num-creatures)
      (let ((x (+ (random (- (rectangle-x2 room) (rectangle-x1 room) 1)) (1+ (rectangle-x1 room))))
            (y (+ (random (- (rectangle-y2 room) (rectangle-y1 room) 1)) (1+ (rectangle-y1 room)))))
        (unless (location-occupied-p creatures x y)
          (if (> (random 100) 80)
              (push (spawn-creature (make-troll) x y) (cdr (last creatures)))
              (push (spawn-creature (make-orc) x y) (cdr (last creatures)))))))
    creatures))

(defun populate-rooms (rooms creatures max-creatures-per-room)
  (dolist (room (cdr rooms))
    (let ((room-creatures (list nil)))
        (gen-room-creatures room room-creatures max-creatures-per-room)
        (setf room-creatures (cdr room-creatures)) ; remove nil first elem
        (unless (null room-creatures)
          (nconc creatures room-creatures))))
    creatures)

(defmethod gen-room-items ((room rectangle-room) items max-items-per-room)
  (let ((num-items (random max-items-per-room)))
    (dotimes (i num-items)
      (let ((x (+ (random (- (rectangle-x2 room) (rectangle-x1 room) 1)) (1+ (rectangle-x1 room))))
            (y (+ (random (- (rectangle-y2 room) (rectangle-y1 room) 1)) (1+ (rectangle-y1 room)))))
        (unless (or (location-furnished-p items x y)
                    (location-occupied-p *entities* x y))
          (if (< (random 100) 70)
              (push (spawn-item (make-item 1) x y) (cdr (last items)))
              (push (spawn-item (make-item 2) x y) (cdr (last items)))))))
    items))

(defun furnish-rooms (rooms items max-items-per-room)
  (dolist (room (cdr rooms))
    (let ((room-items (list nil)))
      (gen-room-items room room-items max-items-per-room)
      (setf room-items (cdr room-items))
      (unless (null room-items)
        (nconc items room-items))))
  items)
