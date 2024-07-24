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
  (let ((num-rooms 0) (w 0) (h 0) (x 0) (y 0) (rooms (list nil)) (new-room) (invalid-location))
    (dotimes (r max-rooms)
      (setf invalid-location nil)
      (setf w (+ (random (- room-max-size room-min-size)) room-min-size))
      (setf h (+ (random (- room-max-size room-min-size)) room-min-size))
      (setf x (random (- scene-width w)))
      (setf y (random (- scene-height h)))
      (setf new-room (create-entity 'rectangle-room :x x :y y :w w :h h))
      (dotimes (other-room num-rooms)
        (setf invalid-location (or invalid-location
                                  (intersects-rectangle-room-p new-room (nth (1+ other-room) rooms)))))
      (unless invalid-location
        (push new-room (cdr (last rooms)))
        (build-rect-room s new-room)
        (setf num-rooms (1+ num-rooms))
        (when (>= num-rooms 2)
          (let ((prev-x (car (find-center-rectangle-room (nth (- num-rooms 1) rooms))))
                (prev-y (cadr (find-center-rectangle-room (nth (- num-rooms 1) rooms))))
                (curr-x (car (find-center-rectangle-room (nth num-rooms rooms))))
                (curr-y (cadr (find-center-rectangle-room (nth num-rooms rooms)))))
            (if (= (random 2) 1)
                (progn
                  (build-horizontal-corridor s (create-entity 'corridor :baseline prev-y :s prev-x :e curr-x))
                  (build-vertical-corridor s (create-entity 'corridor :baseline curr-x :s prev-y :e curr-y)))
                (progn
                  (build-vertical-corridor s (create-entity 'corridor :baseline prev-x :s prev-y :e curr-y))
                  (build-horizontal-corridor s (create-entity 'corridor :baseline curr-y :s prev-x :e curr-x))))))))
      (let ((spawn-room (nth 1 rooms)))
        spawn-room)))

(defmethod generate-creatures ((s scene))
  (with-slots ((w width) (h height)) s
    (let ((nb-creatures 0) (creatures (list nil)) (new-creature))
      (dotimes (y h)
        (dotimes (x w)
          (let ((cell (aref (scene/cells s) x y)))
            (cond ((eq (slot-value cell 'terrain/terrain) :wall)
                   (progn
                     (setf new-creature (make-wall x y))
                     (push new-creature (cdr (last creatures)))
                     (1+ nb-creatures)))
                  ((eq (slot-value cell 'terrain/terrain) :ground)
                   (progn
                     (when (= (random 100) 1)
                       (setf new-creature (make-npc x y))
                       (push new-creature (cdr (last creatures)))
                       (1+ nb-creatures))))
                  (T
                   (format t "not supported terrain ~a~%" (slot-value cell 'terrain/terrain)))))))
      creatures)))

