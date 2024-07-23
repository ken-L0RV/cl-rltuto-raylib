;;;; game-map.lis

(in-package #:cl-rltuto-raylib)

(defparameter *room-max-size* 10)
(defparameter *room-min-size* 5)
(defparameter *max-rooms* 25)

(defclass scene ()
  ((width :initarg :w :accessor scene/w)
   (height :initarg :h :accessor scene/h)
   (cells :accessor scene/cells))
  (:documentation "scene object that contains an array of cells"))

(defmethod initialize-instance :after ((s scene) &rest initargs)
  "init scene cells slot to a 2D array on object instantiation"
  (declare (ignore initargs))
  (setf (scene/cells s) (make-array (list (scene/w s) (scene/w s)))))

(defmethod initialise-cells ((s scene) (player creature))
  "instantantiate a scene cells"
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (setf (aref (scene/cells s) x y) (create-entity 'cell))))
  ;; call map procgen to set terrain here
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (setf (slot-value (aref (scene/cells s) x y) 'impassable/state) T)
      (setf (slot-value (aref (scene/cells s) x y) 'opaque/state) T)))
  ;(let ((room-1 (create-entity 'rectangle-room :x 15 :y 15 :w 10 :h 5))
  ;      (room-2 (create-entity 'rectangle-room :x 0 :y 4 :w 4 :h 20))
  ;      (corr-h-1 (create-entity 'corridor :baseline 10 :s 20 :e 4))
  ;      (corr-h-2 (create-entity 'corridor :baseline 20 :s 20 :e 10))
  ;      )
  ;  (build-rect-room s room-1)
  ;  (build-rect-room s room-2)
  ;  (build-horizontal-corridor s corr-h-1)
  ;  (build-vertical-corridor s corr-h-2))
  (generate-scene s *max-rooms* *room-min-size* *room-max-size* *scene-width* *scene-height* player))

(defmethod build-rect-room ((s scene) (r rectangle-room))
  (loop :for y :from (1+ (rectangle-y1 r)) :below (rectangle-y2 r)
        :do
        (loop :for x :from (1+ (rectangle-x1 r)) :below (rectangle-x2 r)
              :do
              (setf (slot-value (aref (scene/cells s) x y) 'impassable/state) nil)
              (setf (slot-value (aref (scene/cells s) x y) 'opaque/state) nil))))

(defmethod build-horizontal-corridor ((s scene) (c corridor))
  (let ((start (corridor-start c))
        (stop (corridor-end c)))
    (when (> start stop) (rotatef start stop))
    (loop :for x :from start :below (1+ stop)
          :do
          (setf (slot-value (aref (scene/cells s) x (corridor-baseline c)) 'impassable/state) nil)
          (setf (slot-value (aref (scene/cells s) x (corridor-baseline c)) 'opaque/state) nil))))

(defmethod build-vertical-corridor ((s scene) (c corridor))
  (let ((start (corridor-start c))
        (stop (corridor-end c)))
    (when (> start stop) (rotatef start stop))
    (loop :for y :from start :below (1+ stop)
          :do
          (setf (slot-value (aref (scene/cells s) (corridor-baseline c) y) 'impassable/state) nil)
          (setf (slot-value (aref (scene/cells s) (corridor-baseline c) y) 'opaque/state) nil))))

(defmethod generate-scene ((s scene) max-rooms room-min-size room-max-size scene-width scene-height (player creature))
  (let ((num-rooms 0) (w 0) (h 0) (x 0) (y 0) (rooms (list nil)) (new-room) (invalid-location))
    (dotimes (r max-rooms)
      (setf invalid-location nil)
      (setf w (+ (random (- room-max-size room-min-size)) room-min-size))
      (setf h (+ (random (- room-max-size room-min-size)) room-min-size))
      (setf x (random (- scene-width w)))
      (setf y (random (- scene-height h)))
      (format t "gen room x ~a y ~a w ~a h ~a~%" x y w h)
      (setf new-room (create-entity 'rectangle-room :x x :y y :w w :h h))
      (format t "new room ~a~%" new-room)
      (dotimes (other-room num-rooms)
        (format t "other room ~a~%" other-room)
        (setf invalid-location (or invalid-location
                                  (intersects-rectangle-room new-room (nth (1+ other-room) rooms))))
        (format t "intersect ~a~%" (intersects-rectangle-room new-room (nth (1+ other-room) rooms)))
        )
      (format t "invld loc ~a~%" invalid-location)
      (unless invalid-location
        (push new-room (cdr (last rooms)))
        (build-rect-room s new-room)
        (setf num-rooms (1+ num-rooms))
        (when (>= num-rooms 2)
          (let ((prev-x (car (center-rectangle-room (nth (- num-rooms 1) rooms))))
                (prev-y (cadr (center-rectangle-room (nth (- num-rooms 1) rooms))))

                (curr-x (car (center-rectangle-room (nth num-rooms rooms))))
                (curr-y (cadr (center-rectangle-room (nth num-rooms rooms))))
                )
            (if (= (random 2) 1)
                (progn
                  (build-horizontal-corridor s (create-entity 'corridor :baseline prev-y :s prev-x :e curr-x))
                  (format t "corridor h ~a ~a ~a~%" prev-y prev-x curr-x)
                  (build-vertical-corridor s (create-entity 'corridor :baseline curr-x :s prev-y :e curr-y))
                  (format t "corridor v ~a ~a ~a~%" curr-x prev-y curr-y)
                  )
                (progn
                  (build-vertical-corridor s (create-entity 'corridor :baseline prev-x :s prev-y :e curr-y))
                  (format t "corridor v ~a ~a ~a~%" prev-x prev-y curr-y)
                  (build-horizontal-corridor s (create-entity 'corridor :baseline curr-y :s prev-x :e curr-x))
                  (format t "corridor h ~a ~a ~a~%" curr-y prev-x curr-x)
                  )
                )
            )
            )
        )
      (format t "num rooms ~a~%" num-rooms)
      )
      (format t "rooms ~a~%" rooms)
      (setf (slot-value player 'location/x) (car (center-rectangle-room (nth 1 rooms))))
      (setf (slot-value player 'location/y) (cadr (center-rectangle-room (nth 1 rooms))))
    ))

(defmethod print-scene ((s scene))
  "scene object print"
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (format t "scene ~a [~a ~a] ~a~%" s x y (aref (scene/cells s) x y)))))

(defmethod draw-scene ((s scene))
  "scene draw call"
  (draw-cells (scene/w s) (scene/h s) (scene/cells s)))

(defun make-scene (width height)
  "scene constructor - cells must still be initialised afterwards"
  (let (s)
    (setf s (make-instance 'scene :w width :h height))))


(defmethod init-scene ((s scene))
  "init scene with creatures (everything but terrain is a creature"
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (let ((cell (aref (scene/cells s) x y)))
        (when (slot-value cell 'impassable/state)
          (create-entity 'creature
                         :location/x x
                         :location/y y
                         :avatar/visual "#"
                         :avatar/color :black))))))
