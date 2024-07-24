;;;; entities.lisp

;;;; game entities

(in-package #:cl-rltuto-raylib)

;;;; creature
(define-entity creature (location visible visual))

(defmethod entity-created :after ((e creature))
  "if a creature is invisible, it has no visual and if it has a visual, it is visible"
  (with-slots ((visible visible/visible) (visual visual/visual)) e
    (unless (eq visual "")
      (setf visible T))))

(defmethod move ((c creature) dx dy)
  "create movement in a scene"
  (with-slots ((x location/x) (y location/y)) c
    (setf x (+ x dx))
    (setf y (+ y dy))))

(defun make-npc (x y)
  (create-entity 'creature :location/x x :location/y y :visual/visual "@" :visual/color :yellow))

(defun make-wall (x y)
  (create-entity 'creature :location/x x :location/y y :visual/visual "#" :visual/color :black))

;;;; cell
(define-entity cell (impassable opaque terrain visual))

(defmethod entity-created :after ((e cell))
  "if a cell is impassable, it is also opaque by default"
  (with-slots ((terrain terrain/terrain)) e
    (update-terrain e terrain)))

(defmethod update-terrain ((c cell) new-terrain)
  (with-slots ((impassable impassable/impassable)
               (opaque opaque/opaque)
               (terrain terrain/terrain)
               (color visual/color)) c
    (cond ((eq new-terrain :ground) (progn (setf impassable nil)
                                           (setf opaque nil)
                                           (setf terrain :ground)
                                           (setf color :gray)))
          ((eq new-terrain :wall) (progn (setf impassable T)
                                         (setf opaque T)
                                         (setf terrain :wall)
                                         (setf color :darkgray)))
          ((eq new-terrain :water) (progn (setf impassable T)
                                          (setf opaque nil)
                                          (setf terrain :water)
                                          (setf color :blue))))))

;;;; rectangle-room
(define-entity rectangle-room (shape))

(defmethod initialize-instance :after ((e rectangle-room) &key x y w h)
  (with-slots ((v shape/vertices)) e
    (let ((x2 (+ x w)) (y2 (+ y h)))
      (setf v (list (list x y) (list x2 y2))))))

(defmethod rectangle-x1 ((r rectangle-room))
  (with-slots ((v shape/vertices)) r
    (caar v)))

(defmethod rectangle-y1 ((r rectangle-room))
  (with-slots ((v shape/vertices)) r
    (cadar v)))

(defmethod rectangle-x2 ((r rectangle-room))
  (with-slots ((v shape/vertices)) r
    (caadr v)))

(defmethod rectangle-y2 ((r rectangle-room))
  (with-slots ((v shape/vertices)) r
    (cadadr v)))

(defmethod intersects-rectangle-room-p ((r0 rectangle-room) (r1 rectangle-room))
  ;(format t "r0 ~a ~a ~a ~a~%" (rectangle-x1 r0) (rectangle-x2 r0) (rectangle-y1 r0) (rectangle-y2 r0))
  ;(format t "r1 ~a ~a ~a ~a~%" (rectangle-x1 r1) (rectangle-x2 r1) (rectangle-y1 r1) (rectangle-y2 r1))
  (and (<= (rectangle-x1 r0) (rectangle-x2 r1))
       (>= (rectangle-x2 r0) (rectangle-x1 r1))
       (<= (rectangle-y1 r0) (rectangle-y2 r1))
       (>= (rectangle-y2 r0) (rectangle-y1 r1))))

(defmethod find-center-rectangle-room ((r rectangle-room))
  (let ((x1 (rectangle-x1 r))
        (y1 (rectangle-y1 r))
        (x2 (rectangle-x2 r))
        (y2 (rectangle-y2 r)))
    (list (ceiling (/ (+ x1 x2) 2)) (ceiling (/ (+ y1 y2) 2)))))

;;;; corridor
(define-entity corridor (shape))

(defmethod initialize-instance :after ((c corridor) &key baseline s e)
  (with-slots ((v shape/vertices)) c
    (setf v (list baseline (list s e)))))

(defmethod corridor-baseline ((c corridor))
  (with-slots ((v shape/vertices)) c
    (car v)))

(defmethod corridor-start ((c corridor))
  (with-slots ((v shape/vertices)) c
    (caadr v)))

(defmethod corridor-end ((c corridor))
  (with-slots ((v shape/vertices)) c
    (cadadr v)))
