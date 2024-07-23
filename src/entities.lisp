;;;; entities.lisp

;;;; game entities

(in-package #:cl-rltuto-raylib)

;;;; creature
(define-entity creature (visible location avatar))

(defmethod entity-created :after ((e creature))
  "if a creature is invisible, it has no avatar"
  (with-slots ((visible visible/state) (avatar avatar/visual)) e
    (if (null visible)
        (setf avatar ""))))

(defmethod move ((c creature) dx dy)
  "create movement in a scene"
  (let ((x (slot-value c 'location/x))
        (y (slot-value c 'location/y)))
    (setf (slot-value c 'location/x) (mod (+ x dx) *scene-width*))
    (setf (slot-value c 'location/y) (mod (+ y dy) *scene-height*))))

;;;; cell
(define-entity cell (impassable opaque terrain avatar))

(defmethod entity-created :after ((e cell))
  "if a cell is impassable, it is also opaque by default"
  (with-slots ((opaque opaque/state) (impassable impassable/state)) e
    (if (null opaque)
        (setf opaque impassable))))

;;;; rectangle-room
(define-entity rectangle-room (shape))

(defmethod initialize-instance :after ((e rectangle-room) &key x y w h)
  (with-slots ((v shape/vertex)) e
    (let ((x1 x) (y1 y) (x2 (+ x w)) (y2 (+ y h)))
      (setf v (list (list x1 y1) (list x2 y2))))))

(defmethod rectangle-x1 ((r rectangle-room))
  (caar (slot-value r 'shape/vertex)))

(defmethod rectangle-y1 ((r rectangle-room))
  (cadar (slot-value r 'shape/vertex)))

(defmethod rectangle-x2 ((r rectangle-room))
  (caadr (slot-value r 'shape/vertex)))

(defmethod rectangle-y2 ((r rectangle-room))
  (cadadr (slot-value r 'shape/vertex)))

(defmethod intersects-rectangle-room ((r0 rectangle-room) (r1 rectangle-room))
  (format t "r0 ~a ~a ~a ~a~%" (rectangle-x1 r0) (rectangle-x2 r0) (rectangle-y1 r0) (rectangle-y2 r0))
  (format t "r1 ~a ~a ~a ~a~%" (rectangle-x1 r1) (rectangle-x2 r1) (rectangle-y1 r1) (rectangle-y2 r1))
  (and (<= (rectangle-x1 r0) (rectangle-x2 r1))
       (>= (rectangle-x2 r0) (rectangle-x1 r1))
       (<= (rectangle-y1 r0) (rectangle-y2 r1))
       (>= (rectangle-y2 r0) (rectangle-y1 r1))))

(defmethod center-rectangle-room ((r rectangle-room))
  (let ((x1 (rectangle-x1 r))
        (x2 (rectangle-x2 r))
        (y1 (rectangle-y1 r))
        (y2 (rectangle-y2 r)))
    (list (ceiling (/ (+ x1 x2) 2)) (ceiling (/ (+ y1 y2) 2)))))

;;;; corridor
(define-entity corridor (shape))

(defmethod initialize-instance :after ((c corridor) &key baseline s e)
  (with-slots ((v shape/vertex)) c
    (setf v (list baseline (list s e)))))

(defmethod corridor-baseline ((c corridor))
     (car (slot-value c 'shape/vertex)))

(defmethod corridor-start ((c corridor))
     (caadr (slot-value c 'shape/vertex)))

(defmethod corridor-end ((c corridor))
     (cadadr (slot-value c 'shape/vertex)))

;(defparameter *vcorr* (create-entity 'corridor 
;                                     :baseline 1 :s 13 :e 27))
;*vcorr*
;(slot-value *vcorr* 'shape/vertex)
;(car (slot-value *vcorr* 'shape/vertex))
;(caadr (slot-value *vcorr* 'shape/vertex))
;(cadadr (slot-value *vcorr* 'shape/vertex))
;(setf *vcorr* nil)

;(defvar *rroom* (create-entity 'rectangle-room :shape/vertex (list (list 0 1) (list 2 3))))
;(slot-value *rroom* 'shape/vertex)
;(car (slot-value *rroom* 'shape/vertex))
;(caar (slot-value *rroom* 'shape/vertex))
;(cadar (slot-value *rroom* 'shape/vertex))
;(cdr (slot-value *rroom* 'shape/vertex))
;(cadr (slot-value *rroom* 'shape/vertex))
;(caadr (slot-value *rroom* 'shape/vertex))
;(cadadr (slot-value *rroom* 'shape/vertex))
