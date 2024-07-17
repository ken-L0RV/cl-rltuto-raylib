;;;; entities.lisp

;;;; game entities

(in-package #:cl-rltuto-raylib)

(define-entity cell (impassable opaque terrain avatar))

(defmethod entity-created :after ((e cell))
  "if a cell is impassable, it is also opaque by default"
  (with-slots ((opaque opaque/state) (impassable impassable/state)) e
    (if (null opaque)
        (setf opaque impassable))))

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
