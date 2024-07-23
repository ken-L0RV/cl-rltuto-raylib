;;;; components.lisp

;;;; game components

(in-package #:cl-rltuto-raylib)

(define-aspect impassable
               (state :initform nil :documentation "cannot be traversed" ))

(define-aspect opaque
               (state :initform nil :documentation "cannot be seen through"))

(defparameter *terrains-kinds* (list 'ground
                                     'wall))
(define-aspect terrain
               (terrain :initform 'ground  :documentation "terrain kind"))

(define-aspect visible
               (state :initform T :documentation "can be seen"))

(define-aspect location
               (x :initform 0 :documentation "x pos in current scene")
               (y :initform 0 :documentation "y pos in current scene"))

(define-aspect avatar
               (visual :initform "" :documentation "has a visual representation")
               (color :initform :red :documentation "visual representation color")) 

(define-aspect shape
               (vertex :initform "" :documentation "list of vertex coordinates"))
