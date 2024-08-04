;;;; components.lisp

;;;; game components

(in-package #:cl-rltuto-raylib)

(define-aspect impassable
               (impassable :initform nil :documentation "cannot be traversed" ))

(define-aspect opaque
               (opaque :initform nil :documentation "cannot be seen through"))

(define-aspect visible
               (visible :initform nil :documentation "can be seen"))

(define-aspect discovered
               (discovered :initform nil :documentation "has been seen by the player"))

(define-aspect terrain
               (terrain :initform :ground :documentation "terrain kind"))

(define-aspect location
               (x :initform 0 :documentation "x pos in current scene")
               (y :initform 0 :documentation "y pos in current scene"))

(define-aspect visual
               (visual :initform "" :documentation "has a visual representation")
               (color :initform :pink :documentation "visual representation color")) 

(define-aspect shape
               (vertices :initform nil :documentation "list of a shape vertices (x,y)"))

(define-aspect perceptive
               (perceptive :initform nil :documentation "can perceive its environment"))

(define-aspect name
               (name :initform "" :documentation "has a name"))
