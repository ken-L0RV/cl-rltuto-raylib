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
               (perceptive :initform nil :documentation "can perceive its environment")
               (radius :initform 7 :documentation "perception radius"))

(define-aspect name
               (name :initform "" :documentation "has a name")
               (id :initform 0 :documentation "has an unique id"))

(define-aspect vitality
               (current :initform 0 :documentation "current vitality")
               (maximum :initform 1 :documentation "maximum vitality"))

(define-aspect power
               (power :initform 0 :documentation "power stat value"))

(define-aspect defense
               (defense :initform 0 :documentation "defence stat value"))

(define-aspect ai
               (ai :initform T :documentation "is AI controlled"))

(define-aspect consumable
               (consumable :initform nil :documentation "can be consumed"))

(define-aspect charges
               (number :initform 1 :documentation "has charges"))

(define-aspect potency
               (potency :initform 0 :documentation "has a potency"))

(define-aspect inventory
               (num-slots :initform 0 :documentation "max number of held items")
               (slots :documentation "held items"))

(define-aspect effect
               (effect :documentation "has an effect on the world")
               (target :documentation "effect target"))
