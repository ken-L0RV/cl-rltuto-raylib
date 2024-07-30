;;;; systems.lisp

;;;; game systems

(in-package #:cl-rltuto-raylib)

(define-system log-all-entities ((entity))
  "log every entity"
  (print entity))

(define-system draw-all-creatures ((entity visible location))
  "draw all visible creatures with a position"
  (draw-creature entity))

(define-system log-impassable-cells ((entity impassable))
 "log all impassable cells"
 (format t "~a~%" entity))

(define-system log-opaque-cells ((entity opaque))
 "log all opaque cells"
 (format t "~a~%" entity))

(define-system log-perceptive-entities ((entity perceptive))
  "log every entity"
  (print entity))
