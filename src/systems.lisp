;;;; systems.lisp

;;;; game systems

(in-package #:cl-rltuto-raylib)

(define-system log-all-entities ((entity))
  "log every entity"
  (print entity))
;(run-log-all-entities)

(define-system draw-all-creatures ((entity visible location))
  "draw all visible creatures with a position"
  (draw-creature entity))
