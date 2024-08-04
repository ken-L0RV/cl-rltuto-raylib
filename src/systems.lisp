;;;; systems.lisp

;;;; game systems

(in-package #:cl-rltuto-raylib)

(define-system log-all-entities ((entity))
  "log every entity"
  (print entity))

(define-system update-creature-visibility ((entity visible location))
  "update entities visibility depending on the cell visibility"
  (with-slots ((x location/x) (y location/y) (visible visible/visible)) entity
    (let ((cell (get-cell *main-scene* (list x y))))
      (if (or (cell-visible-p cell)
              (match-cell-terrain-p cell :wall))
          (setf visible T)
          (setf visible nil)))))

(define-system draw-visible-creatures ((entity visible location))
  "draw all visible creatures with a position"
  (when (visible/visible entity)
    (draw-creature entity)))

(define-system log-impassable-cells ((entity impassable))
 "log all impassable cells"
 (format t "~a~%" entity))

(define-system log-opaque-cells ((entity opaque))
 "log all opaque cells"
 (format t "~a~%" entity))

