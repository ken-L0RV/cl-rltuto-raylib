;;;; display-raylib.lisp

;;;; raylib draw calls

(in-package #:cl-rltuto-raylib)

(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)
(defparameter *cell-size* 20)
(defparameter *grid-origin-x* *cell-size*)
(defparameter *grid-origin-y* *cell-size*)

(defun draw-cell-grid (width height cells)
  "draws a rectangular grid of width x heigh cells from the top-left corner"
  (dotimes (y height)
    (dotimes (x width)
      (draw-cell (aref cells x y) x y))))

(defun draw-cell (cell x y)
  "draw a cell"
  (let  ((color))
        (if (slot-value cell 'opaque/state)
            (setf color :darkgray)
            (setf color :gray))
        (draw-rectangle (+ *grid-origin-x* (* x *cell-size*))
                        (+ *grid-origin-y* (* y *cell-size*))
                        *cell-size*
                        *cell-size*
                        color)))

(defun draw-creature (creature)
  "draws a creature visual at x y on a cell grid"
      (let ((visual (slot-value creature 'avatar/visual))
            (color (slot-value creature 'avatar/color))
            (x (slot-value creature 'location/x))
            (y (slot-value creature 'location/y)))
        (draw-text visual
                   (+ *grid-origin-x* (* x *cell-size*))
                   (+ *grid-origin-y* (* y *cell-size*))
                   *cell-size* color)))

(defun handle-keys ()
  (let ((action nil))
    (when (is-key-pressed :key-right) (setf action (list :movement (cons 1 0))))
    (when (is-key-pressed :key-left) (setf action (list :movement (cons -1 0))))
    (when (is-key-pressed :key-down) (setf action (list :movement (cons 0 1))))
    (when (is-key-pressed :key-up) (setf action (list :movement (cons 0 -1))))
    action))

(defun draw-screen (player scene)
  (with-window (*screen-width* *screen-height* "roguelike tuto w/ raylib in cl")
    (set-target-fps 60)
    (loop
      until (window-should-close)
      do
      (let* ((action (handle-keys))
             (movement (getf action :movement)))
        (when movement
          (let* ((player-x (slot-value player 'location/x))
                 (player-y (slot-value player 'location/y))
                 (cells (scene/cells scene))
                 (cell (aref cells (+ player-x (car movement)) (+ player-y (cdr movement))))
                )
            (unless (slot-value cell 'impassable/state)
              (move player (car movement) (cdr movement)))))
        (with-drawing
          (clear-background :black)
          (draw-fps 0 0)
          (draw-scene scene)
          (run-draw-all-creatures))))))
