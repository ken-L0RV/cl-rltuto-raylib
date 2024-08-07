;;;; display-raylib.lisp

;;;; raylib draw calls

(in-package #:cl-rltuto-raylib)

(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)
(defparameter *cell-size* 20)
(defparameter *grid-origin-x* *cell-size*)
(defparameter *grid-origin-y* *cell-size*)

(defun extract-rgba (color)
  (let ((a (logand color #xFF))
        (b (logand (ash color -8) #xFF))
        (g (logand (ash color -16) #xFF))
        (r (logand (ash color -24) #xFF)))
    (list r g b a)))

(defun build-rgba (rgba)
  (let ((r (car rgba))
        (g (cadr rgba))
        (b (caddr rgba))
        (a (cadddr rgba)))
    (make-rgba r g b a)))

(defun alter-rgba-alpha (rgba coeff)
  (let ((alpha (cadddr rgba)))
    (setf (cadddr rgba) (truncate (* alpha coeff))))
  rgba)

(defun gen-alt-color (color coeff)
  (let ((rgba (extract-rgba color)))
    (setf rgba (alter-rgba-alpha rgba coeff))
    (build-rgba rgba)))

(defun draw-game-over ()
  "draws game over screen"
  (draw-text "Game Over " (round (/ *screen-width* 5)) (/ *screen-height* 2) 100 :raywhite))

(defun draw-creature (creature)
  "draws a creature visual at x y on a cell grid"
      (let ((visual (slot-value creature 'visual/visual))
            (color (slot-value creature 'visual/color))
            (x (slot-value creature 'location/x))
            (y (slot-value creature 'location/y)))
        (draw-text visual
                   (+ *grid-origin-x* (* x *cell-size*))
                   (+ *grid-origin-y* (* y *cell-size*))
                   *cell-size* color)))

(defun draw-cell (cell x y)
  "draw a cell"
  (let ((color (slot-value cell 'visual/color)))
    (when (and (cell-discovered-p cell)
               (not (cell-visible-p cell)))
      (setf color (gen-alt-color (get-color color) 0.5)))
    (draw-rectangle (+ *grid-origin-x* (* x *cell-size*))
                    (+ *grid-origin-y* (* y *cell-size*))
                    *cell-size*
                    *cell-size*
                    color)))

(defun draw-cell-grid-explored (width height cells)
  "draws a rectangular grid of width x heigh cells from the top-left corner"
  (dotimes (y height)
    (dotimes (x width)
      (let* ((cell (aref cells x y))
             (visible (slot-value cell 'visible/visible))
             (discovered (slot-value cell 'discovered/discovered)))
        (when (or visible discovered)
          (draw-cell cell x y))))))

(defun draw-cell-grid (width height cells)
  "draws a rectangular grid of width x heigh cells from the top-left corner"
  (dotimes (y height)
    (dotimes (x width)
      (draw-cell (aref cells x y) x y))))

(defmethod draw-scene ((s scene))
  "scene draw call"
  ;(draw-cell-grid (scene/w s) (scene/h s) (scene/cells s))
  (draw-cell-grid-explored (scene/w s) (scene/h s) (scene/cells s)))

(defun update-fov (scene creature)
  (reset-scene-visibility scene)
  (cast-light scene (list (slot-value creature 'location/x)
                          (slot-value creature 'location/y))))

(defun handle-keys ()
  (let ((action nil))
    (when (is-key-pressed :key-right) (setf action (list :movement (cons 1 0))))
    (when (is-key-pressed :key-left) (setf action (list :movement (cons -1 0))))
    (when (is-key-pressed :key-down) (setf action (list :movement (cons 0 1))))
    (when (is-key-pressed :key-up) (setf action (list :movement (cons 0 -1))))
    (when (is-key-pressed :key-c) (setf action (list :reveal-scene T)))
    action))

(defun draw-screen (player scene)
  (let ((game-state :player-turn))
    (with-window (*screen-width* *screen-height* "roguelike tuto w/ raylib in cl")
      (set-target-fps 60)
      (loop
        until (window-should-close)
        do
        (if (eql game-state :exit)
            (draw-game-over)
            (setf game-state (game-tick player *entities* scene game-state)))
        (with-drawing
          (clear-background :black)
          (draw-fps 0 0)
          (draw-scene scene)
          (run-update-creature-visibility)
          (run-draw-visible-corpses)
          (run-draw-visible-creatures))))))

