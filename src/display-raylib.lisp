;;;; display-raylib.lisp

;;;; raylib draw calls

(in-package #:cl-rltuto-raylib)

;;;; gui parameters
(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)
(defparameter *cell-size* 20)
(defparameter *grid-origin-x* *cell-size*)
(defparameter *grid-origin-y* *cell-size*)
;;;; visible scene size
(defparameter *visible-scene-width* 45)
(defparameter *visible-scene-height* 34)
(defparameter *v-origin-x* nil)
(defparameter *v-origin-y* nil)

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
        (setf x (- x *v-origin-x*))
        (setf y (- y *v-origin-y*))
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

(defmethod draw-scene ((s scene))
  "scene draw call"
  (draw-cell-grid-explored (scene/w s) (scene/h s) (scene/cells s)))

(defun set-visible-scene (player scene)
  (let ((player-x (slot-value player 'location/x))
        (player-y (slot-value player 'location/y))
        (v-width-half (floor (/ *visible-scene-width* 2)))
        (v-height-half (floor (/ *visible-scene-height* 2)))
        (cells (slot-value scene 'cells))
        (visible-cells (make-array (list *visible-scene-width* *visible-scene-height*))))
    (let* ((e-bound (>= (+ player-x v-width-half) *scene-width*))
           (s-bound (>= (+ player-y v-height-half) *scene-height*))
           (w-bound (< (- player-x v-width-half) 0))
           (n-bound (< (- player-y v-height-half) 0)))

      (setf *v-origin-x* (- player-x v-width-half))
      (setf *v-origin-y* (- player-y v-height-half))

      (when e-bound
        (setf *v-origin-x* (- *scene-width* *visible-scene-width*)))
      (when s-bound
        (setf *v-origin-y* (- *scene-height* *visible-scene-height*))) 
      (when w-bound
        (setf *v-origin-x* 0))
      (when n-bound
        (setf *v-origin-y* 0))
      ;(format t "e ~a s ~a w ~a n ~a~%" e-bound s-bound w-bound n-bound)
      ;(format t "player-x ~a player-y ~a v-width-half ~a v-height-half ~a~%" player-x player-y v-width-half v-height-half)
      ;(format t "*v-origin-x* ~a *v-origin-y* ~a~%" *v-origin-x* *v-origin-y*)
      (loop :for y :from *v-origin-y* :below (+ *visible-scene-height* *v-origin-y*)
            :do
            (loop :for x :from *v-origin-x* :below (+ *visible-scene-width* *v-origin-x*)
                  :do
                  (let ((w (- x *v-origin-x*))
                        (h (- y *v-origin-y*)))
                    ;(format t "x ~a y ~a w ~a h ~a~%" x y w h)
                    (setf (aref visible-cells w h) (aref cells x y)))))
      visible-cells)))

(defun handle-keys ()
  (let ((action nil))
    (when (or (is-key-pressed :key-right)
              (is-key-down :key-l)) (setf action (list :movement (cons 1 0))))
    (when (or (is-key-pressed :key-down)
              (is-key-down :key-j)) (setf action (list :movement (cons 0 1))))
    (when (or (is-key-pressed :key-left)
              (is-key-down :key-h)) (setf action (list :movement (cons -1 0))))
    (when (or (is-key-pressed :key-up)
              (is-key-down :key-k)) (setf action (list :movement (cons 0 -1))))
    (when (is-key-pressed :key-kp-0) (setf action (list :wait T)))
    (when (is-key-pressed :key-c) (setf action (list :reveal-scene T)))
    action))

(defun draw-visible-screen (player scene)
  (let* ((game-state :player-turn)
         (visible-scene (make-scene *visible-scene-width* *visible-scene-height*)))
    (with-window (*screen-width* *screen-height* "roguelike tuto w/ raylib in cl")
      (set-target-fps 60)
      (loop
        until (window-should-close)
        do
        (setf game-state (game-tick player *entities* scene game-state))
        (with-drawing
          (clear-background :black)
          (draw-fps 0 0)

          (draw-ui player)

          (setf (slot-value visible-scene 'cells) (set-visible-scene player scene) )
          (draw-scene visible-scene)

          (run-update-creature-visibility)
          (run-draw-visible-corpses)
          (run-draw-visible-creatures)

          (update-cursor-focus)

          (when (eql game-state :exit)
            (draw-game-over)))))))

;(defun draw-cell-grid (width height cells)
;  "draws a rectangular grid of width x heigh cells from the top-left corner"
;  (dotimes (y height)
;    (dotimes (x width)
;      (draw-cell (aref cells x y) x y))))
