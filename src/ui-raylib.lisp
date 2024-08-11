;;;; ui-raylib.lisp

;;;; raylib draw calls

(in-package #:cl-rltuto-raylib)

(defparameter *ui-origin-x* (+ *grid-origin-x* (* *visible-scene-width* *cell-size*)))
(defparameter *ui-origin-y* (+ *grid-origin-x* (* *visible-scene-height* *cell-size*)))

(defparameter *max-drawn-messages* 14)
(defparameter *drawn-messages* (make-array (list *max-drawn-messages*) :element-type 'string :initial-element ""))
(defparameter *cursor-focus-message* nil)

(defun screen-to-cell (p p-origin)
  (+ p-origin (floor (/ (- p *cell-size*) *cell-size*))))

(defun update-cursor-focus ()
  "draws the name of the entity under cursor focus"
  (let* ((c-pos (get-mouse-position))
         (sx-pos (screen-to-cell (vx c-pos) *v-origin-x*))
         (sy-pos (screen-to-cell (vy c-pos) *v-origin-y*))
         (target (location-occupant *entities* sx-pos sy-pos)))
    (if target
        (let ((name (slot-value target 'name/name)))
          (setf *cursor-focus-message* name))
        (setf *cursor-focus-message* ""))))

(defun draw-cursor-focus (message)
  "draws the message log box"
  (draw-text (format nil "~A" message)
             (+ *grid-origin-x* (* *cell-size* 4))
             (+ *ui-origin-y* (* *cell-size* 0))
             *cell-size*
             :yellow))

(defun update-drawn-messages ()
  (let* ((log *messages*)
         (len-log (list-length log))
         (num-msg nil))
    (if (< len-log *max-drawn-messages*)
        (setf num-msg len-log)
        (setf num-msg *max-drawn-messages*))
    (dotimes (i num-msg)
      (setf (aref *drawn-messages* (- *max-drawn-messages* (1+ i))) (getf (nth (- len-log (1+ i)) log) :message)))))

(defun draw-message-log (messages num-msg)
  "draws the message log box"
  (dotimes (m num-msg)
    (let ((msg (aref messages m)))
      (draw-text (format nil "~A" msg)
                 (+ *ui-origin-x* (* *cell-size* 1))
                 (+ *grid-origin-y* (* *cell-size* (- 33 m)))
                 15 ;*cell-size*
                 :white))))

(defun draw-creature-loc (creature)
  "draws the player vitality value"
  (let ((p-x (slot-value creature 'location/x))
        (p-y (slot-value creature 'location/y)))
    (draw-text (format nil "(~2d,~2d)" p-x p-y)
               *grid-origin-x*
               (+ *ui-origin-y* (* *cell-size* 0))
               *cell-size*
               :white)))

(defun draw-creature-health (creature)
  "draws the player vitality value"
  (let ((current (slot-value creature 'vitality/current))
        (maximum (slot-value creature 'vitality/maximum)))
    (draw-text (format nil "~2d/~2d" current maximum)
               (+ *ui-origin-x* (* *cell-size* 1))
               (+ *grid-origin-y* (* *cell-size* 1))
               *cell-size*
               :raywhite)))

(defun draw-creature-health-bar (creature)
  "draws the player health bar"
  (let* ((current (slot-value creature 'vitality/current))
         (maximum (slot-value creature 'vitality/maximum))
         (h-ratio (/ current maximum))
         (max-bar-length (* *cell-size* 16))
         (curr-bar-length (round (* h-ratio max-bar-length))))
    (draw-rectangle (+ *ui-origin-x* (* *cell-size* 1))
                    (+ *grid-origin-y* (* *cell-size* 1))
                    max-bar-length
                    *cell-size*
                    :red)
    (draw-rectangle (+ *ui-origin-x* (* *cell-size* 1))
                    (+ *grid-origin-y* (* *cell-size* 1))
                    curr-bar-length
                    *cell-size*
                    :green)))

(defun draw-creature-name (creature)
  "draws the player vitality value"
  (let ((name (slot-value creature 'name/name)))
    (draw-text (format nil "~A" name)
               (+ *ui-origin-x* (* *cell-size* 1))
               (+ *grid-origin-y* (* *cell-size* 0))
               *cell-size*
               :white)))

(defun draw-ui (creature)
  (draw-creature-loc creature)
  (draw-creature-health-bar creature)
  (draw-creature-health creature)
  (draw-creature-name creature)
  (draw-cursor-focus *cursor-focus-message*)
  (draw-message-log *drawn-messages* *max-drawn-messages*))

