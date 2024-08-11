;;;; fov-raycasting.lisp

;;;; fov using raycasting
;;;; from https://www.roguebasin.com/index.php/Eligloscode

(in-package #:cl-rltuto-raylib)

(defconstant +rays+ 180)
(defconstant +rays-angle+ 0.01745)

(defmethod compute-raycast ((s scene) origin x y radius)
  (let* ((px (car origin)) (py (cadr origin))
         (ox (float (+ px 0.5)))
         (oy (float (+ py 0.5)))
         (coords (list (truncate ox) (truncate oy)))
         (cell (get-cell s (list (truncate ox) (truncate oy)))))
    (dotimes (v radius)
      (let ((coords (list (truncate ox) (truncate oy)))
            (cell (get-cell s (list (truncate ox) (truncate oy)))))
        (reveal-cell cell)
        (when (cell-opaque-p cell)
          (return-from compute-raycast nil))
        (setf ox (+ ox x))
        (setf oy (+ oy y))))))

(defmethod cast-light ((s scene) origin radius)
  (let ((x) (y) )
    (dotimes (i +rays+)
      (setf x (cos (* (* 2 i) +rays-angle+)))
      (setf y (sin (* (* 2 i) +rays-angle+)))
      (compute-raycast s origin x y radius))))

(defun update-fov (scene creature)
  (reset-scene-visibility scene)
  (cast-light scene
              (list (slot-value creature 'location/x) (slot-value creature 'location/y))
              (slot-value creature 'perceptive/radius)))

