;;;; game-map.lis

(in-package #:cl-rltuto-raylib)

(defclass scene ()
  ((width :initarg :w :accessor scene/w)
   (height :initarg :h :accessor scene/h)
   (cells :accessor scene/cells))
  (:documentation "scene object that contains an array of cells"))

(defmethod initialize-instance :after ((s scene) &rest initargs)
  "init scene cells slot to a 2D array on object instantiation"
  (declare (ignore initargs))
  (setf (scene/cells s) (make-array (list (scene/w s) (scene/w s)))))

(defmethod initialise-cells ((s scene))
  "instantantiate a scene cells"
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (setf (aref (scene/cells s) x y) (create-entity 'cell)))))

(defmethod print-scene ((s scene))
  "scene object print"
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (format t "scene ~a [~a ~a] ~a~%" s x y (aref (scene/cells s) x y)))))

(defmethod draw-scene ((s scene))
  "scene draw call"
  (draw-cells (scene/w s) (scene/h s) (scene/cells s)))

(defun make-scene (width height)
  "scene constructor - cells must still be initialised afterwards"
  (let (s)
    (setf s (make-instance 'scene :w width :h height))))
