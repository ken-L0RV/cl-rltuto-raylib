;;;; scene.lisp

;;;; scene object and related functions

(in-package #:cl-rltuto-raylib)

(defclass scene ()
  ((width :initarg :w :accessor scene/w)
   (height :initarg :h :accessor scene/h)
   (cells :accessor scene/cells))
  (:documentation "scene object that contains an array of cells"))

(defmethod initialize-instance :after ((s scene) &rest initargs)
  "init scene cells slot to a 2D array on object instantiation"
  (declare (ignore initargs))
  (setf (scene/cells s) (make-array (list (scene/w s) (scene/w s))))
  (init-cells s))

(defmethod init-cells ((s scene))
  "instantantiate a scene cells"
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (setf (aref (scene/cells s) x y) (create-entity 'cell :terrain/terrain :wall)))))

(defmethod print-scene ((s scene))
  "scene object print"
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (format t "scene ~a [~a ~a] ~a~%" s x y (aref (scene/cells s) x y)))))

(defun make-scene (width height)
  "scene constructor - cells must still be initialised afterwards"
  (let (s)
    (setf s (make-instance 'scene :w width :h height))))

(defmethod get-cell ((s scene) coords)
  (with-slots ((cells cells)) s
    (aref cells (car coords) (cadr coords))))

(defmethod reveal-scene ((s scene))
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (reveal-cell (get-cell s (list x y))))))

(defmethod reset-scene-visibility ((s scene))
  (dotimes (y (scene/h s))
    (dotimes (x (scene/w s))
      (setf (slot-value (aref (scene/cells s) x y) 'visible/visible) nil))))
