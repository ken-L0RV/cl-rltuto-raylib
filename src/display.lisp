;;;; display-raylib.lisp

;;;; graphic lib. agnostic draw calls

(in-package #:cl-rltuto-raylib)

(defun draw-cells (width height cells)
  (draw-cell-grid width height cells))

(defun start-draw (player scene)
  (draw-screen player scene))
