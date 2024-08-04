;;;; run.lisp

(load "cl-rltuto-raylib.asd")

(ql:quickload "cl-rltuto-raylib")

(in-package #:cl-rltuto-raylib)

(handler-case
  (main) (error (c)
         (format t "~&An error occured: ~a~&" c)
         (uiop:quit 1)))
