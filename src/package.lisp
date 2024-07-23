;;;; package.lisp

(defpackage #:cl-rltuto-raylib
  (:use #:cl)
  (:import-from #:beast
                #:define-aspect
                #:define-entity
                #:create-entity
                #:destroy-entity
                #:define-system
                #:entity-created
                #:entity-destroyed
                #:clear-entities)
  (:import-from #:raylib
                #:with-window
                #:set-target-fps
                #:window-should-close
                #:is-key-down
                #:is-key-pressed
                #:with-drawing
                #:clear-background
                #:draw-fps
                #:draw-text
                #:draw-rectangle)
  (:export #:main))
