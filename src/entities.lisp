;;;; entities.lisp

;;;; game entities

(in-package #:cl-rltuto-raylib)

;;;; creature
(define-entity creature (location visible visual perceptive name impassable vitality power defense ai inventory))

(defmethod entity-created :after ((e creature))
  "if a creature has a visual, it is visible"
  (with-slots ((visible visible/visible)
               (visual visual/visual)
               (impassable impassable/impassable)
               (max-vit vitality/maximum)
               (curr-vit vitality/current)
               (num-slots inventory/num-slots)
               (slots inventory/slots)) e
    (unless (eq visual "")
      (setf visible T))
    (setf impassable T)
    (setf curr-vit max-vit)
    (setf slots (make-array (list num-slots)))
    ))

(defmethod change-creature-vitality ((c creature) amount)
  (with-slots ((c-vit vitality/current) (m-vit vitality/maximum)) c
    (let ((new-vit (+ c-vit amount))
          (o-c-vit c-vit))
      (cond ((> new-vit m-vit) (setf c-vit m-vit))
            ((< new-vit 0) (setf c-vit 0))
            (T (setf c-vit new-vit)))
      (- c-vit o-c-vit))))

(defmethod damage-vitality ((c creature) amount)
  (with-slots ((vit vitality/current)) c
    (change-creature-vitality c (* amount -1))))

(defmethod heal-vitality ((c creature) amount)
  (with-slots ((vit vitality/current)) c
    (change-creature-vitality c amount)))

(defmethod attack-creature ((attacker creature) (defender creature))
  (with-slots ((a-power power/power) (a-name name/name)) attacker
    (with-slots ((d-defense defense/defense) (d-vit vitality/current) (d-name name/name)) defender
     (let ((damage (- a-power d-defense)))
       ;(damage-vitality defender damage)
       ;(format nil "The ~A attacks the ~A for ~a damage." a-name d-name damage)))))
       (format nil "The ~A attacks the ~A for ~a damage." a-name d-name (abs (damage-vitality defender damage)))))))

(defmethod heal-creature ((target creature) amount)
  (with-slots ((c-vit vitality/current) (m-vit vitality/maximum) (name name/name)) target
    (if (= c-vit m-vit)
        nil
        (format nil "The ~A heals for ~a." name (heal-vitality target amount)))))

(defmethod creature-dead-p ((c creature))
  (with-slots ((vitality vitality/current)) c
    (<= vitality 0)))

(defmethod make-corpse ((c creature))
  (with-slots ((visual visual/visual)
               (color visual/color)
               (name name/name)
               (impassable impassable/impassable)
               (ai ai/ai)) c
    (setf visual "%")
    (setf color :red)
    (setf name (concatenate 'string name " corpse"))
    (setf impassable nil)
    (setf ai nil)))

(defmethod kill-creature ((c creature))
  ;(format t "destroy creature ~a~%" c)
  ;(setf *entities* (delete c *entities*))
  ;(destroy-entity c)
  (make-corpse c))

(defmethod move ((c creature) dx dy)
  "create movement in a scene"
  (with-slots ((x location/x) (y location/y)) c
    (setf x (+ x dx))
    (setf y (+ y dy))))

(defmethod spawn-creature ((c creature) x y)
  (move c x y)
  c)

(defmethod creature-location ((c creature))
  (with-slots ((x location/x) (y location/y)) c
    (list x y)))

(defun location-occupied-p (entities x y)
  (dolist (entity entities)
    (unless (or (null entity) (creature-dead-p entity))
      (when (and (= (slot-value entity 'location/x) x)
                 (= (slot-value entity 'location/y) y))
        (return entity)))))

(defun location-occupant (entities x y)
  (dolist (entity entities)
    (unless (or (null entity))
      (when (and (= (slot-value entity 'location/x) x)
                 (= (slot-value entity 'location/y) y))
        (return entity)))))

(defmethod inventory-free-slot ((c creature))
  (with-slots ((num-slots inventory/num-slots) (slots inventory/slots)) c
    ;(format t "n-slot ~a slots ~a~%" num-slots slots)
    (dotimes (n num-slots)
      (let ((item (aref slots n)))
        ;(format t "inventory ~a item ~a~%" n item)
        (unless (item? item)
          (return n))))))

(defun make-player ()
  (create-entity 'creature :visual/visual "@" :visual/color :white
                 :perceptive/perceptive T :name/name "Player" :ai/ai nil
                 :vitality/maximum 30 :power/power 5 :defense/defense 2
                 :inventory/num-slots 4))

(defun make-npc ()
  (create-entity 'creature :visual/visual "@" :visual/color :yellow
                 :perceptive/perceptive T :name/name "NPC"
                 :vitality/maximum 1 :power/power 1 :defense/defense 1))

(defun make-troll ()
  (create-entity 'creature :visual/visual "T" :visual/color :green
                 :perceptive/perceptive T :name/name "Troll"
                 :vitality/maximum 16 :power/power 4 :defense/defense 1))

(defun make-orc ()
  (create-entity 'creature :visual/visual "o" :visual/color :green
                 :perceptive/perceptive T :name/name "Orc"
                 :vitality/maximum 10 :power/power 3 :defense/defense 0))

(defun make-wall ()
  (create-entity 'creature :visual/visual "#" :visual/color :black
                 :name/name "Wall" :ai/ai nil
                 :vitality/maximum 999 :power/power 0 :defense/defense 999))

;;;; cell
(define-entity cell (impassable opaque visible discovered terrain visual))

(defmethod entity-created :after ((e cell))
  "if a cell is impassable, it is also opaque by default"
  (with-slots ((terrain terrain/terrain)) e
    (update-terrain e terrain)))

(defmethod update-terrain ((c cell) new-terrain)
  (with-slots ((impassable impassable/impassable)
               (opaque opaque/opaque)
               (terrain terrain/terrain)
               (color visual/color)) c
    (cond ((eq new-terrain :ground) (progn (setf impassable nil)
                                           (setf opaque nil)
                                           (setf terrain :ground)
                                           (setf color :gray)))
          ((eq new-terrain :wall) (progn (setf impassable T)
                                         (setf opaque T)
                                         (setf terrain :wall)
                                         (setf color :darkgray)))
          ((eq new-terrain :water) (progn (setf impassable T)
                                          (setf opaque nil)
                                          (setf terrain :water)
                                          (setf color :blue))))))

(defmethod reveal-cell ((c cell))
  (with-slots ((visible visible/visible)
               (discovered discovered/discovered)) c
    (setf visible T)
    (setf discovered T)))

(defmethod match-cell-terrain-p ((c cell) terrain)
  (with-slots ((cell-terrain terrain/terrain)) c
    (eq cell-terrain terrain)))

(defmethod cell-impassable-p ((c cell))
  (with-slots ((impassable impassable/impassable)) c
    impassable))

(defmethod cell-opaque-p ((c cell))
  (with-slots ((opaque opaque/opaque)) c
    opaque))

(defmethod cell-visible-p ((c cell))
  (with-slots ((visible visible/visible)) c
    visible))

(defmethod cell-discovered-p ((c cell))
  (with-slots ((discovered discovered/discovered)) c
    discovered))

;;;; rectangle-room
(define-entity rectangle-room (shape))

(defmethod initialize-instance :after ((e rectangle-room) &key x y w h)
  (with-slots ((v shape/vertices)) e
    (let ((x2 (+ x w)) (y2 (+ y h)))
      (setf v (list (list x y) (list x2 y2))))))

(defmethod rectangle-x1 ((r rectangle-room))
  (with-slots ((v shape/vertices)) r
    (caar v)))

(defmethod rectangle-y1 ((r rectangle-room))
  (with-slots ((v shape/vertices)) r
    (cadar v)))

(defmethod rectangle-x2 ((r rectangle-room))
  (with-slots ((v shape/vertices)) r
    (caadr v)))

(defmethod rectangle-y2 ((r rectangle-room))
  (with-slots ((v shape/vertices)) r
    (cadadr v)))

(defmethod intersects-rectangle-room-p ((r0 rectangle-room) (r1 rectangle-room))
  (and (<= (rectangle-x1 r0) (rectangle-x2 r1))
       (>= (rectangle-x2 r0) (rectangle-x1 r1))
       (<= (rectangle-y1 r0) (rectangle-y2 r1))
       (>= (rectangle-y2 r0) (rectangle-y1 r1))))

(defmethod find-center-rectangle-room ((r rectangle-room))
  (let ((x1 (rectangle-x1 r))
        (y1 (rectangle-y1 r))
        (x2 (rectangle-x2 r))
        (y2 (rectangle-y2 r)))
    (list (ceiling (/ (+ x1 x2) 2)) (ceiling (/ (+ y1 y2) 2)))))

;;;; corridor
(define-entity corridor (shape))

(defmethod initialize-instance :after ((c corridor) &key baseline s e)
  (with-slots ((v shape/vertices)) c
    (setf v (list baseline (list s e)))))

(defmethod corridor-baseline ((c corridor))
  (with-slots ((v shape/vertices)) c
    (car v)))

(defmethod corridor-start ((c corridor))
  (with-slots ((v shape/vertices)) c
    (caadr v)))

(defmethod corridor-end ((c corridor))
  (with-slots ((v shape/vertices)) c
    (cadadr v)))

(define-entity item (name location visible visual consumable charges potency))

(defun location-item (items x y)
  (dolist (item items)
    ;(format t "item ~a~%" item)
    (unless (null item)
      (let ((ix (location/x item))
            (iy (location/y item)))
        (unless (or (null ix) (null iy))
          (when (and (= ix x)
                     (= iy y))
            (return item)))))))

(defun location-furnished-p (entities x y)
  (dolist (entity entities)
    (unless (null entity)
      (when (and (= (slot-value entity 'location/x) x)
                 (= (slot-value entity 'location/y) y))
        (return entity)))))

(defmethod spawn-item ((i item) dx dy)
  (with-slots ((x location/x) (y location/y)) i
    (setf x (+ x dx))
    (setf y (+ y dy)))
  i)

(defun make-potion (id)
  (cond ((= id 1)
         (create-entity 'item :name/name "Small vitality potion"
                        :visual/visual "!" :visual/color :purple
                        :consumable/consumable T :charges/number 1 :potency/potency 4))
        (T
         (format t "unrecognized potion id ~a~%" id))))

(defmethod use-potion ((i item) id target)
  (let ((results (list T)))
    (cond ((= id 1)
           (setf results (use-potion-vit-1 i target)))
          (T
           (format t "unrecognized potion kind~%")))
    results))

(defmethod use-potion-vit-1 ((i item) (c creature))
  (with-slots ((i-charges charges/number) (i-potency potency/potency)) i
    (let ((new-charges (- i-charges 1))
          (result (heal-creature c i-potency)))
      (list new-charges result))))

