(in-package #:cl-user)
(defpackage #:glider/scenes/title
  (:use #:cl
        #:sdl2
        #:glider/const
        #:glider/util
        #:glider/vm
        #:glider/actors
        #:glider/combinators)
  (:export #:init-title))
(in-package #:glider/scenes/title)

(defun make-rpolygon-drawer (r n)
  (let ((pts (coerce (loop
                       :for i :from 1 :upto n
                       :for deg := (* i (/ 360 n))
                       :collect (cons (* r (cos (to-rad deg)))
                                      (* r (sin (to-rad deg)))))
                     'simple-vector)))
    (lambda (renderer a)
      (set-render-draw-color renderer 100 200 200 255)
      (loop
        :for i :from 0 :below n
        :for p1 := (svref pts i)
        :for p2 := (svref pts (mod (1+ i) n))
        :do (render-draw-line renderer
                              (floor (+ (actor-x a) (car p1)))
                              (floor (+ (actor-y a) (cdr p1)))
                              (floor (+ (actor-x a) (car p2)))
                              (floor (+ (actor-y a) (cdr p2))))))))

(defun make-event ()
  `((0 . (:fire 100 400 ,#'$id ,(make-rpolygon-drawer 20 3)))
    (0 . (:fire 210 400 ,#'$id ,(make-rpolygon-drawer 20 4)))
    (0 . (:fire 320 400 ,#'$id ,(make-rpolygon-drawer 20 5)))
    (0 . (:fire 100 510 ,#'$id ,(make-rpolygon-drawer 20 6)))
    (0 . (:fire 210 510 ,#'$id ,(make-rpolygon-drawer 20 7)))
    (0 . (:fire 320 510 ,#'$id ,(make-rpolygon-drawer 20 8)))))

(defun init-title (g)
  (let ((actors (init-actors)))
    (loop
      :for a :across actors
      :do (setf (actor-sfn a) (lambda () nil)))
    (setf (global-vm g) (make-vm :tick 0
                                 :actors actors
                                 :etable (make-event))))
  (lambda (renderer)
    (let ((vm (global-vm g)))
      (execute vm)
      (render-clear renderer)
      (set-render-draw-color renderer 0 0 25 255)
      (set-render-draw-blend-mode renderer :blend)
      (render-fill-rect renderer (make-rect 0 0 *screen-width* *screen-height*))
      (loop
        :for a :across (vm-actors vm)
        :when (actor-available? a)
        :do (progn
              (funcall (actor-act-fn a) vm a (actor-sfn a))
              (funcall (actor-draw-fn a) renderer a)))
      (incf (vm-tick vm)))))
