(in-package #:cl-user)
(defpackage #:glider/shooter
  (:use #:cl
        #:sdl2
        #:glider/vm
        #:glider/actors
        #:glider/combinators)
  (:shadowing-import-from #:glider/const
                          #:*shooter-offset-x*
                          #:*shooter-offset-y*
                          #:*shooter-width*
                          #:*shooter-height*
                          #:*game-images*
                          #:texture-texture
                          #:texture-width
                          #:texture-height)
  (:import-from #:glider/util
                #:to-deg
                #:to-rad)
  (:export #:shooter-init
           #:shooter-proc))
(in-package #:glider/shooter)


(defun %aim (gw x y)
  (lambda (vm a sfn)
    (multiple-value-bind (i n)
        (funcall sfn)
      (+ (atan y x) (to-rad (* (- i (/ n 2)) gw))))))

(defun %rotate (gw)
  (lambda (vm a sfn)
    (multiple-value-bind (i n)
        (funcall sfn)
      (to-rad (+ (* (actor-start-tick a) (actor-start-tick a) 0.03)
                 (* (- i (/ n 2)) gw))))))

(defun %move-1 (tri-fn ang-fn v)
  (lambda (vm a sfn)
    (* (funcall tri-fn (funcall ang-fn vm a sfn)) v)))

(defun %move-2 (tri-fn ang-fn init-v v)
  (lambda (vm a sfn)
    (* (funcall tri-fn (funcall ang-fn vm a sfn))
       (+ init-v
          (/ v (+ 1 (/ (- (vm-tick vm) (actor-start-tick a)) 10)))))))

(defun %count-n? (n)
  (lambda (vm a sfn)
    (declare (ignore sfn))
    (zerop (mod (- (vm-tick vm) (actor-start-tick a)) n))))

;; TODO: use queues (instead of plain lists)
(defparameter *event-table*
  `((0 . (:fire 600 200 ,($when (%count-n? 2)
                                ($times ($fire ($move (%move-2 #'cos (%rotate 72) 1 14)
                                                      (%move-2 #'sin (%rotate 72) 1 14)))
                                        5))))
    (100 . (:fire 480 180 ,($when (%count-n? 13)
                                  ($times ($fire ($move (%move-1 #'cos (%aim 10 1 3) 3)
                                                        (%move-1 #'sin (%aim 10 1 3) 3)))
                                          7))))
    (100 . (:fire 720 180 ,($when (%count-n? 13)
                                  ($times ($fire ($move (%move-1 #'cos (%aim 10 -1 3) 3)
                                                        (%move-1 #'sin (%aim 10 -1 3) 3)))
                                          7))))
))

(defparameter *vm* nil)

(defun shooter-init ()
  (let ((actors (init-actors)))
    (loop
      :for a :across actors
      :do (setf (actor-draw-fn a)
                (lambda (renderer a)
                  (set-render-draw-color renderer 0 255 100 100)
                  (render-fill-rect renderer (make-rect (floor (actor-x a))
                                                        (floor (actor-y a))
                                                        10 10)))
                (actor-sfn a) (lambda () nil)))
    (setf *vm* (make-vm :tick 0
                        :actors actors
                        :etable *event-table*))))

(defun shooter-proc (renderer)
  (execute *vm*)
  (loop
    :for a :across (vm-actors *vm*)
    :when (actor-available? a)
    :do (progn
          (funcall (actor-act-fn a) *vm* a (actor-sfn a))
          (funcall (actor-draw-fn a) renderer a)))
  (incf (vm-tick *vm*)))
