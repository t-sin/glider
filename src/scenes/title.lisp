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

(defun make-event ()
  `((0 . (:fire 0 0 $id))))

(defun make-drawer ()
  (lambda (renderer a)
    ))


(defun init-title (g)
  (let ((actors (init-actors)))
    (loop
      :for a :across actors
      :do (setf (actor-draw-fn a) (make-drawer)
                (actor-sfn a) (lambda () nil)))
    (setf (global-vm g) (make-vm :tick 0
                                 :actors actors
                                 :etable (make-event))))
  (lambda (renderer)
    (let ((vm (global-vm g)))
      (execute vm)
      (loop
        :for a :across (vm-actors vm)
        :when (actor-available? a)
        :do (progn
              (funcall (actor-act-fn a) vm a (actor-sfn a))
              (funcall (actor-draw-fn a) renderer a)))
      (incf (vm-tick vm)))))
