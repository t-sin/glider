(in-package #:cl-user)
(defpackage #:glider/actors
  (:use #:cl)
  (:export #:actor
           #:make-actor
           #:actor-p
           #:actor-x
           #:actor-y
           #:actor-angle
           #:actor-start-tick
           #:actor-sfn
           #:actor-available?
           #:actor-act-fn
           #:actor-draw-fn

           #:init-actors))
(in-package #:glider/actors)

(defstruct actor
  x y angle sfn
  start-tick
  available?
  act-fn draw-fn)

(defun init-actors (&optional (n 5000))
  (let ((array (make-array n)))
    (loop
      :for n :from 0 :below (length array)
      :do (setf (aref array n) (make-actor)))
    array))
