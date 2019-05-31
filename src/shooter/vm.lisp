(in-package #:cl-user)
(defpackage #:glider/vm
  (:use #:cl #:glider/actors)
  (:import-from #:glider/util
                #:to-rad
                #:to-deg)
  (:export #:vm
           #:make-vm
           #:vm-actors
           #:vm-tick
           #:vm-etable
           
           #:alloc-actor
           #:vm-fire
           #:execute))
(in-package #:glider/vm)

(defstruct vm
  actors tick etable)

(defun alloc-actor (vm)
  (let ((idx (position-if #'(lambda (a) (null (actor-available? a)))
                          (vm-actors vm))))
    (when idx
      (aref (vm-actors vm) idx))))

(defun vm-fire (vm sx sy act-fn draw-fn)
  (let ((a (alloc-actor vm)))
    (when a
      (setf (actor-available? a) t
            (actor-x a) sx
            (actor-y a) sy
            (actor-start-tick a) (vm-tick vm)
            (actor-act-fn a) act-fn
            (actor-draw-fn a) draw-fn))))

(defun execute (vm)
  (loop
    :for queue := (vm-etable vm)
    :for e := (car queue)
    :while (and queue (= (car e) (vm-tick vm)))
    :do (progn
          (let ((op (cdr e)))
            (case (car op)
              (:fire (apply #'vm-fire vm (cdr op)))
              (:interrupt)
              (t nil)))
          (setf (vm-etable vm) (cdr queue)))))
