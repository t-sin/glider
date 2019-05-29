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

(defun vm-shot-to (vm sx sy tx ty v)
  (let ((a (alloc-actor vm)))
    (when a
      (setf (actor-available? a) t
            (actor-x a) sx
            (actor-y a) sy
            (actor-sfn a) sfn
            (actor-act-fn a) #'(lambda (vm a)
                                 (declare (ignore vm))
                                 (incf (actor-x a) (/ v tx))
                                 (incf (actor-y a) (/ v ty)))))))

(defun vm-fire (vm sx sy act-fn)
  (let ((a (alloc-actor vm)))
    (when a
      (setf (actor-available? a) t
            (actor-x a) sx
            (actor-y a) sy
            (actor-start-tick a) (vm-tick vm)
            (actor-act-fn a) act-fn))))

(defun execute (vm)
  (loop
    :for queue := (vm-etable vm)
    :for e := (car queue)
    :while (and queue (= (car e) (vm-tick vm)))
    :do (progn
          (let ((op (cdr e)))
            (ecase (car op)
              (:fire (apply #'vm-fire vm (cdr op)))
              (:interrupt)))
          (setf (vm-etable vm) (cdr queue)))))
