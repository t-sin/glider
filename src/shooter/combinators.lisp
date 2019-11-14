(in-package #:cl-user)
(defpackage #:glider/combinators
  (:nicknames :c)
  (:use #:cl 
        #:glider/actors
        #:glider/vm)
  (:import-from #:glider/util
                #:to-rad
                #:to-deg)
  (:export #:$id

           ;; control flow
           #:$when
           #:$progn
           #:$times

           ;; stateful
           #:$count
           #:$while
           #:$schedule

           ;; side-effective
           #:$move
           #:$fire
           #:$disable))
(in-package #:glider/combinators)

(defun $id (&rest args) args)

(defun $when (pred f)
  (lambda (vm a sfn)
    (when (funcall pred vm a sfn)
      (funcall f vm a sfn))))

(defun $times (f n)
  (lambda (vm a sfn)
    (declare (ignore sfn))
    (dotimes (i n)
      (let ((%i i) (%n n))
        (flet ((n* () (values %i %n)))
          (funcall f vm a #'n*))))))

(defun $progn (&rest flis)
  (lambda (vm a sfn)
    (dolist (f flis)
      (funcall f vm a sfn))))

(defun $schedule (&rest events)
  (let ((e (first events))
        (events (rest events)))
    (lambda (vm a sfn)
      (when (and (consp e) (> (- (vm-tick vm) (actor-start-tick a)) (car e)))
        (funcall (cdr e) vm a sfn)
        (setf e (first events)
              events (rest events))))))

(defun $while (f frames)
  (let ((start nil))
    (lambda (vm a sfn)
      (when (null start)
        (setf start (vm-tick vm)))
      (when (>= (- (vm-tick vm) start) frames)
        (setf (actor-available? a) nil))
      (funcall f vm a sfn))))

(defun $count (f &optional (d 1))
  (let ((c 0))
    (lambda (vm a sfn)
      (declare (ignore sfn))
      (funcall f vm a (let ((c* c)) (lambda () c*)))
      (incf c d))))

(defun $move (dx-fn dy-fn)
  (lambda (vm a sfn)
    (incf (actor-x a) (funcall dx-fn vm a sfn))
    (incf (actor-y a) (funcall dy-fn vm a sfn))))

(defun $fire (f)
  (lambda (vm a sfn)
    (vm-fire vm (actor-x a) (actor-y a) 
             (lambda (vm a %sfn)
               (declare (ignore %sfn))
               (funcall f vm a sfn))
             (actor-draw-fn a))))

(defun $disable ()
  (lambda (vm a sfn)
    (declare (ignore vm sfn))
    (setf (actor-available? a) nil)))