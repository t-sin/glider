(in-package #:cl-user)
(defpackage #:glider/shooter
  (:use #:cl #:sdl2)
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
  (:import-from #:glider/actors
                #:actor-x
                #:actor-y
                #:actor-angle
                #:actor-available?
                #:actor-act-fn
                #:actor-draw-fn
                #:init-actors)
  (:export #:shooter-init
           #:shooter-proc))
(in-package #:glider/shooter)

(defstruct vm
  actors tick equeue)

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
            (actor-act-fn a) #'(lambda (vm a)
                                 (declare (ignore vm))
                                 (incf (actor-x a) (/ v tx))
                                 (incf (actor-y a) (/ v ty)))))))

(defun vm-shot (vm sx sy act-fn)
  (let ((a (alloc-actor vm)))
    (setf (actor-available? a) t
          (actor-x a) sx
          (actor-y a) sy
          (actor-act-fn a) act-fn)))

(defun execute (vm)
  (loop
    :for queue := (vm-equeue vm)
    :for e := (car queue)
    :while (and queue (= (car e) (vm-tick vm)))
    :do (progn
          (let ((op (cdr e)))
            (ecase (car op)
              (:shot (apply #'vm-shot vm (cdr op)))
              (:shot-to (apply #'vm-shot-to vm (cdr op)))
              (:interrupt)))
          (setf (vm-equeue vm) (cdr queue)))))

;;;
;;; shooter

;; TODO: use queues (instead of plain lists)
(defparameter *event-queue*
  `((0 . (:shot 500 150 ,(let* ((count 0)
                                (ph (to-rad 90)))
                           (lambda (vm a)
                             (setf (actor-x a) (+ 500 (* 100 (cos (* 0.03 count)))))
                             (when (mod count 10)
                               (vm-shot *vm* (actor-x a) (actor-y a)
                                        (let ((rad ph))
                                          (lambda (vm a)
                                            (incf (actor-x a)
                                                  (* 2 (cos rad)))
                                            (incf (actor-y a)
                                                  (* 2 (sin rad)))))))
                             (incf ph (to-rad 20))
                             (incf count)))))))

(defparameter *vm* nil)

(defun shooter-init ()
  (let ((actors (init-actors)))
    (loop
      :for a :across actors
      :do (setf (actor-draw-fn a)
                (lambda (renderer a)
                  (set-render-draw-color renderer 0 255 100 100)
                  (render-draw-rect renderer (make-rect (floor (actor-x a))
                                                        (floor (actor-y a))
                                                        10 10)))))
    (setf *vm* (make-vm :tick 0
                        :actors actors
                        :equeue *event-queue*))))

(defun shooter-proc (renderer)
  (let ((tick (vm-tick *vm*)))
    (execute *vm*)
    (loop
      :for a :across (vm-actors *vm*)
      :when (actor-available? a)
      :do (progn
            (funcall (actor-act-fn a) *vm* a)
            (funcall (actor-draw-fn a) renderer a))))
  (incf (vm-tick *vm*)))
