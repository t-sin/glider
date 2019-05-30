(in-package #:cl-user)
(defpackage #:glider/shooter
  (:use #:cl
        #:sdl2
        #:glider/util
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
  (:export #:shooter-init
           #:shooter-proc))
(in-package #:glider/shooter)


(defun %aim-n-way (gw x y) ;; not primitive
  (lambda (vm a sfn)
    (multiple-value-bind (i n)
        (funcall sfn)
      (+ (atan y x) (to-rad (* (- i (/ n 2)) gw))))))

(defun %aim (x y)
  (lambda (vm a sfn)
    (atan y x)))

(defun %rotate (gw)
  (lambda (vm a sfn)
    (multiple-value-bind (i n)
        (funcall sfn)
      (to-rad (+ (* (actor-start-tick a) (actor-start-tick a) 0.03)
                 (* (- i (/ n 2)) gw))))))

(defun %move-1 (ang-fn v)
  (flet ((move (f)
           (lambda (vm a sfn)
             (* (funcall f (funcall ang-fn vm a sfn)) v))))
    (list (move #'cos) (move #'sin))))

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
  `((0 . (:fire ,(/ *shooter-width* 2) 100
          ,($when (%count-n? 2)
                  ($times ($fire ($move (%move-2 #'cos (%rotate 72) 1 14)
                                        (%move-2 #'sin (%rotate 72) 1 14)))
                          5))))
    (100 . (:fire ,(- (/ *shooter-width* 2) 150) 70
            ,($when (%count-n? 13)
                    ($times ($fire (apply #'$move (%move-1 (%aim-n-way 10 1 3) 3)))
                            7))))
    (100 . (:fire ,(+ (/ *shooter-width* 2) 150) 70
            ,($when (%count-n? 13)
                    ($times ($fire (apply #'$move (%move-1 (%aim-n-way 10 -1 3) 3)))
                            7))))
    (200 . (:fire ,(/ *shooter-width* 2) 200
            ,($progn (apply #'$move (%move-1 (%aim 10 200) 5))
                     ($schedule
                      `(10 . ,($fire (apply #'$move (%move-1 (%aim 10 10) 1))))
                      `(20 . ,($fire (apply #'$move (%move-1 (%aim 10 10) 1))))
                      `(30 . ,($fire (apply #'$move (%move-1 (%aim 10 10) 1))))))))
    ))

(defparameter *vm* nil)

(defun default-drawer ()
  (lambda (renderer a)
    (multiple-value-bind (x y)
        (onto-screen (actor-x a) (actor-y a))
      (set-render-draw-color renderer 0 255 100 100)
      (let* ((r 10)
             (r/2 (floor (/ r 2))))
        (render-fill-rect renderer (make-rect (- x r/2) (- y r/2) r r))))))

(defun shooter-init ()
  (let ((actors (init-actors)))
    (loop
      :for a :across actors
      :do (setf (actor-draw-fn a) (default-drawer)
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
