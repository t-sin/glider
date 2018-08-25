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
  (:export #:shooter-init
           #:shooter-proc))
(in-package #:glider/shooter)

;;;
;;; actors

(defstruct actor
  tag available? start-tick px py
  pos-fn draw-fn act-fn)

(defun init-actors ()
  (let ((array (make-array 5000)))
    (loop
      :for n :from 0 :below (length array)
      :do (setf (aref array n) (make-actor)))
    array))


;; disable-on-out
(defun disable-on-out (a pos tick)
  (declare (ignore tick))
  (let ((x (car pos))
        (y (cdr pos)))
    (when (or (> -15 (- x *shooter-offset-x*)) (< *shooter-width* (- x *shooter-offset-x*))
              (> -15 (- y *shooter-offset-y*)) (< *shooter-height* (- y *shooter-offset-y*)))
      (setf (actor-available? a) nil))))

;;;
;;; vm

(defstruct vm
  actors tick equeue)

(defun alloc-actor (vm)
  (let ((idx (position-if #'(lambda (a) (null (actor-available? a)))
                          (vm-actors vm))))
    (when idx
      (aref (vm-actors vm) idx))))

(defun rad-to-deg (rad)
  (* rad (/ 180 PI)))

(defun deg-to-rad (deg)
  (* deg (/ PI 180)))

(defun vm-shot-to (vm sx sy v rad)
  (let ((a (alloc-actor vm)))
    (when a
      (setf (actor-available? a) t
            (actor-px a) sx
            (actor-py a) sy
            (actor-pos-fn a) #'(lambda (x y tick)
                                 (declare (ignore tick))
                                 (cons (+ x (* v (cos rad)))
                                       (+ y (* v (sin rad)))))))))

(defun vm-shot (vm sx sy move-fn)
  (let ((a (alloc-actor vm)))
    (setf (actor-available? a) t
          (actor-px a) sx
          (actor-py a) sy
          (actor-pos-fn a) move-fn)))

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

(defparameter *event-queue*
  `(,@(loop
        :for n :from 0 :upto 200
        :append (loop
                  :for n2 :from 2 :upto 6
                  :for deg := (+ 90 (* 80 (sin (/ n PI))))
                  :collect (cons (* n 5) `(:shot-to 500 100 ,(* n2 0.7) ,(deg-to-rad deg)))))))

(defun draw-bullet (renderer pos dir tick)
  (declare (ignore tick))
  (let* ((img (getf *game-images* :bullet))
         (w (texture-width img))
         (h (texture-height img))
         (tex (glider/const:texture-texture img)))
    (set-texture-blend-mode tex :add)
    (render-copy renderer tex
                 :dest-rect (make-rect (floor (- (car pos) (/ w 2)))
                                       (floor (-(cdr pos) (/ h 2)))
                                       w h))))

(defparameter *vm* nil)

(defun shooter-init ()
  (setf *vm* (make-vm :tick 0
                      :actors (init-actors)
                      :equeue *event-queue*)))

(defun shooter-proc (renderer)
  (let ((tick (vm-tick *vm*)))
    (execute *vm*)
    (loop
      :for a :across (vm-actors *vm*)
      :when (and a (actor-available? a))
      :do (let ((pos (funcall (actor-pos-fn a)
                              (actor-px a) (actor-py a) tick)))
            (setf (actor-px a) (car pos)
                  (actor-py a) (cdr pos))
            (when pos
              (funcall #'draw-bullet renderer pos 0 tick)
;;              (funcall (actor-act-fn a) a pos tick)
              ))))
  (incf (vm-tick *vm*)))
