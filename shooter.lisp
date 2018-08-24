(in-package #:cl-user)
(defpackage #:bhshooter/shooter
  (:use #:cl #:sdl2)
  (:shadowing-import-from #:bhshooter/const
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
(in-package #:bhshooter/shooter)

;;;
;;; actors

(defstruct actor
  tag available? start-tick px py
  pos-fn draw-fn act-fn)
(defparameter *actors*
  (let ((array (make-array 5000 :element-type 'actor)))
    (loop
      :for n :from 0 :below (length array)
      :do (setf (aref array n) (make-actor)))
    array))


(defun alloc-actor ()
  (let ((idx (position-if #'(lambda (a) (null (actor-available? a)))
                          *actors*)))
    (when idx
      (aref *actors* idx))))


;; disable-on-out
(defun disable-on-out (a pos tick)
  (declare (ignore tick))
  (let ((x (car pos))
        (y (cdr pos)))
    (when (or (> -15 (- x *shooter-offset-x*)) (< *shooter-width* (- x *shooter-offset-x*))
              (> -15 (- y *shooter-offset-y*)) (< *shooter-height* (- y *shooter-offset-y*)))
      (setf (actor-available? a) nil))))

;;;
;;; shooter

(defun shooter-init ())

(defparameter *tick* 0)

(defun shooter-proc (renderer)
  (loop
    :for a :across *actors*
    :when (and a (actor-available? a))
    :do (let ((pos (funcall (actor-pos-fn a) *tick*)))
          (when pos
            (funcall (actor-draw-fn a) renderer pos *tick*)
            (funcall (actor-act-fn a) a pos *tick*))))
  (incf *tick*))
