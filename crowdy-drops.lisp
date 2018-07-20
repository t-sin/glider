(in-package #:cl-user)
(defpackage #:crowdy-drops
  (:use #:cl #:sdl2))
(in-package #:crowdy-drops)

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

;;;
;;; game objects

(defstruct object pos-fn draw-fn act-fn available?)
(defparameter *objects* (make-array 5000 :initial-element nil))

(defun make-object* (pos-fn draw-fn act-fn)
  (make-object :pos-fn (lambda (tick) (funcall pos-fn tick))
               :draw-fn draw-fn
               :act-fn act-fn
               :available? t))

(defun alloc-object (pos-fn draw-fn act-fn)
  (let ((idx (position-if #'(lambda (o) (or (null o) (null (object-available? o)))) *objects*)))
    (when idx
      (let ((obj (make-object* pos-fn draw-fn act-fn)))
        (setf (aref *objects* idx) obj)
        obj))))

;;;
;;; bullet

(defun draw-bullet (renderer pos tick)
  (declare (ignore tick))
  (set-render-draw-color renderer 50 100 180 255)
  (render-fill-rect renderer
                    (make-rect (floor (car pos)) (floor (cdr pos)) 10 10)))

(defun disable-on-out (obj pos tick)
  (declare (ignore tick))
  (let ((x (car pos))
        (y (cdr pos)))
    (when (or (> 0 x) (< *screen-width* x)
              (> 0 y) (< *screen-height* y))
      (setf (object-available? obj) nil))))

(defun shoot-arround (v)
  (loop
    :for deg :from 0 :upto 360 :by 15
    :do (let ((obj (alloc-object (let ((dx (* v (cos (* deg (/ pi 180)))))
                                       (dy (* v (sin (* deg (/ pi 180)))))
                                       (x 400)
                                       (y 300))
                                   #'(lambda (tick)
                                       (declare (ignore tick))
                                       (cons (incf x dx) (incf y dy))))
                                 #'draw-bullet
                                 #'disable-on-out)))
          (when obj
            (setf (object-available? obj) t)))))

;;;
;;; system

(defun game-init ()
  (alloc-object #'(lambda (tick)
                    (when (zerop (mod tick 40))
                      (dotimes (n 10)
                        (shoot-arround (float (1+ (* 1.6 n))))))
                    (cons 400 300))
                #'(lambda (renderer pos tick)
                    (declare (ignore tick))
                    (set-render-draw-color renderer 255 0 0 255)
                    (render-fill-rect renderer
                                      (make-rect (floor (car pos)) (floor (cdr pos)) 20 20)))
                #'disable-on-out))

(defparameter *tick* 0)

(defun game-proc (renderer)
  (loop
    :for o :across *objects*
    :when (and o (object-available? o))
    :do (let ((pos (funcall (object-pos-fn o) *tick*)))
          (when pos
            (funcall (object-draw-fn o) renderer pos *tick*)
            (funcall (object-act-fn o) o pos *tick*))))
  (incf *tick*))

(defun game-main ()
  (with-init (:video)
    (with-window (window :title "sdl2 test"
                              :w *screen-width*
                              :h *screen-height*
                              :flags '(:shown))
      (with-renderer (renderer window :index -1 :flags '(:accelerrated))
        (game-init)
        (with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
           (when (scancode= (scancode-value keysym) :scancode-escape)
             (push-event :quit)))
          (:idle ()
           (set-render-draw-color renderer 255 255 255 0)
           (render-clear renderer)
           (game-proc renderer)
           (render-present renderer))
          (:quit () t))))))
