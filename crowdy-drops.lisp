(in-package #:cl-user)
(defpackage #:crowdy-drops
  (:use #:cl #:sdl2))
(in-package #:crowdy-drops)

(defparameter *screen-width* 1200)
(defparameter *screen-height* 800)


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
        (setf (object-available? obj) t)
        obj))))

;;;
;;; bullet

(defun draw-bullet (renderer pos tick)
  (declare (ignore tick))
  (set-render-draw-blend-mode renderer :blend)
  (set-render-draw-color renderer 50 100 180 120)
  (render-draw-rect renderer
                    (make-rect (floor (- (car pos) 5)) (floor (- (cdr pos) 5)) 10 10)))

(defun disable-on-out (obj pos tick)
  (declare (ignore tick))
  (let ((x (car pos))
        (y (cdr pos)))
    (when (or (> 0 x) (< *screen-width* x)
              (> 0 y) (< *screen-height* y))
      (setf (object-available? obj) nil))))

(defun shoot-arround (v d x y)
  (do ((deg d (+ deg 20)))
      ((>= deg (+ d 360)))
    (alloc-object (let ((dx (* v (cos (* deg (/ pi 180)))))
                        (dy (* v (sin (* deg (/ pi 180)))))
                        (x x)
                        (y y))
                    #'(lambda (tick)
                        (declare (ignore tick))
                        (cons (incf x dx) (incf y dy))))
                  #'draw-bullet
                  #'disable-on-out)))

(defun shoot (v)
  (alloc-object (let ((dy v)
                      (y 600))
                  #'(lambda (tick)
                      (declare (ignore tick))
                      (cons 400 (incf y dy))))
                #'draw-bullet
                #'disable-on-out))

;;;
;;; system

(let ((d 0))
  (defun act-enemy (tick)
    (let ((half-width (/ *screen-width* 2))
          (half-height (- (/ *screen-height* 2) 200)))
      (when (zerop (mod tick 10))
        (shoot-arround 2 (incf d 11.30) half-width half-height))
      (cons half-width half-height))))
        

(defun draw-enemy (renderer pos tick)
  (declare (ignore tick))
  (set-render-draw-blend-mode renderer :blend)
  (set-render-draw-color renderer 255 255 255 200)
  (render-fill-rect renderer (make-rect (- (car pos) 10) (- (cdr pos) 10) 20 20)))

(defun game-init ()
  (alloc-object #'act-enemy #'draw-enemy #'disable-on-out))

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
           (set-render-draw-color renderer 255 255 255 255)
           (set-render-draw-blend-mode renderer :add)
           (set-render-draw-color renderer 0 0 20 255)
           (render-clear renderer)
           (game-proc renderer)
           (render-present renderer)
           (delay (floor 10)))
          (:quit () t))))))
