(in-package #:cl-user)
(defpackage #:bhshooter
  (:use #:cl #:sdl2))
(in-package #:bhshooter)

(defparameter *screen-width* 1200)
(defparameter *screen-height* 800)


;;;
;;; move abstraction

(defun make-move-linear (x y v deg)
  (let* ((rad (* deg (/ pi 180)))
         (dx (* v (cos rad)))
         (dy (* v (sin rad)))
         (x x)
         (y y))
    (lambda (tick)
      (declare (ignore tick))
      (cons (prog1 x (incf x dx)) (prog1 y (incf y dy))))))

(defun make-move (x y incfn)
  (let ((x x)
        (y y))
    #'(lambda (tick)
        (multiple-value-bind (newx newy)
            (funcall incfn tick x y)
          (setf x newx y newy)
          (cons newx newy)))))

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
  ;; (set-render-draw-color renderer 50 100 180 120)
  ;; (render-draw-rect renderer
  ;;                   (make-rect (floor (- (car pos) 5)) (floor (- (cdr pos) 5)) 10 10))
  (let* ((img (getf *game-images* :bullet))
         (w (texture-width img))
         (h (texture-height img))
         (tex (texture-texture img)))
    (set-texture-blend-mode tex :add)
    (render-copy renderer tex
                 :dest-rect (make-rect (floor (- (car pos) (/ w 2))) (floor (- (cdr pos) (/ h 2)))
                                       w h))))

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
    (alloc-object (make-move x y
                             (let ((d deg))
                               #'(lambda (tick x y)
                                   (declare (ignore tick))
                                   (values (+ x (* v (cos (* (/ pi 180) d))))
                                           (+ y (* v (sin (* (/ pi 180) d))))))))
                  #'draw-bullet
                  #'disable-on-out)))

;;;
;;; shooter

(let ((d 0))
  (defun act-enemy (tick)
    (let ((half-width (/ *screen-width* 2))
          (half-height (- (/ *screen-height* 2) 200)))
      (when (zerop (mod tick 10))
        (shoot-arround 2 (incf d 102.30) half-width half-height))
      (cons half-width half-height))))
        

(defun draw-enemy (renderer pos tick)
  (declare (ignore tick))
  (set-render-draw-color renderer 255 255 255 200)
  (render-fill-rect renderer (make-rect (- (car pos) 10) (- (cdr pos) 10) 20 20)))

(defun shooter-init ()
  (alloc-object #'act-enemy #'draw-enemy #'disable-on-out))

(defparameter *tick* 0)

(defun shooter-proc (renderer)
  (loop
    :for o :across *objects*
    :when (and o (object-available? o))
    :do (let ((pos (funcall (object-pos-fn o) *tick*)))
          (when pos
            (funcall (object-draw-fn o) renderer pos *tick*)
            (funcall (object-act-fn o) o pos *tick*))))
  (incf *tick*))

;;;
;;; system

(defstruct texture
  renderer width height texture)

(defun load-png (filename renderer)
  (sdl2-image:init '(:png))
  (let ((surface (sdl2-image:load-image filename)))
    (make-texture :renderer renderer
                  :width (surface-width surface)
                  :height (surface-height surface)
                  :texture (create-texture-from-surface renderer surface))))

(defparameter *game-images* nil)

(defun game-init (renderer)
  (setf (getf *game-images* :bg) (load-png #P"bg.png" renderer))
  (setf (getf *game-images* :bullet) (load-png #P"bullet.png" renderer)))

(defun game-proc (renderer)
  (set-render-draw-color renderer 0 0 20 255)
  (set-render-draw-blend-mode renderer :add)
  (render-clear renderer)
  (shooter-proc renderer)
  (render-copy renderer (texture-texture (getf *game-images* :bg))
               :dest-rect (make-rect 0 0 1200 800)))

(defun game-main ()
  (with-init (:video)
    (with-window (window :title "sdl2 test"
                              :w *screen-width*
                              :h *screen-height*
                              :flags '(:shown))
      (with-renderer (renderer window :index -1 :flags '(:accelerrated))
        (game-init renderer)
        (shooter-init)
        (with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
           (when (scancode= (scancode-value keysym) :scancode-escape)
             (push-event :quit)))
          (:idle ()
           (game-proc renderer)
           (render-present renderer)
           (delay (floor 10)))
          (:quit () t))))))
