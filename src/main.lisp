(defpackage #:glider
  (:use #:cl #:sdl2
        #:glider/const)
  (:import-from #:glider/scenes/shooter
                #:init-shooter)
  (:export #:game-main))
(in-package #:glider)

(defparameter *global* nil)

(defun game-init (renderer)
  (setf *global* (make-global))
  (setf (global-scene-fn *global*) (init-shooter *global*))

  ;; TODO: this referencing may be a performance bottle neck
  (setf (getf glider/const:*game-images* :bg) (load-png #P"assets/bg.png" renderer))
  (setf (getf glider/const:*game-images* :bullet) (load-png #P"assets/bullet.png" renderer)))

(defun game-proc (renderer)
  (set-render-draw-color renderer 0 0 20 255)
  (set-render-draw-blend-mode renderer :add)
  (render-clear renderer)
  (funcall (global-scene-fn *global*) renderer)
  (render-copy renderer (texture-texture (getf *game-images* :bg))
               :dest-rect (make-rect 0 0 1200 800)))

(defun game-main ()
  (with-init (:video)
    (with-window (window :title "The Glider"
                              :w *screen-width*
                              :h *screen-height*
                              :flags '(:shown))
      (with-renderer (renderer window :index -1 :flags '(:accelerrated))
        (game-init renderer)
        (with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
           (when (scancode= (scancode-value keysym) :scancode-escape)
             (push-event :quit)))
          (:idle ()
           (game-proc renderer)
           (render-present renderer)
           (delay (floor 10)))
          (:quit () t))))))
