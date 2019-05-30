(defpackage #:glider
  (:use #:cl #:sdl2
        #:glider/game)
  (:shadowing-import-from #:glider/const
                          #:*screen-width*
                          #:*screen-height*
                          #:*game-images*
                          #:texture-texture
                          #:texture-width
                          #:texture-height
                          #:load-png)
  (:export #:game-main))
(in-package #:glider)

(defun game-init (renderer)
  (setf (getf glider/const:*game-images* :bg) (load-png #P"assets/bg.png" renderer))
  (setf (getf glider/const:*game-images* :bullet) (load-png #P"assets/bullet.png" renderer)))

(defun game-proc (renderer)
  (set-render-draw-color renderer 0 0 20 255)
  (set-render-draw-blend-mode renderer :add)
  (render-clear renderer)
  (shooter-proc renderer)
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
