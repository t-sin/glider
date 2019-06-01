(defpackage #:glider
  (:use #:cl #:sdl2
        #:glider/const
        #:glider/util)
  (:import-from #:glider/scenes/shooter
                #:init-shooter)
  (:import-from #:glider/scenes/title
                #:init-title)
  (:export #:game-main))
(in-package #:glider)

(defparameter *global* nil)

(defun game-init (renderer)
  (setf *global* (make-global))
  (setf (global-scene-fn *global*) (init-title *global*))

  ;; TODO: this referencing may be a performance bottle neck
  (setf (getf glider/const:*game-images* :bg) (load-png #P"assets/bg.png" renderer))
  (setf (getf glider/const:*game-images* :bullet) (load-png #P"assets/bullet.png" renderer)))

(defun game-proc (renderer)
  (funcall (global-scene-fn *global*) renderer))

(defun game-main ()
  (with-init (:video)
    (sdl2-image:init '(:png))
    (unwind-protect
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
               (:quit () t))))
      (sdl2-image:quit))))
