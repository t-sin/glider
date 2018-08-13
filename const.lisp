(in-package #:cl-user)
(defpackage #:bhshooter/const
  (:use #:cl #:sdl2)
  (:export #:*screen-width*
           #:*screen-height*
           #:*game-images*
           #:texture-renderer
           #:texture-width
           #:texture-height
           #:texture-texture
           #:load-png))
(in-package #:bhshooter/const)

(defstruct texture
  renderer width height texture)

(defun load-png (filename renderer)
  (sdl2-image:init '(:png))
  (let ((surface (sdl2-image:load-image filename)))
    (make-texture :renderer renderer
                  :width (surface-width surface)
                  :height (surface-height surface)
                  :texture (create-texture-from-surface renderer surface))))

(defparameter *screen-width* 1200)
(defparameter *screen-height* 800)

(defparameter *game-images* nil)
