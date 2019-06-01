(in-package #:cl-user)
(defpackage #:glider/util
  (:use #:cl #:glider/const)
  (:export #:to-deg
           #:to-rad
           #:onto-screen
           #:load-png))
(in-package #:glider/util)

(defun to-deg (rad)
  (* rad (/ 180 PI)))

(defun to-rad (deg)
  (* deg (/ PI 180)))

(defun onto-screen (x y)
  (values (floor (+ *shooter-offset-x* x))
          (floor (+ *shooter-offset-y* y))))

(defun load-png (filename renderer)
  (let ((surface (sdl2-image:load-image filename)))
    (make-texture :renderer renderer
                  :width (sdl2:surface-width surface)
                  :height (sdl2:surface-height surface)
                  :texture (sdl2:create-texture-from-surface renderer surface))))
