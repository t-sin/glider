(in-package #:cl-user)
(defpackage #:glider/util
  (:use #:cl)
  (:export #:to-deg
           #:to-rad))
(in-package #:glider/util)

(defun to-deg (rad)
  (* rad (/ 180 PI)))

(defun to-rad (deg)
  (* deg (/ PI 180)))
