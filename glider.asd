(defsystem #:glider
  :class :package-inferred-system
  :description "Algae Glider; a bullet-hell shooter"
  :version "0.1"
  :author "TANAKA Shinichi"
  :license "?"
  :depends-on ("sdl2"
               "sdl2-image")
  :components ((:module "src"
                :components ((:file "const")
                             (:file "util")
                             (:module "shooter"
                              :components ((:file "actors")
                                           (:file "vm")
                                           (:file "combinators"))
                              :serial t)
                             (:module "scenes"
                              :components ((:file "shooter")))
                             (:file "main"))
                :serial t)))
