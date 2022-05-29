;;;; practical-lisp.asd

(asdf:defsystem #:practical-lisp
  :description "Describe practical-lisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:sdl2)
  :components ((:file "package")
               (:file "utils/sdl2x")
               (:file "serpinski" :depends-on ("utils/sdl2x"))
               (:file "sdl2-graphs")))
