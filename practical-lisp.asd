;;;; practical-lisp.asd

(asdf:defsystem #:practical-lisp
  :description "Describe practical-lisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:sdl2)
  :components ((:file "package")
               (:file "color")
               (:file "render")
               (:file "game-object/game-object")
               (:file "game-object/grid" :depends-on ("game-object/game-object"))
               (:file "game" :depends-on ("game-object/game-object" "game-object/grid"))
               (:file "utils/sdl2x")
               (:file "serpinski" :depends-on ("utils/sdl2x"))
               (:file "sdl2-graphs" :depends-on ("color" "render" "game" "game-object/game-object" "utils/sdl2x"))))
