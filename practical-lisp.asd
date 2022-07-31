;;;; practical-lisp.asd

(asdf:defsystem #:practical-lisp
  :description "Describe practical-lisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:sdl2 :trivia)
  :components ((:file "package")
               (:file "utils/slyx")
               (:file "color")
               (:file "render")
               (:file "game-object/game-object")
               (:file "game-object/graph")
               (:file "game-object/grid-graph" :depends-on ("game-object/game-object"  "game-object/graph"))
               (:file "game-object/grid" :depends-on ("game-object/game-object"))
               (:file "game" :depends-on ("game-object/game-object" "game-object/grid" "game-object/grid-graph" ))
               (:file "utils/sdl2x")
               (:file "serpinski" :depends-on ("utils/sdl2x"))
               (:file "main" :depends-on ("color" "render" "game" "game-object/game-object" "utils/sdl2x" "utils/slyx"))))
