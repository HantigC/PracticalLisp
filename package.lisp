;;;; package.lisp



(defpackage #:sdl2x
  (:use :cl :sdl2)
  (:export :with-window-renderer))


(defpackage #:render
  (:use :cl :sdl2)
  (:export
   :draw-rects
   :draw-rect
   :sdl2-game-renderer
   :clear
   :color-rect
   :set-color
   :make-sdl2-game-renderer))


(defpackage #:slyx
  (:use :cl :slynk)
  (:export :update-slynk))


(defpackage #:game
  (:use :cl)
  (:export
   :game-class
   :make-game
   :process-game-input
   :game-init
   :update-game
   :render-game))


(defpackage #:graph
  (:use :cl)
  (:export
   :grid-graph-class
   :squere-type
   :bfs
   :dfs
   :bfs-path
   :uniform-cost-search
   :dfs-path
   :cross-type
   :make-grid-graph
   :get-neighbours))


(defpackage #:game-object
  (:use :cl :trivia)
  (:export
   :game-object-class
   :init
   :process-input
   :update
   :grid-go
   :make-grid-go
   :draw
   :make-grid-graph-go))


(defpackage #:color
  (:use :cl)
  (:export
   :*black*
   :*white*
   :*red*
   :*green*
   :*blue*
   :make-random))

(defpackage #:main
  (:use :cl :sdl2x :sdl2)
  (:export :run))


(defpackage #:sdl2-tutorial-serpinki
  (:use :cl :sdl2 :sdl2x)
  (:export :run))
