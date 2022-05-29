;;;; package.lisp
(defpackage #:sdl2x
  (:use :cl :sdl2)
  (:export :with-window-renderer))


(defpackage #:sdl2-graphs
  (:use :cl :sdl2x :sdl2)
  (:export :run))


(defpackage #:sdl2-tutorial-serpinki
  (:use :cl :sdl2 :sdl2x)
  (:export :run))
