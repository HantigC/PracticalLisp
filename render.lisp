(in-package :render)
(require :sdl2)


(defclass sdl2-game-renderer ()
  ((drawer :initarg :drawer
           :accessor drawer)))

(defun make-sdl2-game-renderer (sdl2-drawer)
  (make-instance 'sdl2-game-renderer :drawer sdl2-drawer))

(defgeneric clear (obj &key c)
  (:documentation "process input"))


(defgeneric draw-rects (obj rects color)
  (:documentation "process input"))


(defgeneric draw-rect (obj rect color)
  (:documentation "process input"))


(defun set-color (renderer rgba)
  (destructuring-bind (r g b a)
      rgba
      (sdl2:set-render-draw-color renderer r g b a)))


(defmethod clear ((obj sdl2-game-renderer) &key (c color:*white*))
  (with-slots (drawer) obj
    (set-color drawer c)
    (sdl2:render-clear drawer)))


(defmethod draw-rect ((obj sdl2-game-renderer) rect c)
  (with-slots (drawer) obj
    (set-color drawer c)
    (sdl2:render-draw-rect drawer rect)))


(defmethod draw-rects ((obj sdl2-game-renderer) rects c)
  (with-slots (drawer) obj

    (set-color drawer c)
    (loop for rect in rects do
      (sdl2:render-draw-rect drawer rect))))
