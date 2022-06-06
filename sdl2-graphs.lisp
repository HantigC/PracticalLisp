(in-package :sdl2-graphs)
(require :sdl2)

(defparameter *screen-width* 1024)
(defparameter *screen-height* 720)

(defun arange (n &key (start 0) (step 1))
    (loop for i from start to n by step collect i))


(defun make-grid (width height stride-x stride-y
                  &key (start-x 0) (start-y 0) (space-x 0) (space-y 0))
  (loop for x from start-x to width by stride-x
        append (loop for y from start-y to height by stride-y
                     collect `(,x ,y ,(- stride-x space-x) ,(- stride-y space-y)))))

(defun make-rects (x-slices y-slices)
  (let ((width *screen-width*)
        (height *screen-height*)
        (stride-x (floor *screen-width* x-slices))
        (stride-y (floor *screen-height* y-slices)))
    (loop for rect in (make-grid width height stride-x stride-y)
          collect (destructuring-bind (x y w h) rect
                    (sdl2:make-rect x y w h)))))

(defun run ()
  (let ((rects (make-rects 64 48)))
    (sdl2x:with-window-renderer (window renderer "This" *screen-width* *screen-height*)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               ;; Clear screen
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               (sdl2:set-render-draw-color renderer #x00 #xFF #x00 #xFF)
               (loop for rect in rects do (sdl2:render-draw-rect renderer rect))

               (sdl2:render-present renderer))))))
