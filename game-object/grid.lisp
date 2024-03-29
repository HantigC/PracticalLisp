(in-package :game-object)


(defclass grid-go (game-object-class)
  ((color
    :initarg :color
    :accessor color
    :type list)))


(defun make-grid (width height stride-x stride-y
                  &key (start-x 0) (start-y 0) (space-x 0) (space-y 0))
  (loop for y from start-y below height by stride-y
        collect (loop for x from start-x below width by stride-x
                      collect (list y x (- stride-y space-y) (- stride-x space-x)))))


(defun make-rects (width height
                   &key (x-length nil x-length-p) (y-length nil y-length-p)
                     (x-slices nil x-slices-p) (y-slices nil y-slices-p))

  (let ((stride-x (cond
                    (x-length-p x-length)
                    (x-slices-p (floor width x-slices))
                    (t (error "specify x-length or x-slices"))))
        (stride-y (cond
                    (y-length-p y-length)
                    (y-slices-p (floor height y-slices))
                    (t (error "specify x-length or x-slices")))))
    (loop for rects in (make-grid width height stride-x stride-y)
          collect (loop for (y x h w) in rects
                        collect (sdl2:make-rect x y w h)))))


(defun make-grid-go (width height x-stride y-stride &key (color color:*black*)
                                                      (name "grid"))
  (make-instance 'grid-go
                 :rects (make-rects width height :x-length x-stride :y-length y-stride)
                 :name name
                 :color color))


(defmethod draw ((obj grid-go) renderer)
  (with-slots (rects color) obj
    (render:draw-rects renderer rects color)))
