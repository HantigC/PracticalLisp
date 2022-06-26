(in-package :sdl2x)

(require :sdl2)


(defmacro with-window-renderer ((window renderer title  screen-width screen-height
                                 &key (x :centered) (y :centered)(flags '(:show)))
                                &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :x ,x
                        :y ,y
                        :title ,title
                        :w ,screen-width
                        :h ,screen-height
                        :flags ,flags)
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))
