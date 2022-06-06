(in-package :sdl2x)

(require :sdl2)


(defmacro with-window-renderer ((window renderer title screen-width screen-height) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window

                        :title ,title
                        :w ,screen-width
                        :h ,screen-height
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))
