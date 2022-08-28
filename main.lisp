(in-package :main)
(require :sdl2)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defun arange (n &key (start 0) (step 1))
    (loop for i from start to n by step collect i))



(defmacro with-game
    ((game-sym renderer &key screen-width screen-height stride-x-size stride-y-size)
     &body body)
  `(let ((,game-sym (game:make-game ,renderer
                                    :screen-width ,screen-width
                                    :screen-height ,screen-height
                                    :stride-y-size ,stride-y-size
                                    :stride-x-size ,stride-x-size)))
     (progn (game:game-init ,game-sym)
            ,@body)))


(defun run ()
  (sdl2x:with-window-renderer (window renderer "This" *screen-width* *screen-height*
                               :x 10000 :y 0 :flags '(:shown :always-on-top))
    (with-game (game-obj (render:make-sdl2-game-renderer renderer)
                :screen-width *screen-width* :screen-height *screen-height*
                :stride-y-size 10 :stride-x-size 10)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)

        (:keydown
         (:keysym keysym)
         (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-b)
           (game:process-game-input game-obj (list :key-down "b"))))
        (:mousebuttondown (:x x :y y)
                          (game:process-game-input game-obj (list :mouse-down y x))
                          (format t "Mouse click way at ~a ~a ~%" x y))
        (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                      (game:process-game-input game-obj (list :mouse-hover y x))
                      (format t "Mouse hover way at ~a ~a ~a ~a ~a ~%" x y xrel yrel state))
        (:idle ()
               (game:update-game game-obj)
               (game:render-game game-obj)
               (slyx:update-slynk)
               (sdl2:render-present renderer))))))
