(defpackage #:sdl2-tutorial-08-geometry-rendering
  (:use :cl)
  (:export :run))

(in-package :sdl2-tutorial-08-geometry-rendering)


(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window

                        :title "SDL2 Tutorial 08"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))


(defun draw-triangle (renderer x1 y1 x2 y2 x3 y3)
  (sdl2:render-draw-line renderer x1 y1 x2 y2)
  (sdl2:render-draw-line renderer x2 y2 x3 y3)
  (sdl2:render-draw-line renderer x3 y3 x1 y1))



(defun serpinski (p1 p2 p3 &optional (depth 3))
  (cond
    ((= depth 0) nil)
    (T
     (destructuring-bind
         ((x1 y1) (x2 y2) (x3 y3))
         (list  p1 p2 p3)
       (let* ((x12m (floor (+ x1 x2) 2))
              (x13m (floor (+ x1 x3) 2))
              (x23m (floor (+ x2 x3) 2))
              (y12m (floor (+ y1 y2) 2))
              (y13m (floor (+ y1 y3) 2))
              (y23m (floor (+ y2 y3) 2))
              (a1-triangles (serpinski `(,x1 ,y1) `(,x12m ,y12m) `(,x13m ,y13m) (1- depth)))
              (a2-triangles (serpinski `(,x12m ,y12m) `(,x2 ,y2) `(,x23m ,y23m) (1- depth)))
              (a3-triangles (serpinski `(,x13m ,y13m) `(,x23m ,y23m) `(,x3 ,y3) (1- depth))))

         (append `((,x12m ,y12m) (,x23m ,y23m) (,x13m ,y13m))
                 a1-triangles
                 a2-triangles
                 a3-triangles))))))

(defun draw-triangles (renderer triangles)
  (cond
    ((null triangles))
    (T (destructuring-bind
           ((x1 y1) (x2 y2) (x3 y3) &rest other-triangles)
           triangles
         (draw-triangle renderer x1 y1 x2 y2 x3 y3)
         (draw-triangles renderer other-triangles)))))


(defun run ()
  (let ((serpinski-triangles (serpinski `(,(/ *screen-width* 2) 0)
                                        `(0 ,(1- *screen-height*))
                                        `(,(1- *screen-width*) ,(1- *screen-height*))
                                        10)))

    (with-window-renderer (window renderer)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               ;; Clear screen
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)


               (sdl2:set-render-draw-color renderer #x00 #xFF #x00 #xFF)
                                        ; (draw-triangles renderer)
               (draw-triangles renderer serpinski-triangles)
               (draw-triangles renderer
                               `((,(/ *screen-width* 2) 0)
                                 (0 ,(1- *screen-height*))
                                 (,(1- *screen-width*) ,(1- *screen-height*))))



               ;; Update screen

               (sdl2:render-present renderer))))))

(run)
