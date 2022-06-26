(in-package :sdl2-graphs)
(require :sdl2)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defun arange (n &key (start 0) (step 1))
    (loop for i from start to n by step collect i))


(defun make-grid (width height stride-x stride-y
                  &key (start-x 0) (start-y 0) (space-x 0) (space-y 0))
  (loop for x from start-x to width by stride-x
        append (loop for y from start-y to height by stride-y
                     collect `(,x ,y ,(- stride-x space-x) ,(- stride-y space-y)))))

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
    (loop for rect in (make-grid width height stride-x stride-y)
          collect (destructuring-bind (x y w h) rect
                    (sdl2:make-rect x y w h)))))

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun update-slynk ()
  "Called from within the main loop, this keep the lisp repl
   working while cepl runs"
  (continuable
    (let ((connection (or slynk::*emacs-connection*
                       (slynk::default-connection))))
        (when connection
          (slynk::handle-requests connection t)))))

(defun make-fun (renderer)
  (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF))


(defstruct point-2d x y)

(defparameter *neighbours-cross* '((-1 0) (1 0) (0 -1) (0 1)))
(defparameter *neighbours-squere* '((-1 -1) (0 -1) (1 -1)
                                    (-1 0) (1 0)
                                    (-1 1) (0 1) (1 1)))

(defun point-2d-eq (p1 p2)
  (cond
    ((null p1) nil)
    ((null p2) nil)
    ((and (= (point-2d-x p1) (point-2d-x p2))
          (= (point-2d-y p1) (point-2d-y p2))))))


(defun get-nbghs (x y
                  &key
                    (x-stride 1)
                    (y-stride 1)
                    (min-x 0)
                    (min-y 0)
                    max-x
                    max-y
                    (neighbours-pattern *neighbours-squere*)
                    (make-it #'list))
  (loop for (offset-x offset-y) in neighbours-pattern
        for x-n  = (+ x (* offset-x x-stride))
        for y-n  = (+ y (* offset-y y-stride))
        when (and (<= min-x x-n max-x) (<= min-y y-n max-y))
          collect (funcall make-it x-n y-n)))


(defun make-assoc-graph (grid-width grid-height x-stride y-stride)
  (loop for x from 0 to grid-width by x-stride
        append (loop for y from 0 to grid-height by y-stride
                     collect (list
                              (make-point-2d :x x :y y)
                              (get-nbghs x y :max-x (1- grid-width)
                                             :max-y (1- grid-height)
                                             :x-stride x-stride
                                             :y-stride y-stride
                                             :make-it #'(lambda (x y) (make-point-2d :x x :y y)))))))


(defun bfs (graph start &key (test #'point-2d-eq) (end nil end-p))
  (do* ((tobe-visited (list start) (append (cdr tobe-visited) neighs))
        (next-node (car tobe-visited) (car tobe-visited))
        (neighs (cadr (assoc next-node graph :test test)))
        (finished (list next-node))
        (visited (list start)))


       ((or (null next-node) (when end-p (find end neighs :test #'point-2d-eq)))  visited)

      (setq neighs (remove-if-not #'(lambda (x) (null (find x visited :test #'point-2d-eq)))
                                  (cadr (assoc next-node *graph* :test #'point-2d-eq)))
            finished (append visited (list next-node))
            visited (append visited neighs))))



(defun select-rect (x y x-length y-length)
  (let ((start-x (* (floor x x-length) x-length))
        (start-y (* (floor y y-length) y-length)))
    (sdl2:make-rect start-x start-y x-length y-length)))

(defun point-2d-to-rect (p-2d x-length y-length)
  (select-rect (point-2d-x p-2d)
               (point-2d-y p-2d)
               x-length
               y-length))

(defparameter *graph* (make-assoc-graph 640 480 10 10))

(defun select-point (x y x-length y-length)
  (let ((start-x (* (floor x x-length) x-length))
        (start-y (* (floor y y-length) y-length)))
    (make-point-2d :x start-x :y start-y)))
(defparameter p (select-point 54 55 10 10))

(defun point-2d-scale (p &optional (x 1) (y 1))
  (let* ((p-x (point-2d-x p))
         (p-y (point-2d-y p)))
    (make-point-2d :x (* p-x x) :y (* p-y y))))


(defun set-color (renderer rgba)
  (destructuring-bind (r g b a)
      rgba
      (sdl2:set-render-draw-color renderer r g b a)))


(defun points-2d-to-rects (points rect-width rect-height)
  (mapcar #'(lambda (p) (point-2d-to-rect p rect-width rect-height))
          points))

(defparameter *start-color* '(#x99 #x99 #x99 #xFF))
(defparameter *backgroud-color* '(#xFF #xFF #xFF #xFF))
(defparameter *grid-color* '(#x00 #x00 #x00 #xFF))


(defun make-random-color ()
  (list (random 256) (random 256) (random 256) #xFF))

(defun clear-renderer (renderer)
  (set-color renderer *backgroud-color*)
  (sdl2:render-clear renderer))

(defun run ()
  (let* ((x-length (floor *screen-width* 64))
         (y-length (floor *screen-height* 48))
         (grid-locations nil)
         (graph (make-assoc-graph 640 480 x-length y-length))
         (visited-rects nil)
         (rects (make-rects *screen-width* *screen-height*
                            :x-length x-length :y-length y-length))
         (a-down-p nil))
    (sdl2x:with-window-renderer
        (window renderer "This" *screen-width* *screen-height* :x 10000 :y 0 :flags '(:shown :always-on-top))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown
         (:keysym keysym)
         (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
           (setq a-down-p t)))

        (:keyup
         (:keysym keysym)
         (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
           (sdl2:push-event :quit))
         (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
           (setq a-down-p nil)))

        (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                      (when a-down-p
                        (format t "Mouse clicked way at ~a ~a ~a ~a ~a ~%" x y xrel yrel state)))
        (:mousebuttondown (:x x :y y :clicks clicks)
                          (progn
                            (push (select-point x y x-length y-length) grid-locations)
                            (when (> (length grid-locations) 1)
                              (push
                               (list (make-random-color)
                                     (points-2d-to-rects (bfs graph (second grid-locations)
                                                              :end (first grid-locations))
                                                         x-length
                                                         y-length))

                               visited-rects)

                              (format t "Mouse clicked way at ~a ~a ~a ~%" x y clicks))))
        (:idle ()
               ;; Clear screen
               (clear-renderer renderer)



               (loop for (color rects) in (reverse visited-rects)
                     do (progn
                          (set-color renderer color)
                          (loop for rect in rects
                                do (sdl2:render-fill-rect renderer rect))))


               (set-color renderer *start-color*)
               (loop for p in grid-locations
                     do (sdl2:render-fill-rect renderer (point-2d-to-rect p x-length y-length)))
               (set-color renderer *grid-color*)
               (loop for rect in rects do (sdl2:render-draw-rect renderer rect))

               (sdl2:render-present renderer)
               (update-slynk))))))
