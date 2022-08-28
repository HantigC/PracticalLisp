(in-package :game-object)
(require :trivia)

(defclass grid-graph-go ()
  ((rects :initarg :rects :accessor rects :initform nil)
   (colors :initarg :colors :accessor colors :initform nil)
   (x-stride :initarg :x-stride :accessor x-stride :initform 10)
   (y-stride :initarg :y-stride :accessor y-stride :initform 10)
   (grid-width :initarg :grid-width)
   (grid-height :initarg :grid-height)
   (colors-lifetime :initarg :colors-lifetime :accessor colors-lifetime :initform nil)
   (grid-graph-obj :initarg :grid-graph-obj :accessor grid-graph-obj :initform nil)
   (processed-input :initarg :processed-input :accessor processed-input :initform nil)
   (hover-coords :initarg :hover-coords :accessor hover-coords :initform nil)
   (mouse-coords :initarg :mouse-coords :accessor mouse-coords :initform nil)))


(defun make-grid-graph-go ()
  (make-instance 'grid-graph-go))


(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))


(defmethod init ((obj grid-graph-go) game-plist)
  (with-slots
        (x-stride y-stride colors rects grid-graph-obj colors-lifetime grid-width grid-height)
      obj
    (let ((grid-width-c (floor (getf game-plist :screen-width) x-stride))
          (grid-height-c  (floor (getf game-plist :screen-height) y-stride)))
      (setf colors (make-array (list grid-height-c grid-width-c) :initial-element color:*white*)
            colors-lifetime (make-array (list grid-height-c grid-width-c) :initial-element nil)

            rects (list-to-2d-array (make-rects (getf game-plist :screen-width)
                                                (getf game-plist :screen-height)
                                                :x-length x-stride
                                                :y-length y-stride))
            grid-width grid-width-c
            grid-height grid-height-c
            grid-graph-obj (graph:make-grid-graph grid-width-c grid-height-c)))))

(defmethod init-from-grid-bounds ((obj grid-graph-go) grid-height-in grid-width-in)
  (with-slots (x-stride y-stride colors rects grid-graph-obj colors-lifetime grid-width grid-height
               hover-coords mouse-coords processed-input) obj
      (setf colors (make-array (list grid-height-in grid-width-in) :initial-element color:*white*)
            colors-lifetime (make-array (list grid-height-in grid-width-in) :initial-element nil)
            rects (list-to-2d-array (make-rects (* x-stride grid-width-in)
                                                (* y-stride grid-height-in)
                                                :x-length x-stride
                                                :y-length y-stride))
            grid-width grid-width-in
            grid-height grid-height-in
            hover-coords nil
            mouse-coords nil
            processed-input nil

            grid-graph-obj (graph:make-grid-graph grid-width-in grid-height-in))))


(defmethod process-input ((obj grid-graph-go) input)
  (with-slots (mouse-coords hover-coords x-stride y-stride grid-graph-obj grid-width grid-height) obj
    (trivia:match input
      ((list :mouse-down y x) (push (list (floor y y-stride) (floor x x-stride)) mouse-coords))
      ((list :mouse-hover y x) (setf hover-coords (list (floor y y-stride) (floor x x-stride))))
      ((list :key-down "b") (init-from-grid-bounds obj grid-height grid-width)))))


(defmethod draw ((obj grid-graph-go) renderer)
  (with-slots (rects colors colors-lifetime) obj
    (destructuring-bind (h w) (array-dimensions rects)
      (loop for y from 0 below h do
        (loop for x from 0 below w do
          (progn
            (render:color-rect renderer (aref rects y x) (aref colors y x))
            (render:draw-rect renderer (aref rects y x) color:*black*)
            (when (aref colors-lifetime y x)
              (render:color-rect renderer (aref rects y x) color:*green*))))))))



(defmethod update ((obj grid-graph-go))
  (with-slots (colors grid-graph-obj mouse-coords hover-coords processed-input colors-lifetime) obj
    (destructuring-bind (h w) (array-dimensions colors-lifetime)
      (setf colors-lifetime (make-array (list h w) :initial-element nil)))
    (when (= (length  hover-coords) 2) (destructuring-bind (y x) hover-coords
                                         (setf (aref colors-lifetime y x) t)))

    (cond
      ((= (length mouse-coords) 1) (destructuring-bind (y x) (car mouse-coords)
                                     (setf (aref colors y x) color:*red*)))
      ((= (length mouse-coords) 2) (let ((new-color (color:make-random))
                                         (bfs-traverse (graph:uniform-cost-search grid-graph-obj
                                                                  (cadr mouse-coords)
                                                                  :end-node (car mouse-coords))))
                                     (loop for (y x) in bfs-traverse
                                           do (setf (aref colors y x) new-color))
                                     (setf processed-input (append processed-input (list mouse-coords)))
                                     (loop for ((ye xe) (ys xs)) in processed-input
                                           do (setf (aref colors ys xs) color:*red*
                                                    (aref colors ye xe) color:*green*)))
       (setf mouse-coords nil)))))
