(in-package :game-object)
(require :trivia)

(defclass grid-graph-go ()
  ((rects :initarg :rects :accessor rects :initform nil)
   (colors :initarg :colors :accessor colors :initform nil)
   (x-stride :initarg :x-stride :accessor x-stride :initform 10)
   (y-stride :initarg :y-stride :accessor y-stride :initform 10)
   (grid-graph-obj :initarg :grid-graph-obj :accessor grid-graph-obj :initform nil)
   (processed-input :initarg :processed-input :accessor processed-input :initform nil)
   (mouse-coords :initarg :mouse-coords :accessor mouse-coords :initform nil)))


(defun make-grid-graph-go ()
  (make-instance 'grid-graph-go))


(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))


(defmethod init ((obj grid-graph-go) game-plist)
  (with-slots (x-stride y-stride colors rects grid-graph-obj) obj
    (let ((grid-width (floor (getf game-plist :screen-width) x-stride))
          (grid-height  (floor (getf game-plist :screen-height) y-stride)))
      (setf colors (make-array (list grid-height grid-width) :initial-element color:*white*)
            rects (list-to-2d-array (make-rects (getf game-plist :screen-width)
                                                (getf game-plist :screen-height)
                                                :x-length x-stride
                                                :y-length y-stride))
            grid-graph-obj (graph:make-grid-graph grid-width grid-height )))))


(defmethod process-input ((obj grid-graph-go) input)
  (with-slots (mouse-coords x-stride y-stride) obj
    (trivia:match input
      ((list :mouse-down y x) (push (list (floor y y-stride) (floor x x-stride)) mouse-coords))
      ((list :mouse-hover y x) (push (list (floor y y-stride) (floor x x-stride)) mouse-coords)))))


(defmethod draw ((obj grid-graph-go) renderer)
  (with-slots (rects colors) obj
    (destructuring-bind (h w) (array-dimensions rects)
      (loop for y from 0 below h do
        (loop for x from 0 below w do
          (progn
            (render:color-rect renderer (aref rects y x) (aref colors y x))
            (render:draw-rect renderer (aref rects y x) color:*black*)))))))

(defmethod update ((obj grid-graph-go))
  (with-slots (colors grid-graph-obj mouse-coords processed-input) obj
    (cond
      ((= (length mouse-coords) 1) (destructuring-bind (y x) (car mouse-coords)
                                     (setf (aref colors y x) color:*red*)))
      ((= (length mouse-coords) 2) (let ((new-color (color:make-random))
                                         (bfs-traverse (graph:bfs grid-graph-obj
                                                                  (cadr mouse-coords)
                                                                  :end-node (car mouse-coords))))
                                     (loop for (y x) in bfs-traverse
                                           do (setf (aref colors y x) new-color))
                                     (setf processed-input (append processed-input (list mouse-coords)))
                                     (loop for ((ye xe) (ys xs)) in processed-input
                                           do (setf (aref colors ys xs) color:*red*
                                                    (aref colors ye xe) color:*green*)))
       (setf mouse-coords nil)))))
