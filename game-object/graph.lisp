(in-package :graph)


(defgeneric get-neighbours (obj item)
  (:documentation "Get neighbors for the item"))

(defgeneric bfs (obj start-node &key end-node)
  (:documentation "Breath First Search"))

(defgeneric dfs (obj start-node &key end-node)
  (:documentation "Depth First Search"))


(defclass grid-graph-class ()
  ((grid-width :initarg :grid-width)
   (grid-height :initarg :grid-height)
   (neighbours-pattern :initarg :neighbours-pattern)))




(defparameter *squere-neighbours-pattern*
  '((-1 -1) (-1 0) (-1 1)
    (0  -1) (0  1)
    (1  -1) (1  0) (1  1)))


(defparameter *cross-neighbours-pattern*
  '((-1 0) (0 1) (1 0) (0 -1)))

(defparameter squere-type "squere")
(defparameter cross-type "cross")

(defparameter pattern-alist
  (list (cons squere-type  *squere-neighbours-pattern*)
        (cons cross-type  *cross-neighbours-pattern*)))


(defun make-grid-graph (grid-width grid-height &key (neighbours-pattern squere-type))
  (let ((pattern (cdr (assoc neighbours-pattern pattern-alist))))
    (make-instance 'grid-graph-class
                   :grid-width grid-width
                   :grid-height grid-height
                   :neighbours-pattern pattern)))

(defun multiple-2daref (array-obj index-list)
  (mapcar #'(lambda (idx) (aref-2d array-obj idx))
          index-list))

(defun aref-2d (array-obj coord)
  (destructuring-bind (y x) coord
    (aref array-obj y x)))

(defmacro setf-2d (array-obj coord val)
  (let ((y-sym (gensym))
        (x-sym (gensym)))
    `(destructuring-bind (,y-sym ,x-sym) ,coord
       (setf (aref ,array-obj ,y-sym ,x-sym) ,val))))


(defun traverse (obj start-node &key (end-node nil end-node-p) append-nodes)
  (with-slots (grid-width grid-height) obj
    (let ((visited-mask (make-array (list grid-height grid-width) :initial-element nil)))
      (setf-2d visited-mask start-node T)
      (labels
          ((start (nodes)
             (cond
               ((endp nodes) nil)
               (T
                (let* ((curr-node (car nodes))
                       (unvisited-neighbours
                         (remove-if #'(lambda (coord) (aref-2d visited-mask coord))
                                    (get-neighbours obj curr-node))))
                  (loop for (y x) in unvisited-neighbours
                        do (setf-2d visited-mask (list y x) T))
                  (if (and end-node-p (some #'(lambda (coord) (equal coord end-node))
                                            unvisited-neighbours))
                      nil
                      (append unvisited-neighbours
                              (start (funcall append-nodes :nodes (cdr nodes)
                                                           :unvisited-neighbours unvisited-neighbours)))))))))

        (cons start-node (start (list start-node)))))))


(defun bfs-append (&key nodes unvisited-neighbours)
  (append nodes unvisited-neighbours))


(defun dfs-append (&key nodes unvisited-neighbours)
  (append unvisited-neighbours nodes ))


(defmethod bfs ((obj grid-graph-class) start-node &key end-node)
  (traverse obj start-node :append-nodes #'bfs-append :end-node end-node))


(defmethod dfs ((obj grid-graph-class) start-node &key end-node)
  (traverse obj start-node :append-nodes #'dfs-append :end-node end-node))


(defun traverse-path (obj start-node &key (end-node nil end-node-p) append-nodes)

  (with-slots (grid-width grid-height) obj
    (let ((visited-mask (make-array (list grid-height grid-width) :initial-element nil)))
      (setf-2d visited-mask start-node T)
      (labels
          ((traverse (nodes &optional (path nil))
             (cond
               ((endp nodes) nil)
               (T
                (let* ((curr-node (car nodes))
                       (unvisited-neighbours
                         (remove-if #'(lambda (coord) (aref-2d visited-mask coord))
                                    (get-neighbours obj curr-node))))
                  (loop for (y x) in unvisited-neighbours
                        do (setf-2d visited-mask (list y x) T))
                  (if (and end-node-p (equal curr-node end-node))
                      path
                      (traverse (funcall append-nodes :nodes (cdr nodes)
                                                      :unvisited-neighbours unvisited-neighbours)
                                (append path (loop for coord in unvisited-neighbours
                                                   collect (cons coord (list curr-node))))))))))
           (extract-path (curr-node discoveries)
             (if
              (equal start-node curr-node)
              (list curr-node)
              (cons curr-node (extract-path (cadr (assoc curr-node discoveries :test #'equal))
                                            discoveries)))))


        (extract-path end-node (traverse (list start-node)))))))

(defmethod bfs-path ((obj grid-graph-class) start-node &key end-node)
  (traverse-path obj start-node :end-node end-node :append-nodes #'bfs-append))


(defmethod dfs-path ((obj grid-graph-class) start-node &key end-node)
  (traverse-path obj start-node :end-node end-node :append-nodes #'dfs-append))


(defmethod get-neighbours ((obj grid-graph-class) item)
  (destructuring-bind (y x) item
    (with-slots (neighbours-pattern grid-width grid-height) obj
      (loop for (y-o x-o) in neighbours-pattern
            for x-n = (+ x x-o)
            for y-n = (+ y y-o)
            when (and (< -1 x-n grid-width) (< -1 y-n grid-height))
              collect (list y-n x-n)))))
