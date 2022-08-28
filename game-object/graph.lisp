(in-package :graph)
(require :alexandria)


(defgeneric get-neighbours (obj item)
  (:documentation "Get neighbors for the item"))

(defgeneric bfs (obj start-node &key end-node) (:documentation "Breath First Search"))
(defgeneric dfs (obj start-node &key end-node)
  (:documentation "Depth First Search"))


(defclass grid-graph-class ()
  ((grid-width :initarg :grid-width)
   (grid-height :initarg :grid-height)
   (visited-mask :initarg :visited-mask)
   (neighbours-pattern :initarg :neighbours-pattern)))

(defclass priority-queue-class ()
  ((items :initarg :items :initform nil)))



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


(defun make-grid-graph (grid-width grid-height &key (neighbours-pattern cross-type))
  (let ((pattern (cdr (assoc neighbours-pattern pattern-alist))))
    (make-instance 'grid-graph-class
                   :grid-width grid-width
                   :grid-height grid-height
                   :visited-mask (make-array (list grid-height grid-width) :initial-element nil)
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
  (with-slots (grid-width grid-height visited-mask) obj
    (let ((visited-mask-copy (alexandria:copy-array visited-mask)))
      (setf-2d visited-mask-copy start-node T)
      (labels
          ((start (nodes)
             (cond
               ((endp nodes) nil)
               (T
                (let* ((curr-node (car nodes))
                       (unvisited-neighbours
                         (remove-if #'(lambda (coord) (aref-2d visited-mask-copy coord))
                                    (get-neighbours obj curr-node))))
                  (loop for (y x) in unvisited-neighbours
                        do (setf-2d visited-mask-copy (list y x) T))
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
  (append unvisited-neighbours nodes))


(defmethod bfs ((obj grid-graph-class) start-node &key end-node)
  (traverse obj start-node :append-nodes #'bfs-append :end-node end-node))


(defmethod dfs ((obj grid-graph-class) start-node &key end-node)
  (traverse obj start-node :append-nodes #'dfs-append :end-node end-node))


(defun traverse-path (obj start-node &key (end-node nil end-node-p) append-nodes)

  (with-slots (grid-width grid-height visited-mask) obj
    (let ((visited-mask-copy (alexandria:copy-array visited-mask)))
      (setf-2d visited-mask-copy start-node T)
      (labels
          ((traverse (nodes &optional (path nil))
             (cond
               ((endp nodes) nil)
               (T
                (let* ((curr-node (car nodes))
                       (unvisited-neighbours
                         (remove-if #'(lambda (coord) (aref-2d visited-mask-copy coord))
                                    (get-neighbours obj curr-node))))
                  (loop for (y x) in unvisited-neighbours
                        do (setf-2d visited-mask-copy (list y x) T))
                  (if (and end-node-p (equal curr-node end-node))
                      path
                      (traverse (funcall append-nodes :nodes (cdr nodes)
                                                      :unvisited-neighbours unvisited-neighbours)
                                (append path (loop for coord in unvisited-neighbours
                                                   collect (cons coord (list curr-node)))))))))))


        (extract-path end-node (traverse (list start-node)) start-node)))))

(defun extract-path (curr-node discoveries start-node)
  (if
   (equal start-node curr-node)
   (list curr-node)
   (cons curr-node
         (extract-path (cadr (assoc curr-node discoveries :test #'equal))
                       discoveries
                       start-node))))


(defmethod add-to-queue ((obj priority-queue-class) item &key cost)
  (with-slots (items) obj
    (labels
        ((add-to-list-ordered (items)
             (cond
               ((null items) (list (cons item cost)))
               ((>= cost (cdar items)) (cons (car items)
                                            (add-to-list-ordered (cdr items))))
               (T (cons (cons item cost) items)))))
      (setf items (add-to-list-ordered items)))))


(defmethod update-queue ((obj priority-queue-class) item &key cost)
  (with-slots (items) obj
    (setf (cdr (assoc item items)) cost)
    (setf items (sort items #'(lambda (x y) (< (cdr x) (cdr y)))))))



(defmethod get-from-queue  ((obj priority-queue-class) item &key (test #'equal))
  (with-slots (items) obj
    (assoc item items :test test)))

(defmethod pop-from-queue ((obj priority-queue-class))
  (with-slots (items) obj
    (let ((new-items (cdr items))
          (pop-item (car items)))
      (setf items new-items)
      pop-item)))



(defmethod is-in-queue ((obj priority-queue-class) item &key (test #'equal))
  (if (get-from-queue obj item :test test) T nil))


(defun uniform-cost-search (obj start-node &key end-node)
  (with-slots (grid-width grid-height visited-mask) obj
    (let ((visited-mask-copy (alexandria:copy-array visited-mask))
          (path-map nil)
          (priority-queue (make-instance 'priority-queue-class)))

      (add-to-queue priority-queue start-node :cost 0)
      (loop
        (destructuring-bind (curr-node . cost) (pop-from-queue priority-queue)
          (when (equal curr-node end-node)
            (let ((path (extract-path curr-node path-map start-node)))
              (loop for coord in path do (setf-2d visited-mask coord T))
              (return path)))
          (setf-2d visited-mask-copy curr-node T)
          (loop for node in (get-neighbours obj curr-node)
                do (let ((node-cost (get-from-queue priority-queue node))
                         (new-cost (+ cost 1)))
                     (cond
                       ((null node-cost)
                        (when (not (aref-2d visited-mask-copy node))
                          (add-to-queue priority-queue node :cost new-cost)
                          (setq path-map (cons (cons node (list curr-node)) path-map))))
                       ((>= (cdr node-cost) new-cost)
                        (update-queue priority-queue (car node-cost) :cost new-cost)
                        (setf (cdr (assoc node path-map :test #'equal)) (list curr-node)))))))))))


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
