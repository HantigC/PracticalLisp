(in-package :game-object)


(defclass game-object-class ()
  ((rects
    :initarg :rects
    :accessor rects
    :initform nil
    :type list)
   (name
    :initarg :name
    :accessor name
    :initform nil)))


(defgeneric draw (obj renderer)
  (:documentation "Draw game object"))
