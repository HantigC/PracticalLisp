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

(defgeneric init (obj game-plist)
  (:documentation "init go"))

(defgeneric process-input (obj input)
  (:documentation "porcess input game-onbect"))

(defmethod process-input (obj input))

(defgeneric draw (obj renderer)
  (:documentation "Draw game object"))

(defgeneric update (obj)
  (:documentation "update game object"))
