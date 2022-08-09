(in-package :game)

(defclass game-class ()
  ((renderer :initarg :renderer :initform nil)
   (game-objects :initarg game-objects :initform nil :type list)
   (inputs :initarg :inputs :initform nil :type list)

   (stride-y-size :initarg :stride-y-size :type int)
   (stride-x-size :initarg :stride-x-size :type int)

   (screen-width :initarg :screen-width :type int)
   (screen-height :initarg :screen-height :type int)))


(defun make-game (renderer
                  &key screen-width
                    screen-height
                    (stride-x-size 10)
                    (stride-y-size 10))

  (make-instance 'game-class
                 :renderer renderer
                 :inputs nil
                 :screen-width screen-width
                 :stride-x-size stride-x-size
                 :stride-y-size stride-y-size
                 :screen-height screen-height))


(defgeneric game-init (obj)
  (:documentation "Init your game"))


(defgeneric process-game-input (obj input)
  (:documentation "process input"))


(defgeneric update-game (obj)
  (:documentation "update game"))


(defgeneric render-game (obj)
  (:documentation "Render your game"))


(defmethod game-init ((obj game-class))
  (with-slots (game-objects screen-width screen-height stride-x-size stride-y-size) obj
    (let ((grid-graph-obj (game-object:make-grid-graph-go)))
      (game-object:init grid-graph-obj (list :screen-width screen-width
                                             :screen-height screen-height))
      (setf game-objects (list grid-graph-obj)))))


(defmethod process-game-input ((obj game-class) input)
  (with-slots (inputs game-objects) obj
    (loop for game-obj in game-objects do
      (game-object:process-input game-obj input))))


(defmethod update-game ((obj game-class))
  (print "update game")
  (with-slots (game-objects) obj
    (loop for game-obj in game-objects do
      (game-object:update game-obj))))



(defmethod render-game ((obj game-class))
  (print "render game")
  (with-slots (renderer game-objects) obj
    (render:clear renderer :c color:*red*)
    (loop for game-obj in game-objects
          do (game-object:draw game-obj renderer))))
