(in-package :color)


(defparameter *black* '(#x00 #x00 #x00 #xFF))
(defparameter *white* '(#xFF #xFF #xFF #xFF))
(defparameter *red* '(#xFF #x00 #x00 #xFF))
(defparameter *green* '(#x00 #xFF #x00 #xFF))
(defparameter *blue* '(#x00 #x00 #xFF #xFF))

(defun make-random (&optional (alpha #xFF))
  (list (random 256) (random 256) (random 256) alpha))
