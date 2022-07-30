(in-package :slyx)

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
