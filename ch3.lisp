(defvar *db* nil)


(defun add-record (cd) (push cd *db*))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))


(defun dump-db-once ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))


(defun show-prompt (prompt)
  (format *query-io* "~a: " prompt)
  (read-line *query-io*))


(defun prompt-for-cd ()
  (make-cd
   (show-prompt "Title")
   (show-prompt "Artist")
   (or (parse-integer (show-prompt "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))


(defun add-cds ()
  (loop
    (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another one [y/n]: ")) (return))))


(defun save-db (filename)
  (with-open-file (out filename
		   :direction :output
		   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))


(defun read-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))


(defun select (selector-fn)
  (remove-if-not selector-fn *db*))


(defun artist-selector (artist)
  "This is ARTIST selector."
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun dumb-where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))


(defun dumb-update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row)
	 *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defun make-comp-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comp-list (fields)
  (loop while fields
	collecting (make-comp-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comp-list clauses))))

(defmacro backwards (expr) (reverse expr))

(defun select-artist (artist)
  (select (artist-selector artist)))


; (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
; (add-record (make-cd "Fly" "Dixie Chicks" 8 t))
; (add-record (make-cd "Home" "Dixie Chicks" 9 t))

(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))


(read-db "/home/hsc/Documents/Projects/Learn/Lisp/db.txt")

(select (where :artist "Dixie Chicks"))
