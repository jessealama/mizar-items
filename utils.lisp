;;; utils.lisp

(in-package :mizar)

(defun concat (&rest strings)
  (apply 'concatenate 'string strings))

(defun first-n (lst n)
  (loop 
     for i from 1 upto n
     for elt in lst
     collecting elt into items
     finally (return items)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro until (condition &body body)
  `(do nil (,condition) ,@body))

(defmacro while (condition &body body)
  `(do nil ((not ,condition)) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun values-for-keys-less-than (table key &key (predicate #'<))
  (let (result)
    (maphash #'(lambda (k v)
		 (when (funcall predicate k key)
		   (push v result)))
	     table)
    result))

;;; utils.lisp ends here