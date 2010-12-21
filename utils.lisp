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

;;; utils.lisp ends here