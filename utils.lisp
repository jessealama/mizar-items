;;; utils.lisp

(in-package :mizar)

(defun concat (&rest strings)
  (apply 'concatenate 'string strings))

;;; utils.lisp ends here