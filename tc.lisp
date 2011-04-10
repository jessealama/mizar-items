
(in-package :mizar)

(defmacro define-tc-for-dimension (dimension)
  "A macro for defining optimized functions for computing transitive
closures.

The new function will be called \"TC-<NUM>\", where NUM is the
dimension passed in an as argument.  The function will accept square
bit arrays of dimension DIMENSION by DIMENSION.  The function is
destructive: it modiies its argument.  It returns NIL.  The function
is optimzed entirely for speed; don't expect to get any useful
information."
  (let* ((tc-fun-name (format nil "tc-~d" dimension))
	(tc-fun-symbol (make-symbol tc-fun-name)))
    `(defun ,tc-fun-symbol (square-arr)
       (declare (type (array bit (,dimension ,dimension)) square-array)
		(optimize (compilation-speed 0)
			  (debug 0)
			  (safety 0)
			  (space 0)
			  (speed 3)))
       (dotimes (i ,dimension)
	 (dotimes (j ,dimension)
	   (unless (zerop (aref square-array i j))
	     (dotimes (k ,dimension)
	       (unless (zerop (aref square-array j k))
		 (setf (aref square-array i k) 1)))))))))
