
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

(defclass item-transitive-closure ()
  ((item-table :type hash-table
	       :documentation "A mapping from all known items to natural numbers."
	       :accessor item-table)
   (adj-matrix :type (array bit)
	       :accessor adj-matrix
	       :documentation "An adjacent matrix representing the
	       transitive closure of the graph
	       *ITEM-DEPENDENCY-GRAPH-FORWARD*, with indices labelled
	       by natural numbers according to ITEM-TABLE.")))

(defparameter *the-tc* nil)

(defmethod initialize-instance :around ((class item-transitive-closure)
					&rest initargs
					&key &allow-other-keys)
  (or *the-tc*
      (let ((item-table (make-hash-table :test #'equal))
	    (next-index 0))
	(flet ((register-item (item)
		 (unless (gethash item item-table)
		   (setf (gethash item item-table) next-index
			 next-index (1+ next-index)))))
	  (loop
	     with tc = (call-next-method)
	     with num-items = (count-items)
	     with adj-matrix = (make-array (list num-items num-items)
					   :element-type 'bit
					   :initial-element 0)
	     for item being the hash-keys in *item-dependency-graph-forward*
	     for deps being the hash-values in *item-dependency-graph-forward*
	     do
	       (register-item item)
	       (dolist (dep deps)
		 (register-item dep))
	       (let ((i (gethash item item-table)))
		 (dolist (dep deps)
		   (let ((j (gethash dep item-table)))
		     (setf (aref adj-matrix i j) 1))))
	     finally
	       (setf (adj-matrix tc) adj-matrix
		     (item-table tc) tc
		     *the-tc* tc)
	       (return tc))))))

(defmethod item-index ((tc item-transitive-closure) item)
  (gethash item (item-table tc)))

(defmethod exists-path-from-to ((tc item-transitive-closure) from to)
  (let ((i (item-index *the-tc* from))
	(j (item-index *the-tc* to)))
    (not (zerop (aref (adj-matrix tc) i j)))))