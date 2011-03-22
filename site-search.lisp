
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching for paths between items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +search-depth+ 15
  :test #'=
  :documentation "The depth limit for doing searches.")

(defstruct (item-search-problem (:include problem)))

(defmethod successors ((isp item-search-problem) node)
  (mapcar #'(lambda (item)
	      (cons item item))
	  (gethash (node-state node) *true-item-dependency-graph-forward*)))

(defun all-paths (source destination)
  (if (string= source destination)
      (list (list source))
      (mapcar #'(lambda (path)
		  (cons source path))
	      (reduce #'append 
		      (mapcar #'(lambda (successor)
				  (all-paths successor destination))
			      (gethash source *true-item-dependency-graph-forward*))))))

(defun all-paths-from-via (source destination via)
  (let ((paths-from-source-to-via (all-paths source via)))
    (when paths-from-source-to-via
      (let ((paths-from-via-to-destination (all-paths via destination)))
	(when paths-from-via-to-destination
	  (map-product #'(lambda (path-1 path-2)
			   (append path-1 (cdr path-2)))
		       paths-from-source-to-via
		       paths-from-via-to-destination))))))

(defun one-path-from-via (source destination via)
  "Find one path from SOURCE to DESTINATION that passes through VIA.
If there is no such path, return nil."
  (let ((source-to-via-problem (make-item-search-problem 
				:initial-state source
				:goal via))
	(via-to-destination-problem (make-item-search-problem
				     :initial-state via
				     :goal destination)))
    (multiple-value-bind (solution-to-via-found? solution-to-via)
	(bounded-depth-first-search source-to-via-problem +search-depth+)
      (if solution-to-via-found?
	  (multiple-value-bind (solution-to-dest-found? solution-to-dest)
	      (bounded-depth-first-search via-to-destination-problem
					  +search-depth+)
	    (if solution-to-dest-found?
		(append (explain-solution solution-to-via)
			(cdr (explain-solution solution-to-dest)))
		(if (null solution-to-dest)
		    (values nil nil)
		    (values nil :cut-off))))
	  (if (null solution-to-via)
	      (values nil nil)
	      (values nil :cut-off))))))

(defun all-paths-pass-through (source destination via)
  "Determine whether all paths from SOURCE to DESTINATION pass through
VIA.  Return two values: if all paths from SOURCE to DESTINATION do
pass through VIA, return T and NIL; otherwise, return NIL and a path
from SOURCE to DESTINATION that does not pass through VIA.

Note that STRING= is used as the hard-coded test for vertex equality."
  (every-with-falsifying-witness (all-paths source destination)
				 #'(lambda (path)
				     (member via path :test #'string=))))

(defun all-paths-avoid (source destination bad-guy)
  "Detemine whether all paths from node SOURCE to node DESTINATION
avoid (that is, do not pass through) node BAD-GUY.  Returns two
values: if there is a path from SOURCE to DESTINATION that passes
through BAD-GUY, return NIL as the first value and that withnessing
path as the second value; otherwise, return T as the first value and
NIL as the second value."
  (let* ((to-bad-guy (make-item-search-problem :goal bad-guy
					       :initial-state source))
	 (to-bad-guy-solution (bounded-depth-first-search to-bad-guy
							  +search-depth+)))
    (if to-bad-guy-solution
	(let* ((from-bad-guy (make-item-search-problem :goal destination
						       :initial-state bad-guy))
	       (from-bad-guy-solution (bounded-depth-first-search from-bad-guy
								  +search-depth+)))
	  (if from-bad-guy-solution
	      (let ((path-to-bad-guy (explain-solution to-bad-guy-solution))
		    (path-from-bad-guy (explain-solution from-bad-guy-solution)))
		(values nil (append path-to-bad-guy (cdr path-from-bad-guy))))
	      (values t nil)))
	(values t nil))))
