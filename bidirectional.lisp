
(in-package :mizar)

(defclass bidi-problem (problem)
  nil)

(defgeneric predecessors (problem node))

(defclass delta-bidi-problem (bidi-problem)
  ((delta 
    :accessor delta
    :initform (make-hash-table :test #'eq))))

(defmethod goal-test ((problem delta-bidi-problem) node)
  (gethash (node-state node) (delta problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bidirectional search with deltas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun develop-delta (mouth depth problem)
  (loop
     with delta = (make-hash-table :test #'eq)
     with q = (make-initial-queue mouth)
     for node = (remove-front q)
     do
       (when (<= (node-depth node) depth)
	 (loop
	    with succs = (predecessors problem node)
	    for (action . succ) in succs
	    do
	      (multiple-value-bind (whatever seen-before?)
		  (gethash succ delta)
		(declare (ignore whatever))
		(unless seen-before?
		  (let ((succ-node (make-instance 'node
						  :state succ
						  :parent node)))
		    (setf (gethash succ delta) succ-node)
		    (enqueue-at-front q (list succ-node)))))))
       (when (empty-queue? q)
	 (return delta))))

(defun merge-forward-and-backward-nodes (forward-node backward-node)
  "FORWARD-NODE is a node whose ultimate ancestor represents the start
  of a search problem.  BACKWARD-NODE is a node whose ultimate
  ancestor represents the end of a search problem.  Their states
  should be the same; the idea is that a birectional search starting
  from the initial state has progressed, in a forward manner, to
  FORWARD-NODE and, in a backward manner, to BACKWARD-NODE.  A
  solution is thus available.  Return a new node whose state is the
  goal of the search problem, and which merges FORWARD-NODE and
  BACKWARD-NODE into a coherent solution."
  (do ((a forward-node b)
       (b (node-parent backward-node) (node-parent b)))
      ((null b) a)
    (setf (node-parent b) a)))

(defun bidirectional-limited-dfs-with-delta (bidi-problem delta-depth search-depth)
  (let* ((source (problem-initial-state bidi-problem))
	 (destination (problem-goal bidi-problem))
	 (delta (develop-delta destination delta-depth bidi-problem))
	 (limit (if (integerp search-depth) search-depth most-positive-fixnum))
	 (nodes (make-initial-queue source
				    :queueing-function #'enqueue-at-front))
	 (node nil)
	 (cutoff nil))
    (loop
       (when (empty-queue? nodes)
	 (return (when cutoff :cut-off)))
       (setf node (remove-front nodes))
       (if (> (node-depth node) limit) (setf cutoff t))
       (multiple-value-bind (node-in-delta we-in-the-delta?)
	   (gethash (node-state node) delta)
	 (when we-in-the-delta?
	   (return (merge-forward-and-backward-nodes node node-in-delta))))
       (enqueue-at-front nodes (expand node bidi-problem)))))