
(in-package :mizar)

(defmethod predecessors ((problem problem) node)
  "Return an alist of (action . state) pairs from which this state can be accessed"
  (declare (ignore node))
  (error "You need to define a SUCCESSORS method for ~A" problem))

(defun develop-delta (mouth depth)
  (declare (ignore mouth depth))
  (make-hash-table :test #'eq))

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

(defun bidirectional-limited-dfs-with-delta (source destination delta-depth search-depth)
  (let ((delta (develop-delta destination delta-depth))
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
       (enqueue-at-front nodes (expand node problem)))))