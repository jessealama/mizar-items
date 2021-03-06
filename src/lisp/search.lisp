
;;; search.lisp: Problems, nodes, search trees, search strategies

(in-package :mizar)

(defclass problem ()
  ((initial-state
    :initform nil
    :accessor problem-initial-state
    :initarg :initial-state
    :documentation "A state in the domain.")
   (goal
    :initform nil
    :accessor problem-goal
    :initarg :goal
    :documentation "Optionally store the desired state here.")
   (num-expanded
    :initform 0
    :accessor problem-num-expanded
    :documentation "Number of nodes expanded in search for solution."))
  (:documentation "A problem is defined by the initial state, and
the type of problem it is.  We will be defining subtypes of PROBLEM
later on.  For bookkeeping, we count the number of nodes expanded."))

(defclass node ()
  ((state
    :initform nil
    :accessor node-state
    :initarg :state
    :documentation "A state in the domain.")
   (parent
    :initform nil
    :accessor node-parent
    :initarg :parent
    :documentation "The parent node of this node")
   (action
    :initform nil
    :accessor node-action
    :initarg :action
    :documentation "The domain action leading to state")
   (successors
    :initform nil
    :accessor node-successors
    :documentation "A list of successor nodes")
   (depth
    :initform 0
    :accessor node-depth
    :initarg :depth
    :documentation "Depth of node in tree (root = 0)")
   (g-cost
    :initform 0
    :accessor node-g-cost
    :initarg :g-cost
    :documentation "Path cost from root to node")
   (h-cost
    :initform 0
    :accessor node-h-cost
    :initarg :h-cost
    :documentation "Estimated distance from state to goal")
   (f-cost
    :initform 0
    :accessor node-f-cost
    :initarg :f-cost
    :documentation "g-cost + h-cost")
   (expanded?
    :initform nil
    :accessor node-expanded?
    :documentation "Any successors examined?")))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (with-slots (state)
	node
      (if (node-parent node)
	  (format stream "~A [parent: ~A]" state (node-state (node-parent node)))
	  (format stream "~A (root node)" state)))))

(defmethod successors ((problem problem) node)
  "Return an alist of (action . state) pairs, reachable from this state."
  (declare (ignore node))
  (error "You need to define a SUCCESSORS method for ~A" problem))

(defmethod goal-test ((problem problem) node)
  "Return true or false: is this a goal node?  This default method
checks if the state is equal to the state stored in the problem-goal
slot.  You will need to define your own method if there are multiple
goals, or if you need to compare them with something other than
EQUAL."
  (equal (node-state node) (problem-goal problem)))

(defmethod h-cost ((problem problem) state) 
  "The estimated cost from state to a goal for this problem.  
  If you don't overestimate, then A* will always find optimal solutions.
  The default estimate is always 0, which certainly doesn't overestimate."
  (declare (ignore state))
  0)

(defmethod edge-cost ((problem problem) node action state)
  "The cost of going from one node to the next state by taking action.
  This default method counts 1 for every action.  Provide a method for this if 
  your subtype of problem has a different idea of the cost of a step."
  (declare (ignore node action state))
  1)

(defun node-ancestors (node)
  "The ancestors of NODE, starting with its most distant
ancestor (i.e., the ancestor of NODE whose parent is NIL)."
  (labels ((node-ancestors-backwards (n)
	     (if (node-parent n)
		 (cons n (node-ancestors-backwards (node-parent n)))
		 (list n))))
    (reverse (node-ancestors-backwards (node-parent node)))))

(defun always-zero (n a s)
  (declare (ignore n a s))
  0)

(defun always-one (n a s)
  (declare (ignore n a s))
  1)

(defun expand (node problem)
  (if (node-expanded? node)
      (node-successors node)
      (progn
	(setf (node-expanded? node) t)
	(incf (problem-num-expanded problem))
	(loop 
	   with nodes = nil
	   for successor in (successors problem node)
	   do
	     (destructuring-bind (action . state)
		 successor
	       (let ((g (+ (node-g-cost node)
			   (edge-cost problem node action state)))
		     (h (h-cost problem state)))
		 (push (make-instance 'node
				      :parent node 
				      :action action 
				      :state state
				      :g-cost g
				      :h-cost h
				      :f-cost (max (node-f-cost node) (+ g h))
				      :depth (1+ (node-depth node)))
		     nodes)))
	   finally
	     (setf (node-successors node) nodes)
	     (return nodes)))))

(defun create-start-node (problem)
  "Make the starting node, corresponding to the problem's initial state."
  (make-instance 'node :state (problem-initial-state problem)))

(defun leaf-nodes (node)
  "All nodes reachable from NODE (via the successor function) that are either unexpanded or have no successors (and are expanded)."
  (if (node-expanded? node)
      (let ((succ (node-successors node)))
	(if (null succ)
	    (list node)
	    (apply #'append (mapcar #'leaf-nodes succ))))
      (list node)))

(defun expandable-leaf-nodes (node)
  "All leaf nodes reachable from NODE that can be expanded."
  (remove-if #'node-expanded? (leaf-nodes node)))

(defun first-splitting-descendent (node)
  "The first descendant of NODE that has multiple successors.  If
there are no such nodes (i.e., the set of descendents of NODE forms a
linear sequence), return NIL."
  (let ((succs (node-successors node)))
    (if (null succs)
	nil
	(if (null (cdr succs))
	    (first-splitting-descendent (first succs))
	    node))))

(defun make-initial-queue (initial-state 
			   &key (queueing-function #'enqueue-at-end))
  (let ((q (make-empty-queue)))
    (funcall queueing-function q (list (make-instance 'node
						      :state initial-state)))
    q))

(defun general-search (problem queueing-function)
  "Expand nodes according to the specification of PROBLEM until we find
  a solution or run out of nodes to expand.  The QUEUING-FN decides which
  nodes to look at first."
  (let ((nodes (make-initial-queue (problem-initial-state problem)
				   :queueing-function queueing-function)))
    (let (node)
      (loop (if (empty-queue? nodes) (return nil))
	 (setf node (remove-front nodes))
	 (if (goal-test problem node) (return node))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-bounded-search (problem queueing-function depth)
  "Expand nodes according to the specification of PROBLEM until we
  find a solution or run out of nodes to expand or exceed the
  specified DEPTH.  QUEUING-FN decides which nodes to look at
  first."
  (let ((nodes (make-initial-queue (problem-initial-state problem)
				   :queueing-function queueing-function)))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (remove-front nodes))
	 (if (> (node-depth node) depth) (return (values nil :cut-off)))
	 (if (goal-test problem node) (return (values t node)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-bounded-search-with-nodes (problem queueing-function depth &optional queue)
  "Expand nodes according to the specification of PROBLEM until we
find a solution or run out of nodes to expand or exceed the specified
DEPTH.  QUEUING-FN decides which nodes to look at first.  QUEUE is a
initial queue of node.  (NIL is an acceptable value for QUEUE.) This
function behaves like a breadth-first search in the sense that as soon
as a node is encountered whose depth exceeds DEPTH, it stops.

Returns three values: (SUCCESS SOLUTION REMAINING-NODES)."
  (let ((nodes (or queue
		   (make-initial-queue (problem-initial-state problem)
				       :queueing-function queueing-function))))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil nodes)))
	 (setf node (remove-front nodes))
	 (if (> (node-depth node) depth) (return (values nil :cut-off nodes)))
	 (if (goal-test problem node) (return (values t node nodes)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-search-with-nodes (problem queueing-function &optional queue)
  (let ((nodes (or queue
		   (make-initial-queue (problem-initial-state problem)
				       :queueing-function queueing-function))))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (remove-front nodes))
	 (if (goal-test problem node) (return (values node nodes)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-search-for-bottom (problem queueing-function &optional queue)
  "Expand nodes according to the specification of PROBLEM until we
find a node with no successors or we run out of nodes to expand.  The
QUEUING-FN decides which nodes to look at first."
  (let ((nodes (or queue
		   (make-initial-queue (problem-initial-state problem)
				       :queueing-function queueing-function))))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (remove-front nodes))
	 (if (goal-test problem node) (return (values node nodes)))
	 (let ((successors (expand node problem)))
	   (if successors
	       (funcall queueing-function nodes successors)
	       (return (values node nodes))))))))

(defun explain-solution (node)
  "Give the sequence of actions that produced NODE.  When NODE is a
solution to a search problem, this function gives a \"printout\" of
how the node was obtained, starting from an initial node."
  (labels ((explain-backwards (n) 
	     (if (node-parent n)
		 (cons (node-action n)
		       (explain-backwards (node-parent n)))
		 (list (node-state n)))))
    (reverse (explain-backwards node))))

(defun breadth-first-search (problem)
  "Search the shallowest nodes in the search tree first."
  (general-search problem #'enqueue-at-end))

(defun bounded-breadth-first-search (problem depth)
  "Search the shallowest nodes in the search tree first, but don't go
deeper than DEPTH."
  (general-bounded-search problem #'enqueue-at-end depth))

(defun bounded-depth-first-search (problem depth)
  "Search the deepest nodes in the search tree first."
  (general-bounded-search problem #'enqueue-at-front depth))

(defun bounded-breadth-first-search-with-nodes (problem depth &optional queue)
  "Search the shallowest nodes in the search tree first, but don't go
deeper than DEPTH.  QUEUE is an (possibly empty) initial pool of
nodes.  NIL is a permissible value for QUEUE."
  (general-bounded-search-with-nodes problem #'enqueue-at-end depth queue))

(defun bounded-depth-first-search-with-nodes (problem depth &optional queue)
  "Search the deepst nodes in the search tree first, but don't go
deeper than DEPTH.  QUEUE is an (possibly empty) initial pool of
nodes.  NIL is a permissible value for QUEUE."
  (let ((nodes (or queue
		   (make-initial-queue (problem-initial-state problem)
				       :queueing-function #'enqueue-at-front))))
    (let (node cutoff)
      (loop
	 (if (empty-queue? nodes) (return (values nil (when cutoff :cut-off) nodes)))
	 (setf node (remove-front nodes))
	 (if (> (node-depth node) depth) (setf cutoff t))
	 (if (goal-test problem node) (return (values t node nodes)))
	 (enqueue-at-front nodes (expand node problem))))))

(defun breadth-first-search-for-bottom-with-nodes (problem &optional queue)
  "Search the shallowest nodes in the search tree first."
  (general-search-for-bottom problem #'enqueue-at-end queue))

(defun breadth-first-search-with-nodes (problem &optional queue)
  "Search the shallowest nodes in the search tree first."
  (general-search-with-nodes problem #'enqueue-at-end queue))

(defun depth-first-search (problem)
  "Search the deepest nodes in the search tree first."
  (general-search problem #'enqueue-at-front))

(defun depth-first-search-for-bottom (problem)
  (general-search-for-bottom problem #'enqueue-at-front))

(defun iterative-deepening-search (problem)
  "Do a series of depth-limited searches, increasing depth each time."
  (loop for depth = 0 do
       (let ((solution (depth-limited-dfs-search problem depth)))
	 (unless (eq solution :cut-off) (return solution)))))

(defun depth-limited-dfs-search (problem &optional limit (node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree."
  (cond ((goal-test problem node) node)
        ((and (integerp limit)
	      (>= (node-depth node) limit))
	 :cut-off)
        (t (loop for n in (expand node problem) do
		(let ((solution (depth-limited-dfs-search problem limit n)))
		  (when (and solution
			     (not (eq solution :cut-off)))
		    (return solution)))))))

(defun exhaustive-depth-limited-search (problem &optional limit
			              (node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree.
Expand until there are no more nodes of depth less than LIMIT that are
unexpanded."
  (let ((to-do (list node)))
    (until (null to-do)
      (let ((current-node (pop to-do)))
	(when (< (node-depth current-node) limit)
	  (unless (node-expanded? current-node)
	    (expand current-node problem))
	  (dolist (successor (node-successors current-node))
	    (push successor to-do)))))))
						       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Avoiding repeated states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun looping-node? (node &optional depth (test #'equal))
  "Did this node's state appear previously in the path?"
  (let ((n (node-parent node)))
    (if depth
	(loop for i from 1 to depth do
	     (when (null n)
	       (return nil))
	     (when (funcall test (node-state node) (node-state n))
	       (return t))
	     (setf n (node-parent n)))
	(loop for i = 1 do
	     (when (null n)
	       (return nil))
	     (when (funcall test (node-state node) (node-state n))
	       (return t))
	     (setf n (node-parent n))))))

(defun return-node? (node &optional (test #'equal))
  "Is this a node that returns to the state it just came from?"
  (looping-node? node 2 test))

(defun eliminate-returns (nodes)
  "Get rid of nodes that return to the state they just came from,
i.e., where the last two actions just undo each other."
  (remove-if #'return-node? nodes))

(defun eliminate-cycles (nodes &optional (test #'equal))
  "Get rid of nodes that end in a state that has appeared before in
the path."
  (remove-if #'(lambda (node)
		 (looping-node? node nil test))
	     nodes))

(defun eliminate-all-duplicates (nodes node-table)
  "Get rid of all nodes that have been seen before in any path."
  (let ((result nil))
   (loop for node in nodes do
	(let ((state (node-state node)))
	  (when (not (gethash state node-table))
	    (push node result))
	  (setf (gethash state node-table) node)))
   result))

(defun no-cycles-depth-first-search (problem &optional (test #'equal))
  "Do depth-first search, but eliminate paths with repeated states."
  (general-search problem
		  #'(lambda (old-q nodes)
		      (enqueue-at-front old-q 
					(eliminate-cycles nodes
							  test)))))

(defun no-cycles-depth-first-search-for-bottom (problem &optional (test #'equal))
  "Do depth-first search, but eliminate paths with repeated states."
  (general-search-for-bottom problem
			     #'(lambda (old-q nodes)
				 (enqueue-at-front old-q 
						   (eliminate-cycles nodes
								     test)))))

(defun no-returns-breadth-first-search (problem)
  "Do breadth-first search, but eliminate immediate returns to a prior
state."
  (general-search problem
		  #'(lambda (old-q nodes)
		      (enqueue-at-end old-q (eliminate-returns nodes)))))
			      
(defun no-duplicates-breadth-first-search (problem)
  "Do breadth-first search, but eliminate all duplicate states."
  (let ((table (make-hash-table :test #'equal)))
    (general-search problem
		    #'(lambda (old-q nodes)
			(enqueue-at-end old-q (eliminate-all-duplicates
					       nodes table))))))

;; Heuristic search

(defun best-first-search (problem eval-fn)
  "Search the nodes with the best evaluation first. [p 93]"
  (general-search problem #'(lambda (old-q nodes) 
			      (enqueue-by-priority old-q nodes eval-fn))))

(defun greedy-search (problem)
  "Best-first search using H (heuristic distance to goal). [p 93]"
  (best-first-search problem #'node-h-cost))

(defun tree-a*-search (problem)
  "Best-first search using estimated total cost, or (F = G + H). [p 97]"
  (best-first-search problem #'node-f-cost))

(defun uniform-cost-search (problem)
  "Best-first search using the node's depth as its cost.  Discussion on [p 75]"
  (best-first-search problem #'node-depth))

(defgeneric best-first-search-marking-deadends (problem eval-fn &optional nodes))

(defmethod best-first-search-marking-deadends (problem eval-fn &optional nodes)
  (let* ((deadends (make-hash-table :test #'equal))
	 (queueing-function #'(lambda (old-q nodes) 
				(enqueue-by-priority old-q nodes eval-fn)))
	 (nodes (or nodes
		    (make-initial-queue (problem-initial-state problem)
					:queueing-function queueing-function))))
    (defmethod successors :around ((p (eql problem)) node)
      (let ((succs (call-next-method)))
	(remove-if #'(lambda (state) (gethash state deadends))
		   succs
		   :key #'car)))
    (let (node)
      (loop 
	 (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (loop
		       named find-viable-node
		       for n = (remove-front nodes)
		       do
			 (unless (gethash (node-state n) deadends)
			   (return-from find-viable-node n))
			 (when (empty-queue? nodes)
			   (return-from find-viable-node nil))))
	 (when (null node)
	   (return (values nil nil)))
	 (if (goal-test problem node) (return (values node nodes)))
	 (let ((expanded (expand node problem)))
	   (if expanded
	       (funcall queueing-function nodes expanded)
	       (progn
		 (format t "Deadend encountered: ~a~%" (node-state node))
		 (setf (gethash (node-state node) deadends) t))))))))

(defun greedy-search-w/o-repeated-deadends (problem &optional nodes)
  "Best-first search using H (heuristic distance to goal). [p 93]"
  (best-first-search-marking-deadends problem #'node-h-cost nodes))

(defun tree-a*-search-w/o-repeated-deadends (problem)
  "Best-first search using estimated total cost, or (F = G + H). [p 97]"
  (best-first-search-marking-deadends problem #'node-f-cost))

;;; search.lisp ends here