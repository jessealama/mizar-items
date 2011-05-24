
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching for paths between items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +search-depth+ 15
  :test #'=
  :documentation "The depth limit for doing searches.")

(defclass item-search-problem (delta-bidi-problem)
  nil)

(defmethod successors ((isp item-search-problem) node)
  (let ((deps (gethash (node-state node) *item-dependency-graph-forward*)))
    (mapcar #'(lambda (item) (cons item item)) deps)))

(defmethod predecessors ((isp item-search-problem) node)
  (let ((deps (gethash (node-state node) *item-dependency-graph-backward*)))
    (mapcar #'(lambda (item) (cons item item)) deps)))

(let ((table (make-hash-table :test #'equal)))
  (defun all-paths (source destination)
    (let ((key (cons source destination)))
      (multiple-value-bind (old-paths present?)
	  (gethash key table)
	(if present?
	    old-paths
	    (let ((new-paths
		   (if (string= source destination)
		       (list (list source))
		       (let ((source-pos (mml-lar-index-of-item source))
			     (dest-pos (mml-lar-index-of-item destination)))
			 (if (> dest-pos source-pos)
			     nil
			     (mapcar #'(lambda (path)
					 (cons source path))
				     (reduce #'append 
					     (mapcar #'(lambda (successor)
							 (all-paths successor destination))
						     (gethash source *item-dependency-graph-forward*)))))))))
	      (setf (gethash key table) new-paths)))))))

(let ((table (make-hash-table :test #'equal)))
  (defun count-paths (source destination)
    (let ((key (cons source destination)))
      (multiple-value-bind (num-known-paths present?)
	  (gethash key table)
	(if present?
	    num-known-paths
	    (let ((num-paths
		   (if (string= source destination)
		       1
		       (let ((source-pos (mml-lar-index-of-item source))
			     (dest-pos (mml-lar-index-of-item destination)))
			 (if (> dest-pos source-pos)
			     0
			     (reduce #'+
				     (mapcar #'(lambda (successor)
						 (count-paths successor destination))
					     (gethash source *item-dependency-graph-forward*))))))))
	      (setf (gethash key table) num-paths)))))))

(defgeneric one-path-quick (source destination))

(let ((symbol-factory (make-hash-table :test #'equal)))
  (defmethod one-path-quick ((source string) (destination string))
    (multiple-value-bind (source-symbol source-present?)
	(gethash source symbol-factory)
      (unless source-present?
	(let ((sym (make-symbol source)))
	  (setf (gethash source symbol-factory) sym
		source-symbol sym)))
      (multiple-value-bind (dest-symbol dest-present?)
	  (gethash destination symbol-factory)
	(unless dest-present?
	  (let ((sym (make-symbol destination)))
	    (setf (gethash destination symbol-factory) sym
		  dest-symbol sym)))
	(one-path-quick source-symbol dest-symbol)))))

(defmethod one-path-quick ((source symbol) (destination symbol))
  (let ((dest-pos (mml-lar-index-of-item (symbol-name destination))))
    (labels ((to-dest (source)
	       (if (eq source destination)
		   (list source)
		   (let ((source-pos (mml-lar-index-of-item (symbol-name source))))
		     (if (> dest-pos source-pos)
			 :fail
			 (loop
			    for successor in (gethash source *item-dependency-graph-forward*)
			    for successor-path = (to-dest successor)
			    do
			      (break "successor is ~a" successor)
			      (when (listp successor-path)
				(return (cons source successor-path)))
			    finally
			      (return :fail)))))))
      (to-dest source)))) 

;; (defun all-paths-from (source))

;; (defun all-paths-from-via (source via))

;; (defun all-paths-to (destination)
;;   (let ((*item-dependency-graph-forward* *item-dependency-graph-backward*))
;;     (mapcar #'reverse (all-paths-from destination))))

;; (defun all-paths-from-via (source destination via)
;;   (let ((paths-from-source-to-via (all-paths source via)))
;;     (when paths-from-source-to-via
;;       (let ((paths-from-via-to-destination (all-paths via destination)))
;; 	(when paths-from-via-to-destination
;; 	  (map-product #'(lambda (path-1 path-2)
;; 			   (append path-1 (cdr path-2)))
;; 		       paths-from-source-to-via
;; 		       paths-from-via-to-destination))))))

(defgeneric one-path-via (source destination &rest via)
  (:documentation "Find one path from SOURCE to DESTINATION that passes
through all the items listed in VIA.  The order of items in VIA is
immaterial.

Returns two values (SOLUTION EXPLANATION).  SOLUTION is the path
found, or NIL.  EXPLANATION is either NIL or the keyword :CUTOFF.  If
SOLUTION is NIL and EXPLANATION is NIL, then there are no paths at all
that start at SOURCE, end at DESTINATION, and pass through all the
items listed in VIA.  If SOLUTION is NIL and EXPLANATION is :CUTOFF,
then this means that the search for the desired paths was terminated
because we reached the cutoff depth specified under
+SEARCH-CUTOFF+.  (If a solution was found, i.e., if SOLUTION is
non-NIL, then the meaning of the secondary return value EXPLANATION is
undefined.)"))

(defmethod one-path-via :around (source destination &rest via)
  "This auxiliary method implements a quick sanity
  check to ensure that we do not search for paths that do not exist.  We check that either:

* the article of SOURCE and the article of DESTINATION are equal, in
  which case we require that

** every item in VIA also comes from the same article,
  
** the fragment number of SOURCE is greater than the fragment number
   of DESTINATION, and
  
** every item in VIA comes from the same article as SOURCE and
   DESTINATION
  
** the fragment number of every item in VIA is (weakly) between the
   item number of DESTINATION and SOURCE

or
  
* the article of SOURCE and VIA are different, in which case we
  require that
  
** SOURCE appears no later in the MML.LAR order than DESTINATION,
  
** Every item in VIA is such that:
  
*** it appears no earlier in MML.LAR order than SOURCE
  
*** it appears no later in MML.LAR order than DESTINATION"
  (destructuring-bind (source-article source-item-kind source-item-number)
      (split ":" source)
    (declare (ignore source-item-kind source-item-number))
    (destructuring-bind (dest-article dest-item-kind dest-item-number)
	(split ":" destination)
      (declare (ignore dest-item-kind dest-item-number))
      (let ((source-mml-pos (mml-lar-index source-article))
	    (dest-mml-pos (mml-lar-index dest-article)))
	(if (< source-mml-pos dest-mml-pos)
	    (values nil nil)
	    (let ((source-fragment (gethash source *item-to-fragment-table*))
		  (dest-fragment (gethash destination *item-to-fragment-table*)))
	      (destructuring-bind (source-fragment-article source-fragment-num-str)
		  (split ":" source-fragment)
		(declare (ignore source-fragment-article)) ; should be string= to SOURCE-ARTICLE
		(destructuring-bind (dest-fragment-article dest-fragment-num-str)
		    (split ":" dest-fragment)
		  (declare (ignore dest-fragment-article)) ; should be string= to DEST-ARTICLE
		  (let ((source-fragment-num (parse-integer source-fragment-num-str))
			(dest-fragment-num (parse-integer dest-fragment-num-str)))
		    (labels ((intermediate-mml-order-ok (intermediate)
			       (let ((int-mml-pos (mml-lar-index intermediate)))
				 (and (<= int-mml-pos dest-mml-pos)
				      (<= source-mml-pos int-mml-pos))))
			     (intermediate-same-source-ok (intermediate) ; assumes INTERMEDIATE and SOURCE come from the first article
			       (let ((int-fragment (gethash intermediate *item-to-fragment-table*)))
				 (destructuring-bind (int-fragment-article int-fragment-num-str)
				     (split ":" int-fragment)
				   (declare (ignore int-fragment-article)) ; should be string= to INT-ARTICLEh
				   (let ((int-fragment-num (parse-integer int-fragment-num-str)))
				     (<= source-fragment-num int-fragment-num)))))
			     (intermediate-same-dest-ok (intermediate) ; assumes INTERMEDIATE and DESTINATION come from the first article
			       (let ((int-fragment (gethash intermediate *item-to-fragment-table*)))
				 (destructuring-bind (int-fragment-article int-fragment-num-str)
				     (split ":" int-fragment)
				   (declare (ignore int-fragment-article)) ; should be string= to INT-ARTICLE
				   (let ((int-fragment-num (parse-integer int-fragment-num-str)))
				     (<= int-fragment-num dest-fragment-num)))))
			     (intermediate-ok (intermediate)
			       (and (intermediate-mml-order-ok intermediate)
				    (destructuring-bind (int-article int-item-kind int-item-num)
					intermediate
				      (declare (ignore int-item-kind int-item-num))
				      (if (string= int-article source-article)
					  (intermediate-same-source-ok intermediate)
					  (if (string= int-article dest-article)
					      (intermediate-same-dest-ok intermediate)
					      t))))))
		      (if (every #'intermediate-ok via)
			  (if (= source-mml-pos dest-mml-pos)
			      (if (< source-fragment-num dest-fragment-num)
				  (call-next-method)
				  (values nil nil))
			      (call-next-method))
			  (values nil nil))))))))))))

(defmethod one-path-via (source destination &rest via)
  "Find one path from SOURCE to DESTINATION that passes through VIA.
If there is no such path, return nil." 
  (let ((source-to-via-problem (make-instance 'item-search-problem 
					      :initial-state source
					      :goal via))
	(via-to-destination-problem (make-instance 'item-search-problem
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

(let ((table (make-hash-table :test #'equal)))
  (defun mml-lar-index-of-item (item)
    (multiple-value-bind (val present?)
	(gethash item table)
      (if present?
	  val
	  (setf (gethash item table)
		(mml-lar-index (item-article item)))))))

(defun items-reachable-from-item (item dependency-graph depth)
  (if (< depth 0)
      nil
      (if (zerop depth)
	  (list item)
	  (append (list item)
		  (remove-duplicates (reduce #'append (mapcar #'(lambda (i)
								  (items-reachable-from-item i dependency-graph (1- depth)))
							      (gethash item dependency-graph)))
				     :test #'string=)))))
  

(defmethod best-first-search-marking-deadends :around ((problem item-search-problem) eval-fn)
  (let ((solution-node (call-next-method)))
    ;; if non-nil, our solution is in the delta
    (when solution-node
      (multiple-value-bind (node-in-delta we-in-the-delta?)
	  (gethash (node-state solution-node)
		   (delta problem))
	(assert we-in-the-delta?)
	(merge-forward-and-backward-nodes solution-node node-in-delta)))))

(defgeneric one-path (source destination &optional limit nodes))

(defmethod one-path (source destination
		     &optional (limit +search-depth+)
		     (nodes (make-empty-queue)))
  "Find one path from SOURCE to DESTINATION that passes through VIA.                                                                                                                                        
If there is no such path, return nil."
  (declare (ignore limit))
  (let* ((problem (make-instance 'item-search-problem
				 :initial-state source
				 :goal destination))
	 (dest-mml-pos (mml-lar-index-of-item destination))
	 (dest-hash (sxhash destination)))
    ;; develop the delta
    (format t "Building the delta to depth 3...")
    (setf (delta problem)
	  (develop-delta (get-and-maybe-set-item-name destination)
			 3
			 problem))
    (flet ((too-far (action-item)
	     (destructuring-bind (action . item)
		 action-item
	       (declare (ignore action))
	       (let* ((node-mml-pos (mml-lar-index-of-item (symbol-name item))))
		 (when (null node-mml-pos)
		   (error "unknown article coming from item '~a'" item))
		 (when (null dest-mml-pos)
		   (error "unknown article coming from item '~a'" destination))
		 (< node-mml-pos dest-mml-pos)))))
      (defmethod goal-test ((p (eql problem)) node)
	(= (sxhash (node-state node)) dest-hash))
      (defmethod successors :around ((problem (eql problem)) node)
	(let* ((candidates (call-next-method))
	       (trimmed-candidates (remove-if #'too-far candidates)))
	  ;; (break "trimmed successors of ~a are ~{~a~% ~}" node (mapcar #'car trimmed-candidates))
	  trimmed-candidates))
      (defmethod h-cost ((problem (eql problem)) state)
	(let ((state-mml-pos (mml-lar-index-of-item (symbol-name state))))
	  (if (< state-mml-pos dest-mml-pos)
	      most-positive-fixnum
	      (- state-mml-pos dest-mml-pos))))
      (when (empty-queue? nodes)
	(enqueue-at-end nodes (list (make-instance 'node :state source))))
      (let ((solution (greedy-search-w/o-repeated-deadends problem)))
	(when solution
	  (explain-solution solution))))))

(defclass path-broker ()
  ((path-table
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor path-table
    :documentation "The table that maps pairs (SOURCE . DESTINATION) to information about paths from SOURCE to DESTINATION."))
  (:documentation "A path broker manages paths from a source to a
  destination.  It acts as an enumerator of these paths."))

(defvar *path-broker* (make-instance 'path-broker)
  "A convenient global instance of PATH-BROKER.")

(defmethod search-state (source destination &optional (broker *path-broker*))
  "The search state, according to BROKER, of the path search from SOURCE to DESTINATION.  The value is a queue."
  (let ((path-table (path-table broker))
	(key (cons source destination)))
    (multiple-value-bind (paths-and-more-nodes present?)
	(gethash key path-table)
      (if present?
	  (destructuring-bind (paths . more-nodes)
	      paths-and-more-nodes
	    (declare (ignore paths))
	    more-nodes)
	  (let ((initial (make-initial-queue source)))
	    (setf (gethash key path-table)
		  (cons nil initial))
	    initial)))))

(define-condition no-more-paths-error (error)
  ((source
    :initarg :source
    :reader source)
   (destination
    :initarg :destination
    :reader destination)
   (broker
    :initarg :broker
    :reader broker))
  (:documentation "An error representing the condition that an additional path fro SOURCE to DESTINATION (according to BROKER) has been requested, but there is no such path.")
  (:report (lambda (condition stream)
	     (format stream "There are no further paths from ~a to ~a (accoding to ~a)"
		     (source condition)
		     (destination condition)
		     (broker condition)))))

(defun register-path (source destination new-path new-nodes broker)
  (let ((path-table (path-table broker))
	(key (cons source destination)))
    (multiple-value-bind (paths-and-more-nodes present?)
	(gethash key path-table)
      (assert present?)
      (destructuring-bind (paths . more-nodes)
	  paths-and-more-nodes
	(declare (ignore more-nodes))
	(setf (gethash key path-table)
	      (cons (append paths (list new-path))
		    new-nodes))))))

(defun terminate-search-session (source destination broker)
  (let ((path-table (path-table broker))
	(key (cons source destination)))
    (multiple-value-bind (paths-and-more-nodes present?)
	(gethash key path-table)
      (assert present?)
      (destructuring-bind (paths . more-nodes)
	  paths-and-more-nodes
	(declare (ignore more-nodes))
	(setf (gethash key path-table)
	      (cons paths (make-empty-queue)))))))

(defgeneric next-path (source destination &optional broker)
  (:documentation "The next path from SOURCE to DESTINATION according
  to the path broker BROKER."))

(defmethod next-path (source destination &optional (broker *path-broker*))
  (let ((more-nodes (search-state source destination broker)))
    (if (empty-queue? more-nodes)
	(error 'no-more-paths-error
	       :source source
	       :destination destination
	       :broker broker
	       )
	(multiple-value-bind (solution-found? solution even-more-nodes)
	    (one-path source destination nil more-nodes)
	  (if solution-found?
	      (progn
		(register-path source destination solution even-more-nodes broker)
		solution)
	      (progn
		(terminate-search-session source destination broker)
		(error 'no-more-paths-error
		       :source source
		       :destination destination
		       :broker broker)))))))

(defgeneric possibly-more-paths? (source destination &optional broker)
  (:documentation "Have we computed all possible paths from SOURCE to
  DESTINATION (according to BROKER)?  If so, return NIL.  Otherwise,
  return T.  There may in fact not be any more paths from SOURCE to
  DESTINATION, but we haven't shown that to be so yet."))

(defmethod possibly-more-paths? (source destination &optional (broker *path-broker*))
  (not (empty-queue? (search-state source destination broker))))

(defun all-known-paths (source destination &optional (broker *path-broker*))
  (let ((path-table (path-table broker))
	(key (cons source destination)))
    (multiple-value-bind (paths-and-more-nodes present?)
	(gethash key path-table)
      (if present?
	  (destructuring-bind (paths . more-nodes)
	      paths-and-more-nodes
	    (declare (ignore more-nodes))
	    paths)
	  (progn
	    (setf (gethash key path-table)
		  (cons nil (make-initial-queue destination)))
	    nil)))))

(defun num-paths-known (source destination &optional (broker *path-broker*))
  (length (all-known-paths source destination broker)))

(defun nth-path (source destination n &optional (broker *path-broker*))
  (let ((num-known-paths (num-paths-known source destination broker)))
    (if (< n num-known-paths)
	(nth n (all-known-paths source destination broker))
	(loop
	   for i from 0 upto (- n num-known-paths)
	   do (next-path source destination broker) ; chuga chuga chuga...
	   finally
	     (let ((all-paths-now (all-known-paths source destination broker)))
	       (return (nth n all-paths-now)))))))

(defun all-possible-paths (source destination &optional (broker *path-broker*))
  (handler-case
      (loop do (next-path source destination broker)) ; over and over and ...

    (no-more-paths-error () (all-known-paths source destination broker))))

(defgeneric one-path-including-and-excluding (source destination to-be-included to-be-excluded)
  (:documentation "Find a path from SOURCE to DESTINATION that passes through all the items listed in TO-BE-INCLUDED but passes through none of the items mentioned in TO-BE-EXCLUDED."))

(defmethod one-path-excluding-and-including (source destination to-be-included to-be-excluded))

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
  (let* ((to-bad-guy (make-instance 'item-search-problem
				    :goal bad-guy
				    :initial-state source))
	 (to-bad-guy-solution (bounded-depth-first-search to-bad-guy
							  +search-depth+)))
    (if to-bad-guy-solution
	(let* ((from-bad-guy (make-instance 'item-search-problem
					    :goal destination
					    :initial-state bad-guy))
	       (from-bad-guy-solution (bounded-depth-first-search from-bad-guy
								  +search-depth+)))
	  (if from-bad-guy-solution
	      (let ((path-to-bad-guy (explain-solution to-bad-guy-solution))
		    (path-from-bad-guy (explain-solution from-bad-guy-solution)))
		(values nil (append path-to-bad-guy (cdr path-from-bad-guy))))
	      (values t nil)))
	(values t nil))))
