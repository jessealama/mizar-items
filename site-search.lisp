
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching for paths between items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +search-depth+ 15
  :test #'=
  :documentation "The depth limit for doing searches.")

(defstruct (item-search-problem (:include problem)))

(defmethod successors ((isp item-search-problem) node)
  (let ((deps (gethash (node-state node) *item-dependency-graph-forward*)))
    (mapcar #'(lambda (item) (cons item item)) deps)))

(defun all-paths (source destination)
  (if (string= source destination)
      (list (list source))
      (mapcar #'(lambda (path)
		  (cons source path))
	      (reduce #'append 
		      (mapcar #'(lambda (successor)
				  (all-paths successor destination))
			      (gethash source *item-dependency-graph-forward*))))))

(defun all-paths-from-via (source destination via)
  (let ((paths-from-source-to-via (all-paths source via)))
    (when paths-from-source-to-via
      (let ((paths-from-via-to-destination (all-paths via destination)))
	(when paths-from-via-to-destination
	  (map-product #'(lambda (path-1 path-2)
			   (append path-1 (cdr path-2)))
		       paths-from-source-to-via
		       paths-from-via-to-destination))))))

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
	    (let ((source-fragment (gethash source *item-to-ckb-table*))
		  (dest-fragment (gethash destination *item-to-ckb-table*)))
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
			       (let ((int-fragment (gethash intermediate *item-to-ckb-table*)))
				 (destructuring-bind (int-fragment-article int-fragment-num-str)
				     (split ":" int-fragment)
				   (declare (ignore int-fragment-article)) ; should be string= to INT-ARTICLEh
				   (let ((int-fragment-num (parse-integer int-fragment-num-str)))
				     (<= source-fragment-num int-fragment-num)))))
			     (intermediate-same-dest-ok (intermediate) ; assumes INTERMEDIATE and DESTINATION come from the first article
			       (let ((int-fragment (gethash intermediate *item-to-ckb-table*)))
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

(defmethod one-path (source destination)
  "Find one path from SOURCE to DESTINATION that passes through VIA.                                                                                                                                        
If there is no such path, return nil."
  (let ((problem (make-item-search-problem :initial-state source
                                           :goal destination)))
    (multiple-value-bind (solution-found? solution)
        (bounded-depth-first-search problem 25)
      (if solution-found?
          (values t (explain-solution solution))
          (if (null solution)
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
