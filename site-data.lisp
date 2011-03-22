
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *itemization-source* "/local/data/alama/non-brutalized-itemizations")

(defparameter *dependency-graph-file*
  (mizar-items-config 'fragment-depdenency-graph))
(defparameter *item-to-ckb-file*
  (mizar-items-config 'item-to-fragment-path))
(defparameter *full-item-dependency-graph*
  (mizar-items-config 'full-item-dependency-graph))
(defparameter *full-vertex-neighbors-dependency-graph*
  (mizar-items-config 'full-vertex-neighbors-dependency-graph))

(defvar *dependency-graph* nil)
(defvar *num-dependency-graph-edges* nil)
(defvar *ckb-dependency-graph-forward* nil)
(defvar *ckb-dependency-graph-backward* nil)
(defvar *true-item-dependency-graph-forward* nil)
(defvar *true-item-dependency-graph-backward* nil)
(defvar *item-to-ckb-table* nil)
(defvar *ckb-to-items-table* nil)
(defvar *vertex-neighbors-dependency-graph* nil)
(defvar *all-ckb-items* nil)
(defvar *all-true-items* nil)
(defvar *graphs-loaded* nil)

(defun write-full-item-dependency-graph ()
  (loop
     with true-item-forward-table = (make-hash-table :test #'equal)
     for item being the hash-keys of *item-to-ckb-table*
     for ckb = (gethash item *item-to-ckb-table*)
     for forward-ckb-deps = (gethash ckb *ckb-dependency-graph-forward*)
     for forward-item-deps = (reduce #'append (mapcar #'(lambda (ckb-dep)
							  (gethash ckb-dep *ckb-to-items-table*))
						      forward-ckb-deps))
     do
       (setf (gethash item true-item-forward-table)
	     forward-item-deps)
     finally
       (with-open-file (item-depgraph *full-item-dependency-graph*
				      :direction :output
				      :if-exists :error
				      :if-does-not-exist :create)
	 (loop
	    for item being the hash-keys of true-item-forward-table
	    for deps = (gethash item true-item-forward-table)
	    do
	      (dolist (dep deps)
		(format item-depgraph "~a ~a~%" item dep))))))

(defun load-full-item-dependency-graphs ()
  (unless (file-exists-p *full-item-dependency-graph*)
    (write-full-item-dependency-graph))
  (let ((lines (lines-of-file *full-item-dependency-graph*))
	(forward-table (make-hash-table :test #'equal))
	(backward-table (make-hash-table :test #'equal)))
    (dolist (line lines)
      (destructuring-bind (lhs rhs)
	  (split " " line)
	(pushnew rhs (gethash lhs forward-table) :test #'string=)
	(pushnew lhs (gethash rhs backward-table) :test #'string=)))
    (setf *true-item-dependency-graph-forward* forward-table
	  *true-item-dependency-graph-backward* backward-table))
  t)

(defun write-vertex-neighbors-dependency-graph ()
  (unless (file-exists-p *full-item-dependency-graph*)
    (write-full-item-dependency-graph))
  (let ((lines (lines-of-file *full-item-dependency-graph*))
	(edges nil)
	(vertex-neighbors (make-hash-table :test #'equal)))
    (dolist (line lines)
      (destructuring-bind (lhs rhs)
	  (split " " line)
	(push (cons lhs rhs) edges)
	(pushnew rhs (gethash lhs vertex-neighbors) :test #'string=)))
    (with-open-file (vertex-neighbors
		     *full-vertex-neighbors-dependency-graph*
		     :direction :output
		     :if-exists :error
		     :if-does-not-exist :create)
      (flet ((write-vertex-and-neighbors (vertex neighbors)
	       (format vertex-neighbors "~a ~{~a~^ ~}~%" vertex neighbors)))
	(maphash #'write-vertex-and-neighbors
		 *true-item-dependency-graph-forward*))))
  t)

(defun load-vertex-neighbors-dependency-graph ()
  (unless (file-exists-p *full-vertex-neighbors-dependency-graph*)
    (write-vertex-neighbors-dependency-graph))
  (let ((lines (lines-of-file *full-vertex-neighbors-dependency-graph*))
	(vertex-neighbors (make-hash-table :test #'equal)))
    (dolist (line lines)
      (destructuring-bind (vertex &rest neighbors)
	  (split " " line)
	(setf (gethash vertex vertex-neighbors) neighbors)))
    (setf *vertex-neighbors-dependency-graph* vertex-neighbors))
  t)

(defun load-dependency-graph ()
  ;; all possible items 
  (let ((all-ckb-items (make-hash-table :test #'equal))
	(all-true-items (make-hash-table :test #'equal)))
    ;; ckb graph
    (let ((lines (lines-of-file *dependency-graph-file*))
	  (edges nil)
	  (ckb-forward-table (make-hash-table :test #'equal))
	  (ckb-backward-table (make-hash-table :test #'equal)))
      (dolist (line lines)
	(destructuring-bind (lhs rhs)
	    (split " " line)
	  (push (cons lhs rhs) edges)
	  (setf (gethash lhs all-ckb-items) t
		(gethash rhs all-ckb-items) t)
	  (pushnew rhs (gethash lhs ckb-forward-table) :test #'string=)
	  (pushnew lhs (gethash rhs ckb-backward-table) :test #'string=)))
      (setf *dependency-graph* edges
	    *num-dependency-graph-edges* (length edges)
	    *ckb-dependency-graph-forward* ckb-forward-table
	    *ckb-dependency-graph-backward* ckb-backward-table))
    ;; items-to-ckbs
    (let ((lines (lines-of-file *item-to-ckb-file*))
	  (item-to-ckb-table (make-hash-table :test #'equal))
	  (ckb-to-items-table (make-hash-table :test #'equal)))
      (dolist (line lines)
	(destructuring-bind (item ckb)
	    (split " " line)
	  (setf (gethash item all-true-items) t)
	  (setf (gethash item item-to-ckb-table) ckb)
	  (pushnew item (gethash ckb ckb-to-items-table) :test #'string=)))
      (setf *item-to-ckb-table* item-to-ckb-table
	    *ckb-to-items-table* ckb-to-items-table))
    ;; if the full item dependency graph doesn't exist, make it
    (unless (file-exists-p *full-item-dependency-graph*)
      (write-full-item-dependency-graph))
    (when (or (null *true-item-dependency-graph-forward*)
	      (null *true-item-dependency-graph-backward*))
      (load-full-item-dependency-graphs))
    ;; if the full vertex-neighbors dependency graph doesn't exist, make it
    (unless (file-exists-p *full-vertex-neighbors-dependency-graph*)
      (write-vertex-neighbors-dependency-graph))
    (when (null *vertex-neighbors-dependency-graph*)
      (load-vertex-neighbors-dependency-graph))
    (setf *all-ckb-items* all-ckb-items
	  *all-true-items* all-true-items))
  (setf *graphs-loaded* t)
  t)

(defvar *article-num-items* nil)

(defun count-miz-in-directory (dir)
  (let ((counter 0))
    (walk-directory dir #'(lambda (foo)
			    (declare (ignore foo))
			    (incf counter))
		    :test #'(lambda (path)
			      (scan "ckb[0-9]+\.miz$" (namestring path))))
    counter))

(defun load-article-num-items (&optional force)
  (if (or force (null *article-num-items*))
    (loop
       with num-items-table = (make-hash-table :test #'equal)
       for (article-name . title) in *articles*
       do
	 (let ((article-dir (concat *itemization-source* "/" article-name "/" "text")))
	   (setf (gethash article-name num-items-table)
		 (count-miz-in-directory article-dir)))
       finally
	 (setf *article-num-items* num-items-table)
	 (return *article-num-items*))
    *article-num-items*))
