
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *itemization-source* "/local/data/alama/non-brutalized-itemizations")

(defparameter *item-to-ckb-file*
  (mizar-items-config 'item-to-fragment-path))

(defvar *dependency-graph* nil)
(defvar *num-dependency-graph-edges* nil)
(defvar *ckb-dependency-graph-forward* nil)
(defvar *ckb-dependency-graph-backward* nil)
(defvar *item-dependency-graph-forward* nil)
(defvar *item-dependency-graph-backward* nil)
(defvar *item-to-ckb-table* nil)
(defvar *ckb-to-items-table* nil)
(defvar *vertex-neighbors-dependency-graph* nil)
(defvar *all-ckb-items* nil)
(defvar *all-items* nil)
(defvar *graphs-loaded* nil)

(defun write-full-item-dependency-graph ()
  (loop
     with item-forward-table = (make-hash-table :test #'equal)
     for item being the hash-keys of *item-to-ckb-table*
     for ckb = (gethash item *item-to-ckb-table*)
     for forward-ckb-deps = (gethash ckb *ckb-dependency-graph-forward*)
     for forward-item-deps = (reduce #'append (mapcar #'(lambda (ckb-dep)
							  (gethash ckb-dep *ckb-to-items-table*))
						      forward-ckb-deps))
     do
       (setf (gethash item item-forward-table)
	     forward-item-deps)
     finally
       (with-open-file (item-depgraph (mizar-items-config 'full-item-dependency-graph)
				      :direction :output
				      :if-exists :error
				      :if-does-not-exist :create)
	 (loop
	    for item being the hash-keys of item-forward-table
	    for deps = (gethash item item-forward-table)
	    do
	      (dolist (dep deps)
		(format item-depgraph "~a ~a~%" item dep))))))

(defun load-full-item-dependency-graphs ()
  (let ((forward-file (mizar-items-config 'vertex-neighbors-forward-graph-path))
	(backward-file (mizar-items-config 'vertex-neighbors-backward-graph-path)))
    (if (file-exists-p forward-file)
	(if (file-exists-p backward-file)
	    (progn
	      (format t "Loading pre-computed vertex-neighbor graph files...")
	      (let ((forward-lines (lines-of-file forward-file))
		    (backward-lines (lines-of-file backward-file))
		    (forward-table (make-hash-table :test #'equal))
		    (backward-table (make-hash-table :test #'equal)))
		(dolist (line forward-lines)
		  (destructuring-bind (lhs &rest neighbors)
		      (split " " line)
		    (setf (gethash lhs forward-table) neighbors)))
		(dolist (line backward-lines)
		  (destructuring-bind (lhs &rest neighbors)
		      (split " " line)
		    (setf (gethash lhs backward-table) neighbors)))
		(format t "done~%")
		(values forward-table backward-table)))
	    (error "The vertex-neighbors file (backwards) doesn't exist at the expected location '~a'" backward-file))
	(error "The vertex-neighbors file (forwards) doesn't exist at the expected location '~a'" forward-file))))

(defun write-vertex-neighbors-dependency-graphs ()
  (if (file-exists-p (mizar-items-config 'full-item-dependency-graph))
      (progn
	(format t "Writing the vertex-neighbors dependency graphs...")
	(with-open-file (vertex-neighbors
			 (mizar-items-config 'vertex-neighbors-forward-graph-path)
			 :direction :output
			 :if-exists :error
			 :if-does-not-exist :create)
	  (flet ((write-vertex-and-neighbors (vertex neighbors)
		   (format vertex-neighbors "~a ~{~a~^ ~}~%" vertex neighbors)))
	    (maphash #'write-vertex-and-neighbors
		     *item-dependency-graph-forward*)))
	(with-open-file (vertex-neighbors
			 (mizar-items-config 'vertex-neighbors-backward-graph-path)
			 :direction :output
			 :if-exists :error
			 :if-does-not-exist :create)
	  (flet ((write-vertex-and-neighbors (vertex neighbors)
		   (format vertex-neighbors "~a ~{~a~^ ~}~%" vertex neighbors)))
	    (maphash #'write-vertex-and-neighbors
		     *item-dependency-graph-backward*)))
	(format t "done~%"))
      (error "We cannot produce the vertex-neighbors dependency graph, because the item-to-item dependency graph does not exist at the expected location '~a'" (mizar-items-config 'full-item-dependency-graph))))

(defun load-vertex-neighbors-dependency-graphs ()
  (let ((backward-path (mizar-items-config 'vertex-neighbors-backward-graph-path))
	(forward-path (mizar-items-config 'vertex-neighbors-forward-graph-path)))
  (if (file-exists-p forward-path)
      (if (file-exists-p backward-path)
	  (let ((backward-lines (lines-of-file backward-path))
		(forward-lines (lines-of-file forward-path))
		(vertex-forward-neighbors (make-hash-table :test #'equal))
		(vertex-backward-neighbors (make-hash-table :test #'equal)))
	    (dolist (line backward-lines)
	      (destructuring-bind (vertex &rest neighbors)
		  (split " " line)
		(setf (gethash vertex vertex-backward-neighbors) neighbors)))
	    (dolist (line forward-lines)
	      (destructuring-bind (vertex &rest neighbors)
		  (split " " line)
		(setf (gethash vertex vertex-backward-neighbors) neighbors)))
	    (setf *item-dependency-graph-backward* vertex-backward-neighbors
		  *item-dependency-graph-forward* vertex-forward-neighbors)
	    t)
	  (error "The vertex-neighbors forward dependency graph doesn't exist at the expected location '~a'" forward-path))
	  (error "The vertex-neighbors backward dependency graph doesn't exist at the expected location '~a'" backward-path))))

(defun load-fragment-depgraph ()
  (if (file-exists-p (mizar-items-config 'fragment-depdenency-graph))
      (let ((all-ckb-items (make-hash-table :test #'equal))
	    (lines (lines-of-file (mizar-items-config 'fragment-depdenency-graph)))
	    (ckb-forward-table (make-hash-table :test #'equal))
	    (ckb-backward-table (make-hash-table :test #'equal)))
	(format t "Loading fragment dependency graph...")
	(dolist (line lines)
	  (destructuring-bind (lhs rhs)
	      (split " " line)
	    (setf (gethash lhs all-ckb-items) t
		  (gethash rhs all-ckb-items) t)
	    (pushnew rhs (gethash lhs ckb-forward-table) :test #'string=)
	    (pushnew lhs (gethash rhs ckb-backward-table) :test #'string=)))
	(format t "done.~%")
	(values ckb-forward-table ckb-backward-table all-ckb-items))
      (error "The fragment dependency graph doesn't exist at the expected location, '~a'" (mizar-items-config 'fragment-depdenency-graph))))

(defun load-item-to-fragment-table ()
  (if (file-exists-p *item-to-ckb-file*)
      (let ((lines (lines-of-file *item-to-ckb-file*))
	    (item-to-ckb-table (make-hash-table :test #'equal))
	    (ckb-to-items-table (make-hash-table :test #'equal)))
	(format t "Loading item-to-fragment table...")
	(dolist (line lines)
	  (destructuring-bind (item ckb)
	      (split " " line)
	    (setf (gethash item item-to-ckb-table) ckb)
	    (pushnew item (gethash ckb ckb-to-items-table) 
		     :test #'string=)))
	(format t "done~%")
	(values item-to-ckb-table ckb-to-items-table))
      (error "The item-to-fragment table doesn't exist at the expected location '~a'" *item-to-ckb-table*)))

(defun load-dependency-graphs (&optional force)

  ;; fragment dependencies
  (when (or force
	    (null *ckb-dependency-graph-forward*)
	    (null *ckb-dependency-graph-backward*))
    (multiple-value-setq (*ckb-dependency-graph-forward*
			  *ckb-dependency-graph-backward*
			  *all-ckb-items*)
      (load-fragment-depgraph)))

  ;; items-to-fragments
  (when (or force
	    (null *item-to-ckb-table*)
	    (null *ckb-to-items-table*))
    (multiple-value-setq (*item-to-ckb-table* *ckb-to-items-table*)
      (load-item-to-fragment-table)))

  ;; if the item dependency graph doesn't exist, make it and write it
  ;; to disk, no matter the value of FORCE
  (let ((item-depgraph-path (mizar-items-config 'full-item-dependency-graph)))
    (unless (file-exists-p item-depgraph-path)
      (write-full-item-dependency-graph)))

  ;; if the full vertex-neighbors dependency graph doesn't exist, make
  ;; it and write it to disk, no matter the value of FORCE
  (let ((forward-path (mizar-items-config 'vertex-neighbors-forward-graph-path))
	(backward-path (mizar-items-config 'vertex-neighbors-backward-graph-path)))
    (when (or (not (file-exists-p forward-path))
	      (not (file-exists-p backward-path)))
      (write-vertex-neighbors-dependency-graphs)))

  (when (or force
	    (null *item-dependency-graph-forward*)
	    (null *item-dependency-graph-backward*))
    (multiple-value-setq (*item-dependency-graph-forward*
			  *item-dependency-graph-backward*)
      (load-full-item-dependency-graphs)))

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
