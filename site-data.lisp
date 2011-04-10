
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mml-version* nil)
(defvar *item-to-item-dependency-graph* nil)
(defvar *ckb-dependency-graph-forward* nil)
(defvar *ckb-dependency-graph-backward* nil)
(defvar *item-dependency-graph-forward* nil)
(defvar *item-dependency-graph-backward* nil)
(defvar *item-to-ckb-table* nil)
(defvar *ckb-to-items-table* nil)
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

(defun load-item-to-item-depgraph ()
  (let (edges)
    (flet ((register-deps (item deps)
	     (dolist (dep deps)
	       (push (cons item dep) edges))))
      (maphash #'(lambda (item ckb)
		   (let* ((dependent-ckbs (gethash ckb *ckb-dependency-graph-forward*))
			  (dependent-items (reduce #'append (mapcar #'(lambda (dep-ckb)
									(gethash dep-ckb *ckb-to-items-table*))
								    dependent-ckbs))))
		     (register-deps item dependent-items)))
	       *item-to-ckb-table*))
    edges))

(defun dependency-tables-from-edge-list ()
  (format t "Computing the vertex-neighbors dependency graph...")
  (let ((forward-table (make-hash-table :test #'equal))
	(backward-table (make-hash-table :test #'equal)))
    (loop
       for (lhs . rhs) in *item-to-item-dependency-graph*
       do
	 (pushnew rhs (gethash lhs forward-table) :test #'string=)
	 (pushnew lhs (gethash rhs backward-table) :test #'string=)
       finally
	 (format t "done~%")
	 (return (values forward-table backward-table)))))

(defun write-vertex-neighbors-dependency-graphs ()
  (format t "Writing the vertex-neighbors dependency graphs...")
  (with-open-file (vertex-neighbors
		   (mizar-items-config 'vertex-neighbors-forward-graph-path)
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (flet ((write-vertex-and-neighbors (vertex neighbors)
		   (format vertex-neighbors "~a ~{~a~^ ~}~%" vertex neighbors)))
	    (maphash #'write-vertex-and-neighbors
		     *item-dependency-graph-forward*)))
	(with-open-file (vertex-neighbors
			 (mizar-items-config 'vertex-neighbors-backward-graph-path)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	  (flet ((write-vertex-and-neighbors (vertex neighbors)
		   (format vertex-neighbors "~a ~{~a~^ ~}~%" vertex neighbors)))
	    (maphash #'write-vertex-and-neighbors
		     *item-dependency-graph-backward*)))
	(format t "done~%"))

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
  (if (file-exists-p (mizar-items-config 'item-to-fragment-path))
      (let ((lines (lines-of-file (mizar-items-config 'item-to-fragment-path)))
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

  ;; the fragment dependencies and the item-to-fragment table are the
  ;; two pieces of information that comes from outside lisp.  The rest
  ;; of the data is computed form these.  But always check first to
  ;; see whether we've already done the computation.

  ;; if the item dependency graph doesn't exist, make it and write it
  ;; to disk, no matter the value of FORCE
  (let ((item-depgraph-path (mizar-items-config 'full-item-dependency-graph)))
    (if (file-exists-p item-depgraph-path)
	(format t "The item-to-item dependency graph already exists; not recomputing it.~%")
	(write-full-item-dependency-graph)))

  ;; now load the full item dependency graph (which is just a list of
  ;; edges)
  (when (or force (null *item-to-item-dependency-graph*)) 
    (setf *item-to-item-dependency-graph* (load-item-to-item-depgraph)))

  ;; if the full vertex-neighbors dependency graph doesn't exist, make
  ;; it and write it to disk, no matter the value of FORCE
  (let ((forward-path (mizar-items-config 'vertex-neighbors-forward-graph-path))
	(backward-path (mizar-items-config 'vertex-neighbors-backward-graph-path)))
    (if (or (not (file-exists-p forward-path))
	    (not (file-exists-p backward-path)))
	(progn
	  (multiple-value-setq (*item-dependency-graph-forward*
				*item-dependency-graph-backward*)
	    (dependency-tables-from-edge-list))
	  (write-vertex-neighbors-dependency-graphs))
	(format t "The item-to-items dependency graphs already exist; not recomputing them.~%")))

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
       for article-name in *mml-lar*
       do
	 (let ((article-dir (concat (mizar-items-config 'itemization-source) "/" article-name "/" "text")))
	   (if (file-exists-p article-dir)
	       (setf (gethash article-name num-items-table)
		     (count-miz-in-directory article-dir))
	       
	       (progn
		 (warn "When counting the number of fragments for '~a', we expected to find a directory at '~a', but it doesn't exist; setting the number of items for this article to 0" article-name article-dir)
		 (setf (gethash article-name num-items-table) 0))))
       finally
	 (setf *article-num-items* num-items-table)
	 (return *article-num-items*))
    *article-num-items*))

(defun load-mml (mml-version)
  (let ((data-path-lisp (file-in-data-dir (format nil "~a.lisp" mml-version)))
	(data-path-lisp-xz (file-in-data-dir (format nil "~a.lisp.xz" mml-version)))
	(data-path-fasl (file-in-data-dir (format nil "~a.fasl" mml-version))))
    (when (file-exists-p data-path-lisp-xz)
      (when (or (and (file-exists-p data-path-lisp)
		     (< (file-write-date data-path-lisp)
			(file-write-date data-path-lisp-xz)))
		(not (file-exists-p data-path-lisp)))
	(format t "Decompressing data at '~a'..." data-path-lisp-xz)
	(let* ((xz-truename (namestring (truename data-path-lisp-xz)))
	       (process (sb-ext:run-program "unxz" (list xz-truename) :search t)))
	  (unless (zerop (sb-ext:process-exit-code process))
	    (error "There was a problem decompressing the compressed data!")))
	(format t "done~%")))
    (unless (file-exists-p data-path-lisp)
      (error "Cannot load data for MML version ~a because there is no
      data file at the expected location '~a'" mml-version data-path-lisp))
    (when (or (not (file-exists-p data-path-fasl))
	      (< (file-write-date data-path-fasl)
		 (file-write-date data-path-lisp)))
      (unless (compile-file data-path-lisp)
	(error "Something went wrong compiling the data file for MML version ~a at '~a'" mml-version data-path-lisp)))
    (unless (file-exists-p data-path-fasl)
      (error "Althought we just compiled the data for MML version ~a, there is no FASL file at the expected location '~a'" mml-version data-path-fasl))
    (format t "Loading data for MML version ~a..." mml-version)
    (load data-path-fasl)
    (format t "done~%")
    (when (or (null *item-dependency-graph-backward*)
	      (null *item-dependency-graph-forward*))
      (error "We loaded the dependency data for MML version ~a at '~a', but the dependency graphs do not have initialized values" mml-version data-path-fasl))
    ;; accumulate all items
    (loop
       initially
	 (setf *all-items* (make-hash-table :test #'equal))
       for k being the hash-key in *item-dependency-graph-forward*
       for vals being the hash-value in *item-dependency-graph-forward*
       do
	 (setf (gethash k *all-items*) t)
	 (dolist (val vals)
	   (setf (gethash val *all-items*) t)))))

(defun count-dependency-graph-edges ()
  (count-hash-table-keys *item-dependency-graph-backward*))

(defun items-for-article (article)
  (loop
     with items = nil
     for k being the hash-keys of *item-dependency-graph-forward*
     for (key-article key-kind key-number) = (split ":" k)
     do
       (when (string= article key-article)
	 (pushnew k items :test #'string=))
     finally
       (return items)))

(defun items-of-kind-for-article (article kind)
  (loop
     with items = nil
     for k being the hash-keys of *item-dependency-graph-forward*
     for (key-article key-kind key-number) = (split ":" k)
     do
       (when (and (string= article key-article)
		  (string= kind key-kind))
	 (pushnew k items :test #'string=))
     finally
       (return items)))

(defun article-title (article)
  (let ((present (member article *articles* :key #'first :test #'string=)))
    (when present
      (second (car present)))))

(defun article-author (article)
  (let ((present (member article *articles* :key #'first :test #'string=)))
    (when present
      (third (car present)))))