
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mml-version* nil)
(defvar *item-to-item-dependency-graph* nil)
(defvar *item-to-fragment-table* nil)
(defvar *ckb-dependency-graph-forward* nil)
(defvar *ckb-dependency-graph-backward* nil)
(defvar *item-dependency-graph-forward* nil)
(defvar *item-dependency-graph-backward* nil)

(defgeneric known-item? (item-identifier))

(defmethod known-item? ((item symbol))
  (multiple-value-bind (val present)
      (gethash item *item-dependency-graph-forward*)
    (declare (ignore val))
    present))

(defmethod known-item? ((item string))
  (known-item? (get-and-maybe-set-item-name item)))

(defun count-miz-in-directory (dir)
  (let ((counter 0))
    (walk-directory dir #'(lambda (foo)
			    (declare (ignore foo))
			    (incf counter))
		    :test #'(lambda (path)
			      (scan "ckb[0-9]+\.miz$" (namestring path))))
    counter))

(defun count-dependency-graph-edges ()
  (hash-table-count *item-dependency-graph-forward*))

(defun items-for-article (article)
  (loop
     with items = nil
     for k being the hash-keys of *item-dependency-graph-forward*
     for key-article = (item-article k)
     for key-kind = (item-kind k)
     for key-number = (item-number k)
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

(defun count-items-of-kind-for-article (article kind)
  (loop
     with kind-regexp = (format nil "~a:~a:[0-9]+" article kind)
     for k being the hash-keys of *item-dependency-graph-forward*
     counting (scan kind-regexp k) into num-items
     finally (return num-items)))

(defun article-title (article)
  (let ((present (member article *articles* :key #'first :test #'string=)))
    (when present
      (second (car present)))))

(defun article-author (article)
  (let ((present (member article *articles* :key #'first :test #'string=)))
    (when present
      (third (car present)))))

(defun count-items ()
  (hash-table-count *item-dependency-graph-forward*))

(defvar *fragment-to-item-table* (make-hash-table :test #'equal))

(defun invert-item-to-fragment-table (item-to-fragment-table)
  (loop
     with table = (make-hash-table :test #'equal)
     for item being the hash-keys in item-to-fragment-table using (hash-value fragment-number)
     for article = (item-article item)
     do
       (push item (gethash (cons article fragment-number) table))
     finally
       (return table))) 

(defgeneric earlier-items-from-same-article (item))

(defmethod earlier-items-from-same-article ((item string))
  (earlier-items-from-same-article (get-and-maybe-set-item-name item)))

(defmethod earlier-items-from-same-article :around ((item symbol))
  (if (gethash item *item-to-fragment-table*)
      (call-next-method)
      (error "The item '~a' doesn't appear in the item-to-fragment table!" (symbol-name item))))

(defmethod earlier-items-from-same-article ((item symbol))
  "A list of all items that appear earlier than ITEM that belong to
  the same article as ITEM"
  (let ((article (item-article item))
	(fragment-number (gethash item *item-to-fragment-table*)))
    (loop
       for i from (1- fragment-number) downto 1
       appending (gethash (cons article i) *fragment-to-item-table*) into earlier
       finally (return earlier))))

(defvar *mml-version* nil
  "The version of the MML from which our items come.")

(defun initialize-items-data (mml-version)
  (setf *mml-version* mml-version)
  (format t "Loading the dependency data...")
  (setf *item-dependency-graph-forward*
	(read-dependency-file (item-dependency-table-for-mml mml-version)))
  (format t "done.~%")
  (format t "Inverting the dependency table...")
  (setf *item-dependency-graph-backward*
	(invert-dependency-table *item-dependency-graph-forward*))
  (format t "done.~%")
  (format t "Loading the item-to-fragment table...")
  (setf *item-to-fragment-table* (load-item-to-fragment-table mml-version))
  (format t "done.~%")
  (format t "Inverting the item-to-fragment table...")
  (setf *fragment-to-item-table*
	(invert-item-to-fragment-table *item-to-fragment-table*))
  (format t "done.~%")
  (format t "Loading the MPTP axiom file...")
  (setf *mptp-table* (load-mptp-axioms-for-mml mml-version))
  (format t "done.~%")
  t)