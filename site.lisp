
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server and application setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *itemization-source*
  "/Users/alama/sources/mizar/mizar-items/itemization")

(defparameter *articles* (list "xboole_0"
			       "boole" 
			       "xboole_1"
			       "enumset1"
			       "zfmisc_1"
			       "subset_1"
			       "subset"
			       "setfam_1"
			       "relat_1"
			       "funct_1"))

(defparameter *dependency-graph-file* 
  "/Users/alama/sources/mizar/mizar-items/depgraph")

(defparameter *dependency-graph* nil)

(defparameter *dependency-graph-forward* nil)

(defparameter *dependency-graph-backward* nil)

(defun load-dependency-graph ()
  (let ((lines (lines-of-file *dependency-graph-file*))
	(edges nil)
	(forward-table (make-hash-table :test #'equal))
	(backward-table (make-hash-table :test #'equal)))
    (dolist (line lines)
      (destructuring-bind (lhs rhs)
	  (split " " line)
	(push (cons lhs rhs) edges)
	(push rhs (gethash lhs forward-table))
	(push lhs (gethash rhs backward-table))))
    (setf *dependency-graph* edges
	  *dependency-graph-forward* forward-table
	  *dependency-graph-backward* backward-table)
    t))

(defun count-miz-in-directory (dir)
  (let ((counter 0))
    (walk-directory dir #'(lambda (foo)
			    (declare (ignore foo))
			    (incf counter))
		    :test #'(lambda (path)
			      (scan "ckb[0-9]*\.miz" (namestring path))))
    counter))

(defun load-article-num-items ()
  (let ((num-items-table (make-hash-table :test #'equal)))
    (dolist (article-name *articles* num-items-table)
      (let ((article-dir (concat *itemization-source* "/" article-name "/" "text")))
	(setf (gethash article-name num-items-table)
	      (count-miz-in-directory article-dir))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *acceptor* (make-instance 'hunchentoot:acceptor :port 4242))

(defun start-server ()
  (hunchentoot:start *acceptor*)
  (setf *message-log-pathname* "/tmp/hunchentoot-messages")
  (setf *access-log-pathname* "/tmp/hunchentoot-access")
  t)

;; set up articles

(defun initialize-uris ()
  (dolist (article *articles* t)
    (let* ((article-dir (format nil "~a/~a" *itemization-source* article))
	   (miz-uri (format nil "/~a.miz" article))
	   (miz-path (format nil "~a/~a.miz" article-dir article))
	   (html-uri (format nil "/~a.html" article))
	   (html-path (format nil "~a/~a.html" article-dir article)))
      ;; static files for the whole article
      (push (create-static-file-dispatcher-and-handler miz-uri 
						       miz-path
						       "text/plain")
	    *dispatch-table*)
      (push (create-static-file-dispatcher-and-handler html-uri
						       html-path
						       "text/html")
	    *dispatch-table*)
      ;; items for the article
      (loop
	 with article-text-dir = (format nil "~a/text" article-dir)
	 with num-items = (count-miz-in-directory article-text-dir)
	 for i from 1 upto num-items
	 for item-uri = (format nil "/~a/~d" article i)
	 for item-path = (format nil "~a/ckb~d.html" article-text-dir i)
	 initially
	   (flet ((emit-article-page ()
		    (with-html-output-to-string (*standard-output* nil 
								   :prologue t
								   :indent t)
		      (:html
		       (:title (format nil "~a" article))
		       (:body
			(:p "The article " article " has " (:b (str num-items)) " items ")
			(:p "See " (:a :href (format nil "/~a.html" article) "an HTMLized presentation of the whole article") ", or " (:a :href (format nil "/~a.miz" article) "its raw source") "."))))))
	     (push (create-regex-dispatcher
		    (format nil "^/~a$|^/~a/$" article article)
		    #'emit-article-page)
		   *dispatch-table*))
	 do
	   (unless (file-exists-p item-path)
	     (error "Can't register URI '~a' to point to '~a', because there's no file at that path" item-uri item-path))
	   (let* ((item-html (file-as-string item-path))
		  (item-name (format nil "~a:~d" article i))
		  (forward-deps (gethash item-name *dependency-graph-forward*))
		  (backward-deps (gethash item-name *dependency-graph-backward*)))
	     (flet ((emit-dependency-page ()
		      (with-html-output-to-string (*standard-output* nil
								     :prologue t
								     :indent t)
			(:html
			 (:title (str item-name))
			 (:body
			  (:table
			   (:tr
			    (:td :rowspan 2 (str item-html))
			    (:td "This item depends on:"
				 (:ul
				  (dolist (forward-dep forward-deps)
				    (htm
				     (:li (str forward-dep)))))))
			   (:tr
			    (:td "These items depend on this one:"
				 (:ul
				  (dolist (backward-dep backward-deps)
				    (htm
				     (:li (str backward-dep)))))))))))))
	       (push (create-regex-dispatcher 
		      (format nil "/~a/~d" article i)
		      #'emit-dependency-page)
		     *dispatch-table*)))))))