
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server and application setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-mizar-favicon-and-title (title &body body)
  `(with-favicon-and-title "/favicon.ico" ,title ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *itemization-source*
  "/Users/alama/sources/mizar/mizar-items/itemization")

(defparameter *articles* (list "hidden"
			       "tarski"
			       "xboole_0"
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
  "/Users/alama/sources/mizar/mizar-items/ckb-ckb-depgraph")
(defparameter *item-to-ckb-file*
  "/Users/alama/sources/mizar/mizar-items/mizar-item-ckb-table")

(defparameter *dependency-graph* nil)
(defparameter *num-dependency-graph-edges* nil)
(defparameter *ckb-dependency-graph-forward* nil)
(defparameter *ckb-dependency-graph-backward* nil)
(defparameter *true-item-dependency-graph-forward* nil)
(defparameter *true-item-dependency-graph-backward* nil)
(defparameter *item-to-ckb-table* nil)
(defparameter *ckb-to-items-table* nil)

(defun load-dependency-graph ()
  ;; ckb graph
  (let ((lines (lines-of-file *dependency-graph-file*))
	(edges nil)
	(ckb-forward-table (make-hash-table :test #'equal))
	(ckb-backward-table (make-hash-table :test #'equal)))
    (dolist (line lines)
      (destructuring-bind (lhs rhs)
	  (split " " line)
	(push (cons lhs rhs) edges)
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
	(setf (gethash item item-to-ckb-table) ckb)
	(pushnew item (gethash ckb ckb-to-items-table) :test #'string=)))
    (setf *item-to-ckb-table* item-to-ckb-table
	  *ckb-to-items-table* ckb-to-items-table))
  (let 	((true-item-forward-table (make-hash-table :test #'equal))
	 (true-item-backward-table (make-hash-table :test #'equal)))
    (loop
       for item being the hash-keys of *item-to-ckb-table*
       for ckb = (gethash item *item-to-ckb-table*)
       for forward-ckb-deps = (gethash ckb *ckb-dependency-graph-forward*)
       for backward-ckb-deps = (gethash ckb *ckb-dependency-graph-backward*)
       do
	 (let ((forward-item-deps 
		(reduce #'append (mapcar #'(lambda (ckb-dep)
					     (gethash ckb-dep *ckb-to-items-table*))
					 forward-ckb-deps))))
	   (setf (gethash item true-item-forward-table)
		 forward-item-deps))
	 (let ((backward-item-deps 
		(reduce #'append (mapcar #'(lambda (ckb-dep)
					     (gethash ckb-dep *ckb-to-items-table*))
					 backward-ckb-deps))))
	   (setf (gethash item true-item-backward-table)
		 backward-item-deps)))
    (setf *true-item-dependency-graph-forward* true-item-forward-table
	  *true-item-dependency-graph-backward* true-item-backward-table))
  t)

(defun count-miz-in-directory (dir)
  (let ((counter 0))
    (walk-directory dir #'(lambda (foo)
			    (declare (ignore foo))
			    (incf counter))
		    :test #'(lambda (path)
			      (scan "ckb[0-9]+\.miz$" (namestring path))))
    counter))

(defparameter *article-num-items* nil)

(defun load-article-num-items ()
  (let ((num-items-table (make-hash-table :test #'equal)))
    (dolist (article-name *articles* (setf *article-num-items* num-items-table))
      (let ((article-dir (concat *itemization-source* "/" article-name "/" "text")))
	(setf (gethash article-name num-items-table)
	      (count-miz-in-directory article-dir))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching for paths between items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (item-search-problem (:include problem)))

(defmethod successors ((isp item-search-problem) node)
  (mapcar #'(lambda (item)
	      (cons item item))
	  (gethash (node-state node) *ckb-dependency-graph-forward*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar items-dispatch-table nil)

(defun items-request-dispatcher (request)
  "Selects a request handler based on a list of individual request
dispatchers all of which can either return a handler or neglect by
returning NIL."
  (loop for dispatcher in items-dispatch-table
        for action = (funcall dispatcher request)
        when action return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

(defvar *acceptor* (make-instance 'hunchentoot:acceptor 
				  :port 4242
				  :request-dispatcher #'items-request-dispatcher))

(defun start-server ()
  (hunchentoot:start *acceptor*)
  t)

(defun setup-server ()
  (load-article-num-items)
  (load-dependency-graph)
  (initialize-uris)
  (setf *message-log-pathname* "/tmp/hunchentoot-messages"
	*access-log-pathname* "/tmp/hunchentoot-access"
	*handle-http-errors-p* t
	*http-error-handler* #'handle-http-error
	*log-lisp-errors-p* t
	*log-lisp-warnings-p* t
	*log-lisp-backtraces-p* t)
  
  t)

(defun handle-http-error (error-code)
  (when (= error-code +http-not-found+)
    (with-mizar-favicon-and-title "No"
      (:p "I still haven't found what you're looking for."))))

(setq *http-error-handler* #'handle-http-error)

;; set up articles

(defun ckb-item-< (item-name-1 item-name-2)
  (destructuring-bind (item-article-name-1 item-num-as-str-1)
      (split ":" item-name-1)
    (destructuring-bind (item-article-name-2 item-num-as-str-2)
	(split ":" item-name-2)
      (or (string< item-article-name-1 item-article-name-2)
	  (let ((item-num-1 (parse-integer item-num-as-str-1))
		(item-num-2 (parse-integer item-num-as-str-2)))
	    (and (string= item-article-name-1 item-article-name-2)
		 (< item-num-1 item-num-2)))))))

(defun true-item-< (item-name-1 item-name-2)
  (destructuring-bind (item-article-name-1 item-kind-1 item-num-as-str-1)
      (split ":" item-name-1)
    (destructuring-bind (item-article-name-2 item-kind-2 item-num-as-str-2)
	(split ":" item-name-2)
      (or (string< item-article-name-1 item-article-name-2)
	  (when (string= item-article-name-1 item-article-name-2)
	    (or (string< item-kind-1 item-kind-2)
		(when (string= item-kind-1 item-kind-2)
		  (let ((item-num-1 (parse-integer item-num-as-str-1))
			(item-num-2 (parse-integer item-num-as-str-2)))
		    (< item-num-1 item-num-2)))))))))

(defun emit-about-page ()
  (with-mizar-favicon-and-title "fine-grained dependencies in the mizar mathematical library"
    (:p "This site aims to illustrate fine-grained dependency
    information about " (:a :href "http://www.mizar.org" "the Mizar
    Mathematical Library") ", a large collection of formalized mathematical knowledge.")
    (:p "For more information about the method for computing this
    information and some applications of it, see:")
    (:ul
     (:li (str "&ldquo;") (:a :href "http://centria.di.fct.unl.pt/~alama/materials/preprints/dependencies-in-formal-mathematics.pdf" "Dependencies in formal mathematics") (str "&rdquo;"))
     (:li (str "&ldquo;") (:a :href "http://centria.di.fct.unl.pt/~alama/materials/preprints/premise-selection-for-mathematics-by-corpus-analysis-and-kernel-methods.pdf" "Premise selection for mathematics by corpus analysis and kernel methods") (str "&rdquo;")))
    (:hr)
    (:address (:a :href "mailto:j.alama@fct.unl.pt" "Contact the maintainer"))))

(defun emit-path-between-items ()
  (let ((uri (request-uri*)))
    (register-groups-bind (article-1 num-1 article-2 num-2)
	("^/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/?$" uri)
      (let* ((ai (format nil "~a:~a" article-1 num-1))
	     (bj (format nil "~a:~a" article-2 num-2))
	     (ai-to-bj-problem (make-item-search-problem 
				:initial-state ai
				:goal bj))
	     (solution (depth-first-search ai-to-bj-problem)))
	(with-mizar-favicon-and-title (fmt "from ~a to ~a" ai bj)
	  (if solution
	      (htm
	       (:p (fmt "Here is a path from ~a to ~a" ai bj))
	       (:ol
		(let ((ai-uri (format nil "/~a/~a" article-1 num-1)))
		  (htm
		   (:li ((:a :href ai-uri)
			 (str ai)))))
		(dolist (step (explain-solution solution))
		  (destructuring-bind (step-article step-num)
		      (split ":" step)
		    (let ((step-uri (format nil "/~a/~a" step-article step-num)))
		      (htm
		       (:li ((:a :href step-uri)
			     (str step)))))))))
	      (htm
	       (:p (fmt "There is no path from ~a to ~a." ai bj)
		   " Care to " ((:a :href (fmt "/~a/~a/~a/~a"
					       article-2 num-2
					       article-1 num-1))
				"search for a path going the other way")
		   "?"))))))))

(defun emit-path-between-items-via-item ()
  (let ((uri (request-uri*)))
    (register-groups-bind (source-article source-num 
			   via-article via-num
			   destination-article destination-num)
	("^/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/?$" uri)
      (let ((source (format nil "~a:~a" source-article source-num))
	    (via (format nil "~a:~a" via-article via-num))
	    (destination (format nil "~a:~a" destination-article destination-num)))	
	(let ((source-to-via-problem (make-item-search-problem 
				      :initial-state source
				      :goal via))
	      (via-to-destination-problem (make-item-search-problem
					   :initial-state via
					   :goal destination)))
	  (let ((solution-to-via (depth-first-search source-to-via-problem))
		(solution-to-destination (depth-first-search via-to-destination-problem)))
	    (with-mizar-favicon-and-title (fmt "from ~a to ~a via ~a" source destination via)
	      (if solution-to-via
		  (if solution-to-destination
		      (htm
		       (:p (fmt "Here is a path from ~a to ~a via ~a" source destination via))
		       (:ol
			(let ((source-uri (format nil "/~a/~a" source-article source-num)))
			  (htm
			   (:li (:b ((:a :href source-uri) (str source))))))
			(dolist (step (all-but-last (explain-solution solution-to-via)))
			  (destructuring-bind (step-article step-num)
			      (split ":" step)
			    (let ((step-uri (format nil "/~a/~a" step-article step-num)))
			      (htm
			       (:li ((:a :href step-uri)
				     (str step)))))))
			(let ((via-uri (format nil "/~a/~a" via-article via-num)))
			  (htm
			   (:li (:b ((:a :href via-uri)) (str via)))))
			(dolist (step (all-but-last (cdr (explain-solution solution-to-destination))))
			  (destructuring-bind (step-article step-num)
			      (split ":" step)
			    (let ((step-uri (format nil "/~a/~a" step-article step-num)))
			      (htm
			       (:li ((:a :href step-uri)
				     (str step)))))))
			(let ((destination-uri (format nil "/~a/~a" destination-article destination-num)))
			  (htm
			   (:li (:b ((:a :href destination-uri)) (str destination)))))))
		      (htm
		       (:p "There is a path from " (str source) " to " (str via) ", but there is no path from " (str via) " to " (str destination) ".")))
		  (if solution-to-destination
		      (htm
		       (:p "There is no path from " (str source) " to " (str via) ", but there is a path from " (str via) " to " (str destination) "."))
		      (htm
		       (:p "There is no path from " (str source) " to " (str via) ", nor is there is a path from " (str via) " to " (str destination) ".")))))))))))

(defun emit-article-page (article)
  (let ((num-items (gethash article *article-num-items*)))
    #'(lambda ()
	(with-mizar-favicon-and-title (fmt "~a" article)
	  (:p "The article " article " has " (:b (str num-items)) " items ")
	  (:p "See " (:a :href (format nil "/~a.html" article) "an HTMLized presentation of the whole article") ", or " (:a :href (format nil "/~a.miz" article) "its raw source") ".")))))

(defun emit-dependency-page (article item-number)
  (let* ((article-dir (format nil "~a/~a" *itemization-source* article))
	 (article-text-dir (format nil "~a/text" article-dir))
	 (item-path (format nil "~a/ckb~d.html" article-text-dir item-number))
	 (item-html (file-as-string item-path))
	 (item-name (format nil "~a:~d" article item-number))
	 (forward-deps (gethash item-name *ckb-dependency-graph-forward*))
	 (backward-deps (gethash item-name *ckb-dependency-graph-backward*))
	 (forward-deps-sorted (sort forward-deps #'ckb-item-<))
	 (backward-deps-sorted (sort backward-deps #'ckb-item-<)))
    #'(lambda ()
	(with-mizar-favicon-and-title (str item-name)
	  (:table
	   (:tr
	    (:td :rowspan 2 (str item-html))
	    (:td "This item immediately depends on:"
		 (if forward-deps-sorted
		     (htm
		      (:ul
		       (dolist (forward-dep forward-deps-sorted)
			 (destructuring-bind (dep-name dep-num)
			     (split ":" forward-dep)
			   (let ((dep-uri (format nil "/~a/~d" dep-name dep-num)))
			     (htm
			      (:li ((:a :href dep-uri)
				    (str forward-dep)))))))))
		     (htm (:p (:em "(none)"))))))
	   (:tr
	    (:td "These items immediately depend on this one:"
		 (if backward-deps-sorted
		     (htm
		      (:ul
		       (dolist (backward-dep backward-deps-sorted)
			 (destructuring-bind (dep-name dep-num)
			     (split ":" backward-dep)
			   (let ((dep-uri (format nil "/~a/~d" dep-name dep-num)))
			     (htm
			      (:li ((:a :href dep-uri)
				    (str backward-dep)))))))))
		     (htm (:p (:em "(none)")))))))))))

(defun emit-random-page ()
  (let* ((random-edge-index (random *num-dependency-graph-edges*))
	 (random-zero-one (random 2))
	 (random-edge (nth random-edge-index *dependency-graph*))
	 (random-vertex (if (zerop random-zero-one)
			    (car random-edge)
			    (cdr random-edge))))
    (destructuring-bind (random-article-name random-item-num)
	(split ":" random-vertex)
      (let ((random-uri (format nil "/~a/~a" 
				random-article-name random-item-num)))
	(redirect random-uri)))))

(defmacro register-static-file-dispatcher (uri path &optional mime-type)
  `(progn
     (unless (file-exists-p ,path)
       (error "Can't register URI '~a' to point to '~a', because there's no file at that path" ,uri ,path))
     (push (create-static-file-dispatcher-and-handler ,uri 
						      ,path
						      ,mime-type)
	   items-dispatch-table)
     t))

(defmacro register-directory-dispatcher (uri path &optional mime-type)
  `(progn
     (unless (file-exists-p ,path)
       (error "Can't register URI '~a' to point to '~a', because there's no file at that path" ,uri ,path))
     (unless (directory-p ,path)
       (error "Can't register URI '~a' to point to directory '~a', because '~a' isn't a directory" ,uri ,path ,path))
     (unless (scan "/$" ,uri)
       (error "Can't register URI '~a' to point to directory '~a', because '~a'  doesn't end with a slash '/'" ,uri ,path ,uri))
     (push (create-folder-dispatcher-and-handler ,uri ,path ,mime-type)
	   items-dispatch-table)
     t))

(defmacro register-regexp-dispatcher (uri-regexp dispatcher)
  `(progn
     (push
      (create-regex-dispatcher ,uri-regexp ,dispatcher)
      items-dispatch-table)
     t))

(defmacro register-exact-uri-dispatcher (uri dispatcher)
  (let ((exact-uri (concatenate 'string "^" uri "$")))
    `(register-regexp-dispatcher ,exact-uri ,dispatcher)))

(defun emit-mizar-item-page ()
  (let ((request-uri (request-uri*))
	(mizar-item-regexp "^/([a-z_0-9]+)/([a-z]+)/([0-9]+)$"))
    (register-groups-bind (article-name item-kind item-number)
        (mizar-item-regexp request-uri)
      (let* ((item-key (format nil "~a:~a:~a" article-name item-kind item-number))
	     (ckb-for-item (gethash item-key *item-to-ckb-table*))
	     (article-dir (format nil "~a/~a" *itemization-source* article-name))
	     (article-text-dir (format nil "~a/text" article-dir))
	     (items-for-ckb (gethash ckb-for-item *ckb-to-items-table*))
	     (forward-deps (gethash item-key *true-item-dependency-graph-forward*))
	     (backward-deps (gethash item-key *true-item-dependency-graph-backward*))
	     (forward-deps-sorted (sort forward-deps #'true-item-<))
	     (backward-deps-sorted (sort backward-deps #'true-item-<)))
	(destructuring-bind (ckb-article-name ckb-number)
	    (split ":" ckb-for-item)
	  (declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	  (let* ((ckb-item-path (format nil "~a/ckb~d.html"
				   article-text-dir
				   ckb-number))
		 (item-html (file-as-string ckb-item-path)))
	(with-mizar-favicon-and-title (str item-key)
	  (:p (str item-key) " is toplevel item " (str ckb-number) " of article " (str article-name) ".")
	  (if (null (cdr items-for-ckb)) ; the CKB for this item generates only this item
	      (htm 
	       (:p "This is the only item generated by that toplevel item."))
	      
	      (htm
	       (:p "This toplevel item generates this item, as well as these other items:")
	       (:ul
		(dolist (other-item items-for-ckb)
		  (destructuring-bind (other-item-article other-item-kind other-item-number)
		      (split ":" other-item)
		    (let ((other-item-uri (format nil "/~a/~a/~a" other-item-article other-item-kind other-item-number)))
		      (htm
		       (:li ((:a :href other-item-uri) (str other-item))))))))))
	  (:table
	   ((:tr :valign "top")
	    (:td 
	     (if forward-deps-sorted
		 (htm
		  (:table
		   (:caption "This item immediately depends on")
		   (dolist (forward-dep forward-deps-sorted)
		    (destructuring-bind (dep-name dep-kind dep-num)
			(split ":" forward-dep)
		      (let ((dep-uri (format nil "/~a/~a/~a" dep-name dep-kind dep-num)))
			(htm
			 (:tr (:td ((:a :href dep-uri) (str forward-dep))))))))))
		 (htm (:p (:em "(This item immediately depends on nothing.)")))))
	    (:td (str "&lArr;"))
	    (:td :rowspan 2 (str item-html))
	    (:td (str "&rArr;"))
	    (:td 
	     (if backward-deps-sorted
		 (htm
		  (:table
		   (:caption "These items immediately depend on this one:")
		   (dolist (backward-dep backward-deps-sorted)
		     (destructuring-bind (dep-name dep-kind dep-num)
			 (split ":" backward-dep)
		       (let ((dep-uri (format nil "/~a/~a/~a" dep-name dep-kind dep-num)))
			 (htm
			  (:tr (:td ((:a :href dep-uri) (str backward-dep))))))))))
		 (htm (:p (:em "(No item immediately depends on this one.)"))))))))))))))
	  

(defun initialize-uris ()
  ;; about page
  (register-exact-uri-dispatcher "/about" #'emit-about-page)
  (register-exact-uri-dispatcher "/random" #'emit-random-page)
  (register-static-file-dispatcher "/favicon.ico" "/Users/alama/sources/mizar/mizar-items/mizar.ico")
  ;; directory setup
  (push 'hunchentoot-dir-lister:dispatch-dir-listers items-dispatch-table)
  (dolist (article *articles*)
    (let* ((article-dir (format nil "~a/~a" *itemization-source* article))
	   (miz-uri (format nil "/~a.miz" article))
	   (miz-path (format nil "~a/~a.miz" article-dir article))
	   (html-uri (format nil "/~a.html" article))
	   (html-path (format nil "~a/~a.html" article-dir article))
	   (prel-dir-uri (format nil "/~a/prel/" article))
	   (prel-dir-path (format nil "~a/prel/" article-dir))
	   (text-dir-uri (format nil "/~a/text" article))
	   (text-dir-path (format nil "~a/text/" article-dir)))
      ;; static files for the whole article
      (register-static-file-dispatcher miz-uri miz-path "text/plain")
      (register-static-file-dispatcher html-uri html-path "text/html")
      (hunchentoot-dir-lister:add-simple-lister prel-dir-uri prel-dir-path)
      (hunchentoot-dir-lister:add-simple-lister text-dir-uri text-dir-path)
      ;; CKB items for the article
      (loop
	 with num-items = (gethash article *article-num-items*)
	 for i from 1 upto num-items
	 initially
	   (register-regexp-dispatcher 
	    (format nil "^/~a$|^/~a/$" article article)
	    (emit-article-page article))
	 do
	   (register-regexp-dispatcher
	    (format nil "^/~a/~d$" article i)
	    (emit-dependency-page article i)))
      ;; "true" items
      (register-regexp-dispatcher
       (format nil "^/~a/[a-z]+/[0-9]+$" article)
       #'emit-mizar-item-page)))
  ;; set up path searcher
  (let ((ai-to-bj-uri-regex "^/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/?$"))
    (register-regexp-dispatcher ai-to-bj-uri-regex #'emit-path-between-items))
  (let ((source-via-dest-regex "^/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/?$"))
    (register-regexp-dispatcher source-via-dest-regex #'emit-path-between-items-via-item))
    t)
