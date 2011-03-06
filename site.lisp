
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server and application setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "/Users/alama/sources/mizar/mizar-items/depgraph")

(defparameter *dependency-graph* nil)
(defparameter *num-dependency-graph-edges* nil)

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
	(pushnew rhs (gethash lhs forward-table) :test #'string=)
	(pushnew lhs (gethash rhs backward-table) :test #'string=)))
    (setf *dependency-graph* edges
	  *num-dependency-graph-edges* (length edges)
	  *dependency-graph-forward* forward-table
	  *dependency-graph-backward* backward-table)
    t))

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
	  (gethash (node-state node) *dependency-graph-forward*)))

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
    (with-title "No"
      (:p "I still haven't found what you're looking for."))))

(setq *http-error-handler* #'handle-http-error)

;; set up articles

(defun item-< (item-name-1 item-name-2)
  (destructuring-bind (item-article-name-1 item-num-as-str-1)
      (split ":" item-name-1)
    (destructuring-bind (item-article-name-2 item-num-as-str-2)
	(split ":" item-name-2)
      (or (string< item-article-name-1 item-article-name-2)
	  (let ((item-num-1 (parse-integer item-num-as-str-1))
		(item-num-2 (parse-integer item-num-as-str-2)))
	    (and (string= item-article-name-1 item-article-name-2)
		 (< item-num-1 item-num-2)))))))

(defun emit-about-page ()
  (with-title "fine-grained dependencies in the mizar mathematical library"
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
      (let ((ai (format nil "~a:~a" article-1 num-1))
	    (bj (format nil "~a:~a" article-2 num-2))) 
	(let ((ai-to-bj-problem (make-item-search-problem 
				 :initial-state ai
				 :goal bj)))
	  (let ((solution (depth-first-search ai-to-bj-problem)))
	    (if solution
		(with-html-output-to-string (*standard-output* nil
							       :prologue t
							       :indent t)
		  (with-title (fmt "from ~a to ~a" ai bj)
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
				  (str step))))))))))
		(with-html-output-to-string (*standard-output* nil
							       :prologue t
							       :indent t)
		  (with-title (fmt "from ~a to ~a" ai bj)
		    (:p (fmt "There is no path from ~a to ~a." ai bj)
			" Care to " ((:a :href (fmt "/~a/~a/~a/~a"
						    article-2 num-2
						    article-1 num-1))
				     "search for a path going the other way")
			"?"))))))))))

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
		(solution-to-destination (depth-first-search via-to-destination-problem))
		(title (format nil "from ~a to ~a via ~a" source destination via)))
	    (if solution-to-via
		(if solution-to-destination
		    (with-title (str title)
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
		    (with-title (str title)
		      (:p "There is a path from " (str source) " to " (str via) ", but there is no path from " (str via) " to " (str destination) ".")))
		(if solution-to-destination
		    (with-title (str title)
		      (:p "There is no path from " (str source) " to " (str via) ", but there is a path from " (str via) " to " (str destination) "."))
		    (with-title (str title)
		      (:p "There is no path from " (str source) " to " (str via) ", nor is there is a path from " (str via) " to " (str destination) "."))))))))))

(defun emit-article-page (article)
  (let ((num-items (gethash article *article-num-items*)))
    #'(lambda ()
	(with-html-output-to-string (*standard-output* nil 
						       :prologue t
						       :indent t)
	  (with-title (fmt "~a" article)
	    (:p "The article " article " has " (:b (str num-items)) " items ")
	    (:p "See " (:a :href (format nil "/~a.html" article) "an HTMLized presentation of the whole article") ", or " (:a :href (format nil "/~a.miz" article) "its raw source") "."))))))

(defun emit-dependency-page (article item-number)
  (let* ((article-dir (format nil "~a/~a" *itemization-source* article))
	 (article-text-dir (format nil "~a/text" article-dir))
	 (item-path (format nil "~a/ckb~d.html" article-text-dir item-number))
	 (item-html (file-as-string item-path))
	 (item-name (format nil "~a:~d" article item-number))
	 (forward-deps (gethash item-name *dependency-graph-forward*))
	 (backward-deps (gethash item-name *dependency-graph-backward*))
	 (forward-deps-sorted (sort forward-deps #'item-<))
	 (backward-deps-sorted (sort backward-deps #'item-<)))
    #'(lambda ()
	(with-html-output-to-string (*standard-output* nil
						       :prologue t
						       :indent t)
	  (with-title (str item-name)
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
		       (htm (:p (:em "(none)"))))))))))))

(defmacro register-static-file-dispatcher (uri path &optional mime-type)
  `(progn
     (unless (file-exists-p ,path)
       (error "Can't register URI '~a' to point to '~a', because there's no file at that path" ,uri ,path))
     (push (create-static-file-dispatcher-and-handler ,uri 
						      ,path
						      ,mime-type)
	   items-dispatch-table)))

(defmacro register-regexp-dispatcher (uri-regexp dispatcher)
  `(push
    (create-regex-dispatcher ,uri-regexp ,dispatcher)
    items-dispatch-table))

(defun initialize-uris ()
  ;; about page
  (register-regexp-dispatcher "^/about$" #'emit-about-page)
  (dolist (article *articles*)
    (let* ((article-dir (format nil "~a/~a" *itemization-source* article))
	   (miz-uri (format nil "/~a.miz" article))
	   (miz-path (format nil "~a/~a.miz" article-dir article))
	   (html-uri (format nil "/~a.html" article))
	   (html-path (format nil "~a/~a.html" article-dir article)))
      ;; static files for the whole article
      (register-static-file-dispatcher miz-uri miz-path "text/plain")
      (register-static-file-dispatcher html-uri html-path "text/html")
      ;; items for the article
      (loop
	 with num-items = (gethash article *article-num-items*)
	 for i from 1 upto num-items
	 initially
	   (push (create-regex-dispatcher
		  (format nil "^/~a$|^/~a/$" article article)
		  (emit-article-page article))
		  items-dispatch-table)
	 do
	   (push (create-regex-dispatcher 
		  (format nil "^/~a/~d$" article i)
		  (emit-dependency-page article i))
		 items-dispatch-table))))
  ;; set up path searcher
  (let ((ai-to-bj-uri-regex "^/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/?$"))
    (register-regexp-dispatcher ai-to-bj-uri-regex #'emit-path-between-items))
  (let ((source-via-dest-regex "^/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/([a-z_0-9]+)/([0-9]+)/?$"))
    (register-regexp-dispatcher source-via-dest-regex #'emit-path-between-items-via-item))
    t)
