
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server and application setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-mizar-favicon-and-title (title &body body)
  `(with-favicon-and-title "/favicon.ico" ,title ,@body))

(defmacro miz-item-html (title &body body)
  `(with-html
     (:head 
      ((:link :rel "icon" :href "/favicon.ico" :type "image/x-icon"))
      ((:link :href "/mhtml.css" :rel "stylesheet" :type "text/css"))
      ((:script :src "/mhtml.js" :type "text/ecmascript"))
      (:title ,title))
     (:body
      ((:table :border "1"
	       :summary "navigation"
	       :class "header"
	       :width "100%")
       (:tr
	(:td
	 ((:a :href "") "main")
	 ((:span :class "hide") ":")
	 ((:span :class "menu")
	  ((:a :href "about") "about")
	  ((:a :href "random") "random")))))
      ,@body)))

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
(defparameter *all-ckb-items* nil)
(defparameter *all-true-items* nil)

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
    (setf *all-ckb-items* all-ckb-items
	  *all-true-items* all-true-items))
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
	  (gethash (node-state node) *true-item-dependency-graph-forward*)))

(defun all-paths (source destination)
  (if (string= source destination)
      (list (list source))
      (mapcar #'(lambda (path)
		  (cons source path))
	      (reduce #'append 
		      (mapcar #'(lambda (successor)
				  (all-paths successor destination))
			      (gethash source *true-item-dependency-graph-forward*))))))

(defun all-paths-from-via (source destination via)
  (let ((paths-from-source-to-via (all-paths source via)))
    (when paths-from-source-to-via
      (let ((paths-from-via-to-destination (all-paths via destination)))
	(when paths-from-via-to-destination
	  (map-product #'(lambda (path-1 path-2)
			   (append path-1 (cdr path-2)))
		       paths-from-source-to-via
		       paths-from-via-to-destination))))))

(defun one-path-from-via (source destination via)
  "Find one path from SOURCE to DESTINATION that passes through VIA.
If there is no such path, return nil."
  (let ((source-to-via-problem (make-item-search-problem 
				:initial-state source
				:goal via))
	(via-to-destination-problem (make-item-search-problem
				     :initial-state via
				     :goal destination)))
    (let ((solution-to-via (depth-first-search source-to-via-problem)))
      (when solution-to-via
	(let ((solution-to-destination (depth-first-search via-to-destination-problem)))
	  (when solution-to-destination
	    (append (explain-solution solution-to-via)
		    (cdr (explain-solution solution-to-destination)))))))))

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
	 (to-bad-guy-solution (depth-first-search to-bad-guy)))
    (if to-bad-guy-solution
	(let* ((from-bad-guy (make-item-search-problem :goal destination
						       :initial-state bad-guy))
	       (from-bad-guy-solution (depth-first-search from-bad-guy)))
	  (if from-bad-guy-solution
	      (let ((path-to-bad-guy (explain-solution to-bad-guy-solution))
		    (path-from-bad-guy (explain-solution from-bad-guy-solution)))
		(values nil (append path-to-bad-guy (cdr path-from-bad-guy))))
	      (values t nil)))
	(values t nil))))

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
	*log-lisp-backtraces-p* t
	*show-lisp-errors-p* t)
  t)

(defun handle-http-error (error-code)
  (when (= error-code +http-not-found+)
    (miz-item-html "No"
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

(define-constant +article-name-regexp+ "[a-z_0-9]+" 
  :test #'string=)
(define-constant +number-regexp+ "[0-9]+"
  :test #'string=)
(define-constant +item-kinds-prefixes+ (list "k" "m" "r" "v")
  :test #'equal)
(define-constant +cluster-kinds+ (list "c" "f" "r")
  :test #'equal)
(define-constant +item-kinds-string+ 
  (append (mapcar #'(lambda (sym)
		      (concat sym "constructor"))
		  +item-kinds-prefixes+)
	  (mapcar #'(lambda (sym)
		      (concat sym "pattern"))
		  +item-kinds-prefixes+)
	  (mapcar #'(lambda (sym)
		      (concat sym "definiens"))
		  +item-kinds-prefixes+)
	  '("deftheorem"
	    "theorem"
	    "lemma"
	    "scheme"
	    "identification")
	  (mapcar #'(lambda (sym)
		      (concat sym "cluster"))
		  +cluster-kinds+))
  :test #'equal)

(define-constant +item-kind-regexp+ 
    (reduce #'regexp-disjoin +item-kinds-string+)
  :test #'string=)

(define-constant +true-item-uri-regexp+
    (exact-regexp (concat "/" "item"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"))
  :test #'string=)

(defun uri-for-item (article kind number)
  (format nil "/item/~a/~a/~a" article kind number))

(define-constant +ckb-item-uri-regexp+
    (exact-regexp (concat "/" "fragment"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/?"))
  :test #'string=)

(define-constant +article-uri-regexp+
    (exact-regexp (concat "/" "article"
			  "/" "(" +article-name-regexp+ ")" "/?"))
  :test #'string=)

(defun uri-for-article (article)
  (format nil "/article/~a" article))

(define-constant +path-between-true-items-uri-regexp+
    (exact-regexp (concat "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/?" ; maybe end with a '/'
			  ))
  :test #'string=)

(define-constant +path-between-true-items-via-item-uri-regexp+
    (exact-regexp (concat "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/?" ; maybe end with a '/'
			  ))
  :test #'string=)

(defun emit-about-page ()
  (miz-item-html "fine-grained dependencies in the mizar mathematical library"
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
  (register-groups-bind (article-1 kind-1 num-1 article-2 kind-2 num-2)
      (+path-between-true-items-uri-regexp+ (request-uri*))
    ;; check that these items exist
    (let ((source-item (format nil "~a:~a:~a" article-1 kind-1 num-1))
	  (destination-item (format nil "~a:~a:~a" article-2 kind-2 num-2)))
      (if (gethash source-item *all-true-items*)
	  (if (gethash destination-item *all-true-items*)
	      (let* ((problem (make-item-search-problem 
			       :initial-state source-item
			       :goal destination-item))
		     (solution (depth-first-search problem)))
		(miz-item-html (fmt "from ~a to ~a" source-item destination-item)
		  (if solution
		      (htm
		       (:p (fmt "Here is a path from ~a to ~a" source-item destination-item))
		       (:ol
			(let ((source-uri (format nil "/~a/~a/~a" article-1 kind-1 num-1)))
			  (htm
			   (:li ((:a :href source-uri)
				 (str source-item)))))
			(dolist (step (explain-solution solution))
			  (destructuring-bind (step-article step-kind step-num)
			      (split ":" step)
			    (let ((step-uri (format nil "/~a/~a/~a" step-article step-kind step-num)))
			      (htm
			       (:li ((:a :href step-uri)
				     (str step)))))))))
		      (htm
		       (:p (fmt "There is no path from ~a to ~a." source-item destination-item)
			   " Care to " ((:a :href (fmt "/~a/~a/~a/~a/~a/~a"
						       article-2 kind-2 num-2
						       article-1 kind-1 num-1))
					"search for a path going the other way")
			   "?")))))
	      (miz-item-html "Invalid URI"
		(:p "The requested destination item, '" (str destination-item) "', is not the name of any known item.")))
	  (miz-item-html "Invalid URI"
	    (:p "The requested source item, '" (str source-item) "', is not the name of any known item."))))))

(defun emit-path-between-items-via-item ()
  (register-groups-bind (source-article source-kind source-num 
					via-article via-kind via-num
					destination-article destination-kind destination-num)
      (+path-between-true-items-via-item-uri-regexp+ (request-uri*))
    (let ((source (format nil "~a:~a:~a" source-article source-kind source-num))
	  (via (format nil "~a:~a:~a" via-article via-kind via-num))
	  (destination (format nil "~a:~a:~a" destination-article destination-kind destination-num)))	
      (if (gethash source *all-true-items*)
	  (if (gethash via *all-true-items*)
	      (if (gethash destination *all-true-items*)
		  (let ((get-params (get-parameters*)))
		    (if (null get-params) ; first time here, eh?
			(miz-item-html (fmt "from ~a to ~a via ~a" source destination via)
			  (:dl
			   (:dt "Source")
			   (:dd (str source))
			   (:dt "Destination")
			   (:dd (str destination))
			   (:dt "Via")
			   (:dd (str via)))
			  (:p "What kind of search would you like to do?")
			  (:ul
			   (:li "Find one path from source to destination;)")
			   (:li "Find " (:em "all") " paths;")
			   (:li "Find a path from the source to the destination that " (:em "avoids") " the the intermediate verte;x")
			   (:li "Find " (:em "all") " paths from the source to the destination that avood the intermediate vertex.")))
			(miz-item-html "You're asking too much"
			  (:p "I can't handle this: " (fmt "~A" get-params)))))
		  (miz-item-html "Invalid URI"
		    (:p "There given destination item, '" (str destination) "', is not the name of any known item.")))
	      (miz-item-html "Invalid URI"
		(:p "There given intermediate item, '" (str via) "', is not the name of any known item.")))
	  (miz-item-html "Invalid URI"
	    (:p "There given source item, '" (str source) "', is not the name of any known item."))))))

(defun emit-article-page ()
  (register-groups-bind (article)
      (+article-uri-regexp+ (request-uri*))
    (let ((num-items (gethash article *article-num-items*)))
      (miz-item-html (fmt "~a" article)
	(:p "The article " (str article) " has " (:b (str num-items)) " items ")
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
  (let ((random-vertex (random-elt (hash-table-keys *all-true-items*))))
    (destructuring-bind (article kind number)
	(split ":" random-vertex)
      (redirect (uri-for-item article kind number)))))

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
  (let ((exact-uri (exact-regexp uri)))
    `(register-regexp-dispatcher ,exact-uri ,dispatcher)))

(defun emit-mizar-item-page ()
  (register-groups-bind (article-name item-kind item-number)
      (+true-item-uri-regexp+ (request-uri*))
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
	  (miz-item-html (str item-key)
	    (:p (str item-key) " comes from toplevel item " (str ckb-number) " of article " (str article-name) ".")
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
	      ((:td :colspan 2) (str item-html)))
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
		   (htm (:p (:em "(No item immediately depends on this one.)")))))))))))))

(defun emit-ckb-item-page ()
  (register-groups-bind (article-name item-number)
      (+ckb-item-uri-regexp+ (request-uri*))
    (let* ((item-key (format nil "~a:~a" article-name item-number))
	   (article-dir (format nil "~a/~a" *itemization-source* article-name))
	   (article-text-dir (format nil "~a/text" article-dir))
	   (items-for-ckb (gethash item-key *ckb-to-items-table*))
	   (forward-deps (gethash item-key *ckb-dependency-graph-forward*))
	   (backward-deps (gethash item-key *ckb-dependency-graph-backward*))
	   (forward-deps-sorted (sort forward-deps #'ckb-item-<))
	   (backward-deps-sorted (sort backward-deps #'ckb-item-<)))
      (destructuring-bind (ckb-article-name ckb-number)
	  (split ":" item-key)
	(declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	(let* ((ckb-item-path (format nil "~a/ckb~a.html"
				      article-text-dir
				      ckb-number))
	       (item-html (file-as-string ckb-item-path)))
	  (miz-item-html (str item-key)
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
		       (destructuring-bind (dep-name dep-num)
			   (split ":" forward-dep)
			 (let ((dep-uri (format nil "/~a/~a" dep-name dep-num)))
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
		       (destructuring-bind (dep-name dep-num)
			   (split ":" backward-dep)
			 (let ((dep-uri (format nil "/~a/~a" dep-name dep-num)))
			   (htm
			    (:tr (:td ((:a :href dep-uri) (str backward-dep))))))))))
		   (htm (:p (:em "(No item immediately depends on this one.)")))))))))))))

(defun emit-main-page ()
  (miz-item-html (str "fine-grained dependencies in mizar")
    (:p "Interested in learning more about the " (:tt "MIZAR") "
    Mathematical Library (MML)?  This site provides a way to
    get a handle on the large contents of the MML.")
    (:p "This site is based on MML version 4.156.1102
    and " (:tt "MIZAR") " binaries based on version 7.11.07.  For more information, consult the " (:tt "MIZAR") " homepage at")
    (:blockquote
     ((:a :href "http://www.mizar.org/") "http://www.mizar.org/"))
    (:p (:b "Note:") " There are slight differences between the
    official MML and official binary releases available on
    the " (:tt "MIZAR") " homepage.  To process " (:tt "MIZAR") "
    articles so that they can be analyzed in the way that I do, some
    pre-processors are applied to normalize the text of the articles
    of the MML.  The texts needed to be adjusted to suit this
    pre-processing.  Thankfully, very few articles of the MML needed
    to be adjusted.  Moreover, there are are a couple trivial
    differences between the " (:tt "MIZAR") " verifier that is being
    used here and the verifier that one would obtain by downloading
    binaries from the " (:tt "MIZAR") " homepage.  For those who
    know " (:tt "MIZAR") ", the differences are:")
    (:ul
     (:li "The official verifier permits only 50 reservation
     statements to be present in an article.  Because of some
     transformations taht I carry out on the texts, this turns out to
     be too low; I permit 100 reservations.")
     (:li "The official verifier permits at most 90 variables to be
     reserved.  Again, for my purposes, this is too low, and I changed
     it to 150."))
    (:p "These differences have no logical significance; the verifier
    with these changes validates a slightly larger set of texts, but
    the difference has no bearing on the mathematical soundness of
    the " (:tt "MIAR") " verifier.  Informally, the difference is
    this: the verifier would reject locutions such as")
    ((:blockquote :style "font-style:oblique;")
     "Let X" (:sub "1") ", X" (:sub "2") ", " (str "&hellip;") ", X" (:sub "90") ", X" (:sub "91") " be sets such that " (str "&hellip;"))
    (:p "simply because there are too many variables: here, there are 91, but the hard-coded limit in the official " (:tt "MIZAR") " verifier is 90.")))
    

(defun initialize-uris ()
  ;; intro
  (register-exact-uri-dispatcher "/" #'emit-main-page)
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
      (hunchentoot-dir-lister:add-simple-lister text-dir-uri text-dir-path)))
  (register-regexp-dispatcher +article-uri-regexp+ #'emit-article-page)
  (register-regexp-dispatcher +ckb-item-uri-regexp+ #'emit-ckb-item-page)
  (register-regexp-dispatcher +true-item-uri-regexp+ #'emit-mizar-item-page)
  (register-regexp-dispatcher +path-between-true-items-via-item-uri-regexp+
			      #'emit-path-between-items)
  (register-regexp-dispatcher +path-between-true-items-via-item-uri-regexp+
			      #'emit-path-between-items-via-item)
  t)
