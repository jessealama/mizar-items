;;; site-paths.lisp Functionality for exploring paths of dependence

(in-package :mizar)

(defgeneric explain-search-solution (source destination solution))

(defmethod explain-search-solution ((source symbol) destination solution)
  (explain-search-solution (symbol-name source) destination solution))

(defmethod explain-search-solution (source (destination symbol) solution)
  (explain-search-solution source (symbol-name destination) solution))

(defmethod explain-search-solution ((source string) (destination string) (steps list))
  (destructuring-bind (source-article source-item-kind source-item-number-str)
      (split ":" source)
    (destructuring-bind (dest-article dest-item-kind dest-item-number-str)
	(split ":" destination)
      (let ((source-uri (uri-for-item-as-string source))
	    (dest-uri (uri-for-item-as-string destination)))
	(with-html-output-to-string (s nil :indent nil)
	  ((:table :class "dependence-path")
	   (:caption
	    "A path of dependence from "
	    ((:a :href source-uri :title source) ((:span :class "article-name") (str source-article)) ":" (str source-item-kind) ":" (str source-item-number-str))
	    " to "
	    ((:a :href dest-uri :title destination) ((:span :class "article-name") (str dest-article)) ":" (str dest-item-kind) ":" (str dest-item-number-str)))
	   (:thead (:tr (:th "Item")))
	   (:tbody
	    (if (length= 1 steps)
		(let* ((item (car steps))
		       (item-html (html-for-item item)))
		  (htm ((:tr :class "dependence-path-node")
			(:td (str item-html)))))
		(loop
		   initially 
		     (let* ((first-step (first steps))
			    (first-step-html (html-for-item first-step))
			    (first-step-uri (uri-for-item-as-string first-step)))
		       (htm
			((:tr :class "dependence-path-node")
			 (:td ((:a :href first-step-uri :title first-step) (str first-step-html))))))
		   for step-from in steps
		   for step-to in (cdr steps)
		   for step-from-html = (html-for-item step-from)
		   for step-from-uri = (uri-for-item-as-string step-from)
		   for step-to-html = (html-for-item step-to)
		   for step-to-uri = (uri-for-item-as-string step-to)
		   for dependence-uri = (dependence-uri-for-items step-from step-to)
		   for dependence-link-title = (dependence-link-title step-from step-to)
		   do
		     (htm
		      ((:tr :class "dependence-path-edge")
		       ((:td :class "arrow") ((:a :href dependence-uri :title dependence-link-title) (str +downward-arrow-entity+))))
		      ((:tr :class "dependence-path-node")
		       (:td ((:a :href step-to-uri :title step-to) (str step-to-html))))))))))))))

(defmethod explain-search-solution (source destination (solution node))
  (explain-search-solution source destination (explain-solution solution)))

(defun path-form-as-string (source destination)
  (with-html-output-to-string (dummy)
    (:fieldset
     (:legend "Specify a path of dependence from a source to a destination")
     ((:form :action "/path"
	     :method "get"
	     :enctype "text/plain")
      (:table
       (:tr
	(:td ((:label :for "source-field") "Source"))
	(:td (if source
		 (htm ((:input :type "textarea"
			       :name "from"
			       :id "from-field"
			       :value source)))
		 (htm ((:input :type "textarea"
			       :id "from-field"
			       :name "from"))))))
       
       (:tr
	(:td ((:label :for "destination-field") "Destination"))
	(:td (if destination
		 (htm ((:input :type "textarea"
			       :name "to"
			       :id "to-field"
			       :value destination)))
		 (htm ((:input :type "textarea"
			       :name "to"
			       :id "to-field"))))))
       (:tr
	((:td :colspan "2")
	 ((:input :type "submit"
		  :value "Search")))))))))

(defvar *path-table* (make-hash-table :test #'equal)
  "A tale mapping pairs (cons cells) (SOURCE . DESTINATION) to pairs (PATHS . MORE-NODES).  The interpretation is that PATHS is a list of all paths computed so far from SOURCE to DESTINATION, and MORE-NODES is a queue representing a 'frozen' search state: the search for all paths from SOURCE to DESTINATION was most recently stopped, but there were MORE-NODES to consider that could lead to more paths being found.")

(defgeneric paths-from-to (source destination))

(defmethod paths-from-to ((source string) destination)
  (paths-from-to (get-and-maybe-set-item-name source) destination))

(defmethod paths-from-to (source (destination string))
  (paths-from-to source (get-and-maybe-set-item-name destination)))

(defmethod paths-from-to ((source symbol) (destination symbol))
  (multiple-value-bind (paths-and-more-paths we-done-been-here-before?)
      (gethash (cons source destination) *path-table*)
    (cond (we-done-been-here-before? 
	   (destructuring-bind (path-list . more-nodes)
	       paths-and-more-paths
	     (values path-list more-nodes))) 
	  (t ;; no one has asked for a path between these two
	   (values nil (make-initial-queue source))))))

(defgeneric register-path-with-nodes (source destination path nodes))

(defmethod register-path-with-nodes ((source string) destination path nodes)
  (register-path-with-nodes (get-and-maybe-set-item-name source) destination path nodes))

(defmethod register-path-with-nodes (source (destination string) path nodes)
  (register-path-with-nodes source (get-and-maybe-set-item-name destination) path nodes))

(defmethod register-path-with-nodes ((source symbol) (destination symbol) path nodes)
  (multiple-value-bind (path-list more-nodes)
      (paths-from-to source destination)
    (setf (gethash (cons source destination) *path-table*)
	  (cons (if (member path path-list :test #'equalp)
		    path-list
		    (cons path path-list))
		nodes))
    more-nodes))

(defgeneric update-paths-with-nodes (source destination new-nodes))

(defmethod update-paths-with-nodes ((source string) destination new-nodes)
  (update-paths-with-nodes (get-and-maybe-set-item-name source) destination new-nodes))

(defmethod update-paths-with-nodes (source (destination string) new-nodes)
  (update-paths-with-nodes source (get-and-maybe-set-item-name destination) new-nodes))

(defmethod update-paths-with-nodes ((source symbol) (destination symbol) new-nodes)
  (multiple-value-bind (paths-and-nodes known?)
      (gethash (cons source destination) *path-table*)
    (if known?
	(destructuring-bind (paths . nodes)
	    paths-and-nodes
	  (setf (gethash (cons source destination) *path-table*)
		(cons paths new-nodes))
	  nodes)
	(setf (gethash (cons source destination) *path-table*)
	      (cons nil new-nodes)))))

(defgeneric emit-path-between-items-form ())

(defmethod emit-path-between-items-form ()
  (let ((source (get-parameter "from"))
	(destination (get-parameter "to")))
    (if (and source (string/= source ""))
	(if (and destination (string/= destination ""))
	    (if (known-item? source)
		(if (known-item? destination)
		    (redirect (path-between-items-uri source destination 1)
			      :code +http-see-other+)
		    (miz-item-html ("invalid item")
			(:return-code +http-bad-request+)
		      ((:p :class "error-message")
		       "The given destination, '" (str destination) "', is not the name of a known item.")
		      (str (path-form-as-string source nil))))
		(miz-item-html ("invalid item")
		    (:return-code +http-bad-request+)
		  ((:p :class "error-message") 
		   "The given source, '" (str source) "', is not the name of a known item.")
		  (str (path-form-as-string nil destination))))
	    (miz-item-html ("specify a destination")
		nil
	      ((:p :class "error-message")
	       "You must specify a destination item.")
	      (str (path-form-as-string source nil))))
	(if destination
	    (miz-item-html ("specify a source")
		nil
	      ((:p :class "error-message")
	       "You must specify a source item.")
	      (str (path-form-as-string nil destination)))
	    (miz-item-html ("specify a source and destination")
		nil
	      (str (path-form-as-string nil nil)))))))

(defgeneric emit-path-between-items ()
  (:documentation "Display a path that shows a path between two items, possibly with some intermediate items in between."))

(defmethod emit-path-between-items :around ()
  (destructuring-bind (empty path-part source destination path-number-str)
      (split "/" (request-uri*))
    (declare (ignore empty path-part))
    (if (and source (string/= source ""))
	(if (and destination (string/= destination ""))
	  (if (known-item? source)
	      (if (known-item? destination)
		  ;; now check paths.  Generate an error page only
		  ;; when we have computed all paths from the source
		  ;; to the destination, but the given path number
		  ;; is out-of-bounds.
		  (let ((path-number (parse-integer path-number-str)))
		    (multiple-value-bind (paths more-nodes)
			(paths-from-to source destination)
		      (let ((num-paths (length paths)))
			(if (<= path-number num-paths)
			    (call-next-method)
			    (if (empty-queue? more-nodes)
				(miz-item-html ("invalid path request")
				    (:return-code +http-not-found+)
				  ((:p :class "error-message")
				   "There aren't that many paths between"
				   (str (pretty-print-item source))
				   " and "
				   (str (pretty-print-item destination))
				   "; there are (only) "
				   (fmt "~d" num-paths)
				   " such paths.  Please supply a different path number."))
				(if (> path-number (1+ num-paths))
				    (let ((next-path-uri (path-between-items-uri source destination (1+ num-paths)))
					  (next-path-link-title (format nil "Path number #~d from ~a to ~a" (1+ num-paths) source destination)))
				      (miz-item-html ("search for a path")
					  (:return-code +http-see-other+)
					(:p (if (zerop num-paths)
						(htm "We haven't computed any paths from "
						     (str (pretty-print-item source))
						     " to "
						     (str (pretty-print-item destination)) ".")
						(htm "We have already computed "
						     (fmt "~d" num-paths)
						     " path(s) from "
						     (str (pretty-print-item source))
						     " to "
						     (str (pretty-print-item destination)) ". There may be more paths, but we don't know yet.")) 
					    " You have requested path number " (str path-number-str) ".  Since paths are computed one at a time, we cannot process your request for path number " (str path-number-str) " before computing path number " (fmt "~d" (1+ num-paths)) " (which might not even exist).  To proceed, specify a smaller path number for a path that is known to exist, or "
					    ((:a :href next-path-uri
						 :title next-path-link-title) "search for path " (fmt "~d" (1+ num-paths)) "."))))
				    (call-next-method)))))))
		  (miz-item-html ("invalid item")
		      (:return-code +http-bad-request+)
		    ((:p :class "error-message")
		     "The given destination, '" (str destination) "', is not the name of a known item.")
		    (str (path-form-as-string source nil))))
	      (miz-item-html ("invalid item")
		  (:return-code +http-bad-request+)
		((:p :class "error-message") 
		 "The given source, '" (str source) "', is not the name of a known item.")
		(str (path-form-as-string nil destination))))
	  (miz-item-html ("specify a destination")
	      nil
	    ((:p :class "error-message")
	     "You must specify a destination item.")
	    (str (path-form-as-string source nil))))
      (if destination
	  (miz-item-html ("specify a source")
	      nil
	    ((:p :class "error-message")
	     "You must specify a source item.")
	    (str (path-form-as-string nil destination)))
	  (miz-item-html ("specify a source and destination")
	      nil
	    (str (path-form-as-string nil nil)))))))

(defgeneric render-path-search-solution (source destination path path-number link-to-previous? link-to-next?))

(defmethod render-path-search-solution ((source string) destination path path-number link-to-previous? link-to-next?)
  (render-path-search-solution (get-and-maybe-set-item-name source) destination path path-number link-to-previous? link-to-next?))

(defmethod render-path-search-solution (source (destination string) path path-number link-to-previous? link-to-next?)
  (render-path-search-solution source (get-and-maybe-set-item-name destination) path path-number link-to-previous? link-to-next?))

(defmethod render-path-search-solution ((source symbol) (destination symbol) path path-number link-to-previous? link-to-next?)
  (let* ((explanation (explain-search-solution source destination path))
	 (path-len (length explanation))
	 (source-link (link-to-item source))
	 (dest-link (link-to-item destination))
	 (source-uri-link-title (item-link-title-from-string (symbol-name source)))
	 (dest-uri-link-title (item-link-title-from-string (symbol-name destination)))
	 (next-path-uri (when link-to-next? (path-between-items-uri source destination (1+ path-number))))
	 (next-path-link-title (format nil "Next path from ~a to ~a" source-uri-link-title dest-uri-link-title))
	 (prev-path-uri (when link-to-previous? (path-between-items-uri source destination (1- path-number))))
	 (prev-path-link-title (format nil "Previous path from ~a to ~a" source-uri-link-title dest-uri-link-title)))
    (with-html-output-to-string (dummy)
      ((:table :class "item-table")
       (:tr
	((:td :width "25%" :align "left")
	 ((:table :class "item-info")
	  ((:tr :class "item-info-row")
	   ((:td :colspan "2" :class "item-info-heading") "Path Info"))
	  ((:tr :class "item-info-row")
	   ((:td :class "item-info-key") "Source")
	   ((:td :class "item-info-value")
	    (str source-link)))
	  ((:tr :class "item-info-row")
	   ((:td :class "item-info-key") "Destination")
	   ((:td :class "item-info-value")
	    (str dest-link)))
	  ((:tr :class "item-info-row")
	 ((:td :class "item-info-key") "Length")
	 ((:td :class "item-info-value") (str path-len)))
	((:tr :class "item-info-row")
	 ((:td :class "item-info-key") "Path Number")
	 ((:td :class "item-info-value")
	  "[" (if prev-path-uri
		  (htm ((:a :href prev-path-uri :title prev-path-link-title) "&lt;"))
		  (htm "&lt;"))
	  "]"
	  " "
	  (fmt "~d" path-number)
	  " "
	  "[" (if next-path-uri
		  (htm ((:a :href next-path-uri :title next-path-link-title) "&gt;"))
		  (htm "&gt;"))
	  "]"))
	  ((:tr :class "item-info-row")
	   ((:td :colspan "2" :class "item-info-heading") "Items"))
	  ((:tr :class "item-info-row")
	   ((:td :colspan "2" :class "item-info-value")
	    (:ol
	     (loop
		for step in path
		for step-link = (link-to-item step)
		do
		  (htm (:li (str step-link)))))))))
      ((:td :width "75%" :valign "top")
       (str explanation)))))))

(defmethod emit-path-between-items ()
  (destructuring-bind (empty path-part source destination path-number-str)
      (split "/" (request-uri*))
    (declare (ignore empty path-part))
    (multiple-value-bind (paths more-nodes)
	(paths-from-to source destination)
      (let ((path-number (parse-integer path-number-str))
	    (num-paths (length paths)))
      (if (<= path-number num-paths)
	  (let* ((path (nth (1- path-number) paths))
		 (html (render-path-search-solution source
						    destination
						    path
						    path-number
						    (> path-number 1)
						    (or (not (empty-queue? more-nodes))
							(<= path-number num-paths)))))
	    (miz-item-html ("path found")
		nil
	      (str html)))
	  (multiple-value-bind (solution more-nodes)
	      (one-path source destination +search-depth+ more-nodes)
	    (cond (solution
		   (if (eq solution :cut-off)
		       (progn
			 ;; update the path table
			 (update-paths-with-nodes source destination more-nodes)
			 (miz-item-html ("search cut off")
			     nil
			   (:p "There may be a path from " (str source) " to " (str destination) ", but we were unable to find one given the current search limits.  (Searches are currenly restricted to not go deeper than depth " (:b (str +search-depth+)))))
		       (progn
			 (register-path-with-nodes source destination solution more-nodes)
			 (miz-item-html ("path found")
			     nil
			   (str (render-path-search-solution source
							     destination
							     solution
							     path-number
							     (> path-number 1)
							     (or (not (empty-queue? more-nodes))
								 (<= path-number num-paths))))))))
		  (t
		   ;; update the path table -- we're done with source and destination
		   (update-paths-with-nodes source destination (make-empty-queue))
		   (miz-item-html ("no path")
		       nil
		     (:p "There is no path of dependence from " (str (link-to-item source)) " to " (str (link-to-item destination)) "."))))))))))

;;; site-paths.lisp ends here