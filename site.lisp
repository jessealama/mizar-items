
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +downward-arrow-entity+ "&dArr;"
  :test #'string=
  :documentation "A downward-pointing arrow (see also http://www.blooberry.com/indexdot/html/tagpages/entities/arrow.htm).")

(define-constant +upward-arrow-entity+ "&uArr;"
  :test #'string=
  :documentation "An upward-pointing arrow (see also http://www.blooberry.com/indexdot/html/tagpages/entities/arrow.htm).")

(define-constant +cross-symbol+ "&#10008;"
  :test #'string=
  :documentation "A heavy cross symbol (see http://www.danshort.com/HTMLentities/index.php?w=dingb).")

(define-constant +checkmark-symbol+ "&#10004;"
  :test #'string=
  :documentation "A heavy checkmark symbol (see http://www.danshort.com/HTMLentities/index.php?w=dingb).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; URI regular expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +article-name-regexp+ "^[a-z_0-9]{1,8}$" 
  :test #'string=)
(define-constant +positive-natural-number-regexp+ "[1-9][0-9]*"
  :test #'string=
  :documentation "A regular expression matching positive natural numbers.  Starting with the zero digit 0 is not permitted")
(define-constant +number-regexp+
    (regexp-disjoin "0" 
		    +positive-natural-number-regexp+)
  :test #'string=
  :documentation "A regular expression that matches a natural number.  (0 is a natural number.)")
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
	  '("definiens"
	    "deftheorem"
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

(define-constant +item-name-regexp+
    (concat +article-name-regexp+
	    ":"
	    "(" +item-kind-regexp+ ")"
	    ":"
	    "(" +positive-natural-number-regexp+ ")")
  :test #'string=)

(define-constant +item-uri-regexp+
    (exact-regexp (concat "/" "item"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"))
  :test #'string=)

(defun uri-for-fragment (article fragment-number)
  (format nil "/fragment/~a/~d" article fragment-number))

(defun uri-for-item (article kind number)
  (format nil "/item/~a/~a/~a" article kind number))

(defun uri-for-item-as-string (item-string)
  (destructuring-bind (article kind number)
      (split-item-identifier item-string)
    (uri-for-item article kind number)))

(defun uri-for-item-as-symbol (item-symbol)
  (uri-for-item-as-string (symbol-name item-symbol)))

(defgeneric link-to-item (item))

(defmethod link-to-item ((item symbol))
  (link-to-item (symbol-name item)))

(defmethod link-to-item ((item string))
  (let ((uri (uri-for-item-as-string item))
	(title (item-link-title-from-string item)))
    (with-html-output-to-string (dummy)
      ((:a :href uri :title title)
       (str (pretty-print-item item))))))

(defun link-to-item-with-components (article kind number)
  (link-to-item (item-from-components article kind number)))

(define-constant +fragment-uri-regexp+
    (exact-regexp (concat "/" "fragment"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +number-regexp+ ")"))
  :test #'string=)

(define-constant +itemized-article-uri-regexp+
    (exact-regexp (concat "/" "article"
			  "/" "(" +article-name-regexp+ ")" "/?"))
  :test #'string=)

(define-constant +article-html-uri-regexp+
    (exact-regexp (concat "/" "(" +article-name-regexp+ ")" ".html"))
  :test #'string=
  :documentation "A regular expression matching the URI of an HTML representation of a whole, non-itemized presentation of an article")

(define-constant +article-xml-uri-regexp+
    (exact-regexp (concat "/" "(" +article-name-regexp+ ")" ".xml"))
  :test #'string=
  :documentation "A regular expression matching the URI of an XML representation of an article.")

(defun uri-for-article (article)
  (format nil "/article/~a" article))

(define-constant +path-between-items-form-regexp+
    (exact-regexp "/path")
  :test #'string=)

(define-constant +path-between-items-uri-regexp+
    (exact-regexp (concat "/" "path"
			  "/" "(" +item-name-regexp+ ")"
			  "/" "(" +item-name-regexp+ ")"
			  "/" "(" +positive-natural-number-regexp+ ")"))
  :test #'string=
  :documentation "A regular expression that matches the URI (sans query parameters) for searching for paths in the items dependency graph.")

(defun path-between-items-uri (from to path-number)
  (format nil "/path/~a/~a/~d" from to path-number))

(define-constant +requires-uri-regexp+
    (exact-regexp
     (concat "/" "item"
	     "/" "(" +article-name-regexp+ ")"
	     "/" "(" +item-kind-regexp+ ")"
	     "/" "(" +number-regexp+ ")"
	     "/" "requires"))
  :test #'string=
  :documentation "A regular expression for matching URIs associated with the action of computing what an item requires/depends on.")

(define-constant +supports-uri-regexp+
    (exact-regexp
     (concat "/" "item"
	     "/" "(" +article-name-regexp+ ")"
	     "/" "(" +item-kind-regexp+ ")"
	     "/" "(" +number-regexp+ ")"
	     "/" "supports"))
  :test #'string=
  :documentation "A regular expression for matching URIs associated with the action of computing what an item requires/depends on.")

(define-constant +item-regexp+
    (concat +article-name-regexp+
	    ":"
	    "(" +item-kind-regexp+ ")"
	    ":"
	    "(" +number-regexp+ ")" )
  :test #'string=
  :documentation "A regular expression matching names of items.")

(define-constant +dependence-uri-regexp+
    (exact-regexp
     (concat "/" "dependence"
	     "/" "(" +item-regexp+ ")"
	     "/" "(" +item-regexp+ ")"))
  :test #'string=
  :documentation "A regular expression matching URIs associated with displaying the dependence of one item on another.")

(define-constant +sufficiency-uri-regexp+
    (exact-regexp (concat "/" "dependence"
			  "/" "(" +item-regexp+ ")"
			  "/" "sufficiency"))
  :test #'string=
  :documentation "A regular expression matching URIs associated with
  the task of verifying that the set of items claimed to be needed for
  a given item is sufficient to (re)justify the item.")

(define-constant +minimality-uri-regexp+
    (exact-regexp (concat "/" "dependence"
		  "/" "(" +item-regexp+ ")"
		  "/" "minimality"))
  :test #'string=
  :documentation "A regular expression matching URIs associated with
  the task of verifying that nothing can be removed from the set of
  items claimed to be needed for a given item without
  countersatisfiability.  (Actually, we might not always get
  countersatisfiability.)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emitting XML/HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defparameter *absrefs-params*
;;   (list (xuriella:make-parameter "0" "explainbyfrom")))

;; (defparameter *absrefs-sylesheet*
;;   (xuriella:parse-stylesheet
;;    (pathname (mizar-items-config 'absrefs-stylesheet))))

;; (let* ((absrefs-path (mizar-items-config 'absrefs-stylesheet))
;;        (orig-absrefs-sha1 (ironclad:digest-file :sha1 absrefs-path))
;;        (cache (make-hash-table :test #'equalp)))
;;   (labels ((transform (filename)
;; 	     (xuriella:apply-stylesheet *absrefs-sylesheet*
;; 					(pathname filename)
;; 					:parameters *absrefs-params*))
;; 	   (update-cache (path sha1)
;; 	     (setf (gethash sha1 cache) (transform path))))
;;     (defun absrefs (article-xml-path)
;;       (let ((new-absrefs-sha1 (ironclad:digest-file :sha1 absrefs-path))
;; 	    (xml-sha1 (ironclad:digest-file :sha1 article-xml-path)))
;; 	(cond ((equalp orig-absrefs-sha1 new-absrefs-sha1) ; stylesheet unchanged
;; 	       (multiple-value-bind (cached present?)
;; 		   (gethash xml-sha1 cache)
;; 		 (if present?
;; 		     cached
;; 		     (update-cache article-xml-path xml-sha1))))
;; 	      (t ; the stylesheet has changed
;; 	       (clrhash cache) ; all old values are now unreliable
;; 	       (setf orig-absrefs-sha1 new-absrefs-sha1)
;; 	       (update-cache article-xml-path xml-sha1)))))))

;; (defparameter *mhtml-params*
;;   (list (xuriella:make-parameter "1" "colored")
;; 	(xuriella:make-parameter "1" "proof_links")
;; 	(xuriella:make-parameter "1" "titles")))

;; (defparameter *mhtml-stylesheet*
;;   (xuriella:parse-stylesheet
;;    (pathname (mizar-items-config 'mhtml-stylesheet))))

;; (let* ((mhtml-path (mizar-items-config 'mhtml-stylesheet))
;;        (orig-mhtml-sha1 (ironclad:digest-file :sha1 mhtml-path))
;;        (cache (make-hash-table :test #'equalp)))
;;   (labels ((transform (filename &optional source-article-name)
;; 	     (let ((dir (directory-namestring filename))
;; 		   (source-article-param (xuriella:make-parameter "source_article" source-article-name))
;; 		   (mizar-items-param (xuriella:make-parameter "mizar_items" "1")))
;; 	       (flet ((file-in-dir (uri)
;; 			(let ((path (puri:uri-path uri)))
;; 			  (when (scan ".(idx|eno|dfs|eth|esh|atr)$" path)
;; 			    (register-groups-bind (after-root)
;; 				("^/(.+)" path)
;; 			      (let ((new-path (merge-pathnames after-root dir)))
;; 				(setf (puri:uri-path uri)
;; 				      (namestring new-path)))))
;; 			  uri)))
;; 		 (xuriella:apply-stylesheet
;; 		  *mhtml-stylesheet*
;; 		  (absrefs filename)
;; 		  :parameters (append
;; 			       (when source-article-name
;; 				 (list source-article-param
;; 				       mizar-items-param))
;; 			       *mhtml-params*)
;; 		  :uri-resolver #'file-in-dir))))
;; 	   (update-cache (path sha1 &optional source-file-name)
;; 	     (setf (gethash sha1 cache) (transform path source-file-name))))
;;     (defun mhtml (article-xml-path &optional source-article-name)
;;       (let ((new-mhtml-sha1 (ironclad:digest-file :sha1 mhtml-path))
;; 	    (xml-sha1 (ironclad:digest-file :sha1 article-xml-path)))
;; 	(cond ((equalp orig-mhtml-sha1 new-mhtml-sha1) ; stylesheet unchanged
;; 	       (multiple-value-bind (cached present?)
;; 		   (gethash xml-sha1 cache)
;; 		 (if present?
;; 		     cached
;; 		     (update-cache article-xml-path xml-sha1 source-article-name))))
;; 	      (t ; the stylesheet has changed
;; 	       (clrhash cache) ; all old values are now unreliable
;; 	       (setf orig-mhtml-sha1 new-mhtml-sha1)
;; 	       (update-cache article-xml-path xml-sha1 source-article-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fragment-< (item-name-1 item-name-2)
  (destructuring-bind (item-article-name-1 item-num-as-str-1)
      (split ":" item-name-1)
    (destructuring-bind (item-article-name-2 item-num-as-str-2)
	(split ":" item-name-2)
      (or (string< item-article-name-1 item-article-name-2)
	  (let ((item-num-1 (parse-integer item-num-as-str-1))
		(item-num-2 (parse-integer item-num-as-str-2)))
	    (and (string= item-article-name-1 item-article-name-2)
		 (< item-num-1 item-num-2)))))))

(defun item-< (item-name-1 item-name-2)
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

(defun link-for-item (item)
  (destructuring-bind (article kind num)
      (split ":" item)
    (format nil "/item/~a/~a/~a" article kind num)))


(defun dependence-uri-for-items (item-1 item-2)
  (format nil "/dependence/~a/~a" item-1 item-2))

(defun dependence-link-title (dependent-item supporting-item)
  (let ((dep-article (item-article dependent-item))
	(dep-kind (item-kind dependent-item))
	(dep-num (item-number dependent-item))
	(sup-article (item-article supporting-item))
	(sup-kind (item-kind supporting-item))
	(sup-num (item-number supporting-item)))
    (format nil 
	    "~:@(~a~):~a:~a depends on ~:@(~a~):~a:~a"
	    dep-article dep-kind dep-num
	    sup-article sup-kind sup-num)))

(defun article-listing ()
  (with-html-output-to-string (foo)
    ((:table :class "article-listing" :rules "rows")
     (:thead
      (:tr
       (:th "MML Name")
       (:th "Title")
       (:th "Authors")))
     (:tbody
      (loop
	 for article in *mml-lar*
	 do
	   (with-slots (name title authors)
	       article
	     (let ((title-escaped (escape-string title))
		   (article-uri (uri-for-article name))
		   (author-list-pretty (if authors
					   (format nil "~{~a~#[~;, and~:;,~]~}" authors)
					   "(author information not known)")))
	       (htm
		(:tr
		 ((:td :class "article-name")
		  ((:a :href article-uri :title title-escaped)
		   (str name)))
		 ((:td :class "article-title") (str title))
		 ((:td :class "author-list")
		  (str author-list-pretty)))))))))))

(defgeneric emit-article-xml ()
  (:documentation "Emit an XML representation of an article."))

(defmethod emit-article-xml :around ()
  (register-groups-bind (article)
      (+article-xml-uri-regexp+ (request-uri*))
    (if (member article *mml-lar* :test #'string=)
	(call-next-method)
	(miz-item-html ("article not found")
	    (:return-code +http-not-found+)
	  (:p ((:span :class "article-name") (str article)) " is not known.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-article-xml ()
  (register-groups-bind (article)
      (+article-xml-uri-regexp+ (request-uri*))
    (let ((xml-path (xml-path-for-article article)))
      (handle-static-file xml-path "application/xml"))))

(defgeneric emit-unitemized-article-page ()
  (:documentation "An HTML representation of an itemized article."))

(defmethod emit-unitemized-article-page :around ()
  (register-groups-bind (article)
      (+article-html-uri-regexp+ (request-uri*))
    (if (member article *mml-lar* :test #'string=)
	(call-next-method)
	(miz-item-html ("article not found")
	    (:return-code +http-not-found+)
	  (:p ((:span :class "article-name") (str article)) " is not known.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-unitemized-article-page ()
  (register-groups-bind (article)
      (+article-html-uri-regexp+ (request-uri*))
    (let* ((html (html-for-article article))
	   (bib-title (article-title article))
	   (title (if bib-title
		      (format nil "~a: ~a" article bib-title)
		      (format nil "~a" article))))
      (miz-item-html (title)
	  nil
	(str html)))))

(defgeneric emit-itemized-article-page ()
  (:documentation "An HTML representation of an itemized article."))

(defmethod emit-itemized-article-page :around ()
  (register-groups-bind (article)
      (+itemized-article-uri-regexp+ (request-uri*))
    (if (belongs-to-mml article)
	(call-next-method)
	(miz-item-html ("article not found")
	    (:return-code +http-not-found+)
	  (:p ((:span :class "article-name") (str article)) " is not known.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defun landmarks-for-article (article)
  (declare (ignore article))
  nil)

(defun canonicalize-name (name)
  (substitute #\- #\Space (format nil "~(~a~)" name)))

(defun uri-for-author (name)
  (format nil "/author/~a" (canonicalize-name name)))

(defmethod emit-itemized-article-page ()
  (register-groups-bind (article-name)
      (+itemized-article-uri-regexp+ (request-uri*))
    (let* ((article (find article-name *mml-lar* :key #'name :test #'string=))
	   (article-name (name article))
	   (article-pos-in-mml-lar (position article-name *mml-lar* :key #'name :test #'string=))
	   (prev-article (unless (zerop article-pos-in-mml-lar)
			   (nth (1- article-pos-in-mml-lar)
				*mml-lar*)))
	   (prev-article-uri (when prev-article
			       (uri-for-article (name prev-article))))
	   (prev-article-link-title (when prev-article
				      (format nil "~:@(~a~): ~a"
					      (name prev-article)
					      (title prev-article))))
	   (next-article (unless (= article-pos-in-mml-lar
				    (1- (length *mml-lar*)))
			   (nth (1+ article-pos-in-mml-lar)
				*mml-lar*)))
	   (next-article-name (when next-article
				(name next-article)))
	   (next-article-uri (when next-article-name
			       (uri-for-article next-article-name)))
	   (next-article-link-title (when next-article
				      (format nil "~:@(~a~): ~a"
					      (name next-article)
					      (title next-article))))
	   (authors (authors article))
	   (mscs (mscs article))
	   (landmarks (landmarks-for-article article))
	   (num-fragments (length (fragments article)))
	   (source-uri (format nil "/~a.miz" article-name))
	   (source-link-title (format nil "Source of ~:@(~a~)" (name article)))
	   (title (or (title article)
		      "(title information missing)")))
      (miz-item-html (title)
	  nil
	(htm
	 (:table
	  (:tr
	   (:td
	    ((:table :class "item-table")
	     (:tr
	      ((:td :width "25%" :align "left")
	       ((:table :class "item-info")
		((:tr :class "item-info-row")
		 ((:td :colspan "2" :class "item-info-heading") "Article Info"))
		((:tr :class "item-info-row")
		 ((:td :class "item-info-key") "Name")
		 ((:td :class "article-name")
		  (str article-name)))
		((:tr :class "item-info-row")
		 ((:td :class "item-info-key") "Title")
		 ((:td :class "article-title")
		  (str title)))
		((:tr :class "item-info-row")
		 ((:td :class "item-info-key") "Fragments")
		 ((:td :class "item-info-value") (str num-fragments)))
		((:tr :class "item-info-row")
		 ((:td :class "item-info-key") "Position in MML")
		 ((:td :class "item-info-value")
		  "[" (if prev-article
			  (htm ((:a :href prev-article-uri :title prev-article-link-title) "&lt;"))
			  (htm "&lt"))
		  "]"
		  " "
		  (str article-pos-in-mml-lar)
		  " "
		  "[" (if next-article
			  (htm ((:a :href next-article-uri :title next-article-link-title) "&gt;"))
			  (htm "&gt;"))
		  "]"))
		((:tr :class "item-info-row")
		 ((:td :colspan "2" :class "item-info-heading") "Authors"))
		((:tr :class "item-info-row")
		 ((:td :colspan "2")
		  (if authors
		      (htm
		       ((:ul :class :path-options)
			(dolist (author authors)
			  (let* ((author-link-title (format nil "Articles by ~a" author))
				 (author-uri (uri-for-author author)))
			    (htm
			     (:li
			      ((:a :href author-uri :title author-link-title)
			       (str author))))))))
		      (htm (str "(author information unknown)")))))
		((:tr :class "item-info-row")
		 ((:td :colspan "2" :class "item-info-heading") "Mathematics Subject Classification"))
		((:tr :class "item-info-row")
		 ((:td :colspan "2")
		  (if mscs
		      (htm
		       ((:ul :class :path-options)
			(dolist (msc mscs)
			  (let* ((msc-link-title (msc-pretty msc))
				 (msc-uri (uri-for-msc author)))
			    (htm
			     (:li
			      ((:a :href msc-uri :title msc-link-title)
			       (str msc))))))))
		      (htm (str "(author information unknown)")))))
		((:tr :class "item-info-row")
		 ((:td :colspan "2" :class "item-info-heading") "Landmarks"))
		((:tr :class "item-info-row")
		 ((:td :colspan "2")
		  (if landmarks
		      (htm (str "at least one!"))
		      (htm (:em "(none)")))))
		((:tr :class "item-info-row")
		 ((:td :colspan "2" :class "item-info-heading") "Actions"))
		((:tr :class "item-info-row")
		 ((:td :colspan "2")
		  ((:ul :class "path-options")
		   (:li ((:a :href source-uri :title source-link-title)
			 "Get the source for this article")))))))
	      ((:td :width "75%" :valign "top")
	       (loop
		  with article-dir = (format nil "~a/~a" (mizar-items-config 'html-source) article)
		  with article-text-dir = (format nil "~a/text" article-dir)
		  for i from 1 upto num-fragments
		  for i-str = (format nil "fragment-~d" i)
		  for fragment-path = (format nil "~a/ckb~d.html" article-text-dir i)
		  for item-html = (file-as-string fragment-path)
		  for item-uri = (format nil "/fragment/~a/~d" article i)
		  for item-link-title = (format nil "Fragment ~d of ~:@(~a~)" i article)
		  do
		    (htm
		     ((:div :class "fragment-listing" :id i-str)
		      
		      ((:a :href item-uri :title item-link-title))
		      (str item-html)))))))))))))))

(defun http-sensitive-redirect (new-uri)
  (let ((client-server-protocol (server-protocol*)))	
    (redirect new-uri
	      :code (if (string= client-server-protocol "HTTP/1.1")
			+http-temporary-redirect+
			+http-moved-temporarily+))))

(defun emit-random-item ()
  (let ((item (random-elt (hash-table-keys *item-dependency-graph-forward*))))
    (http-sensitive-redirect (uri-for-item-as-symbol item))))

(defmacro register-static-file-dispatcher (uri path &optional mime-type)
  `(progn
     (unless (file-exists-p ,path)
       (warn "Can't register URI '~a' to point to '~a', because there's no file at that path" ,uri ,path))
     (push (create-static-file-dispatcher-and-handler ,uri 
						      ,path
						      ,mime-type)
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

(defun split-item-identifier (item-string)
  (split ":" item-string))

(defun xml-path-for-item (item-string)
  (destructuring-bind (article-name item-kind item-number)
      (split-item-identifier item-string)
    (let* ((item-key (format nil "~a:~a:~a" article-name item-kind item-number))
	   (ckb-for-item (gethash item-key *item-to-fragment-table*))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir)))
      (when ckb-for-item
	(destructuring-bind (ckb-article-name ckb-number)
	    (split-item-identifier ckb-for-item)
	  (declare (ignore ckb-article-name)) ;; same as ARTICLE
	  (format nil "~a/ckb~d.xml" article-text-dir ckb-number))))))

(defun xml-path-for-article (article-name)
  (let ((article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name)))
    (format nil "~a/~a.xml" article-dir article-name)))

(defun html-path-for-article (article-name)
  (let ((article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name)))
    (format nil "~a/~a.html" article-dir article-name)))

(defun html-path-for-fragment (fragment-string)
  (destructuring-bind (article-name fragment-number)
      (split-item-identifier fragment-string)
    (let* ((article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir)))
      (format nil "~a/ckb~d.html" article-text-dir fragment-number))))

(defun html-path-for-item (item-string)
  (destructuring-bind (article-name item-kind item-number)
      (split-item-identifier item-string)
    (let* ((item-key (item-from-components article-name item-kind item-number))
	   (item (get-and-maybe-set-item-name item-key))
	   (ckb-number (gethash item *item-to-fragment-table*))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir)))
      (if ckb-number
	  (format nil "~a/ckb~d.html" article-text-dir ckb-number)
	  (error "There is no known fragment for the item '~a'" item-string)))))

(defgeneric html-for-item (item))

(defmethod html-for-item ((item symbol))
  (html-for-item (symbol-name item)))

(defmethod html-for-item ((item-string string))
  (let ((path (html-path-for-item item-string)))
    (when (file-exists-p path)
      (file-as-string path))))

;; (defun html-for-item (item-string)
;;   (mhtml (xml-path-for-item item-string)))

(defun html-for-article (article-name)
  (file-as-string (html-path-for-article article-name)))

;; (defun html-for-article (article-name)
;;   (mhtml (xml-path-for-article article-name)))

(defun pretty-item-kind (item-kind)
  (switch (item-kind :test #'string=)
    ("definiens" "definiens")
    ("deftheorem" "definitional theorem")
    ("kconstructor" "function definition")
    ("mconstructor" "type definition")
    ("rconstructor" "relation definition")
    ("vconstructor" "adjective definition")
    ("ccluster" "registration")
    ("fcluster" "registration")
    ("rcluster" "registration")
    ("kpattern" "notation (function)")
    ("mpattern" "notation (type)")
    ("rpattern" "notation (relation)")
    ("vpattern" "notation (adjective)")
    ("theorem" "theorem")
    ("lemma" "lemma")
    ("scheme" "scheme")
    ("identification" "identification")
    (t "(unknown)")))

(defun requires-uri-for-item (article-name item-kind item-number)
  (format nil "/item/~a/~a/~a/requires" article-name item-kind item-number))

(defun supports-uri-for-item (article-name item-kind item-number)
  (format nil "/item/~a/~a/~a/supports" article-name item-kind item-number))

(defun search-from-uri (item)
  (format nil "/path?from=~a" item))

(defun search-to-uri (item)
  (format nil "/path?to=~a" item))

(defun search-via-uri (item)
  (format nil "/path?via=~a" item))

(defgeneric emit-item-page ()
  (:documentation "Emit an HTML representation of a single Mizar item."))

(defmethod emit-item-page :around ()
  (register-groups-bind (article-name item-kind item-number-str)
      (+item-uri-regexp+ (request-uri*))
    (let ((item (item-from-components article-name item-kind item-number-str)))
      (if (known-item? item)
	  (let ((html-path (html-path-for-item item))
		(item-name-pretty (item-inline-name article-name item-kind item-number-str)))
	    (if (file-exists-p html-path)
		(if (empty-file-p html-path)
		    (miz-item-html ("error")
			(:return-code +http-internal-server-error+)
		      (:p "The HTML file for the item " (str item-name-pretty) " exists, but has zero size.")
		      (:p "Please inform the site maintainers about this."))
		    (call-next-method))
		(miz-item-html ("error")
		    (:return-code +http-internal-server-error+)
		  (:p "The HTML file for the item " (str item-name-pretty) " does not exist.")
		  (:p "Please inform the site maintainers about this."))))
	  (miz-item-html ("error")
	      (:return-code +http-bad-request+)
	    (:p "The identifier")
	    (:blockquote
	     (str item))
	    (:p "is not the name of a known item"))))))

(defun absolute-uri-for-item (article kind number)
  "Use *MML-VERSION* to give an absolute location for the item determined by ARTICLE, KIND, and NUMBER.

We currently handle only one version of the MML to be around at any
one time; later, when we do support multiple MMLs, this will be useful."
  (format nil "/item/~a/~a/~a" article kind number))

(defmethod emit-item-page ()
  (register-groups-bind (article-name item-kind item-number-str)
      (+item-uri-regexp+ (request-uri*))
    (let* ((item-number (parse-integer item-number-str))
	   (item-kind-pretty (pretty-item-kind item-kind))
	   (article-uri (uri-for-article article-name))
	   (article (find article-name *mml-lar* :key #'name :test #'string=))
	   (num-items-for-article (length (fragments article)))
	   (item-key (format nil "~a:~a:~d" article-name item-kind item-number))
	   (item (get-and-maybe-set-item-name item-key))
	   (ckb-number (gethash item *item-to-fragment-table*))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir))
	   (article-name-uc (format nil "~:@(~a~)" article-name))
	   (fragment-path (format nil "~a/ckb~d.html"
				  article-text-dir
				  ckb-number))
	   (item-html (file-as-string fragment-path))
	   (absolute-uri (absolute-uri-for-item article-name item-kind item-number-str))
	   (prev-ckb-uri (unless (= ckb-number 1)
			   (uri-for-fragment article-name (1- ckb-number))))
	   (prev-ckb-link-title (format nil "Previous fragment in ~:@(~a~)" article-name))
	   (next-ckb-uri (unless (= ckb-number num-items-for-article)
			   (uri-for-fragment article-name (1+ ckb-number))))
	   (next-ckb-link-title (format nil "Next fragment in ~:@(~a~)" article-name))
	   (prev-item-kind-uri (unless (= item-number 1)
				 (uri-for-item article-name item-kind (1- item-number))))
	   (prev-item-kind-link-title (format nil "Previous item of this kind in ~:@(~a~)" article-name))
	   (next-item-kind-uri (unless (= item-number (count-items-of-kind-for-article article-name item-kind))
				 (uri-for-item article-name item-kind (1+ item-number))))
	   (next-item-kind-link-title (format nil "Next item of this kind in ~:@(~a~)" article-name))
	   (search-from-uri (search-from-uri item-key))
	   (search-from-link-title (format nil "Search for paths of dependence starting from ~:@(~a~):~a:~d" article-name item-kind item-number))
	   (search-to-uri (search-to-uri item-key))
	   (search-to-link-title (format nil "Search for paths of dependence ending at ~:@(~a~):~a:~d" article-name item-kind item-number))
	   (search-via-uri (search-via-uri item-key))
	   (search-via-link-title (format nil "Search for paths of dependence passing through ~:@(~a~):~a:~d" article-name item-kind item-number))
	   (requires-uri (requires-uri-for-item article-name item-kind item-number))
	   (supports-uri (supports-uri-for-item article-name item-kind item-number)))
      (miz-item-html (item-key)
	  (:content-location absolute-uri)
	((:table :class "item-table")
	 (:tr
	  ((:td :width "25%" :align "left")
	   ((:table :class "item-info")
	    ((:tr :class "item-info-row")
	     ((:td :colspan "2" :class "item-info-heading") "Item Info"))
	    ((:tr :class "item-info-row")
	     ((:td :class "item-info-key") "Article")
	     ((:td :class "item-info-value")
	      ((:a :href article-uri :class "article-name" :title article-name-uc)
	       ((:span :class "article-name") (str article-name)))))
	    ((:tr :class "item-info-row")
	     ((:td :class "item-info-key") "Fragment")
	     ((:td :class "item-info-value")
	      "[" (if prev-ckb-uri
		      (htm ((:a :href prev-ckb-uri :title prev-ckb-link-title) "&lt;"))
		      (htm "&lt;"))
	      "]"
	      " "
	      (str ckb-number)
	      " "
	      "[" (if next-ckb-uri
		      (htm ((:a :href next-ckb-uri :title next-ckb-link-title) "&gt;"))
		      (htm "&gt;"))
	      "]"))
	    ((:tr :class "item-info-row")
	     ((:td :class "item-info-key") "Item Kind")
	     ((:td :class "item-info-value") (str item-kind-pretty)))
	    ((:tr :class "item-info-row")
	     ((:td :class "item-info-key") "Number")
	     ((:td :class "item-info-value")
	      "[" (if prev-item-kind-uri
		      (htm ((:a :href prev-item-kind-uri :title prev-item-kind-link-title) "&lt;"))
		      (htm "&lt"))
	      "]"
	      " "
	      (str item-number-str)
	      " "
	      "[" (if next-item-kind-uri
		      (htm ((:a :href next-item-kind-uri :title next-item-kind-link-title) "&gt;"))
		      (htm "&gt;"))
	      "]"))
	    (when (constructor-item? item)
	      (htm
	       ((:tr :class "item-info-row")
		((:td :colspan "2" :class "item-info-heading") "Properties"))
	       ((:tr :class "item-info-row")
		((:td :colspan "2") (:em "(none known)")))))
	    ((:tr :class "item-info-row")
	     ((:td :colspan "2" :class "item-info-heading") "Actions"))
	    ((:tr :class "item-info-row")
	     ((:td :colspan "2") "Search for a path of dependence:"
	      ((:ul :class "path-options")
	       (:li ((:a :href search-from-uri :title search-from-link-title) (:b "starting at")))
	       (:li ((:a :href search-via-uri :title search-via-link-title) (:b "passing through")))
	       (:li ((:a :href search-to-uri :title search-to-link-title) (:b "ending at"))))
	      "this item."))
	    ((:tr :class "item-info-row")
	     ((:td :colspan "2") "See what this item"
	      ((:ul :class "item-dep-options")
	       (:li ((:a :href requires-uri :title "Items on which this item depends") (:b "requires")))
	       (:li ((:a :href supports-uri :title "Items that this item supports") (:b "supports"))))))))
	  ((:td :width "75%" :valign "top")
	   (str item-html))))))))

(defgeneric emit-fragment-page ()
  (:documentation "Emit an HTML representation of a fragment of a Mizar article."))

(defmethod emit-fragment-page :around ()
  (register-groups-bind (article-name fragment-number-str)
      (+fragment-uri-regexp+ (request-uri*))
    (let ((fragment (fragment-from-components article-name fragment-number-str)))
      (if (fragment->items fragment)
	  (let ((html-path (html-path-for-fragment fragment))
		(fragment-name-pretty (fragment-inline-name article-name fragment-number-str)))
	    (if (file-exists-p html-path)
		(if (empty-file-p html-path)
		    (miz-item-html ("error")
			(:return-code +http-internal-server-error+)
		      (:p "The HTML file for the fragment " (str fragment-name-pretty) " exists, but has zero size.")
		      (:p "Please inform the site maintainers about this."))
		    (call-next-method))
		(miz-item-html ("error")
		    (:return-code +http-internal-server-error+)
		  (:p "The HTML file for the fragment " (str fragment-name-pretty) " does not exist.")
		  (:p "Please inform the site maintainers about this."))))
	  (miz-item-html ("error")
	      (:return-code +http-bad-request+)
	    (:p "The identifier")
	    (:blockquote
	     (str fragment))
	    (:p "is not the name of a known fragment")))
      (miz-item-html ("error")
	  (:return-code +http-bad-request+)
	(:p ((:span :class "article-name") (str article-name)) " is not known, or not yet suitably processed for this site.  Please try again later.")))))

(defmethod emit-fragment-page ()
  (register-groups-bind (article-name fragment-number-str)
      (+fragment-uri-regexp+ (request-uri*))
    (let* ((fragment-number (parse-integer fragment-number-str))
	   (article-uri (uri-for-article article-name))
	   (num-items-for-article (gethash article-name *article-num-items*))
	   (fragment (format nil "~a:~d" article-name fragment-number))
	   (fragment-items (fragment->items fragment))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir))
	   (article-name-uc (format nil "~:@(~a~)" article-name))
	   (fragment-path (format nil "~a/ckb~d.html" article-text-dir fragment-number-str))
	   (fragment-html (file-as-string fragment-path))
	   (prev-ckb-uri (unless (= fragment-number 1)
			   (uri-for-fragment article-name (1- fragment-number))))
	   (prev-ckb-link-title (format nil "Previous fragment in ~:@(~a~)" article-name))
	   (next-ckb-uri (unless (= fragment-number num-items-for-article)
			   (uri-for-fragment article-name (1+ fragment-number))))
	   (next-ckb-link-title (format nil "Next fragment in ~:@(~a~)" article-name)))
	(miz-item-html (fragment)
	    nil
	  ((:table :class "item-table")
	   (:tr
	    ((:td :width "25%" :align "left")
	     ((:table :class "item-info")
	      ((:tr :class "item-info-row")
	       ((:td :colspan "2" :class "item-info-heading") "Fragment Info"))
	      ((:tr :class "item-info-row")
	       ((:td :class "item-info-key") "Article")
	       ((:td :class "item-info-value")
		((:a :href article-uri :class "article-name" :title article-name-uc)
		 ((:span :class "article-name") (str article-name)))))
	      ((:tr :class "item-info-row")
	       ((:td :class "item-info-key") "Number")
	       ((:td :class "item-info-value")
		"[" (if prev-ckb-uri
			(htm ((:a :href prev-ckb-uri :title prev-ckb-link-title) "&lt;"))
			(htm "&lt;"))
		"]"
		" "
		(str fragment-number-str)
		" "
		"[" (if next-ckb-uri
			(htm ((:a :href next-ckb-uri :title next-ckb-link-title) "&gt;"))
			(htm "&gt;"))
		"]"))
	      ((:tr :class "item-info-row")
	       ((:td :colspan "2" :class "item-info-heading") "Generated Items"))
	      ((:tr :class "item-info-row")
	       ((:td :colspan "2" :class "item-info-value")
		(:ul
		 (dolist (gen-item fragment-items)
		   (let ((gen-item-link (link-to-item gen-item)))
		     (htm (:li (str gen-item-link))))))))))
	    ((:td :width "75%" :valign "top")
	     (str fragment-html))))))))

(defun emit-articles-page ()
  (miz-item-html ("articles from the mml")
      nil
    (str (article-listing))))

(defun emit-main-page ()
  (miz-item-html ("fine-grained dependencies in mizar")
      nil
    (:h1 "welcome")
    (:p "Interested in learning more about the " ((:a :href "http://www.mizar.org/") (:tt "MIZAR") " Mathematical Library") " (MML), the largest corpus of formalized mathematics in the world?  This site provides a way to
    get a handle on the large contents of the MML. This site presents the MML by showing its " (:b "items") " and showing, for each item, what it " (:b "depends")  " upon and conversely (what items depend on the item).")
    (:p "The dependency graph that this site lets you explore has " 
	(:b (str (count-items-of-dependency-graph *item-dependency-graph-forward*))) " nodes (items) and "
	(:b (str (count-edges-of-depgraph *item-dependency-graph-forward*))) " edges.")
    (:h1 "getting started")
    (:p "One can visit " ((:a :href "/articles") "the complete list of articles") ".  Alternatively, one can visit " ((:a :href "/random-item") " a random item") " or " ((:a :href "/random-path") "search for a path between two random items") ".")
    (:p "You might want to visit the " ((:a :href "/landmarks") "landmarks") " page to get acquainted with how this site provides fine-grained dependency information for some notable results of mathematics.") 
    (:h1 "learning more about " (:tt "MIZAR"))
    (:p "The " (:tt "MIZAR") " system and its library, the MML, are rather complex.  To learn more about the system, see the excellent overview article")
    (:blockquote
     (:p
      "&ldquo;"
      ((:a :href "http://jfr.cib.unibo.it/article/view/1980") "Mizar in a nutshell")
      "&rdquo;, by Adam Grabowski, Artur Kornilowicz, and Adam Naumowicz, " (:em "Journal of Formalized Reasoning") " " (:b "3") "(2), (2010), pp. 153&ndash;245"))
    (:p "For a historical overview, see:")
    (:blockquote
     (:p
      "&ldquo;"
      ((:a :href "http://markun.cs.shinshu-u.ac.jp/mizar/mma.dir/2005/mma2005(2).pdf") "MIZAR: The first 30 years")
      "&rdquo;, by Roman Mutuszewski and Piotr Rudnicki, " (:em "Mechanized Mathematics and its Applications") " " (:b "4") "(1), (2005), pp. 3&ndash;24"))))

(defun item-uri (item-identifier)
  (format nil "/item/~a" (substitute #\/ #\: item-identifier)))
     
(defun emit-landmarks-page ()
  (miz-item-html ("landmarks")
      nil
    (:p "One might also be interested in entering the vast space of " (:tt "MIZAR") " items by inspecting some landmarks.")
    (:p "This page is divided into the following sections:")
    (:ul
     (:li ((:a :href "#mmlquery") "A selected list taken from the work of the MML Query project"))
     (:li ((:a :href "#100theorems") "100 Theorems")))
    ((:h1 :id "mmlquery")
     "A selected list taken from the work of the MML Query project")
    (:p "The following is a list of theorems selected by the maintainers of the " ((:a :href "http://mmlquery.mizar.org/" :title "MML Query") "MML Query") " project, " ((:a :href "http://webdocs.cs.ualberta.ca/~piotr/Mizar/Important_thms.html" :title "Name-carrying facts/theorems/definitions in MML") "extended by Piotr Rudnicki")  ".")
    (:ul
     (loop
	for (entry items) in +piotr-theorems+
	do
	  (if (stringp items) ; unique reference ("/item/article/theorem/5")
	      (htm
	       (:li ((:a :href items :title entry) (str entry))))
	      (if (stringp (car items))
		  (let ((num-alternatives (length items))) ; list of anonymous alternatives ("/item/article/deftheorem/1" "/item/article/deftheorem/5")
		    (if (> num-alternatives 2) ; count alternatives: "Alternative 1" "Alternative 2", ...
			(let ((primary-alternative (first items))
			      (remaining-alternatives (rest items)))
			  (htm
			   (:li 
			    ((:a :href primary-alternative :title entry) (str entry))
			    "["
			    (let ((second-alternative (car remaining-alternatives))
				  (second-alternative-text "alternative 1")
				  (second-alternative-title (format nil "~a (alternative 1)" entry)))
			      (htm
			       ((:a :href second-alternative :title second-alternative-title)
				(str second-alternative-text))))
			    (loop
			       for alternative in (cdr remaining-alternatives)
			       for i from 2
			       for alternative-text = (format nil "alternative ~d" i)
			       for alternative-title = (format nil "~a (alternative ~d" entry i)
			       do
				 (htm
				  ", "
				  ((:a :href alternative :title alternative-title)
				   (str alternative-text))))
			    "]")))
			(let* ((primary (first items))
			       (alternative (second items)) ; only one alternative
			       (alternative-title (format nil "~a (alternative)" entry)))
			  (htm
			   (:li
			    (str entry)
			    (:ul
			     (:li
			      ((:a :href primary :title entry) "Primary version"))
			     (:li
			      ((:a :href alternative :title alternative-title) "Another version"))))))))
		  (htm  ; list of named alternatives (("First" "/item/article/theorem/25") ("Second" "/item/article/theorem/50") ...)
		   (:li (str entry)
			(:ul
			 (dolist (item items)
			   (destructuring-bind (name uri)
			       item
			     (let ((alternative-name (format nil "~a (~a)"
							     entry 
							     name)))
			       (htm
				(:li ((:a :href uri 
					  :title alternative-name)
				      (str name))))))))))))))
    ((:h1 :id "100theorems")
     "100 Theorems")
    (:p "The following is a list of 'notable' mathematical results formalized in " (:tt "MIZAR") ".  What does 'notable' mean here?  Certainly, it's a value term.  This list comes from Freek Wiedijk's "((:a :href "http://www.cs.ru.nl/~freek/100/" :title "Formalizing 100 Theorems") "100 theorems") " and its associated list of " ((:a :href "http://www.cs.ru.nl/~freek/100/mizar.html" :title "Formalizing 100 Theorems in Mizar") "theorems formalized in " (:tt "MIZAR")) ".  Here is the list, with links to the corresponding entries in this site's database.")
    (:dl
     (loop
	for i from 1 upto 100
	for theorem-name across +100-theorems+
	for theorem-name-escaped = (escape-string theorem-name)
	do
	  (htm
	   ((:dt :class "theorem-name") (fmt "~d. ~a" i theorem-name))
	   (:dd
	    (let ((formalizations (gethash i +mizar-formalized+)))
	      (if formalizations
		  (if (cdr formalizations) ; more than one formalization
		      (htm
		       (:ul
			(dolist (formalization formalizations)
			  (htm
			   (:li
			    (let* ((formalization-uri (item-uri formalization))
				   (formalization-html-path (html-path-for-item formalization)))
			      (if formalization-html-path
				  (if (file-exists-p formalization-html-path)
				      (if (empty-file-p formalization-html-path)
					  (htm "(The HTML representation exists but is empty; please notify the site maintainer.)")
					  (let ((formalization-html (file-as-string formalization-html-path)))
					    (htm ((:a :href formalization-uri
						      :class "mhtml-text"
						      :title theorem-name-escaped))
						 (str formalization-html))))
				      (htm "(HTML representation not present)")))))))))
		      (let* ((formalization (car formalizations))
			     (formalization-uri (item-uri formalization))
			     (formalization-html-path (html-path-for-item formalization)))
			(if formalization-html-path
			    (if (file-exists-p formalization-html-path)
				(if (empty-file-p formalization-html-path)
				    (htm "(The HTML representation exists but is empty; please notify the site maintainer.)")
				    (let ((formalization-html (file-as-string formalization-html-path)))
				      (htm
				       ((:a :href formalization-uri
					    :class "mhtml-text"
					    :title theorem-name-escaped)
					(str formalization-html)))))
				(htm "(HTML representation not present)"))
			    (htm "(HTML representation not present)"))))
		  (htm
		   (:em "(not yet formalized in " (:tt "MIZAR") ")"))))
	    (let ((100theorems-uri (format nil "http://www.cs.ru.nl/~~freek/100/#~d" i))
		  (100theorems-title (format nil "Known formalizations of: ~a" theorem-name-escaped)))
	      (htm
	       (:p "[" ((:a :href 100theorems-uri
			    :title 100theorems-title)
			"other formalizations") "]")))))))))

(defun emit-feedback-page ()
  (miz-item-html ("feedback")
      nil
    (:p
     "Thanks for using this site.  The maintainer is " ((:a :href "http://centria.di.fct.unl.pt/~alama/") "Jesse Alama") ".  If your have questions, comments, bug reports (e.g., broken links), or feature requests, please do " ((:a :href "mailto:jesse.alama@gmail.com") "send an email") "; your feedback is appreciated.")))

(defun items-by-mml-order (item-list)
  (sort (copy-list item-list) #'item-<))

(defun fragment-from-components (article-name item-number)
  (format nil "~a:~a" article-name item-number))

(defun item-from-components (article-name item-kind item-number)
  (format nil "~a:~a:~a" article-name item-kind item-number))

(defun item-link-title (article kind number)
  (format nil "~:@(~a~):~a:~a" article kind number))

(defun item-link-title-from-string (item-string)
  (destructuring-bind (article kind number)
      (split-item-identifier item-string)
    (item-link-title article kind number)))

(defun item-inline-name (article kind number)
  (with-html-output-to-string (foo)
    (:span
     ((:span :class "article-name") (str article))
     ":"
     (str kind)
     ":"
     (str number))))

(defun fragment-inline-name (article number)
  (with-html-output-to-string (foo)
    (:span
     ((:span :class "article-name") (str article))
     ":"
     (str number))))

(defun pretty-print-item (item)
  (destructuring-bind (article kind number)
      (split-item-identifier item)
    (item-inline-name article kind number)))

(defun earlier-in-tsorted-list (item-1 item-2)
  (let ((lar-1 (mml-lar-index-of-item item-1))
	(lar-2 (mml-lar-index-of-item item-2)))
    (cond ((< lar-1 lar-2) t)
	  ((= lar-1 lar-2)
	   (multiple-value-bind (ckb-1 present-1?)
	       (gethash item-1 *item-to-fragment-table*)
	     (if present-1?
		 (multiple-value-bind (ckb-2 present-2?)
		     (gethash item-2 *item-to-fragment-table*)
		   (if present-2?
		       (< ckb-1 ckb-2)
		       (error "We cannot determine whether item '~a' is topologically less than item '~a', coming from the same article, because '~a' cannot be found in the item-to-fragment table" item-1 item-2 item-2)))
		 (error "We cannot determine whether item '~a' is topologically less than item '~a' because '~a', coming from the same article, cannot be found in the item-to-fragment table" item-1 item-2 item-1))))
	  (t nil))))

(defun item-requires-tsorted (item)
  (let* ((requires (gethash item *item-dependency-graph-forward*))
	 (sorted (sort (copy-list requires) #'earlier-in-tsorted-list)))
    sorted))

(defun item-supports-tsorted (item)
  (let* ((requires (gethash item *item-dependency-graph-backward*))
	 (sorted (sort (copy-list requires) #'earlier-in-tsorted-list)))
    sorted))

(defgeneric item-article (item-identifier))

(defmethod item-article ((item symbol))
  (get item 'article))

(defmethod item-article ((item-identifier string))
  (item-article (get-and-maybe-set-item-name item-identifier)))

(defgeneric item-number (item-itemtifier))

(defmethod item-number ((item symbol))
  (get item 'number))

(defmethod item-number ((item-identifier string))
  (destructuring-bind (article kind number)
      (split-item-identifier item-identifier)
    (declare (ignore article kind))
    number))

(defgeneric item-kind (item-itemtifier))

(defmethod item-kind ((item symbol))
  (get item 'kind))

(defmethod item-kind ((item-identifier string))
  (destructuring-bind (article kind number)
      (split-item-identifier item-identifier)
    (declare (ignore article number))
    kind))

(defun items-by-article (item-list)
  (let ((by-article (make-hash-table :test #'equal)))
    (dolist (item item-list)
      (let ((article (item-article item)))
	(push item (gethash article by-article))))
    (loop
       with by-article-as-list = nil
       for k being the hash-keys in by-article
       do
	 (push (cons k (gethash k by-article))
	       by-article-as-list)
       finally
	 (return by-article-as-list))))

(defun items-by-article-in-mml-lar-order (item-list)
  (let* ((by-article (items-by-article item-list))
	 (sorted (sort by-article #'mml-< :key #'first)))
    sorted))

(defgeneric emit-requires-page ()
  (:documentation "An HTML presentation of a page showing what a given item requires"))

(defmethod emit-requires-page :around ()
  (register-groups-bind (article kind number)
      (+requires-uri-regexp+ (request-uri*))
    (if (belongs-to-mml article)
	(let ((item (item-from-components article kind number)))
	  (if (known-item? item)
	      (call-next-method)
	      (let ((article-uri (uri-for-article article)))
		(miz-item-html ("unknown item")
		    (:return-code +http-not-found+)
		  (:p "The item "
		      ((:a :href article-uri :class "article-name")
		       (str article))
		      ":"
		      (str kind)
		      ":"
		      (str number)
		      " is not known.")))))
	(miz-item-html ("unknown article")
	    (:return-code +http-not-found+)
	  (:p (fmt "'~a'" article) " is not the name of a known article.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-requires-page ()
  (register-groups-bind (article kind number)
      (+requires-uri-regexp+ (request-uri*))
    (let* ((item (item-from-components article kind number))
	   (item-html (html-for-item item))
	   (item-link-title (item-link-title article kind number))
	   (deps (item-requires-tsorted (get-and-maybe-set-item-name item)))
	   (requires-page-title (format nil "requirements of ~a" item-link-title)))
      (miz-item-html (requires-page-title)
	  nil
	((:table :class "item-table")
	 (:tr
	  ((:td :width "25%" :align "left")
	   ((:table :class "item-info")
	    ((:tr :class "item-info-row")
	     ((:td :colspan "2" :class "item-info-heading") "Dependence Info"))
	    ((:tr :class "item-info-row")
	     ((:td :class "item-info-key") "Item")
	     ((:td :class "item-info-value") (str (link-to-item-with-components article kind number))))))
	  ((:td :width "75%" :valign "top")
	   (:div
	    (str item-html))
	   (if (null deps)
	    (htm (:p (:em "(This item requires nothing.)")))
	    (htm (:ul
		  (loop
		     for (article . deps-from-article) in (items-by-article-in-mml-lar-order deps)
		     do
		       (htm
			(:li ((:span :class "article-name") (str article))
			     (:ul
			      (dolist (dep deps-from-article)
				(let* ((dep-article (item-article dep))
				       (dep-kind (item-kind dep))
				       (dep-num (item-number dep))
				       (dep-uri (dependence-uri-for-items item dep))
				       (dep-link-title (dependence-link-title item dep))
				       (dep-inline-name (item-inline-name dep-article dep-kind dep-num)))
				  (htm
				   (:li ((:a :href dep-uri :title dep-link-title)
					 (str dep-inline-name))))))))))))))))))))

(defgeneric emit-supports-page ()
  (:documentation "An HTML presentation of the items that a given item supports."))

(defmethod emit-supports-page :around ()
  (register-groups-bind (article kind number)
      (+supports-uri-regexp+ (request-uri*))
    (if (belongs-to-mml article)
	(let ((item (item-from-components article kind number)))
	  (if (known-item? item)
	      (call-next-method)
	      (let ((article-uri (uri-for-article article)))
		(miz-item-html ("unknown item")
		    (:return-code +http-not-found+)
		  (:p "The item "
		      ((:a :href article-uri :class "article-name")
		       (str article))
		      ":"
		      (str kind)
		      ":"
		      (str number)
		      " is not known.")))))
	(miz-item-html ("unknown article")
	    (:return-code +http-not-found+)
	  (:p (fmt "'~a'" article) " is not the name of a known article.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-supports-page ()
  (register-groups-bind (article kind number)
      (+supports-uri-regexp+ (request-uri*))
    (let* ((item (item-from-components article kind number))
	   (item-html (html-for-item item))
	   (item-link-title (item-link-title article kind number))
	   (supports (item-supports-tsorted (get-and-maybe-set-item-name item)))
	   (supports-page-title (format nil "what ~a supports" item-link-title)))
      (miz-item-html (supports-page-title)
	  nil
	((:table :class "item-table")
	 (:tr
	  ((:td :width "25%" :align "left")
	   ((:table :class "item-info")
	    ((:tr :class "item-info-row")
	     ((:td :colspan "2" :class "item-info-heading") "Dependence Info"))
	    ((:tr :class "item-info-row")
	     ((:td :class "item-info-key") "Item")
	     ((:td :class "item-info-value") (str (link-to-item-with-components article kind number))))))
	  ((:td :width "75%" :valign "top")
	   (:div
	    (str item-html))
	   (if (null supports)
	    (htm (:p (:em "(This item supports nothing.)")))
	    (htm (:ul
		  (loop
		     for (article . supports-from-article) in (items-by-article-in-mml-lar-order supports)
		     do
		       (htm
			(:li ((:span :class "article-name") (str article))
			     (:ul
			      (dolist (dep supports-from-article)
				(let* ((dep-article (item-article dep))
				       (dep-kind (item-kind dep))
				       (dep-num (item-number dep))
				       (dep-uri (dependence-uri-for-items dep item))
				       (dep-link-title (dependence-link-title dep item))
				       (dep-inline-name (item-inline-name dep-article dep-kind dep-num)))
				  (htm
				   (:li ((:a :href dep-uri :title dep-link-title)
					 (str dep-inline-name))))))))))))))))))))

(defgeneric emit-dependence-page ()
  (:documentation "Emit an HTML description of the dependence between two items."))

(defmethod emit-dependence-page :around ()
  ;; sanity check
  (register-groups-bind (supporting-item whatever ignoreme dependent-item)
      (+dependence-uri-regexp+ (request-uri*))
    (declare (ignore whatever ignoreme)) ;; WHATEVER matches the item kind inside SUPPORTING-ITEM
    (if supporting-item
	(if dependent-item
	    (if (known-item? supporting-item)
		(if (known-item? dependent-item)
		    (let ((supp-html-path (html-path-for-item supporting-item))
			  (dep-html-path (html-path-for-item dependent-item)))
		      (if (file-exists-p supp-html-path)
			  (if (file-exists-p dep-html-path)
			      (call-next-method)
			      (miz-item-html ("error")
				  (:return-code +http-internal-server-error+)
				(:p "The HTML file for the item")
				(:blockquote
				 (fmt "~a" dependent-item))
				(:p "doesn't exist at the expected location")
				(:blockquote
				 (fmt "~a" dep-html-path))
				(:p "Please inform the site administrator about this.")))
			  (miz-item-html ("error")
			      (:return-code +http-internal-server-error+)
			    (:p "The HTML file for the item")
			    (:blockquote
			     (fmt "~a" supporting-item))
			    (:p "doesn't exist at the expected location")
			    (:blockquote
			     (fmt "~a" supp-html-path))
			    (:p "Please inform the site administrator about this."))))
		    (miz-item-html ("error")
			(:return-code +http-bad-request+)
		      (:p (fmt "'~a' is not the name of a known item." dependent-item))))
		(miz-item-html ("error")
		    (:return-code +http-bad-request+)
		  (:p (fmt "'~a' is not the name of a known item." supporting-item))))
	    (miz-item-html ("error")
		(:return-code +http-bad-request+)
	      (:p (fmt "Something went wrong matching the requested URI, '~a', against the pattern for this page" (request-uri*)))))
	(miz-item-html ("error")
	    (:return-code +http-bad-request+)
	  (:p (fmt "Something went wrong matching the requested URI, '~a', against the pattern for this page" (request-uri*)))))))

(defmethod emit-dependence-page ()
  (register-groups-bind (supporting-item whatever ignoreme dependent-item)
      (+dependence-uri-regexp+ (request-uri*))
    (declare (ignore whatever ignoreme)) ;; WHATEVER matches the item kind inside SUPPORTING-ITEM
    (destructuring-bind (supp-article supp-kind supp-num)
	(split-item-identifier supporting-item)
      (destructuring-bind (dep-article dep-kind dep-num)
	  (split-item-identifier dependent-item)
	(let* ((supp-html (html-for-item supporting-item))
	       (dep-html (html-for-item dependent-item))
	       (supp-title (item-link-title supp-article supp-kind supp-num))
	       (dep-title (item-link-title dep-article dep-kind dep-num))
	       (title (format nil "~a depends on ~a" supp-title dep-title))
	       (supp-name-inline (item-inline-name supp-article supp-kind supp-num))
	       (dep-name-inline (item-inline-name dep-article dep-kind dep-num))
	       (supp-uri (item-uri supporting-item))
	       (dep-uri (item-uri dependent-item)))
      (miz-item-html (title)
	  nil
	(:p "The item " ((:a :href supp-uri :title supp-title) (str supp-name-inline)))
	(:div (str supp-html))
	(:p "depends on " ((:a :href dep-uri :title dep-title) (str dep-name-inline)))
	(:div (str dep-html))
	(:p "because the attempt to verify "
	    ((:a :href supp-uri :title supp-title) (str supp-name-inline))
	    " in the absence of "
	    ((:a :href dep-uri :title dep-title) (str dep-name-inline))
	    " would generate the following " (:tt "MIZAR") " errors:")
	(:blockquote
	 (:em "(not implemented yet; check back soon)"))))))))

(defvar *mptp-table* (make-hash-table :test #'equal))

(defgeneric emit-sufficiency-page ()
  (:documentation "A page that represents the task of showing that the
  set of items that we claim is minimally sufficient for justifying a
  given item really is sufficient."))

(defmethod emit-sufficiency-page :around ())

(defmethod emit-sufficiency-page ()
  (register-groups-bind (item)
      (+sufficiency-uri-regexp+ (request-uri*))
    (let ((dep-problem (verify-immediate-dependence-problem item *mptp-table*)))
      (miz-item-html ("verify sufficiency")
	  nil
	(:p "To verify the sufficiency of the claimed dependencies for
	item, we will post the following problem to the SystemOnTPTP site:")
	(:ul
	 (dolist (dep-problem-formula dep-problem)
	   (htm (:li (str dep-problem-formula)))))
	(:p "Does that look right?")))))

(defgeneric emit-minimality-page ()
  (:documentation "A page that represents the task of showing that the set of items that is claimed to be minimally sufficient to (re)justify a given item really is minimal."))

(defmethod emit-minimality-page :around ())

(defmethod emit-minimality-page ()
  (register-groups-bind (item)
      (+minimality-uri-regexp+ (request-uri*))
    (let ((title (format nil "verify the minimality of the set of dependencies for ~a" item)))
      (miz-item-html (title)
	  nil
	(:p "To verify that...")))))

(defun initialize-uris ()
  ;; ecmascript, css
  (register-static-file-dispatcher "/mhtml.css"
				   (mizar-items-config 'mhtml-css-path)
				   "text/css")
  (register-static-file-dispatcher "/mhtml.js"
				   (mizar-items-config 'mhtml-js-path)
				   "text/ecmascript")
  (register-static-file-dispatcher "/full-item-depgraph"
				   (mizar-items-config 'full-item-dependency-graph)
				   "text/plain")
  (register-static-file-dispatcher "/item-to-fragment-table"
				   (mizar-items-config 'item-to-fragment-path)
				   "text/plain")
  (register-static-file-dispatcher "/full-vertex-neighbors-depgraph-backward"
				   (mizar-items-config 'vertex-neighbors-backward-graph-path)
				   "text/plain")
  (register-static-file-dispatcher "/full-vertex-neighbors-depgraph-forward"
				   (mizar-items-config 'vertex-neighbors-forward-graph-path)
				   "text/plain")
  ;; intro
  (register-exact-uri-dispatcher "/" #'emit-main-page)
  ;; about page
  (register-exact-uri-dispatcher "/about" #'emit-about-page)
  ;; feedback page
  (register-exact-uri-dispatcher "/feedback" #'emit-feedback-page)
  ;; articles page
  (register-exact-uri-dispatcher "/articles" #'emit-articles-page)
  (register-exact-uri-dispatcher "/landmarks" #'emit-landmarks-page)
  (register-exact-uri-dispatcher "/random-item" #'emit-random-item)
  (register-static-file-dispatcher "/favicon.ico" (mizar-items-config 'favicon-path))
  (loop
     for article in *mml-lar*
     for article-name = (name article)
     do
       (let* ((article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	      (miz-uri (format nil "/~a.miz" article-name))
	      (miz-path (format nil "~a/~a.miz" article-dir article-name))
	      (prel-dir-uri (format nil "/~a/prel/" article-name))
	      (prel-dir-path (format nil "~a/prel/" article-dir))
	      (text-dir-uri (format nil "/~a/text" article-name))
	      (text-dir-path (format nil "~a/text/" article-dir)))
	 ;; static files for the whole article
	 (register-static-file-dispatcher miz-uri miz-path "text/plain")
	 (hunchentoot-dir-lister:add-simple-lister prel-dir-uri prel-dir-path)
	 (hunchentoot-dir-lister:add-simple-lister text-dir-uri text-dir-path)))

  ;; three presentations of an article: itemized HTML, unitemized HTML, raw XML
  (register-regexp-dispatcher +itemized-article-uri-regexp+ #'emit-itemized-article-page)
  (register-regexp-dispatcher +article-html-uri-regexp+ #'emit-unitemized-article-page)
  (register-regexp-dispatcher +article-xml-uri-regexp+ #'emit-article-xml)

  ;; fragments
  (register-regexp-dispatcher +fragment-uri-regexp+ #'emit-fragment-page)

  ;; items, what they require, and what they support
  (register-regexp-dispatcher +item-uri-regexp+ #'emit-item-page)
  (register-regexp-dispatcher +requires-uri-regexp+ #'emit-requires-page)
  (register-regexp-dispatcher +supports-uri-regexp+ #'emit-supports-page)

  ;; paths
  (register-regexp-dispatcher +path-between-items-form-regexp+
			      #'emit-path-between-items-form)
  (register-regexp-dispatcher +path-between-items-uri-regexp+
			      #'emit-path-between-items)

  ;; dependence between items
  (register-regexp-dispatcher +dependence-uri-regexp+ #'emit-dependence-page)

  ;; sufficiency and minimality
  (register-regexp-dispatcher +sufficiency-uri-regexp+ #'emit-sufficiency-page)
  (register-regexp-dispatcher +minimality-uri-regexp+ #'emit-minimality-page)

  t)
