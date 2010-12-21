;;; article.lisp A representation of mizar articles

(in-package :mizar)

(defclass article ()
  ((name
    :initarg :name
    :accessor name
    :type string
    :documentation "The name of this article, as a string.")
   (path
    :initarg :path
    :reader path
    :type pathname)
   (vocabularies
    :initarg :vocabularies
    :accessor vocabularies
    :type list)
   (notations
    :initarg :notations
    :accessor notations
    :type list)
   (constructors
    :initarg :constructors
    :accessor constructors
    :type list)
   (requirements
    :initarg :requirements
    :accessor requirements
    :type list)
   (definitions
     :initarg :definitions
     :accessor definitions
     :type list)
   (theorems
    :initarg :theorems
    :accessor theorems
    :type list)
   (schemes
    :initarg :schemes
    :accessor schemes
    :type list)
   (lines :initarg :lines
	  :accessor lines
	  :type list)
   (xml-doc :initarg :xml-doc
	    :accessor xml-doc)
   (idx-table :initarg :idx-table
	      :accessor idx-table
	      :type hash-table))
  (:documentation "A representation of a mizar article."))

(defun line-at (article line-number)
  (nth (1- line-number) 
       (lines article))) ; start counting lines at 1

(defun refresh-text (article)
  "Ensure that the LINES slot of ARTICLE accurately reflects the
  contents of the file pointed to by ARTICLE's PATH slot."
  (setf (lines article) (lines-in-file (path article)))
  article)

(defun article-xml-path (article)
  (replace-extension (path article) "miz" "xml"))

(defun refresh-xml (article)
  (let ((article-xml-path (article-xml-path article)))
    (if (probe-file article-xml-path)
	(setf (xml-doc article)
	      (cxml:parse-file article-xml-path
			       (cxml-dom:make-dom-builder)))
	(error "Cannot refresh XML for article ~S because no XML file for the article exists!" article))
    article))

(defun refresh-idx (article)
  (let ((article-idx-path (replace-extension (path article) "miz" "idx"))
	(idx-table (make-hash-table :test #'eq)))
    (if (probe-file article-idx-path)
	(let ((idx-doc (cxml:parse-file article-idx-path
					(cxml-dom:make-dom-builder))))
	  (xpath:do-node-set (symbol-node
			      (xpath:evaluate "/Symbols/Symbol" idx-doc))
	    (setf (gethash (value-of-nr-attribute symbol-node) idx-table)
		  (value-of-name-attribute symbol-node)))
	  (setf (idx-table article) idx-table)
	  article)
	(error "No IDX file for article under ~S" (path article)))))

(defun evl (evl-path)
  (if (probe-file evl-path)
      (let ((environ (make-hash-table :test #'equal))) ; keys will be strings
	(let ((doc (cxml:parse-file evl-path (cxml-dom:make-dom-builder))))
	  (xpath:do-node-set (directive-node
			      (xpath:evaluate "/Environ/Directive" doc))
	    (let ((directive-name (value-of-name-attribute directive-node)))
	      (setf (gethash directive-name environ)
	    	    (mapcar #'value-of-name-attribute
	    		    (xpath:all-nodes (xpath:evaluate "Ident"
	    						     directive-node)))))))
	environ)
      (error "No EVL file at ~S" evl-path)))

(defmethod initialize-instance :after ((article article) &key)
  ;; if only a name is given, attempt to fetch the article
  ;; environment, and set the values of the appropriate slots
  (when (and (slot-boundp article 'path)
	     (not (slot-boundp article 'vocabularies))
	     (not (slot-boundp article 'notations))
	     (not (slot-boundp article 'requirements))
	     (not (slot-boundp article 'constructors))
	     (not (slot-boundp article 'definitions))
	     (not (slot-boundp article 'theorems))
	     (not (slot-boundp article 'schemes)))
    (envget article "-l")
    (let* ((path (path article))
	   (evl-path (replace-extension path "miz" "evl"))
	   (environment-as-table (evl evl-path)))
      (setf (vocabularies article)
	    (gethash "Vocabularies" environment-as-table))
      (setf (notations article)
	    (gethash "Notations" environment-as-table))
      (setf (requirements article)
	    (gethash "Requirements" environment-as-table))
      (setf (constructors article)
	    (gethash "Constructors" environment-as-table))
      (setf (definitions article)
	    (gethash "Definitions" environment-as-table))
      (setf (theorems article)
	    (gethash "Theorems" environment-as-table))
      (setf (schemes article)
	    (gethash "Schemes" environment-as-table))))
  (unless (slot-boundp article 'lines)
    (refresh-text article))
  article)

(defun article-lines-matching (regex article)
  "Return a list of triples (LINE COLUMN MATCH), where LINE and COLUMN
  are the line and column within ARTICLE that matches the regular
  expression REGEX; MATCH is the matched string."
  (let ((scanner (create-scanner regex))
	(lines (lines article))
	(result nil))
    (dolist (line lines (reverse result))
      ; doesn't look for multiple occurences of REGEX within line
      ; for an elegant solution, we should have a multiple-value do/while/until
      (multiple-value-bind (line-num column-num)
	  (scan scanner line)
	(when (and (integerp line-num) (integerp column-num))
	  (push (list line-num column-num (subseq line line-num column-num))
		result))))))

(defun region (article begin-line-num begin-col-num end-line-num end-col-num)
  (loop 
     with newline = (make-string 1 :initial-element #\Newline)
     for l from begin-line-num upto end-line-num
     for maybe-line = (line-at article l)
     collecting (if (= l begin-line-num)
		    (if (= begin-line-num end-line-num)
			(subseq maybe-line begin-col-num end-col-num)
			(concat (subseq maybe-line begin-col-num) newline))
		    (if (= l end-line-num)
			(subseq maybe-line 0 end-col-num)
			(concat maybe-line newline))) into lines
     finally (return (apply #'concat lines))))

(defgeneric complete-environment (article)
  (:documentation "All articles mentioned in the environment of
ARTICLE.  The notations, constructors, requirements, definitions,
theorems, and schemes directives are consulted; the vocabularies
directive is not consulted."))

(defmethod complete-environment ((article article))
  (remove-duplicates (append (notations article)
			     (constructors article)
			     (requirements article)
			     (definitions article)
			     (theorems article)
			     (schemes article))))

(defmethod complete-environment ((article-name string))
  (complete-environment (make-instance 'article
				       :name article-name)))

;;; article.lisp ends here