
(in-package :mizar)

(defgeneric constructors-and-properties (atr-file)
  (:documentation "The constructors in ATR-FILE that have at least one property attached to them."))

(defmethod constructors-and-properties ((atr-file string))
  (constructors-and-properties (pathname atr-file)))

(defmethod constructors-and-properties :around (atr-file)
  (let ((sheet (mizar-items-config 'propertied-constructors-stylesheet)))
    (if (file-exists-p sheet)
	(call-next-method)
	(error "The stylesheet for listing constructors and their properties doesn't exist at the expected location '~a' " sheet))))

(defparameter *propertied-constructors-stylesheet*
  (xuriella:parse-stylesheet (pathname (mizar-items-config 'propertied-constructors-stylesheet))))

(defmethod constructors-and-properties ((atr-file pathname))
  (xuriella:apply-stylesheet *propertied-constructors-stylesheet* atr-file))

(defgeneric constructors-with-properties (atr-file))

(defmethod constructors-with-properties ((atr-file string))
  (constructors-with-properties (pathname atr-file)))

(defmethod constructors-with-properties ((atr-file pathname))
  (loop
     with propertied-constructors = nil
     for line in (split #\Newline (constructors-and-properties atr-file))
     do
       (destructuring-bind (constructor . properties)
	   (split #\Space line)
	 (when properties
	   (push (cons constructor properties) propertied-constructors)))
     finally
     (return propertied-constructors)))

(defgeneric remove-property-from-constructor (constructor-identifier property atr-file))

(defmethod remove-property-from-constructor :around (constructor-identifier property (atr-file string))
  (if (string= atr-file "")
      (error "The empty string cannot be understood as denoting a .atr file.")
      (call-next-method)))

(defmethod remove-property-from-constructor (constructor-identifier property (atr-file string))
  ;; try to interpret ATR-FILE as a piece of XML: look for "<" as its
  ;; first character.  Otherwise, treat it as a file.
  (if (char= (char atr-file 0) #\<)
      (destructuring-bind (kind nr aid relnr)
	  (split #\- constructor-identifier)
	(let ((kind-param (xuriella:make-parameter kind "target_kind"))
	      (nr-param (xuriella:make-parameter nr "target_nr"))
	      (aid-param (xuriella:make-parameter aid "target_aid"))
	      (relnr-param (xuriella:make-parameter relnr "target_relnr"))
	      (property-param (xuriella:make-parameter property "target_property")))
	  (xuriella:apply-stylesheet *strip-prop-stylesheet*
				     atr-file
				     :parameters (list kind-param
						       nr-param
						       aid-param
						       relnr-param
						       property-param))))
      (remove-property-from-constructor constructor-identifier property (pathname atr-file))))

(defmethod remove-property-from-constructor :around (constructor-identifier propery (atr-file pathname))
  (unless (file-exists-p atr-file)
    (error "The supplied ATR path, '~a', doesn't exist" atr-file))
  (let ((sheet (mizar-items-config 'strip-prop-stylesheet)))
    (if (file-exists-p sheet)
	(call-next-method)
	(error "The strip-properties stylesheet doesn't exist at the expected location '~a'" sheet))))

(defmethod remove-property-from-constructor (constructor-identifier property (atr-file pathname))
  (destructuring-bind (kind nr aid relnr)
      (split #\- constructor-identifier)
    (let ((kind-param (xuriella:make-parameter kind "target_kind"))
	  (nr-param (xuriella:make-parameter nr "target_nr"))
	  (aid-param (xuriella:make-parameter aid "target_aid"))
	  (relnr-param (xuriella:make-parameter relnr "target_relnr"))
	  (property-param (xuriella:make-parameter property "target_property")))
      (xuriella:apply-stylesheet *strip-prop-stylesheet*
				 atr-file
				 :parameters (list kind-param
						   nr-param
						   aid-param
						   relnr-param
						   property-param)))))

(defgeneric properties-for-constructor (constructor-identifier atr-file)
  (:documentation "List the properties for the constructor identified by CONSTRUCTOR-IDENTIFIER in ATR-FILE."))

(defmethod properties-for-constructor :around (constructor-identifier atr-file)
  (declare (ignore constructor-identifier atr-file))
  (let ((sheet (mizar-items-config 'list-properties-stylesheet)))
    (if (file-exists-p sheet)
	(call-next-method)
	(error "The list-properties XSL stylesheet doesn't exist at the expected location '~a'" sheet))))

(defparameter *list-properties-stylesheet*
  (xuriella:parse-stylesheet (pathname (mizar-items-config 'list-properties-stylesheet))))

(defmethod properties-for-constructor (constructor-identifier atr-file)
  (destructuring-bind (kind nr aid relnr)
      (split #\- constructor-identifier)
    (let ((kind-param (xuriella:make-parameter kind "target_kind"))
	  (nr-param (xuriella:make-parameter nr "target_nr"))
	  (aid-param (xuriella:make-parameter aid "target_aid"))
	  (relnr-param (xuriella:make-parameter relnr "target_relnr")))
      (split #\Newline
	     (xuriella:apply-stylesheet *list-properties-stylesheet*
					atr-file
					:parameters (list kind-param
						    nr-param
						    aid-param
						    relnr-param))))))

(defparameter *strip-prop-stylesheet*
  (xuriella:parse-stylesheet (pathname (mizar-items-config 'strip-prop-stylesheet))))

(defun remove-all-properties-from-constructor (constructor-identifier atr-file)
  (destructuring-bind (kind nr aid relnr)
      (split #\- constructor-identifier)
    (let ((kind-param (xuriella:make-parameter kind "target_kind"))
	  (nr-param (xuriella:make-parameter nr "target_nr"))
	  (aid-param (xuriella:make-parameter aid "target_aid"))
	  (relnr-param (xuriella:make-parameter relnr "target_relnr"))
	  (property-param (xuriella:make-parameter "all" "target_property")))
      (xuriella:apply-stylesheet *strip-prop-stylesheet*
				 atr-file
				 :parameters (list kind-param
						   nr-param
						   aid-param
						   relnr-param
						   property-param)))))

(defgeneric verify-with-atr-file (article atr)
  (:documentation "Verify ARTICLE using ATR.  The mizar verifier will be called with the .atr file specified by ATR, which may or may not be equal to the .atr file that the mizar accommodator would have generated when applied to ARTICLE."))

(defmethod verify-with-atr-file ((article pathname) (atr pathname))
  (handler-case (accom article :flags '("-q" "-s" "-l"))
    (mizar-error (err)
      (unless (innocent-accomodator-errorp err)
	(error "There were non-innocent accommodator errors for ~a" article))))
  (copy-file atr (atr-file-for-article article))
  (handler-case (verifier article :flags '("-q" "-l"))
    (mizar-error (err)
      (unless (only-*4-errors err)
	(error "There were non-*4 verifier errors for ~a" article)))))

(defmethod verify-with-atr-file ((article pathname) (atr string))
  (let ((atr-path (atr-file-for-article article)))
    (with-open-file (atr-stream atr-path
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
      (format atr-stream "~a" atr))
    (verify-with-atr-file article atr-path)))

(defun replace-atr (atr-path new-atr-content)
  (with-open-file (atr-stream atr-path
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
    (format atr-stream "~a" new-atr-content)))

(defun properties-needed-for-constructor (constructor property-list article)
  "Find a sublist of PROPERTY-LIST, assumed to be a list of property names (e.g., \"Asymmetry\", \"Projectivity\", etc.) that are truly needed for the mizar verifier to accept ARTICLE as correct."
  ;; Heuristic: first try to remove all properties from the
  ;; constuctor.  If the article is still verifiable, then we know
  ;; right away that the empty list is the right value to return.
  ;; Otherwise, grind away.
  (if (null property-list)
      nil
      (loop
	 with needed = nil
	 with atr-path = (atr-file-for-article article)
	 with original-atr = (file-as-string atr-path)
	 initially
	   (let ((fully-stripped-atr (remove-all-properties-from-constructor constructor original-atr)))
	     (replace-atr atr-path fully-stripped-atr)
	     (when (handler-case (verifier article :flags '("-q" "-l"))
		     (mizar-error () (replace-atr atr-path original-atr)))
	       (replace-atr atr-path original-atr)
	       (return nil)))
	 for property in property-list
	 for new-atr = (remove-property-from-constructor constructor
							 property
							 atr-path)
	 do
	   (replace-atr atr-path new-atr)
	   (handler-case (verifier article :flags '("-q" "-l"))
	     (mizar-error ()
	       ;; (break "We encountered a verifier error when removing~%~%  ~a~%~%from constructor~%~%  ~a;~%~%here are the lines of the error file:~%~%~{~a~%~}" property constructor (mizar-error-lines err))
	       ;; (unless (only-*4-errors err)
	       ;;   (warn "verifier signaled an error, at least one of which was not *4, when checking whether constructor ~a really needs the property ~a; here are the lines of the error file:~%~%~{~a~%~}" constructor property (mizar-error-lines err)))
	       (push property needed)))
	   (replace-atr atr-path original-atr)
	 finally
	   (return needed))))

(defgeneric constructors-with-needed-properties (article)
  (:documentation "A list of constructors from ARTICLE together with properties that cannot be removed from the constructors."))

(defmethod constructors-with-needed-properties :around ((article pathname))
  (if (file-exists-p article)
      (call-next-method)
      (error "There is no file at the given article path,~%~%  ~a" article)))

(defmethod constructors-with-needed-properties ((article pathname))
  (let* ((pruned-atr-path (pruned-atr-file-for-article article))
	 (atr-path (if (file-exists-p pruned-atr-path)
		       pruned-atr-path
		       (atr-file-for-article article)))
	 (initial-list (constructors-with-properties atr-path)))
    (loop
       with trimmed-list = nil
       for (constructor . properties) in initial-list
       for needed = (properties-needed-for-constructor constructor
						       properties
						       article)
       do
	 (when needed
	   (push (cons constructor needed) trimmed-list))
       finally
	 (return trimmed-list))))

(defgeneric constructors-w/-needed-properties-for-fragmented-article (article-name)
  (:documentation "Given the name ARTICLE-NAME of an article that has already been
  divided into fragments, compute, for each fragment, the constructors
  that need properties."))

(defmethod constructors-w/-needed-properties-for-fragmented-article :around ((article-name string))
  (if (string= article-name "")
      (error "The empty string is not the name of any article")
      (let* ((itemization-store (mizar-items-config 'itemization-source))
	     (article-dir (format nil "~a/~a"
				  itemization-store
				  article-name)))
	(if (file-exists-p article-dir)
	    (let ((article-text-subdir (format nil "~a/text" article-dir)))
	      (if (file-exists-p article-text-subdir)
		  (let ((first-fragment-name (format nil "~a/ckb1.miz" article-text-subdir)))
		    (if (file-exists-p first-fragment-name)
			(call-next-method)
			(error "The text subdirectory~%~%  ~a~%~%of article~%~%  ~a~%~%does not contain 'ckb1.miz',~%so evidently there are no fragments available"
			       article-text-subdir article-name)))
		  (error "The article~%~%  ~a~%~%lacks a text subdirectory at the expected location~%~%~a" article-name article-text-subdir)))
	    (error "A directory for article~%~%  ~a~%~%cannot be found under the itemization store~%~%  ~a" article-name itemization-store)))))

(defmethod constructors-w/-needed-properties-for-fragmented-article ((article-name string))
  (loop
     with fragments = (fragments-for-article article-name)
     with num-fragments = (length fragments)
     for i from 1 upto num-fragments
     for fragment in fragments
     collect (cons i (constructors-with-needed-properties fragment))))

(defun dereference-constructor-name (constructor-identifier reference-article)
  "Given a CONSTRUCTOR-IDENTIFIER of the form

  <kind>-<nr>-<article>-<relnr>,

find the item corresponding to this.  <article> might be a
\"fragment\" article, i.e., it looks like

  CKB<number>.

In this case, REFERENCE-ARTICLE is used; it is understood that the the
constructor comes from the result of itemizing REFERENCE-ARTICLE."
  (destructuring-bind (kind nr article relnr)
      (split #\- constructor-identifier)
    (declare (ignore relnr))
    (let ((article-lc (format nil "~(~a~)" article))
	  (reference-article-lc (format nil "~(~a~)" reference-article)))
      (if (scan "^ckb[1-9][0-9]*" article-lc)
	  (register-groups-bind (fragment-num)
	      ("^ckb([1-9][0-9]*)" article-lc)
	    (let* ((fragment (format nil "~a:~a" reference-article-lc fragment-num))
		   (items (fragment->items fragment)))
	      (if items
		  (let ((constructor (find-if #'(lambda (item)
						  (scan ":.constructor:" item))
					      items
					      :key #'symbol-name)))
		    (or constructor
			(error "Unable to find a suitable constructor among the items~%~%~{~a  ~%~}~%~%generated by the fragment~%~%  ~a" items fragment)))
		  (error "Unable to find any items corresponding to the fragment~%~%  ~a~%" fragment))))
	  (get-and-maybe-set-item-name
	   (format nil "~a:~(~a~)constructor:~a" article-lc kind nr))))))