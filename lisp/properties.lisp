;;; properties.lisp --- Work with properties attached to mizar constructors

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
	  (xuriella:apply-stylesheet (mizar-items-config 'strip-prop-stylesheet)
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

(defun dereferenced-constructors-w/-needed-properties-for-fragmented-article (article)
  (mapcar #'(lambda (entry)
	      (destructuring-bind (fragment-number . needed)
		  entry
		(cons fragment-number
		      (mapcar #'(lambda (prop-entry)
				  (destructuring-bind (constr . props)
				      prop-entry
				    (cons (dereference-constructor-name
					   constr article)
					  props)))
			      needed))))
	  (constructors-w/-needed-properties-for-fragmented-article article)))

(defun needed-properties-table-for-article (article)
  (loop
     with table = (make-hash-table :test #'equal)
     for entry in (dereferenced-constructors-w/-needed-properties-for-fragmented-article article)
     do
       (destructuring-bind (fragment-number . needed)
	   entry
	 (loop
	    with needed-table = (make-hash-table :test #'equal)
	    for (constructor . properties) in needed
	    do
	      (setf (gethash constructor needed-table) properties)
	    finally
	      (setf (gethash (cons article fragment-number) table)
		    needed-table)))
     finally
       (return table)))

(defvar *properties-needed-for-fragment* nil
  "A mapping from pairs

  (<article> . <fragment-number>)

to lists of lists

  ((<constructor-item-1> . <needed-properties-of-constructor-item-1>) ...)")

(defun make-properties-needed-for-fragment-table ()
  (loop
     with table = (make-hash-table :test #'equal)
     for article in *mml-lar*
     for article-name = (name article)
     do
       (format t "Processing ~a..." article-name)
       (needed-properties-table-for-article article-name)
       (format t "done~%")
     finally
       (return table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Property data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *property-table* nil
  "A mapping from items to association lists of the form.  The members
  of the association lists have the form

\\(constructuctor . properties)

The interpretation of a mapping from an item I to an association list

  ((constructor-1 . properties-1) (constructor-2 . properties-2) ...)

is that the item I requires from constructor-1 each of the properties listed in properties-1, from constructor-2 each of the properties listed in properties-2, etc.")

(defgeneric load-needed-properties (mml-version)
  (:documentation "Load the table of properties needed for each constructor for the
  MML version MML-VERSION."))

(defmethod load-needed-properties :around (mml-version)
  (let ((needed-file (needed-property-file-for-mml mml-version)))
    (if (file-exists-p needed-file)
      (call-next-method)
      (error "The needed property file does not exist at the expected location~%~%  ~a~%" (namestring needed-file)))))

(defun update-needed-property (fragment property constructor table)
  (flet ((update-property-alist (alist property)
	     (rplacd alist
		     (cons property (cdr alist)))))
    (multiple-value-bind (current-entry present?)
	(gethash fragment table)
      (if present?
	  (let ((ass (assoc constructor current-entry)))
	    (if ass
		(update-property-alist ass property)
		(setf (gethash fragment table)
		      (acons constructor
			     (list property)
			     current-entry))))
	(setf (gethash fragment table)
	      (acons constructor (list property) nil)))))
  table)

(defmethod load-needed-properties (mml-version)
  (let ((needed-path (needed-property-file-for-mml mml-version))
	(needed-table (make-hash-table :test #'equal)))
    (flet ((update-property-alist (alist property)
	     (rplacd alist
		     (cons property (cdr alist)))))
      (loop
	 for line in (lines-of-file needed-path)
	 for i from 1
	 do
	   (destructuring-bind (fragment property constructor-item-str)
	       (split #\Space line)
	     (update-needed-property fragment property (get-and-maybe-set-item-name constructor-item-str) needed-table))
	 finally
	   (return needed-table)))))

(defgeneric fragment-of-item (item)
  (:documentation "The fragment from which ITEM comes (if any)."))

(defmethod fragment-of-item ((item string))
  (fragment-of-item (get-and-maybe-set-item-name item)))

(defmethod fragment-of-item ((item symbol))
  (multiple-value-bind (fragment-number present?)
      (gethash item *item-to-fragment-table*)
    (when present?
      (format nil "~a:~d" (item-article item) fragment-number))))

(defgeneric item-needs-constructor-property (item property constructor)
  (:documentation "Does ITEM need PROPERTY of CONSTRUCTOR?"))

(defmethod item-needs-constructor-property ((item string) property constructor)
  (item-needs-constructor-property (get-and-maybe-set-item-name item)
				   property
				   constructor))

(defmethod item-needs-constructor-property (item property (constructor string))
  (item-needs-constructor-property item
				   property
				   (get-and-maybe-set-item-name constructor)))

(let ((needs-table (make-hash-table :test #'equal)))
  (defmethod item-needs-constructor-property :around ((item symbol) property (constructor symbol))
    (let ((key (list item property constructor)))
      (multiple-value-bind (needed seen-before?)
	  (gethash key needs-table)
	(if seen-before?
	    needed
	    (let ((needed (call-next-method)))
	      (setf (gethash key needs-table)
		    needed))))))
  (defun clear-needed-constructors-table ()
    (setf needs-table (make-hash-table :test #'equal))))

(defmethod item-needs-constructor-property ((item symbol) property (constructor symbol))
  ;; do a simple depth-first search
  (let ((fragment (fragment-of-item item))
	(needs nil))
    (multiple-value-bind (all-needs any-needs?)
	(gethash fragment *property-table*)
      (when any-needs?
	(let ((needed-properties-of-constructor (assoc constructor all-needs :test #'string=)))
	  (when needed-properties-of-constructor
	    (when (member property needed-properties-of-constructor :test #'string=)
	      (setf needs t))))))
    (or needs
	(some #'(lambda (item)
		  (item-needs-constructor-property item property constructor))
	      (gethash item *item-dependency-graph-forward*)))))

(defun fragment-table-to-item-table (fragment-table)
  "Make a copy of FRAGMENT-TABLE, whose keys are assumed to be
  fragments (i.e., cons cells whose car is a string [article name] and
  whose cdr is a positive natural number).  The keys of the new table
  will be items, and the value of an item I will be identical (in the
  sense of EQ) to the value in FRAGMENT-TABLE for the fragment that
  generates I.

Warning: since the values in the new hash table will be EQ to the
values in FRAGMENT-TABLE, modifying them will modify both tables."
  (loop
     with new-table = (make-hash-table :test #'eq)
     for fragment being the hash-keys in fragment-table using (hash-value val)
     for i from 1
     do
       (when (zerop (mod i 1000))
	 (format t "Done with ~d items" i)
	 (terpri))
       (destructuring-bind (article fragment-number-str)
	   (split #\: fragment)
	 (let ((fragment-pair (cons article (parse-integer fragment-number-str))))
	   (dolist (item (gethash fragment-pair *fragment-to-item-table*))
	     (setf (gethash item new-table) val))))
     finally
       (return new-table)))

(defun expand-property-table ()
  (flet ((present-in-table (item constructor property)
	   (multiple-value-bind (alist present?)
	       (gethash item *property-table*)
	     (when present?
	       (member property (cdr (assoc constructor alist)) :test #'string=)))))
    (loop
       with num-expanded = 0
       for item being the hash-keys in *property-table* using (hash-value needed-properties)
       for i from 1
       do
	 (when (zerop (mod i 1000))
	   (format t "Done with ~d items" i)
	   (terpri))
	 (dolist (needed-property needed-properties)
	   (destructuring-bind (constructor . properties)
	       needed-property
	     (dolist (dependent-item (dependents item))
	       (dolist (property properties)
		 (unless (present-in-table dependent-item constructor property)
		   (update-needed-property dependent-item property constructor *property-table*)
		   (incf num-expanded))))))
       finally
	 (return num-expanded))))

;;; properties.lisp ends here
