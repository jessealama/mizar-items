
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

(defmethod constructors-and-properties ((atr-file pathname))
  (let ((sheet (pathname (mizar-items-config 'propertied-constructors-stylesheet))))
    (xuriella:apply-stylesheet sheet atr-file)))

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
	      (property-param (xuriella:make-parameter property "target_property"))
	      (sheet (pathname (mizar-items-config 'strip-prop-stylesheet))))
	  (xuriella:apply-stylesheet sheet
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
	  (property-param (xuriella:make-parameter property "target_property"))
	  (sheet (pathname (mizar-items-config 'strip-prop-stylesheet))))
      (xuriella:apply-stylesheet sheet
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

(defmethod properties-for-constructor (constructor-identifier atr-file)
  (destructuring-bind (kind nr aid relnr)
      (split #\- constructor-identifier)
    (let ((kind-param (xuriella:make-parameter kind "target_kind"))
	  (nr-param (xuriella:make-parameter nr "target_nr"))
	  (aid-param (xuriella:make-parameter aid "target_aid"))
	  (relnr-param (xuriella:make-parameter relnr "target_relnr"))
	  (sheet (pathname (mizar-items-config 'list-properties-stylesheet))))
      (split #\Newline
	     (xuriella:apply-stylesheet sheet
					atr-file
					:parameters (list kind-param
						    nr-param
						    aid-param
						    relnr-param))))))

(defun remove-all-properties-from-constructor (constructor-identifier atr-file)
  (let ((properties (properties-for-constructor constructor-identifier atr-file)))
    (loop
       with atr = atr-file
       for property in properties
       do
	 (setf atr (remove-property-from-constructor constructor-identifier property atr))
       finally
	 (return atr))))

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
	       (format t "Heuristic succeeded!~%")
	       (replace-atr atr-path original-atr)
	       (return nil)))
	 for property in property-list
	 for new-atr = (remove-property-from-constructor constructor
							 property
							 atr-path)
	 do
	   (replace-atr atr-path new-atr)
	   (handler-case (verifier article :flags '("-q" "-l"))
	     (mizar-error (err)
	       (break "We encountered a verifier error when removing~%~%  ~a~%~%from constructor~%~%  ~a;~%~%here are the lines of the error file:~%~%~{~a~%~}" property constructor (mizar-error-lines err))
	       ;; (unless (only-*4-errors err)
	       ;;   (warn "verifier signaled an error, at least one of which was not *4, when checking whether constructor ~a really needs the property ~a; here are the lines of the error file:~%~%~{~a~%~}" constructor property (mizar-error-lines err)))
	       (push property needed)))
	   (replace-atr atr-path original-atr)
	 finally
	   (return needed))))

(defgeneric constructors-with-needed-properties (article)
  (:documentation "A list of constructors from ARTICLE together with properties that cannot be removed from the constructors."))

(defmethod constructors-with-needed-properties ((article pathname))
  (let* ((atr-path (atr-file-for-article article))
	 (initial-list (constructors-with-properties atr-path)))
    (loop
       with trimmed-list = nil
       initially
	 (format t "About to consider ~d constructors..." (length initial-list))
       for (constructor . properties) in initial-list
       do
	 (push (cons constructor
		     (properties-needed-for-constructor constructor
							properties
							article))
	       trimmed-list) 
       finally
	 (return trimmed-list))))