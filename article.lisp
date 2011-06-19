;;; article.lisp A representation of mizar articles

(in-package :mizar)

(defclass article ()
  ((name
    :initarg :name
    :accessor name
    :type (or nil string)
    :initform nil
    :documentation "The name of this article, as a string.")
   (mml-version
    :initarg :mml-version
    :initform nil
    :type (or nil string)
    :accessor mml-version
    :documentation "The version of the MML that this article comes from")
   (items
    :initarg :items
    :accessor items
    :type list
    :initform nil
    :documentation "The items into which this article is divided.")
   (path
    :initarg :path
    :accessor path
    :type pathname)
   (authors
    :type list
    :accessor authors
    :initarg :authors
    :initform nil
    :documentation "A list of authors for an article.")
   (title
    :type (or nil string)
    :accessor title
    :initform nil
    :initarg :title
    :documentation "The title of this article.")
   (text
    :initarg :text
    :accessor text
    :type string
    :documentation "TEXT, if set, contains a string representing only the text proper of an article; it is supposed to exclude the initial environment.  Contrast with the FULL-TEXT slot.")
   (full-text
    :initarg :full-text
    :accessor full-text
    :type string
    :documentation "FULL-TEXT, if set,
    contains a string representing the entire text of an article,
    complete with the environment.  Contrast with TEXT slot, which is intended to contain only the text proper, sans environment, of an article.")
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
   (registrations
    :initarg :registrations
    :accessor registrations
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
	      :type hash-table)
   (pretext
    :initarg :pretext
    :accessor pretext
    :type string
    :initform ""))
  (:documentation "A representation of a mizar article."))

;;; Article construction

(let ((article-table (make-hash-table :test #'equal)))
  (defmethod make-instance :around ((class (eql 'article)) &rest initargs)
    (let ((name-tail (member :name initargs)))
      (if name-tail
	  (let ((name (second name-tail)))
	    (let ((mml-tail (member :mml-version initargs)))
	      (if mml-tail
		  (let* ((mml-version (second mml-tail))
			 (key (cons name mml-version)))
		    (multiple-value-bind (existing present?)
			(gethash key article-table)
		      (if present?
			  existing
			  (let ((new-article (call-next-method)))
			    (setf (gethash key article-table) new-article)))))
		  (call-next-method))))
	  (error "Articles must have a name!"))))
  (defun article-table () article-table))

(defun make-article-copying-environment-from (article)
  "Make a new article, and initialize its environment by copying the
environment of ARTICLE.  If an environment slot of ARTICLE is
unbound, it will be bound in the new article and have value NIL."
  (let ((new-article (make-instance 'article)))
    (setf (vocabularies new-article) (when (slot-boundp article 'vocabularies)
				       (vocabularies article))
	  (notations new-article) (when (slot-boundp article 'notations)
				    (notations article))
	  (constructors new-article) (when (slot-boundp article 'constructors)
				       (constructors article))
	  (requirements new-article) (when (slot-boundp article 'requirements)
				       (requirements article))
	  (registrations new-article) (when (slot-boundp article 'registrations)
					(registrations article))
	  (definitions new-article) (when (slot-boundp article 'definitions)
				      (definitions article))
	  (theorems new-article) (when (slot-boundp article 'theorems)
				   (theorems article))
	  (schemes new-article) (when (slot-boundp article 'schemes)
				  (schemes article)))
    new-article))

(defmethod initialize-instance :after ((article article) &key)
  ;; if only a name is given, attempt to fetch the article
  ;; environment, and set the values of the appropriate slots
  (when (and (slot-boundp article 'path)
	     (not (slot-boundp article 'vocabularies))
	     (not (slot-boundp article 'notations))
	     (not (slot-boundp article 'requirements))
	     (not (slot-boundp article 'registrations))
	     (not (slot-boundp article 'constructors))
	     (not (slot-boundp article 'definitions))
	     (not (slot-boundp article 'theorems))
	     (not (slot-boundp article 'schemes)))
    (handler-case
	(refresh-environment article t)
      (mizar-error () (warn "Unable to call envget on the article under ~A.  Is the directory writable?" (path article)))))
  (when (and (slot-boundp article 'path)
	     (not (slot-boundp article 'lines)))
    (let ((path (path article)))
      (if (file-exists-p path)
	  (refresh-text article)
	  (warn "Although the article has a path, ~A, there is no file (yet) at that location" path))))
  (when (and (slot-boundp article 'path)
	     (not (slot-boundp article 'name)))
    (let* ((filename (file-namestring (path article)))
	   (basename (pathname-name filename)))
      (setf (name article) basename)))
  article)

(defun make-article (name mml-version)
  (let ((article (make-instance 'article
		 :name name
		 :mml-version mml-version)))
    (destructuring-bind (authors title)
	(handler-case 
	    (title-and-author name mml-version)
	  (error (err) (progn
			 (warn "Error when getting the title and author information for article ~a of MML version ~a.  The error was:~%~a~%Using default values..." name mml-version err)
		      (list "(no author information was available)"
			    "(no title information was available)"))))
      (setf (authors article) (split "and" authors)
	    (title article) title))
    article))

(defun copy-article (article)
  (let ((new-article (make-article-copying-environment-from article)))
    (when (slot-boundp article 'text)
      (setf (text new-article) (text article)))
    (when (slot-boundp article 'full-text)
      (setf (full-text new-article) (full-text article)))
    new-article))

;;; Printing articles

(defmethod print-object ((article article) stream)
  (print-unreadable-object (article stream)
    (if (and (slot-boundp article 'name) (name article))
	(let ((name (name article)))
	  (if (and (slot-boundp article 'mml-version) (mml-version article))
	      (let ((mml-version (mml-version article)))
		(format stream "~:@(~a~) (MML version ~a" name mml-version))
	      (format stream "~:@(~a~) (MML version unknown)" name)))
	(format stream "(unknown name, unknown MML version)"))))

(defun file-exists-under-prel (local-db file)
  (let ((prel (concat (namestring (pathname-as-directory local-db))
		      (namestring (pathname-as-directory "prel")))))
    (let ((file-path (make-pathname :directory prel
				    :name file)))
      (probe-file file-path))))

(defun trim-environment (article &optional local-db)
  "Remove entries in the environment that don't exist in the MML.  If
  LOCAL-DB is non-nil, use it as a further source of
  information (specifically, consult its prel and dict subdirectories
  to determine whether certain things specified in the environment
  really do exist)."
  (with-slots (vocabularies
	       notations
	       constructors
	       requirements
	       registrations
	       definitions
	       theorems
	       schemes)
      article
    (declare (ignore vocabularies requirements)) ;; ignore for now
    (setf notations (remove-if-not #'(lambda (notation)
				       (or (belongs-to-mml notation)
					   (file-exists-under-prel local-db 
								   (format nil "~(~a~).dno" notation))))
				   notations)
	  constructors (remove-if-not #'(lambda (constructor)
					  (or (belongs-to-mml constructor)
					      (file-exists-under-prel local-db
								      (format nil "~(~a~).dco" constructor))))
				      constructors)
	  registrations (remove-if-not #'(lambda (registration)
					   (or (belongs-to-mml registration)
					       (file-exists-under-prel local-db
								       (format nil "~(~a~).dcl" registration))
					       (file-exists-under-prel local-db
								       (format nil "~(~a~).did" registration))))
				       registrations)
	  definitions (remove-if-not #'(lambda (definition)
					 (or (belongs-to-mml definition)
					     (file-exists-under-prel local-db
								     (format nil "~(~a~).def" definition))))
				     definitions)
	  theorems (remove-if-not #'(lambda (theorem)
				      (or (belongs-to-mml theorem)
					  (file-exists-under-prel local-db
								  (format nil "~(~a~).the" theorem))))
				  theorems)
	  schemes (remove-if-not #'(lambda (scheme)
				     (or (belongs-to-mml scheme)
					 (file-exists-under-prel local-db
								 (format nil "~(~a~).sch" scheme))))
				 schemes))
    (values notations constructors registrations definitions theorems schemes)))

(defgeneric line-at (text line-number))

(defmethod line-at ((article article) line-number)
  (nth (1- line-number) 
       (lines article))) ; start counting lines at 1

(defun num-lines (article)
  (length (lines article)))

(defun refresh-environment (article &optional envget)
  (when envget
    (envget article (directory-namestring (path article)) "-l"))
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
    (setf (registrations article)
	  (gethash "Registrations" environment-as-table))
    (setf (definitions article)
	  (gethash "Definitions" environment-as-table))
    (setf (theorems article)
	  (gethash "Theorems" environment-as-table))
    (setf (schemes article)
	  (gethash "Schemes" environment-as-table))))

(defun refresh-text (article)
  "Ensure that the LINES slot of ARTICLE accurately reflects the
  contents of the file pointed to by ARTICLE's PATH slot."
  (let ((path (path article)))
    (if (probe-file path)
	(setf (lines article) (lines-in-file path))
	(error "Unable to refresh the text for article ~S: its path '~A' doesn't exist"
	     article path))))

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

(defun label-for-vid (article vid)
  (with-slots (idx-table)
      article
    (gethash vid (idx-table article))))

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

(defun article-lines-matching (regex article)
  "Return a list of triples (LINE COLUMN MATCH), where LINE and COLUMN
  are the line and column within ARTICLE that matches the regular
  expression REGEX; MATCH is the matched string."
  (loop
     with scanner = (create-scanner regex)
     with results = nil
     for line in (lines article)
     for current-line-num from 1
     do
      ; doesn't look for multiple occurences of REGEX within line
      ; for an elegant solution, we should have a multiple-value do/while/until
       (multiple-value-bind (start-col end-col start-register end-register)	  
	   (scan scanner line)
	 (declare (ignore start-register))
	 (when start-col ; we have a match
	   (push (list current-line-num 
		       (aref end-register 0)
		       (subseq line start-col end-col))
		 results)))
     finally (return (reverse results))))

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

(defun region-up-to (article up-to-line up-to-col)
  (region article 1 0 up-to-line up-to-col))

(defun region-after (article after-line-num after-col-num)
  (let* ((num-lines (num-lines article))
	 (last-line (line-at article num-lines))
	 (last-line-length (length last-line)))
    ;; (let ((after-line (line-at article after-line-num)))
    ;;   (if (<= (length after-line) after-col-num)
    ;; 	  (if (= after-line-num num-lines)
    ;; 	      ""
    ;; 	      (region article (1+ after-line-num) 0 num-lines last-line-length))
	  (region article after-line-num after-col-num num-lines last-line-length)))

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

(defun print-directive (stream directive-name directive-contents)
  (let ((without-hidden (remove "HIDDEN" directive-contents :test #'string=)))
    (when without-hidden
      (format stream "~A " directive-name)
      (loop
	 with len = (length without-hidden)
	 for name in without-hidden
	 for i from 0 upto (1- len)
	 do
	   (format stream name)
	   (if (= i (1- len))
	       (format stream ";")
	       (format stream ", ")))
      (terpri stream))
    t))

(defun print-vocabularies (article item-stream)
  (let ((directive-contents (vocabularies article)))
    (print-directive item-stream "vocabularies" directive-contents)))

(defun print-notations (article item-stream)
  (let ((directive-contents (notations article)))
    (print-directive item-stream "notations" directive-contents)))

(defun print-constructors (article item-stream)
  (let ((directive-contents (constructors article)))
    (print-directive item-stream "constructors" directive-contents)))

(defun print-requirements (article item-stream)
  (let ((directive-contents (requirements article)))
    (print-directive item-stream "requirements" directive-contents)))

(defun print-registrations (article item-stream)
  (let ((directive-contents (registrations article)))
    (print-directive item-stream "registrations" directive-contents)))

(defun print-theorems (article item-stream)
  (let ((directive-contents (theorems article)))
    (print-directive item-stream "theorems" directive-contents)))

(defun print-definitions (article item-stream)
  (let ((directive-contents (definitions article)))
    (print-directive item-stream "definitions" directive-contents)))

(defun print-schemes (article item-stream)
  (let ((directive-contents (schemes article)))
    (print-directive item-stream "schemes" directive-contents)))

(defun write-article (article)
  (let ((path (path article)))
    (if (ensure-directories-exist path)
	(with-open-file (miz path
			     :direction :output
			     :if-exists :supersede)
	  (cond ((slot-boundp article 'full-text)
		 (format miz "~A~%" (full-text article)))
		((slot-boundp article 'text)
		 (let ((pretext (pretext article)))
		   (unless (string= pretext "")
		     (dolist (line (split (make-string 1 :initial-element #\Newline) pretext))
		       (format miz ":: ~a~%" line))))
		 (format miz "environ~%")
		 (print-vocabularies article miz)
		 (print-notations article miz)
		 (print-constructors article miz)
		 (print-requirements article miz)
		 (print-registrations article miz)
		 (print-theorems article miz)
		 (print-definitions article miz)
		 (print-schemes article miz)
		 (format miz "begin~%")
		 (format miz "~A~%" (text article)))
		(t
		 (error "Neither a complete text nor a text proper is known for ~S" article))))
	(error "The path ~A is invalid: we cannot ensure that the directories mentioned in it exist." path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trimming the environment to eliminate uneeded parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun verifiable? (article &optional (directory (sb-posix:getcwd)))
  (write-article article)
  (handler-case (and (accom article directory "-q" "-s" "-l")
		     (verifier article directory "-q" "-s" "-l")
		     t)
    (mizar-error () nil)))

(defun verifiable-with-directive (article directive-name directive-contents &optional (directory (sb-posix:getcwd)))
  (let ((new-article (make-article-copying-environment-from article)))
    (setf (text new-article) (text article))
    (setf (path new-article) (path article))
    (cond ((string= directive-name "vocabularies")
	   (setf (vocabularies new-article) directive-contents))
	  ((string= directive-name "notations")
	   (setf (notations new-article) directive-contents))
	  ((string= directive-name "constructors")
	   (setf (constructors new-article) directive-contents))
	  ((string= directive-name "registrations")
	   (setf (registrations new-article) directive-contents))
	  ((string= directive-name "requirements")
	   (setf (requirements new-article) directive-contents))
	  ((string= directive-name "definitions")
	   (setf (definitions new-article) directive-contents))
	  ((string= directive-name "theorems")
	   (setf (theorems new-article) directive-contents))
	  ((string= directive-name "schemes")
	   (setf (schemes new-article) directive-contents))
	  (t
	   (error "Unknown directive name '~A'" directive-name)))
    (write-article new-article)
    (verifiable? new-article directory)))

(defun find-minimal-vocabularies (article &optional (directory (sb-posix:getcwd)))
  (shortest-admissible-final-segment
   (vocabularies article)
   #'(lambda (lst)
       (verifiable-with-directive article "vocabularies" lst directory))))

(defun find-minimal-notations (article &optional (directory (sb-posix:getcwd)))
  (shortest-admissible-final-segment
   (notations article)
   #'(lambda (lst)
       (verifiable-with-directive article "notations" lst directory))))

(defun find-minimal-constructors (article &optional (directory (sb-posix:getcwd)))
  (shortest-admissible-final-segment
   (constructors article)
   #'(lambda (lst)
       (verifiable-with-directive article "constructors" lst directory))))

(defun find-minimal-requirements (article &optional (directory (sb-posix:getcwd)))
  (shortest-admissible-final-segment
   (requirements article)
   #'(lambda (lst)
       (verifiable-with-directive article "requirements" lst directory))))

(defun find-minimal-registrations (article &optional (directory (sb-posix:getcwd)))
  (shortest-admissible-final-segment
   (registrations article)
   #'(lambda (lst)
       (verifiable-with-directive article "registrations" lst directory))))

(defun find-minimal-definitions (article &optional (directory (sb-posix:getcwd)))
  (shortest-admissible-final-segment
   (definitions article)
   #'(lambda (lst)
       (verifiable-with-directive article "definitions" lst directory))))

(defun find-minimal-theorems (article &optional (directory (sb-posix:getcwd)))
  (shortest-admissible-final-segment
   (theorems article)
   #'(lambda (lst)
       (verifiable-with-directive article "theorems" lst directory))))

(defun find-minimal-schemes (article &optional (directory (sb-posix:getcwd)))
  (shortest-admissible-final-segment
   (schemes article)
   #'(lambda (lst)
       (verifiable-with-directive article "schemes" lst directory))))

(defun minimize-environment (article &optional (directory (sb-posix:getcwd)))
  (warn "Minimizing the environment for ~A" article)
  (let ((new-article (copy-article article)))
    (setf (path new-article) (path article))
    (setf (vocabularies new-article)
	  (find-minimal-vocabularies new-article directory))
    (setf (theorems new-article)
	  (find-minimal-theorems new-article directory))
    (setf (requirements new-article)
	  (find-minimal-requirements new-article directory))
    (setf (schemes new-article)
	  (find-minimal-schemes new-article directory))
    (setf (notations new-article)
	  (find-minimal-notations new-article directory))
    (setf (constructors new-article)
	  (find-minimal-constructors new-article directory))
    (setf (registrations new-article)
	  (find-minimal-registrations new-article directory))
    (setf (definitions new-article)
	  (find-minimal-definitions new-article directory))
    ;; report shortenings
    (warn "we cut ~d items from the vocabularies directive" (- (length (vocabularies article)) (length (vocabularies new-article))))
    (warn "we cut ~d items from the theorems directive" (- (length (theorems article)) (length (theorems new-article))))
    (warn "we cut ~d items from the requirements directive" (- (length (requirements article)) (length (requirements new-article))))
    (warn "we cut ~d items from the schemes directive" (- (length (schemes article)) (length (schemes new-article))))
    (warn "we cut ~d items from the notations directive" (- (length (notations article)) (length (notations new-article))))
    (warn "we cut ~d items from the constructors directive" (- (length (constructors article)) (length (constructors new-article))))
    (warn "we cut ~d items from the definitions directive" (- (length (definitions article)) (length (definitions new-article))))
    ;; now sync
    (setf (vocabularies article)
	  (vocabularies new-article))
    (setf (notations article)
	  (notations new-article))
    (setf (constructors article)
	  (constructors new-article))
    (setf (requirements article)
	  (requirements new-article))
    (setf (registrations article)
	  (registrations new-article))
    (setf (definitions article)
	  (definitions new-article))
    (setf (theorems article)
	  (theorems new-article))
    (setf (schemes article)
	  (schemes new-article))
    article))

;;; article.lisp ends here