;;; minimize.lisp --- Minimally sufficient conditions for a MIZAR text

(in-package :mizar)

(defparameter *keep-elements-stylesheet*
  (path-for-stylesheet "keep-elements"))
(defparameter *uninteresting-attributes-stylesheet*
  (path-for-stylesheet "uninteresting-attributes"))

(defgeneric minimize (article)
  (:documentation "Find the smallest envionment with respect to which ARTICLE is a verifiable MIZAR article."))

(defgeneric verifiable-wrt-environment (article environment-kind environment)
  (:documentation "Is ARTICLE verifiable with respect to
  ENVIRONMENT?

ENVIRONMENT is to be the content of or a reference to a particular
MIZAR environment file, e.g., the .atr file, or the .eno file, etc.
It is not intended to be the \"full\" environment of an article (i.e.,
the result of the accommodator).

ENVIRONMENT-KIND indicates what kind of environment ENVIRONMENT is,
e.g., constructor environment, notation environment, etc."))

(defparameter *environment-kinds*
  '("aco" "atr" "eno" "dfs" "ecl" "eid")
  "File extensions of known environment files generated by the
  accommodator.")

;; (defmethod verifiable-wrt-environment ((article pathname)
;; 				       (environment-kind stringl)
;; 				       (environment string))
;;   (unless (member article-kind *environment-kinds* :test #'string=)
;;     (error "The article kind '~a' is not a known envionment kind; the known kinds are:~%~%~{* ~a~%~}" *environment-kinds*)))

(defgeneric minimize-environment-for-article (article environment)
  (:documentation "Given ENVIRONMENT for ARTICLE, compute the smallest subenvironment of ENVIRONMENT with respect to which ARTICLE is MIZAR verifiable."))

(defmethod minimize-environment-for-article ((article pathname)
					     (environment pathname))
  (flet ((verifiable-wrt-environment (environment))
	 (handler-case
	     (verifier article :flags '("-q" "-l"))
	   (mizar-error () nil)))))

(defmethod minimize :around ((article pathname))
  (if (file-exists-p article)
      (call-next-method)
      (error "There is no article at '~a'." article)))

(defmethod minimize ((article-path pathname))
  (minimize (make-instance 'article
			   :path article-path)))

(defparameter *extension-to-root-element-table*
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "eno" table) "Notations"
	  (gethash "erd" table) "ReductionRegistrations"
	  (gethash "epr" table) "PropertyRegistration" ; sic
	  (gethash "dfs" table) "Definientia"
	  (gethash "eid" table) "IdentifyRegistrations"
	  (gethash "ecl" table) "Registrations"
	  (gethash "esh" table) "Schemes"
	  (gethash "eth" table) "Theorems")
    table)
  "A hash table mapping Mizar file extensions for XML files to the expected root elements of these files.")

(defgeneric minimize-extension (article extension))

(defmethod minimize-extension :around ((article article) (extension string))
  (if (present-in-table? extension *extension-to-root-element-table*)
      (call-next-method)
      (error "The extension '~a' is not registered in the root element table." extension)))

(defun write-document (document path)
  (with-open-file (xml-file path
			    :direction :output
			    :if-exists :supersede
			    :element-type '(unsigned-byte 8))
    (dom:map-document (cxml:make-octet-stream-sink xml-file) document)))

(defun write-nodes-xuriella (indices-to-keep path)
  (let* ((indices-token-string (tokenize (mapcar #'1+ indices-to-keep)))
	 (new-xml-stream (make-string-output-stream))
	 (to-keep-param (xuriella:make-parameter indices-token-string
						 "to-keep")))
    (xuriella:apply-stylesheet *keep-elements-stylesheet*
			       path
			       :parameters (list to-keep-param)
			       :output new-xml-stream)
    (alexandria:write-string-into-file (get-output-stream-string new-xml-stream)
				       path
				       :if-exists :supersede)))

(defun write-nodes (indices-to-keep path)
  (let ((indices-token-string (tokenize (mapcar #'1+ indices-to-keep))))
    (apply-stylesheet *keep-elements-stylesheet*
		      path
		      (list (cons "to-keep" indices-token-string))
		      path)))

(defgeneric nodes-equal? (node-1 node-2))

(defmethod nodes-equal? ((node-1 t) (node-2 t))
  (error "Don't know how to compare node~%~%  ~a~%~%to~%~%  ~a" node-1 node-2))

(defmethod nodes-equal? ((node-1 dom:text) (node-2 dom:text))
  (string= (dom:data node-1)
	   (dom:data node-2)))

(defmethod nodes-equal? ((node-1 dom:attr) (node-2 dom:attr))
  (let ((name-1 (dom:node-name node-1))
	(name-2 (dom:node-name node-2)))
    (when (string= name-1 name-2)
      (let ((value-1 (dom:value node-1))
	    (value-2 (dom:value node-2)))
	(string= value-1 value-2)))))

(defun uninteresting-attribute-p (attribute)
  (let ((name (dom:node-name attribute)))
    (or (string= name "pid")
	(string= name "relnr")
	(string= name "redefnr")
	(string= name "line")
	(string= name "col")
	(string= name "x")
	(string= name "y")
	(string= name "mizfiles"))))

(defmethod nodes-equal? ((node-1 dom:element) (node-2 dom:element))
  (let ((name-1 (dom:node-name node-1))
	(name-2 (dom:node-name node-2)))
    (when (string= name-1 name-2)
      (let ((attributes-1 (remove-if #'uninteresting-attribute-p
				     (dom:items (dom:attributes node-1))))
	    (attributes-2 (remove-if #'uninteresting-attribute-p
				     (dom:items (dom:attributes node-2)))))
	(flet ((attr-subset (attribute-list-1 attribute-list-2)
		 (every #'(lambda (attr-1)
			    (some #'(lambda (attr-2) (nodes-equal? attr-1 attr-2))
				  attribute-list-2))
			attribute-list-1)))
	  (if (and (attr-subset attributes-1 attributes-2)
		   (attr-subset attributes-2 attributes-1))
	      (let ((children-1 (dom:child-nodes node-1))
		    (children-2 (dom:child-nodes node-2)))
		(loop
		   for child-1 across children-1
		   for child-2 across children-2
		   do
		     (unless (nodes-equal? child-1 child-2)
		       (return nil))
		   finally
		     (return t)))
	      (progn
		(break "Nodes have different attributes.")
		nil)))))))

(defun equivalent-miz-xmls? (xml-path-1 xml-path-2)
  (let* ((path-1 (ccl:native-translated-namestring xml-path-1))
	 (path-2 (ccl:native-translated-namestring xml-path-2))
	 (error-output (make-string-output-stream))
	 (equiv-proc (run-program "/Users/alama/sources/mizar/mizar-items/perl/bin/equivalent-miz-xml.pl"
				  (list path-1 path-2)
				  :search nil
				  :input nil
				  :output nil
				  :error error-output
				  :wait t)))
    (if (zerop (process-exit-code equiv-proc))
	t
	(let ((error-string (get-output-stream-string error-output)))
	  (break "~a is not equivalent to ~a:~%~%~a" path-1 path-2 error-string)
	  nil))))

(defmethod minimize-extension ((article article) (extension string))
  (let* ((file-to-minimize (file-with-extension article extension))
	 (analyzer-xml (file-with-extension article "xml"))
	 (analyzer-xml-orig (file-with-extension article "xml.orig"))
	 (analyzer-xml-orig-trimmed-path (file-with-extension article
							      "xml.orig.trimmed"))
	 (file-to-minimize-copy (file-with-extension article (format nil "~a.orig" extension))))
    (unless (file-exists-p analyzer-xml)
      (error "The .xml for ~a is missing." (name article)))
    (apply-stylesheet *uninteresting-attributes-stylesheet*
		      analyzer-xml-orig
		      nil
		      analyzer-xml-orig-trimmed-path)
    (if (file-exists-p file-to-minimize)
	(let* ((doc (cxml:parse-file file-to-minimize (cxml-dom:make-dom-builder)))
	       (document-element (dom:document-element doc))
	       (nodes (xpath:all-nodes (xpath:evaluate "*" document-element)))
	       (analyzer-xml-orig-sha1 (ironclad:digest-file :sha1
							     analyzer-xml-orig-trimmed-path)))

	  ;; save a known good copy
	  (copy-file file-to-minimize file-to-minimize-copy
		     :if-to-exists :supersede
		     :finish-output t)

	  (format t "~a: " (namestring file-to-minimize))
	  (labels ((restore ()
		     (copy-file file-to-minimize-copy
				file-to-minimize
				:if-to-exists :supersede
				:finish-output t)
		     (copy-file analyzer-xml-orig
				analyzer-xml
				:if-to-exists :supersede
				:finish-output t)
		     t
		     ;; (break "Take a look now at ~a" (namestring file-to-minimize))
		     )
		   (analyzable-and-has-same-meaning (trial-indices)

		     ;; write to disk
		     (write-nodes trial-indices file-to-minimize)

		     ;; (break "Look at ~a" (namestring file-to-minimize))

		     ;; test whether this is ok
		     (multiple-value-bind (analyzer-ok? analyzer-crashed?)
			 (analyzer article)
		       (prog1
			   (cond (analyzer-ok?
				  (let ((new-trimmed-xml-path (file-with-extension article "xml.trimmed")))
				    (apply-stylesheet *uninteresting-attributes-stylesheet*
						      analyzer-xml
						      nil
						      new-trimmed-xml-path)
				    (let ((new-sha1 (ironclad:digest-file :sha1 new-trimmed-xml-path)))
				      (cond ((mismatch analyzer-xml-orig-sha1
						     new-sha1)
					   (break "Different XMLs, after stripping uninteresting attributes:~%~%  ~a~%~%  ~a~%" (namestring analyzer-xml-orig) (namestring analyzer-xml))
					   nil)
					  (t
					 ;; (format t "Analyzable, but XML changed.~%")
					   t)))))
				 (analyzer-crashed?
				  ;; (warn "Analyzer crash.")
				  nil)
				 (t
				  ;; (format t "Analyzer did not crash, but it failed.")
				  nil))
			 ;; (format t "Restoring...~%")
			 (restore)))))
	    (let ((minimal (minimal-sublist-satisfying nodes
						       #'analyzable-and-has-same-meaning)))
	      ;; (format t "Done computing minimal.~%")
	      (loop
		 for i from 0 upto (1- (length nodes))
		 initially (format t "[")
		 do
		   (if (find i minimal)
		       (format t "+")
		       (format t "-"))
		 finally (format t "]~%"))

	      (write-nodes minimal file-to-minimize)))))))

(defgeneric minimize-notations (article))

(defmethod minimize-notations ((article article))
  (minimize-extension article "eno"))

(defgeneric minimize-reductions (article))

(defmethod minimize-reductions ((article article))
  (minimize-extension article "erd"))

(defgeneric minimize-property-registrations (article))

(defmethod minimize-property-registrations ((article article))
  (minimize-extension article "epr"))

(defgeneric minimize-definientia (article))

(defmethod minimize-definientia ((article article))
  (minimize-extension article "dfs"))

(defgeneric minimize-identifications (article))

(defmethod minimize-identifications ((article article))
  (minimize-extension article "eid"))

(defgeneric minimize-clusters (article))

(defmethod minimize-clusters ((article article))
  (minimize-extension article "ecl"))

(defgeneric minimize-environment (article))

(defmethod minimize-environment ((article article))
  (minimize-notations article)              ;; .eno
  (minimize-reductions article)             ;; .erd
  (minimize-property-registrations article) ;; .epr
  (minimize-definientia article)            ;; .dfs
  (minimize-identifications article)        ;; .eid
  (minimize-clusters article))              ;; .ecl

(defmethod minimize :before ((article article))
  (let* ((analyzer-xml (file-with-extension article "xml"))
	 (analyzer-xml-orig (file-with-extension article "xml.orig")))
    (unless (file-exists-p analyzer-xml)
      (error "The .xml for ~a is missing." (namestring (path article))))
    (copy-file analyzer-xml analyzer-xml-orig)))

(defmethod minimize ((article article))
  (minimize-environment article)
  ;; (minimize-properties article)
  ;; (minimize-requirements article)
  )

(defgeneric minimize-requirements (article &optional working-directory)
  (:documentation "From the explicitly required set of requirements
  for ARTICLE, compute the smallest subset with respect to which the article is still verifiable.  Verification will take place in WORKING-DIRECTORY, if supplied."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minimizing requirements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric requirements-of-article (article)
  (:documentation "The contents of the requirements environment directive of ARTICLE."))

(defmethod requirements-of-article :around ((article-path pathname))
  (if (file-exists-p article-path)
      (let ((evl-file (replace-extension article-path "miz" "evl")))
	(if (file-exists-p evl-file)
	    (call-next-method)
	    (error "The .evl file for~%~%  ~a~%~%does not exist at the expected location~%~%  ~a~%" article-path evl-file)))
      (error "The file~%~%  ~a~%~%does not exist~%" article-path)))

(defgeneric minimize-requirements-of-itemized-db (miz-db)
  (:documentation "Minimize the requirements of every article fragment appearing under MIZ-DB."))

(defmethod minimize-requirements-of-itemized-db :around ((miz-db-path pathname))
  (if (file-exists-p miz-db-path)
      (if (directory-pathname-p miz-db-path)
	  (let ((text-subdir (merge-pathnames "text/" miz-db-path)))
	    (if (file-exists-p text-subdir)
		(call-next-method)
		(error "The supplied mizar database directory,~%~%  ~a~%~%lacks a text subdirectory!~%" miz-db-path)))
	  (error "The supplied mizar database directory,~%~%  ~a~%~%is not actually a directory!~%" miz-db-path))
      (error "The mizar database directory~%~%  ~a~%~%does not exist!~%" miz-db-path)))

(defmethod minimize-requirements-of-itemized-db ((miz-db-path pathname))
  (loop
     with text-subdir = (merge-pathnames "text/" miz-db-path)
     with ckbs = (directory (merge-pathnames "ckb*.miz" text-subdir))
     for ckb in ckbs
     do
       (minimize-requirements ckb miz-db-path)
     finally
       (return t)))

(defun minimize-requirements-of-itemized-db-no-errors (miz-db-path)
  (handler-case
      (progn
	(handler-bind ((warning #'muffle-warning))
	  (minimize-requirements-of-itemized-db miz-db-path))
	(format t "~a: success~%" miz-db-path)
	t)
      (error ()
	(format *error-output* "~a: failure~%" miz-db-path))))

;;; minimize.lisp ends here