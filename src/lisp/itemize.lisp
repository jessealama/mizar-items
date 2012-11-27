;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

(defparameter *split-stylesheet* (path-for-stylesheet "split"))
(defparameter *itemize-stylesheet* (path-for-stylesheet "itemize"))
(defparameter *extend-evl-stylesheet* (path-for-stylesheet "extend-evl"))
(defparameter *wsm-stylesheet* (path-for-stylesheet "wsm"))
(defparameter *print-evl-stylesheet* (path-for-stylesheet "print-evl"))

(defparameter *dom-builder* (cxml-dom:make-dom-builder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Itemization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric itemize (article)
  (:documentation "Break up ARTICLE into its constituent items."))

(defmethod itemize :around ((article-path pathname))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "There is no file at '~a'." (native-namestring article-path))))

(defmethod itemize ((article-path pathname))
  (itemize (make-instance 'article :path article-path)))

(defmethod itemize :around ((article article))
  (let* ((path (path article))
	 (directory (pathname (directory-namestring path)))
	 (file (pathname-name path))
	 (itemization-path (merge-pathnames file directory))
	 (itemization-directory (pathname-as-directory itemization-path)))
    (when (file-exists-p itemization-directory)
      (error "The itemization directory~%~%  ~a~%~%already exists." (namestring itemization-directory)))
    (ensure-directories-exist itemization-directory)
    #+ccl
    (ccl::with-preserved-working-directory (itemization-directory)
      (call-next-method))
    #+sbcl
    (let ((cwd (sb-posix:getcwd)))
      (sb-posix:chdir itemization-directory)
      (unwind-protect (call-next-method)
	(sb-posix:chdir cwd)))
    #-(or sbcl ccl)
    (error "Don't know how to change directory to the itemization directory.")
    ))

(defmethod itemize :before ((article article))
  (let* ((path (path article))
	 (directory (pathname (directory-namestring path)))
	 (file (pathname-name path))
	 (itemization-path (merge-pathnames file directory))
	 (itemization-directory (pathname-as-directory itemization-path))
	 (db (make-instance 'itemized-db
			    :location itemization-directory
			    :article article))
	 (article-in-db (copy-to-db article db))
	 (split-wsx (file-with-extension article-in-db "wsx.split"))
	 (itemized-wsx (file-with-extension article-in-db "wsx.split.itemized")))
    (apply-stylesheet *split-stylesheet* article-in-db nil split-wsx)
    (apply-stylesheet *itemize-stylesheet* split-wsx nil itemized-wsx)))

(defmethod itemize ((article article))
  (let* ((path (path article))
	 (directory (pathname (directory-namestring path)))
	 (file (pathname-name path))
	 (itemization-directory (pathname-as-directory (merge-pathnames file directory)))
	 (db (make-instance 'itemized-db
			    :location itemization-directory
			    :article article))
	 (article-in-db (copy-to-db article db))
	 (itemized-wsx (file-with-extension article-in-db "wsx.split.itemized"))
	 (article-evl (file-with-extension article-in-db "evl"))
	 (text-subdir (text-subdirectory db))
	 (itemized-xml-doc (cxml:parse-file itemized-wsx *dom-builder*))
	 (text-proper-nodes (remove-if #'dom:text-node-p
				       (dom:child-nodes
					(dom:document-element itemized-xml-doc))))
	 (num-fragments (length text-proper-nodes)))

    (loop
       for text-proper-node across text-proper-nodes
       for i from 1 upto num-fragments
       for ckb-wsx = (merge-pathnames (format nil "ckb~d.wsx" i) text-subdir)
       for wsx-doc = (cxml-dom:create-document text-proper-node)
       do
	 (with-open-file (ckb ckb-wsx
			      :direction :output
			      :element-type '(unsigned-byte 8)
			      :if-exists :error)
	   (dom:map-document (cxml:make-octet-stream-sink ckb) wsx-doc)))

    ;; make the .tpr files for the fragments
    (loop
       for i from 1 upto num-fragments
       for ckb-wsx = (merge-pathnames (format nil "ckb~d.wsx" i) text-subdir)
       for ckb-tpr = (merge-pathnames (format nil "ckb~d.tpr" i) text-subdir)
       do
	 (apply-stylesheet *wsm-stylesheet* ckb-wsx nil ckb-tpr))

    ;; create the new .evl, new .evd, then the new .miz; verify; repeat.
    (loop
       for i from 1 upto num-fragments
       for ckb-wsx = (merge-pathnames (format nil "ckb~d.wsx" i) text-subdir)
       for ckb-tpr = (merge-pathnames (format nil "ckb~d.tpr" i) text-subdir)
       for ckb-evd = (merge-pathnames (format nil "ckb~d.evd" i) text-subdir)
       for ckb-evl = (merge-pathnames (format nil "ckb~d.evl" i) text-subdir)
       for ckb-basename = (merge-pathnames (format nil "ckb~d" i) text-subdir)
       for ckb-miz = (merge-pathnames (format nil "ckb~d.miz" i) text-subdir)
       for ckb-xml = (merge-pathnames (format nil "ckb~d.xml" i) text-subdir)
       for ckb-xml-orig = (merge-pathnames (format nil "ckb~d.xml.orig" i) text-subdir)
       for vocabularies = (mapcar #'pathname-name (vocabulary-files db))
       for notations = (mapcar #'pathname-name (notation-files db))
       for definitions = (mapcar #'pathname-name (definition-files db))
       for schemes = (mapcar #'pathname-name (scheme-files db))
       for theorems = (mapcar #'pathname-name (theorem-files db))
       for registrations = (mapcar #'pathname-name (registration-files db))
       for constructors = (mapcar #'pathname-name (constructor-files db))
       for requirements = (mapcar #'pathname-name (requirement-files db))
       for extend-evl-parameters = (list
				    (cons "vocabularies" (tokenize (mapcar #'uppercase vocabularies)))
				    (cons "notations" (tokenize (mapcar #'uppercase notations)))
				    (cons "definitions" (tokenize (mapcar #'uppercase definitions)))
				    (cons "theorems" (tokenize (mapcar #'uppercase theorems)))
				    (cons "registrations" (tokenize (mapcar #'uppercase registrations)))
				    (cons "constructors" (tokenize (mapcar #'uppercase constructors)))
				    (cons "requirements" (tokenize (mapcar #'uppercase requirements)))
				    (cons "schemes" (tokenize (mapcar #'uppercase schemes))))
       do
	 (apply-stylesheet *wsm-stylesheet* ckb-wsx nil ckb-tpr)
	 (apply-stylesheet *extend-evl-stylesheet*
			   article-evl
			   extend-evl-parameters
			   ckb-evl)
	 (apply-stylesheet *print-evl-stylesheet*
			   ckb-evl
			   nil
			   ckb-evd)
	 (stop-if-nil
	   (mglue ckb-basename)
	   (accom ckb-miz)
	   (verifier ckb-miz)
	   (copy-file ckb-xml ckb-xml-orig)
	   (exporter ckb-miz)
	   (transfer ckb-miz)))
    db))

;;; itemize.lisp ends here
