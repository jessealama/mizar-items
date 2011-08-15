;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Itemization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun verify-and-export (article directory)
  (accom article directory "-q" "-l" "-s")
  (verifier article directory "-q" "-l" "-s")
  (exporter article directory "-q" "-l" "-s")
  (transfer article directory "-q" "-l" "-s"))

(defun write-new-symbols (symbols symbol-table directory)
  "For each symbol (actually, a string) in SYMBOLS that is not already
  accounted for in SYMBOL-TABLE (i.e., appearing as a key in the
  table), write a vocabulary file under DIRECTORY.  The filenames will
  all have the form SYM<n>.voc, where <n> is a natural number.  Since
  the values of SYMBOL-TABLE are assumed to be numbers, we look at the
  maximum of these numbers, add 1, and start <n> there, increasing it
  by one as we go through the list of symbols in SYMBOLS that do not
  appear as keys in SYMBOL-TABLE.  SYMBOL-TABLE will be modified: any
  symbol in SYMBOLS not appearing in SYMBOL-TABLE as a key will be put
  into SYMBOL-TABLE as a key, with the value corresponding to whatever
  <n> is for the symbol.  The (potentially) modified SYMBOL-TABLE is the final value."
  (let ((vals (values-of-table symbol-table))
	(keys (keys symbol-table))
	(next-symbol-number 1))
    (unless (null vals)
      (setf next-symbol-number (1+ (apply 'max vals))))
    (let ((new-symbols (set-difference symbols keys :test #'string=)))
      (loop
	 for sym in new-symbols
	 for i from next-symbol-number
	 for voc-filename = (format nil "sym~d.voc" i)
	 for voc-path = (concat directory voc-filename)
	 do
	   (with-open-file (sym-file voc-path :direction :output)
	     (format sym-file "~A~%" sym))
	   (setf (gethash sym symbol-table) i)
	 finally
	   (return symbol-table)))))

(defun itemize-no-errors (article)
  (handler-case
      (progn
	(handler-bind ((warning #'muffle-warning))
	  (itemize article))
	(format t "~a: success~%" article)
	t)
      (error ()
	(format *error-output* "~a: failure~%" article)
	nil)))

(defgeneric xsl-split-article (article)
  (:documentation "Divide any 'multi-part' elements of ARTICLE.  This means:

* divide definition blocks that define multiple things into several 'singleton' definition blocks,
* likewise for notation blocks,
* divide reservations that reserve multiple variables into 'singleton' reservations"))

(defmethod xsl-split-article ((article article))
  (error "We haven't yet defined XSL-SPLIT-ARTICLE for objects of class ARTICLE.  Sorry."))

(defmethod xsl-split-article ((article string))
  "Split an article given as a string.

If ARTICLE is the empty string, signal an error.  If ARTICLE is not the empty string, look at its first character.  If the first character is a forward slash '/', then interpret ARTICLE as a path to an article file on disk, and proceed accordingly.  Otherwise, interpret ARTICLE as the string representation of a MIZAR article, save the article to a temporary location on disk, and proceed as if ARTICLE were that file on disk."
  (if (string= article "")
      (error "We cannot split an empty article!")
      (let ((first-char (char article 0)))
	(if (char= first-char #\/)
	    (xsl-split-article (pathname article))
	    (let ((temp-article-path (temporary-file :extension ".miz")))
	      (with-open-file (temp-article temp-article-path
					    :direction :output
					    :if-exists :error
					    :if-does-not-exist :create)
		(format temp-article "~a" article))
	      (xsl-split-article temp-article-path)
	      (delete-file temp-article-path))))))

(defmethod xsl-split-article ((article pathname))
  (let ((article-wsx (replace-extension article "miz" "wsx"))
	(split-stylesheet (mizar-items-config 'split-stylesheet)))
    (let ((first-one (apply-stylesheet split-stylesheet article-wsx nil nil)))
      ;; we have to apply this stylesheet twice to ensure that loci
      ;; are truly sequentually numbered.  One pass isn't enough.
      ;; Perhaps there should simply be a separate stylesheet to
      ;; accomplish this.
      (let ((second-one (apply-stylesheet split-stylesheet first-one nil nil)))
	(apply-stylesheet split-stylesheet second-one nil nil)))))

(defun xsl-itemize-article (article)
  (let ((free-variables-stylesheet (mizar-items-config 'free-variables-stylesheet))
	(itemize-stylesheet (mizar-items-config 'itemize-stylesheet)))
    (let ((free-variables-article (apply-stylesheet free-variables-stylesheet (xsl-split-article article) nil nil)))
      (apply-stylesheet itemize-stylesheet free-variables-article nil nil))))

(defun ckb-< (ckb-path-1 ckb-path-2)
  (let ((ckb-pattern "^ckb([0-9]+)$"))
    (register-groups-bind (ckb-num-1-as-str)
	(ckb-pattern (pathname-name ckb-path-1))
      (register-groups-bind (ckb-num-2-as-str)
	  (ckb-pattern (pathname-name ckb-path-2))
	(let ((ckb-num-1 (parse-integer ckb-num-1-as-str))
	      (ckb-num-2 (parse-integer ckb-num-2-as-str)))
	  (< ckb-num-1 ckb-num-2))))))

(defgeneric extend-evl (evl-file prel-dir)
  (:documentation "Extend the .evl file EVL-FILE with whatever the contents of PREL-DIR.  If, for example, there is a file 'foo.sch' in PREL-DIR, then EVL-FILE will be extended so that, in its Schemes directives, we find 'FOO' as an Ident."))

(defmethod extend-evl ((evl-file string) prel-dir)
  (extend-evl (pathname evl-file) prel-dir))

(defmethod extend-evl (evl-file (prel-dir string))
  (extend-evl evl-file (pathname prel-dir)))

(defmethod extend-evl :around ((evl-file pathname) (prel-dir pathname))
  (if (file-exists-p evl-file)
      (if (file-exists-p prel-dir)
	  (if (directory-p prel-dir)
	      (call-next-method)
	      (error "The specified prel DB, '~a', isn't actually a directory" (namestring prel-dir)))
	  (call-next-method))
      (error "The specified .evl file, '~a', doesn't exist" (namestring evl-file))))

(defmethod extend-evl ((evl-file pathname) (prel-dir pathname))
  (if (file-exists-p prel-dir)
      (let ((more-notations "")
	    (more-definitions "")
	    (more-theorems "")
	    (more-schemes "")
	    (more-registrations "")
	    (more-constructors "")
	    (more-requirements ""))
	(flet ((pad-string (string new-bit)
		 (format nil "~a~a," string (uppercase new-bit))))
	  (flet ((add-to-notations (article)
		   (setf more-notations (pad-string more-notations article)))
		 (add-to-definitions (article)
		   (setf more-definitions (pad-string more-definitions article)))
		 (add-to-theorems (article)
		   (setf more-theorems (pad-string more-theorems article)))
		 (add-to-schemes (article)
		   (setf more-schemes (pad-string more-schemes article)))
		 (add-to-registrations (article)
		   (setf more-registrations (pad-string more-registrations article)))
		 (add-to-constructors (article)
		   (setf more-constructors (pad-string more-constructors article)))
		 (add-to-requirements (article)
		   (setf more-requirements (pad-string more-requirements article))))
	    (flet ((dispatch-exported-file (path)
		     (cond ((dno-file-p path) (add-to-notations (pathname-name path)))
			   ((dcl-file-p path) (add-to-registrations (pathname-name path)))
			   ((eid-file-p path) (add-to-registrations (pathname-name path)))
			   ((sch-file-p path) (add-to-schemes (pathname-name path)))
			   ((dco-file-p path) (add-to-constructors (pathname-name path)))
			   ((def-file-p path) (add-to-definitions (pathname-name path)))
			   ((the-file-p path) (add-to-theorems (pathname-name path)))
			   (t
			    (error "Don't know how to deal with the prel file '~a'" (namestring path))))))
	      (loop
		 for extension in (list "dno" "dcl" "eid" "sch" "def" "dco" "the")
		 do
		   (loop
		      with files = (files-in-directory-with-extension prel-dir extension)
		      with sorted-files = (sort files #'ckb-<)
		      for path in sorted-files 
		      do (dispatch-exported-file path))))))
	(apply-stylesheet (mizar-items-config 'extend-evl-stylesheet)
			  evl-file
			  (list (cons "notations" more-notations)
				(cons "definitions" more-definitions)
				(cons "theorems" more-theorems)
				(cons "schemes" more-schemes)
				(cons "registrations" more-registrations)
				(cons "constructors" more-constructors)
				(cons "requirements" more-requirements))
			  nil))
      (apply-stylesheet (mizar-items-config 'extend-evl-stylesheet)
		      evl-file
		      nil
		      nil)))

(defgeneric itemize (article))

(defmethod itemize :around ((article-path pathname))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "There is no article at ~a" article-path)))

(defmethod itemize :before ((article-path pathname))
  (accom article-path :flags '("-q" "-l"))
  (newparser article-path :flags '("-q" "-l")))

(defmethod itemize ((article-path pathname))
  (let* ((itemized-article (xsl-itemize-article article-path))
	 (xml-doc (handler-case (cxml:parse itemized-article (cxml-dom:make-dom-builder))
		    (error (err) (error "There was an error parsing the result of itemizing the article at~%~%  ~a;~%~%The error was: ~a" article-path err))))
	 (evl-file (replace-extension article-path "miz" "evl"))
	 (bundle-xpath "Items/Item-Bundle")
	 (article-name (pathname-name article-path))
	 (wsm-stylesheet (mizar-items-config 'wsm-stylesheet))
	 (items-dir (format nil "/~{~a/~}~a/" (cdr (pathname-directory article-path)) article-name))
					;                                              ^^^ PATHNAME-DIRECTORY gives a list with a useless first component
					;                                     ^ ensures that the path ends with '/'
					;                                ^ ensures that the path starts with '/'
	 (prel-dir (format nil "~aprel/" items-dir)))
					;                               ^^^ squishing these together is OK because ITEMS-DIR ends with a '/'
    (handler-case
	(ensure-directories-exist items-dir)
      (file-error () (error "We cannot ensure that the directory '~a' exists, so we cannot save the items of ~a into directory." items-dir article-name)))
    (xpath:do-node-set (bundle (xpath:evaluate bundle-xpath xml-doc))
      (let* ((bundlenr (dom:get-attribute bundle "bundlenr")))
	(let ((text-proper-set (xpath:evaluate "Text-Proper[1]" bundle)))
	  (if (xpath:node-set-empty-p text-proper-set)
	      (error "Empty node set for Text-Proper!")
	      (let* ((text-proper (first (xpath:all-nodes text-proper-set)))
		     (doc (rune-dom:create-document text-proper))
		     (bundle-path (format nil "~a~a.wsi" items-dir bundlenr)))
					;                                     ^^^^ we can squash these together like this because ITEMs-DIR starts and ends with a '/'
		(with-open-file (bundle-xml bundle-path
					    :direction :output
					    :if-does-not-exist :create
					    :if-exists :supersede
					    :element-type '(unsigned-byte 8))
					;                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
					;                                           Watch out: omitting this key can lead to trouble
		  (dom:map-document (cxml:make-octet-stream-sink bundle-xml) doc))
		;; create the bundle's new evl
		(let ((bundle-temp-evl-path (format nil "~ackb~a.evl1" items-dir bundlenr))
		      (extended-evl (extend-evl evl-file prel-dir)))
		  (write-string-into-file extended-evl bundle-temp-evl-path
					  :if-exists :supersede
					  :if-does-not-exist :create)
		  (let ((bundle-miz-path (format nil "~ackb~a.miz" items-dir bundlenr))
			(bundle-miz-text (apply-stylesheet wsm-stylesheet
							   bundle-path
							   (list (cons "evl" (namestring bundle-temp-evl-path)))
							   nil)))
		    (write-string-into-file bundle-miz-text bundle-miz-path
					    :if-exists :supersede
					    :if-does-not-exist :create)
		    (accom bundle-miz-path :working-directory items-dir :flags '("-q" "-l"))
		    (verifier bundle-miz-path :working-directory items-dir :flags '("-q" "-l"))
		    (exporter bundle-miz-path :working-directory items-dir :flags '("-q" "-l"))
		    (transfer bundle-miz-path :working-directory items-dir :flags '("-q" "-l")))))))))
    (xpath:evaluate (format nil "count(~a)" bundle-xpath) xml-doc)))

;;; itemize.lisp ends here