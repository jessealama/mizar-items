;;; mizar-xsl.lisp -- Applying stylesheets to mizar XML content

(in-package :mizar)

;;; absrefs

(defparameter *xsl4mizar-root* 
  (ensure-directories-exist (mizar-items-config 'xsl4mizar-path)))
(defparameter *addabsrefs-stylesheet*
  (probe-file (make-pathname :directory *xsl4mizar-root*
			     :name "addabsrefs.xsl")))

(defgeneric absrefs (article))

(defmethod absrefs ((article-path pathname))
  (let ((article-xml-path (replace-extension article-path "miz" "xml"))
	(new-article-xml-path (replace-extension article-path "miz" "xml1")))
    (if (probe-file article-xml-path)
	(progn
	  (run-program "xsltproc"
		       (list (namestring *addabsrefs-stylesheet*)
			     (namestring article-xml-path)
			     "-o"
			     (namestring new-article-xml-path))
		       :search t)
	  t)
	(error "File does not exist: ~S" article-xml-path))))

(defmethod absrefs ((article-path string))
  (absrefs (pathname article-path)))

(defmethod absrefs ((article article))
  (absrefs (path article))
  (refresh-xml article))

(defgeneric mhtml (article)
  (:documentation "Compute the HTML representation of an article."))

(defmethod mhtml ((article-path-string string))
  (mhtml (pathname article-path-string)))

(defmethod mhtml ((article-path pathname))
  (let ((new-article-xml-path (replace-extension article-path "miz" "xml1"))
	(article-html-path (replace-extension article-path "miz" "html")))
    (if (probe-file new-article-xml-path)
	(run-program "xsltproc"
		     (list (namestring (mizar-items-config 'mhtml-stylesheet))
			   (namestring new-article-xml-path)
			   "-o"
			   (namestring article-html-path))
		     :search t)
	(error "File does not exist: ~S" new-article-xml-path))))

(defgeneric environment (article))

(defmethod environment :around ((article-path pathname))
  (let ((evl-file (replace-extension article-path "miz" "evl")))
    (if (file-exists-p evl-file)
	(call-next-method)
	(error "The .evl file for ~a doesn't exist" (namestring article-path)))))

(defmethod environment ((article-path pathname))
  (let ((evl-file (replace-extension article-path "miz" "evl")))
    (apply-stylesheet (mizar-items-config 'env-stylesheet) evl-file nil nil)))

(defmethod environment ((article article))
  (error "Computing the environment for an article isn't supported yet!"))

;;; mizar-xsl.lisp ends here