(defmacro with-stylesheet ((stylesheet-pathname) &body body)
  `(invoke-with-stylesheet (lambda () ,@body) ,stylesheet-pathname))

(defun invoke-with-stylesheet (fn stylesheet-pathname)
  (xuriella:apply-stylesheet (pathname stylesheet-pathname)
                             (cxml:with-xml-output (stp:make-builder)
                               (funcall fn))
			     :parameters (list (xuriella:make-parameter "0" "mk_header" "mk_header"
					  )
					       (xuriella:make-parameter "1" "generate_items" "generate_items"))))

(defun show-directory ()
  (with-stylesheet ("/Users/alama/sources/mizar/mizar-items/directory.xsl")
    (cxml:with-element "directory"
      (let ((directory (user-homedir-pathname)))
	(cxml:attribute "namestring" (namestring directory))
	(dolist (file (directory (merge-pathnames "*.*" directory)))
	  (cxml:with-element "file"
	    (cxml:text (enough-namestring file directory))))))))

(defun show-miz ()
  (with-stylesheet ("/Users/alama/sources/mizar/xsl4mizar/MHTML/mhtml_main.xsl")
    (cxml:parse-file "/Users/alama/sources/mizar/mizar-items/itemization/xboole_0/text/ckb1.xml1" (cxml-dom:make-dom-builder))))