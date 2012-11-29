;;; article.lisp A representation of mizar articles

(in-package :mizar)

(defclass article ()
  ((path
    :initarg :path
    :initform (error "Every article must have a path.")
    :accessor path
    :type pathname))
  (:documentation "A representation of a Mizar article."))

(defmethod initialize-instance :after ((article article)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((path (path article)))
    (if (file-exists-p path)
	(let ((extension (pathname-type path)))
	  (if extension
	      (unless (string= extension "miz")
		(error "The extension of a Mizar article should be '.miz'; the given path~%~%  ~a~%~%does not have this extension." (namestring path)))))
	(error "There is no file at '~a'." (namestring path))))
  article)

(defmethod print-object ((article article) stream)
  (print-unreadable-object (article stream :type t :identity nil)
    (format stream "~a" (namestring (path article)))))

(defgeneric file-with-extension (article extension))

(defmethod file-with-extension ((article-path pathname) extension)
  (let ((dirname (directory-namestring article-path))
	(name (pathname-name article-path)))
    (let ((file (format nil "~a.~a" name extension)))
      (merge-pathnames file dirname))))

(defmethod file-with-extension ((article article) extension)
  (let* ((path (path article))
	 (name (pathname-name path))
	 (dir (pathname (directory-namestring path))))
    (merge-pathnames (format nil "~a.~a" name extension) dir)))

(defgeneric err-file (article))

(defmethod err-file ((article article))
  (file-with-extension article "err"))

(defmethod err-file ((article-path pathname))
  (let ((directory (pathname-as-directory (pathname (directory-namestring article-path))))
	(name (pathname-name article-path)))
    (merge-pathnames (format nil "~a.err" name) directory)))

(defgeneric miz-file (article))

(defmethod miz-file ((article article))
  (file-with-extension article "miz"))

(defgeneric empty-err-file? (article))

(defmethod empty-err-file? ((article article))
  (empty-err-file? (path article)))

(defmethod empty-err-file? ((article-path pathname))
  (zerop
   (with-open-file (file (err-file article-path))
     (file-length file))))

(defun message-file ()
  (merge-pathnames "mizar.msg" (mizfiles)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Articles and local databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy-to-db ((thing article) (db local-db))
  (let* ((name (pathname-name (path thing)))
	 (new-path (merge-pathnames (format nil "~a.miz" name) (location db))))
    (copy-to-db (path thing) db)
    (make-instance 'article
		   :path new-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying stylesheets to articles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod apply-stylesheet (stylesheet (thing article) parameters output)
  (makeenv thing)
  (wsmparser thing)
  (let ((wsx-file (file-with-extension thing "wsx")))
    (apply-stylesheet stylesheet wsx-file parameters output)))

(defgeneric parse-tree (article)
  (:documentation "An XML document representing the parse tree of ARTICLE."))

(defmethod parse-tree ((article article))
  (parse-tree (path article)))

(defmethod parse-tree :before ((article pathname))
  (unless (file-exists-p (file-with-extension article "wsx"))
    (makeenv article)
    (wsmparser article)))

(defmethod parse-tree ((article pathname))
  (parse-xml-file (file-with-extension article "wsx")))

(defgeneric items (article)
  (:documentation "The toplevel items of ARTICLE"))

(defmethod items ((article article))
  (items (path article)))

(defmethod items :before ((article pathname))
  (unless (file-exists-p (file-with-extension article "wsx"))
    (makeenv article)
    (wsmparser article)))

(defparameter *miz2lisp-stylesheet* (path-for-stylesheet "miz2lisp"))

(defmethod items ((article pathname))
  (let ((wsx-doc (cxml:parse-file (file-with-extension article "wsx")
				  (cxml-dom:make-dom-builder))))
    (children (make-item-from-xml (dom:document-element wsx-doc)))))

(defun form->item (form)
  (multiple-value-bind (expanded expandable)
      (macroexpand-1 form)
    (if expandable
	expanded
	(error "The form~%~%  ~a~%~%did not match any known form-to-item rules." form))))

(defmacro |text-proper| ((articleid) &body body)
  (make-instance 'text-proper-item
		 :articleid articleid
		 :toplevel-items (mapcar #'form->item body)))

(defmacro |section-pragma| ()
  (make-instance 'section-pragma-item))

(defmacro |reserve| (&key |variables| |type|)
  (unless (every #'symbolp |variables|)
    (error "All reserved variables in a reservation ought to be symbols."))
  (make-instance 'reservation-item
		 :variables |variables|
		 :type (form->item |type|)))

(defmacro |standard-type| (&body body)
  (unless body
    (error "Empty standard type."))
  (let ((radix (first body)))
    (unless (symbolp radix)
      (error "Non-symbol radix:~%~%  ~a~%" radix))
    (make-instance 'standard-type
		   :radix radix
		   :arguments (mapcar #'form->item (rest body)))))

(defclass mizar-item ()
  nil)

(defclass text-proper-item (mizar-item)
  ((articleid
    :type symbol
    :initarg :articleid
    :accessor articleid)
   (toplevel-items
    :type list
    :initarg :toplevel-items
    :accessor toplevel-items)))

(defclass section-pragma-item (mizar-item)
  nil)

(defclass reservation-item (mizar-item)
  ((variables
    :type list
    :accessor variables
    :initarg :variables
    :initform (error "To create a reservation, please supply a non-null list of variables."))
   (type
    :type mizar-type
    :accessor reserved-type
    :initarg :type
    :initform (error "To create a reservation, please supply a type for the reserved variables."))))

(defclass mizar-type (mizar-item)
  nil)

(defclass standard-type (mizar-type)
  ((radix
    :type symbol
    :accessor radix
    :initarg :radix
    :initform (error "To create a standard type, please supply a radix."))
   (arguments
    :type list
    :accessor arguments
    :initarg :arguments
    :initform nil)))

(defgeneric parse (article))

(defmethod parse ((article article))
  (parse (path article)))

(defmethod parse :before ((article pathname))
  (let ((tpr-path (file-with-extension article "tpr"))
	(msm-path (file-with-extension article "msm")))
    (makeenv article)
    (wsmparser article)
    (msmprocessor article)
    (msplit article)
    (copy-file msm-path tpr-path)
    (mglue article)
    (accom article)
    (wsmparser article)
    (msmprocessor article)))

(defmethod parse ((article pathname))
  (let ((wsx (file-with-extension article "wsx")))
    (unless (file-exists-p wsx)
      (error "The .wsx file is missing for ~a." (native-namestring article)))
    (let ((article-as-lisp-string (apply-stylesheet *miz2lisp-stylesheet* wsx nil nil)))
      (let ((article-as-lisp (with-readtable (find-readtable :modern)
			       (read-from-string article-as-lisp-string))))
	(form->item article-as-lisp)))))
