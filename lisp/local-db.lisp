
(in-package :mizar)

(defclass local-db ()
  ((location
    :type pathname
    :initarg :location
    :accessor location
    :initform (error "Local databases need to be in a directory.")))
  (:documentation "A local-db is a representation of a Mizar local database."))

(defmethod print-object ((db local-db) stream)
  (let* ((articles (article-paths db))
	 (vocabularies (vocabulary-files db))
	 (location (location db)))
    (print-unreadable-object (db stream :type t :identity nil)
      (format stream "~a (~d vocabulary files, ~d articles)"
	      (namestring location)
	      (length vocabularies)
	      (length articles)))))

(defun subdirectory-of-db (db subdir)
  (pathname-as-directory (merge-pathnames subdir (location db))))

(defun dict-subdirectory (db)
  (subdirectory-of-db db "dict"))

(defun prel-subdirectory (db)
  (subdirectory-of-db db "prel"))

(defun text-subdirectory (db)
  (subdirectory-of-db db "text"))

(defmethod initialize-instance :after ((db local-db)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((dict-subdir (dict-subdirectory db))
	(prel-subdir (prel-subdirectory db))
	(text-subdir (text-subdirectory db)))
    (ensure-directories-exist dict-subdir)
    (ensure-directories-exist prel-subdir)
    (ensure-directories-exist text-subdir)))

(defgeneric copy-to-db (thing db)
  (:documentation "Copy THING to the local database DB."))

(defmethod copy-to-db ((thing pathname) (db local-db))
  (let ((name (pathname (file-namestring thing)))
	(location (location db)))
    (let ((name-in-db (merge-pathnames name location)))
      (copy-file thing name-in-db))))

(defgeneric vocabulary-files (db))

(defmethod vocabulary-files ((db pathname))
  (let ((dict-subdir (pathname-as-directory (merge-pathnames "dict" db))))
    (remove-if-not #'(lambda (path)
		       (scan "\.voc$" (namestring path)))
		   (remove-if #'directory-pathname-p
			      (list-directory dict-subdir)))))

(defmethod vocabulary-files ((db local-db))
  (vocabulary-files (location db)))

(defgeneric prel-files (db))

(defmethod prel-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (remove-if #'directory-pathname-p
	       (list-directory prel-subdir))))

(defmethod prel-files ((db local-db))
  (prel-files (location db)))

(defgeneric text-files (db))

(defmethod text-files ((db pathname))
  (let ((text-subdir (pathname-as-directory (merge-pathnames "text" db))))
    (remove-if #'directory-pathname-p
	       (list-directory text-subdir))))

(defmethod text-files ((db local-db))
  (text-files (location db)))

(defgeneric article-paths (db))

(defmethod article-paths ((db local-db))
  (let* ((text-subdir (text-subdirectory db))
	 (article-wildcard (format nil "~a*.miz" (namestring text-subdir))))
    (directory article-wildcard)))

(defun files-in-directory-with-extension (directory extension)
  (remove-if-not #'(lambda (path)
		     (string= (pathname-type path) extension))
		 (remove-if #'directory-pathname-p
			    (list-directory directory))))

(defgeneric notation-files (db))

(defmethod notation-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "dno")))

(defmethod notation-files ((db local-db))
  (notation-files (location db)))

(defgeneric definition-files (db))

(defmethod definition-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "def")))

(defmethod definition-files ((db local-db))
  (definition-files (location db)))

(defgeneric scheme-files (db))

(defmethod scheme-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "sch")))

(defmethod scheme-files ((db local-db))
  (scheme-files (location db)))

(defgeneric identification-files (db))

(defmethod identification-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "did")))

(defmethod identification-files ((db local-db))
  (identification-files (location db)))

(defgeneric reduction-files (db))

(defmethod reduction-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "drd")))

(defmethod reduction-files ((db local-db))
  (reduction-files (location db)))

(defgeneric cluster-files (db))

(defmethod cluster-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "dcl")))

(defmethod cluster-files ((db local-db))
  (cluster-files (location db)))

(defgeneric property-reduction-files (db))

(defmethod property-reduction-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "epr")))

(defmethod property-reduction-files ((db local-db))
  (property-reduction-files (location db)))

(defgeneric registration-files (db))

(defmethod registration-files ((db pathname))
  (append (property-reduction-files db)
	  (reduction-files db)
	  (cluster-files db)
	  (identification-files db)))

(defmethod registration-files ((db local-db))
  (registration-files (location db)))

(defgeneric theorem-files (db))

(defmethod theorem-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "the")))

(defmethod theorem-files ((db local-db))
  (theorem-files (location db)))

(defgeneric requirements-files (db))

(defmethod requirement-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "dre")))

(defmethod requirement-files ((db local-db))
  (requirement-files (location db)))

(defgeneric constructors-files (db))

(defmethod constructor-files ((db pathname))
  (let ((prel-subdir (pathname-as-directory (merge-pathnames "prel" db))))
    (files-in-directory-with-extension prel-subdir "dco")))

(defmethod constructor-files ((db local-db))
  (constructor-files (location db)))
