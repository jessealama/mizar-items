
(in-package :mizar)

(defclass local-db ()
  ((location
    :type pathname
    :initarg :location
    :accessor location
    :initform (error "Local databases need to be in a directory.")))
  (:documentation "A local-db is a representation of a Mizar local database."))

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
