
(in-package :mizar)

(defclass library ()
  ((articles
    :type list
    :initform nil
    :accessor articles
    :initarg :articles)
   (version
    :type string
    :initform ""
    :accessor :version
    :initarg :version)
   (binary-version
    :type string
    :initform ""
    :accessor binary-version
    :initarg :binary-version)))

(defmethod print-object ((lib library) stream)
  (print-unreadable-object (lib stream)
    (with-slots (articles version binary-version)
	lib
      (if (string= version "")
	  (if (string= binary-version "")
	      (format stream "(version unset) (binary version unset) with ~d articles" (length articles))
	      (format stream "(version unset) (binary version ~a) with ~d articles" binary-version (length articles)))
	  (if (string= binary-version "")
	      (format stream "version ~a (binary version unset) with ~d articles" version (length articles))
	      (format stream "version ~a (binary version ~a) with ~d articles" version binary-version (length articles)))))))

(define-condition duplicate-article-error (error)
  ((library
    :initarg :library
    :reader library)
   (article
    :initarg :article
    :reader article))
  (:documentation "An error representing the addition to a library of
  an article that already exists under the same name as the proposed article.")
  (:report
   (lambda (condition stream)
     (with-slots (library name)
	 condition
       (format stream "There is already an article in the library ~a with the name '~a'" library name)))))

(defmethod add-article ((library library) (article article))
  (if (member article (articles library) :test #'string= :key #'name)
      (error 'duplicate-article-error
	     :library library
	     :article article)
      (prog2
	  (push article (articles library))
	  library)))

(defclass mizar-library ()
  ((location
    :initarg :location
    :accessor location
    :type string
    :initform *mizfiles*)
   (mml-lar
    :accessor mml-lar
    :type list))
  (:documentation "A wrapper around a copy of the MML."))

(defmethod initialize-instance :after ((lib mizar-library) &key)
  (setf (mml-lar lib) 
	(lines-of-file (concat *mizfiles* "mml.lar"))))

(defmethod print-object ((lib mizar-library) stream)
  (with-slots (location mml-lar)
      lib
    (print-unreadable-object (lib stream :type nil)
      (format stream "location ~A, with ~d articles" location (length mml-lar)))))

(defparameter *default-mizar-library* (make-instance 'mizar-library)
  "A Mizar library using the value of MIZFILES in the environment.")