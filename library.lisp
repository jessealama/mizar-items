
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
  (if (member article (articles library)
	      :test #'equal-article)
      (error 'duplicate-article-error
	     :library library
	     :article article)
      (push article (articles library))))