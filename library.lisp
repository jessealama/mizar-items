
(in-package :mizar)

(defclass library ()
  ((articles
    :type list
    :accessor articles
    :initarg :articles)
   (version
    :type string
    :accessor :version
    :initarg :version)
   (binary-version
    :type string
    :accessor binary-version
    :initarg :binary-version)))

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