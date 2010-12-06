;;; article.lisp A representation of mizar articles

(in-package :mizar)

(defclass article ()
  ((name
    :initarg :name
    :accessor name
    :type string
    :documentation "The name of this article, as a string.")
   (path
    :initarg :path
    :reader path
    :type pathname)
   (vocabularies
    :initarg :vocabularies
    :accessor vocabularies
    :type list)
   (notations
    :initarg :notations
    :accessor notations
    :type list)
   (constructors
    :initarg :constructors
    :accessor constructors
    :type list)
   (requirements
    :initarg :requirements
    :accessor requirements
    :type list)
   (definitions
     :initarg :definitions
     :accessor definitions
     :type list)
   (theorems
    :initarg :theorems
    :accessor theorems
    :type list)
   (schemes
    :initarg :schemes
    :accessor schemes
    :type list))
  (:documentation "A representation of a mizar article."))

(defgeneric complete-environment (article)
  (:documentation "All articles mentioned in the environment of
ARTICLE.  The notations, constructors, requirements, definitions,
theorems, and schemes directives are consulted; the vocabularies
directive is not consulted."))

(defmethod complete-environment ((article article))
  (remove-duplicates (append (notations article)
			     (constructors article)
			     (requirements article)
			     (definitions article)
			     (theorems article)
			     (schemes article))))

;;; article.lisp ends here