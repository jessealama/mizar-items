
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
