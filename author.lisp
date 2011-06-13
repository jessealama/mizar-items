
(in-package :mizar)

(defclass author ()
  ((name
    :type string
    :accessor name
    :initarg :name
    :documentation "The name of an author.")))

(defmethod initialize-instance ((author author) &rest initargs)
  (if (null (name author))
      (error "Authors must have a name")
      (call-next-method)))