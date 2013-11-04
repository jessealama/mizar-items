
(in-package :mizar)

(defclass environment ()
  (vocabularies
   :type list
   :initarg :vocabularies
   :initform nil
   :accessor vocabularies))
