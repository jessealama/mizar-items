
(in-package :mizar)

(defgeneric constructors-and-properties (atr-file)
  (:documentation "The constructors in ATR-FILE that have at least one property attached to them."))

(defmethod constructors-and-properties ((atr-file string))
  (constructors-and-properties (pathname atr-file)))

(defmethod constructors-and-properties ((atr-file pathname))
  (xuriella:apply-stylesheet (mizar-items-config 'propertied-constructors-stylesheet) atr-file))

(defgeneric constructors-with-properties (atr-file))

(defmethod constructors-with-properties ((atr-file string))
  (constructors-with-properties (pathname atr-file)))

(defmethod constructors-with-properties ((atr-file pathname))
  (loop
     with propertied-constructors = nil
     for line in (split #\Newline (constructors-and-properties atr-file))
     do
       (destructuring-bind (constructor . properties)
	   (split #\Space line)
	 (when properties
	   (push (cons constructor properties) propertied-constructors)))
     finally
     (return propertied-constructors)))

(defgeneric remove-property-from-constructor (constructor-identifier property atr-file))

(defmethod remove-property-from-constructor (constructor-identifier property (atr-file string))
  (remove-property-from-constructor constructor-identifier property (pathname atr-file)))

(defmethod remove-property-from-constructor (constructor-identifier property (atr-file pathname))
  (destructuring-bind (kind nr aid relnr)
      (split #\- constructor-identifier)
    (let ((kind-param (xuriella:make-parameter kind "target_kind"))
	  (nr-param (xuriella:make-parameter nr "target_nr"))
	  (aid-param (xuriella:make-parameter aid "target_aid"))
	  (relnr-param (xuriella:make-parameter relnr "target_relnr"))
	  (property-param (xuriella:make-parameter property "target_property")))
      (xuriella:apply-stylesheet (mizar-items-config 'strip-prop-stylesheet)
				 atr-file
				 :parameters (list kind-param
						   nr-param
						   aid-param
						   relnr-param
						   property-param)))))
