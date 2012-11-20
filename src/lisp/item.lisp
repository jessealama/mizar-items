;;; item.lisp Bits of mizar articles

(in-package :mizar)

(defclass item ()
  ((xml-node
    :accessor xml-node
    :initarg :xml-node
    :initform (error "To create an item, a snippet of XML is required.")))
  (:documentation "A piece of a Mizar article."))

(defun make-item-from-xml (xml)
  (let ((name (xpath "name (.)" xml)))
    (error "Don't know how to make an item from XML nodes named '~a'" name)))

(defclass quantified-formula (item)
  ((matrix
    :accessor matrix
    :type item)
   (variables
    :accessor variables
    :type list))
  (:documentation "A quantified formula."))

(defmethod initialize-instance :after ((formula quantified-formula) &rest initargs)
  (declare (ignore initargs))
  (let ((xml (xml-node formula)))
    (let ((segments-xml (xpath "*[position() < last()]" xml))
	  (formula-xml (first (xpath "*[position() = last()]" xml))))
      (loop
	 with variables-and-types = nil
	 for segment-xml in segments-xml
	 do
	   (cond ((xpath "self::Explicitly-Qualified-Segment" segment-xml)
		  (let ((type (first (xpath "*[position() = last()]" segment-xml)))
			(variables (xpath "Variables/*" segment-xml)))
		    (dolist (variable variables)
		      (setf variables-and-types
			    (append variables-and-types
				    (list (cons (make-item-from-xml variable) type)))))))
		 ((xpath "self::Implicitly-Qualified-Segment" segment-xml)
		  (let ((variables (xpath "*" segment-xml)))
		    (dolist (variable variables)
		      (setf variables-and-types
			    (append variables-and-types
				    (list (cons (make-item-from-xml variable) nil)))))))
		 (t
		  (error "Don't know how to deal with a segment that is neither an explicitly nor an implicitly qualified segment.")))
	 finally
	   (setf (matrix formula) (make-item-from-xml formula-xml))
	   (setf (variables formula) variables-and-types)
	   (return formula)))))

(defclass universal-quantifier-formula (quantified-formula)
  nil
  (:documentation "A universally quantified formula"))

(defclass existential-quantifier-formula (quantified-formula)
  nil
  (:documentation "A universally quantified formula"))

;;; item.lisp ends here
