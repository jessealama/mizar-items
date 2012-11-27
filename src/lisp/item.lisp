;;; item.lisp Bits of mizar articles

(in-package :mizar)

(defclass item ()
  ((xml-node
    :accessor xml-node
    :initarg :xml-node
    :initform (error "To create an item, a snippet of XML is required."))
   (line
    :reader line
    :initarg :line
    :initform nil
    :type (or null integer))
   (column
    :reader column
    :initarg :column
    :initform nil
    :type (or null integer))
   (children
    :type list
    :initform nil
    :initarg :children
    :accessor children))
  (:documentation "A piece of a Mizar article."))

(defun make-item-from-xml (xml)
  (let ((name (xpath:evaluate "name (.)" xml))
	(children (xpath "*" xml)))
    (cond ((string= name "Item")
	   (make-instance (let ((kind-attribute (xpath:evaluate "@kind" xml)))
			    (if (xpath:node-set-empty-p kind-attribute)
				(error "Don't know how to deal with an Item element that lacks a kind attribute.")
				(let ((kind (xpath:string-value kind-attribute)))
				  (cond ((string= kind "Reservation")
					 'reservation-item)
					((string= kind "Section-Pragma")
					 'section-pragma)
					((string= kind "Scheme-Block-Item")
					 'scheme-block-item)
					((string= kind "Scheme-Head")
					 'scheme-head)
					((string= kind "Definition-Item")
					 'definition-item)
					((string= kind "Theorem-Item")
					 'theorem-item)
					((string= kind "Regular-Statement")
					 'regular-statement)
					(t
					 (error "Don't know how to deal with Item nodes whose kind attribute is~%~%  ~a~%" kind))))))
			  :xml-node xml
			  :children (mapcar #'make-item-from-xml children)))
	  ((string= name "Block")
	   (make-instance 'mizar-block
			  :xml-node xml
			  :children (mapcar #'make-item-from-xml children)))
	  ((string= name "Standard-Type")
	   (make-instance 'standard-type
			  :xml-node xml
			  :children (mapcar #'make-item-from-xml children)))
	  ((string= name "Variable")
	   (make-instance 'variable-item
			  :xml-node xml
			  :children (mapcar #'make-item-from-xml children)))
	  ((string= name "Variables")
	   (mapcar #'make-item-from-xml children))
	  (t
	   (error "Don't know how to make an item from XML nodes named '~a'" name)))))

(defclass mizar-block (item)
  nil)

(defclass mizar-type (item)
  nil)

(defclass standard-type (mizar-type)
  nil)

(defclass qualified-segment (item)
  ((variables
    :type list
    :accessor variables
    :initform (error "To create a qualified segment, please supply a non-null list of variables.")
    :initarg :variables)))

(defclass implicitly-qualified-segment (qualified-segment)
  nil)

(defclass explicitly-qualified-segment (qualified-segment)
  ((variable-type
    :type item
    :accessor variable-type
    :initarg :type
    :initform (error "To create an explicitly qualified segment, please supply a type for the qualified variables."))))

(defclass variable-item (item)
  nil)

(defclass regular-statement (item)
  nil)

(defclass theorem-item (item)
  nil)

(defclass definition-item (item)
  nil)

(defclass scheme-block-item (item)
  nil)

(defclass scheme-head (item)
  nil)

(defclass section-pragma (item)
  nil)

(defclass reservation-item (item)
  nil)

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
