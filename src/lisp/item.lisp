;;; item.lisp Bits of mizar articles

(in-package :mizar)

(defclass item ()
  ((xml-node
    :accessor xml-node
    :type dom:node
    :initarg :xml-node
    :initform (error "To create an item, a snippet of XML is required."))
   (children
    :accessor children
    :type list
    :initform nil
    :initarg :children))
  (:documentation "A piece of a Mizar article."))

(defun make-item-from-xml (xml)
  (let ((name (xpath:evaluate "name (.)" xml))
	(children (xpath "*" xml)))
    (make-instance (cond ((string= name "Item")
			  (let ((kind-attribute (xpath:evaluate "@kind" xml)))
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
					((string= kind "Private-Predicate-Definition")
					 'private-predicate-definition)
					((string= kind "Choice-Statement")
					 'choice-statement)
					((string= kind "Exemplification")
					 'exemplification)
					((string= kind "Generalization")
					 'generalization)
					((string= kind "Conclusion")
					 'conclusion)
					((string= kind "Loci-Declaration")
					 'loci-declaration)
					((string= kind "Attribute-Definition")
					 'attribute-definition)
					((string= kind "Cluster")
					 'cluster)
					((string= kind "Correctness-Condition")
					 'correctness-condition)
					((string= kind "Functor-Definition")
					 'functor-definition)
					((string= kind "Predicate-Definition")
					 'predicate-definition)
					((string= kind "Assumption")
					 'assumption)
					((string= kind "Property")
					 'property)
					((string= kind "Correctness")
					 'correctness)
					((string= kind "Pred-Antonym")
					 'predicate-antonym)
					((string= kind "Existential-Assumption")
					 'existential-assumption)
					(t
					 (error "Don't know how to deal with Item nodes whose kind attribute is~%~%  ~a~%" kind)))))))
			 ((string= name "Text-Proper")
			  'text-proper)
			 ((string= name "Block")
			  'mizar-block)
			 ((string= name "Type-List")
			  'type-list-item)
			 ((string= name "Type-Specification")
			  'type-specification)
			 ((string= name "Standard-Type")
			  'standard-type)
			 ((string= name "Scheme")
			  'scheme-item)
			 ((string= name "Schematic-Variables")
			  'schematic-variables-item)
			 ((string= name "Variable")
			  'variable-item)
			 ((string= name "Variables")
			  'variables-item)
			 ((string= name "Functor-Segment")
			  'functor-segment-item)
			 ((string= name "Predicate-Segment")
			  'predicate-segment-item)
			 ((string= name "Explicitly-Qualified-Segment")
			  'explicitly-qualified-segment)
			 ((string= name "Implicitly-Qualified-Segment")
			  'implicitly-qualified-segment)
			 ((string= name "Proposition")
			  'proposition)
			 ((string= name "Label")
			  'label-item)
			 ((string= name "Existential-Quantifier-Formula")
			  'existential-quantifier-formula)
			 ((string= name "Universal-Quantifier-Formula")
			  'universal-quantifier-formula)
			 ((string= name "Conditional-Formula")
			  'conditional-formula)
			 ((string= name "Biconditional-Formula")
			  'biconditional-formula)
			 ((string= name "Conjunctive-Formula")
			  'conjunctive-formula)
			 ((string= name "Disjunctive-Formula")
			  'disjunctive-formula)
			 ((string= name "Negated-Formula")
			  'negated-formula)
			 ((string= name "Predicative-Formula")
			  'predicative-formula)
			 ((string= name "Attributive-Formula")
			  'attributive-formula)
			 ((string= name "Private-Predicate-Formula")
			  'private-predicate-formula)
			 ((string= name "Arguments")
			  'arguments-item)
			 ((string= name "Simple-Term")
			  'simple-term)
			 ((string= name "Placeholder-Term")
			  'placeholder-term)
			 ((string= name "Private-Functor-Term")
			  'private-functor-term)
			 ((string= name "Straightforward-Justification")
			  'straightforward-justification)
			 ((string= name "Scheme-Justification")
			  'scheme-justification)
			 ((string= name "Conditions")
			  'conditions-item)
			 ((string= name "Local-Reference")
			  'local-reference)
			 ((string= name "Definition-Reference")
			  'definition-reference)
			 ((string= name "Thesis")
			  'thesis)
			 ((string= name "Attribute-Pattern")
			  'attribute-pattern)
			 ((string= name "Predicate-Pattern")
			  'predicate-pattern)
			 ((string= name "Operation-Functor-Pattern")
			  'operation-functor-pattern)
			 ((string= name "Locus")
			  'locus)
			 ((string= name "Loci")
			  'loci-item)
			 ((string= name "Definiens")
			  'definiens)
			 ((string= name "Existential-Registration")
			  'existential-registration)
			 ((string= name "Functorial-Registration")
			  'functorial-registration)
			 ((string= name "Adjective-Cluster")
			  'adjective-cluster)
			 ((string= name "Adjective")
			  'adjective)
			 ((string= name "Contradiction")
			  'contradiction)
			 ((string= name "Global-Choice-Term")
			  'global-choice-term)
			 ((string= name "it-Term")
			  'it-term)
			 ((string= name "Infix-Term")
			  'infix-term)
			 ((string= name "Circumfix-Term")
			  'circumfix-term)
			 ((string= name "Right-Circumflex-Symbol")
			  'right-circumflex-symbol)
			 ((string= name "Clustered-Type")
			  'clustered-type)
			 ((string= name "Single-Assumption")
			  'single-assumption)
			 ((string= name "Collective-Assumption")
			  'collective-assumption)
			 ((string= name "Redefine")
			  'redefine)
			 ((string= name "Theorem-Reference")
			  'theorem-reference)
			 ((string= name "Correctness")
			  'correctness)
			 ((string= name "NegatedAdjective")
			  'negated-adjective)
			 ((string= name "Provisional-Formulas")
			  'provisional-formulas)
			 (t
			  (error "Don't know how to make an item from XML nodes named '~a'" name)))
		   :xml-node xml
		   :children (mapcar #'make-item-from-xml children))))

(defclass redefine (item)
  nil)

(defclass correctness-condition (item)
  nil)

(defclass correctness (item)
  nil)

(defclass property (item)
  nil)

(defclass private-predicate-definition (item)
  nil)

(defclass mizar-term (item)
  nil)

(defclass global-choice-term (mizar-term)
  nil)

(defclass placeholder-term (mizar-term)
  nil)

(defclass simple-term (mizar-term)
  nil)

(defclass private-functor-term (mizar-term)
  nil)

(defclass it-term (mizar-term)
  nil)

(defclass infix-term (mizar-term)
  nil)

(defclass circumfix-term (mizar-term)
  nil)

(defclass right-circumflex-symbol (item)
  nil)

(defclass arguments-item (item)
  nil)

(defclass proposition (item)
  nil)

(defclass label-item (item)
  nil)

(defclass local-reference (item)
  nil)

(defclass theorem-reference (item)
  nil)

(defclass definition-reference (item)
  nil)

(defclass straightforward-justification (item)
  nil)

(defclass scheme-justification (item)
  nil)

(defclass formula-item (item)
  nil)

(defclass negated-formula (formula-item)
  nil)

(defclass predicative-formula (formula-item)
  nil)

(defclass attributive-formula (formula-item)
  nil)

(defclass assumption (item)
  nil)

(defclass existential-assumption (item)
  nil)

(defclass collective-assumption (item)
  nil)

(defclass single-assumption (item)
  nil)

(defclass contradiction (formula-item)
  nil)

(defclass private-predicate-formula (formula-item)
  nil)

(defclass existential-quantifier-formula (formula-item)
  nil)

(defclass choice-statement (item)
  nil)

(defclass exemplification (item)
  nil)

(defclass generalization (item)
  nil)

(defclass conclusion (item)
  nil)

(defclass loci-declaration (item)
  nil)

(defclass thesis (item)
  nil)

(defclass conditions-item (item)
  nil)



(defclass universal-quantifier-formula (formula-item)
  nil)

(defclass conditional-formula (formula-item)
  nil)

(defclass biconditional-formula (formula-item)
  nil)

(defclass conjunctive-formula (formula-item)
  nil)

(defclass disjunctive-formula (formula-item)
  nil)

(defclass text-proper (item)
  nil)

(defclass mizar-block (item)
  nil)

(defclass mizar-type (item)
  nil)

(defclass standard-type (mizar-type)
  nil)

(defclass clustered-type (mizar-type)
  nil)

(defclass type-list-item (item)
  nil)

(defclass type-specification (item)
  nil)

(defclass segment-item (item)
  nil)

(defclass functor-segment-item (segment-item)
  nil)

(defclass predicate-segment-item (segment-item)
  nil)

(defclass qualified-segment (segment-item)
  nil)

(defclass implicitly-qualified-segment (qualified-segment)
  nil)

(defclass explicitly-qualified-segment (qualified-segment)
  nil)

(defclass variable-item (item)
  nil)

(defclass variables-item (item)
  nil)

(defclass scheme-item (item)
  nil)

(defclass schematic-variables-item (item)
  nil)

(defclass regular-statement (item)
  nil)

(defclass theorem-item (item)
  nil)

(defclass definition-item (item)
  nil)

(defclass functor-definition (item)
  nil)

(defclass predicate-definition (item)
  nil)

(defclass cluster (item)
  nil)

(defclass adjective-cluster (item)
  nil)

(defclass adjective (item)
  nil)

(defclass existential-registration (item)
  nil)

(defclass functorial-registration (item)
  nil)

(defclass definiens (item)
  nil)

(defclass attribute-definition (item)
  nil)

(defclass attribute-pattern (item)
  nil)

(defclass predicate-pattern (item)
  nil)

(defclass negated-adjective (item)
  nil)

(defclass provisional-formulas (item)
  nil)

(defclass predicate-antonym (item)
  nil)

(defclass operation-functor-pattern (item)
  nil)

(defclass locus (item)
  nil)

(defclass loci-item (item)
  nil)

(defclass scheme-block-item (item)
  nil)

(defclass scheme-head (item)
  nil)

(defclass section-pragma (item)
  nil)

(defun reservation-item-p (x)
  (eql (type-of x) 'reservation-item))

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
