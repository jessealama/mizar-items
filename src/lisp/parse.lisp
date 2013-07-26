;;; parse.lisp -- Parse Mizar articles

(in-package :mizar)

(defgeneric line (x)
  (:documentation "The line at which a parse tree node occurs"))

(defmethod previous-sibling ((x dom:element))
  (dom:previous-sibling x))

(defmethod previous-element-sibling ((x dom:element))
  (loop
     :with y = x
     :for sibling = (dom:previous-sibling y)
     :when (null sibling) :do (return nil)
     :when (typep sibling 'dom:element) :do (return sibling)
     :do (setf y sibling)))

(defmethod children ((x dom:element))
  (coerce (dom:child-nodes x) 'list))

(defmethod element-children ((x dom:element))
  (remove-if-not #'element-p (children x)))

(defun element-p (x)
  (typep x 'dom:element))

(defmethod last-element-child ((x dom:element))
  (let ((elements (remove-if-not #'element-p (reverse (children x)))))
    (when elements
      (first elements))))

(defmethod parent ((x dom:element))
  (dom:parent-node x))

(defmethod name ((x dom:element))
  (dom:node-name x))

(defmethod last-child ((x dom:element))
  (dom:last-child x))

(defmethod clone ((x dom:element))
  (dom:clone-node x t))

(defmethod kind ((x dom:element))
  (get-attribute "kind" x))

(defmethod has-attribute ((x dom:element) attribute)
  (dom:has-attribute x attribute))

(defclass parse-tree-node ()
  ((node
    :type (or null dom:element)
    :initform nil
    :accessor node)))

(defmethod owner-document ((x parse-tree-node))
  (dom:owner-document (node x)))

(defun make-element (element-name owner)
  (dom:create-element owner element-name))

(defun toplevel-ancestor (x)
  (xpath:first-node (xpath:evaluate "ancestor::*[parent::Text-Proper]" x)))

(defun scheme-justification-p (x)
  (and (typep x 'dom:element)
       (string= (dom:node-name x) "Scheme-Justification")))

(defun private-predicate-formula-p (x)
  (and (typep x 'dom:element)
       (string= (dom:node-name x) "Private-Predicate-Formula")))

(defun placeholder-term-p (x)
  (and (typep x 'dom:element)
       (string= (dom:node-name x) "Placeholder-Term")))

(defun make-exported-theorem-node (proposition justification)
  (let ((theorem (make-element "Item" (owner-document proposition))))
    (dom:set-attribute theorem "kind" "Theorem-Item")
    (dom:append-child theorem proposition)
    (dom:append-child theorem justification)
    theorem))

(defun make-unexported-theorem-node (proposition justification)
  (let ((theorem (make-element "Item" (owner-document proposition))))
    (dom:set-attribute theorem "kind" "Regular-Statement")
    (dom:set-attribute theorem "shape" "Compact-Statement")
    (dom:append-child theorem proposition)
    (dom:append-child theorem justification)
    theorem))

(defun make-proposition-node (content &optional label)
  (let ((proposition (make-element "Proposition" (owner-document content))))
    (when label
      (let ((label-node (make-element "Label" (owner-document content))))
        (dom:set-attribute label-node "spelling" label)
        (dom:append-child proposition label-node)))
    (dom:append-child proposition content)
    proposition))

(defun make-pragma (spelling owner-document)
  (let ((pragma (make-element "Item" owner-document)))
    (dom:set-attribute pragma "kind" "Pragma")
    (dom:set-attribute pragma "spelling" spelling)
    pragma))

(defun make-local-reference-node (spelling owner-document)
  (let ((ref (make-element "Local-Reference" owner-document)))
    (dom:set-attribute ref "spelling" spelling)
    ref))

(defun make-straightforward-justification-node (owner-document &rest references)
  (let ((by (dom:create-element owner-document "Straightforward-Justification")))
    (dolist (ref references by)
      (dom:append-child by ref))))

(defgeneric parse (x))

(defmethod parse :before ((x pathname))
  (makeenv x)
  (analyzer x))

(defmethod parse ((x pathname))
  (make-parse-tree-node (parse-xml-file (file-with-extension x "msx"))))

(defmethod line ((x parse-tree-node))
  (let ((n (node x)))
    (when (has-attribute n "line")
      (parse-integer (get-attribute "line" n)))))

(defmethod col ((x parse-tree-node))
  (let ((n (node x)))
    (when (has-attribute n "col")
      (parse-integer (get-attribute "col" n)))))

(defmethod spelling ((x parse-tree-node))
  (get-attribute "spelling" (node x)))

(defmethod idnr ((x parse-tree-node))
  (let ((n (node x)))
    (when (has-attribute n "idnr")
      (parse-integer (get-attribute "idnr" n)))))

(defmethod nr ((x parse-tree-node))
  (let ((n (node x)))
    (when (has-attribute n "nr")
      (parse-integer (get-attribute "nr" n)))))

(defclass justification (parse-tree-node)
  nil)

(defclass reservation (parse-tree-node)
  ((variables
    :type list
    :initform nil
    :initarg :variables
    :reader variables)
   (reservation-type
    :type mizar-type
    :initarg :type
    :initform (error "To make a reservation, a type is required.")
    :reader reservation-type)))

(defclass theorem (parse-tree-node)
  ((proposition
    :type proposition
    :initarg :proposition
    :initform (error "To make a theorem, a proposition is required.")
    :reader proposition)
   (justification
    :type justification
    :initarg :justification
    :initform (error "To make a theorem, a justification is required.")
    :reader justification)))

(defclass straightforward-justification (justification)
  ((references
    :type list
    :initform nil
    :initarg :references
    :reader references)))

(defclass theorem-reference (parse-tree-node)
  nil)

(defclass local-reference (parse-tree-node)
  nil)

(defclass definition-reference (parse-tree-node)
  nil)

(defclass scheme-justification (justification)
  nil)

(defclass block-item (parse-tree-node)
  ((items
    :type list
    :initform nil
    :initarg :items
    :reader items)))

(defclass proof-block (justification block-item)
  nil)

(defclass now-reasoning (block-item)
  nil)

(defclass hereby-reasoning (block-item)
  nil)

(defclass scheme-block (block-item)
  nil)

(defclass notation-block (block-item)
  nil)

(defclass scheme (parse-tree-node)
  ((name
    :type string
    :reader name
    :initarg :name
    :initform (error "To make a scheme, a name is required."))
   (variables
    :type list
    :initform nil
    :initarg :variables
    :reader variables)
   (conclusion
    :type formula
    :initform (error "To make a scheme, a conclusion is required.")
    :initarg :conclusion
    :reader conclusion)
   (provisions
    :type list
    :initform nil
    :initarg :provisions
    :reader provisions)
   (justification
    :type list
    :reader justification
    :initarg :justification
    :initform nil)))

(defmethod justification-of ((x justification))
  (previous-element-sibling x))

(defclass pred-antonym (parse-tree-node)
  ((old-pattern
    :type predicate-pattern
    :initarg :old-pattern
    :initform (error "To create a predicate antonym, a pattern is required.")
    :reader old-pattern)
   (new-pattern
    :type predicate-pattern
    :initarg :new-pattern
    :initform (error "To create a predicate antonym, a pattern is required.")
    :reader new-pattern)))

(defclass choice-statement (parse-tree-node)
  ((conditions
    :type list
    :initform (error "To make a choice statement, a non-empty list of conditions is required.")
    :initarg :conditions
    :reader conditions)
   (variables
    :type list
    :initarg :variables
    :initform (error "To make a choice statement, a non-empty list of variables is required."))))

(defclass text-proper (block-item)
  nil)

(defclass definitional-block (block-item)
  nil)

(defclass registration-block (block-item)
  nil)

(defclass section-pragma (parse-tree-node)
  nil)

(defclass pragma (parse-tree-node)
  nil)

(defclass mizar-type (parse-tree-node)
  nil)

(defclass standard-type (mizar-type)
  nil)

(defclass clustered-type (mizar-type)
  ((cluster
    :type adjective-cluster
    :initarg :cluster
    :initform (error "To create a clustered type, an adjective cluster is required.")
    :reader cluster)
   (radix-type
    :type mizar-type
    :initarg :radix-type
    :initform (error "To create a clustered type, a radix type is required.")
    :reader radix-type)))

(defclass functor-segment (parse-tree-node)
  ((variables
    :type list
    :initarg :variables
    :initform nil
    :reader variables)
   (segment-type
    :type mizar-type
    :initarg :segment-type
    :initform (error "To make a functor segment, a type is required.")
    :reader segment-type)))

(defclass predicate-segment (parse-tree-node)
  ((variables
    :type list
    :initarg :variables
    :initform nil
    :reader variables)))

(defclass explicitly-qualified-segment (parse-tree-node)
  ((variables
    :type list
    :initarg :variables
    :initform nil
    :reader variables)
   (segment-type
    :type mizar-type
    :initarg :segment-type
    :initform (error "To make an explicitly qualified segment, a type is required.")
    :reader segment-type)))

(defclass implicitly-qualified-segment (parse-tree-node)
  ((variables
    :type list
    :initarg :variables
    :initform nil
    :reader variables)))

(defclass mizar-term (parse-tree-node)
  nil)

(defclass mizar-definition (parse-tree-node)
  nil)

(defclass attribute-definition (mizar-definition)
  ((pattern
    :type (or null attribute-pattern)
    :initarg :pattern
    :initform nil
    :reader pattern)
   (definiens
       :type (or null parse-tree-node)
     :initform nil
     :initarg :definiens
     :reader definiens)))

(defclass functor-definition (mizar-definition)
  ((pattern
    :type (or null functor-pattern)
    :initarg :pattern
    :initform nil
    :reader pattern)
   (result-type
    :type (or null mizar-type)
    :initarg :result-type
    :initform nil
    :reader result-type)
   (definiens
       :type (or null parse-tree-node)
     :initform nil
     :initarg :definiens
     :reader definiens)))

(defclass predicate-definition (mizar-definition)
  ((pattern
    :type (or null predicate-pattern)
    :initarg :pattern
    :initform nil
    :reader pattern)
   (definiens
       :type (or null parse-tree-node)
     :initform nil
     :initarg :definiens
     :reader definiens)))

(defclass functor-pattern (parse-tree-node)
  nil)

(defclass operation-functor-pattern (functor-pattern)
  ((left-arguments
    :type list
    :initform nil
    :initarg :left
    :reader left-arguments)
   (right-arguments
    :type list
    :initform nil
    :initarg :right
    :reader right-arguments)))

(defclass mizar-pattern (parse-tree-node)
  nil)

(defclass attribute-pattern (mizar-pattern)
  ((loci
    :type list
    :initform nil
    :initarg :loci
    :reader loci)))

(defclass predicate-pattern (mizar-pattern)
  ((left-arguments
    :type list
    :initform nil
    :initarg :left
    :reader left-arguments)
   (right-arguments
    :type list
    :initform nil
    :initarg :right
    :reader right-arguments)))

(defclass mizar-variable (mizar-term)
  nil)

(defclass simple-term (mizar-term)
  nil)

(defclass infix-term (mizar-term)
  ((left-arguments
    :type list
    :initform nil
    :initarg :left
    :reader left-arguments)
   (right-arguments
    :type list
    :initform nil
    :initarg :right
    :reader right-arguments)))

(defclass right-circumflex-symbol (parse-tree-node)
  nil)

(defclass circumfix-term (mizar-term)
  ((between
    :type list
    :initform nil
    :initarg :between
    :reader between)
   (right
    :type right-circumflex-symbol
    :initform (error "To make a circumfix term, a right circumflex symbol is required.")
    :initarg :right
    :reader right)))

(defclass it-term (mizar-term)
  nil)

(defclass global-choice-term (mizar-term)
  ((argument
    :type mizar-type
    :initarg :argument
    :initform (error "To make a global choice term, a type is required.")
    :reader argument)))

(defclass locus (mizar-term)
  nil)

(defclass placeholder-term (mizar-term)
  nil)

(defclass private-functor-term (mizar-term)
  ((arguments
    :type list
    :initform nil
    :initarg :arguments
    :reader arguments)))

(defclass private-predicate-formula (formula)
  ((arguments
    :type list
    :initform nil
    :initarg :arguments
    :reader arguments)))

(defclass private-predicate-definition (parse-tree-node)
  ((var
    :type mizar-variable
    :initarg :variable
    :initform (error "To make a private predicate definition, a variable is required.")
    :reader definiendum)
   (arg-types
    :type list
    :initform nil
    :initarg :arg-types
    :reader arg-types)
   (definiens
       :type formula
     :initarg :definiens
     :initform (error "To make a private predicate definition, a definiens is required.")
     :reader definiens)))

(defclass formula (parse-tree-node)
  nil)

(defclass quantifier-formula (formula)
  ((segments
    :type list
    :initarg :segments
    :initform (error "To create a quantifier formula, a list of variable segments is required.")
    :reader segments)
   (matrix
    :type formula
    :initarg :matrix
    :initform (error "To create a quantifier formula, a matrix is required.")
    :reader matrix)))

(defclass existential-quantifier-formula (quantifier-formula)
  nil)

(defclass universal-quantifier-formula (quantifier-formula)
  nil)

(defclass atomic-formula (formula)
  nil)

(defclass predicative-formula (atomic-formula)
  ((left-arguments
    :type list
    :initform nil
    :initarg :left
    :reader left-arguments)
   (right-arguments
    :type list
    :initform nil
    :initarg :right
    :reader right-arguments)))

(defclass attributive-formula (atomic-formula)
  ((term
    :type mizar-term
    :initarg :term
    :initform (error "To make an attributive formula, a term is required.")
    :reader term)
   (adjectives
    :type adjective-cluster
    :initform (error "To make an attributive formula, an adjective cluster is required.")
    :initarg :adjectives
    :reader adjectives)))

(defclass contradiction (atomic-formula)
  nil)

(defclass thesis (atomic-formula)
  nil)

(defclass negated-formula (formula)
  ((argument
    :type formula
    :initform (error "To create a negated formula, an argument is required.")
    :initarg :argument
    :reader argument)))

(defclass biconditional-formula (formula)
  ((lhs
    :type formula
    :initform (error "To create a biconditional formula, a left-hand side is required.")
    :initarg :lhs
    :reader lhs)
   (rhs
    :type formula
    :initform (error "To create a biconditional formula, a right-hand side is required.")
    :initarg :rhs
    :reader rhs)))

(defclass conditional-formula (formula)
  ((lhs
    :type formula
    :initform (error "To create a conditional formula, a left-hand side is required.")
    :initarg :lhs
    :reader lhs)
   (rhs
    :type formula
    :initform (error "To create a conditional formula, a right-hand side is required.")
    :initarg :rhs
    :reader rhs)))

(defclass conjunctive-formula (formula)
  ((lhs
    :type formula
    :initform (error "To create a conjunctive formula, a left-hand side is required.")
    :initarg :lhs
    :reader lhs)
   (rhs
    :type formula
    :initform (error "To create a conjunctive formula, a right-hand side is required.")
    :initarg :rhs
    :reader rhs)))

(defclass disjunctive-formula (formula)
  ((lhs
    :type formula
    :initform (error "To create a disjunctive formula, a left-hand side is required.")
    :initarg :lhs
    :reader lhs)
   (rhs
    :type formula
    :initform (error "To create a disjunctive formula, a right-hand side is required.")
    :initarg :rhs
    :reader rhs)))

(defclass regular-statement (formula)
  nil)

(defclass compact-statement (regular-statement)
  ((content
    :type proposition
    :initarg :proposition
    :initform (error "To make a compact statement, a proposition is required.")
    :reader content)))

(defclass diffuse-statement (regular-statement)
  ((label
    :type (or null string)
    :initarg :label
    :initform nil
    :reader label)
   (content
    :type parse-tree-node
    :initarg :content
    :initform (error "To make a diffuse statement, content is required.")
    :reader content)))

(defclass proposition (parse-tree-node)
  ((label
    :type (or null string)
    :initarg :label
    :initform nil
    :reader label)
   (content
    :type formula
    :initarg :content
    :initform (error "To make a proposition, content is required.")
    :reader content)))

(defclass definiens (parse-tree-node)
  ((label
    :type (or null string)
    :initarg :label
    :initform nil
    :reader label)
   (content
    :type (or formula mizar-term)
    :initarg :content
    :initform (error "To make a definiens, content is required.")
    :reader content)))

(defclass cluster (parse-tree-node)
  ((registration
    :type registration
    :initform (error "To make a cluster, a registration is required.")
    :initarg :registration
    :reader registration)))

(defclass registration (parse-tree-node)
  nil)

(defclass existential-registration (registration)
  ((adjective-cluster
    :type adjective-cluster
    :initarg :adjective-cluster
    :initform (error "To make an existential registration, an adjective cluster is required.")
    :reader adjective-cluster)
   (radix-type
    :type mizar-type
    :initarg :radix-type
    :initform (error "To make an existential registration, a radix type is required.")
    :reader radix-type)))

(defclass functorial-registration (registration)
  ((term
    :type mizar-term
    :initarg :term
    :initform (error "To make a functorial registration, a term is required.")
    :reader term)
   (adjectives
    :type adjective-cluster
    :initarg :adjectives
    :initform (error "To make a functorial registration, an adjective cluster is required.")
    :reader adjectives)))

(defclass adjective-cluster (parse-tree-node)
  ((adjectives
    :type list
    :initform nil
    :initarg :adjectives
    :reader adjectives)))

(defclass adjective (parse-tree-node)
  ((negated
    :type boolean
    :initform nil
    :initarg :negated
    :accessor negated-p)))

(defclass conclusion (parse-tree-node)
  nil)

(defclass compact-conclusion (conclusion)
  ((content
    :type proposition
    :initarg :proposition
    :initform (error "To make a compact conclusion, a proposition is required.")
    :reader content)))

(defclass diffuse-conclusion (conclusion)
  ((label
    :type (or null string)
    :initarg :label
    :initform nil
    :reader label)
   (content
    :type parse-tree-node
    :initarg :content
    :initform (error "To make a diffuse conclusion, content is required.")
    :reader content)))

(defclass exemplification (parse-tree-node)
  ((example
    :type mizar-term
    :initarg :example
    :initform (error "To make an exemplification, an example is required.")
    :reader example)))

(defclass generalization (parse-tree-node)
  ((segments
   :type list
   :initarg :segments
   :initform (error "To create a generalization, a list of variable segments is required.")
   :reader segments)))

(defclass loci-declaration (parse-tree-node)
  ((segments
   :type list
   :initarg :segments
   :initform (error "To create a loci declaration, a list of variable segments is required.")
   :reader segments)))

(defclass mizar-property (parse-tree-node)
  ((justification
    :type justification
    :initarg :justification
    :initform (error "To make an existence condition, a justification is required.")
    :reader justification)))

(defclass functor-property (mizar-property)
  nil)

(defclass predicate-property (mizar-property)
  nil)

(defclass commutativity-property (functor-property)
  nil)

(defclass idempotence-property (functor-property)
  nil)

(defclass symmetry-property (predicate-property)
  nil)

(defclass asymmetry-property (predicate-property)
  nil)

(defclass reflexivity-property (predicate-property)
  nil)

(defclass irreflexivity-property (predicate-property)
  nil)

(defclass assumption (parse-tree-node)
  nil)

(defclass single-assumption (assumption)
  ((content
    :type proposition
    :initarg :proposition
    :initform (error "To make a single assumption, a proposition is required.")
    :reader content)))

(defclass collective-assumption (assumption)
  ((assumptions
    :type list
    :initarg :assumptions
    :initform nil
    :reader assumptions)))

(defclass existential-assumption (assumption)
  ((segments
   :type list
   :initarg :segments
   :initform (error "To create an existential assumption, a list of variable segments is required.")
   :reader segments)
   (conditions
    :type list
    :initarg :conditions
    :initform (error "To create an existential assumption, a list of conditions is rquired.")
    :reader conditions)))

(defclass correctness-condition (parse-tree-node)
  ((justification
    :type justification
    :initarg :justification
    :initform (error "To make an existence condition, a justification is required.")
    :reader justification)))

(defclass existence-condition (correctness-condition)
  nil)

(defclass uniqueness-condition (correctness-condition)
  nil)

(defclass coherence-condition (correctness-condition)
  nil)

(defclass compatibility-condition (correctness-condition)
  nil)

(defgeneric make-parse-tree-node (x))

(defmethod make-parse-tree-node :around ((x dom:element))
  (let ((result (call-next-method)))
    (unless (typep result 'parse-tree-node)
      (error "MAKE-PARSE-TREE-NODE on~%~%  ~a~%~%did not give a PARSE-TREE-NODE value." x))
    (setf (node result) x)
    result))

(defmethod make-parse-tree-node ((x dom:element))
  (let ((n (dom:node-name x))
        (k (dom:get-attribute x "kind")))
    (cond ((string= n "Item")
           (if (stringp k)
               (cond ((string= k "Choice-Statement")
                      (let ((conditions (xpath "Conditions/*" x))
                            (segments (xpath "Implicitly-Qualified-Segment | Implicitly-Qualified-Segment" x)))
                        (make-instance 'choice-statement
                                       :conditions (mapcar #'make-parse-tree-node conditions)
                                       :variables (mapcar #'make-parse-tree-node segments))))
                     ((string= k "Assumption")
                      (cond ((xpath "Single-Assumption" x)
                             (make-instance 'single-assumption
                                            :proposition (make-parse-tree-node (first (xpath "Single-Assumption/Proposition" x)))))
                            ((xpath "Collective-Assumption" x)
                             (make-instance 'collective-assumption
                                            :assumptions (mapcar #'make-parse-tree-node (xpath "Collective-Assumption/Conditions/*" x))))
                            (t
                             (error "Unable to make sense of an assumption item."))))
                     ((string= k "Regular-Statement")
                      (unless (dom:has-attribute x "shape")
                        (error "Regular-Statement item without a shape attribute!"))
                      (let ((shape (dom:get-attribute x "shape")))
                        (cond ((string= shape "Compact-Statement")
                               (make-instance 'compact-statement
                                              :proposition (make-parse-tree-node (first (xpath "Proposition" x)))))
                              ((string= shape "Diffuse-Statement")
                               (let ((label (xpath "Label" x))
                                     (content (first (xpath "*[position() = last()]" x))))
                                 (make-instance 'diffuse-statement
                                                :label (when label (dom:get-attribute (first label) "spelling"))
                                                :content (make-parse-tree-node content))))
                              (t
                               (error "Unable to make sense of regular statement items whose shape is '~a'." shape)))))
                     ((string= k "Existential-Assumption")
                      (let ((segments (xpath "Implicitly-Qualified-Segment | Explicitly-Qualified-Segment" x))
                            (conditions (xpath "Conditions/*" x)))
                        (make-instance 'existential-assumption
                                       :segments (mapcar #'make-parse-tree-node segments)
                                       :conditions (mapcar #'make-parse-tree-node conditions))))
                     ((string= k "Definition-Item")
                      (let ((definitional-block-node (xpath "Block[@kind = \"Definitional-Block\"]" x))
                            (registration-block-node (xpath "Block[@kind = \"Registration-Block\"]" x))
                            (notation-block-node (xpath "Block[@kind = \"Notation-Block\"]" x)))
                        (cond (definitional-block-node
                               (make-parse-tree-node (first definitional-block-node)))
                              (registration-block-node
                               (make-parse-tree-node (first registration-block-node)))
                              (notation-block-node
                               (make-parse-tree-node (first notation-block-node)))
                              (t
                               (error "Don't know how to handle a definition item.")))))
                     ((string= k "Attribute-Definition")
                      (let ((pattern (xpath "Attribute-Pattern" x))
                            (definiens (xpath "Definiens" x)))
                        (make-instance 'attribute-definition
                                       :pattern (when pattern (make-parse-tree-node (first pattern)))
                                       :definiens (when definiens (make-parse-tree-node (first definiens))))))
                     ((string= k "Pred-Antonym")
                      (let ((new-pattern (first (xpath "Predicate-Pattern[1]" x)))
                            (old-pattern (first (xpath "Predicate-Pattern[2]" x))))
                        (make-instance 'pred-antonym
                                       :old-pattern (make-parse-tree-node old-pattern)
                                       :new-pattern (make-parse-tree-node new-pattern))))
                     ((string= k "Functor-Definition")
                      (let ((pattern (xpath "Operation-Functor-Pattern" x))
                            (definiens (xpath "Definiens" x))
                            (result-type (xpath "Type-Specification/*" x)))
                        (make-instance 'functor-definition
                                       :pattern (when pattern (make-parse-tree-node (first pattern)))
                                       :result-type (when result-type (make-parse-tree-node (first result-type)))
                                       :definiens (when definiens (make-parse-tree-node (first definiens))))))
                     ((string= k "Predicate-Definition")
                      (let ((pattern (xpath "Predicate-Pattern" x))
                            (definiens (xpath "Definiens" x)))
                        (make-instance 'predicate-definition
                                       :pattern (when pattern (make-parse-tree-node (first pattern)))
                                       :definiens (when definiens (make-parse-tree-node (first definiens))))))
                     ((string= k "Conclusion")
                      (unless (dom:has-attribute x "shape")
                        (error "Conclusion item without a shape attribute!"))
                      (let ((shape (dom:get-attribute x "shape")))
                        (cond ((string= shape "Compact-Statement")
                               (make-instance 'compact-conclusion
                                              :proposition (make-parse-tree-node (first (xpath "Proposition" x)))))
                              ((string= shape "Diffuse-Statement")
                               (let ((label (xpath "Label" x))
                                     (content (first (xpath "*[position() = last()]" x))))
                                 (make-instance 'diffuse-conclusion
                                                :label (when label (dom:get-attribute (first label) "spelling"))
                                                :content (make-parse-tree-node content))))
                              (t
                               (error "Unable to make sense of conclusion items whose shape is '~a'." shape)))))
                     ((string= k "Cluster")
                      (make-parse-tree-node (first (xpath "*" x))))
                     ((string= k "Private-Predicate-Definition")
                      (let ((var (first (xpath "Variable" x)))
                            (arg-types (xpath "Type-List/*" x))
                            (definiens (first (xpath "*[position() = last()]" x))))
                        (make-instance 'private-predicate-definition
                                       :variable (make-parse-tree-node var)
                                       :arg-types (mapcar #'make-parse-tree-node arg-types)
                                       :definiens (make-parse-tree-node definiens))))
                     ((string= k "Correctness-Condition")
                      (let ((c (dom:get-attribute x "condition")))
                        (when (null c)
                          (error "Correctness condition without a condition attribute."))
                        (cond ((string= c "existence")
                               (make-instance 'existence-condition
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              ((string= c "uniqueness")
                               (make-instance 'uniqueness-condition
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              ((string= c "coherence")
                               (make-instance 'coherence-condition
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              ((string= c "compatibility")
                               (make-instance 'compatibility-condition
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              (t
                               (error "Don't know how to make a correctness condition item for '~a'." c)))))
                     ((string= k "Correctness")
                      (let ((conditions (xpath "Conditions" x)))
                        (unless conditions
                          (error "Conditions child missing under a Correctness condition."))
                        (let ((correctness (xpath "Correctness" (first conditions))))
                          (unless correctness
                            (error "Correctness child missing under a Correcntess node."))
                          (let ((condition (dom:get-attribute (first correctness) "condition")))
                            (unless condition
                              (error "Correctness node lacks a condition attribute."))
                            (let ((justification (xpath "following-sibling::*[1]" (first conditions))))
                              (cond ((string= condition "coherence")
                                     (make-instance 'coherence-condition
                                                    :justification (make-parse-tree-node (first justification))))
                                    (t
                                     (error "Unknown correctness condition '~a'." condition))))))))
                     ((string= k "Exemplification")
                      (let ((example (first (xpath "*" x))))
                        (make-instance 'exemplification
                                       :example (make-parse-tree-node example))))
                     ((string= k "Generalization")
                      (let ((segments (xpath "Implicitly-Qualified-Segment | Explicitly-Qualified-Segment" x)))
                        (make-instance 'generalization
                                       :segments (mapcar #'make-parse-tree-node segments))))
                     ((string= k "Loci-Declaration")
                      (let ((segments (xpath "Implicitly-Qualified-Segment | Explicitly-Qualified-Segment" x)))
                        (make-instance 'generalization
                                       :segments (mapcar #'make-parse-tree-node segments))))
                     ((string= k "Section-Pragma")
                      (make-instance 'section-pragma))
                     ((string= k "Theorem-Item")
                      (let ((prop (first (xpath "Proposition" x)))
                            (just (first (xpath "*[position() = last()]" x))))
                        (make-instance 'theorem
                                       :proposition (make-parse-tree-node prop)
                                       :justification (make-parse-tree-node just))))
                     ((string= k "Reservation")
                      (let ((variables (xpath "Variables/Variable" x))
                            (type (first (xpath "*[position() = last()]" x))))
                        (make-instance 'reservation
                                       :type (make-parse-tree-node type)
                                       :variables (mapcar #'make-parse-tree-node variables))))
                     ((string= k "Pragma")
                      (make-instance 'pragma))
                     ((string= k "Property")
                      (let ((p (dom:get-attribute x "property")))
                        (when (null p)
                          (error "Unable to make sense of a property item that lacks a prperty attribute."))
                        (cond ((string= p "commutativity")
                               (make-instance 'commutativity-property
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              ((string= p "idempotence")
                               (make-instance 'idempotence-property
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              ((string= p "symmetry")
                               (make-instance 'symmetry-property
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              ((string= p "asymmetry")
                               (make-instance 'asymmetry-property
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              ((string= p "reflexivity")
                               (make-instance 'reflexivity-property
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              ((string= p "irreflexivity")
                               (make-instance 'irreflexivity-property
                                              :justification (make-parse-tree-node (first (xpath "*" x)))))
                              (t
                               (error "Don't know how to make sense of property '~a'." p)))))
                     ((string= k "Scheme-Block-Item")
                      (make-parse-tree-node (first (element-children x))))
                     (t
                      (error "Don't know how to handle an Item node of kind '~a'." k)))
               (error "Don't know how to handle an Item node that lacks a kind attribute.")))
          ((string= n "Block")
           (if (stringp k)
               (cond ((string= k "Scheme-Block")
                      (let ((head-node (first (xpath "Item[@kind = \"Scheme-Head\"]" x))))
                        (let ((variable-nodes (xpath "Schematic-Variables/*" head-node))
                              (formula-node
                               (if (xpath "Provisional-Formulas" head-node)
                                   (first (xpath "preceding-sibling::*[1]" (first (xpath "Provisional-Formulas" head-node))))
                                   (first (xpath "*[position() = last()]" head-node))))
                              (justification-nodes (xpath "following-sibling::*" head-node))
                              (scheme-node (first (xpath "Scheme" head-node)))
                              (provisions (xpath "Provisional-Formulas/*" head-node)))
                          (make-instance 'scheme
                                         :name (spelling scheme-node)
                                         :variables (mapcar #'make-parse-tree-node variable-nodes)
                                         :conclusion (make-parse-tree-node formula-node)
                                         :justification (mapcar #'make-parse-tree-node justification-nodes)
                                         :provisions (mapcar #'make-parse-tree-node provisions)))))
                     ((string= k "Definitional-Block")
                      (make-instance 'definitional-block
                                     :items (mapcar #'make-parse-tree-node (xpath "*" x))))
                     ((string= k "Registration-Block")
                      (make-instance 'registration-block
                                     :items (mapcar #'make-parse-tree-node (xpath "*" x))))
                     ((string= k "Proof")
                      (make-instance 'proof-block
                                     :items (mapcar #'make-parse-tree-node (xpath "*" x))))
                     ((string= k "Now-Reasoning")
                      (make-instance 'now-reasoning
                                     :items (mapcar #'make-parse-tree-node (xpath "*" x))))
                     ((string= k "Hereby-Reasoning")
                      (make-instance 'hereby-reasoning
                                     :items (mapcar #'make-parse-tree-node (xpath "*" x))))
                     ((string= k "Notation-Block")
                      (make-instance 'notation-block
                                     :items (mapcar #'make-parse-tree-node (xpath "*" x))))
                     (t
                      (error "Don't know how to handle a block of kind '~a'." k)))
               (error "Don't know how to handle a block that lacks a kind attribute.")))
          ((string= n "Global-Choice-Term")
           (make-instance 'global-choice-term
                          :argument (make-parse-tree-node (first (xpath "*" x)))))
          ((string= n "Theorem-Reference")
           (make-instance 'theorem-reference))
          ((string= n "Definition-Reference")
           (make-instance 'definition-reference))
          ((string= n "Local-Reference")
           (make-instance 'local-reference))
          ((string= n "Right-Circumflex-Symbol")
           (make-instance 'right-circumflex-symbol))
          ((string= n "it-Term")
           (make-instance 'it-term))
          ((string= n "Straightforward-Justification")
           (make-instance 'straightforward-justification
                          :references (mapcar #'make-parse-tree-node (xpath "*" x))))
          ((string= n "Clustered-Type")
           (let ((cluster (first (xpath "Adjective-Cluster" x)))
                 (radix (first (xpath "*[position() = last()]" x))))
             (make-instance 'clustered-type
                            :radix-type (make-parse-tree-node radix)
                            :cluster (make-parse-tree-node cluster) )))
          ((string= n "Operation-Functor-Pattern")
           (let ((left (xpath "Loci[1]/*" x))
                 (right (xpath "Loci[2]/*" x)))
             (make-instance 'operation-functor-pattern
                            :left (mapcar #'make-parse-tree-node left)
                            :right (mapcar #'make-parse-tree-node right))))
          ((string= n "Predicate-Pattern")
           (let ((left (xpath "Loci[1]/*" x))
                 (right (xpath "Loci[2]/*" x)))
             (make-instance 'predicate-pattern
                            :left (mapcar #'make-parse-tree-node left)
                            :right (mapcar #'make-parse-tree-node right))))
          ((string= n "Adjective-Cluster")
           (make-instance 'adjective-cluster
                          :adjectives (mapcar #'make-parse-tree-node (xpath "*" x))))
          ((string= n "Adjective")
           (make-instance 'adjective))
          ((string= n "NegatedAdjective")
           (let ((adj-node (first (xpath "*[1]" x))))
             (let ((adj (make-parse-tree-node adj-node)))
               (setf (negated-p adj) t)
               adj)))
          ((string= n "Existential-Registration")
           (let ((cluster (first (xpath "Adjective-Cluster" x)))
                 (mode (first (xpath "*[position() = last()]" x))))
             (make-instance 'existential-registration
                            :radix-type (make-parse-tree-node mode)
                            :adjective-cluster (make-parse-tree-node cluster))))
          ((string= n "Functorial-Registration")
           (let ((term (first (xpath "*[1]" x)))
                 (adjectives (first (xpath "*[2]" x))))
             (make-instance 'functorial-registration
                            :term (make-parse-tree-node term)
                            :adjectives (make-parse-tree-node adjectives))))
          ((string= n "Attribute-Pattern")
           (make-instance 'attribute-pattern
                          :loci (mapcar #'make-parse-tree-node (xpath "Locus" x))))
          ((string= n "Proposition")
           (let ((label (xpath "Label" x))
                 (content-node (first (xpath "*[position() = last()]" x))))
             (make-instance 'proposition
                            :content (make-parse-tree-node content-node)
                            :label (when label (dom:get-attribute (first label) "spelling")))))
          ((string= n "Definiens")
           (let ((label (xpath "Label" x))
                 (content-node (first (xpath "*[position() = last()]" x))))
             (make-instance 'definiens
                            :content (make-parse-tree-node content-node)
                            :label (when label (dom:get-attribute (first label) "spelling")))))
          ((string= n "Variable")
           (make-instance 'mizar-variable))
          ((string= n "Locus")
           (make-instance 'locus))
          ((string= n "Simple-Term")
           (make-instance 'mizar-variable))
          ((string= n "Placeholder-Term")
           (make-instance 'placeholder-term))
          ((string= n "Private-Functor-Term")
           (let ((args (xpath "*" x)))
             (make-instance 'private-functor-term
                            :arguments (mapcar #'make-parse-tree-node args))))
          ((string= n "Private-Predicate-Formula")
           (let ((args (xpath "*" x)))
             (make-instance 'private-predicate-formula
                            :arguments (mapcar #'make-parse-tree-node args))))
          ((string= n "Standard-Type")
           (make-instance 'standard-type))
          ((string= n "Functor-Segment")
           (let ((variable-nodes (xpath "Variables/*" x))
                 (segment-type-node (first (xpath "Type-Specification/*[1]" x))))
             (make-instance 'functor-segment
                            :variables (mapcar #'make-parse-tree-node variable-nodes)
                            :segment-type (make-parse-tree-node segment-type-node))))
          ((string= n "Predicate-Segment")
           (let ((variable-nodes (xpath "Variables/*" x)))
             (make-instance 'predicate-segment
                            :variables (mapcar #'make-parse-tree-node variable-nodes))))
          ((string= n "Negated-Formula")
           (let ((arg-node (first (xpath "*[1]" x))))
             (make-instance 'negated-formula
                            :argument (make-parse-tree-node arg-node))))
          ((string= n "Biconditional-Formula")
           (let ((lhs-node (first (xpath "*[1]" x)))
                 (rhs-node (first (xpath "*[2]" x))))
             (make-instance 'biconditional-formula
                            :lhs (make-parse-tree-node lhs-node)
                            :rhs (make-parse-tree-node rhs-node))))
          ((string= n "Conditional-Formula")
           (let ((lhs-node (first (xpath "*[1]" x)))
                 (rhs-node (first (xpath "*[2]" x))))
             (make-instance 'conditional-formula
                            :lhs (make-parse-tree-node lhs-node)
                            :rhs (make-parse-tree-node rhs-node))))
          ((string= n "Conjunctive-Formula")
           (let ((lhs-node (first (xpath "*[1]" x)))
                 (rhs-node (first (xpath "*[2]" x))))
             (make-instance 'conjunctive-formula
                            :lhs (make-parse-tree-node lhs-node)
                            :rhs (make-parse-tree-node rhs-node))))
          ((string= n "Disjunctive-Formula")
           (let ((lhs-node (first (xpath "*[1]" x)))
                 (rhs-node (first (xpath "*[2]" x))))
             (make-instance 'disjunctive-formula
                            :lhs (make-parse-tree-node lhs-node)
                            :rhs (make-parse-tree-node rhs-node))))
          ((string= n "Contradiction")
           (make-instance 'contradiction))
          ((string= n "Thesis")
           (make-instance 'thesis))
          ((string= n "Predicative-Formula")
           (let ((left-nodes (xpath "Arguments[1]/*" x))
                 (right-nodes (xpath "Arguments[2]/*" x)))
             (make-instance 'predicative-formula
                            :left (mapcar #'make-parse-tree-node left-nodes)
                            :right (mapcar #'make-parse-tree-node right-nodes))))
          ((string= n "Attributive-Formula")
           (let ((term (first (xpath "*[1]" x)))
                 (adjectives (first (xpath "*[2]" x))))
             (make-instance 'attributive-formula
                            :term (make-parse-tree-node term)
                            :adjectives (make-parse-tree-node adjectives))))
          ((string= n "Infix-Term")
           (let ((left-nodes (xpath "Arguments[1]/*" x))
                 (right-nodes (xpath "Arguments[2]/*" x)))
             (make-instance 'infix-term
                            :left (mapcar #'make-parse-tree-node left-nodes)
                            :right (mapcar #'make-parse-tree-node right-nodes))))
          ((string= n "Circumfix-Term")
           (let ((right (first (xpath "*[1]" x)))
                 (between (xpath "*[position() > 1]" x)))
             (make-instance 'circumfix-term
                            :right (make-parse-tree-node right)
                            :between (mapcar #'make-parse-tree-node between))))
          ((string= n "Existential-Quantifier-Formula")
           (let ((segment-nodes (xpath "Implicitly-Qualified-Segment | Explicitly-Qualified-Segment" x))
                 (formula-node (first (xpath "*[position() = last()]" x))))
             (make-instance 'existential-quantifier-formula
                            :segments (mapcar #'make-parse-tree-node segment-nodes)
                            :matrix (make-parse-tree-node formula-node))))
          ((string= n "Universal-Quantifier-Formula")
           (let ((segment-nodes (xpath "Implicitly-Qualified-Segment | Explicitly-Qualified-Segment" x))
                 (formula-node (first (xpath "*[position() = last()]" x))))
             (make-instance 'universal-quantifier-formula
                            :segments (mapcar #'make-parse-tree-node segment-nodes)
                            :matrix (make-parse-tree-node formula-node))))
          ((string= n "Explicitly-Qualified-Segment")
           (let ((variable-nodes (xpath "Variables/*" x))
                 (segment-type (first (xpath "*[position() = last()]" x))))
             (make-instance 'explicitly-qualified-segment
                            :variables (mapcar #'make-parse-tree-node variable-nodes)
                            :segment-type (make-parse-tree-node segment-type))))
          ((string= n "Implicitly-Qualified-Segment")
           (let ((variable-nodes (xpath "Variables/*" x)))
             (make-instance 'implicitly-qualified-segment
                            :variables (mapcar #'make-parse-tree-node variable-nodes))))
          ((string= n "Text-Proper")
           (make-instance 'text-proper
                          :items (mapcar #'make-parse-tree-node (element-children x))))
          (t
           (error "How to handle nodes like '~a'?" x)))))

;;; parse.lisp ends here
