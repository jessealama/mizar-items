;;; article.lisp A representation of mizar articles

(in-package :mizar)

(defclass article ()
  ((path
    :initarg :path
    :initform (error "Every article must have a path.")
    :accessor path
    :type pathname))
  (:documentation "A representation of a Mizar article."))

(defmethod initialize-instance :after ((article article)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((path (path article)))
    (if (file-exists-p path)
	(let ((extension (pathname-type path)))
	  (if extension
	      (unless (string= extension "miz")
		(error "The extension of a Mizar article should be '.miz'; the given path~%~%  ~a~%~%does not have this extension." (namestring path)))))
	(error "There is no file at '~a'." (namestring path))))
  article)

(defmethod print-object ((article article) stream)
  (print-unreadable-object (article stream :type t :identity nil)
    (format stream "~a" (namestring (path article)))))

(defgeneric file-with-extension (article extension))

(defmethod file-with-extension ((article-path pathname) extension)
  (let ((dirname (directory-namestring article-path))
	(name (pathname-name article-path)))
    (let ((file (format nil "~a.~a" name extension)))
      (merge-pathnames file dirname))))

(defmethod file-with-extension ((article article) extension)
  (let* ((path (path article))
	 (name (pathname-name path))
	 (dir (pathname (directory-namestring path))))
    (merge-pathnames (format nil "~a.~a" name extension) dir)))

(defgeneric err-file (article))

(defmethod err-file ((article article))
  (file-with-extension article "err"))

(defmethod err-file ((article-path pathname))
  (let ((directory (pathname-as-directory (pathname (directory-namestring article-path))))
	(name (pathname-name article-path)))
    (merge-pathnames (format nil "~a.err" name) directory)))

(defgeneric miz-file (article))

(defmethod miz-file ((article article))
  (file-with-extension article "miz"))

(defgeneric empty-err-file? (article))

(defmethod empty-err-file? ((article article))
  (empty-err-file? (path article)))

(defmethod empty-err-file? ((article-path pathname))
  (zerop
   (with-open-file (file (err-file article-path))
     (file-length file))))

(defun message-file ()
  (merge-pathnames "mizar.msg" (mizfiles)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Articles and local databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy-to-db ((thing article) (db local-db))
  (let* ((name (pathname-name (path thing)))
	 (new-path (merge-pathnames (format nil "~a.miz" name) (location db))))
    (copy-to-db (path thing) db)
    (make-instance 'article
		   :path new-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying stylesheets to articles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod apply-stylesheet (stylesheet (thing article) parameters output)
  (makeenv thing)
  (wsmparser thing)
  (let ((wsx-file (file-with-extension thing "wsx")))
    (apply-stylesheet stylesheet wsx-file parameters output)))

(defgeneric parse-tree (article)
  (:documentation "An XML document representing the parse tree of ARTICLE."))

(defmethod parse-tree ((article article))
  (parse-tree (path article)))

(defmethod parse-tree :before ((article pathname))
  (unless (file-exists-p (file-with-extension article "wsx"))
    (makeenv article)
    (wsmparser article)))

(defmethod parse-tree ((article pathname))
  (parse-xml-file (file-with-extension article "wsx")))

(defgeneric items (article)
  (:documentation "The toplevel items of ARTICLE"))

(defmethod items ((article article))
  (items (path article)))

(defmethod items :before ((article pathname))
  (unless (file-exists-p (file-with-extension article "wsx"))
    (makeenv article)
    (wsmparser article)))

(defparameter *miz2lisp-stylesheet* (path-for-stylesheet "miz2lisp"))

(defmethod items ((article pathname))
  (let ((wsx-doc (cxml:parse-file (file-with-extension article "wsx")
				  (cxml-dom:make-dom-builder))))
    (children (make-item-from-xml (dom:document-element wsx-doc)))))

(defun form->item (form)
  (multiple-value-bind (expanded expandable)
      (macroexpand-1 form)
    (cond (expandable expanded)
	  ((stringp form)
	   (cond ((string= form "thesis")
		  (make-instance 'thesis-item))
		 (t
		  form)))
	  (t
	   (error "The form~%~%  ~a~%~%did not match any known form-to-item rules, nor is is a string." form)))))

(defgeneric render (item stream)
  (:documentation "Render ITEM as a piece of Mizar text to STREAM."))

(defun write-article (text-proper path)
  (with-open-file (miz-stream path
			      :direction :output
			      :if-exists :supersede)
    (render text-proper miz-stream)))

(defmacro |text-proper| ((articleid) &body body)
  (make-instance 'text-proper-item
		 :articleid articleid
		 :toplevel-items (mapcar #'form->item body)))

(defmacro |section-pragma| ()
  (make-instance 'section-pragma-item))

(defmacro |reserve| (variables type)
  (make-instance 'reservation-item
		 :variables (mapcar #'form->item variables)
		 :type (form->item type)))

(defmacro |standard-type| (radix &rest arguments)
  (make-instance 'standard-type
		 :radix (form->item radix)
		 :arguments (mapcar #'form->item arguments)))

(defclass mizar-item ()
  nil)

(defclass mizar-term (mizar-item)
  nil)

(defclass global-choice-term (mizar-term)
  ((type
    :type mizar-type
    :accessor choice-term-type
    :initarg :type
    :initform (error "A global choice term requires a type."))))

(defclass it-term (mizar-term)
  nil)

(defmacro |it| ()
  (make-instance 'it-term))

(defmacro |the| (type)
  (make-instance 'global-choice-term
		 :type (form->item type)))

(defclass variable-item (mizar-term)
  ((spelling
    :accessor spelling
    :type string
    :initarg :spelling
    :initform (error "To make a variable, please provide a spelling."))))

(defclass schematic-variable (variable-item)
  ((arguments
    :type list
    :accessor arguments
    :initarg :arguments
    :initform nil)))

(defclass schematic-functor-variable (schematic-variable)
  ((value-type
    :type mizar-type
    :accessor value-type
    :initarg :value-type
    :initform (error "To define a schematic functor variable, please supply a value type."))))

(defclass schematic-predicate-variable (schematic-variable)
  nil)

(defclass private-functor-term (mizar-term)
  ((spelling
    :accessor spelling
    :type string
    :initarg :spelling
    :initform (error "To make a variable, please provide a spelling."))
   (arguments
   :type list
   :accessor arguments
   :initarg :arguments
   :initform nil)))

(defclass private-predicate-formula (atomic-formula)
  ((predicate
    :type string
    :accessor predicate
    :initarg :predicate
    :initform (error "Missing predicate from a predicative formula"))
   (arguments
    :type list
    :accessor arguments
    :initarg :arguments
    :initform nil)))

(defclass simple-variable (variable-item)
  ((type
    :accessor variable-type
    :type mizar-type
    :initarg :type
    :initform (error "To make a variable, a type is required."))))

(defclass text-proper-item (mizar-item)
  ((articleid
    :type string
    :initarg :articleid
    :accessor articleid)
   (toplevel-items
    :type list
    :initarg :toplevel-items
    :accessor toplevel-items)))

(defmethod render ((text text-proper-item) stream)
  (format stream "environ")
  (dolist (item (toplevel-items text))
    (terpri stream)
    (render item stream)))

(defmethod print-object ((text text-proper-item) stream)
  (print-unreadable-object (text stream :type t :identity nil)
    (format stream "~a~%~{~a~%~}" (articleid text) (toplevel-items text))))

(defmacro |scheme| (name schematic-variables conclusion provisos &rest justification)
  (make-instance 'scheme-item
		 :name name
		 :schematic-variables (mapcar #'form->item schematic-variables)
		 :conclusion (form->item conclusion)
		 :provisos (mapcar #'form->item provisos)
		 :justification (mapcar #'form->item justification)))

(defclass formula-item (mizar-item)
  nil)

(defclass quantified-formula (formula-item)
  ((variables
    :type list
    :accessor variables
    :initarg :variables
    :initform (error "To create a quantified formula, please supply a list of variables."))
   (matrix
    :type formula-item
    :accessor matrix
    :initarg :matrix
    :initform (error "To create a quantified formula, please supply a matrix."))))

(defclass existential-quantifier-formula (quantified-formula)
  nil)

(defclass universal-quantifier-formula (quantified-formula)
  nil)

(defclass atomic-formula (formula-item)
  nil)

(defclass predicative-formula (atomic-formula)
  ((predicate
    :type string
    :accessor predicate
    :initarg :predicate
    :initform (error "Missing predicate from a predicative formula"))
   (left-arguments
    :type list
    :accessor left-arguments
    :initarg :left-arguments
    :initform nil)
   (right-arguments
    :type list
    :accessor right-arguments
    :initarg :right-arguments
    :initform nil)))

(defclass attributive-formula (atomic-formula)
  ((term
    :type (or string mizar-term)
    :accessor term
    :initarg :term
    :initform (error "To make an attributive formula, a term is required."))
   (type
    :type (or mizar-type adjective-cluster)
    :accessor attributive-type
    :initarg :type
    :initform (error "To make an attributive formula, a type is required."))))

(defclass qualifying-formula (atomic-formula)
  ((term
    :type (or string mizar-term)
    :accessor term
    :initarg :term
    :initform (error "To make a qualifying formula, a term is required."))
   (type
    :type (or mizar-type adjective-cluster)
    :accessor attributive-type
    :initarg :type
    :initform (error "To make a qualifying formula, a type is required."))))

(defclass adjective (mizar-item)
  ((negated
    :type boolean
    :initform nil
    :accessor negated-p
    :initarg :negated)
   (spelling
    :type string
    :initform (error "Please supply a spelling for an adjective.")
    :initarg :spelling
    :accessor spelling)))

(defmacro |adjective| (adjective)
  (make-instance 'adjective
		 :spelling (form->item adjective)))

(defmacro |negated-adjective| (adjective)
  (make-instance 'adjective
		 :spelling (form->item adjective)
		 :negated t))

(defmacro |attributive-formula| (term type)
  (make-instance 'attributive-formula
		 :term (form->item term)
		 :type (form->item type)))

(defmacro |qualifying-formula| (term type)
  (make-instance 'qualifying-formula
		 :term (form->item term)
		 :type (form->item type)))

(defclass infix-term (mizar-term)
  ((functor
    :type string
    :accessor functor
    :initarg :functor
    :initform (error "Missing functor from an infix term"))
   (left-arguments
    :type list
    :accessor left-arguments
    :initarg :left-arguments
    :initform nil)
   (right-arguments
    :type list
    :accessor right-arguments
    :initarg :right-arguments
    :initform nil)))

(defmacro |predicative-formula| (spelling left-arguments right-arguments)
  (make-instance 'predicative-formula
		 :predicate spelling
		 :left-arguments (mapcar #'form->item left-arguments)
		 :right-arguments (mapcar #'form->item right-arguments)))

(defmacro |infix-term| (functor left-arguments right-arguments)
  (make-instance 'infix-term
		 :functor functor
		 :left-arguments (mapcar #'form->item left-arguments)
		 :right-arguments (mapcar #'form->item right-arguments)))

(defclass circumfix-term (mizar-term)
  ((left
    :type string
    :accessor left
    :initarg :left
    :initform (error "To create a circumfix term, a left symbol is required."))
   (right
    :type string
    :accessor right
    :initarg :right
    :initform (error "To create a circumfix term, a right symbol is required."))
   (arguments
    :type list
    :accessor arguments
    :initarg :arguments
    :initform nil)))

(defmacro |circumfix-term| (left right &rest arguments)
  (make-instance 'circumfix-term
		 :left left
		 :right right
		 :arguments (mapcar #'form->item arguments)))

(defclass assumption-item (mizar-item)
  nil)

(defclass single-assumption (assumption-item)
  ((proposition
    :type proposition-item
    :accessor proposition
    :initarg :proposition
    :initform (error "To create a single assumption, a proposition is required."))))

(defclass collective-assumption (assumption-item)
  ((conditions
    :type list
    :accessor conditions
    :initarg :conditions
    :initform (error "A list of conditions for a collective assumption is necessary."))))

(defmacro |collective-assumption| (&rest conditions)
  (make-instance 'collective-assumption
		 :conditions (mapcar #'form->item conditions)))

(defclass existential-assumption (mizar-item)
  ((variables
    :type list
    :accessor variables
    :initarg :variables
    :initform (error "To make an existential assumption, a non-null list of variables is needed."))
   (conditions
    :type list
    :accessor conditions
    :initarg :conditions
    :initform (error "To make an existential assumption, a non-null list of conditions is needed."))))

(defmacro |existential-assumption| (variables conditions)
  (make-instance 'existential-assumption
		 :variables (mapcar #'form->item variables)
		 :conditions (mapcar #'form->item conditions)))

(defmacro |assumption| (assumption)
  (if assumption
      (form->item assumption)
      (error "Empty assumption.")))

(defmacro |single-assumption| (proposition)
  (make-instance 'single-assumption
		 :proposition (form->item proposition)))

(defmacro |private-functor-term| (functor &rest arguments)
  (make-instance 'private-functor-term
		 :spelling functor
		 :arguments (mapcar #'form->item arguments)))

(defmacro |private-predicate-formula| (predicate &rest arguments)
  (make-instance 'private-predicate-formula
		 :predicate predicate
		 :arguments (mapcar #'form->item arguments)))

(defclass negated-formula (formula-item)
  ((argument
    :accessor unnegate
    :type formula-item
    :initarg :argument
    :initform (error "To create a negated formula, an argument is require."))))

(defmacro |negated-formula| (argument)
  (make-instance 'negated-formula
		 :argument (form->item argument)))

(defclass biconditional-formula (formula-item)
  ((lhs
    :type formula-item
    :accessor lhs
    :initarg :lhs
    :initform (error "Missing left-hand side from a biconditional."))
   (rhs
    :type formula-item
    :accessor rhs
    :initarg :rhs
    :initform (error "Missing right-hand side from a biconditional."))))

(defclass conditional-formula (formula-item)
  ((antecedent
    :type formula-item
    :accessor antecedent
    :initarg :antecedent
    :initform (error "Missing antecedent of a conditional."))
   (consequent
    :type formula-item
    :accessor consequent
    :initarg :consequent
    :initform (error "Missing consequent of a conditional."))))

(defclass conjunctive-formula (formula-item)
  ((lhs
    :type formula-item
    :accessor lhs
    :initarg :lhs
    :initform (error "Missing left-hand side from a conjunction."))
   (rhs
    :type formula-item
    :accessor rhs
    :initarg :rhs
    :initform (error "Missing right-hand side from a conjunction."))))

(defclass disjunctive-formula (formula-item)
  ((lhs
    :type formula-item
    :accessor lhs
    :initarg :lhs
    :initform (error "Missing left-hand side from a disjunction."))
   (rhs
    :type formula-item
    :accessor rhs
    :initarg :rhs
    :initform (error "Missing right-hand side from a disjunction."))))

(defmacro |conditional-formula| (antecedent consequent)
  (make-instance 'conditional-formula
		 :antecedent (form->item antecedent)
		 :consequent (form->item consequent)))

(defmacro |conjunctive-formula| (lhs rhs)
  (make-instance 'conjunctive-formula
		 :lhs (form->item lhs)
		 :rhs (form->item rhs)))

(defmacro |disjunctive-formula| (lhs rhs)
  (make-instance 'disjunctive-formula
		 :lhs (form->item lhs)
		 :rhs (form->item rhs)))

(defmacro |biconditional-formula| (lhs rhs)
  (make-instance 'biconditional-formula
		 :lhs (form->item lhs)
		 :rhs (form->item rhs)))

(defparameter *reserved-variables* (make-hash-table :test #'equal))

(defun variable-list->variables (variable-list)
  (loop
     for variable in variable-list
     collect (cond ((stringp variable)
		    (multiple-value-bind (reserved-variable known?)
			(gethash variable *reserved-variables*)
		      (if known?
			  reserved-variable
			  (error "~a is not reserved." variable))))
		   ((listp variable)
		    (let ((variable-spelling (first variable))
			  (type (second variable)))
		      (make-instance 'simple-variable
				     :spelling variable-spelling
				     :type (form->item type)))))
       into variables
       finally (return variables)))

(defmacro |existential-quantifier-formula| (variables matrix)
  (make-instance 'existential-quantifier-formula
		 :variables (variable-list->variables variables)
		 :matrix (form->item matrix)))

(defmacro |universal-quantifier-formula| (variables matrix)
  (make-instance 'universal-quantifier-formula
		 :variables (variable-list->variables variables)
		 :matrix (form->item matrix)))

(defclass scheme-item (mizar-item)
  ((name
    :type string
    :accessor name
    :initarg :name
    :initform (error "To specify a scheme, a name is required."))
   (schematic-variables
    :type list
    :accessor schematic-variables
    :initarg :schematic-variables
    :initform nil)
   (provisos
    :type list
    :accessor provisos
    :initarg :provisos
    :initform nil)
   (conclusion
    :type formula-item
    :accessor conclusion
    :initarg :conclusion
    :initform (error "To specify a scheme, a conclusion is required."))
   (justification
    :type list
    :accessor justification
    :initarg :justification
    :initform nil)))

(defclass justification-item (mizar-item)
  nil)

(defclass definition-reference (mizar-item)
  ((article
   :type string
   :accessor article
   :initarg :article
   :initform (error "To create a definition reference, an article is required."))
   (nr
    :type integer
    :accessor nr
    :initarg :nr
    :initform (error "To create a definition reference, an nr is required."))))

(defclass theorem-reference (mizar-item)
  ((article
   :type string
   :accessor article
   :initarg :article
   :initform (error "To create a theorem reference, an article is required."))
   (nr
    :type integer
    :accessor nr
    :initarg :nr
    :initform (error "To create a theorem reference, an nr is required."))))

(defmacro |definition-reference| (article nr)
  (make-instance 'definition-reference
		 :article article
		 :nr nr))

(defmacro |theorem-reference| (article nr)
  (make-instance 'theorem-reference
		 :article article
		 :nr nr))

(defclass straightforward-justification (justification-item)
  ((references
    :type list
    :accessor references
    :initarg :references
    :initform nil)
   (linked
    :type boolean
    :accessor linked-p
    :initarg :linked
    :initform nil)))

(defmethod print-object ((just straightforward-justification) stream)
  (print-unreadable-object (just stream :type t :identity nil)
    (when (linked-p just)
      (format stream "(linked) "))
    (format stream "~{~a~}" (references just))))

(defclass proof-item (justification-item)
  ((steps
    :type list
    :accessor steps
    :initarg :steps
    :initform nil)))

(defmacro |proof| (&rest steps)
  (make-instance 'proof-item
		 :steps (mapcar #'form->item steps)))

(defclass scheme-justification (justification-item)
  ((arguments
    :type list
    :accessor arguments
    :initarg :arguments
    :initform nil)))

(defclass external-scheme-justification (scheme-justification)
  ((article
    :type string
    :accessor article
    :initform (error "To make an external scheme justification, an article must be supplied.")
    :initarg :article)
   (nr
    :type integer
    :accessor nr
    :initform (error "To make an external scheme justification, a number must be provided.")
    :initarg :nr)))

(defclass internal-scheme-justification (scheme-justification)
  ((spelling
    :type string
    :accessor spelling
    :initarg :spelling
    :initform (error "To make an article-internal scheme justification, a spelling is needed."))))

(defmacro |straightforward-justification| (linked-p &rest references)
  (make-instance 'straightforward-justification
		 :references (mapcar #'form->item references)
		 :linked (if (null linked-p)
			     nil
			     t)))

(defmacro |internal-scheme-justification| (spelling &rest arguments)
  (make-instance 'internal-scheme-justification
		 :spelling (form->item spelling)
		 :arguments (mapcar #'form->item arguments)))

(defmacro |external-scheme-justification| (article nr &rest arguments)
  (make-instance 'external-scheme-justification
		 :article (form->item article)
		 :nr nr
		 :arguments (mapcar #'form->item arguments)))

(defclass exemplification (mizar-item)
  ((variables
    :type list
    :accessor variables
    :initarg :variables
    :initform (error "To make an exemplification, a non-null list of variables is needed."))))

(defmacro |take| (&rest variables)
  (make-instance 'exemplification
		 :variables (mapcar #'form->item variables)))

(defclass justified-mixin ()
  ((justification
    :type (or null justification-item)
    :accessor justification
    :initarg :justification
    :initform nil)))

(defclass choice-statement (mizar-item justified-mixin)
  ((variables
    :type list
    :accessor variables
    :initarg :variables
    :initform (error "To make a choice statement, a non-null list of variables is needed."))
   (conditions
    :type list
    :accessor conditions
    :initarg :conditions
    :initform (error "To make a choice statement, a non-null list of conditions is needed."))))

(defclass generalization (mizar-item)
  ((variables
    :type list
    :accessor variables
    :initarg :variables
    :initform (error "To make a generalization, a non-null list of variables is needed."))))

(defclass loci-declaration (mizar-item)
  ((variables
    :type list
    :accessor variables
    :initarg :variables
    :initform (error "To make a loci declaration, a non-null list of variables is needed."))))

(defmacro |generalization| (&rest variables)
  (make-instance 'generalization
		 :variables (variable-list->variables variables)))

(defmacro |loci-declaration| (&rest variables)
  (make-instance 'loci-declaration
		 :variables (variable-list->variables variables)))

(defclass attribute-definition (mizar-item)
  ((spelling
    :type string
    :accessor spelling
    :initform (error "To create an attribute definition, a spelling is required for the new attribute.")
    :initarg :spelling)
   (label
    :type (or null string)
    :accessor label
    :initform nil
    :initarg :label)
   (definiens
       :type formula-item
     :accessor definiens
     :initarg :definiens
     :initform (error "To create an attribute definition, a definiens is required."))))

(defclass mode-definition (mizar-item)
  ((pattern
    :type (or null mode-pattern)
    :accessor pattern
    :initform nil
    :initarg :pattern)
      (label
    :type (or null string)
    :accessor label
    :initform nil
    :initarg :label)
   (definiens
       :type (or null mizar-type formula-item)
     :accessor definiens
     :initform nil
     :initarg :definiens)))

(defmacro |expandable-mode-definition| (pattern type)
  (make-instance 'mode-definition
		 :pattern (form->item pattern)
		 :definiens (form->item type)))

(defmacro |standard-mode-definition| (pattern label definiens)
  (make-instance 'mode-definition
		 :pattern (form->item pattern)
		 :label label
		 :definiens (form->item definiens)))

(defclass functor-definition (mizar-item)
  ((pattern
    :type (or null functor-pattern)
    :accessor pattern
    :initform nil
    :initarg :pattern)
   (label
    :type (or null string)
    :accessor label
    :initform nil
    :initarg :label)
   (definiens
       :type (or null mizar-term formula-item)
     :accessor definiens
     :initform nil
     :initarg :definiens)))

(defclass predicate-definition (mizar-item)
  ((pattern
    :type (or null predicate-pattern)
    :accessor pattern
    :initform nil
    :initarg :pattern)
   (label
    :type (or null string)
    :accessor label
    :initform nil
    :initarg :label)
   (definiens
       :type (or null formula-item)
     :accessor definiens
     :initform nil
     :initarg :definiens)))

(defmacro |functor-definition| (pattern label definiens)
  (make-instance 'functor-definition
		 :label label
		 :pattern (form->item pattern)
		 :definiens (form->item definiens)))

(defmacro |predicate-definition| (pattern label definiens)
  (make-instance 'predicate-definition
		 :label label
		 :pattern (form->item pattern)
		 :definiens (form->item definiens)))

(defmacro |attribute-definition| (spelling label definiens)
  (make-instance 'attribute-definition
		 :spelling (form->item spelling)
		 :label (form->item label)
		 :definiens (form->item definiens)))

(defclass functor-pattern (mizar-pattern)
  nil)

(defclass mode-pattern (mizar-pattern)
  nil)

(defclass bracket-functor-pattern (functor-pattern)
  ((left
    :type string
    :accessor left
    :initarg :left
    :initform (error "To create a circumfix term, a left symbol is required."))
   (right
    :type string
    :accessor right
    :initarg :right
    :initform (error "To create a circumfix term, a right symbol is required."))
   (arguments
    :type list
    :accessor arguments
    :initarg :arguments
    :initform nil)))

(defmacro |bracket-functor-pattern| (left right &rest arguments)
  (make-instance 'bracket-functor-pattern
		 :left left
		 :right right
		 :arguments (mapcar #'form->item arguments)))

(defclass operation-functor-pattern (functor-pattern)
  ((spelling
    :type string
    :accessor spelling
    :initform (error "To make a functor pattern, please supply a spelling.")
    :initarg :spelling)
   (left-arguments
    :type list
    :accessor left-arguments
    :initform nil
    :initarg :left-arguments)
   (right-arguments
    :type list
    :accessor right-arguments
    :initform nil
    :initarg :right-arguments)))

(defclass mode-pattern (mizar-pattern)
  ((spelling
    :type string
    :accessor spelling
    :initform (error "To make a mode pattern, please supply a spelling.")
    :initarg :spelling)
   (arguments
    :type list
    :accessor arguments
    :initform nil
    :initarg :arguments)))

(defmacro |mode-pattern| (spelling &rest arguments)
  (make-instance 'mode-pattern
		 :spelling spelling
		 :arguments (mapcar #'form->item arguments)))

(defclass predicate-pattern (mizar-pattern)
  ((spelling
    :type string
    :accessor spelling
    :initform (error "To make a predicate pattern, please supply a spelling.")
    :initarg :spelling)
   (left-arguments
    :type list
    :accessor left-arguments
    :initform nil
    :initarg :left-arguments)
   (right-arguments
    :type list
    :accessor right-arguments
    :initform nil
    :initarg :right-arguments)))

(defmacro |operation-functor-pattern| (spelling left right)
  (make-instance 'operation-functor-pattern
		 :spelling spelling
		 :left-arguments (mapcar #'form->item left)
		 :right-arguments (mapcar #'form->item right)))

(defclass iterative-equality (mizar-item)
  ((initial-equation
    :type proposition-item
    :accessor initial-equation
    :initarg :initial-equation
    :initform (error "An iterative equality needs an initial equation."))
   (initial-justification
    :type justification-item
    :accessor initial-justification
    :initarg :initial-justification
    :initform (error "An iterative equality needs an initial justification."))
   (steps
    :type list
    :accessor steps
    :initform nil
    :initarg :steps)))

(defmacro |iterative-equality| (initial-equation initial-justification &rest steps)
  (make-instance 'iterative-equality
		 :initial-equation (form->item initial-equation)
		 :initial-justification (form->item initial-justification)
		 :steps (mapcar #'form->item steps)))

(defmacro |predicate-pattern| (spelling left right)
  (make-instance 'predicate-pattern
		 :spelling spelling
		 :left-arguments (mapcar #'form->item left)
		 :right-arguments (mapcar #'form->item right)))

(defclass conclusion (mizar-item justified-mixin)
  ((proposition
    :type (or proposition-item iterative-equality)
    :accessor proposition
    :initarg :proposition
    :initform (error "To create a conclusion item, a proposition is necessary."))))

(defclass diffuse-conclusion (mizar-item)
  ((proposition
    :type hereby-reasoning
    :accessor proposition
    :initarg :proposition
    :initform (error "To create a diffuse conclusion item, a proposition is necessary."))))

(defmacro |thus| (proposition &optional justification)
  (make-instance 'conclusion
		 :proposition (form->item proposition)
		 :justification (when justification (form->item justification))))

(defmacro |thus-diffuse| (diffuse-proposition)
  (make-instance 'diffuse-conclusion
		 :proposition (form->item diffuse-proposition)))

(defmacro |consider| (variables conditions justification)
  (make-instance 'choice-statement
		 :variables (mapcar #'form->item variables)
		 :conditions (mapcar #'form->item conditions)
		 :justification (form->item justification)))

(defclass predicate-segment (mizar-item)
  ((variables
    :type list
    :initform (error "To create a predicate-segment object, a non-null list of variables is required.")
    :initarg :variables
    :accessor variables)
   (type-list
    :type list
    :accessor type-list
    :initarg :type-list
    :initform nil)))

(defclass functor-segment (mizar-item)
  ((variables
    :type list
    :initform (error "To create a functor-segment object, a non-null list of variables is required.")
    :initarg :variables
    :accessor variables)
   (type-list
    :type list
    :accessor type-list
    :initarg :type-list
    :initform nil)
   (type
    :type mizar-type
    :accessor functor-segment-type
    :initarg :type
    :initform (error "To create a functor-segment object, a type is required."))))

(defclass correctness-condition (mizar-item justified-mixin)
  nil)

(defclass coherence-condition (correctness-condition)
  nil)

(defclass compatibility-condition (correctness-condition)
  nil)

(defclass existence-condition (correctness-condition)
  nil)

(defclass uniqueness-condition (correctness-condition)
  nil)

(defclass commutativity-condition (correctness-condition)
  nil)

(defclass idempotence-condition (correctness-condition)
  nil)

(defclass correctness-item (correctness-condition)
  nil)

(defclass symmetry-condition (correctness-condition)
  nil)

(defclass antisymmetry-condition (correctness-condition)
  nil)

(defclass reflexivity-condition (correctness-condition)
  nil)

(defclass irreflexivity-condition (correctness-condition)
  nil)

(defmacro |correctness| (justification)
  (make-instance 'correctness-item
		 :justification (form->item justification)))

(defmacro |existence| (justification)
  (make-instance 'existence-condition
		 :justification (form->item justification)))

(defmacro |uniqueness| (justification)
  (make-instance 'uniqueness-condition
		 :justification (form->item justification)))

(defmacro |coherence| (justification)
  (make-instance 'coherence-condition
		 :justification (form->item justification)))

(defmacro |compatibility| (justification)
  (make-instance 'compatibility-condition
		 :justification (form->item justification)))

(defmacro |commutativity| (justification)
  (make-instance 'commutativity-condition
		 :justification (form->item justification)))

(defmacro |idempotence| (justification)
  (make-instance 'idempotence-condition
		 :justification (form->item justification)))

(defmacro |symmetry| (justification)
  (make-instance 'symmetry-condition
		 :justification (form->item justification)))

(defmacro |reflexivity| (justification)
  (make-instance 'reflexivity-condition
		 :justification (form->item justification)))

(defmacro |irreflexivity| (justification)
  (make-instance 'irreflexivity-condition
		 :justification (form->item justification)))

(defmacro |asymmetry| (justification)
  (make-instance 'antisymmetry-condition
		 :justification (form->item justification)))

(defmacro |definitional-block| (&rest items)
  (make-instance 'definition-block
		 :items (mapcar #'form->item items)))

(defclass block-mixin ()
  ((items
    :type list
    :accessor items
    :initform (error "To create a definition block, a non-null list of items is required.")
    :initarg :items)))

(defclass definition-block (mizar-item block-mixin)
  nil)

(defclass case-block (mizar-item block-mixin)
  nil)

(defclass suppose-block (mizar-item block-mixin)
  ((suppositions
    :type list
    :accessor suppositions
    :initarg :suppositions
    :initform nil)))

(defmacro |case-block| (&rest items)
  (make-instance 'case-block
		 :items (mapcar #'form->item items)))

(defmacro |suppose-block| (assumptions &rest items)
  (make-instance 'suppose-block
		 :suppositions (mapcar #'form->item assumptions)
		 :items (mapcar #'form->item items)))

(defclass hereby-reasoning (mizar-item block-mixin)
  nil)

(defmacro |hereby| (&rest items)
  (make-instance 'hereby-reasoning
		 :items (mapcar #'form->item items)))

(defclass per-cases (justified-mixin)
  nil)

(defmacro |per-cases| (justification)
  (make-instance 'per-cases
		 :justification (form->item justification)))

(defclass mizar-pattern (mizar-item)
  nil)

(defclass mizar-definition (mizar-item)
  nil)

(defclass private-predicate-definition (mizar-definition)
  ((predicate
    :type string
    :accessor predicate
    :initarg :predicate
    :initform (error "To create a private predicate definition, a predicate is required."))
   (signature
    :type list
    :accessor :signature
    :initarg :signature
    :initform nil)
   (definiens
     :type formula-item
     :accessor definiens
     :initarg :definiens
     :initform (error "To create a private predicate definition, a definiens is required."))))

(defmacro |private-predicate-definition| (predicate signature definiens)
  (make-instance 'private-predicate-definition
		 :predicate predicate
		 :signature (mapcar #'form->item signature)
		 :definiens (form->item definiens)))

(defclass proposition-item (mizar-item)
  ((label
    :type (or null string)
    :accessor label
    :initarg :label
    :initform nil)
   (formula
    :type formula-item
    :accessor formula
    :initarg :formula
    :initform (error "To make a proposition, a formula is required."))o))

(defmethod print-object ((prop proposition-item) stream)
  (print-unreadable-object (prop stream :type t :identity nil)
    (let ((label (label prop)))
      (when label
	(format stream "(~a) " label))
      (format stream "~a" (formula prop)))))

(defclass regular-statement (mizar-item justified-mixin)
  ((proposition
    :type proposition-item
    :accessor proposition
    :initarg :proposition
    :initform (error "To make a regular statement, a proposition is needed."))))

(defmacro |regular-statement| (proposition justification)
  (make-instance 'regular-statement
		 :proposition (form->item proposition)
		 :justification (form->item justification)))

(defclass theorem-item (mizar-item justified-mixin)
  ((proposition
    :type proposition-item
    :accessor proposition
    :initarg :proposition
    :initform (error "To make a theorem, a proposition is required."))))

(defmethod print-object ((theorem theorem-item) stream)
  (print-unreadable-object (theorem stream :type t :identity nil)
    (let ((prop (proposition theorem))
	  (justification (justification theorem)))
      (format stream "~a ~a" prop justification))))

(defmacro |theorem-item| (proposition justification)
  (make-instance 'theorem-item
		 :proposition (form->item proposition)
		 :justification (form->item justification)))

(defclass diffuse-statement (mizar-item)
  ((reasoning
    :type list
    :accessor reasoning
    :initarg :reasoning
    :initform (error "To create a diffuse statement, a non-null list of steps is required."))))

(defmacro |diffuse-statement| (&rest reasoning)
  (make-instance 'diffuse-statement
		 :reasoning (mapcar #'form->item reasoning)))

(defclass thesis-item (formula-item)
  nil)

(defmacro |thesis| ()
  (make-instance 'thesis-item))

(defmacro |proposition| (label formula)
  (make-instance 'proposition-item
		 :label (when label (form->item label))
		 :formula (form->item formula)))

(defmacro |functor-segment| (variables type-list value-type)
  (let ((value-type (form->item value-type))
	(type-list (mapcar #'form->item type-list)))
    (flet ((make-schematic-functor-variable (variable)
	     (make-instance 'schematic-functor-variable
			    :arguments type-list
			    :value-type value-type
			    :spelling variable)))
      (mapcar #'make-schematic-functor-variable variables))))

(defmacro |predicate-segment| (variables type-list)
  (make-instance 'predicate-segment
		 :variables (mapcar #'form->item variables)
		 :type-list (mapcar #'form->item type-list)))

(defclass section-pragma-item (mizar-item)
  nil)

(defmethod print-object ((section section-pragma-item) stream)
  (print-unreadable-object (section stream :type t :identity nil)))

(defclass reservation-item (mizar-item)
  ((variables
    :type list
    :accessor variables
    :initarg :variables
    :initform (error "To create a reservation, please supply a non-null list of variables."))
   (type
    :type mizar-type
    :accessor reserved-type
    :initarg :type
    :initform (error "To create a reservation, please supply a type for the reserved variables."))))

(defmethod initialize-instance :after ((reservation reservation-item) &rest initargs)
  (declare (ignore initargs))
  (let ((variables (variables reservation))
	(type (reserved-type reservation)))
    (dolist (variable variables)
      (setf (gethash variable *reserved-variables*) type))))

(defclass mizar-type (mizar-item)
  nil)

(defclass standard-type (mizar-type)
  ((radix
    :type string
    :accessor radix
    :initarg :radix
    :initform (error "To create a standard type, please supply a radix."))
   (arguments
    :type list
    :accessor arguments
    :initarg :arguments
    :initform nil)))

(defclass adjective-cluster (mizar-item)
  ((adjectives
    :type list
    :accessor adjectives
    :initarg :adjectives
    :initform nil)))

(defclass clustered-type (mizar-type)
  ((cluster
    :type adjective-cluster
    :accessor cluster
    :initarg :cluster
    :initform (error "To make a clustered type, an adjective cluster is needed."))
   (argument
    :type mizar-type
    :accessor argument
    :initarg :argument
    :initform (error "To make a clustered type, an argument type is required."))))

(defmacro |clustered-type| (cluster argument)
  (make-instance 'clustered-type
		 :cluster (form->item cluster)
		 :argument (form->item argument)))

(defmacro |adjective-cluster| (&rest adjectives)
  (make-instance 'adjective-cluster
		 :adjectives (mapcar #'form->item adjectives)))

(defgeneric parse (article))

(defmethod parse ((article article))
  (parse (path article)))

(defmethod parse :before ((article pathname))
  (makeenv article)
  (wsmparser article))

(defmethod parse ((article pathname))
  (let ((wsx (file-with-extension article "wsx")))
    (unless (file-exists-p wsx)
      (error "The .wsx file is missing for ~a." (native-namestring article)))
    (let ((article-as-lisp-string (apply-stylesheet *miz2lisp-stylesheet* wsx nil nil)))
      (let ((article-as-lisp (with-readtable (find-readtable :modern)
			       (read-from-string article-as-lisp-string))))
	(form->item article-as-lisp)))))

(defmethod parse ((article string))
  (let ((temp (temporary-file :base "article" :extension "miz")))
    (write-string-into-file article temp
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (unwind-protect (parse temp)
      (delete-file temp))))
