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
	  ((symbolp form) form)
	  (t
	   (error "The form~%~%  ~a~%~%did not match any known form-to-item rules, nor is is a user symbol." form)))))

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
  ((spelling
    :accessor spelling
    :type symbol
    :initarg :spelling
    :initform (error "To make a variable, please provide a spelling."))))

(defclass variable-item (mizar-term)
  nil)

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
  ((arguments
   :type list
   :accessor arguments
   :initarg :arguments
   :initform nil)))

(defclass private-predicate-formula (atomic-formula)
  ((predicate
    :type symbol
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
    :type symbol
    :initarg :articleid
    :accessor articleid)
   (toplevel-items
    :type list
    :initarg :toplevel-items
    :accessor toplevel-items)))

(defmacro |scheme| (name schematic-variables conclusion provisos &rest justification)
  (make-instance 'scheme-item
		 :name name
		 :schematic-variables (mapcar #'form->item schematic-variables)
		 :conclusion (form->item conclusion)
		 :provisos provisos
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
    :type symbol
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

(defmacro |predicative-formula| (spelling left-arguments right-arguments)
  (make-instance 'predicative-formula
		 :predicate spelling
		 :left-arguments (mapcar #'form->item left-arguments)
		 :right-arguments (mapcar #'form->item right-arguments)))

(defmacro |private-functor-term| (functor &rest arguments)
  (make-instance 'private-functor-term
		 :spelling functor
		 :arguments (mapcar #'form->item arguments)))

(defmacro |private-predicate-formula| (predicate &rest arguments)
  (make-instance 'private-predicate-formula
		 :predicate predicate
		 :arguments (mapcar #'form->item arguments)))

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

(defmacro |conjunctive-formula| (lhs rhs)
  (make-instance 'conjunctive-formula
		 :lhs (form->item lhs)
		 :rhs (form->item rhs)))

(defmacro |iff| (lhs rhs)
  (make-instance 'biconditional-formula
		 :lhs (form->item lhs)
		 :rhs (form->item rhs)))

(defparameter *reserved-variables* (make-hash-table :test #'eql))

(defun variable-list->variables (variable-list)
  (loop
     for variable in variable-list
     collect (cond ((symbolp variable)
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
				     :type type))))
       into variables
       finally (return variables)))

(defmacro |∃| (variables matrix)
  (make-instance 'existential-quantifier-formula
		 :variables (variable-list->variables variables)
		 :matrix (form->item matrix)))

(defmacro |∀| (variables matrix)
  (make-instance 'universal-quantifier-formula
		 :variables (variable-list->variables variables)
		 :matrix (form->item matrix)))

(defclass scheme-item (mizar-item)
  ((name
    :type symbol
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

(defclass private-predicate-definition (mizar-item)
  ((predicate
    :type symbol
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
    :type symbol
    :accessor radix
    :initarg :radix
    :initform (error "To create a standard type, please supply a radix."))
   (arguments
    :type list
    :accessor arguments
    :initarg :arguments
    :initform nil)))

(defgeneric parse (article))

(defmethod parse ((article article))
  (parse (path article)))

(defmethod parse :before ((article pathname))
  (let ((tpr-path (file-with-extension article "tpr"))
	(msm-path (file-with-extension article "msm")))
    (makeenv article)
    (wsmparser article)
    (msmprocessor article)
    (msplit article)
    (copy-file msm-path tpr-path)
    (mglue article)
    (accom article)
    (wsmparser article)
    (msmprocessor article)))

(defmethod parse ((article pathname))
  (let ((wsx (file-with-extension article "wsx")))
    (unless (file-exists-p wsx)
      (error "The .wsx file is missing for ~a." (native-namestring article)))
    (let ((article-as-lisp-string (apply-stylesheet *miz2lisp-stylesheet* wsx nil nil)))
      (let ((article-as-lisp (with-readtable (find-readtable :modern)
			       (read-from-string article-as-lisp-string))))
	(form->item article-as-lisp)))))
