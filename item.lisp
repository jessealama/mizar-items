;;; item.lisp Bits of mizar articles

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass item ()
  ((source-article
    :initarg :source-article
    :accessor source-article)
   (additional-vocabularies
    :initarg :additional-vocabularies
    :initform nil
    :type list
    :accessor additional-vocabularies)
   (additional-notations
    :initarg :additional-notations
    :initform nil
    :type list
    :accessor additional-notations)
   (additional-constructors
    :initarg :additional-constructors
    :initform nil
    :type list
    :accessor additional-constructors)
   (additional-registrations
    :initarg :additional-registrations
    :initform nil
    :type list
    :accessor additional-registrations)
   (additional-requirements
    :initarg :additional-requirements
    :initform nil
    :type list
    :accessor additional-requirements)
   (additional-definitions
    :initarg :additional-definitions
    :initform nil
    :type list
    :accessor additional-definitions)
   (additional-theorems
    :initarg :additional-theorems
    :initform nil
    :type list
    :accessor additional-theorems)
   (additional-schemes
    :initarg :additional-schemes
    :initform nil
    :type list
    :accessor additional-schemes)
   (begin-line-number
    :initarg :begin-line-number
    :accessor begin-line-number)
   (end-line-number
    :initarg :end-line-number
    :accessor end-line-number)
   (begin-column-number
    :initarg :begin-column-number
    :accessor begin-column-number)
   (end-column-number
    :initarg :end-column-number
    :accessor end-column-number)
   (text
    :initarg :text
    :accessor text)
   (context-items
    :initarg :context
    :accessor context-items
    :type list
    :initform nil)
   (name
    :initarg :name
    :accessor name
    :initform nil
    :type (or string nil))
   (dependencies
    :initarg :dependencies
    :accessor dependencies
    :type list
    :initform nil))
  (:documentation "A mizar item represents a free-standing piece of a
  mizar article, such as a definition, a theorem, a notation, or a registration."))

(defun copy-item (item)
  (let ((new-item (make-instance 'item)))
    (setf (source-article new-item) (source-article item))
    (setf (begin-line-number new-item) (begin-line-number item))
    (setf (end-line-number new-item) (end-line-number item))
    (setf (begin-column-number new-item) (begin-column-number item))
    (setf (end-column-number new-item) (end-column-number item))
    (setf (text new-item) (text item))
    (setf (context-items new-item) (context-items item))
    (setf (name new-item) (name item))
    (setf (dependencies new-item) (dependencies item))
    (setf (additional-vocabularies new-item) (additional-vocabularies item))
    (setf (additional-notations new-item) (additional-notations item))
    (setf (additional-constructors new-item) (additional-constructors item))
    (setf (additional-registrations new-item) (additional-registrations item))
    (setf (additional-requirements new-item) (additional-requirements item))
    (setf (additional-definitions new-item) (additional-definitions item))
    (setf (additional-theorems new-item) (additional-theorems item))
    (setf (additional-schemes new-item) (additional-schemes item))
    new-item))

(defmethod initialize-instance :after ((item item) &key)
  "If we know the bounds and the soruce article, compute the text (if not already set)."
  (when (and (slot-boundp item 'source-article)
	     (slot-boundp item 'begin-line-number)
	     (slot-boundp item 'begin-column-number)
	     (slot-boundp item 'end-line-number)
	     (slot-boundp item 'end-column-number)
	     (not (slot-boundp item 'text)))
    (setf (text item)
	  (ensure-final-semicolon
	   (region (source-article item)
		   (begin-line-number item)
		   (begin-column-number item)
		   (end-line-number item)
		   (end-column-number item))))))

(defmethod line-at :before ((item item) line-number)
  (with-slots (begin-line-number)
      item
    (when (< line-number begin-line-number)
      (error "Cannot set line number ~d within item ~S because that item begins only at line ~d"
	     line-number item begin-line-number))))

(defmethod line-at :before ((item item) line-number)
  (with-slots (end-line-number)
      item
    (when (> line-number end-line-number)
      (error "Cannot set line number ~d within item ~S because that item end at line ~d"
	     line-number item end-line-number))))

(defmethod line-at ((item item) line-number)
  (with-slots (begin-line-number)
      item
    (let ((text-as-array (lines-as-array (text item))))
      (aref text-as-array (- line-number begin-line-number)))))

(defun set-line (item line-number new-line)
  (with-slots (begin-line-number end-line-number)
      item
    (when (< line-number begin-line-number)
      (error "Cannot set line number ~d within item ~S because that item begins only at line ~d"
	     line-number item begin-line-number))
    (when (> line-number end-line-number)
      (error "Cannot set line number ~d within item ~S because that item end at line ~d"
	     line-number item end-line-number))
    (let ((text-as-array (lines-as-array (text item))))
      (setf (aref text-as-array (- line-number begin-line-number)) new-line)
      (setf (text item) (array->newline-delimited-string text-as-array))))
  item)

(defclass xml-item (item)
  ((xml-node 
    :initarg :node
    :accessor xml-node)
   (vid
    :initarg :vid
    :accessor vid-value)
   (nr
    :initarg :nr
    :accessor nr-value)))

(defclass labelled-item-mixin ()
  ((label
    :initarg :label
    :accessor label)))

(defclass pseudo-item (xml-item)
  ()
  (:documentation "A pseudo-item represents a non-exportable part of a
  mizar article.  This means: reserve, set,
  consider, reconsider, deffunc, and defpred statements.  These are
  statements that cannot usefully comprise a stand-alone article
  fragment, since the mizar exporter produces nothing from them,
  whereas items that occur later in the article from which a
  pseudo-item is taken can refer to this item.  Such items need to be
  'carried along', prefixed to an exportable item."))

(defclass reservation-item (pseudo-item)
  ())

(defclass set-item (pseudo-item)
  ())

(defclass consider-item (pseudo-item)
  ())

(defclass reconsider-item (pseudo-item)
  ())

(defclass deffunc-item (pseudo-item)
  ())

(defclass defpred-item (pseudo-item)
  ())

(defclass iterequality-item (xml-item labelled-item-mixin)
  ())

(defclass now-item (pseudo-item labelled-item-mixin)
  ())

(defclass theorem-item (xml-item labelled-item-mixin)
  ())

(defclass scheme-item (xml-item labelled-item-mixin)
  ((schemenr
    :initarg :schemenr
    :accessor schemenr)))

(defclass proposition-item (xml-item labelled-item-mixin)
  ())

(defclass definition-item (xml-item)
  ((deftheorems
     :initarg :deftheorems
     :accessor deftheorems)))

(defclass deftheorem-item (xml-item labelled-item-mixin)
  ((source :initarg :source
	   :accessor source
	   :type definition-item)))

(defclass notation-item (xml-item)
  ())

(defclass registration-item (xml-item)
  ())

(defun item-contained-in-item (item-1 item-2)
  "Determine whether ITEM-1 occurs wholly inside ITEM-2."
  (with-slots ((source-1 source-article))
      item-1
    (with-slots ((source-2 source-article))
	item-2
      (if (eq source-1 source-2)
	  (with-slots ((bl-1 begin-line-number)
		       (bc-1 begin-column-number)
		       (el-1 end-line-number)
		       (ec-1 end-column-number))
	      item-1
	    (with-slots ((bl-2 begin-line-number)
			 (bc-2 begin-column-number)
			 (el-2 end-line-number)
			 (ec-2 end-column-number))
		item-2
	      (cond ((< bl-1 bl-2) nil)
		    ((= bl-1 bl-2) (cond ((< bc-1 bc-2) nil)
					 ((= bc-1 bc-2) nil)
					 (t (cond ((< el-1 el-2) t)
						  ((= el-1 el-2) (< ec-1 ec-2))
						  (t nil)))))
		    (t (cond ((< el-1 el-2) t)
			     ((= el-1 el-2) (< ec-1 ec-2))
			     (t nil))))))

	  (error "Cannot compare ~S and ~S: the first comes from article ~S, but the second comes from a differet article, ~S"
		 item-1 item-2 source-1 source-2)))))

(defun disjoin-items (item-list &optional (result nil))
  "Return a sublist of ITEM-LIST which is such that no item is wholly
contained in any other.  The order of the items returned might be
strange; sort if necessary."
  (if (null item-list)
      result
      (let ((item (car item-list)))
	(disjoin-items (cdr item-list)
		       (if (some #'(lambda (known-item)
				     (item-contained-in-item item known-item))
				 result)
			   result
			   (cons item result))))))

(defun item-lex-less (item-1 item-2)
  "Determine whether ITEM-1 occurs earlier than ITEM-2.  This
  procedure assumes that neither item is wholly contained in the
  other, so that we need to look only at where the items begin."
  (with-slots ((source-1 source-article))
      item-1
    (with-slots ((source-2 source-article))
	item-2
      (if (eq source-1 source-2)
	  (with-slots ((bl-1 begin-line-number)
		       (bc-1 begin-column-number))
	      item-1
	    (with-slots ((bl-2 begin-line-number)
			 (bc-2 begin-column-number))
		item-2
	      (or (< bl-1 bl-2)
		  (and (= bl-1 bl-2)
		       (< bc-1 bc-2)))))
	  (error "Cannot compare ~S and ~S: the first comes from article ~S, but the second comes from a differet article, ~S"
		 item-1 item-2 source-1 source-2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outputting items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric write-item (item &key directory)
  (:documentation "Generate an article corresponding to ITEM.  The
  article will be written under the directory DIRECTORY, and its name
  will be '<name>.miz', where <name> is the value of the NAME slot in
  ITEM."))

(defmethod write-item :before (item &key directory)
  (unless (probe-file directory)
    (error "Unable to write item to directory ~S: the directory doesn't exist" directory)))

(defmethod write-item :before (item &key directory)
  (declare (ignore directory))
  (let ((name (name item)))
    (when (> (length name) 8)
      (error "Invalid item name: the proposed name '~A' is longer than eight characters" name))))

(defun item->article (item)
  (with-slots (additional-vocabularies
	       additional-notations
	       additional-constructors
	       additional-registrations
	       additional-requirements
	       additional-definitions
	       additional-theorems
	       additional-schemes)
      item
    (let* ((original-article (source-article item))
	   (article-for-item (make-article-copying-environment-from original-article)))
      (setf (vocabularies article-for-item)
	    (remove-duplicates (append (vocabularies article-for-item) additional-vocabularies)
			       :test #'string=))
      (setf (notations article-for-item)
	    (remove-duplicates (append (notations article-for-item) additional-notations)
			       :test #'string=))
      (setf (constructors article-for-item)
	    (remove-duplicates (append (constructors article-for-item) additional-constructors)
			       :test #'string=))
      (setf (requirements article-for-item)
	    (remove-duplicates (append (requirements article-for-item) additional-requirements)
			       :test #'string=))
      (setf (registrations article-for-item)
	    (remove-duplicates (append (registrations article-for-item) additional-registrations)
			       :test #'string=))
      (setf (definitions article-for-item)
	    (remove-duplicates (append (definitions article-for-item) additional-definitions)
			       :test #'string=))
      (setf (theorems article-for-item)
	    (remove-duplicates (append (theorems article-for-item) additional-theorems)
			       :test #'string=))
      (setf (schemes article-for-item)
	    (remove-duplicates (append (schemes article-for-item) additional-schemes)
			       :test #'string=))
      (setf (text article-for-item) (concat (apply #'concat (mapcar #'(lambda (item)
									(format nil "~A~%" (text item)))
								    (context-items item)))
					    (text item)))
      article-for-item)))

(defmethod write-item (item &key (directory (sb-posix:getcwd)))
  (let ((article-for-item (item->article item)))
    (setf (path article-for-item)
	  (pathname-as-file (concat (namestring (pathname-as-directory (pathname directory)))
				    (format nil "~A.miz" (name item)))))
    (write-article article-for-item)))

;;; item.lisp ends here