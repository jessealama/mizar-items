
(in-package :mizar)

(defclass author ()
  ((name
    :type string
    :accessor name
    :initarg :name
    :documentation "The name of an author.")))

(define-condition nameless-author-error (error)
  ((proposed-name
    :initarg :proposed-name
    :initform nil
    :reader proposed-name))
  (:report
   (lambda (condition stream)
     (with-slots (proposed-name)
	 condition
       (if (null proposed-name)
	   (format stream "Authors must have a name, but none was supplied.")
	   (format stream "Authors must have a name; '~a' doesn't count as one, because the name must be a string." proposed-name))))))

(defmethod initialize-instance :after ((author author) &rest initargs)
  (declare (ignore initargs))
  (if (slot-boundp author 'name)
      (when (null (name author))
	(error 'nameless-author-error))
      (error 'nameless-author-error)))

(let ((authors (make-hash-table :test #'equal)))
  (defmethod make-instance :around ((class (eql 'author)) &rest initargs)
    (let ((name-tail (member :name initargs)))
      (if name-tail
	  (let ((name (second name-tail)))
	    (if name
		(if (stringp name)
		    (multiple-value-bind (existing present?)
			(gethash name authors)
		      (if present?
			  (progn () existing)
			  (let ((new-author (call-next-method)))
			    (setf (gethash name authors) new-author)
			    new-author)))
		    (error 'nameless-author-error :proposed-name name))
		(error 'nameless-author-error :proposed-name nil)))
	  (error 'nameless-author-error :proposed-name nil))))
  (defun authors-table () authors))

(defmethod print-object ((author author) stream)
  (print-unreadable-object (author stream)
    (if (slot-boundp author 'name)
	(format stream "~a" (name author))
	(format stream "(anonymous)"))))