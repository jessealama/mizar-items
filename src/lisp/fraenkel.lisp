
(in-package :mizar)

(defgeneric explicitly-qualify-fraenkels (article)
  (:documentation "Ensure, for every Fraenkel term in ARTICLE, that all reseved variables appearing in the Fraenkel term that could be explicitly qualified do get explicitly qualified."))

(defmethod explicitly-qualify-fraenkels ((article article))
  (explicitly-qualify-fraenkels (path article)))

(defmethod explicitly-qualify-fraenkels ((article pathname))
  (let ((items (parse-tree article))
	(name (pathname-name article)))
    (let ((transformed (map 'item
			    #'explicitly-qualify-fraenkels
			    items)))
      (let ((text (make-instance 'text-proper-item
				 :attributes (list (cons "name" name))
				 :children transformed)))
	(write-text text article)))))

(defmethod explicitly-qualify-fraenkels ((item item))
  (make-instance 'item
		 :attributes (attributes item)
		 :children (map 'item
				#'explicitly-qualify-fraenkels
				(children item))))

(defmethod explicitly-qualify-fraenkels ((list list))
  (map 'list #'explicitly-qualify-fraenkels list))

(defmethod explicitly-qualify-fraenkels ((item fraenkel-term))
  (let ((term (term item))
	(qualifiers (qualifiers item))
	(formula (formula item)))
    (let ((qualified-term (explicitly-qualify-fraenkels term))
	  (qualified-qualifiers (explicitly-qualify-fraenkels qualifiers))
	  (qualified-formula (explicitly-qualify-fraenkels formula)))
      (let ((free-in-term (free-variables qualified-term))
	    (free-in-formula (free-variables qualified-formula))
	    (free-in-qualifiers (free-variables qualified-qualifiers)))
	(let ((free-outside-term (set-union free-in-formula
					    free-in-qualifiers)))
	  (let ((to-inspect (set-difference free-outside-term
					    free-in-term)))
	    (let ((still-free (remove-if #'(lambda (x)
					     (member x qualified-qualifiers))
					 to-inspect)))
	      (let ((more-qualifiers (mapcar #'(lambda (x)
						 (make-instance 'explicitly-qualified-segment
								:variables (list x)
								:type (type-of x)))
					     still-free)))
		(make-instance 'fraenkel-term
			   :term qualified-term
			   :qualifiers (append more-qualifiers
					       qualified-qualifiers)
			   :formula qualified-formula)))))))))
