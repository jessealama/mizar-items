
(in-package :mizar)

(defgeneric without-reservations (article)
  (:documentation "Compute the reservation-free form of ARTICLE."))

(defmethod without-reservations :around ((article-path pathname))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "No such file '~a'" (native-namestring article-path))))

(defmethod without-reservations :before ((article-path pathname))
  (let ((tpr-path (file-with-extension article-path "tpr"))
	(msm-path (file-with-extension article-path "msm")))
    (makeenv article-path)
    (wsmparser article-path)
    (msmprocessor article-path)
    (msplit article-path)
    (copy-file msm-path tpr-path)
    (mglue article-path)
    (accom article-path)
    (wsmparser article-path)
    (msmprocessor article-path)))

(defun reservation-node-p (thing)
  (let ((kind-attribute (xpath:evaluate "@kind" thing)))
    (and (string= (xpath:evaluate "name (.)" thing) "Item")
	 (not (xpath:node-set-empty-p kind-attribute))
	 (string= (xpath:string-value kind-attribute) "Reservation"))))

(defmethod without-reservations ((article-path pathname))
  (loop
     with items = (items article-path)
     for item in (remove-if #'reservation-node-p items)
     collect (without-reservations item) into wrm-items
     finally (return wrm-items)))

(defmethod without-reservations ((text-node dom:text))
  text-node)

(defmethod without-reservations ((node dom:element))
  (let ((name (xpath:evaluate "name (.)" node))
	(document (dom:owner-document node)))
    (if (string= name "Fraenkel-Term")
	(error "hey")
	(let ((new-node (dom:create-element document name)))
	  (dolist (child (children (xpath "*" node)) new-node)
	    (dom:append-child new-node (without-reservations child)))))))
