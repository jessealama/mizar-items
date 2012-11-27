
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

(defmethod without-reservations ((article-path pathname))
  (loop
     with items = (items article-path)
     for item in (remove-if #'reservation-item-p items)
     collect (without-reservations item) into wrm-items
     finally (return wrm-items)))
