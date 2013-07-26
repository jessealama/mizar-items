
(in-package :mizar)

(defgeneric without-reservations (article)
  (:documentation "Compute the reservation-free form of ARTICLE."))

(defmethod without-reservations :around ((article-path pathname))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "No such file '~a'" (native-namestring article-path))))

(defmethod without-reservations :before ((article-path pathname))
  (let (($-$ (file-with-extension article-path "$-$"))
        (miz (file-with-extension article-path "miz")))
    (accom article-path)
    (msplit article-path)
    (unhereby article-path)
    (edtfile article-path)
    (copy-file $-$ miz)
    (dellink article-path)
    (edtfile article-path)
    (copy-file $-$ miz)
    (analyzer article-path)))

(defmethod without-reservations ((article-path pathname))
  (let ((msx (file-with-extension article-path "msx")))
    (apply-stylesheet *wrm-stylesheet* msx nil msx)))

(defmethod without-reservations :after ((article-path pathname))
  (let ((msx (file-with-extension article-path "msx"))
        (tpr (file-with-extension article-path "tpr")))
    (apply-stylesheet *pp-stylesheet* msx nil tpr))
  (mglue article-path)
  (accom article-path)
  (analyzer article-path))

;;; reservations.lisp ends here
