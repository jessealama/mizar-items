;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

(defparameter *split-stylesheet* (path-for-stylesheet "split"))
(defparameter *itemize-stylesheet* (path-for-stylesheet "itemize"))
(defparameter *extend-evl-stylesheet* (path-for-stylesheet "extend-evl"))
(defparameter *wsm-stylesheet* (path-for-stylesheet "wsm"))

(defparameter *dom-builder* (cxml-dom:make-dom-builder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Itemization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric itemize (article)
  (:documentation "Break up ARTICLE into its constituent items."))

(defmethod itemize :around ((article-path pathname))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "There is no file at '~a'." (namestring article-path))))

(defmethod itemize ((article-path pathname))
  (itemize (make-instance 'article
			  :path article-path)))

(defmethod itemize :around ((article article))
  (let* ((path (path article))
	 (directory (pathname (directory-namestring path)))
	 (file (pathname-name path))
	 (itemization-directory (pathname-as-directory (merge-pathnames file directory))))
    (if (file-exists-p itemization-directory)
	(error "The itemization directory~%~%  ~a~%~%already exists." (namestring itemization-directory))
	(call-next-method))))

(defmethod itemize ((article article))
  (let* ((path (path article))
	 (directory (pathname (directory-namestring path)))
	 (file (pathname-name path))
	 (itemization-directory (pathname-as-directory (merge-pathnames file directory))))
    (ensure-directories-exist itemization-directory)
    (let* ((db (make-instance 'local-db
			      :location itemization-directory))
	   (article-in-db (copy-to-db article db))
	   (split-wsx (file-with-extension article-in-db "wsx.split"))
	   (itemized-wsx (file-with-extension article-in-db "wsx.split.itemized"))
	   (text-subdir (text-subdirectory db)))
      (apply-stylesheet *split-stylesheet* article-in-db nil split-wsx)
      (apply-stylesheet *itemize-stylesheet* split-wsx nil itemized-wsx)
      (let* ((itemized-xml-doc (cxml:parse-file itemized-wsx *dom-builder*))
	     (text-proper-nodes (remove-if #'dom:text-node-p
					   (dom:child-nodes
					    (dom:document-element itemized-xml-doc))))
	     (num-fragments (length text-proper-nodes)))
	;; make the .wsx files for the fragments
	(loop
	   for text-proper-node across text-proper-nodes
	   for i from 1 upto num-fragments
	   for ckb-wsx = (merge-pathnames (format nil "ckb~d.wsx" i) text-subdir)
	   for wsx-doc = (cxml-dom:create-document text-proper-node)
	   do
	     (with-open-file (ckb ckb-wsx
				  :direction :output
				  :element-type '(unsigned-byte 8)
				  :if-exists :error)
	       (dom:map-document (cxml:make-octet-stream-sink ckb) wsx-doc)))

	;; make the .tpr files for the fragments

	(loop
	   for i from 1 upto num-fragments
	   for ckb-wsx = (merge-pathnames (format nil "ckb~d.wsx" i) text-subdir)
	   for ckb-tpr = (merge-pathnames (format nil "ckb~d.tpr" i) text-subdir)
	   do
	     (apply-stylesheet *wsm-stylesheet* ckb-wsx nil ckb-tpr))))))

;;; itemize.lisp ends here
