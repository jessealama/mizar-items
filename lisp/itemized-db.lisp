
(in-package :mizar)

(define-constant +fragment-base+
    "ckb"
  :test #'string=
  :documentation "The fixed prefix (in lowercase) that all article fragments have.")

(defclass itemized-db (local-db)
  ((main-article
   :accessor main-article
   :type article
   :initform (error "To construct an itemized local database, an article object is needed.")
   :initarg :article)))

(defmethod print-object ((db itemized-db) stream)
  (let* ((location (location db))
	 (article-wildcard "*.miz")
	 (articles (directory article-wildcard))
	 (vocabularies (vocabulary-files db))
	 (article (main-article db))
	 (article-name (pathname-name (path article))))
    (print-unreadable-object (db stream :type t :identity nil)
      (format stream "~@(~a~) [~a] (~d vocabulary files, ~d fragments)"
	      article-name
	      (namestring location)
	      (length vocabularies)
	      (length articles)))))

(defmethod minimize ((db itemized-db))
  (let ((location (location db))
	(articles (article-paths db)))
    #+ccl
    (ccl::with-preserved-working-directory (location)
      (dolist (article articles)
	;; (format t "Looking at article ~a~%" (pathname-name article))
	(minimize article)))))

(defun minimize-itemized-db (dirname)
  (let* ((ensure-directory (pathname-as-directory dirname))
	 (miz-files (directory (format nil "~a*.miz" (namestring ensure-directory)))))
    (cond ((null miz-files)
	   (error "There appear to be no .miz files in the directory~%~%  ~a" (namestring ensure-directory)))
	  ((rest miz-files)
	   (error "There are multiple .miz files in the directory~%~%  ~a" (namestring ensure-directory)))
	  (t
	   (let ((article (make-instance 'article
					 :path (first miz-files))))
	     (let ((db (make-instance 'itemized-db
				      :article article
				      :location ensure-directory)))
	       (minimize db)))))))
