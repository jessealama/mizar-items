;;; mizar.lisp Interface to the mizar tools and the MML

(in-package :mizar)

(define-constant +mizar-system-version+
    "7.13.01"
  :test #'string=
  :documentation "The system version of the Mizar that we currently support.")

(define-constant +mml-version+
    "4.181.1147"
  :test #'string=
  :documentation "The MML version that we support.")

(define-constant +mizar-version+
    (format nil "~a-~a" +mizar-system-version+ +mml-version+)
  :test #'string=
  :documentation "The system number and MML version that we use, in that order, separated by a dash.")

(define-constant +mizar-release-root-dir+
    (pathname "/Users/alama/sources/mizar/release/")
  :test #'equal
  :documentation "The directory under which we look for Mizar installations.")

(defun path-for-tool (tool &optional (mizar-version +mizar-version+))
  (let ((directory-for-version (merge-pathnames (format nil "~a/" mizar-version)
						+mizar-release-root-dir+)))
    (if (directory-exists-p directory-for-version)
	(let ((bin-subdir (merge-pathnames "bin/" directory-for-version)))
	  (if (directory-exists-p bin-subdir)
	      (merge-pathnames tool bin-subdir)
	      (error "The bin subdirectory~%~%  ~a~%~%of~%~%  ~a~%~%does not exist." bin-subdir directory-for-version)))
	(error "The release directory ~a doest not exist." directory-for-version))))

(defun mizfiles (&optional (mizar-system-version +mizar-system-version+)
		           (mml-version +mml-version+))
  (let ((full-system-name (format nil "~a-~a" mizar-system-version mml-version)))
    (pathname (format nil "~a~a/"
		      (namestring +mizar-release-root-dir+)
		      full-system-name))))

(defgeneric run-mizar-tool (tool flags article))

(defmethod run-mizar-tool :around (tool flags article)
  (declare (ignore flags article))
  (let ((tool-path (path-for-tool tool)))
    (if (file-exists-p tool-path)
	(call-next-method)
	(error "We cannot find ~a at its expected location~%~%  ~a~%" tool tool-path))))

(defmethod run-mizar-tool (tool flags (article pathname))
  #+ccl
  (let ((tool-path (path-for-tool tool)))
    (let ((proc (ccl:run-program tool-path
				 (append flags (list (namestring article)))
				 :wait t
				 :input nil
				 :output nil
				 :error nil)))
      (multiple-value-bind (status exit-code)
	  (ccl:external-process-status proc)
	(declare (ignore status))
	(values (and (numberp exit-code)
		     (zerop exit-code))
		(when (and (numberp exit-code)
			   (not (zerop exit-code)))
		  (or (not (file-exists-p (err-file article)))
		      (empty-err-file? article)))))))
  #+sbcl
  (let* ((err-path (file-with-extension article "err"))
	 (tool-path (path-for-tool tool))
	 (proc (sb-ext:run-program tool-path
				   (append flags
					   (list (native-namestring article)))
				   :environment (list (format nil "MIZFILES=~a" (namestring (mizfiles))))
				   :search nil
				   :wait t
				   :input nil
				   :output nil
				   :error nil)))
    (let ((exit-code (sb-ext:process-exit-code proc)))
      (values (and (numberp exit-code)
		   (zerop exit-code))
	      (and (file-exists-p err-path)
		   (empty-err-file? article)))))
  #-(or ccl sbcl)
  (error "We don't handle your Common Lisp.  Sorry."))

(defmethod run-mizar-tool (tool flags (article article))
  #+ccl
  (let ((path (miz-file article))
	 (tool-path (path-for-tool tool)))
    (let ((proc (ccl:run-program tool-path
				 (append flags (list (namestring path)))
				 :wait t
				 :input nil
				 :output nil
				 :error nil)))
      (multiple-value-bind (status exit-code)
	  (ccl:external-process-status proc)
	(declare (ignore status))
	(values (and (numberp exit-code)
		     (zerop exit-code))
		(when (and (numberp exit-code)
			   (not (zerop exit-code)))
		  (or (not (file-exists-p (err-file article)))
		      (empty-err-file? article)))))))
  #+sbcl
  (let* ((path (miz-file article))
	 (err-path (file-with-extension article "err"))
	 (tool-path (path-for-tool tool))
	 (proc (sb-ext:run-program tool-path
				   (append flags (list (namestring path)))
				   :environment (list (format nil "MIZFILES=~a" (namestring (mizfiles))))
				   :search nil
				   :wait t
				   :input nil
				   :output nil
				   :error nil)))
    (let ((exit-code (sb-ext:process-exit-code proc)))
      (values (and (numberp exit-code)
		   (zerop exit-code))
	      (and (file-exists-p err-path)
		   (empty-err-file? article)))))
  #-(or ccl sbcl)
  (error "We don't handle your Common Lisp.  Sorry."))

(defmacro run-mizar-tool-with-standard-flags (tool article)
  `(run-mizar-tool ,tool '("-q" "-s" "-l") ,article))

(defgeneric makeenv (article))
(defgeneric accom (article))
(defgeneric wsmparser (article))
(defgeneric msmprocessor (article))
(defgeneric msplit (article))
(defgeneric mglue (article))
(defgeneric analyzer (article))
(defgeneric verifier (article))
(defgeneric exporter (article))
(defgeneric transfer (article))

(defmethod accom ((article article))
  (accom (path article)))

(defmethod accom ((article-path pathname))
  (run-mizar-tool-with-standard-flags "accom" article-path))

(defmethod makeenv ((article article))
  (makeenv (path article)))

(defmethod makeenv ((article pathname))
  (run-mizar-tool-with-standard-flags "makeenv" article))

(defmethod wsmparser ((article article))
  (wsmparser (path article)))

(defmethod wsmparser ((article pathname))
  (run-mizar-tool-with-standard-flags "wsmparser" article))

(defmethod msmprocessor ((article article))
  (msmprocessor (path article)))

(defmethod msmprocessor ((article pathname))
  (run-mizar-tool-with-standard-flags "msmprocessor" article))

(defmethod msplit ((article article))
  (msplit (path article)))

(defmethod msplit ((article pathname))
  (run-mizar-tool-with-standard-flags "msplit" article))

(defmethod mglue ((article article))
  (mglue (path article)))

(defmethod mglue ((article-path pathname))
  (run-mizar-tool-with-standard-flags "mglue" article-path))

(defgeneric verifiable (article))

(defmethod verifiable ((article article))
  (verifier (path article)))

(defmethod verifiable ((article-path pathname))
  (run-mizar-tool-with-standard-flags "verifier" article-path))

(defmethod verifier ((article article))
  (verifier (path article)))

(defmethod verifier ((article-path pathname))
  (run-mizar-tool-with-standard-flags "verifier" article-path))

(defmethod analyzer ((article article))
  (run-mizar-tool "verifier" '("-a" "-q" "-s" "-l") article))

(defmethod analyzer ((article-path pathname))
  (run-mizar-tool "verifier" '("-a" "-q" "-s" "-l") article-path))

(defmethod exporter ((article article))
  (exporter (path article)))

(defmethod exporter ((article-path pathname))
  (run-mizar-tool-with-standard-flags "exporter" article-path))

(defmethod transfer ((article article))
  (transfer (path article)))

(defmethod transfer ((article-path pathname))
  (run-mizar-tool-with-standard-flags "transfer" article-path))

;;; mizar.lisp ends here
