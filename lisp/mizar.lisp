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

(defmacro with-mizfiles (&body body)
  #+ccl
  (let ((old-mizfiles (gensym)))
    `(let ((,old-mizfiles (ccl:getenv "MIZFILES")))
       (ccl:setenv "MIZFILES" (namestring (mizfiles)))
       (let ((vals (multiple-value-list (progn ,@body))))
	 (ccl:setenv "MIZFILES" ,old-mizfiles)
	 (apply #'values vals)))))

(defmethod run-mizar-tool (tool flags (article pathname))
  #+ccl
  (let ((tool-path (path-for-tool tool)))
    (with-mizfiles
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
			  (empty-err-file? article))))))))
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

(defmethod run-mizar-tool (tool flags (article article))
  #+ccl
  (let ((path (miz-file article))
	 (tool-path (path-for-tool tool)))
    (with-mizfiles
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
			  (empty-err-file? article))))))))
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
(defgeneric verifier (article))
(defgeneric exporter (article))
(defgeneric transfer (article))

(defmethod accom ((article article))
  (run-mizar-tool-with-standard-flags "accom" article))

(defmethod accom ((article-path pathname))
  (run-mizar-tool-with-standard-flags "accom" article-path))

(defmethod makeenv ((article article))
  (run-mizar-tool-with-standard-flags "makeenv" article))

(defmethod wsmparser ((article article))
  (run-mizar-tool-with-standard-flags "wsmparser" article))

(defmethod msmprocessor ((article article))
  (run-mizar-tool-with-standard-flags "msmprocessor" article))

(defmethod msplit ((article article))
  (run-mizar-tool-with-standard-flags "msplit" article))

(defmethod mglue ((article article))
  (run-mizar-tool-with-standard-flags "mglue" article))

(defmethod mglue ((article-path pathname))
  (run-mizar-tool-with-standard-flags "mglue" article-path))

(defmethod verifier ((article article))
  (run-mizar-tool-with-standard-flags "verifier" article))

(defmethod verifier ((article-path pathname))
  (run-mizar-tool-with-standard-flags "verifier" article-path))

(defmethod exporter ((article article))
  (run-mizar-tool-with-standard-flags "exporter" article))

(defmethod exporter ((article-path pathname))
  (run-mizar-tool-with-standard-flags "exporter" article-path))

(defmethod transfer ((article article))
  (run-mizar-tool-with-standard-flags "transfer" article))

(defmethod transfer ((article-path pathname))
  (run-mizar-tool-with-standard-flags "transfer" article-path))

;;; mizar.lisp ends here
