
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Running programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-program (program args &key search input output error wait if-output-exists)
  #+sbcl
  (sb-ext:run-program program
		      args
		      :search t
		      :input nil
		      :output nil
		      :error nil
		      :wait wait
		      :if-output-exists if-output-exists)
  #+ccl
  (ccl:run-program program
		   args
		   :input input
		   :output output
		   :error error
		   :wait wait
		   :if-output-exists if-output-exists))

(defun process-exit-code (process)
  #+sbcl
  (sb-ext:process-exit-code process)
  #+ccl
  (multiple-value-bind (status exit-code)
      (ccl:external-process-status process)
    (declare (ignore status))
    exit-code))

(defun process-output (process)
  #+sbcl
  (sb-ext:process-output process)
  #+ccl
  (ccl:external-process-output-stream process))

(defun process-error (process)
  #+sbcl
  (sb-ext:process-error process)
  #+ccl
  (ccl:external-process-error-stream process))

(defgeneric run-in-directory (program working-directory args))

(defmethod run-in-directory (program (working-directory sandbox) args)
  (run-in-directory program (location working-directory) args))

(defmethod run-in-directory :around ((program string) (working-directory pathname) (args list))
  (when (string= program "")
    (error "The empty string is not the name of a program!"))
  (unless (file-exists-p working-directory)
    (error "The supplied working directory, '~a', doesn't exist!" working-directory))
  (if (every #'non-empty-stringp args)
      (call-next-method)
      (error "The list of arguments is supposed to consist entirely of non-empty strings!")))

(defmethod run-in-directory ((program string) (working-directory null) (args list))
  (run-program program
	       args
	       :wait t
	       :search t
	       :input nil
	       :output nil
	       :error nil))

(defmethod run-in-directory ((program string) (working-directory pathname) (args list))
  (let ((dir-as-string (directory-namestring
			(pathname-as-directory working-directory))))
    (run-program (mizar-items-config 'exec-in-dir-script-path)
		 (append (list dir-as-string program) args)
		 :search t
		 :wait t
		 :input nil
		 :output nil
		 :error nil)))

(defmethod run-in-directory (program (working-directory string) args)
  (run-in-directory program (pathname working-directory) args))
