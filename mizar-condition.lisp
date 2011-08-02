;;; mizar-condition.lisp -- Conditions signaled by mizar tools

(in-package :mizar)

(defun report-mizar-error (mizar-error stream)
  (with-slots (tool working-directory argument output-stream error-stream exit-code)
      mizar-error
    (if tool
	(if working-directory
	    (if argument
		(format stream "Error applying ~a to ~a in directory ~a" tool argument working-directory)
		(format stream "Error applying ~a in directory ~a" tool working-directory))
	    (if argument
		(format stream "Error applying~%~%  ~a~%~%to~%~%  ~a~%~%(No working directory was supplied.)" tool argument)
		(format stream "Error applying ~a (strangely, neither a working directory nor an argument to the program was supplied)" tool)))
	(if working-directory
	    (if argument
		(format stream "Weird mizar error: no mizar tool was specified, but the work (?) was carried out in directory ~a, and the argument to the missing tool was ~a" working-directory argument)
		(format stream "Weird mizar error: no mizar tool was specified, but the work (?) was carried out in directory ~a" working-directory))
	    (if argument
		(format stream "Weird mizar error: no mizar tool was specified,  nor was a working directory specified, but an argument was given: ~a" argument)
		(format stream "Weird mizar error: no mizar tool was specified, no argument was given, and no working directory was supplied"))))
    (terpri stream)
    (terpri stream)
    (format stream "The exit code was ~d.~%~%" exit-code)
    (if output-stream
	(format stream "The standard output was:~%~{~a~%~}" (stream-lines output-stream))
	(format stream "(Standard output was not recorded.)~%"))
    (if error-stream
	(format stream "The standard error was:~%~{~a~%~}" (stream-lines error-stream))
	(format stream "(Standard error was not recorded.)"))
    (terpri stream)
    (terpri stream)
    (let* ((arg-basename (pathname-name (pathname argument)))
	   (err-basename (format nil "~a.err" arg-basename))
	  (err-file
	   (if working-directory
	       (file-in-directory working-directory err-basename)
	       (when (pathnamep argument)
		 (merge-pathnames err-basename
				  (directory-namestring argument))))))
      (if (file-exists-p err-file)
	  (if (zerop (file-size err-file))
	      (format stream "(The error file exists, but is empty.)")
	      (format stream "Here are the lines of the error file:~%~%~{~a~%~}" (lines-of-file err-file)))
	  (format stream "We expected to find an error file at ~a, but somehow there isn't one." err-file)))))

(defun report-mizar-error-as-string (mizar-error)
  (with-output-to-string (str)
    (report-mizar-error mizar-error str)))

(define-condition mizar-error (error)
  ((tool :initarg :tool :accessor tool)
   (working-directory :initarg :working-directory :accessor working-directory)
   (argument :initarg :argument :accessor argument)
   (exit-code :initarg :exit-code :accessor exit-code)
   (output-stream :initarg :output-stream :accessor output-stream)
   (error-stream :initarg :error-stream :accessor error-stream))
  (:report report-mizar-error)
  (:documentation "An error indicating that the application of a tool of the mizar suite (e.g., makeenv, verifier, envget) did not exit cleanly."))

(defun mizar-error-lines (mizar-error)
  (with-slots (working-directory argument)
      mizar-error
    (let ((err-file (if working-directory
			(file-in-directory working-directory
					   (format nil "~a.err" argument))
			(err-file-for-article argument))))
      (when (file-exists-p err-file)
	(lines-of-file err-file)))))

(defun innocent-accomodator-errorp (mizar-error)
  (flet ((830-error (err-line)
	   (destructuring-bind (line column err-code)
	       (split #\Space err-line)
	     (declare (ignore line column))
	     (string= err-code "830"))))
    (every #'830-error (mizar-error-lines mizar-error))))

(defun only-*4-errors (mizar-error)
  (flet ((4-error (err-line)
	   (destructuring-bind (line column err-code)
	       (split #\Space err-line)
	     (declare (ignore line column))
	     (string= err-code "4"))))
    (every #'4-error (mizar-error-lines mizar-error))))

;;; mizar-condition.lisp ends here