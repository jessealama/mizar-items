;;; mizar-condition.lisp -- Conditions signaled by mizar tools

(in-package :mizar)

(defun mizar-message-file ()
  (pathname (format nil "~a/mizar.msg"
		    #+ccl
		    (ccl:getenv "MIZFILES")
		    #+sbcl
		    (sb-posix:getenv "MIZFILES")
		    #+(and (not ccl) (not sbcl))
		    "")))

(let ((error-table (make-hash-table :test #'eql)))
  (defun explain-error (err-code)
    (multiple-value-bind (known-value found?)
	(gethash err-code error-table)
      (if found?
	  known-value
	  (setf (gethash err-code error-table)
		(let ((message-file (mizar-message-file)))
		  (if (file-exists-p message-file)
		      (let (explanation)
			(with-open-file (messages message-file
						  :direction :input
						  :if-does-not-exist :error)
			  (loop
			     with pattern = (format nil "^# ~d$" err-code)
			     for line = (read-line messages nil :eof)
			     do
			       (cond ((eq line :eof) (return))
				     ((null line) (return))
				     ((scan pattern line)
				      (let ((explanation-line (read-line messages nil :eof)))
					(cond ((eq explanation-line :eof) (return))
					      ((null explanation-line) (return))
					      (t
					       (setf explanation explanation-line)
					       (return))))))))
			explanation)
		      (error "The mmizar error message file does not exist at the expected location~%~%  ~a~%" (namestring message-file))))))))
  (defun error-table ()
    error-table))

(defun explain-err-file (err-file-path)
  (if (file-exists-p err-file-path)
      (loop
	 with explanations = nil
	 for err-line in (lines-of-file err-file-path)
	 do
	   (register-groups-bind (line-str col-str err-code-str)
	       ("^([0-9]+) ([0-9]+) ([0-9]+)$" err-line)
	     (let ((line (parse-integer line-str))
		   (col (parse-integer col-str))
		   (err-code (parse-integer err-code-str)))
	       (let ((explanation (explain-error err-code)))
		 (push (list line col err-code (or explanation
						   "(No explanation available.)"))
		       explanations))))
	 finally
	   (return (reverse explanations)))
      (error "There is no error file at the supplied location '~a'" err-file-path)))

(defun report-mizar-error (mizar-error stream)
  (with-slots (tool working-directory argument output-stream error-stream exit-code)
      mizar-error
    (if tool
	(if working-directory
	    (if argument
		(format stream "Error applying ~a to~%~%  ~a~%~%in directory~%~%  ~a" tool argument working-directory)
		(format stream "Error applying ~a in directory~%~%  ~a" tool working-directory))
	    (if argument
		(format stream "Error applying ~a to~%~%  ~a~%~%(No working directory was supplied.)" tool argument)
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
	      (progn
		(format stream "Here are the lines of the error file:~%~%")
		(loop
		   for (line col err-code explanation) in (explain-err-file err-file)
		   do
		     (format stream "* Line ~d column ~d: ~a~%" line col (format nil "~a" explanation))
;                                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;
; This looks redundant, but it ensures that we get a new string object
; containing EXPLANATION. Because we are memoizing EXPLAIN-ERR, there
; might be identical string objects occurring in the value of
; EXPLANATION.  In this case, some Lisps are clever and indicate that
; there is actually only one object occuring multiple times, thus:
;
;   * Line 1 column 1: #1 = Unknown private predicate
;   * Line 2 column 1: #1
;   * Line 3 column 1: #1
;
; Calling FORMAT here cirumvents such cleverness.
		   finally
		     (terpri stream))))
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