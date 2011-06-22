;;; mizar.lisp Interface to the mizar tools and the MML

(in-package :mizar)

(defvar *mizfiles* 
  (ensure-directory (sb-ext:posix-getenv "MIZFILES"))
  "The directory that contains the MML and the mml.lar file.

The default value is the value of the MIZFILES environment
variable (at load time).")

(define-constant +needed-mizfiles-files+
    '("mml.lar" "mml.ini" "mizar.dct" "mizar.msg" "mml.vct")
  :test #'equalp
  :documentation "The list of files that are needed in a proper $MIZFILES.")

(define-constant +needed-mizfiles-subdirs+
    '("prel" "mml")
  :test #'equalp
  :documentation "The list of subdirectories that need to be present in a proper $MIZFILES.")

(defun set-mizfiles (new-mizfiles)
  "Set *MIZFILES* to NEW-MIZFILES, provided NEW-MIZFILES is a suitable
  for that, viz., NEW-MIZFILES is the name of a directory that
  contains the files

- mml.lar
- mml.ini
- mizar.dct
- mizar.msg
- mml.vct

and subdirectories

- prel
- mml

The presence of these files is, in general, necessary for the mizar
suite to work correctly."
  (let ((dir (ensure-directory new-mizfiles)))
    (flet ((file-ok-in-proposed-mizfiles (file)
	     (file-exists-p (file-in-directory dir file)))
	   (directory-ok (some-dir)
	     (file-exists-p (ensure-directory
			     (file-in-directory dir some-dir)))))
      (multiple-value-bind (all-files-ok bad-file)
	  (every-with-falsifying-witness +needed-mizfiles-files+
					 #'file-ok-in-proposed-mizfiles)
	(if all-files-ok
	    (multiple-value-bind (all-dirs-ok bad-dir)
		(every-with-falsifying-witness +needed-mizfiles-subdirs+
					       #'directory-ok)
	      (if all-dirs-ok
		  (setf *mizfiles* dir)
		  (error "Unable to set $MIZFILES to '~a' because the needed directory '~a' is missing" new-mizfiles bad-dir)))
	    (error "Unable to set $MIZFILES to '~a' because the needed file '~a' does not exist there" new-mizfiles bad-file))))))

(defgeneric belongs-to-mml (article))

(defmethod belongs-to-mml ((article-str string))
  (let ((article-str-lc (lowercase article-str)))
    (member article-str-lc *mml-lar*
	    :test #'string=
	    :key #'name)))

(defmethod belongs-to-mml ((article article))
  (member article *mml-lar*))

(let ((table (make-hash-table :test #'equal)))
  (defun mml-lar-index (article)
    (multiple-value-bind (position present?)
	(gethash article table)
      (if present?
	  position
	  (setf (gethash article table)
		(position article *mml-lar* :test #'string=))))))

(defun mml-< (article-1 article-2)
  (< (mml-lar-index article-1)
     (mml-lar-index article-2)))

(defgeneric run-in-directory (program directory args &key input output if-output-exists))

(defmethod run-in-directory (program (sandbox sandbox) args &key input output if-output-exists)
  (run-in-directory program (location sandbox) args
		    :input input
		    :output output
		    :if-output-exists if-output-exists))

(defmethod run-in-directory (program (directory pathname) args &key input output if-output-exists)
  (let ((real-dir-name (file-exists-p directory)))
    (if real-dir-name
	(if (directory-p real-dir-name)
	    (let ((dir-as-string (directory-namestring (pathname-as-directory real-dir-name))))
	      (sb-ext:run-program (mizar-items-config 'exec-in-dir-script-path)
				  (append (list dir-as-string program) args)
				  :search t
				  :input input
				  :output output
				  :if-output-exists if-output-exists))
	    (error "No such directory ~A" directory))
	(error "No file under the path '~A'" directory))))

(defmacro define-file-transformer (name program &rest arguments)
  ; check that TOOL is real
  (let* ((eval-program (eval program))
	 (check (sb-ext:run-program "which" (list eval-program) :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	`(progn
	   (defgeneric ,name (file &optional directory))
	   (defmethod ,name ((miz-path pathname) &optional (directory (sb-posix:getcwd)))
	     (let* ((tmp-path (replace-extension miz-path "miz" "splork"))
		    (proc (run-in-directory ,eval-program
					    directory
					    (append ',arguments (list (namestring miz-path)))
					    :output tmp-path
					    :if-output-exists :supersede)))
	       (if (zerop (sb-ext:process-exit-code proc))
		   (rename-file tmp-path miz-path)
		   (error "Something went wrong when calling '~A' with arguments ~A; the process exited with code ~S" ,eval-program ',arguments (sb-ext:process-exit-code proc)))))
	     (defmethod ,name ((article-path string) &optional (directory (sb-posix:getcwd)))
	       (,name (pathname article-path) directory))
	     (defmethod ,name ((article article) &optional (directory (sb-posix:getcwd)))
	       (,name (path article) directory)
	       (refresh-text article)))
	(error "The program ~S could not be found in your path (or it is not executable)" eval-program))))

(defmacro define-input-transformer (name program &rest arguments)
  ; check that TOOL is real
  (let* ((eval-program (eval program))
	 (check (sb-ext:run-program "which" (list eval-program) :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	`(progn
	   (defgeneric ,name (file &optional directory))
	   (defmethod ,name ((miz-path pathname) &optional (directory (sb-posix:getcwd)))
	     (let* ((tmp-path (replace-extension miz-path "miz" "splork"))
		    (proc (run-in-directory ,eval-program
					    directory
					    ',arguments 
					    :input miz-path
					    :output tmp-path
					    :if-output-exists :supersede)))
	       (if (zerop (sb-ext:process-exit-code proc))
		   (rename-file tmp-path miz-path)
		   (error "Something went wrong when calling '~A' with arguments ~A; the process exited with code ~S" ,eval-program ',arguments (sb-ext:process-exit-code proc)))))
	     (defmethod ,name ((article-path string) &optional (directory (sb-posix:getcwd)))
	       (,name (pathname article-path) directory))
	     (defmethod ,name ((article article) &optional (directory (sb-posix:getcwd)))
	       (,name (path article) directory)
	       (refresh-text article)))
	(error "The program ~S could not be found in your path (or it is not executable)" eval-program))))

(define-file-transformer strip-comments "sed" "-e" "s/::.*$//")
(define-file-transformer fix-by-and-from (mizar-items-config 'fix-by-and-from-script-path))
(define-input-transformer squeeze-repeated-newlines "tr" "-s" "\\n")
(define-input-transformer squeeze-repeated-spaces "tr" "-s" "[:space:]")
(define-input-transformer expand-canceled (mizar-items-config 'expand-canceled-script-path))

(defun report-mizar-error (mizar-error stream)
  (with-slots (tool working-directory argument output-stream error-stream exit-code)
      mizar-error
    (if tool
	(if working-directory
	    (if argument
		(format stream "Error applying ~a to ~a in directory ~a" tool argument working-directory)
		(format stream "Error applying ~a in directory ~a" tool working-directory))
	    (if argument
		(format stream "Error applying ~a to ~a (strangely, no working directory was given" tool argument)
		(format stream "Error applying ~a (strangely, neither a working directory nor an argument to the program was supplied)" tool)))
	(if working-directory
	    (if argument
		(format stream "Weird mizar error: no mizar tool was specified, but the work (?) was carried out in directory ~a, and the argument to the missing tool was ~a" working-directory argument)
		(format stream "Weird mizar error: no mizar tool was specified, but the work (?) was carried out in directory ~a" working-directory))
	    (if argument
		(format stream "Weird mizar error: no mizar tool was specified,  nor was a working directory specified, but an argument was given: ~a" argument)
		(format stream "Weird mizar error: no mizar tool was specified, no argument was given, and no working directory was supplied"))))
    (terpri stream)
    (format stream "The exit code was ~d.~%" exit-code)
    (if output-stream
	(format stream "The standard output was:~%~{~a~%~}" (stream-lines output-stream))
	(format stream "(Somehow there was no output stream.)~%"))
    (if error-stream
	(format stream "The standard error was:~%~{~a~%~}" (stream-lines error-stream))
	(format stream "(Somehow there was no error stream.)"))
    (terpri stream)
    (let ((err-file (file-in-directory working-directory (format nil "~a.err" argument))))
      (if (file-exists-p err-file)
	  (progn
	    (format stream "Here is the contents of the error file (~a):~%" err-file)
	    (format stream "~{~a~%~}" (lines-of-file err-file)))
	  (format stream "We are unable to read the error file at ~a; sorry." err-file)))))

(define-condition mizar-error (error)
  ((tool :initarg :tool :accessor tool)
   (working-directory :initarg :working-directory :accessor working-directory)
   (argument :initarg :argument :accessor argument)
   (exit-code :initarg :exit-code :accessor exit-code)
   (output-stream :initarg :output-stream :accessor output-stream)
   (error-stream :initarg :error-stream :accessor error-stream))
  (:report report-mizar-error)
  (:documentation "An error indicating that the application of a tool of the mizar suite (e.g., makeenv, verifier, envget) did not exit cleanly."))

(defgeneric run-mizar-tool (tool article directory ignore-exit-code &rest flags))

(defmethod run-mizar-tool :around (tool article-path directory ignore-exit-code &rest flags)
  (declare (ignore tool directory flags))
  (if (probe-file article-path)
      (let ((real-name (file-exists-p (if (typep directory 'sandbox)
					  (location directory)
					  directory))))
	(if real-name
	    (if (directory-p real-name)
		(call-next-method)
		(error "The specified directory, ~A, in which to apply the mizar tool ~A is not a directory!" directory tool))
	    (error "It appears that there is no file at the specified work directory path ~A" directory)))
      (error "No such file: ~S" article-path)))

(defmethod run-mizar-tool (tool article (sandbox sandbox) ignore-exit-code &rest flags)
  (apply 'run-mizar-tool tool article (location sandbox) ignore-exit-code flags))

(defmethod run-mizar-tool ((tool string) (article-path pathname) (directory pathname) ignore-exit-code &rest flags)
  (let ((name (namestring article-path)))
    (let ((proc (run-in-directory tool directory (append flags (list name)))))
      (or ignore-exit-code
	  (or (zerop (sb-ext:process-exit-code proc))
	      (error 'mizar-error :tool tool :working-directory directory :argument article-path :output-stream (sb-ext:process-output proc) :error-stream (sb-ext:process-error proc) :exit-code (sb-ext:process-exit-code proc)))))))

(defmethod run-mizar-tool ((tool string) (article-path string) directory ignore-exit-code &rest flags)
  (apply 'run-mizar-tool tool (pathname article-path) directory ignore-exit-code flags))

(defmethod run-mizar-tool ((tool string) (article article) directory ignore-exit-code &rest flags)
  (if (slot-boundp article 'path)
      (apply 'run-mizar-tool tool (path article) directory ignore-exit-code flags)
      (error "Cannot apply ~S to ~S because we don't know its path"
	     tool article)))

(defmethod run-mizar-tool ((tool symbol) article directory ignore-exit-code &rest flags)
  (apply 'run-mizar-tool 
	 (format nil "~(~a~)" (string tool)) ; lowercase: watch out
	 article
	 directory
	 ignore-exit-code
	 flags))

(defmacro define-mizar-tool (tool)
  ; check that TOOL is real
  (let ((check (sb-ext:run-program "which" (list tool) :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	(let ((tool-as-symbol (intern (format nil "~:@(~a~)" tool))))
	  `(progn
	     (defgeneric ,tool-as-symbol (article directory &rest flags))
	     (defmethod ,tool-as-symbol (article (sandbox sandbox) &rest flags)
	       (apply 'run-mizar-tool ,tool article (location sandbox) nil flags))
	     (defmethod ,tool-as-symbol ((article-path pathname) directory &rest flags)
	       (apply 'run-mizar-tool ,tool article-path directory nil flags))
	     (defmethod ,tool-as-symbol ((article-path string) directory &rest flags)
	       (apply 'run-mizar-tool ,tool article-path directory nil flags))
	     (defmethod ,tool-as-symbol ((article article) directory &rest flags)
	       (apply 'run-mizar-tool ,tool article directory nil flags)
	       article)))
	(error "The mizar tool ~S could not be found in your path (or it is not executable)" tool))))

;; workhorses
(define-mizar-tool "edtfile")
(define-mizar-tool "makeenv")
(define-mizar-tool "accom")
(define-mizar-tool "verifier")
(define-mizar-tool "envget")
(define-mizar-tool "exporter")
(define-mizar-tool "transfer")
(define-mizar-tool "irrths")
(define-mizar-tool "irrvoc")

(defmacro define-mizar-text-transformer (tool &optional (ignore-exit-code nil))
  ; check that TOOL is real
  (let ((check (sb-ext:run-program "which" (list tool) :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	(let ((tool-as-symbol (intern (format nil "~:@(~a~)" tool))))
	  `(progn
	     (defgeneric ,tool-as-symbol (article directory &rest flags))
	     (defmethod ,tool-as-symbol ((article-path pathname) directory &rest flags)
	       (let ((edtfile-path (replace-extension article-path
						      "miz" "$-$")))
		 (apply 'run-mizar-tool ,tool article-path directory ,ignore-exit-code flags)
		 (edtfile article-path directory "-l")
		 (rename-file edtfile-path article-path)))
	     (defmethod ,tool-as-symbol ((article-path string) directory &rest flags)
	       (apply ',tool-as-symbol (pathname article-path) directory flags))
	     (defmethod ,tool-as-symbol ((article article) directory &rest flags)
	       (apply ',tool-as-symbol (path article) directory flags)
	       (refresh-text article))))
	(error "The mizar tool ~S could not be found in your path (or it is not executable)" tool))))

;; our text transformers -- thanks, Karol Pąk et al.! 
(define-mizar-text-transformer "JA1")
(define-mizar-text-transformer "unhereby" t)
(define-mizar-text-transformer "dellink")
(define-mizar-text-transformer "CutSet")
(define-mizar-text-transformer "CutReconsider")
(define-mizar-text-transformer "change") ; clever name...
(define-mizar-text-transformer "ref") ; clever name...

;;; absrefs

(defparameter *xsl4mizar-root* 
  (ensure-directories-exist (mizar-items-config 'xsl4mizar-path)))
(defparameter *addabsrefs-stylesheet*
  (probe-file (make-pathname :directory *xsl4mizar-root*
			     :name "addabsrefs.xsl")))

(defgeneric absrefs (article))

(defmethod absrefs ((article-path pathname))
  (let ((article-xml-path (replace-extension article-path "miz" "xml"))
	(new-article-xml-path (replace-extension article-path "miz" "xml1")))
    (if (probe-file article-xml-path)
	(progn
	  (sb-ext:run-program "xsltproc"
			      (list (namestring *addabsrefs-stylesheet*)
				    (namestring article-xml-path)
				    "-o"
				    (namestring new-article-xml-path))
			      :search t)
	  ;; xsltproc returns non-zero error code on most articles,
	  ;; owing to the bex and fex entities generated by the
	  ;; verifier.  For now, just assume it was successful
	  (rename-file new-article-xml-path article-xml-path))
	(error "File does not exist: ~S" article-xml-path))))

(defmethod absrefs ((article-path string))
  (absrefs (pathname article-path)))

(defmethod absrefs ((article article))
  (absrefs (path article))
  (refresh-xml article))

(defun verify-and-export (article &optional directory)
  (accom article directory "-q" "-s" "-l")
  (verifier article directory "-q" "-s" "-l")
  (exporter article directory "-q" "-s" "-l")
  (transfer article directory "-q" "-s" "-l"))

(defun listvoc (article-name)
  (if (string= article-name "HIDDEN") ; can't list symbols in this special vocab file
      nil
      (let ((proc (sb-ext:run-program (mizar-items-config 'listvoc-script-path)
				      (list article-name)
				      :search t
				      :output :stream)))
	(let ((exit-code (sb-ext:process-exit-code proc)))
	  (if (zerop exit-code)
	      (stream-lines (sb-ext:process-output proc))
	      (error "Something went wrong running listvoc.sh: the exit code was ~d" exit-code))))))

;;; mizar.lisp ends here