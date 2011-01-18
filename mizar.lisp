;;; mizar.lisp Interface to the mizar tools and the MML

(in-package :mizar)

(defparameter *mizfiles* 
  (ensure-directory (sb-ext:posix-getenv "MIZFILES"))
  "The directory that contains the MML and the mml.lar file.

The default value is the value of the MIZFILES environment
variable (at load time).")

(defclass mizar-library ()
  ((location
    :initarg :location
    :accessor location
    :type string
    :initform *mizfiles*)
   (mml-lar
    :accessor mml-lar
    :type list))
  (:documentation "A wrapper around a copy of the MML."))

(defmethod initialize-instance :after ((lib mizar-library) &key)
  (setf (mml-lar lib) 
	(lines-of-file (concat *mizfiles* "mml.lar"))))

(defmethod print-object ((lib mizar-library) stream)
  (with-slots (location mml-lar)
      lib
    (print-unreadable-object (lib stream :type nil)
      (format stream "location ~A, with ~d articles" location (length mml-lar)))))

(defparameter *default-mizar-library* (make-instance 'mizar-library)
  "A Mizar library using the value of MIZFILES in the environment.")

(defun article-exists-p (name &optional (library *default-mizar-library*))
  (member name (mml-lar library) :test #'string=))

(defparameter *mml-lar-path* (make-pathname :directory *mizfiles*
					    :name "mml.lar"))
(defparameter *mml-lar*
  (if (probe-file *mml-lar-path*)
      (lines-of-file *mml-lar-path*)
      (error "Unable to initialize mml.lar: file does not exist under ~A" *mml-lar-path*)))

(defun belongs-to-mml (article-str)
  (let ((article-str-lc (lowercase article-str)))
    (or (string= article-str-lc "tarski")
	(member article-str-lc *mml-lar* :test #'string=))))

(defun run-in-directory (program directory args &key input output if-output-exists)
  (let ((real-dir-name (file-exists-p directory)))
    (if real-dir-name
	(if (directory-p real-dir-name)
	    (let ((dir-as-string (directory-namestring (pathname-as-directory real-dir-name))))
	      (sb-ext:run-program "exec-in-dir.sh" 
				  (append (list dir-as-string program) args)
				  :search t
				  :input input
				  :output output
				  :if-output-exists if-output-exists))
	    (error "No such directory ~A" directory))
	(error "No file under the path '~A'" directory))))

(defmacro define-file-transformer (name program &rest arguments)
  ; check that TOOL is real
  (let* ((check (sb-ext:run-program "which" (list program) 
				    :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	`(progn
	   (defgeneric ,name (file &optional directory))
	   (defmethod ,name ((miz-path pathname) &optional (directory (sb-posix:getcwd)))
	     (let* ((tmp-path (replace-extension miz-path "miz" "splork"))
		    (proc (run-in-directory ,program
					    directory
					    (append ',arguments (list (namestring miz-path)))
					    :output tmp-path
					    :if-output-exists :supersede)))
	       (if (zerop (sb-ext:process-exit-code proc))
		   (rename-file tmp-path miz-path)
		   (error "Something went wrong when calling '~A' with arguments ~A; the process exited with code ~S" ,program ',arguments (sb-ext:process-exit-code proc)))))
	     (defmethod ,name ((article-path string) &optional (directory (sb-posix:getcwd)))
	       (,name (pathname article-path) directory))
	     (defmethod ,name ((article article) &optional (directory (sb-posix:getcwd)))
	       (,name (path article) directory)
	       (refresh-text article)))
	(error "The program ~S could not be found in your path (or it is not executable)" program))))

(defmacro define-input-transformer (name program &rest arguments)
  ; check that TOOL is real
  (let ((check (sb-ext:run-program "which" (list program) :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	`(progn
	   (defgeneric ,name (file &optional directory))
	   (defmethod ,name ((miz-path pathname) &optional (directory (sb-posix:getcwd)))
	     (let* ((tmp-path (replace-extension miz-path "miz" "splork"))
		    (proc (run-in-directory ,program
					    directory
					    ',arguments 
					    :input miz-path
					    :output tmp-path
					    :if-output-exists :supersede)))
	       (if (zerop (sb-ext:process-exit-code proc))
		   (rename-file tmp-path miz-path)
		   (error "Something went wrong when calling '~A' with arguments ~A; the process exited with code ~S" ,program ',arguments (sb-ext:process-exit-code proc)))))
	     (defmethod ,name ((article-path string) &optional (directory (sb-posix:getcwd)))
	       (,name (pathname article-path) directory))
	     (defmethod ,name ((article article) &optional (directory (sb-posix:getcwd)))
	       (,name (path article) directory)
	       (refresh-text article)))
	(error "The program ~S could not be found in your path (or it is not executable)" program))))

(define-file-transformer strip-comments "sed" "-e" "s/::.*$//")
(define-file-transformer fix-by-and-from "/Users/alama/sources/mizar/mizar-items/fix-by-and-from.sh")
(define-input-transformer squeeze-repeated-newlines "tr" "-s" "\\n")
(define-input-transformer squeeze-repeated-spaces "tr" "-s" "[:space:]")

(define-condition mizar-error (error)
  ((tool :initarg :tool :accessor tool)
   (working-directory :initarg :working-directory :accessor working-directory)
   (argument :initarg :argument :accessor argument)))

(defgeneric run-mizar-tool (tool article directory ignore-exit-code &rest flags))

(defmethod run-mizar-tool :around (tool article-path directory ignore-exit-code &rest flags)
  (declare (ignore tool directory flags))
  (if (probe-file article-path)
      (call-next-method)
      (error "No such file: ~S" article-path)))

(defmethod run-mizar-tool :around (tool article-path directory ignore-exit-code &rest flags)
  (declare (ignore tool article-path flags))
  (let ((real-name (file-exists-p directory)))
    (if real-name
	(if (directory-p real-name)
	    (call-next-method)
	    (error "The specified directory, ~A, in which to apply the mizar tool ~A is not a directory!" directory tool))
	(error "It appears that there is no file at the specified work directory path ~A" directory))))

(defmethod run-mizar-tool ((tool string) (article-path pathname) directory ignore-exit-code &rest flags)
  (let ((name (namestring article-path))
	(err-filename (replace-extension article-path "miz" "err")))
    (let ((proc (run-in-directory tool directory (append flags (list name)))))
      (unless ignore-exit-code
	(if (zerop (sb-ext:process-exit-code proc))
	    (if (and (probe-file err-filename)
		     (not (zerop (file-size err-filename))))
		(error 'mizar-error :tool tool :working-directory directory :argument article-path)
		t)
	    (error 'mizar-error :tool tool :working-directory directory :argument article-path))))))

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
	     (defmethod ,tool-as-symbol ((article-path pathname) directory &rest flags)
	       (apply 'run-mizar-tool ,tool article-path directory flags))
	     (defmethod ,tool-as-symbol ((article-path string) directory &rest flags)
	       (apply 'run-mizar-tool ,tool article-path directory flags))
	     (defmethod ,tool-as-symbol ((article article) directory &rest flags)
	       (apply 'run-mizar-tool ,tool article directory flags)
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

;;; absrefs

(defparameter *xsl4mizar-root* 
  (ensure-directories-exist "/Users/alama/sources/mizar/xsl4mizar"))

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
      (let ((proc (sb-ext:run-program "listvoc.sh"
				      (list article-name)
				      :search t
				      :output :stream)))
	(let ((exit-code (sb-ext:process-exit-code proc)))
	  (if (zerop exit-code)
	      (stream-lines (sb-ext:process-output proc))
	      (error "Something went wrong running listvoc.sh: the exit code was ~d" exit-code))))))

;;; mizar.lisp ends here