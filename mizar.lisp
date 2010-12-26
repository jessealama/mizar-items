;;; mizar.lisp Interface to the mizar tools and the MML

(in-package :mizar)

(defparameter *mizfiles* 
  (sb-ext:posix-getenv "MIZFILES")
  "The directory that contains the MML and the mml.lar file.

The default value is the value of the MIZFILES environment
variable (at load time).")

(defclass mizar-library ()
  ((location
    :initarg :location
    :type pathname
    :initform *mizfiles*)
   (mml-lar
    :type list))
  (:documentation "A wrapper around a copy of the MML."))

(defmethod initialize-instance :after ((lib mizar-library) &key)
  (let (lines)
    (with-open-file (mml-lar (concatenate 'string
					  (slot-value lib 'location)
					  "/"
					  "mml.lar"))
      (symbol-macrolet
	  (($line (read mml-lar nil nil)))
	(do ((line $line $line))
	    ((null line))
	  (push line lines))))
    (setf (slot-value lib 'mml-lar) (reverse lines))))

(defclass sandbox ()
  ((articles
    :initarg :articles
    :accessor articles
    :type list
    :initform nil
    :documentation "A list of symbols that name articles that this sandbox is guarding.")
   (location
    :initarg :location
    :reader location
    ;; no writer method -- changing the location is not allowed
    :type pathname
    :documentation "A pathname, which should point to a directory,
     where the contents of the sandbox will be stored."))
  (:documentation "A sandbox is a wrapper around a directory.  It
  stores a list of articles; mizar processing for these articles will
  take place in the directory."))

;; we need to check the validity of the arguments: LOCATION
;; points to a directory, ARTICLE is a list of symbols, and
;; the directory contains files whose names are derived from
;; the list of symbols in ARTICLES
;; (defmethod initialize-instance :after ((s sandbox))
;;   nil) 

(defun fresh-sandbox ()
  (make-instance 'sandbox
		 :location (pathname "/tmp"))) ;; this should obviously not be fixed

(defun verify-in-sandbox (sandbox article)
  "Call the mizar verifier on ARTICLE, which should be a symbol, in
  SANDBOX.  Signals an error if ARTICLE does not actually belong to
  SANDBOX."
  (declare (ignore sandbox article)))

(defun verify-sandbox (sandbox)
  "Verify all mizar articles contained in SANDBOX."
  (declare (ignore sandbox)))

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

(defgeneric trash (sandbox)
  (:documentation "Delete whatever articles that are being monitored
  by SANDBOX, and, if the directory corresponding to SANDBOX is empty,
  delete that too.  Signals a condition if the directory associated
  with SANDBOX contains any 'unknown' files, that is, files that do
  not come from mizar processing of the articles monitored by the sandbox."))

(defmethod trash ((s sandbox))
  (with-slots (location) s
    ;; just delete the directory for now
    (sb-ext:delete-directory location))) ;; not ideal: we shouldn't use sb-ext

(defun run-in-directory (program directory args &key input output if-output-exists)
  (if (directory-p (file-exists-p directory))
      (let ((dir-as-string (directory-namestring (pathname-as-directory directory))))
	(sb-ext:run-program "exec-in-dir.sh" 
			    (append (list dir-as-string program) args)
			    :search t
			    :input input
			    :output output
			    :if-output-exists if-output-exists))
      (error "No such directory ~A" directory)))

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
(define-file-transformer fix-by-and-from "fix-by-and-from.sh")
(define-input-transformer squeeze-repeated-newlines "tr" "-s" "\\n")
(define-input-transformer squeeze-repeated-spaces "tr" "-s" "[:space:]")

(defgeneric run-mizar-tool (tool article directory &rest flags))

(defmethod run-mizar-tool ((tool string) (article-path pathname) directory &rest flags)
  (let ((name (namestring article-path)))
    (if (probe-file article-path)
	(if (directory-p (file-exists-p directory))
	    (let ((proc (run-in-directory tool directory (append flags (list name)))))
	      (if (zerop (sb-ext:process-exit-code proc))
		  (let ((err-filename (replace-extension article-path "miz" "err")))
		    (if (and (probe-file err-filename)
			     (not (zerop (file-size err-filename))))
			(error "Although ~S returned successfully, it nonetheless generated a non-empty error file" tool)
			t))
		  (error "~S did not exit cleanly working on ~S" tool article-path)))
	    (error "The specified directory, ~A, in which to apply the mizar tool ~A is not a directory!" directory tool))
	(error "No such file: ~S" name))))

(defmethod run-mizar-tool ((tool string) (article-path string) directory &rest flags)
  (apply 'run-mizar-tool tool (pathname article-path) directory flags))

(defmethod run-mizar-tool ((tool string) (article article) directory &rest flags)
  (if (slot-boundp article 'path)
      (apply 'run-mizar-tool tool (path article) directory flags)
      (error "Cannot apply ~S to ~S because we don't know its path"
	     tool article)))

(defmethod run-mizar-tool ((tool symbol) article directory &rest flags)
  (apply 'run-mizar-tool 
	 (format nil "~(~a~)" (string tool)) ; lowercase: watch out
	 article
	 directory
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

(defmacro define-mizar-text-transformer (tool)
  ; check that TOOL is real
  (let ((check (sb-ext:run-program "which" (list tool) :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	(let ((tool-as-symbol (intern (format nil "~:@(~a~)" tool))))
	  `(progn
	     (defgeneric ,tool-as-symbol (article directory &rest flags))
	     (defmethod ,tool-as-symbol ((article-path pathname) directory &rest flags)
	       (let ((edtfile-path (replace-extension article-path
						      "miz" "$-$")))
		 (apply 'run-mizar-tool ,tool article-path directory flags)
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

;;; mizar.lisp ends here