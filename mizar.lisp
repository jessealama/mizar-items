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

(defgeneric strip-comments (article))

(defmethod strip-comments ((article-path pathname))
  (if (probe-file article-path)
      (let* ((tmp-path (replace-extension article-path "miz" "stripped"))
	     (proc (sb-ext:run-program "sed" (list "-e" "s/::.*$//" (namestring article-path))
				       :search t
				       :output tmp-path
				       :if-output-exists :error)))
	(if (zerop (sb-ext:process-exit-code proc))
	    (rename-file tmp-path article-path)
	    (error "Something went wrong when stripping comments; the process exited with code ~S" (sb-ext:process-exit-code proc))))
      (error "No such file ~A" article-path)))

(defmethod strip-comments ((article-path string))
  (strip-comments (pathname article-path)))

(defmethod strip-comments ((article article))
  (strip-comments (path article))
  (refresh-text article))

(defgeneric run-mizar-tool (tool article &rest flags))

(defmethod run-mizar-tool ((tool string) (article-path pathname) &rest flags)
  (let ((name (namestring article-path)))
    (if (probe-file article-path)
	(let ((proc (sb-ext:run-program tool (append flags (list name)) :search t)))
	  (if (zerop (sb-ext:process-exit-code proc))
	      (let ((err-filename (replace-extension article-path "miz" "err")))
		(if (and (probe-file err-filename)
			 (not (zerop (file-size err-filename))))
		    (error "Although ~S returned successfully, it nonetheless generated a non-empty error file" tool)
		    t))
	      (error "~S did not exit cleanly working on ~S" tool article-path)))
	(error "No such file: ~S" name))))

(defmethod run-mizar-tool ((tool string) (article-path string) &rest flags)
  (apply 'run-mizar-tool tool (pathname article-path) flags))

(defmethod run-mizar-tool ((tool string) (article article) &rest flags)
  (if (slot-boundp article 'path)
      (apply 'run-mizar-tool tool (path article) flags)
      (error "Cannot apply ~S to ~S because we don't know its path"
	     tool article)))

(defmethod run-mizar-tool ((tool symbol) article &rest flags)
  (apply 'run-mizar-tool 
	 (format nil "~(~a~)" (string tool)) ; lowercase: watch out
	 article
	 flags))

(defmacro define-mizar-tool (tool)
  ; check that TOOL is real
  (let ((check (sb-ext:run-program "which" (list tool) :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	(let ((tool-as-symbol (intern (format nil "~:@(~a~)" tool))))
	  `(progn
	     (defgeneric ,tool-as-symbol (article &rest flags))
	     (defmethod ,tool-as-symbol ((article-path pathname) &rest flags)
	       (apply 'run-mizar-tool ,tool article-path flags))
	     (defmethod ,tool-as-symbol ((article-path string) &rest flags)
	       (apply 'run-mizar-tool ,tool article-path flags))
	     (defmethod ,tool-as-symbol ((article article) &rest flags)
	       (apply 'run-mizar-tool ,tool article flags))))
	(error "The mizar tool ~S could not be found in your path (or it is not executable)" tool))))


(define-mizar-tool "edtfile")

(defmacro define-mizar-text-transformer (tool)
  ; check that TOOL is real
  (let ((check (sb-ext:run-program "which" (list tool) :search t)))
    (if (zerop (sb-ext:process-exit-code check))
	(let ((tool-as-symbol (intern (format nil "~:@(~a~)" tool))))
	  `(progn
	     (defgeneric ,tool-as-symbol (article &rest flags))
	     (defmethod ,tool-as-symbol ((article-path pathname) &rest flags)
	       (let ((edtfile-path (replace-extension article-path
						      "miz" "$-$")))
		 (apply 'run-mizar-tool ,tool article-path flags)
		 (edtfile article-path "-l")
		 (rename-file edtfile-path article-path)))
	     (defmethod ,tool-as-symbol ((article-path string) &rest flags)
	       (apply ',tool-as-symbol (pathname article-path) flags))
	     (defmethod ,tool-as-symbol ((article article) &rest flags)
	       (apply ',tool-as-symbol (path article) flags)
	       (refresh-text article))))
	(error "The mizar tool ~S could not be found in your path (or it is not executable)" tool))))

;; our text transformers -- thanks, Karol Pąk et al.! 
(define-mizar-text-transformer "JA1")
(define-mizar-text-transformer "dellink")
(define-mizar-text-transformer "CutSet")
(define-mizar-text-transformer "CutReconsider")
(define-mizar-text-transformer "change") ; clever name...

;; workhorses
(define-mizar-tool "makeenv")
(define-mizar-tool "accom")
(define-mizar-tool "verifier")
(define-mizar-tool "envget")

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