;;; mizar.lisp Interface to the mizar tools and the MML

(in-package :mizar)

(defvar *mizfiles* 
  #+sbcl
  (ensure-directory (sb-ext:posix-getenv "MIZFILES"))
  #+ccl
  (ensure-directory (ccl:getenv "MIZFILES"))
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
    (find article-str-lc *mml-lar*
	  :test #'string=
	  :key #'name)))

(defmethod belongs-to-mml ((article article))
  (find article *mml-lar*))

(defgeneric mml-lar-index (thing)
  (:documentation "Where THINGS sits in the currentl mml.lar ordering."))

(let ((table (make-hash-table :test #'equal)))
  (defmethod mml-lar-index ((article-name string))
    (multiple-value-bind (position present?)
	(gethash article-name table)
      (if present?
	  position
	  (setf (gethash article-name table)
		(position article-name *mml-lar* :test #'string= :key #'name))))))

(defmethod mml-lar-index ((thing article))
  (mml-lar-index (name thing)))

(defun mml-< (article-1 article-2)
  (< (mml-lar-index article-1)
     (mml-lar-index article-2)))

(defgeneric run-in-directory (program working-directory args))

(defmethod run-in-directory (program (working-directory sandbox) args)
  (run-in-directory program (location sandbox) args))

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
		 :input nil
		 :output nil
		 :error nil)))

(defmethod run-in-directory (program (working-directory string) args)
  (run-in-directory program (pathname working-directory) args))

(defmacro define-file-transformer (name program &rest arguments)
  ; check that TOOL is real
  (let* ((eval-program (eval program))
	 (check (run-program "which" (list eval-program) :search t)))
    (if (zerop (process-exit-code check))
	`(progn
	   (defgeneric ,name (file &optional directory))
	   (defmethod ,name ((miz-path pathname) &optional (directory (user-homedir-pathname)))
	     (let* ((tmp-path (replace-extension miz-path "miz" "splork"))
		    (proc (run-in-directory ,eval-program
					    directory
					    (append ',arguments (list (namestring miz-path)))
					    :output tmp-path
					    :if-output-exists :supersede)))
	       (if (zerop (process-exit-code proc))
		   (rename-file tmp-path miz-path)
		   (error "Something went wrong when calling '~A' with arguments ~A; the process exited with code ~S" ,eval-program ',arguments (process-exit-code proc)))))
	     (defmethod ,name ((article-path string) &optional (directory (user-homedir-pathname)))
	       (,name (pathname article-path) directory))
	     (defmethod ,name ((article article) &optional (directory (user-homedir-pathname)))
	       (,name (path article) directory)
	       (refresh-text article)))
	(error "The program ~S could not be found in your path (or it is not executable)" eval-program))))

(defmacro define-input-transformer (name program &rest arguments)
  ; check that TOOL is real
  (let* ((eval-program (eval program))
	 (check (run-program "which" (list eval-program) :search t)))
    (if (zerop (process-exit-code check))
	`(progn
	   (defgeneric ,name (file &optional directory))
	   (defmethod ,name ((miz-path pathname) &optional (directory (user-homedir-pathname)))
	     (let* ((tmp-path (replace-extension miz-path "miz" "splork"))
		    (proc (run-in-directory ,eval-program
					    directory
					    ',arguments 
					    :input miz-path
					    :output tmp-path
					    :if-output-exists :supersede)))
	       (if (zerop (process-exit-code proc))
		   (rename-file tmp-path miz-path)
		   (error "Something went wrong when calling '~A' with arguments ~A; the process exited with code ~S" ,eval-program ',arguments (process-exit-code proc)))))
	     (defmethod ,name ((article-path string) &optional (directory (user-homedir-pathname)))
	       (,name (pathname article-path) directory))
	     (defmethod ,name ((article article) &optional (directory (user-homedir-pathname)))
	       (,name (path article) directory)
	       (refresh-text article)))
	(error "The program ~S could not be found in your path (or it is not executable)" eval-program))))

(defun atr-file-for-article (article-pathname)
  (let ((article-base (pathname-name article-pathname))
	(article-dir (directory-namestring article-pathname)))
    (merge-pathnames (format nil "~a.atr" article-base)
		     article-dir)))

(defun pruned-atr-file-for-article (article-pathname)
  (let ((article-base (pathname-name article-pathname))
	(article-dir (directory-namestring article-pathname)))
    (merge-pathnames (format nil "~a.atr.pruned" article-base)
		     article-dir)))

(defun err-file-for-article (article-pathname)
  (let ((article-base (pathname-name article-pathname))
	(article-dir (directory-namestring article-pathname)))
    (merge-pathnames (format nil "~a.err" article-base)
		     article-dir)))

(defgeneric run-mizar-tool (tool article &key directory ignore-exit-code flags))

(defmethod run-mizar-tool :around ((tool string) (article pathname) &key directory ignore-exit-code flags)
  (declare (ignore ignore-exit-code))
  (when (string= tool "")
    (error "A mizar tool to be applied (the empty string doesn't count!)"))
  (when directory
    (unless (file-exists-p (ensure-directory directory))
      (error "The supplied work directory '~a' doesn't exist!" directory)))
  (if (listp flags)
      (multiple-value-bind (ok bad-guy)
	  (every-with-falsifying-witness flags #'non-empty-stringp)
	(unless ok
	  (error "The list of flags should contain only non-empty strings; '~a' isn't" bad-guy)))
      (error "The list of flags '~a' isn't actually a list!" flags))
  (if (probe-file article)
      (call-next-method)
      (error "No such file: ~a" article)))

(defmethod run-mizar-tool (tool (article-path pathname) &key directory ignore-exit-code flags)
  (let ((article-dir (cond ((typep directory 'sandbox)
			    (location sandbox))
			   ((null directory)
			    (directory-namestring article-path))
			   ((pathnamep directory)
			    directory)
			   ((stringp directory)
			     (pathname directory))
			   (t
			    (error "Unable to handle the supplied working directory '~a'" directory)))))
    (run-mizar-tool tool article-path
		    :directory article-dir
		    :ignore-exit-code ignore-exit-code
		    :flags flags)))

(defmethod run-mizar-tool ((tool string) (article-path pathname) &key directory ignore-exit-code flags)
  (let ((name (namestring article-path)))
    (let ((proc (run-in-directory tool directory (append flags (list name)))))
      (or ignore-exit-code
	  (or (zerop (process-exit-code proc))
	      (error 'mizar-error
		     :tool tool 
		     :working-directory directory
		     :argument article-path 
		     :output-stream (process-output proc)
		     :error-stream (process-error proc) 
		     :exit-code (process-exit-code proc)))))))

(defmethod run-mizar-tool ((tool string) (article-path string) &key directory ignore-exit-code flags)
  (run-mizar-tool tool (pathname article-path)
		  :directory directory
		  :ignore-exit-code ignore-exit-code
		  :flags flags))

(defmethod run-mizar-tool ((tool string) (article article) &key directory ignore-exit-code flags)
  (if (slot-boundp article 'path)
      (run-mizar-tool tool (path article)
		      :directory directory
		      :ignore-exit-code ignore-exit-code
		      :flags flags)
      (error "Cannot apply ~S to ~S because we don't know its path"
	     tool article)))

(defmethod run-mizar-tool ((tool symbol) article &key directory ignore-exit-code flags)
  (run-mizar-tool (format nil "~(~a~)" (string tool)) ; lowercase: watch out
		  article
		  :directory directory
		  :ignore-exit-code ignore-exit-code
		  :flags flags))

(defmacro define-mizar-tool (tool)
  (let ((tool-as-symbol (intern (format nil "~:@(~a~)" tool))))
    `(progn
       (defgeneric ,tool-as-symbol (article &key working-directory flags))
       (defmethod ,tool-as-symbol ((article-path pathname) &key working-directory flags)
	 (run-mizar-tool ,tool article-path
			 :directory working-directory
			 :ignore-exit-code nil
			 :flags flags))
       (defmethod ,tool-as-symbol ((article-path string) &key working-directory flags)
	 (run-mizar-tool ,tool article-path
			 :directory working-directory
			 :ignore-exit-code nil
			 :flags flags))
       (defmethod ,tool-as-symbol ((article article) &key working-directory flags)
	 (run-mizar-tool ,tool article
			 :directory working-directory
			 :ignore-exit-code nil
			 :flags flags)
	 article))))

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
(define-mizar-tool "errflag")
(define-mizar-tool "addfmsg")
(define-mizar-tool "inacc")
(define-mizar-tool "relprem")
(define-mizar-tool "reliters")

(defmacro define-mizar-text-transformer (tool &optional (ignore-exit-code nil))
  ; check that TOOL is real
  (let ((check (run-program "which" (list tool) :search t)))
    (if (zerop (process-exit-code check))
	(let ((tool-as-symbol (intern (format nil "~:@(~a~)" tool))))
	  `(progn
	     (defgeneric ,tool-as-symbol (article directory &rest flags))
	     (defmethod ,tool-as-symbol ((article-path pathname) directory &rest flags)
	       (let ((edtfile-path (replace-extension article-path
						      "miz" "$-$")))
		 (run-mizar-tool ,tool article-path
				 :directory directory
				 :ignore-exit-code ,ignore-exit-code
				 :flags flags)
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
	  (run-program "xsltproc"
		       (list (namestring *addabsrefs-stylesheet*)
			     (namestring article-xml-path)
			     "-o"
			     (namestring new-article-xml-path))
		       :search t)
	  t)
	(error "File does not exist: ~S" article-xml-path))))

(defmethod absrefs ((article-path string))
  (absrefs (pathname article-path)))

(defmethod absrefs ((article article))
  (absrefs (path article))
  (refresh-xml article))

(defgeneric mhtml (article)
  (:documentation "Compute the HTML representation of an article."))

(defmethod mhtml ((article-path-string string))
  (mhtml (pathname article-path-string)))

(defmethod mhtml ((article-path pathname))
  (let ((new-article-xml-path (replace-extension article-path "miz" "xml1"))
	(article-html-path (replace-extension article-path "miz" "html")))
    (if (probe-file new-article-xml-path)
	(run-program "xsltproc"
		     (list (namestring (mizar-items-config 'mhtml-stylesheet))
			   (namestring new-article-xml-path)
			   "-o"
			   (namestring article-html-path))
		     :search t)
	(error "File does not exist: ~S" new-article-xml-path))))

(defun verify-and-export (article &optional directory)
  (accom article directory "-q" "-s" "-l")
  (verifier article directory "-q" "-s" "-l")
  (exporter article directory "-q" "-s" "-l")
  (transfer article directory "-q" "-s" "-l"))

(defun listvoc (article-name)
  (if (string= article-name "HIDDEN") ; can't list symbols in this special vocab file
      nil
      (let ((proc (run-program (mizar-items-config 'listvoc-script-path)
			       (list article-name)
			       :search t
			       :output :stream)))
	(let ((exit-code (process-exit-code proc)))
	  (if (zerop exit-code)
	      (stream-lines (process-output proc))
	      (error "Something went wrong running listvoc.sh: the exit code was ~d" exit-code))))))

;;; mizar.lisp ends here