
(in-package :mizar)

(defclass sandbox ()
  ((location
    :initarg :location
    :accessor location
    :type pathname))
  (:documentation "A sandbox is a wrapper around a directory."))

(defclass sandbox-factory ()
  ((home
    :initarg :home
    :accessor home
    :type pathname)))

(defparameter *sandbox-factory* (make-instance 'sandbox-factory
					       :home (pathname-as-directory "/tmp")))

(defun fresh-sandbox (&optional (name-prefix ""))
  (loop
     with home-string = (namestring (home *sandbox-factory*))
     for i from 1
     for dir = (concat home-string name-prefix "-" (format nil "~d" i) "/")
     do
       (when (null (file-exists-p dir))
	 (ensure-directories-exist dir)
	 (return (make-instance 'sandbox :location dir)))))

(defun copy-file-to-sandbox (file sandbox)
  (let ((file-in-sandbox (file-in-directory (location sandbox) (file-namestring file))))
    (copy-file file file-in-sandbox)))

(defun make-directory-in-sandbox (directory-name sandbox)
  (let ((dir (ensure-directory
	      (concat (namestring (ensure-directory (location sandbox)))
		      directory-name))))
  (ensure-directories-exist dir)
  dir))

(defun sync-from-to (source-sandbox target-sandbox)
  (let* ((source-directory (location source-sandbox))
	 (target-directory (location target-sandbox))
	 (rsync-proc (sb-ext:run-program "rsync"
					 (list source-directory target-directory)
					 :search t
					 :input nil
					 :output nil
					 :error :stream)))
    (unless (zerop (sb-ext:process-exit-code rsync-proc))
      (let ((err (sb-ext:process-error rsync-proc)))
	(error "Something went wrong calling rsync to synchronize ~A with ~A: the error output was: ~A" source-directory target-directory (stream-lines err))))
    t))

(defun trash-sandbox (sandbox)
  (sb-ext:run-program "rm"
		      (list "-Rf"
			    (namestring (location sandbox)))
		      :search t
		      :input nil
		      :output nil
		      :error nil)
  t)

(defun file-exists-in-sandbox (filename sandbox)
  (let ((location (location sandbox)))
    (file-exists-p (file-in-directory location filename))))

;;; sandbox.lisp ends here