
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
    :initform *mizfiles*))
  (:documentation "A wrapper around a copy of the MML."))

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

(defun run-mizar-utility (utility)
  (declare (ignore utility))
  nil)

;;; mizar.lisp ends here