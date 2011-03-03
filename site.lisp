
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server and application setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *items-server-port* 8000)

(defun make-items-backend ()
  (make-backend
   :httpd
   :host "127.0.0.1"
   :port *items-server-port*))

(defun make-items-server ()
  (make-instance
   'standard-server
   :backend (make-items-backend)))

(defvar *items-server* (make-items-server))

(defun startup-items-server ()
  (startup-server *items-server*))

(defun shutdown-items-server ()
 (shutdown-server *items-server*))

(defclass mizar-items-application (standard-application 
				cookie-session-application-mixin)
  ()
  (:default-initargs
   :url-prefix "/"
    :debug-on-error t))

(defvar *mizar-items-application* (make-instance 'mizar-items-application))

(register-application *items-server* *mizar-items-application*)

(defentry-point "" (:application *mizar-items-application*)
    ()
    (call 'toplevel-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *itemization-source*
  "/Users/alama/sources/mizar/mizar-items/itemization")

(defparameter *articles* (list "xboole_0"
			       "boole" 
			       "xboole_1"
			       "enumset1"
			       "zfmisc_1"
			       "subset_1"
			       "subset"
			       "setfam_1"
			       "relat_1"
			       "funct_1"))

(defparameter *dependency-graph-file* 
  "/Users/alama/sources/mizar/mizar-items/depgraph")

(defun load-dependency-graph ()
  (let ((lines (lines-of-file *dependency-graph-file*))
	(edges nil))
    (dolist (line lines edges)
      (multiple-value-bind (lhs rhs)
	  (split " " line)
	(push (cons lhs rhs) edges)))))

(defparameter *dependency-graph* (load-dependency-graph))

(defun count-miz-in-directory (dir)
  (let ((counter 0))
    (walk-directory dir #'(lambda (foo)
			    (declare (ignore foo))
			    (incf counter))
		    :test #'(lambda (path)
			      (scan "ckb[0-9]*\.miz" (namestring path))))
    counter))

(defun load-article-num-items ()
  (let ((num-items-table (make-hash-table :test #'equal)))
    (dolist (article-name *articles* num-items-table)
      (let ((article-dir (concat *itemization-source* "/" article-name "/" "text")))
	(setf (gethash article-name num-items-table)
	      (count-miz-in-directory article-dir))))))

(defparameter *article-num-items* (load-article-num-items))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcomponent mml-listing-component
    ()
  ())

(defcomponent about-this-site-component
    ()
  ())

(defcomponent contact-component
    ()
  ())

(defcomponent toplevel-window (standard-window-component)
  ()
  (:default-initargs
      :title "Explore fine-grained dependencies in the Mizar Mathematical Library"
      :doctype yaclml:+xhtml-strict-doctype+
      :body
      (make-instance 'tabbed-pane
		     :current-component-key "mml listing"
		     :key-test #'string=
		     :contents 
		     `(("mml listing" . ,(make-instance 'mml-listing-component))
		       ("about this site" . ,(make-instance 'about-this-site-component))
		       ("contact" . ,(make-instance 'contact-component))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rendering methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render ((mml-listing mml-listing-component))
  (<:p "Here's the mml."))

(defmethod render ((about about-this-site-component))
  (<:p "I made this because I wanted to make something cool."))

(defmethod render ((contact contact-component))
  (<:p "my email address is jesse.alama@gmail.com."))
