
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro miz-item-html ((title)
			 (&rest rest
			  &key (content-type "text/html; charset=UTF-8")
			       (xml-declaration "<?xml version='1.0' encoding='UTF-8'?>")
			       (doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
			       (return-code +http-ok+)
			  &allow-other-keys)
			 &body body)
    `(with-html (:content-type ,content-type
	         :xml-declaration ,xml-declaration
		 :doctype ,doctype
		 :return-code ,return-code
		 ,@rest)
       (:head
	((:link :rel "icon" :href "/favicon.ico" :type "image/x-icon"))
	((:link :href "/mhtml.css" :rel "stylesheet" :type "text/css"))
	((:script :src "/mhtml.js" :type "text/ecmascript"))
	(:title ,(if (stringp title) title (list 'str title))))
       (:body
	((:table :border "1"
		 :summary "navigation"
		 :class "header"
		 :width "100%")
	 (:tr
	  (:td
	   ((:span :class "menu")
	    ((:a :href "/") "main")
	    " | "
	    ((:a :href "/about") "about")
	    " | "
	    ((:a :href "/articles") "articles")
	    " | "
	    ((:a :href "/landmarks") "landmarks")
	    " | "
	    ((:a :href "/path") "paths")
	    " | "
	    ((:a :href "/random-item") "random item")
	    " | "
	    ((:a :href "/upload") "submit your own article")))))
	,@body
	(:hr)
	((:div :class "footer")
	 ((:span :class "fleft") "See the " ((:a :href "/feedback") "feedback page") " for information about contacting us.")
	 ((:span :class "menu")
	  "Validate: " ((:a :href "http://jigsaw.w3.org/css-validator/check/referer") "CSS") " | "((:a :href "http://validator.w3.org/check/referer") "XHTML"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server and application setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar items-dispatch-table nil)

(define-constant +unsupported-methods+
    (list :put :delete :trace :connect)
  :test #'set-equal
  :documentation "A list of those HTTP methods which are not supported for any resource")

(defun respond-to-unhandled-request ()
  (let* ((uri (request-uri*))
	 (method (request-method*))
	 (method-name (symbol-name method)))
    (miz-item-html ("unsupported http method")
	(:return-code +http-method-not-allowed+)
      (:p "The " (str method-name) " HTTP method is not allowed for the resource")
	(:blockquote
	 (:tt (str uri)))
	(:p "Perhaps you meant to GET this resource?"))))

(defun respond-to-options ()
  (setf (return-code *reply*) +http-ok+)
  (setf (header-out "Allow") "OPTIONS, GET, HEAD")
  (setf (content-length* *reply*) 0)
  (send-headers))

(defun items-request-dispatcher (request)
  "Selects a request handler based on a list of individual request
dispatchers all of which can either return a handler or neglect by
returning NIL."
  (let ((method (request-method*)))
    (cond ((member method +unsupported-methods+)
	   (respond-to-unhandled-request))
	  ((eq method :options)
	   (respond-to-options))
	  (t
	   (loop
	      for dispatcher in items-dispatch-table
	      for action = (funcall dispatcher request)
	      when action return (let ((response (funcall action)))
				   (if (eq method :head)
				       (progn
					 (setf (content-length*)
					       (length response))
					 (send-headers))
				       response))
	      finally
		(miz-item-html ("not found")
		    (:return-code +http-not-found+)
		  (:p "I can't find what you're looking for.")
		  (:p "You requested the URI:")
		  (:blockquote
		   (:tt (str (request-uri*))))
		  (:p "Here are the parameters that you submitted with your
	request:")
		  (let ((params (get-parameters*)))
		    (if params
			(htm
			 (:dl
			  (loop
			     for (param . value) in (get-parameters*)
			     do
			       (htm
				(:dt param)
				(:dd value)))))
			(htm (:blockquote (:em "(none)")))))))))))

(defvar *acceptor* (make-instance 'hunchentoot:acceptor
				  :port 4242))

;; (defvar *acceptor* (make-instance 'hunchentoot:acceptor
;; 				  :port 4242
;; 				  :request-dispatcher #'items-request-dispatcher))

(defun initialize-server (mml-version)
  (unless (hunchentoot::acceptor-listen-socket *acceptor*)
    (hunchentoot:start *acceptor*))
  (initialize-items-data mml-version)
  (format t "Initializing URIs...")
  (initialize-uris)
  (format t "done~%")
  (setf *message-log-pathname* (mizar-items-config 'server-messages-log-file)
	*access-log-pathname* (mizar-items-config 'server-access-log-file)
	*handle-http-errors-p* nil
	*log-lisp-errors-p* t
	*log-lisp-warnings-p* t
	*log-lisp-backtraces-p* t
	*show-lisp-errors-p* t)
  t)

(defun shutdown-server ()
  (stop *acceptor*))

(defun re-initialize-server (mml-version)
  (shutdown-server)
  (setf *acceptor* (make-instance 'hunchentoot:acceptor
				  :port 4242
				  :acceptor #'items-request-dispatcher))
  (setf items-dispatch-table nil)
  (initialize-server mml-version))
