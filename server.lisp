
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
	    ((:a :href "/random-item") "random item")))))
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
    (list :put :post :delete :trace)
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
  (setf (header-out "Allow") "OPTIONS, GET")
  (setf (content-length* *reply*) 0))

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
	      when action return (funcall action)
	      finally (setf (return-code *reply*) +http-not-found+))))))

(defvar *acceptor* (make-instance 'hunchentoot:acceptor 
				  :port 4242
				  :request-dispatcher #'items-request-dispatcher))

(defun initialize-server (mml-version &optional reload-graphs (articles :all))
  (unless (hunchentoot::acceptor-listen-socket *acceptor*)
    (hunchentoot:start *acceptor*))
  (load-mml mml-version)
  (format t "Loading article item counts...")
  (load-article-num-items reload-graphs)
  (format t "done.~%")
  (multiple-value-setq (*item-to-ckb-table* *ckb-to-items-table*)
    (load-item-to-fragment-table))
  ;; (when (or reload-graphs (null *graphs-loaded*))
  ;;   (load-dependency-graphs reload-graphs))
  (format t "Initializing URIs...")
  (initialize-uris articles)
  (format t "done~%")
  (setf *message-log-pathname* (mizar-items-config 'server-messages-log-file)
	*access-log-pathname* (mizar-items-config 'server-access-log-file)
	*handle-http-errors-p* nil
	*http-error-handler* #'handle-http-error
	*log-lisp-errors-p* t
	*log-lisp-warnings-p* t
	*log-lisp-backtraces-p* t
	*show-lisp-errors-p* t)
  (setf *attribute-quote-char* #\")
  t)

(defun handle-http-error (error-code)
  (if (= error-code +http-not-found+)
      (miz-item-html ("not found")
	  (:return-code +http-not-found+)
	(:p "I can't find what you're looking for.")
	(:p "Your request was:")
	(:dl
	 (loop
	    for (param . value) in (get-parameters*)
	    do
	      (htm
	       (:dt param)
	       (:dd value)))))
      (when (= error-code +http-method-not-allowed+)
	(let* ((uri (request-uri*))
	       (method (request-method*))
	       (method-name (symbol-name method)))
	  (miz-item-html ("unsupported http method")
	      (:return-code +http-method-not-allowed+)
	    (:p "The " (str method-name) " HTTP method is not allowed for the resource")
	    (:blockquote
	     (:tt (str uri)))
	    (:p "Perhaps you meant to GET this resource?"))))))
