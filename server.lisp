
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro miz-item-html (title &body body)
  `(with-html
     (:head 
      ((:link :rel "icon" :href "/favicon.ico" :type "image/x-icon"))
      ((:link :href "/mhtml.css" :rel "stylesheet" :type "text/css"))
      ((:link :href "/screen.css" :rel "stylesheet" :type "text/css"))
      ((:script :src "/mhtml.js" :type "text/ecmascript"))
      (:title ,title))
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
	  ((:a :href "/random-item") "random-item")
	  " | "
	  ((:a :href "/random-path") "random-path")))))
      ,@body
      (:hr)
      ((:div :class "footer")
       ((:span :class "fleft") "See the " ((:a :href "/feedback") "feedback page") " for information about contacting us.")
       ((:span :class "menu")
	"Validate: " ((:a :href "http://jigsaw.w3.org/css-validator/check/referer") "CSS") ((:a :href "http://validator.w3.org/check/referer") "XHTML"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server and application setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar items-dispatch-table nil)

(defun items-request-dispatcher (request)
  "Selects a request handler based on a list of individual request
dispatchers all of which can either return a handler or neglect by
returning NIL."
  (loop for dispatcher in items-dispatch-table
        for action = (funcall dispatcher request)
        when action return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

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
	*handle-http-errors-p* t
	*http-error-handler* #'handle-http-error
	*log-lisp-errors-p* t
	*log-lisp-warnings-p* t
	*log-lisp-backtraces-p* t
	*show-lisp-errors-p* t)
  (setf *attribute-quote-char* #\")
  t)

(defun handle-http-error (error-code)
  (when (= error-code +http-not-found+)
    (miz-item-html "No"
      (:p "I still haven't found what you're looking for."))))

(setq *http-error-handler* #'handle-http-error)
