
(in-package :mizar)

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

(defun initialize-server (&optional reload-graphs (articles :all))
  (unless (hunchentoot::acceptor-listen-socket *acceptor*)
    (hunchentoot:start *acceptor*))
  (format t "Loading article item counts...")
  (load-article-num-items reload-graphs)
  (format t "done.~%")
  (when (or reload-graphs (null *graphs-loaded*))
    (load-dependency-graphs reload-graphs))
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
