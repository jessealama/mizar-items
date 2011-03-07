;;; utils.lisp A collection of utilities for the hunchentoot web server

(in-package :mizar)

(defmacro with-xml-declaration (&body body)
  `(with-html-output-to-string (s)
     "<?xml version='1.1' encoding='UTF-8'?>"
     ,*prologue*
     (htm ,@body)))

(defmacro with-html (&body body)
  `(with-xml-declaration
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	,@body)))

(defmacro with-favicon-and-title (favicon-url title &body body)
  `(with-html
     (:head 
      (:link :rel "icon" :href ,favicon-url :type "image/x-icon")
      (:title ,title))
     (:body ,@body)))


(defmacro with-title (title &body body)
  `(with-html
     (:head (:title ,title))
     (:body ,@body)))

(defmacro define-xml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "application/xhtml+xml")
     ,@body))

(defmacro define-xhtml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "text/html")
     ,@body))

(defmacro create-static-page-dispatcher (page handler)
  "Create a hunchentoot dispatcher that dispatches to HANDLER only for
exact matches for PAGE.  PAGE is a string that begins with \"/\".
Other than that, it should contain only alphanumeric characters. (In
particular, it should not contain a dollar symbol \"$\"). HANDLER is a
symbol naming a function that can handle requests that match
page (exactly).

This macro was created to fill a gap that exists in hunchentoot.  One
can create dispatchers that dispatch based on requests that match a
*prefix* of a string, and one can create dispatchers that dispatch
based on requests matching a *regular expression*.  I wanted a clean
way to create dispatchers that dispatch based on *exact* matches.
Thus, I want to dispatch requests for, say, \"/foo\" to go to
FOO-HANDLER.  If the client requests \"/foobar\", I want it to go to
some other handler (or generate a response with code 404).

Obviously, this kind of thing doesn't really arise in file-based HTTP
servers.  If there's a file called \"foo\" under the root document
directory, then evidently it is the thing to be served when one
requests \"/foo\".  If one requests \"/foobar\" and there is no such
file with that name under the root document directory, then the
response of the HTTP server will indicate an error.  But in the more
dynamic, not-necessarily-file-based approach taken by hunchentoot, one
has the flexibility to dispatch in all sorts of ways depending on the
requested resource; the space of resources that could be successfully
served is limited only by what one can compute based on the request;
the set of requestable resources isn't limited to what files exist
under the root document directory.

Using regular expressions, à la CL-PPCRE, is obviously the way to
solve the problem.  Given a static \"page\" name like \"index\", we
create a regular expression that matches only the string \"index\".
Hunchentoot can do this (with a little help from CL-PPCRE) But I
didn't want to pollute my application code with regular expressions,
which are just a means to solve the problem; there's nothing
interesting about them in this context.  Regular expressions are just
a way to address a gap in hunchentoot.

I don't know whether this is going to be an efficient solution.  I may
need to design another solution later, if the need arises."
  `(create-regex-dispatcher (concatenate 'string
					 "^"
					  ,page
					  "$")
			     ,handler))

(defun fetch-post-parameters (&rest params)
  (apply #'values
	 (mapcar #'post-parameter params)))

(defmacro with-valid-session ((&key (title "Invalid session")
				    (return-code 409)
				    (explanation-forms nil)) &body body)
  `(cond ((session-verify *request*) ,@body)
	 (t
	  (setf (return-code*) ,return-code)
	  (with-title ,title
	    (quote ,explanation-forms)))))

;;; utils.lisp ends here