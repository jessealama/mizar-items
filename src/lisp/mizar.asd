
(in-package :cl-user)

(declaim (optimize (compilation-speed 0) (speed 0) (debug 3)))

(defpackage :mizar-asd
  (:use :cl :asdf))

(in-package :mizar-asd)

(defsystem :mizar
  :description "Tools for interacting with the mizar system"
  :long-description ""
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :serial t
  :depends-on ("xpath"
	       "cl-ppcre"
	       "cl-fad"
	       "trivial-timeout"
	       "com.dvlsoft.clon"
	       "alexandria"
	       "named-readtables"
	       "hunchentoot"
	       "cl-who"
	       "hunchentoot-utils"
	       "xuriella"
	       "clunit"
	       "ironclad"
	       "drakma"
	       "pcall"
	       "osicat")
  :components ((:file "packages")
	       (:file "config")
	       (:file "sandbox")
	       (:file "utils")
	       (:file "xml-utils")
	       (:file "file-utils")
	       (:file "author")
	       (:file "stylesheets")
	       (:file "local-db")
	       (:file "article")
	       (:file "itemized-db")
	       (:file "run")
	       (:file "mizar-condition")
	       (:file "mizar")
	       (:file "mizar-xsl")
	       (:file "item")
	       (:file "itemize")
	       (:file "queue")
	       (:file "search")
	       (:file "bidirectional")
	       (:file "mml")
	       (:file "reservations")
	       (:file "depgraph")
	       (:file "mptp")
	       (:file "properties")
	       (:file "site-data")
	       (:file "tc")
	       (:file "site-search")
	       (:file "server")
	       (:file "100theorems")
	       (:file "piotr")
	       (:file "site-about")
	       (:file "site-paths")
	       (:file "site")
	       (:file "minimize")
	       (:file "ivy-nd")))
