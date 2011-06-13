
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
  :depends-on ("xpath" "cl-ppcre" "com.gigamonkeys.pathnames" "alexandria" "hunchentoot" "cl-who" "hunchentoot-dir-lister" "hunchentoot-utils" "xuriella" "ironclad" "drakma")
  :components ((:file "packages")
	       (:file "config")
	       (:file "sandbox")
	       (:file "utils")
	       (:file "xml-utils")
	       (:file "file-utils")
	       (:file "author")
	       (:file "article")
	       (:file "mizar")
	       (:file "item")
	       (:file "itemize")
	       (:file "queue")
	       (:file "search")
	       (:file "bidirectional")
	       (:file "mml")
	       (:file "depgraph")
	       (:file "mptp")
	       (:file "site-data")
	       (:file "tc")
	       (:file "site-search")
	       (:file "server")
	       (:file "100theorems")
	       (:file "piotr")
	       (:file "site")))
