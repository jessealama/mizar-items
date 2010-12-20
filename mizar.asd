
(in-package :cl-user)

(defpackage :mizar-asd
  (:use :cl :asdf))

(in-package :mizar-asd)

(defsystem :mizar
  :description "Tools for interacting with the mizar system"
  :long-description ""
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :serial t
  :depends-on ("xpath" "cl-ppcre")
  :components ((:file "packages")
	       (:file "xml-utils")
	       (:file "file-utils")
	       (:file "article")
	       (:file "mizar")
	       (:file "item")
	       (:file "itemize")))
