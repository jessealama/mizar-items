
(in-package :cl-user)

(defpackage :mizar
  (:use :cl)
  (:import-from :clunit
		#:defsuite
		#:deftest
		#:assert-true
		#:assert-false
		#:assert-equal
		#:run-suite
		#:run-test)
  (:import-from :osicat
		#:with-temporary-file)
  (:import-from :cl-ppcre
		#:scan
		#:register-groups-bind
		#:split)
  (:import-from :editor-hints.named-readtables
		#:find-readtable)
  (:import-from :xuriella
		#:parse-stylesheet)
  (:import-from :alexandria
                #:sequence-of-length-p
		#:starts-with-subseq
		#:define-constant
		#:set-equal
		#:hash-table-keys
		#:random-elt
		#:copy-file
		#:length=
		#:disjoin
		#:read-file-into-string
		#:write-string-into-file
		#:switch)
  (:import-from :cl-fad
		#:directory-exists-p
		#:directory-pathname-p
		#:walk-directory
		#:list-directory
		#:file-exists-p
		#:pathname-as-directory
		#:pathname-as-file)
  (:use :hunchentoot)
  (:use :cl-who)
  (:use :hunchentoot-utils))

(defvar *mizar-package* (find-package :mizar))

;; early cl-who configuration of attribute delimit character
(setf cl-who:*attribute-quote-char* #\")

;;; packages.lisp ends here
