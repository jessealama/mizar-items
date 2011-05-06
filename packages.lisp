
(in-package :cl-user)

(defpackage :mizar
  (:use :cl :cl-ppcre :com.gigamonkeys.pathnames :alexandria :hunchentoot :cl-who :hunchentoot-utils))

(defvar *mizar-package* (find-package :mizar))

;; early cl-who configuration of attribute delimit character
(setf cl-who:*attribute-quote-char* #\")

;;; packages.lisp ends here