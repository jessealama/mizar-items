
(in-package :cl-user)

(defpackage :mizar
  (:use :cl :cl-ppcre :com.gigamonkeys.pathnames))

(defvar *mizar-package* (find-package :mizar))

;;; packages.lisp ends here