
(in-package :cl-user)

(defpackage :mizar
  (:use :cl :cl-ppcre :dom))

(defvar *mizar-package* (find-package :mizar))

;;; packages.lisp ends here