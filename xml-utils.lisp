;;; xml-util.lisp Utilities for working with XML, XPath, etc.

(in-package :mizar)

(defun value-of-name-attribute (node)
  (let ((name-attribute-as-node-set (xpath:evaluate "@name" node)))
    (let ((name-attribute (first (xpath:all-nodes name-attribute-as-node-set))))
      (xpath:evaluate "string()" name-attribute))))

(defun integer-value-of-attribute (node attribute)
  (let ((xpath (format nil "@~A + 0" attribute))) ; coerce to integer
    (floor (xpath:evaluate xpath node))))

(defun value-of-nr-attribute (node)
  (integer-value-of-attribute node "nr"))

(defun value-of-line-attribute (node)
  (integer-value-of-attribute node "line"))

(defun value-of-col-attribute (node)
  (integer-value-of-attribute node "col"))

(defun value-of-vid-attribute (node)
  (integer-value-of-attribute node "vid"))

(defun line-and-column (line-and-column-node)
  (values (value-of-line-attribute line-and-column-node)
	  (value-of-col-attribute line-and-column-node)))

(defun first-child-with-name (node name)
  (xpath:first-node (xpath:evaluate (format nil "~A[position()=1]" name) node)))

(defun last-child-with-name (node name)
  (xpath:first-node (xpath:evaluate (format nil "~A[position()=last()]" name) node)))

;;; xml-utils.lisp ends here