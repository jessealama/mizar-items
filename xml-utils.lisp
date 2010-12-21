;;; xml-util.lisp Utilities for working with XML, XPath, etc.

(in-package :mizar)

(defun value-of-name-attribute (node)
  (let ((name-attribute-as-node-set (xpath:evaluate "@name" node)))
    (let ((name-attribute (first (xpath:all-nodes name-attribute-as-node-set))))
      (xpath:evaluate "string()" name-attribute))))

(defun integer-value-of-attribute (node attribute)
  (let* ((xpath (format nil "@~A" attribute))
	 (value (xpath:evaluate xpath node)))
    (unless (xpath:node-set-empty-p value)
      (parse-integer (dom:get-attribute node attribute)))))

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

(defun next-non-blank-sibling (node)
  (let ((next (dom:next-sibling node)))
    (if next
	(if (dom:text-node-p next)
	    (let ((next-next (dom:next-sibling next)))
	      (or next-next
		  (error "After the text sibling of ~S, there are no further nodes" node)))
	    next)
	(error "No node follows ~S in the document" node))))

(defun proof-after-proposition (proposition-node)
  "Get the following Proof node following PROPOSITION-NODE.  Ensure
that we didn't fall off the end of the document, and that the
next (non-text) node really is a Proof node.  Return nil otherwise."
  (let ((next (next-non-blank-sibling proposition-node)))
    (if (string= (dom:local-name next) "Proof")
	next
	(error "The next non-blank sibling after ~S is not a proof node (it is ~S)" proposition-node next))))

(defun by-or-from-after-proposition (proposition-node)
  "Get the following By node or From node following PROPOSITION-NODE.
Ensure that we didn't fall off the end of the document, and that the
next (non-text) node really is a By or From node.  Return nil otherwise."
  (let ((next (dom:next-sibling proposition-node)))
    (when next
      (let ((next-next (dom:next-sibling next))) ; this is probably a proof node...
	(and (or (string= (dom:local-name next-next) "By")
		 (string= (dom:local-name next-next) "From"))
	     next-next)))))

(defun deftheorems-after-definitionblock (definitionblock-node)
  "Get all the DefTheorem nodes that follow DEFINITIONBLOCK-NODE."
  (let (deftheorem-nodes)
    ;; first skip over all the Definiens nodes after DEFINITIONBLOCK-NODE
    (let ((next (next-non-blank-sibling definitionblock-node)))
      (while (string= (dom:local-name next) "Definiens")
	(setf next (next-non-blank-sibling next)))
      (while (string= (dom:local-name next) "DefTheorem")
	(push next deftheorem-nodes)
	(setf next (next-non-blank-sibling next))))
    (reverse deftheorem-nodes)))

(defun all-by-descendents (node)
  (xpath:all-nodes (xpath:evaluate ".//By" node)))

(defun all-from-descendents (node)
  (xpath:all-nodes (xpath:evaluate ".//From" node)))

(defun all-ref-descendents (node)
  (xpath:all-nodes (xpath:evaluate ".//Ref" node)))

;;; xml-utils.lisp ends here