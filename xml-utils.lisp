;;; xml-util.lisp Utilities for working with XML, XPath, etc.

(in-package :mizar)

(defun value-of-name-attribute (node)
  (let ((name-attribute-as-node-set (xpath:evaluate "@name" node)))
    (let ((name-attribute (first (xpath:all-nodes name-attribute-as-node-set))))
      (xpath:evaluate "string()" name-attribute))))

(defun value-of-kind-attribute (node)
  (let ((kind-attribute-as-node-set (xpath:evaluate "@kind" node)))
    (let ((kind-attribute (first (xpath:all-nodes kind-attribute-as-node-set))))
      (xpath:evaluate "string()" kind-attribute))))

(defun value-of-constrkind-attribute (node)
  (let ((kind-attribute-as-node-set (xpath:evaluate "@constrkind" node)))
    (let ((kind-attribute (first (xpath:all-nodes kind-attribute-as-node-set))))
      (xpath:evaluate "string()" kind-attribute))))

(defun value-of-aid-attribute (node)
  (let ((value (xpath:evaluate "@aid" node)))
    (unless (xpath:node-set-empty-p value)
      (dom:get-attribute node "aid"))))

(defun integer-value-of-attribute (node attribute)
  (let* ((xpath (format nil "@~A" attribute))
	 (value (xpath:evaluate xpath node)))
    (unless (xpath:node-set-empty-p value)
      (parse-integer (dom:get-attribute node attribute)))))

(defun value-of-nr-attribute (node)
  (integer-value-of-attribute node "nr"))

(defun value-of-constrnr-attribute (node)
  (integer-value-of-attribute node "constrnr"))

(defun value-of-absnr-attribute (node)
  (integer-value-of-attribute node "absnr"))

(defun value-of-articlenr-attribute (node)
  (integer-value-of-attribute node "articlenr"))

(defun value-of-line-attribute (node)
  (integer-value-of-attribute node "line"))

(defun value-of-col-attribute (node)
  (integer-value-of-attribute node "col"))

(defun value-of-vid-attribute (node)
  (integer-value-of-attribute node "vid"))

(defun value-of-schemenr-attribute (node)
  (integer-value-of-attribute node "schemenr"))

(defun line-and-column (line-and-column-node)
  (values (value-of-line-attribute line-and-column-node)
	  (value-of-col-attribute line-and-column-node)))

(defun first-child-with-name (node name)
  (xpath:first-node (xpath:evaluate (format nil "~A[position()=1]" name) node)))

(defun last-child-with-name (node name)
  (xpath:first-node (xpath:evaluate (format nil "~A[position()=last()]" name) node)))

(defun next-non-blank-sibling (node)
  (let ((next (dom:next-sibling node)))
    (when next
      (if (dom:text-node-p next)
	  (dom:next-sibling next)
	  next))))

(defun last-non-blank-node? (node)
  (null (next-non-blank-sibling node)))

(defun node-followed-by-proof (node)
  (let ((next (next-non-blank-sibling node)))
    (and next
	 (string= (dom:local-name next) "Proof"))))

(defun node-followed-by-by-or-from (node)
  (let ((next (next-non-blank-sibling node)))
    (and next
	 (or (string= (dom:local-name next) "By")
	     (string= (dom:local-name next) "From")))))

(defun proposition-ref-bearer (proposition-node)
  (cond ((last-non-blank-node? proposition-node)
	 proposition-node)
	((node-followed-by-proof proposition-node)
	 (proof-after-proposition proposition-node))
	((node-followed-by-by-or-from proposition-node)
	 (next-non-blank-sibling proposition-node))
	(t
	 proposition-node)))

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
	(when (or (string= (dom:local-name next-next) "By")
		 (string= (dom:local-name next-next) "From"))
	  next-next)))))

(defun deftheorems-after-definitionblock (definitionblock-node)
  "Get all the DefTheorem nodes that follow DEFINITIONBLOCK-NODE."
  (let (deftheorem-nodes)
    ;; first skip over all the Definiens nodes after DEFINITIONBLOCK-NODE
    (let ((next (next-non-blank-sibling definitionblock-node)))
      (when next
	(while (and next (string= (dom:local-name next) "Definiens"))
	  (setf next (next-non-blank-sibling next)))
	(while (and next (string= (dom:local-name next) "DefTheorem"))
	  (push next deftheorem-nodes)
	  (setf next (next-non-blank-sibling next)))))
    (reverse deftheorem-nodes)))

(defun definiens-after-definitionblock (definitionblock-node)
  "Get all the Definiens nodes that follow DEFINITIONBLOCK-NODE."
  (let (definiens-nodes)
    ;; first skip over all the Definiens nodes after DEFINITIONBLOCK-NODE
    (let ((next (next-non-blank-sibling definitionblock-node)))
      (when next
	(while (and next (string= (dom:local-name next) "Definiens"))
	  (push next definiens-nodes)
	  (setf next (next-non-blank-sibling next)))))
    (reverse definiens-nodes)))

(defmacro descendents-with-name (node name)
  `(labels ((descendents (node)
	      (if (string= (dom:local-name node) ,name)
		  (list node)
		  (reduce #'append (map 'list #'descendents (remove-if #'dom:text-node-p (dom:child-nodes node)))))))
     (descendents ,node)))

(defun all-by-descendents (node)
  (descendents-with-name node "By"))

(defun all-from-descendents (node)
  (descendents-with-name node "From"))

(defun all-ref-descendents (node)
  (descendents-with-name node "Ref"))

(defun article-local-froms (node)
  (remove-if-not #'(lambda (node)
		     (string= (dom:get-attribute node "articlenr") "0"))
		 (all-from-descendents node)))


(defun all-vid-attribute-values (node)
  (mapcar #'(lambda (attribute)
	      (parse-integer (xpath:evaluate "string()" attribute)))
	  (xpath:all-nodes (xpath:evaluate ".//@vid" node))))

(defun reservation-nodes (toplevel-node)
  (xpath:all-nodes (xpath:evaluate "Article/Reservation" toplevel-node)))

;;; xml-utils.lisp ends here