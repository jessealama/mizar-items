;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Computing candidate items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first-keyword-before (article keyword line-num col-num)
  "Look for the first occurence of the keyword KEYWORD (e.g.,
'theorem', 'definition', or possibly a label such as 'Lm3') before
LINE-NUM and COL-NUM in the text of ARTICLE."
  ; assume the keyword always begins a line, possibly with whitespace
  ; -- a terrible assumption
  (let ((bol-theorem-scanner (create-scanner (format nil "^( *)~A$|^( *)~A[: ]" keyword keyword))))
    (loop for l from line-num downto 1
	  for line = (line-at article l)
       do
	 (multiple-value-bind (begin end registers-begin registers-end)
	     (scan bol-theorem-scanner line)
	   (declare (ignore end))
	   (when begin
	     (if (null (aref registers-begin 0))
		 (return (values l (aref registers-end 1)))
		 (return (values l (aref registers-end 0))))))
       finally
	 (error "We didn't find the required keyword ~A before line ~d and column ~d in article ~S"
		keyword line-num col-num article))))

(defun first-keyword-after (article keyword line-num col-num)
  "Look for the first occurence of the keyword KEYWORD (e.g.,
'theorem', 'definition', or possibly a label such as 'Lm3') before
LINE-NUM and COL-NUM in the text of ARTICLE."
  ; assume the keyword always begins a line, possibly with whitespace
  ; -- a terrible assumption
  (let ((bol-theorem-scanner (create-scanner (format nil "^( *)~A$|^( *)~A[: ]*" keyword keyword))))
    (loop for l from line-num upto (num-lines article)
	  for line = (line-at article l)
       do
	 (multiple-value-bind (begin end registers-begin registers-end)
	     (scan bol-theorem-scanner line :start (if (= l line-num)
						       col-num
						       0))
	   (declare (ignore end))
	   (when begin
	     (if (null (aref registers-begin 0))
		 (return (values l (aref registers-end 1)))
		 (return (values l (aref registers-end 0))))))
       finally
	 (error "We didn't find the required keyword ~A after line ~d and column ~d in article ~S"
		keyword line-num col-num article))))

(defun first-theorem-keyword-before (article line-num col-num)
  (first-keyword-before article "theorem" line-num col-num))

(defun canceled-items (article)
  (with-slots (xml-doc)
      article
    (let (items)
      (xpath:do-node-set (canceled-node (xpath:evaluate "Article/JustifiedTheorem[SkippedProof]" xml-doc))
	(let ((prop-child (first-child-with-name canceled-node "Proposition")))
	  (multiple-value-bind (begin-line begin-col)
	      (line-and-column prop-child)
	    (push (make-instance 'canceled-item
				 :source-article article
				 :begin-line-number begin-line
				 :begin-column-number begin-col
				 :end-line-number begin-line
				 :end-column-number (+ begin-col 9) ; "canceled;"
				 :node canceled-node)
		  items))))
      (reverse items))))

(defun justifiedtheorem-items (article)
  (with-slots (xml-doc)
      article
    (let (items)
      (xpath:do-node-set (justifiedtheorem-node (xpath:evaluate "Article/JustifiedTheorem[not(SkippedProof)]" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (let ((prop-node (first-child-with-name justifiedtheorem-node "Proposition")))
	    (let* ((vid (value-of-vid-attribute prop-node))
		   (nr (value-of-nr-attribute prop-node))
		   (label (label-for-vid article vid))
		   (absnr (value-of-nr-attribute justifiedtheorem-node)))
	      (multiple-value-bind (almost-begin-line-num almost-begin-col-num)
		  (line-and-column prop-node)
		(multiple-value-setq (begin-line-num begin-column-num)
		  (first-theorem-keyword-before article almost-begin-line-num almost-begin-col-num))
		(let ((proof-node (first-child-with-name justifiedtheorem-node "Proof")))
		  (if proof-node
		      (let ((last-endposition-child (last-child-with-name proof-node "EndPosition")))
			(multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child)))
		      (let ((by-or-from (xpath:first-node (xpath:evaluate "By | From" justifiedtheorem-node))))
			(if by-or-from
			    (let ((last-ref-node (last-child-with-name by-or-from "Ref")))
			      (if last-ref-node
				  (multiple-value-setq (end-line-num end-column-num) (line-and-column last-ref-node))
				  (multiple-value-setq (end-line-num end-column-num) (line-and-column by-or-from))))
			    (multiple-value-setq (end-line-num end-column-num) prop-node))))))
	      (push (make-instance 'theorem-item
				   :source-article article
				   :begin-line-number begin-line-num
				   :begin-column-number begin-column-num
				   :end-line-number end-line-num
				   :end-column-number (1+ end-column-num) ; to get the ';'
				   :nr nr
				   :absnr absnr
				   :vid vid
				   :label label
				   :node justifiedtheorem-node)
		  items)))))
	(reverse items))))

(defun proposition-items (article)
  (with-slots (xml-doc)
      article
    (let (items)
      (xpath:do-node-set (proposition-node (xpath:evaluate "Article/Proposition" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (let* ((vid (value-of-vid-attribute proposition-node))
		 (nr (value-of-nr-attribute proposition-node))
		 (label (label-for-vid article vid)))
	    (multiple-value-bind (almost-begin-line-num almost-begin-col-num)
		(line-and-column proposition-node)
	      (multiple-value-setq (begin-line-num begin-column-num)
		(first-keyword-before article (format nil "~A:" label) almost-begin-line-num almost-begin-col-num))
	      (let ((next (next-non-blank-sibling proposition-node)))
		(if next
		    (let ((next-name (dom:local-name next)))
		      (cond ((string= next-name "Proof")
			     (let ((proof-node (proof-after-proposition proposition-node)))
			       (let ((last-endposition-child (last-child-with-name proof-node "EndPosition")))
				 (multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child)))))
			    ((or (string= next-name "By")
				 (string= next-name "From"))
			     (let ((by-or-from (by-or-from-after-proposition proposition-node)))
			       (let ((last-ref-node (last-child-with-name by-or-from "Ref")))
				 (if last-ref-node
				     (multiple-value-setq (end-line-num end-column-num) (line-and-column last-ref-node))
				     (multiple-value-setq (end-line-num end-column-num) (line-and-column by-or-from))))))
			    (t
			     (error "Unknown next sibling after proposition node: ~S" next))))
		    (error "This proposition node (~S) is the final non-blank node of the entire document, but lacks a Proof nor is it immediately justified by a By or From statement" proposition-node))))
	    (push (make-instance 'proposition-item
			       :source-article article
			       :begin-line-number begin-line-num
			       :begin-column-number begin-column-num
			       :end-line-number end-line-num
			       :end-column-number (1+ end-column-num) ; to get the final ';'
			       :nr nr
			       :vid vid
			       :label label
			       :node proposition-node)
		  items))))
	(reverse items))))

(defun block-items (xml-element-name mizar-keyword class-name article)
  (with-slots (xml-doc)
      article
    (let (items)
      (xpath:do-node-set (block-node (xpath:evaluate (format nil "Article/~A" xml-element-name) xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (multiple-value-bind (almost-begin-line-num almost-begin-col-num)
	      (line-and-column block-node)
	      (multiple-value-setq (begin-line-num begin-column-num)
		(first-keyword-before article mizar-keyword almost-begin-line-num almost-begin-col-num))
	      (let ((last-endposition-child (last-child-with-name block-node "EndPosition")))
		(multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child))
		(push (make-instance class-name
				     :source-article article
				     :begin-line-number begin-line-num
				     :begin-column-number begin-column-num
				     :end-line-number end-line-num
				     :end-column-number (if (string= xml-element-name "SchemeBlock") ; Schemes are special
							    end-column-num
							    (1+ end-column-num))
				     :node block-node)
		    items)))))
    (reverse items))))

(defun definition-kind->keyword (definition-kind)
  (cond ((string= definition-kind "K") "func")
	((string= definition-kind "R") "pred")
	((string= definition-kind "M") "mode")
	((string= definition-kind "V") "attr")
	((string= definition-kind "G") "struct")))

(defun attribute-or-predicate (definition-kind)
  (or (string= definition-kind "R")
      (string= definition-kind "V")))

(defun split-definitionblock-node (definitionblock-node article)
  (flet ((keyword-for-item-kind (item-name redefinition? item-kind)
	   (cond ((string= item-name "Definition")
		  (if (string= redefinition? "true")
		      "redefine"
		      (definition-kind->keyword item-kind)))
		 ((string= item-name "Canceled") "canceled")
		 ((string= item-name "Let") "let")
		 ((string= item-name "Assume") "assume")
		 ((string= item-name "DefFunc") "deffunc")
		 (t (error "Unknown node type following a Definition: '~A'" item-name)))))
    (loop
       with new-definitions = nil
       with definition-nodes = (xpath:all-nodes (xpath:evaluate "Definition | Canceled" definitionblock-node))
       with num-definition-nodes = (length definition-nodes)
       with last-line = nil
       with last-col = nil
       with block-end-line = nil
       with block-end-col = nil
       with context = nil
       with endposition-child = (car (last (xpath:all-nodes (xpath:evaluate "EndPosition[position()=last()]" definitionblock-node))))
       for i from 1
       for definition-node in definition-nodes
       for definition-kind = (value-of-kind-attribute definition-node)
       for redefinition = (value-of-redefinition-attribute definition-node)
       for expandable = (value-of-expandable-attribute definition-node)
       for node-type = (dom:local-name definition-node)
       for sibling = (next-non-blank-sibling definition-node)
       for sibling-kind = (value-of-kind-attribute sibling)
       for sibling-name = (dom:local-name sibling)
       for sibling-redefinition = (value-of-redefinition-attribute sibling)
       initially
	 (warn "we have to consider ~d items" num-definition-nodes)
	 (multiple-value-setq (last-line last-col)
	   (line-and-column definitionblock-node))
	 (multiple-value-setq (block-end-line block-end-col)
	   (line-and-column endposition-child))
       do
         ;; first find the beginning of this definition/canceled
	 (let (begin-line-num begin-col-num end-line-num end-col-num)
	   (multiple-value-setq (begin-line-num begin-col-num)
	     (if (and (string= definition-kind "M") (string= expandable "true"))
		 (first-keyword-after article "mode" last-line last-col)
		 (multiple-value-bind (candidate-begin-line-num candidate-begin-col-num)
		     (line-and-column definition-node)
		   (warn "candidate-begin-line = ~d and candidate-begin-col = ~d" candidate-begin-line-num candidate-begin-col-num)
		   (if (and candidate-begin-line-num candidate-begin-col-num)
		       (first-keyword-before article
					     (if (string= redefinition "true")
						 "redefine"
						 (definition-kind->keyword definition-kind))
					     candidate-begin-line-num
					     candidate-begin-col-num)
		       (first-keyword-after article
					    (if (string= node-type "Canceled")
						"canceled"
						(if (string= redefinition "true")
						    "redefine"
						    (definition-kind->keyword definition-kind)))
					    last-line
					    last-col)))))
	   ;; now that we've found the beginning, let's grab the text
	   (push (maybe-strip-semicolon
		  (string-trim
		   (list #\Space #\Newline)
		   (region article last-line last-col begin-line-num begin-col-num)))
		 context)
	   (if (attribute-or-predicate definition-kind)
	       (if (= i num-definition-nodes)
		   (setf end-line-num block-end-line
			 end-col-num (- block-end-col 3)) ; remove "end"
		   (let ((keyword-to-look-for (keyword-for-item-kind sibling-name sibling-redefinition sibling-kind)))
		     (multiple-value-setq (end-line-num end-col-num)
		       (first-keyword-after article
					    keyword-to-look-for
					    begin-line-num
					    (1+ begin-col-num)))
		     (warn "end-line = ~d and end-col is ~d" end-line-num end-col-num)))
	       (let ((last-line-and-col (last (descendents-with-line-and-column definition-node))))
		 (if last-line-and-col
		     (let ((last-line-and-col-node (first last-line-and-col)))
		       (multiple-value-setq (end-line-num end-col-num)
			 (line-and-column last-line-and-col-node))
		       (when (string= (dom:local-name last-line-and-col-node) "From") ; need to adjust forwards
			 (multiple-value-bind (next-word-begin next-word-end)
			     (scan "[^;]+" (line-at article end-line-num) :start end-col-num)
			   (declare (ignore next-word-begin))
			   (setf end-col-num next-word-end)
			   (warn "adjusting! real end is line ~d and col ~d" end-line-num end-col-num))))
		     (if (= i num-definition-nodes)
			 (setf end-line-num block-end-line
			       end-col-num (- block-end-col 3)) ; remove "end"
			 (progn
			   (multiple-value-setq (end-line-num end-col-num)
			     (first-keyword-after article
						  (keyword-for-item-kind sibling-name sibling-redefinition sibling-kind)
						  begin-line-num
						  (1+ begin-col-num)))
			   (if (zerop end-col-num)
			       (setf end-line-num (1- end-line-num)
				     end-col-num (length (line-at article
								  (1- end-line-num))))
			       (setf end-col-num (1- end-col-num))))))))
	   (setf last-line end-line-num
		 last-col end-col-num)
	   (push (ensure-final-semicolon
		  (concat (if (= i 1)
			      (format nil "~%")
			      (format nil "definition~%"))
			  (reduce #'concat (reverse context))
			  (format nil "~%")
			  (ensure-final-semicolon
			   (string-trim
			    '(#\Space #\Newline)
			    (region article
				    begin-line-num
				    begin-col-num
				    end-line-num
				    end-col-num)))
			  (format nil "~%end;")))
		 new-definitions))
       finally (return (reverse new-definitions)))))

(Defun definitionblock-items (article)
  "Definitions are a special case.  They can generate DefTheorem
sibling elements; these need to be stored."
  (let ((items (block-items "DefinitionBlock" "definition" 'definition-item article)))
    (dolist (definitionblock-item items items)
      (let ((definitionblock-node (xml-node definitionblock-item)))
	(let (deftheorem-items)
	  (let ((deftheorem-nodes (deftheorems-after-definitionblock definitionblock-node)))
	    (dolist (deftheorem-node deftheorem-nodes deftheorem-items)
	      ;; we want the vid and nr of the DefTheorem's Proposition
	      ;; child element; the nr and vid of the DefTheorem itself are
	      ;; useless.
	      (let* ((proposition-child (first-child-with-name deftheorem-node "Proposition"))
		     (nr (value-of-nr-attribute proposition-child))
		     (absnr (value-of-nr-attribute deftheorem-node))
		     (vid (value-of-vid-attribute proposition-child))
		     (label (label-for-vid article vid)))
		(push (make-instance 'deftheorem-item
				     :source definitionblock-item
				     :node deftheorem-node
				     :nr nr
				     :absnr absnr
				     :vid vid
				     :label label)
		      deftheorem-items))))
	(setf (deftheorems definitionblock-item) (reverse deftheorem-items)))
	; register definiens
	(let ((definiens-nodes (definiens-after-definitionblock definitionblock-node)))
	  (dolist (definiens-node definiens-nodes)
	    (let ((constrkind (value-of-constrkind-attribute definiens-node))
		  (constrnr (value-of-constrnr-attribute definiens-node)))
	      (push (cons constrkind constrnr) (definientia definitionblock-item)))))
	; now gather any Constructor and Format child elements
	(dolist (definition-node (xpath:all-nodes (xpath:evaluate "Definition" definitionblock-node)))
	  (let ((patterns-for-definition (xpath:all-nodes (xpath:evaluate "Pattern" definition-node)))
		(constructors-for-definition (xpath:all-nodes (xpath:evaluate "Constructor" definition-node))))
	    (dolist (pattern-for-definition patterns-for-definition)
	      (let ((kind (value-of-kind-attribute pattern-for-definition))
		    (nr (value-of-nr-attribute pattern-for-definition)))
		(push (cons kind nr) (definition-patterns definitionblock-item))))
	    (dolist (constructor-for-definition constructors-for-definition)
	      (let ((kind (value-of-kind-attribute constructor-for-definition))
		    (nr (value-of-nr-attribute constructor-for-definition)))
		(push (cons kind nr) (definition-constructors definitionblock-item))))))))))

(defun schemeblock-items (article)
  (let ((items (block-items "SchemeBlock" "scheme" 'scheme-item article)))
    (dolist (item items items)
      (let* ((xml (xml-node item))
	     (schemenr (value-of-schemenr-attribute xml))
	     (vid (value-of-vid-attribute xml))
	     (label (label-for-vid article vid)))
	(setf (schemenr item) schemenr)
	(setf (label item) label)))))

(defun split-registrationblock-node (registrationblock-node article)
  (loop
     with new-registrations = nil
     with registration-nodes = (xpath:all-nodes (xpath:evaluate "Registration | IdentifyRegistration" registrationblock-node))
     with last-line = nil
     with last-col = nil
     with context = nil
     for i from 1
     for registration-node in registration-nodes
     do
       (when (= i 1)
	 (multiple-value-setq (last-line last-col)
	   (line-and-column registrationblock-node)))
       (let (candidate-begin-line-num candidate-begin-col-num
	     begin-line-num begin-col-num end-line-num end-col-num )
	 (let ((correctness-node (xpath:first-node (xpath:evaluate "*[position()=2]" registration-node))))
	   (if correctness-node
	       (let ((proposition-child (first-child-with-name correctness-node
							       "Proposition")))
		 (multiple-value-setq (candidate-begin-line-num candidate-begin-col-num)
		   (line-and-column proposition-child)))
	       (error "This registration node lacks a correctness node!")))
	 (multiple-value-setq (begin-line-num begin-col-num)
	   (first-keyword-before article
				 (if (string= (dom:local-name registration-node)
					      "Registration")
				     "cluster"
				     "identify")
				 candidate-begin-line-num
				 candidate-begin-col-num))
	 (push (maybe-strip-semicolon
		(string-trim
		 (list #\Space #\Newline)
		 (region article last-line last-col begin-line-num begin-col-num)))
	       context)
	 (let ((last-line-and-col (last (descendents-with-line-and-column registration-node))))
	   (if last-line-and-col
	       (let ((last-line-and-col-node (first last-line-and-col)))
		 (multiple-value-setq (end-line-num end-col-num)
		   (line-and-column last-line-and-col-node)))
	       (error "We found a Registration node that lacks descendents with line and column information")))
	 (setf last-line end-line-num
	       last-col end-col-num)
	 (push (ensure-final-semicolon
		(concat (if (= i 1)
			    (format nil "~%")
			    (format nil "registration~%"))
			(reduce #'concat (reverse context))
			(ensure-final-semicolon
			 (string-trim
			  '(#\Space #\Newline)
			  (region article
				  begin-line-num
				  begin-col-num
				  end-line-num
				  end-col-num)))
			(format nil "~%end;")))
	       new-registrations))
  finally (return (reverse new-registrations))))

(defun split-notationblock-node (notationblock-node article)
  (loop
     with new-notations = nil
     with notation-nodes = (xpath:all-nodes (xpath:evaluate "Pattern" notationblock-node))
     with last-line = nil
     with last-col = nil
     with context = nil
     for i from 1
     for notation-node in notation-nodes
     do
       (when (= i 1)
	 (multiple-value-setq (last-line last-col)
	   (line-and-column notationblock-node)))
       (let (candidate-begin-line-num candidate-begin-col-num
	     begin-line-num begin-col-num end-line-num end-col-num)
	 (multiple-value-setq (candidate-begin-line-num candidate-begin-col-num)
	   (line-and-column notationblock-node))
	 (multiple-value-setq (begin-line-num begin-col-num)
	   (first-keyword-before article
				 "antonym|synonym"
				 candidate-begin-line-num
				 candidate-begin-col-num))
	 (push (maybe-strip-semicolon
		(string-trim
		 (list #\Space #\Newline)
		 (region article last-line last-col begin-line-num begin-col-num)))
	       context)
	 (let ((last-line-and-col (last (descendents-with-line-and-column notation-node))))
	   (if last-line-and-col
	       (let ((last-line-and-col-node (first last-line-and-col)))
		 (multiple-value-setq (end-line-num end-col-num)
		   (line-and-column last-line-and-col-node)))
	       (error "We found a Notation node that lacks descendents with line and column information")))
	 (setf last-line end-line-num
	       last-col end-col-num)
	 (push (ensure-final-semicolon
		(concat (if (= i 1)
			    (format nil "~%")
			    (format nil "notation~%"))
			(reduce #'concat (reverse context))
			(ensure-final-semicolon
			 (string-trim
			  '(#\Space #\Newline)
			  (region article
				  begin-line-num
				  begin-col-num
				  end-line-num
				  end-col-num)))
			(format nil "~%end;")))
	       new-notations))
  finally (return (reverse new-notations))))

(defun multi-part-node-p (node)
  (let ((name (dom:local-name node)))
    (cond ((string= name "DefinitionBlock")
	   (> (length 
	       (xpath:all-nodes (xpath:evaluate "Definition | Canceled" node)))
	      1))
	  ((string= name "RegistrationBlock")
	   (> (length 
	       (xpath:all-nodes (xpath:evaluate "Registration | Canceled" node)))
	      1))
	  ((string= name "NotationBlock")
	   (> (length 
	       (xpath:all-nodes (xpath:evaluate "Pattern" node)))
	      1))
	  (t
	   (error "Unknown node type: '~A'" name)))))

(defun split-multipart-node (node article)
  (let ((name (dom:local-name node)))
    (cond ((string= name "DefinitionBlock")
	   (split-definitionblock-node node article))
	  ((string= name "RegistrationBlock")
	   (split-registrationblock-node node article))
	  ((string= name "NotationBlock")
	   (split-notationblock-node node article))
	  (t
	   (error "Unknown node type: '~A'" name)))))

(defun registrationblock-items (article)
  (let ((items (block-items "RegistrationBlock" "registration" 'registration-item article)))
    (dolist (item items items)
      (let ((item-xml (xml-node item)))
	(dolist (registration (xpath:all-nodes (xpath:evaluate "Registration" item-xml)))
	  (dolist (rcluster (xpath:all-nodes (xpath:evaluate "RCluster" registration)))
	    (let ((nr (value-of-nr-attribute rcluster)))
	      (push nr (rclusters item))))
	  (dolist (fcluster (xpath:all-nodes (xpath:evaluate "FCluster" registration)))
	    (let ((nr (value-of-nr-attribute fcluster)))
	      (push nr (fclusters item))))
	  (dolist (ccluster (xpath:all-nodes (xpath:evaluate "CCluster" registration)))
	    (let ((nr (value-of-nr-attribute ccluster)))
	      (push nr (cclusters item)))))
	(dolist (identify-registration (xpath:all-nodes (xpath:evaluate "IdentifyRegistration" item-xml)))
	  (dolist (identify (xpath:all-nodes (xpath:evaluate "Identify" identify-registration)))
	    (let ((nr (value-of-nr-attribute identify))
		  (constrkind (value-of-constrkind-attribute identify)))
	      (push (cons constrkind nr) (identifications item)))))))))

(defun notationblock-items (article)
  (let ((items (block-items "NotationBlock" "notation" 'notation-item article)))
    (dolist (item items items)
      (let ((item-xml (xml-node item)))
	(dolist (pattern-node (xpath:all-nodes (xpath:evaluate "Pattern" item-xml)))
	  (let ((kind (value-of-kind-attribute pattern-node))
		(nr (value-of-nr-attribute pattern-node)))
	    (push (cons kind nr) (patterns item))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass editing-instruction ()
  ((old-label
    :initarg :old-label
    :accessor old-label
    :type string)
   (new-label
    :initarg :new-label
    :accessor new-label
    :type string)
   (target-line-number
    :initarg :target-line-number
    :accessor target-line-number
    :type integer)
   (target-column-number
    :initarg :target-column-number
    :accessor target-column-number
    :type integer)))

(defmethod print-object ((instruction editing-instruction) stream)
  (with-slots (old-label new-label target-line-number target-column-number)
      instruction
    (print-unreadable-object (instruction stream :type nil)
      (format stream "~A ==> ~A (line ~d column ~d)" old-label new-label target-line-number target-column-number))))

(defun instruction-> (editing-instruction-1 editing-instruction-2)
  (with-slots ((l-1 target-line-number) (c-1 target-column-number))
      editing-instruction-1
    (with-slots ((l-2 target-line-number) (c-2 target-column-number))
	editing-instruction-2
      (tuple-lex-less (list l-2 c-2) (list l-1 c-1)))))

(defun definition-editing-instructions (item definition-table items->articles itemization)
  (let* ((item-xml-node (xml-node item))
	 (ref-nodes (if (typep item 'proposition-item)
			(all-ref-descendents (proposition-ref-bearer item-xml-node))
			(all-ref-descendents item-xml-node)))
	 (instructions nil))
    (dolist (ref-node ref-nodes instructions)
      (let (old-label new-label)
	(multiple-value-bind (ref-line-num ref-col-num)
	    (line-and-column ref-node)
	  (when (and ref-line-num ref-col-num)
	    (let ((aid (value-of-aid-attribute ref-node)))
	      ;; (warn "aid is '~A', length ~d" aid (length aid))
	      (if (null aid) ; article-local definition reference
		  (let ((nr (value-of-nr-attribute ref-node))
			(vid (value-of-vid-attribute ref-node)))
		    (when (and nr vid)
		      ;; (warn "nil case, with nr ~d and vid ~d" nr vid)
		      (let ((deftheorem-item (gethash (cons nr vid) definition-table)))
			(when deftheorem-item
			  (let ((definition-item (source deftheorem-item))
				(deftheorem-label (label deftheorem-item)))
			    (if definition-item
				(let* ((deftheorems (deftheorems definition-item))
				       (index (position deftheorem-item deftheorems)))
				  (if index
				      (if deftheorem-label
					  (let ((article-for-definition-item (gethash definition-item items->articles)))
					    (if article-for-definition-item
						(let* ((name (name article-for-definition-item)))
						  (setf old-label deftheorem-label
							new-label (format nil "~:@(~A~):def ~d" name (1+ index))))
						(error "No article is associated with ~S in the item-to-article table"
						       definition-item)))
					  (error "The deftheorem item ~S lacks a label!" deftheorem-item))
				      (error "The deftheorem item ~S cannot be found among the deftheorem items for the definition item ~S" deftheorem-item definition-item)))
				(error "The deftheorem item ~S does not have a source definition item!" deftheorem-item)))))))
		  (let ((absnr (value-of-absnr-attribute ref-node)))
		    (if (null absnr)
			(error "We found a Ref node that has an aid attribute (~A, length ~d), but lacks a value for its absnr attribute" aid (length aid))
			(let ((item-number-and-def-number (gethash (cons aid absnr) (definition-labels-to-items itemization))))
			  (if (null item-number-and-def-number)
			      (setf old-label (format nil "~:@(~A~):def ~d" aid absnr)
				    new-label (format nil "~:@(~A~):def ~d" aid absnr))
			      (destructuring-bind (item-number . def-number)
				  item-number-and-def-number
				(when (or (null item-number) (null def-number))
				  (error "There is no entry in the definition labels-to-items table for (~d . ~d)" aid absnr))
				(setf old-label (format nil "~:@(~A~):def ~d" aid absnr)
				      new-label (format nil "ckb~d:def ~d" item-number def-number)))))))))
	    ;; (warn "old label is '~A', new label is '~A'" old-label new-label)
	    (when (and old-label new-label)
	      (push (make-instance 'editing-instruction
				   :old-label old-label
				   :new-label new-label
				   :target-line-number ref-line-num
				   :target-column-number (- ref-col-num (length old-label)))
		    instructions))))))))

(defun theorem-editing-instructions (item theorem-table items->articles itemization)
  (declare (ignore itemization))
  (let* ((item-xml-node (xml-node item))
	 (ref-nodes (if (typep item 'proposition-item)
			(all-ref-descendents (proposition-ref-bearer item-xml-node))
			(all-ref-descendents item-xml-node)))
	 (instructions nil))
    (dolist (ref-node ref-nodes instructions)
      (multiple-value-bind (ref-line-num ref-col-num)
	  (line-and-column ref-node)
	(let ((nr (value-of-nr-attribute ref-node))
	      (vid (value-of-vid-attribute ref-node)))
	  (when (and nr vid)
	    (let ((theorem-item (gethash (cons nr vid) theorem-table)))
	      (when theorem-item
		(let ((theorem-label (label theorem-item)))
		  (if theorem-label
		      (let ((article-for-theorem-item (gethash theorem-item items->articles)))
			(if article-for-theorem-item
			    (let ((name (name article-for-theorem-item)))
			      (let ((instruction (make-instance 'editing-instruction
								:old-label theorem-label
								:new-label (format nil "~:@(~A~):1" name)
								:target-line-number ref-line-num
								:target-column-number (- ref-col-num (length theorem-label)))))
				(push instruction instructions)))
			    (error "No article is associated with ~S in the item-to-article table"
				   theorem-item)))
		      (error "The theorem item ~S lacks a label!" theorem-item)))))))))))

(defun scheme-editing-instructions (item scheme-table items->articles itemization)
  (let* ((item-xml-node (xml-node item))
	 (item-xml-name (dom:local-name item-xml-node))
	 (from-nodes (append (all-from-descendents item-xml-node)
			     (when (string= item-xml-name "Proposition")
			       (let* ((sibling (next-non-blank-sibling item-xml-node))
				      (sibling-name (dom:local-name sibling)))
				 (if (string= sibling-name "From")
				     (list sibling)
				     (if (string= sibling-name "By")
					 nil
					 (all-from-descendents sibling)))))))
	 (instructions nil))
    (dolist (from-node from-nodes instructions)
      (multiple-value-bind (from-line-num from-col-num)
	  (line-and-column from-node)
	(let ((articlenr (value-of-articlenr-attribute from-node))
	      (old-label nil)
	      (new-label nil))
	  (if (zerop articlenr) ; article-internal scheme reference
	      (let ((schemenr (value-of-absnr-attribute from-node)))
		;; (warn "We found an article-internal scheme reference to scheme number ~d" schemenr)
		(if schemenr
		    (let ((scheme-item (gethash schemenr scheme-table)))
		      (if scheme-item
			  (let ((scheme-label (label scheme-item)))
			    (if scheme-label
				(let ((article-for-scheme-item (gethash scheme-item items->articles)))
				  (setf old-label scheme-label)
				  (if article-for-scheme-item
				      (setf new-label (format nil "~:@(~A~):sch 1" (name article-for-scheme-item)))
				      (error "No article is associated with ~S in the item-to-article table"
					     scheme-item)))
				(error "The scheme item ~S lacks a label!" scheme-item)))
			  (error "No scheme item is associated with the scheme number ~S in the scheme table"
				 schemenr)))
		    (error "The From node ~S lacks a value for the schemenr attribute"
			   from-node)))
	      (let ((aid (value-of-aid-attribute from-node))
		    (absnr (value-of-absnr-attribute from-node)))
		;; (warn "We found a scheme editing instruction.  Its aid is ~A and its absnr is ~A" aid absnr)
		(if aid
		    (if absnr
			(let ((earlier-item-number (gethash (cons aid absnr) (scheme-labels-to-items itemization))))
			  (setf old-label (format nil "~:@(~A~):sch ~d" aid absnr))
			  (if earlier-item-number
			      (setf new-label (format nil "CKB~d:sch 1" earlier-item-number))
			      (setf new-label (format nil "~:@(~A~):sch ~d" aid absnr))))
			(error "The From node ~A lacks an absnr attribute!" from-node))
		    (error "The From node ~A lacks an aid attribute!" from-node))))
	  (let ((instruction (make-instance 'editing-instruction
					    :old-label old-label
					    :new-label new-label
					    :target-line-number from-line-num
					    :target-column-number from-col-num)))
	    (push instruction instructions)))))))

(defun editing-instructions (item definition-table theorem-table scheme-table items->articles itemization)
  (append (definition-editing-instructions item definition-table items->articles itemization)
	  (theorem-editing-instructions item theorem-table items->articles itemization)
	  (scheme-editing-instructions item scheme-table items->articles itemization)))

(defun replace-label (old line new start)
  "Like CL-PPCRE:REGEX-REPLACE, but give the result of replacing OLD
by NEW in LINE starting from column START, and not just the substring
of LINE starting from START."
  (concat (subseq line 0 start) 
	  (regex-replace old line new :start start)))

(defun apply-editing-instructions (item instructions lines)
  (loop
     with sorted-instructions = (sort instructions #'instruction->)
     with item-begin-line = (begin-line-number item)
     for instruction in sorted-instructions
     for old-label = (old-label instruction)
     for new-label = (new-label instruction)
     for target-line-number = (target-line-number instruction)
     for target-column-number = (target-column-number instruction)
     for index = (- target-line-number item-begin-line)
     for line = (aref lines index)
     for new-line = (replace-label old-label line new-label (if (<= target-column-number 0)
								0
								(1- target-column-number)))
     do
       (setf (aref lines index) new-line)
     finally (return lines)))

(defun revised-item-text (item theorem-table definition-table scheme-table items->articles itemization)
  (array->newline-delimited-string
   (apply-editing-instructions item
			       (editing-instructions item theorem-table definition-table scheme-table items->articles itemization)
			       (lines-as-array (text item)))))

(defun rewrite-item-text (item theorem-table definition-table scheme-table items->articles itemization)
  (setf (text item)
	(revised-item-text item theorem-table definition-table scheme-table items->articles itemization)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Itemization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun verify-and-export (article directory)
  (accom article directory "-q" "-l" "-s")
  (verifier article directory "-q" "-l" "-s")
  (exporter article directory "-q" "-l" "-s")
  (transfer article directory "-q" "-l" "-s"))

(defun write-new-symbols (symbols symbol-table directory)
  "For each symbol (actually, a string) in SYMBOLS that is not already
  accounted for in SYMBOL-TABLE (i.e., appearing as a key in the
  table), write a vocabulary file under DIRECTORY.  The filenames will
  all have the form SYM<n>.voc, where <n> is a natural number.  Since
  the values of SYMBOL-TABLE are assumed to be numbers, we look at the
  maximum of these numbers, add 1, and start <n> there, increasing it
  by one as we go through the list of symbols in SYMBOLS that do not
  appear as keys in SYMBOL-TABLE.  SYMBOL-TABLE will be modified: any
  symbol in SYMBOLS not appearing in SYMBOL-TABLE as a key will be put
  into SYMBOL-TABLE as a key, with the value corresponding to whatever
  <n> is for the symbol.  The (potentially) modified SYMBOL-TABLE is the final value."
  (let ((vals (values-of-table symbol-table))
	(keys (keys symbol-table))
	(next-symbol-number 1))
    (unless (null vals)
      (setf next-symbol-number (1+ (apply 'max vals))))
    (let ((new-symbols (set-difference symbols keys :test #'string=)))
      (loop
	 for sym in new-symbols
	 for i from next-symbol-number
	 for voc-filename = (format nil "sym~d.voc" i)
	 for voc-path = (concat directory voc-filename)
	 do
	   (with-open-file (sym-file voc-path :direction :output)
	     (format sym-file "~A~%" sym))
	   (setf (gethash sym symbol-table) i)
	 finally
	   (return symbol-table)))))

(defclass itemization ()
  ((sandbox
    :initarg :sandbox
    :accessor sandbox
    :type sandbox)
   (items
    :initarg :items
    :accessor items
    :initform (make-hash-table :test #'eq)) ; keys are natural numbers
   (names-to-items
    :initarg :names-to-items
    :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0"
    :accessor names-to-items
    :type hash-table)
   (definition-labels-to-items
     :initarg :definition-labels-to-items
     :accessor definition-labels-to-items
     :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0:def 2", values are pairs (<item> . <number>)
     :type hash-table)
   (theorem-labels-to-items
    :initarg :theorem-labels-to-items
    :accessor theorem-labels-to-items
    :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0:2", values are item objects
    :type hash-table)
   (scheme-labels-to-items
    :initarg :scheme-labels-to-items
    :accessor scheme-labels-to-items
    :initform (make-hash-table :test #'equal) ; keys are pairs like ("XBOOLE_0" . "2"), values are positive natural numbers
    :type hash-table)
   (symbol-table
    :initarg :symbol-table
    :initform (make-hash-table :test #'equal) ; keys are strings like "Vempty" and "O\ 32", values are numbers
    :accessor symbol-table
    :type hash-table)
   (num-items
    :initform 0
    :type integer
    :accessor num-items)
   (names->notations
    :initarg :names->notations
    :accessor names->notations
    :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0", values are lists of strings like ("ITEM2" "ITEM5")
    :type hash-table)
   (names->constructors
    :initarg :names->constructors
    :accessor names->constructors
    :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0", values are lists of strings like ("ITEM2" "ITEM5")
    :type hash-table)
   (names->registrations
    :initarg :names->registrations
    :accessor names->registrations
    :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0", values are lists of strings like ("ITEM2" "ITEM5")
    :type hash-table)
   (names->definitions
    :initarg :names->definitions
    :accessor names->definitions
    :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0", values are lists of strings like ("ITEM2" "ITEM5")
    :type hash-table)
   (names->theorems
    :initarg :names->theorems
    :accessor names->theorems
    :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0", values are lists of strings like ("ITEM2" "ITEM5")
    :type hash-table)
   (names->schemes
    :initarg :names->schemes
    :accessor names->schemes
    :initform (make-hash-table :test #'equal) ; keys are strings like "XBOOLE_0", values are lists of strings like ("ITEM2" "ITEM5")
    :type hash-table)
   (constructors
    :initarg :constructors
    :accessor constructors
    :initform (make-hash-table :test #'equal) ; keys are triples like ("XBOOLE_0" "V" 1), values are item objects
    :type hash-table)
   (patterns
    :initarg :patterns
    :accessor patterns
    :initform (make-hash-table :test #'equal) ; keys are triples like ("XBOOLE_0" "V" 1), values are item objects
    :type hash-table)
   (definientia
    :initarg :definientia
    :accessor definientia
    :initform (make-hash-table :test #'equal) ; keys are triples like ("XBOOLE_0" "V" 1), values are item objects
    :type hash-table)
   (identifications
    :initarg :identifications
    :accessor identifications
    :initform (make-hash-table :test #'equal) ; keys are triples like ("XBOOLE_0" "V" 1), values are item objects
    :type hash-table)
   (theorems
    :initarg :theorems
    :accessor theorems
    :initform (make-hash-table :test #'equal) ; keys are triples like ("XBOOLE_0" 1), values are item objects
    :type hash-table)
   (schemes
    :initarg :schemes
    :accessor schemes
    :initform (make-hash-table :test #'equal) ; keys are triples like ("XBOOLE_0" 1), values are item objects
    :type hash-table)
   (rclusters
    :initarg :rclusters
    :accessor rclusters
    :initform (make-hash-table :test #'equal) ; keys are pairs like ("XBOOLE_0" 1), values are item objects
    :type hash-table)
   (fclusters
    :initarg :fclusters
    :accessor fclusters
    :initform (make-hash-table :test #'equal) ; keys are pairs like ("XBOOLE_0" 1), values are item objects
    :type hash-table)
   (cclusters
    :initarg :cclusters
    :accessor cclusters
    :initform (make-hash-table :test #'equal) ; keys are pairs like ("XBOOLE_0" 1), values are item objects
    :type hash-table)))

(defun singleton-if-not-present (thing table)
  "Give the value of THING in the hashtable TABLE, if THING is a key; otherwise, return a singleton list containing THING."
  (let ((val (gethash thing table)))
    (if (null val)
	(list thing)
	val)))

(defun expand-previous-notations (article notations-table)
  (let ((notations (notations article)))
    (reduce #'append (mapcar #'(lambda (thing)
				 (singleton-if-not-present thing notations-table))
			     notations))))

(defun expand-previous-constructors (article constructors-table)
  (let ((constructors (constructors article)))
    (reduce #'append (mapcar #'(lambda (thing)
				 (singleton-if-not-present thing constructors-table))
			     constructors))))

(defun expand-previous-registrations (article registrations-table)
  (let ((registrations (registrations article)))
    (reduce #'append (mapcar #'(lambda (thing)
				 (singleton-if-not-present thing registrations-table))
			     registrations))))

(defun expand-previous-definitions (article definitions-table)
  (let ((definitions (definitions article)))
    (reduce #'append (mapcar #'(lambda (thing)
				 (singleton-if-not-present thing definitions-table))
			     definitions))))

(defun expand-previous-theorems (article theorems-table)
  (let ((theorems (theorems article)))
    (reduce #'append (mapcar #'(lambda (thing)
				 (singleton-if-not-present thing theorems-table))
			     theorems))))

(defun expand-previous-schemes (article schemes-table)
  (let ((schemes (schemes article)))
    (reduce #'append (mapcar #'(lambda (thing)
				 (singleton-if-not-present thing schemes-table))
			     schemes))))

(defun itemize-no-errors (article)
  (handler-case
      (progn
	(handler-bind ((warning #'muffle-warning))
	  (itemize article))
	(format t "~a: success~%" article)
	t)
      (error ()
	(format *error-output* "~a: failure~%" article)
	nil)))

(defgeneric xsl-split-article (article)
  (:documentation "Divide any 'multi-part' elements of ARTICLE.  This means:

* divide definition blocks that define multiple things into several 'singleton' definition blocks,
* likewise for notation blocks,
* divide reservations that reserve multiple variables into 'singleton' reservations"))

(defmethod xsl-split-article ((article article))
  (error "We haven't yet defined XSL-SPLIT-ARTICLE for objects of class ARTICLE.  Sorry."))

(defmethod xsl-split-article ((article string))
  "Split an article given as a string.

If ARTICLE is the empty string, signal an error.  If ARTICLE is not the empty string, look at its first character.  If the first character is a forward slash '/', then interpret ARTICLE as a path to an article file on disk, and proceed accordingly.  Otherwise, interpret ARTICLE as the string representation of a MIZAR article, save the article to a temporary location on disk, and proceed as if ARTICLE were that file on disk."
  (if (string= article "")
      (error "We cannot split an empty article!")
      (let ((first-char (char article 0)))
	(if (char= first-char #\/)
	    (xsl-split-article (pathname article))
	    (let ((temp-article-path (temporary-file :extension ".miz")))
	      (with-open-file (temp-article temp-article-path
					    :direction :output
					    :if-exists :error
					    :if-does-not-exist :create)
		(format temp-article "~a" article))
	      (xsl-split-article temp-article-path)
	      (delete-file temp-article-path))))))

(defmethod xsl-split-article ((article pathname))
  (let ((article-wsx (replace-extension article "miz" "wsx"))
	(split-stylesheet (mizar-items-config 'split-stylesheet)))
    (apply-stylesheet split-stylesheet article-wsx nil nil)))

(defun xsl-itemize-article (article)
  (let ((itemize-stylesheet (mizar-items-config 'itemize-stylesheet)))
    (apply-stylesheet itemize-stylesheet (xsl-split-article article) nil nil)))

(defun sch-file-p (path)
  (file-has-extension path "sch"))

(defun dco-file-p (path)
  (file-has-extension path "dco"))

(defun def-file-p (path)
  (file-has-extension path "def"))

(defun dno-file-p (path)
  (file-has-extension path "dno"))

(defun dcl-file-p (path)
  (file-has-extension path "dcl"))

(defun eid-file-p (path)
  (file-has-extension path "eid"))

(defun the-file-p (path)
  (file-has-extension path "the"))

(defun ckb-< (ckb-path-1 ckb-path-2)
  (let ((ckb-pattern "^ckb([0-9]+)$"))
    (register-groups-bind (ckb-num-1-as-str)
	(ckb-pattern (pathname-name ckb-path-1))
      (register-groups-bind (ckb-num-2-as-str)
	  (ckb-pattern (pathname-name ckb-path-2))
	(let ((ckb-num-1 (parse-integer ckb-num-1-as-str))
	      (ckb-num-2 (parse-integer ckb-num-2-as-str)))
	  (< ckb-num-1 ckb-num-2))))))

(defgeneric extend-evl (evl-file prel-dir)
  (:documentation "Extend the .evl file EVL-FILE with whatever the contents of PREL-DIR.  If, for example, there is a file 'foo.sch' in PREL-DIR, then EVL-FILE will be extended so that, in its Schemes directives, we find 'FOO' as an Ident."))

(defmethod extend-evl ((evl-file string) prel-dir)
  (extend-evl (pathname evl-file) prel-dir))

(defmethod extend-evl (evl-file (prel-dir string))
  (extend-evl evl-file (pathname prel-dir)))

(defmethod extend-evl :around ((evl-file pathname) (prel-dir pathname))
  (if (file-exists-p evl-file)
      (if (file-exists-p prel-dir)
	  (if (directory-p prel-dir)
	      (call-next-method)
	      (error "The specified prel DB, '~a', isn't actually a directory" (namestring prel-dir)))
	  (call-next-method))
      (error "The specified .evl file, '~a', doesn't exist" (namestring evl-file))))

(defmethod extend-evl ((evl-file pathname) (prel-dir pathname))
  (if (file-exists-p prel-dir)
      (let ((more-notations "")
	    (more-definitions "")
	    (more-theorems "")
	    (more-schemes "")
	    (more-registrations "")
	    (more-constructors "")
	    (more-requirements ""))
	(flet ((pad-string (string new-bit)
		 (format nil "~a~a," string (uppercase new-bit))))
	  (flet ((add-to-notations (article)
		   (setf more-notations (pad-string more-notations article)))
		 (add-to-definitions (article)
		   (setf more-definitions (pad-string more-definitions article)))
		 (add-to-theorems (article)
		   (setf more-theorems (pad-string more-theorems article)))
		 (add-to-schemes (article)
		   (setf more-schemes (pad-string more-schemes article)))
		 (add-to-registrations (article)
		   (setf more-registrations (pad-string more-registrations article)))
		 (add-to-constructors (article)
		   (setf more-constructors (pad-string more-constructors article)))
		 (add-to-requirements (article)
		   (setf more-requirements (pad-string more-requirements article))))
	    (flet ((dispatch-exported-file (path)
		     (cond ((dno-file-p path) (add-to-notations (pathname-name path)))
			   ((dcl-file-p path) (add-to-registrations (pathname-name path)))
			   ((eid-file-p path) (add-to-registrations (pathname-name path)))
			   ((sch-file-p path) (add-to-schemes (pathname-name path)))
			   ((dco-file-p path) (add-to-constructors (pathname-name path)))
			   ((def-file-p path) (add-to-definitions (pathname-name path)))
			   ((the-file-p path) (add-to-theorems (pathname-name path)))
			   (t
			    (error "Don't know how to deal with the prel file '~a'" (namestring path))))))
	      (loop
		 for extension in (list "dno" "dcl" "eid" "sch" "def" "dco" "the")
		 do
		   (loop
		      with files = (files-in-directory-with-extension prel-dir extension)
		      with sorted-files = (sort files #'ckb-<)
		      for path in sorted-files 
		      do (dispatch-exported-file path))))))
	(apply-stylesheet (mizar-items-config 'extend-evl-stylesheet)
			  evl-file
			  (list (cons "notations" more-notations)
				(cons "definitions" more-definitions)
				(cons "theorems" more-theorems)
				(cons "schemes" more-schemes)
				(cons "registrations" more-registrations)
				(cons "constructors" more-constructors)
				(cons "requirements" more-requirements))
			  nil))
      (apply-stylesheet (mizar-items-config 'extend-evl-stylesheet)
		      evl-file
		      nil
		      nil)))

(defgeneric itemize (article))

(defmethod itemize :around ((article-path pathname))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "There is no article at ~a" article-path)))

(defmethod itemize :before ((article-path pathname))
  (accom article-path :flags '("-q" "-l"))
  (newparser article-path :flags '("-q" "-l")))

(defmethod itemize ((article-path pathname))
  (let* ((xml-doc (cxml:parse (xsl-itemize-article article-path)
			      (cxml-dom:make-dom-builder)))
	 (evl-file (replace-extension article-path "miz" "evl"))
	 (bundle-xpath "Items/Item-Bundle")
	 (article-name (pathname-name article-path))
	 (wsm-stylesheet (mizar-items-config 'wsm-stylesheet))
	 (items-dir (format nil "/~{~a/~}~a/" (cdr (pathname-directory article-path)) article-name))
;                                              ^^^ PATHNAME-DIRECTORY gives a list with a useless first component
;                                     ^ ensures that the path ends with '/'
;                                ^ ensures that the path starts with '/'
	 (prel-dir (format nil "~aprel/" items-dir)))
;                               ^^^ squishing these together is OK because ITEMS-DIR ends with a '/'
    (handler-case
	(ensure-directories-exist items-dir)
      (file-error () (error "We cannot ensure that the directory '~a' exists, so we cannot save the items of ~a into directory." items-dir article-name)))
    (xpath:do-node-set (bundle (xpath:evaluate bundle-xpath xml-doc))
      (let* ((bundlenr (dom:get-attribute bundle "bundlenr")))
	(let ((text-proper-set (xpath:evaluate "Text-Proper[1]" bundle)))
	  (if (xpath:node-set-empty-p text-proper-set)
	      (error "Empty node set for Text-Proper!")
	      (let* ((text-proper (first (xpath:all-nodes text-proper-set)))
		     (doc (rune-dom:create-document text-proper))
		     (bundle-path (format nil "~a~a.wsi" items-dir bundlenr)))
         ;                                     ^^^^ we can squash these together like this because ITEMs-DIR starts and ends with a '/'
		(with-open-file (bundle-xml bundle-path
					    :direction :output
					    :if-does-not-exist :create
					    :if-exists :supersede
					    :element-type '(unsigned-byte 8))
;                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;                                           Watch out: omitting this key can lead to trouble
		  (dom:map-document (cxml:make-octet-stream-sink bundle-xml) doc))
		;; create the bundle's new evl
		(let ((bundle-temp-evl-path (format nil "~ackb~a.evl1" items-dir bundlenr))
		      (extended-evl (extend-evl evl-file prel-dir)))
		  (write-string-into-file extended-evl bundle-temp-evl-path
					  :if-exists :supersede
					  :if-does-not-exist :create)
		  (let ((bundle-miz-path (format nil "~ackb~a.miz" items-dir bundlenr))
			(bundle-miz-text (apply-stylesheet wsm-stylesheet
							   bundle-path
							   (list (cons "evl" (namestring bundle-temp-evl-path)))
							   nil)))
		    (write-string-into-file bundle-miz-text bundle-miz-path
					    :if-exists :supersede
					    :if-does-not-exist :create)
		    (accom bundle-miz-path :working-directory items-dir :flags '("-q" "-l"))
		    (verifier bundle-miz-path :working-directory items-dir :flags '("-q" "-l"))
		    (exporter bundle-miz-path :working-directory items-dir :flags '("-q" "-l"))
		  (transfer bundle-miz-path :working-directory items-dir :flags '("-q" "-l")))))))))
    (xpath:evaluate (format nil "count(~a)" bundle-xpath) xml-doc)))

;;; itemize.lisp ends here