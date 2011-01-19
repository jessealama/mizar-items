;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Computing candidate items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun single-line-items-by-keyword (article keyword class)
  (loop
     with regex = (format nil "^( *)~A .*$" keyword)
     for (begin end text) in (article-lines-matching regex article)
     for end-col = (length text)
     collecting (make-instance class
			       :source-article article
			       :begin-line-number begin
			       :begin-column-number end
			       :end-line-number begin ; text is one line
			       :end-column-number end-col)
       into items
     finally (return items)))

(defun reservation-items (article)
  (single-line-items-by-keyword article "reserve" 'reservation-item))

(defun set-items (article)
  (single-line-items-by-keyword article "set" 'set-item))

(defun consider-items (article)
  (single-line-items-by-keyword article "consider" 'consider-item))

(defun reconsider-items (article)
  (single-line-items-by-keyword article "reconsider" 'reconsider-item))

(defun defpred-items (article)
  (single-line-items-by-keyword article "defpred" 'defpred-item))

(defun deffunc-items (article)
  (single-line-items-by-keyword article "deffunc" 'deffunc-item))

(defun now-items (article)
  (with-slots (xml-doc)
      article
    (let (items)
      (xpath:do-node-set (now-node (xpath:evaluate "Article/Now" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (let* ((vid (value-of-vid-attribute now-node))
		 (nr (value-of-nr-attribute now-node))
		 (label (label-for-vid article vid)))
	    (multiple-value-bind (almost-begin-line-num almost-begin-col-num)
		(line-and-column now-node)
	      (multiple-value-setq (begin-line-num begin-column-num)
		(first-keyword-before article (format nil "~A:" label) almost-begin-line-num almost-begin-col-num))
	      (let ((last-endposition-child (last-child-with-name now-node "EndPosition")))
		(multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child)))
	      (push (make-instance 'now-item
				   :source-article article
				   :begin-line-number begin-line-num
				   :begin-column-number begin-column-num
				   :end-line-number end-line-num
				   :end-column-number end-column-num
				   :vid vid
				   :nr nr
				   :label label
				   :node now-node)
		    items)))))
	(reverse items))))

(defun iterequality-items (article)
  (with-slots (xml-doc)
      article
    (let (items)
      (xpath:do-node-set (iterequality-node (xpath:evaluate "Article/IterEquality" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  ;; iterequalities are weird: in the xml, we find out now
	  ;; where they begin, but where they end.  To find out where
	  ;; they begin, we have to look backwards from the end for
	  ;; the label.
	  (let* ((vid (value-of-vid-attribute iterequality-node))
		 (nr (value-of-nr-attribute iterequality-node))
		 (label (label-for-vid article vid)))
	    (let ((last-iterstep (last-child-with-name iterequality-node
						       "IterStep")))
	      (let* ((last-by (last-child-with-name last-iterstep "By"))
		     (last-from (last-child-with-name last-iterstep "From"))
		     (real-last (or last-by last-from)))
		(multiple-value-setq (end-line-num end-column-num)
		  (if real-last
		      (line-and-column real-last)
		      (line-and-column iterequality-node)))))
	    (multiple-value-setq (begin-line-num begin-column-num)
	      (first-keyword-before article (format nil "~A:" label) 
				    end-line-num end-column-num))
	    (push (make-instance 'iterequality-item
				 :source-article article
				 :begin-line-number begin-line-num
				 :begin-column-number begin-column-num
				 :end-line-number end-line-num
				 :end-column-number end-column-num
				 :nr nr
				 :vid vid
				 :node iterequality-node)
		  items))))
	(reverse items))))

(defun first-keyword-before (article keyword line-num col-num)
  "Look for the first occurence of the keyword KEYWORD (e.g.,
'theorem', 'definition', or possibly a label such as 'Lm3') before
LINE-NUM and COL-NUM in the text of ARTICLE."
  ; assume the keyword always begins a line, possibly with whitespace
  ; -- a terrible assumption
  (let ((bol-theorem-scanner (create-scanner (format nil "^( *)~A$|^( *)~A" keyword keyword))))
    (loop for l from line-num downto 0
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

(defun first-theorem-keword-before (article line-num col-num)
  (first-keyword-before article "theorem" line-num col-num))

(defun first-definition-keword-before (article line-num col-num)
  (first-keyword-before article "definition" line-num col-num))

(defun first-scheme-keword-before (article line-num col-num)
  (first-keyword-before article "scheme" line-num col-num))

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
		  (first-theorem-keword-before article almost-begin-line-num almost-begin-col-num))
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

(defun definitionblock-items (article)
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

(defgeneric item-candidates (article))

(defmethod item-candidates ((article-path string))
  (let ((article (make-instance 'article :path (pathname article-path))))
    (refresh-xml article)
    (refresh-idx article)
    (item-candidates article)))

(defmethod item-candidates ((article-path pathname))
  (let ((article (make-instance 'article :path article-path)))
    (refresh-xml article)
    (refresh-idx article)
    (item-candidates article)))

(defmethod item-candidates ((article article))
  (let ((reservation-items (reservation-items article))
	(set-items (set-items article))
	(consider-items (consider-items article))
	(reconsider-items (reconsider-items article))
	(defpred-items (defpred-items article))
	(deffunc-items (deffunc-items article))
	(now-items (now-items article))
	(iterequality-items (iterequality-items article))
	(justifiedtheorem-items (justifiedtheorem-items article))
	(proposition-items (proposition-items article))
	(definitionblock-items (definitionblock-items article))
	(schemeblock-items (schemeblock-items article))
	(registrationblock-items (registrationblock-items article))
	(notationblock-items (notationblock-items article)))
    (let ((everything (append reservation-items
			      set-items
			      consider-items
			      reconsider-items
			      defpred-items
			      deffunc-items
			      now-items
			      iterequality-items
			      justifiedtheorem-items
			      proposition-items
			      definitionblock-items
			      schemeblock-items
			      registrationblock-items
			      notationblock-items)))
      (let ((sorted-everything (stable-sort everything #'item-lex-less)))
	(let ((disjoined (reverse (disjoin-items sorted-everything))))

	  ;; Kludge: now that we've got the whittled-down list of
	  ;; items, before returning, assign xml nodes to all pseudo
	  ;; items.  Any pseduo-items remaining after DISJOIN must be
	  ;; toplevel items and need to have XML nodes assigned to them.
	  ;;
	  ;; If the XML line and column information for these items
	  ;; were accurate, there would be no need to do this here.
	  ;; Alas.
	  (let ((class-element-pairs (list '(reservation-item . "Reservation")
					   '(set-item         . "Set")
					   '(consider-item    . "Consider")
					   '(reconsider-item  . "Reconsider")
					   '(defpred-item     . "DefPred")
					   '(deffunc-item     . "DefFunc"))))
	    (dolist (class-element-pair class-element-pairs)
	      (destructuring-bind (class . element-name)
		  class-element-pair
		(loop
		   with xml-doc = (xml-doc article)
		   with items = (remove-if-not #'(lambda (item) (typep item class)) disjoined)
		   with nodes = (xpath:all-nodes (xpath:evaluate (format nil "Article/~A" element-name) xml-doc))
		   for node in nodes
		   for item in items
		   do
		     (setf (xml-node item) node)))))
	  
	  disjoined)))))

(defun non-pseudo-item-candidates (article)
  (remove-if #'(lambda (object)
		 (typep object 'pseudo-item))
	     (item-candidates article)))

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
				      new-label (format nil "I~d:def ~d" item-number def-number)))))))))
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
	 (from-nodes (all-from-descendents item-xml-node))
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
			      (setf new-label (format nil "I~d:sch 1" earlier-item-number))
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
     for new-line = (replace-label old-label line new-label (1- target-column-number))
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

(defun preprocess-text (article &optional (directory (sb-posix:getcwd)))
  (warn "Preprocessing ~S..." article)
  (warn "Stripping comments...")
  (strip-comments article directory)
  (warn "Fixing by and from statements...")
  (fix-by-and-from article directory)
  (warn "Accommodating...")
  (accom article directory "-q" "-l" "-s")
  (warn "JA1...")
  (JA1 article directory "-q" "-l" "-s")
  (warn "dellink...")
  (dellink article directory "-q" "-l" "-s")
  (warn "CutSet...")
  (CutSet article directory "-q" "-l" "-s")
  (warn "CutReconsider...")
  (CutReconsider article directory "-q" "-l" "-s")
  (warn "change...")
  (change article directory "-q" "-l" "-s")
  (warn "Fixing by and from statements...")
  (fix-by-and-from article directory)
  (warn "Squeezing repeated newlines...")
  (squeeze-repeated-newlines article directory)
  (warn "Squeezing repeated spaces...")
  (squeeze-repeated-spaces article directory))

(defun initialize-context-for-items (article)
  ;; compute a conservative estimate of what pseudo-items each item
  ;; will depend on, namely: all previous ones
  (loop
     with candidates = (item-candidates article)
     with num-candidates = (length candidates)
     with earlier-pseudo-items = (make-hash-table :test #'eq :size num-candidates)
     for candidate in candidates
     for candidate-num from 1 upto num-candidates
     do
       (if (typep candidate 'pseudo-item)
	   (setf (gethash candidate-num earlier-pseudo-items)
		 candidate)
	   (setf (context-items candidate)
		 (values-for-keys-less-than earlier-pseudo-items candidate-num)))
     finally (return article)))

(defun verify-and-export (article directory)
  (accom article directory "-q" "-l" "-s")
  (verifier article directory "-q" "-l" "-s")
  (exporter article directory "-q" "-l" "-s")
  (transfer article directory "-q" "-l" "-s"))

(defun minimal-context (item &optional (directory (sb-posix:getcwd)))
  (remove-unneeded
   (context-items item)
   #'(lambda (lst)
       (verifiable? (let ((article (item->article
				    (let ((new-item (copy-item item)))
				      (setf (context-items new-item) lst)
				      new-item))))
		      (setf (path article)
			    (concat (namestring (pathname-as-directory (pathname directory)))
				    "splork.miz"))
		      article)
		    directory))))

(defun minimize-context (item &optional (directory (sb-posix:getcwd)))
  (warn "Minimizing context for item ~S..." item)
  (let* ((context (context-items item))
	 (minimal-context (minimal-context item directory)))
    (setf (context-items item) minimal-context)
    (warn "...done minimizing context.  We eliminated ~d items" (- (length context)
								   (length minimal-context)))))

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

(defgeneric itemize (things &optional itemization-record))

(defmethod itemize :around ((article article) &optional itemization-record)
  (declare (ignore itemization-record))
  (if (slot-boundp article 'path)
      (with-slots (path)
	  article
	(let ((real-name (file-exists-p article)))
	  (if real-name
	      (if (directory-p real-name)
		  (error "The path ~A for the article to itemize isn't a file, but a directory!" path)
		  (call-next-method))
	      (error "The path ~A for the article to itemize doesn't exist" path))))
      (error "The article ~S lacks a path" article)))

(defmethod itemize :around ((article article) &optional itemization-record)
  (declare (ignore itemization-record))
  (if (slot-boundp article 'name)
      (call-next-method)
      (error "Article ~S lacks a name" article)))

(defmethod itemize :around ((article-path pathname) &optional itemization-record)
  (declare (ignore itemization-record))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "No mizar article at ~A" (namestring article-path))))

(defmethod itemize ((article-path pathname) &optional itemization-record)
  (itemize (make-instance 'article :path article-path) itemization-record))

(defmethod itemize ((article-path string) &optional itemization-record)
  (itemize (pathname article-path) itemization-record))

(defmethod itemize ((article-name string) &optional itemization-record)
  (if (article-exists-p article-name)
      (itemize (make-instance 'article
			      :name article-name
			      :path (file-in-directory (concat (location *default-mizar-library*) "mml") article-name "miz"))
	       itemization-record)
      (error "No such article '~A' in the default mizar library at ~A" article-name (location *default-mizar-library*))))

(defmethod itemize ((article article) &optional itemization-record)
  (let ((itemization (or itemization-record
			 (make-instance 'itemization
					:sandbox (fresh-sandbox (name article))))))
    (with-slots (sandbox definition-labels-to-items theorem-labels-to-items scheme-labels-to-items symbol-table)
	itemization
      (copy-file-to-sandbox (path article) sandbox)
      (let* ((name (name article))
	     (name-uc (format nil "~:@(~A~)" name))
	     (directory (location sandbox))
	     (article-in-sandbox (make-instance 'article 
						:name (name article)
						:path (file-in-directory (location sandbox) (name article) "miz"))))
	(warn "Itemizing in the directory ~A" (namestring (location sandbox)))
	(preprocess-text article-in-sandbox directory)
	;; ensure the article XML is now synchonized with the changed text
	(warn "Verifying...")
	(verifier article-in-sandbox directory "-q" "-l" "-s")
	(warn "Generating absolute references...")
	(absrefs article-in-sandbox)
	(refresh-text article-in-sandbox)
	(refresh-idx article-in-sandbox)
	(initialize-context-for-items article-in-sandbox)
	(loop
	   with definition-table = (make-hash-table :test #'equal) ; keys are pairs of integers
	   with theorem-table = (make-hash-table :test #'equal) ; keys are pairs of integers
	   with scheme-table = (make-hash-table :test #'eq) ; keys are integers; for schemes only
	   with items->articles = (make-hash-table :test #'eq) ; keys are item objects
	   with all-candidates = (item-candidates article-in-sandbox)
	   with pseudo-candidates = nil
	   with earlier-item-names = nil
	   with real-items = nil
	   with starting-item-number = (1+ (num-items itemization))
	   with candidate-num = starting-item-number
	   with local-db = (location sandbox)
	   with dict-subdir = (ensure-directory (concat local-db "dict"))
	   with prel-subdir = (ensure-directory (concat local-db "prel"))
	   with text-subdir = (ensure-directory (concat local-db "text"))
	   with article-vocab = (remove "TARSKI" (vocabularies article-in-sandbox) :test #'string=)
	   with symbols = (reduce #'append (mapcar #'listvoc article-vocab))
	   ;with num-symbols = (length symbols)
	   with scheme-nr = 0
	   with deftheorem-nr = 0
	   for candidate in all-candidates
	   initially 
	     (ensure-directories-exist dict-subdir)
	     (ensure-directories-exist prel-subdir)
	     (ensure-directories-exist text-subdir)
	     (write-new-symbols symbols symbol-table dict-subdir)
	     (warn "About to consider ~d candidate items" (length all-candidates))
	   do
	     (warn "Dealing with item ~S" candidate)
	     (rewrite-item-text candidate definition-table theorem-table scheme-table items->articles itemization)
	     (when (typep candidate 'pseudo-item)
	       (push candidate pseudo-candidates))
	     (case (type-of candidate)
	       (scheme-item
		(incf scheme-nr)
		(with-slots (schemenr)
		    candidate
		  (setf (gethash schemenr scheme-table) candidate)))
	       (definition-item
		(let ((deftheorems (deftheorems candidate)))
		  (incf deftheorem-nr (length deftheorems))
		  (dolist (deftheorem deftheorems)
		    (with-slots (nr vid)
			deftheorem
		      (setf (gethash (cons nr vid) definition-table) deftheorem)))))
	       (theorem-item
		(with-slots (nr vid)
		    candidate
		  (setf (gethash (cons nr vid) theorem-table) candidate)))
	       (proposition-item
		(with-slots (nr vid)
		    candidate
		  (setf (gethash (cons nr vid) theorem-table) candidate))))
	     (unless (typep candidate 'pseudo-item)
	       (setf (context-items candidate) (reverse pseudo-candidates))
	       (let* ((item-name (format nil "i~d" candidate-num))
		      (miz-filename (format nil "~A.miz" item-name))
		      (item-path (concat (namestring (pathname-as-directory text-subdir)) miz-filename))
		      (earlier (reverse earlier-item-names))
		      ;; (new-vocabularies (mapcar #'(lambda (num) (format nil "SYM~d" num)) (numbers-from-to 1 num-symbols)))
		      (new-vocabularies (vocabularies article-in-sandbox))
		      (new-notations (append (notations article-in-sandbox) earlier))
		      (new-contructors (append (constructors article-in-sandbox) earlier))
		      (new-requirements (requirements article-in-sandbox))
		      (new-registrations (append (registrations article-in-sandbox) earlier))
		      (new-definitions (append (definitions article-in-sandbox) earlier))
		      (new-theorems (append (theorems article-in-sandbox) earlier))
		      (new-schemes (append (schemes article-in-sandbox) earlier))
		      ;; (new-notations (append (expand-previous-notations article-in-sandbox
		      ;; 							(names->notations itemization))
		      ;; 			     earlier))
		      ;; (new-contructors (append (expand-previous-constructors article-in-sandbox
		      ;; 							     (names->constructors itemization))
		      ;; 			       earlier))
		      ;; (new-registrations (append (expand-previous-registrations article-in-sandbox
		      ;; 								(names->registrations itemization))
		      ;; 				 earlier))
		      ;; (new-definitions (append (expand-previous-definitions article-in-sandbox
		      ;; 							    (names->definitions itemization))
		      ;; 			       earlier))
		      ;; (new-theorems (append (expand-previous-theorems article-in-sandbox
		      ;; 						      (names->theorems itemization))
		      ;; 			    earlier))
		      ;; (new-schemes (append (expand-previous-schemes article-in-sandbox
		      ;; 						    (names->schemes itemization))
		      ;; 			   earlier))
		      (original-text (text candidate))
		      (context (context-items candidate))
		      (context-lines (mapcar #'(lambda (item) (pad-with-newline (text item))) context))
		      (context-lines-as-str (apply #'concat context-lines))
		      (text (concat context-lines-as-str
				    (if (typep candidate 'proposition-item)
					(format nil "theorem~%~A" original-text) ; promote to theorem
					original-text)))
		      (article-for-item (make-instance 'article
						       ;; :vocabularies (if (member "TARSKI" (vocabularies article-in-sandbox) :test #'string=)
						       ;; 			 (cons "TARSKI" new-vocabularies)
						       ;; 			 new-vocabularies)
						       :pretext (dom:map-document (cxml:make-string-sink) (xml-node candidate))
						       :vocabularies new-vocabularies
						       :notations new-notations
						       :constructors new-contructors
						       :requirements new-requirements
						       :registrations new-registrations
						       :definitions new-definitions
						       :theorems new-theorems
						       :schemes new-schemes
						       :path item-path
						       :name item-name
						       :text text)))
		 (multiple-value-bind (notations constructors registrations definitions theorems schemes)
		     (trim-environment article-for-item local-db)
		   (setf (notations candidate) notations
			 (constructors candidate) constructors
			 (registrations candidate) registrations
			 (definitions candidate) definitions
			 (theorems candidate) theorems
			 (schemes candidate) schemes))
		 (restart-case
		     (progn
		       (write-article article-for-item)
		       (verify-and-export article-for-item local-db)
		       (minimize-context candidate (namestring local-db))
		       ;;(minimize-environment article-for-item (namestring local-db))
		       ;; synchronize with CANDIDATE
		       (setf (vocabularies candidate) (vocabularies article-for-item)
			     (notations candidate) (notations article-for-item)
			     (constructors candidate) (constructors article-for-item)
			     (requirements candidate) (requirements article-for-item)
			     (registrations candidate) (registrations article-for-item)
			     (definitions candidate) (definitions article-for-item)
			     (theorems candidate) (theorems article-for-item)
			     (schemes candidate) (schemes article-for-item))
		       (let* ((context (context-items candidate))
			      (context-lines (mapcar #'(lambda (item) (pad-with-newline (text item))) context))
			      (context-lines-as-str (apply #'concat context-lines))
			      (text (concat context-lines-as-str
					    (if (typep candidate 'proposition-item)
						(format nil "theorem~%~A" original-text) ; promote to theorem
						original-text))))
			 (setf (text article-for-item) text))
		       (write-article article-for-item)
		       (push (uppercase item-name) earlier-item-names)
		       (setf (gethash candidate items->articles) article-for-item)
		       (push candidate real-items)
		       (setf (gethash candidate-num (items itemization)) candidate)
		       (incf (num-items itemization))
		       (incf candidate-num))
  (continue-itemizing () 
    (progn
      (warn "We got a mizar error for the item ~S, with text~%~%~A" candidate (text candidate))
      (cond ((typep candidate 'scheme-item)
	     (with-slots (schemenr)
		 candidate
	       (remhash schemenr scheme-table)))
	    ((typep candidate 'definition-item)
	     (with-slots (nr vid)
		 candidate
	       (remhash (cons nr vid) definition-table)))
	    ((or (typep candidate 'theorem-item)
		 (typep candidate 'proposition-item))
	     (with-slots (nr vid)
		 candidate
	       (remhash (cons nr vid) theorem-table))))
      (delete-file (path article-for-item))
      (push candidate pseudo-candidates))))))
  finally 
	     ; record what exported items were just generated by itemization
	     (setf (gethash name-uc (names->notations itemization))
		   (mapcar #'(lambda (num)
			       (format nil "I~d" num))
			   (remove-if-not #'(lambda (num)
					      (file-exists-p (file-in-directory prel-subdir (format nil "i~d.dno" num))))
					  (numbers-from-to starting-item-number candidate-num)))
		   (gethash name-uc (names->constructors itemization))
		   (mapcar #'(lambda (num)
			       (format nil "I~d" num))
			   (remove-if-not #'(lambda (num)
					      (file-exists-p (file-in-directory prel-subdir (format nil "i~d.dco" num))))
					  (numbers-from-to starting-item-number candidate-num)))
		   (gethash name-uc (names->registrations itemization))
		   (mapcar #'(lambda (num)
			       (format nil "I~d" num))
			   (remove-if-not #'(lambda (num)
					      (file-exists-p (file-in-directory prel-subdir (format nil "i~d.dcl" num))))
					  (numbers-from-to starting-item-number candidate-num)))
		   (gethash name-uc (names->definitions itemization))
		   (mapcar #'(lambda (num)
			       (format nil "I~d" num))
			   (remove-if-not #'(lambda (num)
					      (file-exists-p (file-in-directory prel-subdir (format nil "i~d.def" num))))
					  (numbers-from-to starting-item-number candidate-num)))
		   (gethash name-uc (names->theorems itemization))
		   (mapcar #'(lambda (num)
			       (format nil "I~d" num))
			   (remove-if-not #'(lambda (num)
					      (file-exists-p (file-in-directory prel-subdir (format nil "i~d.the" num))))
					  (numbers-from-to starting-item-number candidate-num)))
		   (gethash name-uc (names->schemes itemization))
		   (mapcar #'(lambda (num)
			       (format nil "I~d" num))
			   (remove-if-not #'(lambda (num)
					      (file-exists-p (file-in-directory prel-subdir (format nil "i~d.sch" num))))
					  (numbers-from-to starting-item-number candidate-num))))
	     ; register constructors, notations/patterns, registrations, definiens, theorems, and schemes
	     (dolist (item (remove-if #'(lambda (item) (typep item 'pseudo-item)) all-candidates))
	       (when (typep item 'definition-item)
		 (dolist (constructor-kind-and-nr (definition-constructors item))
		   (destructuring-bind (kind . nr)
		       constructor-kind-and-nr
		     (setf (gethash (list name-uc kind nr) (constructors itemization)) item)))
		 (dolist (pattern-kind-and-nr (definition-patterns item))
		   (destructuring-bind (kind . nr)
		       pattern-kind-and-nr
		     (setf (gethash (list name-uc kind nr) (patterns itemization)) item)))
		 (dolist (constrkind-and-constrnr (definientia item))
		   (destructuring-bind (constrkind . constrnr)
		       constrkind-and-constrnr
		     (setf (gethash (list name-uc constrkind constrnr) (definientia itemization)) item))))
	       (when (typep item 'notation-item)
		 (dolist (pattern-kind-and-nr (patterns item))
		   (destructuring-bind (kind . nr)
		       pattern-kind-and-nr
		     (setf (gethash (list name-uc kind nr) (patterns itemization)) item))))
	       (when (typep item 'registration-item)
		 (dolist (rcluster-nr (rclusters item))
		   (setf (gethash (list name-uc rcluster-nr) (rclusters itemization)) item))
		 (dolist (fcluster-nr (fclusters item))
		   (setf (gethash (list name-uc fcluster-nr) (fclusters itemization)) item))
		 (dolist (ccluster-nr (cclusters item))
		   (setf (gethash (list name-uc ccluster-nr) (cclusters itemization)) item))
		 (dolist (constrkind-and-nr (identifications item))
		   (destructuring-bind (constrkind . nr)
		       constrkind-and-nr
		     (setf (gethash (list name-uc constrkind nr) (identifications itemization)) item))))
	       (when (typep item 'theorem-item)
		 (setf (gethash (list name-uc (absnr item)) (theorems itemization)) item))
	       (when (typep item 'scheme-item)
		 (setf (gethash (list name-uc (schemenr item)) (schemes itemization)) item)))
	     (return itemization))))))

(defmethod itemize ((articles list) &optional itemization-record)
  (if (null articles)
      itemization-record
      (itemize (cdr articles) (itemize (car articles)
				       (or itemization-record
					   (make-instance 'itemization
							  :sandbox (fresh-sandbox "itemization")))))))

(defun dependency-graph (itemization)
  (loop
     with graph = nil
     with text-subdir = (concat (location (sandbox itemization)) "text/")
     with cclusters-table = (cclusters itemization)
     with constructors-table = (constructors itemization)
     with definientia-table = (definientia itemization)
     with fclusters-table = (fclusters itemization)
     with identification-table = (identifications itemization)
     with patterns-table = (patterns itemization)
     with rclusters-table = (rclusters itemization)
     with schemes-table = (schemes itemization)
     with theorems-table = (theorems itemization)
     with item-table = (items itemization)
     for item-number being the hash-keys in item-table
     for item being the hash-values in item-table
     do
       (let ((needed-ccluster-path (file-in-directory text-subdir (format nil "~d-needed-CCluster" item-number)))
	     (needed-constructor-path (file-in-directory text-subdir (format nil "~d-needed-Constructor" item-number)))
	     (needed-definientia-path (file-in-directory text-subdir (format nil "~d-needed-Definiens" item-number)))
	     (needed-fcluster-path (file-in-directory text-subdir (format nil "~d-needed-FCluster" item-number)))
	     (needed-identifications-path (file-in-directory text-subdir (format nil "~d-needed-Identify" item-number)))
	     (needed-pattern-path (file-in-directory text-subdir (format nil "~d-needed-Pattern" item-number)))
	     (needed-rcluster-path (file-in-directory text-subdir (format nil "~d-needed-RCluster" item-number)))
	     (needed-scheme-path (file-in-directory text-subdir (format nil "~d-needed-Scheme" item-number)))
	     (needed-theorem-path (file-in-directory text-subdir (format nil "~d-needed-Theorem" item-number))))
	 (let* ((needed-ccluster-xml (cxml:parse-file needed-ccluster-path (cxml-dom:make-dom-builder)))
		(ccluster-nodes (xpath:all-nodes (xpath:evaluate "Registrations/CCluster" needed-ccluster-xml))))
	   (dolist (ccluster-node ccluster-nodes)
	     (let* ((aid (value-of-aid-attribute ccluster-node))
		    (nr (value-of-nr-attribute ccluster-node))
		    (dependent-item (gethash (list aid nr) cclusters-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph)))))
	 (let* ((needed-constructor-xml (cxml:parse-file needed-constructor-path (cxml-dom:make-dom-builder)))
		(constructor-nodes (xpath:all-nodes (xpath:evaluate "Constructors/Constructor" needed-constructor-xml))))
	   (dolist (constructor-node constructor-nodes)
	     (let* ((aid (value-of-aid-attribute constructor-node))
		    (kind (value-of-kind-attribute constructor-node))
		    (nr (value-of-nr-attribute constructor-node))
		    (dependent-item (gethash (list aid kind nr) constructors-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph)))))
	 (let* ((needed-definientia-xml (cxml:parse-file needed-definientia-path (cxml-dom:make-dom-builder)))
		(definiens-nodes (xpath:all-nodes (xpath:evaluate "Definientia/Definiens" needed-definientia-xml))))
	   (dolist (definiens-node definiens-nodes)
	     (let* ((aid (value-of-aid-attribute definiens-node))
		    (constrkind (value-of-constrkind-attribute definiens-node))
		    (constrnr (value-of-constrnr-attribute definiens-node))
		    (dependent-item (gethash (list aid constrkind constrnr) definientia-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph)))))
	 (let* ((needed-fcluster-xml (cxml:parse-file needed-fcluster-path (cxml-dom:make-dom-builder)))
		(fcluster-nodes (xpath:all-nodes (xpath:evaluate "Registrations/FCluster" needed-fcluster-xml))))
	   (dolist (fcluster-node fcluster-nodes)
	     (let* ((aid (value-of-aid-attribute fcluster-node))
		    (nr (value-of-nr-attribute fcluster-node))
		    (dependent-item (gethash (list aid nr) fclusters-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph)))))
	 (let* ((needed-identifications-xml (cxml:parse-file needed-identifications-path (cxml-dom:make-dom-builder)))
		(identify-nodes (xpath:all-nodes (xpath:evaluate "IdentifyRegistrations/Identify" needed-identifications-xml))))
	   (dolist (identify-node identify-nodes)
	     (let* ((aid (value-of-aid-attribute identify-node))
		    (constrkind (value-of-constrkind-attribute identify-node))
		    (nr (value-of-nr-attribute identify-node))
		    (dependent-item (gethash (list aid constrkind nr) identification-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph)))))
	 (let* ((needed-pattern-xml (cxml:parse-file needed-pattern-path (cxml-dom:make-dom-builder)))
		(pattern-nodes (xpath:all-nodes (xpath:evaluate "Patterns/Pattern" needed-pattern-xml))))
	   (dolist (pattern-node pattern-nodes)
	     (let* ((aid (value-of-aid-attribute pattern-node))
		    (kind (value-of-kind-attribute pattern-node))
		    (nr (value-of-nr-attribute pattern-node))
		    (dependent-item (gethash (list aid kind nr) patterns-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph)))))
	 (let* ((needed-rcluster-xml (cxml:parse-file needed-rcluster-path (cxml-dom:make-dom-builder)))
		(rcluster-nodes (xpath:all-nodes (xpath:evaluate "Registrations/RCluster" needed-rcluster-xml))))
	   (dolist (rcluster-node rcluster-nodes)
	     (let* ((aid (value-of-aid-attribute rcluster-node))
		    (nr (value-of-nr-attribute rcluster-node))
		    (dependent-item (gethash (list aid nr) rclusters-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph)))))
	 (let* ((needed-scheme-xml (cxml:parse-file needed-scheme-path (cxml-dom:make-dom-builder)))
		(scheme-nodes (xpath:all-nodes (xpath:evaluate "Schemes/Scheme" needed-scheme-xml))))
	   (dolist (scheme-node scheme-nodes)
	     (let* ((aid (value-of-aid-attribute scheme-node))
		    (schemenr (value-of-schemenr-attribute scheme-node))
		    (dependent-item (gethash (list aid schemenr) schemes-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph)))))
	 (let* ((needed-theorem-xml (cxml:parse-file needed-theorem-path (cxml-dom:make-dom-builder)))
		(theorem-nodes (xpath:all-nodes (xpath:evaluate "Theorems/Theorem" needed-theorem-xml))))
	   (dolist (theorem-node theorem-nodes)
	     (let* ((aid (value-of-aid-attribute theorem-node))
		    (absnr (value-of-absnr-attribute theorem-node))
		    (dependent-item (gethash (list aid absnr) theorems-table)))
	       (when dependent-item
		 (push (cons item dependent-item) graph))))))
     finally
       (return graph)))


;;; itemize.lisp ends here