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
  (let ((bol-theorem-scanner (create-scanner (format nil "^( *)~A$|^( *)~A " keyword keyword))))
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
		   (label (label-for-vid article vid)))
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
      (let (deftheorem-items)
	(let ((definitionblock-node (xml-node definitionblock-item)))
	  (let ((deftheorem-nodes (deftheorems-after-definitionblock definitionblock-node)))
	    (dolist (deftheorem-node deftheorem-nodes deftheorem-items)
	      ;; we want the vid and nr of the DefTheorem's Proposition
	      ;; child element; the nr and vid of the DefTheorem itself are
	      ;; useless.
	      (let* ((proposition-child (first-child-with-name deftheorem-node "Proposition"))
		     (nr (value-of-nr-attribute proposition-child))
		     (vid (value-of-vid-attribute proposition-child))
		     (label (label-for-vid article vid)))
		(push (make-instance 'deftheorem-item
				     :source definitionblock-item
				     :node deftheorem-node
				     :nr nr
				     :vid vid
				     :label label)
		      deftheorem-items)))))
	(setf (deftheorems definitionblock-item) (reverse deftheorem-items))))))

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
  (block-items "RegistrationBlock" "registration" 'registration-item article))

(defun notationblock-items (article)
  (block-items "NotationBlock" "notation" 'notation-item article))

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

(defun definition-editing-instructions (item definition-table items->articles)
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
				      (let* ((name (name article-for-definition-item))
					     (instruction (make-instance 'editing-instruction
									 :old-label deftheorem-label
									 :new-label (format nil "~:@(~A~):def ~d" name (1+ index))
									 :target-line-number ref-line-num
									 :target-column-number (- ref-col-num (length deftheorem-label)))))
					(push instruction instructions))
				      (error "No article is associated with ~S in the item-to-article table"
					     definition-item)))
				(error "The deftheorem item ~S lacks a label!" deftheorem-item))
			    (error "The deftheorem item ~S cannot be found among thte deftheorem items for the definition item ~S" deftheorem-item definition-item)))
		      (error "The deftheorem item ~S does not have a source definition item!" deftheorem-item)))))))))))

(defun theorem-editing-instructions (item theorem-table items->articles)
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

(defun scheme-editing-instructions (item scheme-table items->articles)
  (let* ((item-xml-node (xml-node item))
	 (from-nodes (article-local-froms item-xml-node))
	 (instructions nil))
    (dolist (from-node from-nodes instructions)
      (multiple-value-bind (from-line-num from-col-num)
	  (line-and-column from-node)
	(let ((schemenr (value-of-absnr-attribute from-node)))
	  (if schemenr
	      (let ((scheme-item (gethash schemenr scheme-table)))
		(if scheme-item
		    (let ((scheme-label (label scheme-item)))
		      (if scheme-label
			  (let ((article-for-scheme-item (gethash scheme-item items->articles)))
			    (if article-for-scheme-item
				(let ((name (name article-for-scheme-item)))
				  (let ((instruction (make-instance 'editing-instruction
								    :old-label scheme-label
								    :new-label (format nil "~:@(~A~):sch 1" name)
								    :target-line-number from-line-num
								    :target-column-number from-col-num)))
				    (push instruction instructions)))
				(error "No article is associated with ~S in the item-to-article table"
				       scheme-item)))
			  (error "The scheme item ~S lacks a label!" scheme-item)))
		    (error "No scheme item is associated with the scheme number ~S in the scheme table"
			   schemenr)))
	      (error "The From node ~S lacks a value for the schemenr attribute"
		     from-node)))))))

(defun editing-instructions (item definition-table theorem-table scheme-table items->articles)
  (append (definition-editing-instructions item definition-table items->articles)
	  (theorem-editing-instructions item theorem-table items->articles)
	  (scheme-editing-instructions item scheme-table items->articles)))

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
     for new-line = (replace-label old-label line new-label target-column-number)
     do
       (when (string= line new-line)
	 (error "Applying the instruction ~S to the line~%~%~A~%~%had no effect!" instruction line))
       (setf (aref lines index) new-line)
     finally (return lines)))

(defun revised-item-text (item theorem-table definition-table scheme-table items->articles)
  (array->newline-delimited-string
   (apply-editing-instructions item
			       (editing-instructions item theorem-table definition-table scheme-table items->articles)
			       (lines-as-array (text item)))))

(defun rewrite-item-text (item theorem-table definition-table scheme-table items->articles)
  (setf (text item)
	(revised-item-text item theorem-table definition-table scheme-table items->articles)))

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

(defgeneric itemize (thing))

(defmethod itemize :around ((article article))
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

(defmethod itemize :around ((article article))
  (if (slot-boundp article 'name)
      (call-next-method)
      (error "Article ~S lacks a name" article)))

(defmethod itemize :around ((article-path pathname))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "No mizar article at ~A" (namestring article-path))))

(defmethod itemize ((article-path pathname))
  (itemize (make-instance 'article :path article-path)))

(defmethod itemize ((article-path string))
  (itemize (pathname article-path)))

(defmethod itemize ((article article))
  (let* ((name (name article))
	 (sandbox (fresh-sandbox name))
	 (directory (location sandbox))
	 (article-in-sandbox (concat (namestring directory) (format nil "~A.miz" name))))
    (warn "Itemizing in the directory ~A" (namestring (location sandbox)))
    (copy-file (path article) article-in-sandbox)
    (preprocess-text article directory)
    ;; ensure the article XML is now synchonized with the changed text
    (warn "Verifying...")
    (verifier article directory "-q" "-l" "-s")
    (warn "Generating absolute references...")
    (absrefs article)
    (refresh-text article)
    (refresh-idx article)
    (initialize-context-for-items article)
    (loop
       with definition-table = (make-hash-table :test #'equal) ; keys are pairs of integers
       with theorem-table = (make-hash-table :test #'equal) ; keys are pairs of integers
       with scheme-table = (make-hash-table :test #'eq) ; keys are integers; for schemes only
       with items->articles = (make-hash-table :test #'eq) ; keys are item objects
       with all-candidates = (item-candidates article)
       with pseudo-candidates = nil
       with earlier-item-names = nil
       with real-items = nil
       with candidate-num = 1
       with local-db = (pathname-as-directory (concat (namestring (pathname-as-directory directory)) name))
       with dict-subdir = (pathname-as-directory (concat (namestring local-db) "dict"))
       with article-vocab = (remove "TARSKI" (vocabularies article) :test #'string=)
       with symbols = (reduce #'append (mapcar #'listvoc article-vocab))
       with num-symbols = (length symbols)
       with text-subdir = (pathname-as-directory (concat (namestring local-db) "text"))
       for candidate in all-candidates
       initially 
	 (loop
	    with len = (length symbols)
	    for sym in symbols
	    for i from 1 upto len
	    for voc-filename = (format nil "sym~d.voc" i)
	    for voc-path = (concat (namestring dict-subdir) voc-filename)
	    initially 
	      (ensure-directories-exist dict-subdir)
	    do
	      (with-open-file (sym-file voc-path :direction :output)
		(format sym-file "~A~%" sym)))
	 (warn "About to consider ~d candidate items" (length all-candidates))
       do
	 (warn "Dealing with item ~S" candidate)
	 (rewrite-item-text candidate definition-table theorem-table scheme-table items->articles)
	 (when (typep candidate 'pseudo-item)
	   (push candidate pseudo-candidates))
	 (case (type-of candidate)
	   (scheme-item
	    (with-slots (schemenr)
		candidate
	      (setf (gethash schemenr scheme-table) candidate)))
	   (definition-item
	    (dolist (deftheorem (deftheorems candidate))
	      (with-slots (nr vid)
		  deftheorem
		(setf (gethash (cons nr vid) definition-table) deftheorem))))
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
	   (let* ((item-name (format nil "item~d" candidate-num))
		  (miz-filename (format nil "~A.miz" item-name))
		  (item-path (concat (namestring (pathname-as-directory text-subdir)) miz-filename))
		  (earlier (reverse earlier-item-names))
		  (new-vocabularies (mapcar #'(lambda (num) (format nil "SYM~d" num)) (numbers-from-to 1 num-symbols)))
		  (new-notations (append (notations article) earlier))
		  (new-contructors (append (constructors article) earlier))
		  (new-requirements (requirements article))
		  (new-registrations (append (registrations article) earlier))
		  (new-definitions (append (definitions article) earlier))
		  (new-theorems (append (theorems article) earlier))
		  (new-schemes (append (schemes article) earlier))
		  (original-text (text candidate))
		  (context (context-items candidate))
		  (context-lines (mapcar #'(lambda (item) (pad-with-newline (text item))) context))
		  (context-lines-as-str (apply #'concat context-lines))
		  (text (concat context-lines-as-str
				(if (typep candidate 'proposition-item)
				    (format nil "theorem~%~A" original-text) ; promote to theorem
				    original-text)))
		  (article-for-item (make-instance 'article
						   :vocabularies (if (member "TARSKI" (vocabularies article) :test #'string=)
								     (cons "TARSKI" new-vocabularies)
								     new-vocabularies)
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
	     (handler-case (progn
			     (write-article article-for-item)
			     (verify-and-export article-for-item local-db)
			     (minimize-context candidate (namestring local-db))
			     (minimize-environment article-for-item (namestring local-db))
					; synchronize with CANDIDATE
			     (setf (vocabularies candidate) (vocabularies article-for-item))
			     (setf (notations candidate) (notations article-for-item))
			     (setf (constructors candidate) (constructors article-for-item))
			     (setf (requirements candidate) (requirements article-for-item))
			     (setf (registrations candidate) (registrations article-for-item))
			     (setf (definitions candidate) (definitions article-for-item))
			     (setf (theorems candidate) (theorems article-for-item))				 
			     (setf (schemes candidate) (schemes article-for-item))				 
			     (push (uppercase item-name) earlier-item-names)
			     (setf (gethash candidate items->articles) article-for-item)
			     (push candidate real-items)
			     (incf candidate-num))
	       (mizar-error () (progn
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
       finally (return (reverse real-items)))))

;;; itemize.lisp ends here