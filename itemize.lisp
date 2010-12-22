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
			      (multiple-value-setq (end-line-num end-column-num) (line-and-column last-ref-node)))
			    (multiple-value-setq (end-line-num end-column-num) prop-node))))))
	      (push (make-instance 'theorem-item
				   :source-article article
				   :begin-line-number begin-line-num
				   :begin-column-number begin-column-num
				   :end-line-number end-line-num
				   :end-column-number end-column-num
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
	      (let ((proof-node (proof-after-proposition proposition-node)))
		(if proof-node
		    (let ((last-endposition-child (last-child-with-name proof-node "EndPosition")))
		      (multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child)))
		    (let ((by-or-from (by-or-from-after-proposition proposition-node)))
		      (if by-or-from
			  (let ((last-ref-node (last-child-with-name by-or-from "Ref")))
			    (multiple-value-setq (end-line-num end-column-num) (line-and-column last-ref-node)))
			  (multiple-value-setq (end-line-num end-column-num) proposition-node))))))
	    (push (make-instance 'proposition-item
			       :source-article article
			       :begin-line-number begin-line-num
			       :begin-column-number begin-column-num
			       :end-line-number end-line-num
			       :end-column-number end-column-num
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
	  (let* ((vid (value-of-vid-attribute block-node))
		 (nr (value-of-nr-attribute block-node))
		 (label (label-for-vid article vid)))
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
				     :end-column-number end-column-num
				     :node block-node
				     :vid vid
				     :nr nr
				     :label label)
		    items))))))
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
		     (vid (value-of-nr-attribute proposition-child))
		     (label (label-for-vid article vid)))
		(push (make-instance 'deftheorem-item
				     :source definitionblock-node
				     :node deftheorem-node
				     :nr nr
				     :vid vid
				     :label label)
		      deftheorem-items)))))
	(setf (deftheorems definitionblock-item) deftheorem-items)))))

(defun schemeblock-items (article)
  (let ((items (block-items "SchemeBlock" "scheme" 'scheme-item article)))
    (with-slots (scheme-table)
	article
      (dolist (item items items)
	(let* ((xml (xml-node item))
	       (schemenr (value-of-schemenr-attribute xml)))
	  (setf (gethash schemenr scheme-table) item))))))

(defun registrationblock-items (article)
  (block-items "RegistrationBlock" "registration" 'registration-item article))

(defun notationblock-items (article)
  (block-items "NotationBlock" "notation" 'notation-item article))

(defun tuple-lex-less (tuple-1 tuple-2)
  "Determine whether TUPLE-1 is lexicographically less than TUPLE-2,
  ignoring all but the first and second components of both tuples."
  (let ((first-1 (first tuple-1))
	(first-2 (first tuple-2))
	(second-1 (second tuple-1))
	(second-2 (second tuple-2)))
    (or (< first-1 first-2)
	(and (= first-1 first-2)
	     (< second-1 second-2)))))

(defun item-candidates (article)
  (let ((everything (append (reservation-items article)
			    (set-items article)
			    (consider-items article)
			    (reconsider-items article)
			    (defpred-items article)
			    (deffunc-items article)
			    (now-items article)
			    (iterequality-items article)
			    (justifiedtheorem-items article)
			    (proposition-items article)
			    (definitionblock-items article)
			    (schemeblock-items article)
			    (registrationblock-items article)
			    (notationblock-items article))))
    (let ((sorted-everything (stable-sort everything #'item-lex-less)))
      (reverse (disjoin-items sorted-everything)))))

(defun preprocess-text (article)
  (strip-comments article)
  (accom article "-q" "-l" "-s")
  (JA1 article "-q" "-l" "-s")
  (dellink article "-q" "-l" "-s")
  (CutSet article "-q" "-l" "-s")
  (CutReconsider article "-q" "-l" "-s")
  (change article "-q" "-l" "-s"))

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

(defun itemize-preprocess (article)
  (preprocess-text article)

  ;; ensure the article XML is now synchonized with the changed text
  (verifier article "-q" "-l" "-s")
  (absrefs article)
  (refresh-text article)
  (refresh-idx article)

  (initialize-context-for-items article))

(defun rewrite-item-text (item theorem-and-definition-table scheme-table items->articles)
  (declare (ignore theorem-and-definition-table))
  (let ((item-xml-node (xml-node item))
	(source-article (source-article item))
	(item-text-as-array (lines-as-array (text item)))
	(begin-line-number (begin-line-number item)))
    (let ((from-nodes (article-local-froms item-xml-node)))
      (dolist (from-node from-nodes)
	(let ((from-line-num (line-and-column from-node)))
	  (let ((line (line-at source-article from-line-num))
		(schemenr (value-of-absnr-attribute from-node)))
	    (if schemenr
		(let ((scheme-item (gethash schemenr scheme-table)))
		  (if scheme-item
		      (let ((scheme-label (label scheme-item)))
			(if scheme-label
			    (let ((article-for-scheme-item (gethash scheme-item items->articles)))
			      (if article-for-scheme-item
				  (let ((name (name article-for-scheme-item)))
				    (setf (aref item-text-as-array (- from-line-num begin-line-number))
					  (regex-replace scheme-label 
							 line
							 (format nil "~:@(~A~):sch 1" name))))
				  (error "No article is associated with ~S in the item-to-article table"
					 scheme-item)))
			    (error "The scheme item ~S lacks a label!" scheme-item)))
		      (error "No scheme item is associated with the scheme number ~S in the scheme table"
			     schemenr)))
		(error "The From node ~S lacks a value for the schemenr attribute"
		       from-node))))))
    (setf (text item)
	  (array->newline-delimited-string item-text-as-array))))

(defun exportable-items (article)
  (itemize-preprocess article)
  (loop
     with nr-vid-table = (make-hash-table :test #'equal) ; keys are pairs of integers; for theorems and definitions
     with scheme-table = (make-hash-table :test #'eq) ; keys are integers; for schemes only
     with candidates = (remove-if-not #'(lambda (candidate)
					  (typep candidate 'pseudo-item))
				      (item-candidates article))
     for candidate in candidates
     collecting (rewrite-item candidate nr-vid-table scheme-table) into items
     finally (return items)))

;;; itemize.lisp ends here