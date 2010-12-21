;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Itemization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun article-uses-article (article-1 article-2)
  (member article-1 (complete-environment article-2)))

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
	  (let ((label (label-for-vid article
				      (value-of-vid-attribute now-node))))
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
				   :end-column-number end-column-num)
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
	  (let ((label (label-for-vid article
				      (value-of-vid-attribute iterequality-node))))
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
				    end-line-num end-column-num)))
	  (push (make-instance 'iterequality-item
			       :source-article article
			       :begin-line-number begin-line-num
			       :begin-column-number begin-column-num
			       :end-line-number end-line-num
			       :end-column-number end-column-num)
		items)))
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

(defun justified-theorem-items (article)
  (with-slots (xml-doc)
      article
    (let (items)
      (xpath:do-node-set (justifiedtheorem-node (xpath:evaluate "Article/JustifiedTheorem[not(SkippedProof)]" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (let ((prop-node (first-child-with-name justifiedtheorem-node "Proposition")))
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
				 :end-column-number end-column-num)
		  items))))
	(reverse items))))

(defun proposition-items (article)
  (with-slots (xml-doc)
      article
    (let (items)
      (xpath:do-node-set (proposition-node (xpath:evaluate "Article/Proposition" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (let ((label (label-for-vid article
				      (value-of-vid-attribute proposition-node))))
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
			       :end-column-number end-column-num)
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
				   :end-column-number end-column-num)
		    items)))))
    (reverse items))))

(defun definitionblock-items (article)
  (block-items "DefinitionBlock" "definition" 'definition-item article))

(defun schemeblock-items (article)
  (block-items "SchemeBlock" "scheme" 'scheme-item article))

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
			    (justified-theorem-items article)
			    (proposition-items article)
			    (definitionblock-items article)
			    (schemeblock-items article)
			    (registrationblock-items article)
			    (notationblock-items article))))
    (let ((sorted-everything (stable-sort everything #'item-lex-less)))
      (reverse (disjoin-items sorted-everything)))))

(defun itemize-preprocess (article)
  (strip-comments article)
  (accom article "-q" "-l" "-s")
  (JA1 article "-q" "-l" "-s")
  (dellink article "-q" "-l" "-s")
  (CutSet article "-q" "-l" "-s")
  (CutReconsider article "-q" "-l" "-s")
  (change article "-q" "-l" "-s")
  (verifier article "-q" "-l" "-s")
  (absrefs article)
  (refresh-text article)
  (refresh-idx article))

;;; itemize.lisp ends here