;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Itemization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun article-uses-article (article-1 article-2)
  (member article-1 (complete-environment article-2)))

(defun itemize (&rest articles)
  (let ((articles (sort articles #'article-uses-article)))
    articles))

(defun reservation-statements (article)
  (article-lines-matching "^ *reserve .*$" article))

(defun set-statements (article)
  (article-lines-matching "^ *set .*$" article))

(defun consider-statements (article)
  (article-lines-matching "^ *consider .*$" article))

(defun reconsider-statements (article)
  (article-lines-matching "^ *reconsider .*$" article))

(defun defpred-statements (article)
  (article-lines-matching "^ *defpred .*$" article))

(defun deffunc-statements (article)
  (article-lines-matching "^ *deffunc .*$" article))

(defun now-statements (article)
  (with-slots (xml-doc)
      article
    (let (statements)
      (xpath:do-node-set (now-node (xpath:evaluate "Article/Now" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (let ((vid (value-of-vid-attribute now-node))
		(label nil))
	    (when vid
	      (setf label (gethash vid (idx-table article))))
	    (multiple-value-bind (almost-begin-line-num almost-begin-col-num)
		(line-and-column now-node)
	      (multiple-value-setq (begin-line-num begin-column-num)
		(first-keyword-before article (format nil "~A:" label) almost-begin-line-num almost-begin-col-num))
	      (let ((last-endposition-child (last-child-with-name now-node "EndPosition")))
		(multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child)))
	      (push (list begin-line-num begin-column-num
			  end-line-num end-column-num)
		    statements)))))
	(reverse statements))))

(defun iter-equality-statements (article)
  (with-slots (xml-doc)
      article
    (let (statements)
      (xpath:do-node-set (iterequality-node (xpath:evaluate "Article/IterEquality" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  ;; iterequalities are weird: in the xml, we find out now
	  ;; where they begin, but where they end.  To find out where
	  ;; they begin, we have to look backwards from the end for
	  ;; the label.
	  (let ((vid (value-of-vid-attribute iterequality-node))
		(label nil))
	    (when vid
	      (setf label (gethash vid (idx-table article))))
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
	  (push (list begin-line-num begin-column-num
		      end-line-num end-column-num)
		statements)))
	(reverse statements))))

(defun first-keyword-before (article keyword line-num col-num)
  "Look for the first occurence of the keyword KEYWORD (e.g.,
'theorem', 'definition', or possibly a label such as 'Lm3') before
LINE-NUM and COL-NUM in the text of ARTICLE."
  ; assume the keyword always begins a line -- a terrible assumption
  (let ((bol-theorem-scanner (create-scanner (format nil "(^~A$)|(^~A )" keyword keyword))))
    (loop for l from line-num downto 0
	  for line = (line-at article l)
       do
	 (when (scan bol-theorem-scanner line)
	   (return (values l 0)))
       finally
	 (error "We didn't find the required keyword ~A before line ~d and column ~d in article ~S"
		keyword line-num col-num article))))

(defun first-theorem-keword-before (article line-num col-num)
  (first-keyword-before article "theorem" line-num col-num))

(defun first-definition-keword-before (article line-num col-num)
  (first-keyword-before article "definition" line-num col-num))

(defun first-scheme-keword-before (article line-num col-num)
  (first-keyword-before article "scheme" line-num col-num))

(defun justified-theorem-statements (article)
  (with-slots (xml-doc)
      article
    (let (statements)
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
	    (push (list begin-line-num begin-column-num
			end-line-num end-column-num)
		  statements))))
	(reverse statements))))

(defun proposition-statements (article)
  (with-slots (xml-doc)
      article
    (let (statements)
      (xpath:do-node-set (proposition-node (xpath:evaluate "Article/Proposition" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (let ((vid (value-of-vid-attribute proposition-node))
		(label nil))
	    (when vid
	      (setf label (gethash vid (idx-table article))))
	    (multiple-value-bind (almost-begin-line-num almost-begin-col-num)
		(line-and-column proposition-node)
	      (multiple-value-setq (begin-line-num begin-column-num)
		(first-keyword-before article (format nil "~A:" label) almost-begin-line-num almost-begin-col-num))
	      (let ((proof-node (proof-after-proposition proposition-node)))
		(if proof-node
		    (let ((last-endposition-child (last-child-with-name proof-node "EndPosition")))
		      (multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child)))
		    (let ((by-or-from (xpath:first-node (xpath:evaluate "By | From" proposition-node))))
		      (if by-or-from
			  (let ((last-ref-node (last-child-with-name by-or-from "Ref")))
			    (multiple-value-setq (end-line-num end-column-num) (line-and-column last-ref-node)))
			  (multiple-value-setq (end-line-num end-column-num) proposition-node))))))
	    (push (list begin-line-num begin-column-num
			end-line-num end-column-num)
		  statements))))
	(reverse statements))))

(defun block-statements (xml-element-name mizar-keyword article)
  (with-slots (xml-doc)
      article
    (let (statements)
      (xpath:do-node-set (block-node (xpath:evaluate (format nil "Article/~A" xml-element-name) xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (multiple-value-bind (almost-begin-line-num almost-begin-col-num)
	      (line-and-column block-node)
	    (multiple-value-setq (begin-line-num begin-column-num)
	      (first-keyword-before article mizar-keyword almost-begin-line-num almost-begin-col-num))
	    (let ((last-endposition-child (last-child-with-name block-node "EndPosition")))
	      (multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child))
	      (push (list begin-line-num begin-column-num
			  end-line-num end-column-num)
		    statements)))))
    (reverse statements))))

(defun definitionblock-statements (article)
  (block-statements "DefinitionBlock" "definition" article))

(defun schemeblock-statements (article)
  (block-statements "SchemeBlock" "scheme" article))

(defun registrationblock-statements (article)
  (block-statements "RegistrationBlock" "registration" article))

(defun notationblock-statements (article)
  (block-statements "NotationBlock" "notation" article))

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
  (sort (append (reservation-statements article)
		(set-statements article)
		(consider-statements article)
		(reconsider-statements article)
		(defpred-statements article)
		(deffunc-statements article)
		(now-statements article)
		(iter-equality-statements article)
		(justified-theorem-statements article)
		(proposition-statements article)
		(definitionblock-statements article)
		(schemeblock-statements article)
		(registrationblock-statements article)
		(notationblock-statements article))
	#'tuple-lex-less))

(defun itemize-preprocess (article)
  (strip-comments article)
  (accom article "-q" "-l" "-s")
  (absrefs article)
  (JA1 article "-q" "-l" "-s")
  (dellink article "-q" "-l" "-s")
  (CutSet article "-q" "-l" "-s")
  (CutReconsider article "-q" "-l" "-s")
  (change article "-q" "-l" "-s")
  (verifier article "-q" "-l" "-s")
  (absrefs article)
  (refresh-text article)
  (refresh-xml article)
  (refresh-idx article))

;;; itemize.lisp ends here