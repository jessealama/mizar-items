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
  (declare (ignore article))
  nil)

(defun deffunc-statements (article)
  (declare (ignore article))
  nil)

(defun now-statements (article)
  (declare (ignore article))
  nil)

(defun iter-equality-statements (article)
  (declare (ignore article))
  nil)

(defun justified-theorem-statements (article)
  (with-slots (xml-doc)
      article
    (let (statements)
      (xpath:do-node-set (justifiedtheorem-node (xpath:evaluate "Article/JustifiedTheorem[not(SkippedProof)]" xml-doc))
	(let (begin-line-num begin-column-num end-line-num end-column-num)
	  (let ((prop-node (first-child-with-name justifiedtheorem-node "Proposition")))
	    (multiple-value-setq (begin-line-num begin-column-num) (line-and-column prop-node))
	    (let ((proof-node (first-child-with-name justifiedtheorem-node "Proof")))
	      (if proof-node
		  (let ((last-endposition-child (last-child-with-name proof-node "EndPosition")))
		    (multiple-value-setq (end-line-num end-column-num) (line-and-column last-endposition-child)))
		  (let ((by-or-from (first-child-with-name prop-node "By | From")))
		    (if by-or-from
			(let ((last-ref-node (last-child-with-name by-or-from "Ref")))
			  (multiple-value-setq (end-line-num end-column-num) (line-and-column last-ref-node)))
			(multiple-value-setq (end-line-num end-column-num) prop-node))))))
	  (push (list begin-line-num begin-column-num
		      end-line-num end-column-num)
		statements)))
      (reverse statements))))

(defun proposition-statements (article)
  (declare (ignore article))
  nil)

(defun definitionblock-statements (article)
  (declare (ignore article))
  nil)

(defun schemeblock-statements (article)
  (declare (ignore article))
  nil)

(defun registrationblock-statements (article)
  (declare (ignore article))
  nil)

(defun notationblock-statements (article)
  (declare (ignore article))
  nil)

(defun triple-lex-less (triple-1 triple-2)
  "Determine whether TRIPLE-1 is lexicographically less than TRIPLE-2,
  ignoring the third component of both triples."
  (let ((first-1 (first triple-1))
	(first-2 (first triple-2))
	(second-1 (second triple-1))
	(second-2 (second triple-2)))
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
	#'triple-lex-less))

(defun itemize-preprocess (article)
  (strip-comments article)
  (accom article "-q" "-l" "-s")
  (verifier article "-q" "-l" "-s")
  (absrefs article)
  (JA1 article "-q" "-l" "-s")
  (dellink article "-q" "-l" "-s")
  (CutSet article "-q" "-l" "-s")
  (CutReconsider article "-q" "-l" "-s")
  (refresh-xml article)
  (refresh-idx article))

;;; itemize.lisp ends here