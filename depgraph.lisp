;;; depgraph.lisp
;;;
;;; Compute the dependency graph among mizar items, assuming a
;;; brutalized store of articles.

(in-package :mizar)

(define-constant +set-pattern+ "hidden:mpattern:1" :test #'string=)
(define-constant +set-constructor+ "hidden:mconstructor:1" :test #'string=)
(define-constant +=-pattern+ "hidden:rpattern:1" :test #'string=)
(define-constant +=-constructor+ "hidden:rconstructor:1" :test #'string=)
(define-constant +<>-pattern+ "hidden:rpattern:2" :test #'string=)
(define-constant +in-pattern+ "hidden:rpattern:3" :test #'string=)
(define-constant +in-constructor+ "hidden:rconstructor:2" :test #'string=)
(define-constant +singleton-pattern+ "tarski:kpattern:1" :test #'string=)
(define-constant +singleton-constructor+ "tarski:kconstructor:1" :test #'string=)
(define-constant +unordered-pair-pattern+ "tarski:kpattern:2" :test #'string=)
(define-constant +unordered-pair-constructor+ "tarski:kconstructor:2" :test #'string=)
(define-constant +subset-pattern+ "tarski:rpattern:1" :test #'string=)
(define-constant +subset-constructor+ "tarski:rconstructor:1" :test #'string=)
(define-constant +ordered-pair-pattern+ "tarski:kpattern:2" :test #'string=)
(define-constant +ordered-pair-constructor+ "tarski:kconstructor:2" :test #'string=)
(define-constant +union-pattern+ "tarski:kpattern:3" :test #'string=)
(define-constant +union-constructor+ "tarski:kconstructor:3" :test #'string=)
(define-constant +are_equipotent-pattern+ "tarski:rpattern:2" :test #'string=)
(define-constant +are_equipotent-constructor+ "tarski:rconstructor:2" :test #'string=)

(define-constant +fragment-filename-pattern+ "^CKB[0-9]+$" :test #'string=)

(defparameter *items-needed-for-fragment*
  (make-hash-table :test #'equal)
  "A table that maps fragment to lists of items.  The intended
  interpretation of a mapping FRAGMENT --> ITEMS is that the fragment
  FRAGMENT immediately depends on each item in ITEMS.")

(defvar *item-to-fragment-table* nil
  "A table that maps items to the fragment from which they came.")

(defun path-for-itemized-article (article-name)
  (pathname-as-directory
   (format nil "~a/~a" (mizar-items-config 'itemization-source) article-name)))

(defun text-subdirectory-of-itemized-article (article-name)
  "A pathname representing the directory under which the article
fragments of the article ARTICLE-NAME can be found."
  (pathname-as-directory
   (format nil "~a/~a/text" (mizar-items-config 'itemization-source) article-name)))

(defun fragment-< (ckb-path-1 ckb-path-2)
  "Assuming that CKB-PATH-1 and CKB-PATH-2 are pathnames that point to
two different article fragments of the same article, return whether
the number of fragment at CKB-PATH-1 is less than the number of the
fragment at CKB-PATH-2."
  (let ((ckb-name-1 (file-namestring ckb-path-1))
	(ckb-name-2 (file-namestring ckb-path-2)))
    (register-groups-bind (ckb-number-str-1)
	("ckb([0-9]+)" ckb-name-1)
      (register-groups-bind (ckb-number-str-2)
	  ("ckb([0-9]+)" ckb-name-2)
	(let ((ckb-number-1 (parse-integer ckb-number-str-1))
	      (ckb-number-2 (parse-integer ckb-number-str-2)))
	  (< ckb-number-1 ckb-number-2))))))

(defun fragments-for-article (article-name)
  "A list of pathnames representing the article fragments of ARTICLE-NAME."
  (let ((text-dir (text-subdirectory-of-itemized-article article-name))
	(ckbs nil))
    (flet ((ckb-only (path)
	     (let ((filename (file-namestring path)))
	       (when (scan "ckb[0-9]+.miz$" filename)
		 (push path ckbs)))))
      (walk-directory text-dir #'ckb-only))
    (let ((ckbs-sorted (sort ckbs #'fragment-<)))
      ckbs-sorted)))

(defun second-line-of-file (path)
  (if (file-exists-p path)
      (let (second-line)
	(with-open-file (s path
			   :direction :input)
	  ;; throw away
	  (read-line s)
	  (setf second-line
		(read-line s)))
	second-line)
      (error "There is no file at '~a', so we cannot read its second line." path)))

(defun final-directory-of-directory-path (path)
  (first (last (pathname-directory path))))

(defun directories-in-directory (dir)
  (remove-if-not #'directory-p
		 (list-directory dir)))

(defun articles-present-in-itemization-directory ()
  (intersection *mml-lar*
		(mapcar #'final-directory-of-directory-path
			(directories-in-directory (mizar-items-config 'itemization-source)))
		:test #'string=))

(defun lines-in-file-matching (path pattern)
  (let (matches)
    (with-open-file (article path
			     :direction :input
			     :if-does-not-exist :error)
      (do ((line (read-line article nil)
		 (read-line article nil)))
	  ((null line))
	(multiple-value-bind (beg end)
	    (scan pattern line)
	  (when beg
	    (push (subseq line beg end) matches)))))
      (reverse matches)))

(defun lines-in-header-matching (path-to-article pattern)
  (let (matches)
    (with-open-file (article path-to-article
			     :direction :input
			     :if-does-not-exist :error)
      (do ((line (read-line article nil)
		 (read-line article nil))
	   (done? nil))
	  ((or (null line)) done?)
	(if (scan "^::" line)
	    (when (scan pattern line)
	      (push line matches))
	    (setf done? t)))
      (reverse matches))))

(defun path-to-fragment-for-article (article-name fragment-number)
  (format nil "~a/~a/text/ckb~d.miz"
	  (mizar-items-config 'itemization-source)
	  article-name
	  fragment-number))

(defun new-value-of-attribute (attr-name xml-line)
  (register-groups-bind (attr-value)
      ((format nil " ~a=\"([^\"]*)\"" attr-name) xml-line)
    attr-value))

(defun new-value-of-aid-attribute (xml-line)
  (new-value-of-attribute "aid" xml-line))

(defun new-value-of-constrnr-attribute (xml-line)
  (new-value-of-attribute "constrnr" xml-line))

(defun new-value-of-kind-attribute (xml-line)
  (new-value-of-attribute "kind" xml-line))

(defun new-value-of-nr-attribute (xml-line)
  (new-value-of-attribute "nr" xml-line))

(defun new-value-of-constrkind-attribute (xml-line)
  (new-value-of-attribute "constrkind" xml-line))

(defun new-value-of-defnr-attribute (xml-line)
  (new-value-of-attribute "defnr" xml-line))

(defun new-value-of-schemenr-attribute (xml-line)
  (new-value-of-attribute "schemenr" xml-line))

(defun new-value-of-relnr-attribute (xml-line)
  (new-value-of-attribute "relnr" xml-line))

(defun new-value-of-propnr-attribute (xml-line)
  (new-value-of-attribute "propnr" xml-line))

(defun scheme-xml-line->item (scheme-line article)
  (let ((schemenr (new-value-of-schemenr-attribute scheme-line))
	(nr (new-value-of-nr-attribute scheme-line)))
    (if schemenr
	(format nil "~(~a~):scheme:~a" article schemenr)
	(format nil "~(~a~):scheme:~a" article nr))))

(defun justifiedtheorem-xml-line->item (justifiedtheorem-line)
  (let ((nr (new-value-of-nr-attribute justifiedtheorem-line))
	(aid (new-value-of-aid-attribute justifiedtheorem-line)))
    (if (scan +fragment-filename-pattern+ aid)
	(format nil "theorem:~a" nr)
	(format nil "~(~a~):theorem:~a" aid nr))))

(defparameter *propnr-of-article->lemma* (make-hash-table :test #'equal)
  "A mapping from pairs (ARTICLE-NAME . PROPNR) to positive natural
  numbers LEMMA-NUMBER.  A mapping represents that the toplevel
  unexported theorem with the indicated PROPNR attibute is the lemma
  number LEMMA-NUMBER.")

(defun unexported-theorem-fragment? (fragment-path)
  (let ((second (second-line-of-file fragment-path)))
    (scan "^:: <Proposition " second)))

(defun initialize-unexported-toplevel-theorems-for-article (article)
  (loop
     initially (format t "Initializing toplevel unexported theorems for article ~a..." article)
     with lemma-nr = 1
     with fragments = (fragments-for-article article)
     for fragment in fragments
     do
       (when (unexported-theorem-fragment? fragment)
	 (let* ((second (second-line-of-file fragment))
		(propnr-str (new-value-of-propnr-attribute second))
		(propnr (parse-integer propnr-str)))
	   (setf (gethash (cons article propnr) *propnr-of-article->lemma*) lemma-nr)
	   (incf lemma-nr)))
     finally
       (format t "done~%")))

(defun theorem-xml-line->item (theorem-line source-article)
  (let* ((nr (new-value-of-nr-attribute theorem-line))
	 (aid (new-value-of-aid-attribute theorem-line))
	 (kind (new-value-of-kind-attribute theorem-line)))
    (if (scan +fragment-filename-pattern+ aid)
	(register-groups-bind (ckb-num-str)
	    ("CKB([0-9]+)" aid)
	  (let* ((fragment-num (parse-integer ckb-num-str))
		 (constrnr (new-value-of-constrnr-attribute theorem-line))
		 (local-ckb-path (path-to-fragment-for-article source-article
							       fragment-num)))
	    (if (string= kind "T")
		(let* ((justifiedtheorem-lines (lines-in-header-matching local-ckb-path
									"<JustifiedTheorem "))
		       (proposition-lines (lines-in-header-matching local-ckb-path
								    "<Proposition ")))
		  (if justifiedtheorem-lines
		      (let ((justifiedtheorem-line (first justifiedtheorem-lines)))
			(justifiedtheorem-xml-line->item justifiedtheorem-line))
		      (let* ((proposition-line (first proposition-lines)))
			(proposition-xml-line->item proposition-line source-article))))
		(let ((deftheorem-lines (lines-in-header-matching local-ckb-path
							       "<DefTheorem ")))
		  (loop
		     for other-deftheorem-line in deftheorem-lines
		     for deftheorem-constrnr = (new-value-of-constrnr-attribute other-deftheorem-line)
		     do
		       (if (string= constrnr deftheorem-constrnr)
			   (let ((other-aid (new-value-of-aid-attribute other-deftheorem-line))
				 (other-nr (new-value-of-nr-attribute other-deftheorem-line)))
			     (return (format nil "~(~a~):deftheorem:~a" other-aid other-nr)))
			   (warn "This deftheorem line '~a' has a constrnr that doesn't match what we're looking for (~a)" other-deftheorem-line constrnr))
		     finally
		       (error "We were unable to resolve the article-local deftheorem line '~a'" theorem-line))))))
	(if (string= kind "D")
	    (format nil "~(~a~):deftheorem:~a" aid nr)
	    (format nil "~(~a~):theorem:~a" aid nr)))))

(defun proposition-xml-line->item (proposition-line article)
  (let* ((propnr-str (new-value-of-propnr-attribute proposition-line))
	 (propnr (parse-integer propnr-str)))
    (multiple-value-bind (lemma-nr present?)
	(gethash (cons article propnr) *propnr-of-article->lemma*)
      (if present?
	  (format nil "~(~a~):lemma:~d" article lemma-nr)
	  (progn
	    (initialize-unexported-toplevel-theorems-for-article article)
	    (multiple-value-bind (final-lemma-nr present-now?)
		(gethash (cons article propnr) *propnr-of-article->lemma*)
	      (if present-now?
		  (format nil "~(~a~):lemma:~d" article final-lemma-nr)
		  (error "There is no proposition from article ~a with PROPNR attribute equal to ~a" article propnr)))))))
)

(defun constructor-xml-line->item (constructor-line source-article)
  (let ((kind (new-value-of-kind-attribute constructor-line))
	(nr (new-value-of-nr-attribute constructor-line))
	(aid (new-value-of-aid-attribute constructor-line)))
    (if (scan +fragment-filename-pattern+ aid)
	(register-groups-bind (ckb-num-str)
	    ("CKB([0-9]+)" aid)
	  (let* ((fragment-num (parse-integer ckb-num-str))
		 (relnr (new-value-of-relnr-attribute constructor-line))
		 (local-ckb-path (path-to-fragment-for-article source-article
							       fragment-num)))
	    (let ((constructor-lines (lines-in-header-matching local-ckb-path
							       "<Constructor .*>")))
	      (loop
		 for other-constructor-line in constructor-lines
		 for constructor-relnr = (new-value-of-relnr-attribute other-constructor-line)
		 do
		   (when (string= relnr constructor-relnr)
		     (let ((other-aid (new-value-of-aid-attribute other-constructor-line))
			   (other-kind (new-value-of-kind-attribute other-constructor-line))
			   (other-nr (new-value-of-nr-attribute other-constructor-line)))
		       (return (format nil "~(~a~):~(~a~)constructor:~a" other-aid other-kind other-nr))))
		 finally
		   (error "We were unable to resolve the article-local constructor line '~a'" constructor-line)))))
	(format nil "~(~a~):~(~a~)constructor:~a" aid kind nr))))

(defun pattern-xml-line->item (pattern-line article)
  (let ((kind (new-value-of-kind-attribute pattern-line))
	(nr (new-value-of-nr-attribute pattern-line))
	(aid (new-value-of-aid-attribute pattern-line)))
    (if (scan +fragment-filename-pattern+ aid)
	(register-groups-bind (ckb-num-str)
	    ("CKB([0-9]+)" aid)
	  (let* ((fragment-num (parse-integer ckb-num-str))
		 (relnr (new-value-of-relnr-attribute pattern-line))
		 (local-ckb-path (path-to-fragment-for-article article
							       fragment-num)))
	    (let ((pattern-lines (lines-in-header-matching local-ckb-path
							       "<Pattern .*>")))
	      (loop
		 for other-pattern-line in pattern-lines
		 for pattern-relnr = (new-value-of-relnr-attribute other-pattern-line)
		 do
		   (when (string= relnr pattern-relnr)
		     (let ((other-aid (new-value-of-aid-attribute other-pattern-line))
			   (other-kind (new-value-of-kind-attribute other-pattern-line))
			   (other-nr (new-value-of-nr-attribute other-pattern-line)))
		       (return (format nil "~(~a~):~(~a~)pattern:~a" other-aid other-kind other-nr))))
		 finally
		   (error "We were unable to resolve the article-local pattern line '~a'" pattern-line)))))
	(format nil "~(~a~):~(~a~)pattern:~a" aid kind nr))))

(defun definiens-xml-line->item (definiens-line source-article)
  (let ((kind (new-value-of-constrkind-attribute definiens-line))
	(nr (new-value-of-defnr-attribute definiens-line))
	(aid (new-value-of-aid-attribute definiens-line)))
    (if (scan +fragment-filename-pattern+ aid)
	(register-groups-bind (ckb-num-str)
	    ("CKB([0-9]+)" aid)
	  (let* ((fragment-num (parse-integer ckb-num-str))
		 (relnr (new-value-of-relnr-attribute definiens-line))
		 (local-ckb-path (path-to-fragment-for-article source-article
							       fragment-num)))
	    (let ((definiens-lines (lines-in-header-matching local-ckb-path
							     "<Definiens ")))
	      (loop
		 for other-definiens-line in definiens-lines
		 for definiens-relnr = (new-value-of-relnr-attribute other-definiens-line)
		 do
		   (when (string= relnr definiens-relnr)
		     (let ((other-aid (new-value-of-aid-attribute other-definiens-line))
			   (other-kind (new-value-of-constrkind-attribute other-definiens-line))
			   (other-nr (new-value-of-defnr-attribute other-definiens-line)))
		       (return (format nil "~(~a~):~(~a~)definiens:~a" other-aid other-kind other-nr))))
		 finally
		   (error "We were unable to resolve the article-local definiens line '~a'" definiens-line)))))
	(format nil "~(~a~):~(~a~)definiens:~a" aid kind nr))))

(defun deftheorem-from-definiens (definiens-item)
  (let ((fragment (gethash definiens-item *item-to-fragment-table*)))
    (when fragment
      (destructuring-bind (fragment-article . fragment-number)
	  fragment
	(loop
	   for k being the hash-keys in *item-to-fragment-table*
	   for (key-article . key-number) = (gethash k *item-to-fragment-table*)
	   do
	     (when (and (string= key-article fragment-article)
			(= key-number fragment-number)
			(deftheorem-item? k))
	       (return k)))))))

(defun deftheorem-xml-line->item (deftheorem-line source-article)
  (let ((nr (new-value-of-nr-attribute deftheorem-line))
	(aid (new-value-of-aid-attribute deftheorem-line)))
    (if (scan +fragment-filename-pattern+ aid)
	(register-groups-bind (ckb-num-str)
	    ("CKB([0-9]+)" aid)
	  (let* ((fragment-num (parse-integer ckb-num-str))
		 (constrnr (new-value-of-constrnr-attribute deftheorem-line))
		 (local-ckb-path (path-to-fragment-for-article source-article
							       fragment-num)))
	    (let ((deftheorem-lines (lines-in-header-matching local-ckb-path
							      "<DefTheorem ")))
	      (loop
		 for other-deftheorem-line in deftheorem-lines
		 for deftheorem-constrnr = (new-value-of-constrnr-attribute other-deftheorem-line)
		 do
		   (if (string= constrnr deftheorem-constrnr)
		       (let ((other-aid (new-value-of-aid-attribute other-deftheorem-line))
			     (other-nr (new-value-of-nr-attribute other-deftheorem-line)))
			 (return (format nil "~(~a~):deftheorem:~a" other-aid other-nr)))
		       (warn "This deftheorem line '~a' has a constrnr that doesn't match what we're looking for (~a)" other-deftheorem-line constrnr))
		 finally
		   (error "We were unable to resolve the article-local deftheorem line '~a'" deftheorem-line)))))
	(format nil "~(~a~):deftheorem:~a" aid nr))))

(defun cluster-xml-line->item (cluster-line)
  (let* ((nr (new-value-of-nr-attribute cluster-line))
	 (aid (new-value-of-aid-attribute cluster-line))
	 (local-aid? (scan "CKB[0-9]+" aid)))
    (cond ((scan "<CCluster .*>" cluster-line)
	   (if local-aid?
	       (format nil "ccluster:~a" nr)
	       (format nil "~(~a~):ccluster:~a" aid nr)))
	  ((scan "<FCluster .*>" cluster-line)
	   (if local-aid?
	       (format nil "fcluster:~a" nr)
	       (format nil "~(~a~):fcluster:~a" aid nr)))
	  ((scan "<RCluster .*>" cluster-line)
	   (if local-aid?
	       (format nil "rcluster:~a" nr)
	       (format nil "~(~a~):rcluster:~a" aid nr)))
	  (t
	   (error "Unhandled cluster line '~a'" cluster-line)))))

(defun identification-xml-line->item (identification-line)
  (let ((nr (new-value-of-nr-attribute identification-line))
	(kind (new-value-of-constrkind-attribute identification-line))
	(aid (new-value-of-aid-attribute identification-line)))
    (if (scan +fragment-filename-pattern+ identification-line)
	(format nil "~(~a~)identification:~a" kind nr)
	(format nil "~(~a~):~(~a~)identification:~a" aid kind nr))))

(defun article-from-fragment-path (path)
  (let ((dir (pathname-directory path)))
    (second (reverse dir))))

(defun fragment-path->items (fragment-path)
  (let ((second-line (second-line-of-file fragment-path))
	(article (article-from-fragment-path fragment-path))
	(items nil))
    (cond ((scan ":: <SchemeBlock " second-line)
	   (push (scheme-xml-line->item second-line article) items))
	  ((scan ":: <JustifiedTheorem " second-line)
	   (push (justifiedtheorem-xml-line->item second-line) items))
	  ((scan ":: <Proposition " second-line)
	   (push (proposition-xml-line->item second-line article) items))
	  ((scan ":: <DefinitionBlock " second-line)
	   (let ((constructors (lines-in-header-matching fragment-path
							 "<Constructor .*>"))
		 (patterns (lines-in-header-matching fragment-path
						     "<Pattern .*>"))
		 (definientia (lines-in-header-matching fragment-path
							"<Definiens "))
		 (deftheorems (lines-in-header-matching fragment-path
							"<DefTheorem ")))
	     ;; constructors
	     (dolist (constructor-line constructors)
	       (push (constructor-xml-line->item constructor-line article) items))
	     ;; patterns
	     (dolist (pattern-line patterns)
	       (push (pattern-xml-line->item pattern-line article) items))
	     ;; definiens
	     (dolist (definiens-line definientia)
	       (push (definiens-xml-line->item definiens-line article) items))
	     ;; deftheorem
	     (dolist (deftheorem-line deftheorems)
	       (push (deftheorem-xml-line->item deftheorem-line article) items))))
	  ((scan ":: <NotationBlock " second-line)
	   (let ((patterns (lines-in-header-matching fragment-path
						     "<Pattern .*>")))
	     (dolist (pattern-line patterns)
	       (let ((kind (new-value-of-kind-attribute pattern-line))
		     (nr (new-value-of-nr-attribute pattern-line)))
		 (push (format nil "~(~a~):~(~a~)pattern:~a" article kind nr)
		       items)))))
	  ((scan ":: <RegistrationBlock" second-line)
	   ;; there should never be more than one of any of these
	   ;; since we have split up registration blocks into
	   ;; "singleton" registrations, but just for consistency of
	   ;; programming we will implement a general solution that
	   ;; would handle arbitrarily many of the three kinds of
	   ;; clusters
	   (let ((cclusters (lines-in-header-matching fragment-path
						      "<CCluster .*>"))
		 (fclusters (lines-in-header-matching fragment-path
						      "<FCluster .*>"))
		 (rclusters (lines-in-header-matching fragment-path
						      "<RCluster .*"))
		 (identifications (lines-in-header-matching fragment-path
							    "<Identify")))
	     ;; cclusters
	     (dolist (cluster-line (append cclusters fclusters rclusters))
	       (push (cluster-xml-line->item cluster-line) items))
	     ;; identifications
	     (dolist (identification-line identifications)
	       (push (identification-xml-line->item identification-line)
		     items))))
	  (t
	   (warn "We don't know how to handle the XML fragment '~a', coming from ~a ~%" second-line fragment-path)
	   nil))
    items))

(defun item-to-fragments-for-article (article-name)
  (loop
     with fragment-paths = (fragments-for-article article-name)
     for fragment-path in fragment-paths
     for i from 1
     for items = (fragment-path->items fragment-path)
     collect (list article-name i items) into article-items
     finally
       (return article-items)))

(defun environment-file-for-fragment (article fragment-number extension)
  (format nil "~a/~a/text/ckb~d.~a"
	  (mizar-items-config 'itemization-source)
	  article
	  fragment-number
	  extension))

(defgeneric clusters-needed-for-fragment (article fragment-number))
(defgeneric theorems-needed-for-fragment (article fragment-number))
(defgeneric schemes-needed-for-fragment (article fragment-number))
(defgeneric definientia-needed-for-fragment (article fragment-number))
(defgeneric patterns-needed-for-fragment (article fragment-number))
(defgeneric identifications-needed-for-fragment (article fragment-number))
(defgeneric constructors-needed-for-fragment (article fragment-number))

;; remove duplicates

(defmethod clusters-needed-for-fragment :around (article fragment-number)
  (let ((needed (call-next-method)))
    (remove-duplicates needed :test #'string=)))

(defmethod theorems-needed-for-fragment :around (article fragment-number)
  (let ((needed (call-next-method)))
    (remove-duplicates needed :test #'string=)))

(defmethod schemes-needed-for-fragment :around (article fragment-number)
  (let ((needed (call-next-method)))
    (remove-duplicates needed :test #'string=)))

(defmethod definientia-needed-for-fragment :around (article fragment-number)
  (let ((needed (call-next-method)))
    (remove-duplicates needed :test #'string=)))

(defmethod patterns-needed-for-fragment :around (article fragment-number)
  (let ((needed (call-next-method)))
    (remove-duplicates needed :test #'string=)))

(defmethod identifications-needed-for-fragment :around (article fragment-number)
  (let ((needed (call-next-method)))
    (remove-duplicates needed :test #'string=)))

(defmethod constructors-needed-for-fragment :around (article fragment-number)
  (let ((needed (call-next-method)))
    (remove-duplicates needed :test #'string=)))

;; dispatch on strings.  This isn't really possible with normal CLOS
;; dispatching, so we are going to dispatch on symbols instead.
;; Thanks to sellout on #lisp on irc.freenode.net for the suggestion.

(defmethod clusters-needed-for-fragment ((article string) fragment-number)
  (clusters-needed-for-fragment (intern article (find-package :mizar))
				fragment-number))
(defmethod theorems-needed-for-fragment ((article string) fragment-number)
  (theorems-needed-for-fragment (intern article (find-package :mizar))
				fragment-number))
(defmethod schemes-needed-for-fragment ((article string) fragment-number)
  (schemes-needed-for-fragment (intern article (find-package :mizar))
			       fragment-number))
(defmethod definientia-needed-for-fragment ((article string) fragment-number)
  (definientia-needed-for-fragment (intern article (find-package :mizar))
      fragment-number))
(defmethod patterns-needed-for-fragment ((article string) fragment-number)
  (patterns-needed-for-fragment (intern article (find-package :mizar))
				fragment-number))
(defmethod identifications-needed-for-fragment ((article string) fragment-number)
  (identifications-needed-for-fragment (intern article (find-package :mizar))
				       fragment-number))
(defmethod constructors-needed-for-fragment ((article string) fragment-number)
  (constructors-needed-for-fragment (intern article (find-package :mizar))
				    fragment-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special cases: HIDDEN and TARSKI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hidden

(define-constant +hidden-symbol+ (intern "hidden" (find-package :mizar)))

(defmethod clusters-needed-for-fragment ((article (eql +hidden-symbol+)) fragment-number)
  nil)

(defmethod theorems-needed-for-fragment ((article (eql +hidden-symbol+)) fragment-number)
  nil)

(defmethod schemes-needed-for-fragment ((article (eql +hidden-symbol+)) fragment-number)
  nil)

(defmethod definientia-needed-for-fragment ((article (eql +hidden-symbol+)) fragment-number)
  nil)

(defmethod patterns-needed-for-fragment ((article (eql +hidden-symbol+)) fragment-number)
  (cond ((= fragment-number 1) nil)
	((= fragment-number 2) (list +set-pattern+))
	((= fragment-number 3) (list +set-pattern+))
	((= fragment-number 4) (list +set-pattern+))))

(defmethod identifications-needed-for-fragment ((article (eql +hidden-symbol+)) fragment-number)
  nil)

(defmethod constructors-needed-for-fragment ((article (eql +hidden-symbol+)) fragment-number)
  (cond ((= fragment-number 1)
	 nil)
	((= fragment-number 2)
	 (list +set-constructor+))
	((= fragment-number 3)
	 (list +set-constructor+))
	((= fragment-number 4)
	 (list +set-constructor+))))

;; tarski

(define-constant +tarski-symbol+ (intern "tarski" (find-package :mizar)))

(defmethod clusters-needed-for-fragment ((article (eql +tarski-symbol+)) fragment-number)
  nil)

(defmethod theorems-needed-for-fragment ((article (eql +tarski-symbol+)) fragment-number)
  nil)

(defmethod schemes-needed-for-fragment ((article (eql +tarski-symbol+)) fragment-number)
  nil)

(defmethod definientia-needed-for-fragment ((article (eql +tarski-symbol+)) fragment-number)
  nil)

(defmethod patterns-needed-for-fragment ((article (eql +tarski-symbol+)) fragment-number)
  (cond ((= fragment-number 1)
	 nil)
	((= fragment-number 2)
	 (list +set-pattern+
	       +in-pattern+
	       +=-pattern+))
	((= fragment-number 3)
	 (list +set-pattern+
	       +in-pattern+
	       +=-pattern+))
	((= fragment-number 4)
	 (list +set-pattern+
	       +in-pattern+
	       +=-pattern+))
	((= fragment-number 5)
	 nil)
	((= fragment-number 6)
	 nil)
	((= fragment-number 7)
	 (list +set-pattern+
	       +in-pattern+))
	((= fragment-number 8)
	 (list +set-pattern+
	       +in-pattern+))
	((= fragment-number 9)
	 nil)
	((= fragment-number 10)
	 nil)
	((= fragment-number 11)
	 (list +set-pattern+
	       +in-pattern+
	       ))
	((= fragment-number 12)
	 (list +set-pattern+
	       +in-pattern+
	       +=-pattern+))
	((= fragment-number 13)
	 (list +set-pattern+
	       +=-pattern+
	       +singleton-pattern+
	       +unordered-pair-pattern+))
	((= fragment-number 14)
	 nil)
	((= fragment-number 15)
	 (list +set-pattern+
	       +in-pattern+
	       +ordered-pair-pattern+
	       +=-pattern+))
	((= fragment-number 16)
	 (list +set-pattern+
	       +in-pattern+
	       +subset-pattern+
	       +are_equipotent-pattern+))))

(defmethod identifications-needed-for-fragment ((article (eql +tarski-symbol+)) fragment-number)
  nil)

(defmethod constructors-needed-for-fragment ((article (eql +tarski-symbol+)) fragment-number)
  (cond ((= fragment-number 1)
	 nil)
	((= fragment-number 2)
	 (list +set-constructor+
	       +in-constructor+
	       +=-constructor+))
	((= fragment-number 3)
	 (list +set-constructor+
	       +in-constructor+
	       +=-constructor+))
	((= fragment-number 4)
	 (list +set-constructor+
	       +in-constructor+
	       +=-constructor+))
	((= fragment-number 5)
	 nil)
	((= fragment-number 6)
	 nil)
	((= fragment-number 7)
	 (list +set-constructor+
	       +in-constructor+))
	((= fragment-number 8)
	 (list +set-constructor+
	       +in-constructor+))
	((= fragment-number 9)
	 nil)
	((= fragment-number 10)
	 nil)
	((= fragment-number 11)
	 (list +set-constructor+
	       +in-constructor+
	       ))
	((= fragment-number 12)
	 (list +set-constructor+
	       +in-constructor+
	       +=-constructor+))
	((= fragment-number 13)
	 (list +set-constructor+
	       +=-constructor+
	       +singleton-constructor+
	       +unordered-pair-constructor+))
	((= fragment-number 14)
	 nil)
	((= fragment-number 15)
	 (list +set-constructor+
	       +in-constructor+
	       +ordered-pair-constructor+
	       +=-constructor+))
	((= fragment-number 16)
	 (list +set-constructor+
	       +in-constructor+
	       +subset-constructor+
	       +are_equipotent-constructor+))))

;; everything else

(defmacro needed-for-fragment (article-symbol fragment-number extension pattern xml-line->item)
  `(let* ((env-file-extension ,extension)
	  (article-name (symbol-name ,article-symbol))
	  (fragment-env-file-path (environment-file-for-fragment article-name
								 ,fragment-number
								 env-file-extension)))
     (flet ((maybe-prepend-article (item)
	      (if (scan "[^:]+:[^:]+:[^:]+" item) ; fully qualified
		  item
		  (format nil "~a:~a" article-name item))))
       (when (file-exists-p fragment-env-file-path)
	 (mapcar #'maybe-prepend-article
		 (mapcar ,xml-line->item
			 (lines-in-file-matching fragment-env-file-path
						 ,pattern)))))))

(defmethod clusters-needed-for-fragment (article fragment-number)
  (needed-for-fragment article fragment-number "ecl" "<[CFR]Cluster .*>" #'cluster-xml-line->item))

(defmethod theorems-needed-for-fragment (article fragment-number)
  (let* ((article-name (symbol-name article))
	 (fragment-env-file-path (environment-file-for-fragment article-name
								fragment-number
								"eth")))
    (when (file-exists-p fragment-env-file-path)
      (mapcar #'(lambda (line) (theorem-xml-line->item line article))
	      (lines-in-file-matching fragment-env-file-path "<Theorem ")))))

(defmethod schemes-needed-for-fragment (article fragment-number)
  (needed-for-fragment article fragment-number "esh" "<Scheme "
		       #'(lambda (line)
			   (scheme-xml-line->item line article))))

(defmethod definientia-needed-for-fragment (article fragment-number)
  (let* ((article-name (symbol-name article))
	 (fragment-env-file-path (environment-file-for-fragment article-name
								fragment-number
								"dfs")))
    (when (file-exists-p fragment-env-file-path)
      (mapcar #'(lambda (line) (definiens-xml-line->item line article))
	      (lines-in-file-matching fragment-env-file-path "<Definiens ")))))

(defmethod patterns-needed-for-fragment (article fragment-number)
  (needed-for-fragment article fragment-number "eno" "<Pattern .*>" #'(lambda (line) (pattern-xml-line->item line article))))

(defmethod identifications-needed-for-fragment (article fragment-number)
  (needed-for-fragment article fragment-number "eid" "<Identify " #'identification-xml-line->item))

(defmethod constructors-needed-for-fragment (article fragment-number)
  (needed-for-fragment article fragment-number "atr.pruned" "<Constructor .*>" #'(lambda (line) (constructor-xml-line->item line article))))

(defun items-needed-for-fragment (article fragment-number)
  (append (clusters-needed-for-fragment article fragment-number)
	  (theorems-needed-for-fragment article fragment-number)
	  (schemes-needed-for-fragment article fragment-number)
	  (definientia-needed-for-fragment article fragment-number)
	  (patterns-needed-for-fragment article fragment-number)
	  (identifications-needed-for-fragment article fragment-number)
	  (constructors-needed-for-fragment article fragment-number)))

(defun items-needed-for-article-by-fragment (article)
  (loop
     with num-fragments = (length (fragments-for-article article))
     for i from 1 upto num-fragments
     collecting (cons i (items-needed-for-fragment article i)) into needed
     finally (return needed)))

(defun items-needed-for-article (article)
  (remove-duplicates
   (reduce #'append
	   (mapcar #'rest
		   (items-needed-for-article-by-fragment article)))
   :test #'string=))

(defun items-needed-for-item (item)
  (let ((fragment (gethash item *item-to-fragment-table*)))
    (destructuring-bind (fragment-article . fragment-number)
	fragment
      (items-needed-for-fragment fragment-article fragment-number))))

(defun items-for-article (article)
  (loop
     with pattern = (format nil "^~a:" article)
     for k being the hash-keys in *item-to-fragment-table*
     when (scan pattern k) collect k into items
     finally (return items)))

(defun items-needed-for-article-by-item (article)
  (loop
     with items = (items-for-article article)
     for item in items
     collecting (cons item (items-needed-for-item item)) into needed
     finally (return needed)))

(defun cluster-item? (item)
  (scan ":[cfr]cluster:" item))

(defun clusters-needed-for-article-by-item (article)
  (loop
     with items = (items-for-article article)
     for item in items
     for needed-items = (items-needed-for-item item)
     collecting (cons item
		      (remove-if-not #'cluster-item? needed-items)) into needed
     finally (return needed)))

(defun definiens-item? (item)
  (scan ":.definiens:" item))

(defun definiens-needed-for-article-by-item (article)
  (loop
     with items = (items-for-article article)
     for item in items
     for needed-items = (items-needed-for-item item)
     collecting (cons item
		      (remove-if-not #'definiens-item? needed-items)) into needed
     finally (return needed)))

(defun deftheorem-item? (item)
  (scan ":deftheorem:" item))

(defun deftheorems-needed-for-article-by-item (article)
  (loop
     with items = (items-for-article article)
     for item in items
     for needed-items = (items-needed-for-item item)
     collecting (cons item
		      (remove-if-not #'deftheorem-item? needed-items)) into needed
     finally (return needed)))

(defun make-item-to-fragment-table ()
  (loop
     with table = (make-hash-table :test #'equal)
     with articles = (append '("hidden" "tarski") (articles-present-in-itemization-directory))
     with num-articles = (length articles)
     for article in articles
     for i from 1
     for mappings = (item-to-fragments-for-article article)
     do
       (format t "~a (~d of ~d)~%" article i num-articles)
       (dolist (mapping mappings)
	 (destructuring-bind (article fragment-number items)
	     mapping
	   (dolist (item items)
	     (setf (gethash item table) (cons article fragment-number)))))
     finally
       (return table)))

(defun make-items-needed-for-item-table ()
  (loop
     with table = (make-hash-table :test #'equal)
     for item being the hash-keys in *item-to-fragment-table*
     for needed-items = (items-needed-for-item item)
     for i from 1
     do
       (setf (gethash item table) needed-items)
       (when (zerop (mod i 1000))
	 (format t "Processed ~d items~%" i))
     finally
       (return table)))

(defun make-items-needed-for-items-across-articles (article-list)
  (loop
     with table = (make-hash-table :test #'equal)
     for article in article-list
     do
       (format t "Computed needed items for items in ~a..." article)
       (loop
	  for item in (items-for-article article)
	  for needed-items = (items-needed-for-item item)
	  do
	    (setf (gethash item table) needed-items))
       (format t "done~%")
     finally
       (return table)))

(defun make-dependency-tables ()
  "Construct two tables, returned as two values: one that maps items
to the fragments from which they come, and another that maps a
fragment to the list of items needed for it."
  (values (make-hash-table :test #'equal)
	  (make-hash-table :test #'equal)))
