;;; depgraph.lisp
;;;
;;; Compute the dependency graph among mizar items, assuming a
;;; brutalized store of articles.

(in-package :mizar)

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
	       (when (scan "ckb[0-9]+.miz" filename)
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

(defun articles-present-in-itemization-directory ()
  (let ((directories (list-directory (mizar-items-config 'itemization-source))))
     (mapcar #'final-directory-of-directory-path directories)))

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

(defun path-to-fragment-for-article-and-number (article-name fragment-number)
  (format nil "~a/~a/text/ckb~d.miz"
	  (mizar-items-config 'itemization-source)
	  article-name
	  fragment-number))

(defun new-value-of-attribute (attr-name xml-line)
  (register-groups-bind (attr-value)
      ((format nil " ~a=\"([^\"]*)\"" attr-name) xml-line)
    attr-value))

(defun new-value-of-kind-attribute (xml-line)
  (new-value-of-attribute "kind" xml-line))

(defun new-value-of-nr-attribute (xml-line)
  (new-value-of-attribute "nr" xml-line))

(defun new-value-of-constrkind-attribute (xml-line)
  (new-value-of-attribute "constrkind" xml-line))

(defun new-value-of-defnr-attribute (xml-line)
  (new-value-of-attribute "defnr" xml-line))

(defun fragment-path->items (fragment-path)
  (let ((second-line (second-line-of-file fragment-path))
	(items nil))
    (cond ((scan ":: <SchemeBlock " second-line)
	   (register-groups-bind (schemenr)
	      (":: <SchemeBlock.* schemenr=\"([0-9]+)\"" second-line)
	    (push (format nil "scheme:~a" schemenr) items)))
	  ((scan ":: <JustifiedTheorem " second-line)
	   (register-groups-bind (theoremnr)
	       (":: <JustifiedTheorem.* nr=\"([0-9]+)\"" second-line)
	     (push (format nil "theorem:~a" theoremnr) items)))
	  ((scan ":: <Proposition " second-line)
	   (let ((nr (new-value-of-nr-attribute second-line)))
	     (push (format nil "lemma:~a" nr) items)))
	  ((scan ":: <DefinitionBlock " second-line)
	   (let ((constructors (lines-in-header-matching fragment-path
							 "<Constructor "))
		 (patterns (lines-in-header-matching fragment-path
						     "<Pattern "))
		 (definientia (lines-in-header-matching fragment-path
							"<Definiens "))
		 (deftheorems (lines-in-header-matching fragment-path
							"<DefTheorem ")))
	     ;; constructors
	     (dolist (constructor-line constructors)
	       (let ((kind (new-value-of-kind-attribute constructor-line))
		      (nr (new-value-of-nr-attribute constructor-line)))
		 (push (format nil "~(~a~)constructor:~a" kind nr)
		       items)))
	     ;; patterns
	     (dolist (pattern-line patterns)
	       (let ((kind (new-value-of-kind-attribute pattern-line))
		     (nr (new-value-of-nr-attribute pattern-line)))
		 (push (format nil "~(~a~)pattern:~a" kind nr)
		       items)))
	     ;; definiens
	     (dolist (definiens-line definientia)
	       (let ((kind (new-value-of-constrkind-attribute definiens-line))
		     (nr (new-value-of-defnr-attribute definiens-line)))
		 (push (format nil "~(~a~)definiens:~a" kind nr)
		       items)))
	     ;; deftheorem
	     (dolist (deftheorem-line deftheorems)
	       (let ((nr (new-value-of-nr-attribute deftheorem-line)))
		 (push (format nil "deftheorem:~a" nr)
		       items)))))
	  ((scan ":: <NotationBlock " second-line)
	   (let ((patterns (lines-in-header-matching fragment-path
						     "<Pattern ")))
	     (dolist (pattern-line patterns)
	       (let ((kind (new-value-of-kind-attribute pattern-line))
		     (nr (new-value-of-nr-attribute pattern-line)))
		 (push (format nil "~(~a~)pattern:~a" kind nr)
		       items)))))
	  ((scan ":: <RegistrationBlock" second-line)
	   ;; there should never be more than one of any of these
	   ;; since we have split up registration blocks into
	   ;; "singleton" registrations, but just for consistency of
	   ;; programming we will implement a general solution that
	   ;; would handle arbitrarily many of the three kinds of
	   ;; clusters
	   (let ((cclusters (lines-in-header-matching fragment-path
						      "<CCluster "))
		 (fclusters (lines-in-header-matching fragment-path
						      "<FCluster "))
		 (rclusters (lines-in-header-matching fragment-path
						      "<RCluster "))
		 (identifications (lines-in-header-matching fragment-path
							    "<Identify")))
	     ;; cclusters
	     (dolist (ccluster-line cclusters)
	       (let ((nr (new-value-of-nr-attribute ccluster-line)))
		 (push (format nil "ccluster:~a" nr)
		       items)))
	     ;; fclusters
	     (dolist (fcluster-line fclusters)
	       (let ((nr (new-value-of-nr-attribute fcluster-line)))
		 (push (format nil "fcluster:~a" nr)
		       items)))
	     ;; rclusters
	     (dolist (rcluster-line rclusters)
	       (let ((nr (new-value-of-nr-attribute rcluster-line)))
		 (push (format nil "rcluster:~a" nr)
		       items)))
	     ;; identifications
	     (dolist (identification-line identifications)
	       (let ((nr (new-value-of-nr-attribute identification-line))
		     (kind (new-value-of-constrkind-attribute identification-line)))
		 (push (format nil "~(~a~)identification:~a" kind nr)
		       items)))))
	  (t
	   nil))
    items))

(defun item-to-fragments-for-article (article-name)
  (loop
     with fragment-paths = (fragments-for-article article-name)
     for fragment-path in fragment-paths
     for i from 1
     for items = (fragment-path->items fragment-path)
     collect (cons i items) into article-items
     finally
       (return article-items)))

(defun make-item-to-fragment-table ()
  (loop
     with articles = (articles-present-in-itemization-directory)
     for article in articles
     for mappings = (item-to-fragments-for-article article)
     appending mappings
   ))

(defun make-dependency-tables ()
  "Construct two tables, returned as two values: one that maps items
to the fragments from which they come, and another that maps a
fragment to the list of items needed for it."
  (values (make-hash-table :test #'equal)
	  (make-hash-table :test #'equal)))

(defun items-needed-for-fragment (fragment)
  (multiple-value-bind (value present?)
      (gethash fragment *items-needed-for-fragment*)
    (if present?
	value
	(error "The fragment '~a' is not present in the items-needed-for-fragment table." fragment))))