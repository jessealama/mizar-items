
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +downward-arrow-entity+ "&dArr;"
  :test #'string=
  :documentation "A downward-pointing arrow (see also http://www.blooberry.com/indexdot/html/tagpages/entities/arrow.htm).")

(define-constant +upward-arrow-entity+ "&uArr;"
  :test #'string=
  :documentation "An upward-pointing arrow (see also http://www.blooberry.com/indexdot/html/tagpages/entities/arrow.htm).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; URI regular expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +article-name-regexp+ "[a-z_0-9]+" 
  :test #'string=)
(define-constant +positive-natural-number-regexp+ "[1-9][0-9]*"
  :test #'string=
  :documentation "A regular expression matching positive natural numbers.  Starting with the zero digit 0 is not permitted")
(define-constant +number-regexp+
    (regexp-disjoin "0" 
		    +positive-natural-number-regexp+)
  :test #'string=
  :documentation "A regular expression that matches a natural number.  (0 is a natural number.)")
(define-constant +item-kinds-prefixes+ (list "k" "m" "r" "v")
  :test #'equal)
(define-constant +cluster-kinds+ (list "c" "f" "r")
  :test #'equal)
(define-constant +item-kinds-string+ 
  (append (mapcar #'(lambda (sym)
		      (concat sym "constructor"))
		  +item-kinds-prefixes+)
	  (mapcar #'(lambda (sym)
		      (concat sym "pattern"))
		  +item-kinds-prefixes+)
	  '("definiens"
	    "deftheorem"
	    "theorem"
	    "lemma"
	    "scheme"
	    "identification")
	  (mapcar #'(lambda (sym)
		      (concat sym "cluster"))
		  +cluster-kinds+))
  :test #'equal)

(define-constant +item-kind-regexp+ 
    (reduce #'regexp-disjoin +item-kinds-string+)
  :test #'string=)

(define-constant +item-name-regexp+
    (concat +article-name-regexp+
	    ":"
	    "(" +item-kind-regexp+ ")"
	    ":"
	    "(" +positive-natural-number-regexp+ ")")
  :test #'string=)

(define-constant +item-uri-regexp+
    (exact-regexp (concat "/" "item"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"))
  :test #'string=)

(defun uri-for-fragment (article fragment-number)
  (format nil "/fragment/~a/~d" article fragment-number))

(defun uri-for-item (article kind number)
  (format nil "/item/~a/~a/~a" article kind number))

(defun uri-for-item-as-string (item-string)
  (destructuring-bind (article kind number)
      (split-item-identifier item-string)
    (uri-for-item article kind number)))

(defun link-to-item (item)
  (let ((uri (uri-for-item-as-string item))
	(title (item-link-title-from-string item)))
    (with-html-output-to-string (dummy)
      ((:a :href uri :title title)
       (str (pretty-print-item item))))))

(define-constant +fragment-uri-regexp+
    (exact-regexp (concat "/" "fragment"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +number-regexp+ ")"))
  :test #'string=)

(define-constant +itemized-article-uri-regexp+
    (exact-regexp (concat "/" "article"
			  "/" "(" +article-name-regexp+ ")" "/?"))
  :test #'string=)

(define-constant +article-html-uri-regexp+
    (exact-regexp (concat "/" "(" +article-name-regexp+ ")" ".html"))
  :test #'string=
  :documentation "A regular expression matching the URI of an HTML representation of a whole, non-itemized presentation of an article")

(define-constant +article-xml-uri-regexp+
    (exact-regexp (concat "/" "(" +article-name-regexp+ ")" ".xml"))
  :test #'string=
  :documentation "A regular expression matching the URI of an XML representation of an article.")

(defun uri-for-article (article)
  (format nil "/article/~a" article))

(define-constant +path-between-items-form-regexp+
    (exact-regexp "/path")
  :test #'string=)

(define-constant +path-between-items-uri-regexp+
    (exact-regexp (concat "/" "path"
			  "/" "(" +item-name-regexp+ ")"
			  "/" "(" +item-name-regexp+ ")"
			  "/" "(" +positive-natural-number-regexp+ ")"))
  :test #'string=
  :documentation "A regular expression that matches the URI (sans query parameters) for searching for paths in the items dependency graph.")

(defun path-between-items-uri (from to path-number)
  (format nil "/path/~a/~a/~d" from to path-number))

(define-constant +requires-uri-regexp+
    (exact-regexp
     (concat "/" "item"
	     "/" "(" +article-name-regexp+ ")"
	     "/" "(" +item-kind-regexp+ ")"
	     "/" "(" +number-regexp+ ")"
	     "/" "requires"))
  :test #'string=
  :documentation "A regular expression for matching URIs associated with the action of computing what an item requires/depends on.")

(define-constant +supports-uri-regexp+
    (exact-regexp
     (concat "/" "item"
	     "/" "(" +article-name-regexp+ ")"
	     "/" "(" +item-kind-regexp+ ")"
	     "/" "(" +number-regexp+ ")"
	     "/" "supports"))
  :test #'string=
  :documentation "A regular expression for matching URIs associated with the action of computing what an item requires/depends on.")

(define-constant +item-regexp+
    (concat +article-name-regexp+
	    ":"
	    "(" +item-kind-regexp+ ")"
	    ":"
	    +number-regexp+)
  :test #'string=
  :documentation "A regular expression matching names of items.")

(define-constant +dependence-uri-regexp+
    (exact-regexp
     (concat "/" "dependence"
	     "/" "(" +item-regexp+ ")"
	     "/" "(" +item-regexp+ ")"))
  :test #'string=
  :documentation "A regular expression matching URIs associated with displaying the dependence of one item on another.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emitting XML/HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defparameter *absrefs-params*
;;   (list (xuriella:make-parameter "0" "explainbyfrom")))

;; (defparameter *absrefs-sylesheet*
;;   (xuriella:parse-stylesheet
;;    (pathname (mizar-items-config 'absrefs-stylesheet))))

;; (let* ((absrefs-path (mizar-items-config 'absrefs-stylesheet))
;;        (orig-absrefs-sha1 (ironclad:digest-file :sha1 absrefs-path))
;;        (cache (make-hash-table :test #'equalp)))
;;   (labels ((transform (filename)
;; 	     (xuriella:apply-stylesheet *absrefs-sylesheet*
;; 					(pathname filename)
;; 					:parameters *absrefs-params*))
;; 	   (update-cache (path sha1)
;; 	     (setf (gethash sha1 cache) (transform path))))
;;     (defun absrefs (article-xml-path)
;;       (let ((new-absrefs-sha1 (ironclad:digest-file :sha1 absrefs-path))
;; 	    (xml-sha1 (ironclad:digest-file :sha1 article-xml-path)))
;; 	(cond ((equalp orig-absrefs-sha1 new-absrefs-sha1) ; stylesheet unchanged
;; 	       (multiple-value-bind (cached present?)
;; 		   (gethash xml-sha1 cache)
;; 		 (if present?
;; 		     cached
;; 		     (update-cache article-xml-path xml-sha1))))
;; 	      (t ; the stylesheet has changed
;; 	       (clrhash cache) ; all old values are now unreliable
;; 	       (setf orig-absrefs-sha1 new-absrefs-sha1)
;; 	       (update-cache article-xml-path xml-sha1)))))))

;; (defparameter *mhtml-params*
;;   (list (xuriella:make-parameter "1" "colored")
;; 	(xuriella:make-parameter "1" "proof_links")
;; 	(xuriella:make-parameter "1" "titles")))

;; (defparameter *mhtml-stylesheet*
;;   (xuriella:parse-stylesheet
;;    (pathname (mizar-items-config 'mhtml-stylesheet))))

;; (let* ((mhtml-path (mizar-items-config 'mhtml-stylesheet))
;;        (orig-mhtml-sha1 (ironclad:digest-file :sha1 mhtml-path))
;;        (cache (make-hash-table :test #'equalp)))
;;   (labels ((transform (filename &optional source-article-name)
;; 	     (let ((dir (directory-namestring filename))
;; 		   (source-article-param (xuriella:make-parameter "source_article" source-article-name))
;; 		   (mizar-items-param (xuriella:make-parameter "mizar_items" "1")))
;; 	       (flet ((file-in-dir (uri)
;; 			(let ((path (puri:uri-path uri)))
;; 			  (when (scan ".(idx|eno|dfs|eth|esh|atr)$" path)
;; 			    (register-groups-bind (after-root)
;; 				("^/(.+)" path)
;; 			      (let ((new-path (merge-pathnames after-root dir)))
;; 				(setf (puri:uri-path uri)
;; 				      (namestring new-path)))))
;; 			  uri)))
;; 		 (xuriella:apply-stylesheet
;; 		  *mhtml-stylesheet*
;; 		  (absrefs filename)
;; 		  :parameters (append
;; 			       (when source-article-name
;; 				 (list source-article-param
;; 				       mizar-items-param))
;; 			       *mhtml-params*)
;; 		  :uri-resolver #'file-in-dir))))
;; 	   (update-cache (path sha1 &optional source-file-name)
;; 	     (setf (gethash sha1 cache) (transform path source-file-name))))
;;     (defun mhtml (article-xml-path &optional source-article-name)
;;       (let ((new-mhtml-sha1 (ironclad:digest-file :sha1 mhtml-path))
;; 	    (xml-sha1 (ironclad:digest-file :sha1 article-xml-path)))
;; 	(cond ((equalp orig-mhtml-sha1 new-mhtml-sha1) ; stylesheet unchanged
;; 	       (multiple-value-bind (cached present?)
;; 		   (gethash xml-sha1 cache)
;; 		 (if present?
;; 		     cached
;; 		     (update-cache article-xml-path xml-sha1 source-article-name))))
;; 	      (t ; the stylesheet has changed
;; 	       (clrhash cache) ; all old values are now unreliable
;; 	       (setf orig-mhtml-sha1 new-mhtml-sha1)
;; 	       (update-cache article-xml-path xml-sha1 source-article-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *handled-articles* nil
  "Articles that we can handle (i.e., articles for which we have
accurate dependency information and which are stored properly.")

(defun handled-article? (article)
  (member article *handled-articles* :test #'string=))

(defvar *unhandled-articles* nil
  "Articles that might be present in our dependency data, but
  which we do not handle.")

(defun fragment-< (item-name-1 item-name-2)
  (destructuring-bind (item-article-name-1 item-num-as-str-1)
      (split ":" item-name-1)
    (destructuring-bind (item-article-name-2 item-num-as-str-2)
	(split ":" item-name-2)
      (or (string< item-article-name-1 item-article-name-2)
	  (let ((item-num-1 (parse-integer item-num-as-str-1))
		(item-num-2 (parse-integer item-num-as-str-2)))
	    (and (string= item-article-name-1 item-article-name-2)
		 (< item-num-1 item-num-2)))))))

(defun item-< (item-name-1 item-name-2)
  (destructuring-bind (item-article-name-1 item-kind-1 item-num-as-str-1)
      (split ":" item-name-1)
    (destructuring-bind (item-article-name-2 item-kind-2 item-num-as-str-2)
	(split ":" item-name-2)
      (or (string< item-article-name-1 item-article-name-2)
	  (when (string= item-article-name-1 item-article-name-2)
	    (or (string< item-kind-1 item-kind-2)
		(when (string= item-kind-1 item-kind-2)
		  (let ((item-num-1 (parse-integer item-num-as-str-1))
			(item-num-2 (parse-integer item-num-as-str-2)))
		    (< item-num-1 item-num-2)))))))))

(defun emit-about-page ()
  (miz-item-html ("fine-grained dependencies in the mizar mathematical library")
      nil
    (:p "This site aims to illustrate fine-grained dependency
    information about " (:a :href "http://www.mizar.org" "the Mizar
    Mathematical Library") ", a large collection of formalized mathematical knowledge.")
    (:h1 "how are dependencies computed?")
    (:p "For more information about the method for computing this
    information and some applications of it, see:")
    (:ul
     (:li (str "&ldquo;") (:a :href "http://centria.di.fct.unl.pt/~alama/materials/preprints/dependencies-in-formal-mathematics.pdf" "Dependencies in formal mathematics") (str "&rdquo;"))
     (:li (str "&ldquo;") (:a :href "http://centria.di.fct.unl.pt/~alama/materials/preprints/premise-selection-for-mathematics-by-corpus-analysis-and-kernel-methods.pdf" "Premise selection for mathematics by corpus analysis and kernel methods") (str "&rdquo;")))
    (:h1 "what exactly are &ldquo;items&rdquo;?")
    (:p "An " (:b "item") " is anything exported by a top-level piece of
    a " (:tt "MIZAR") " article.")
    (:p "Here are all the different kinds of items:")
    (:dl
     (:dt "theorem")
     (:dd
      (:p "A theorem is a proved mathematical statement.  For example (from FUNCT_1):")
      ((:div :class "miz-example")
     (:pre "
theorem
  [x,y] in f iff x in dom f &amp; y = f.x
proof
  thus [x,y] in f implies x in dom f &amp; y = f.x
  proof
    assume
A1: [x,y] in f;
    hence x in dom f by RELAT_1:def 4;
    hence thesis by A1,Def4;
  end;
  thus thesis by Def4;
end;"))
     (:p "This theorem states a simple property of functions, viewed extensionally as graphs: the pair (" (:em "x,y") ") belongs to " (:em "f") " precisely when " (:em "x") " is in the domain of " (:em "f") " and " (:em "y") " is the value of " (:em "f") " at " (:em "x") ".  The theorem is followed by a proof, which refers to earlier definitions from the same article (that's what the " (:tt "Def4") " is), as well as a definition taken from another article ( " (:tt "RELAT_1:def 4") ")."))
     (:dt "lemma")
     (:dd
      (:p "A lemma is just like a theorem, but is lacks the keyword '" (:tt "theorem") "'.  Example (from ZFMISC_1):")
      ((:div :class "miz-example")
       (:pre "
Lm1: {x} c= X iff x in X
proof
  x in {x} by TARSKI:def 1;
  hence {x} c= X implies x in X by TARSKI:def 3;
  assume
A1: x in X;
  let y;
  thus thesis by A1,TARSKI:def 1;
end;"))
      (:p "Here, the lemma is called " (:tt "Lm1") ".  It says that the singleton set {" (:em "x") "} is included (in " (:tt "MIZAR") "-ese, that's '"(:tt "c=") "') in " (:em "X") " precisely when " (:em "x") " is a member of " (:em "X") ".  The statement of a lemma is geneally followed by a proof.  In the example above, the proof has six lines.  It uses two definitions defined in the article TARSKI: " (:tt "TARSKI:def 1") " and " (:tt "TARSKI:def 3") "."))
     (:dt "scheme")
     (:dd
      (:p "A scheme represents a collection of theorems that are
      substitution instances of some theorem-pattern.  For example:")
      ((:div :class "miz-example")
       (:pre "
scheme
  Extensionality { X,Y() -> set, P[set] } : X() = Y()
provided
A1: for x holds x in X() iff P[x] and
A2: for x holds x in Y() iff P[x]
proof
A3: x in Y() implies x in X()
  proof
    assume x in Y();
    then P[x] by A2;
    hence thesis by A1;
  end;
  x in X() implies x in Y()
  proof
    assume x in X();
    then P[x] by A1;
    hence thesis by A2;
  end;
  hence thesis by A3,TARSKI:2;
end;"))
      (:p "This scheme, taken from " (:tt "XBOOLE_0") ", faciltates proofs of set equality via the principle of extensionality.  (" (:b "Note") ": in set theory extensionality is traditionally not given as a " (:em "scheme") ", but rather as a single (formula) axiom.  To be clear, in " (:tt "MIZAR") " that is also the case; see " ((:a :href "/item/tarski/theorem/1" :title "The axiom of extensionality in MIZAR") "the axiom in the foundational article TARSKI") ".  However, one can also profitably formulate extensionality as a scheme.  Extensionality-as-axiom is used to justify extensionality-as-scheme.)  In this example, the parameters of the scheme are terms " (:em "X") " and " (:em "Y") ", assumed to be sets (in " (:tt "MIZAR") "-ese, we say that that the terms " (:em "X") " and " (:em "Y") "have the type " (:tt "set") ", which is indicated by the arrow " (:tt "->") ") and a property " (:em "P") " of sets.  The scheme says that we are entitled to conclude that the terms " (:em "X") " and " (:em "Y") " are equal if we have proved two things:")
      (:ul
       (:li "for every " (:em "x") " we have that " (:em "x") " is in " (:em "X") "iff the property " (:em "P") " holds of " (:em "x") ", and") 
       (:li "for every " (:em "x") " we have that " (:em "x") " is in " (:em "X") "iff the property " (:em "P") " holds of " (:em "x") ".") )

      (:p "The proof that follows is a justification that indeed " (:em "X") " and " (:em "Y") " are equal under these conditions.  (The reference to " (:tt "TARSKI:2") " is a reference to the " (:em "axiom") " of extensionality in Tarski-Grothendieck set theory.)"))
     (:dt "constructor")
     (:dd (:em "(definition and exmaple forthcoming)"))
     (:dt "pattern")
     (:dd (:em "(definition and exmaple forthcoming)"))
     (:dt "deftheorem")
     (:dd (:em "(definition and exmaple forthcoming)"))
     (:dt "definiens")
     (:dd (:em "(definition and exmaple forthcoming)"))
     (:dt "cluster")
     (:dd (:em "(definition and exmaple forthcoming)"))
     (:dt "identification")
     (:dd (:em "(definition and exmaple forthcoming)")))
    (:p "There's another way to look at items.  The item kinds " (:em "theorem") "," (:em "lemma") "," (:em "scheme") "," (:em "cluster") ", and " (:em "identification") " are, for lack of a better term, " (:b "self-standing") " because and when a top-level block for one of thes kinds of items is processed by the " (:tt "MIZAR") " verifier, one and only one item is produced.  The four items kinds " (:em "constructor") "," (:em "pattern") "," (:em "deftheorem") ", and " (:em "definiens") " are, in general, not self-standing in this sense.  All of these items are produced by a " (:tt "MIZAR") "definition, but moreover, one " (:tt "MIZAR") " definition can produce " (:b "multiple") " items.  For example: the definition (taken from the article " ((:a :href "/article/xboole_0") (:tt "XBOOLE_0")) ")")
    ((:div :class "miz-example")
     (:pre "
definition
  let X be set;
  attr X is empty means
  :Def1:
  not ex x being set st x in X;
end;"))
    (:p "is definition of empty set.  The " (:tt "MIZAR") "verifier produces " (:em "four") " items from this single definition: one constructor, one pattern, one definiens, and one deftheorem.  One can see this by viewing" ((:a :href "/fragment/xboole_0/2") "the view of the article fragment for this definition") ".")
    (:h1 "for the expert: divergences from the official MIZAR Mathematical Library")
    (:p "This site is based on MML version 4.156.1102
    and " (:tt "MIZAR") " binaries based on version 7.11.07.  For more information, consult the " (:tt "MIZAR") " homepage at")
    (:blockquote
     ((:a :href "http://www.mizar.org/") "http://www.mizar.org/"))
    (:p "There are slight differences between the
    official MML and official binary releases available on
    the " (:tt "MIZAR") " homepage.  To process " (:tt "MIZAR") "
    articles so that they can be analyzed in the way that I do, some
    pre-processors are applied to normalize the text of the articles
    of the MML.  The texts needed to be adjusted to suit this
    pre-processing.  Thankfully, very few articles of the MML needed
    to be adjusted.  Moreover, there are are a couple trivial
    differences between the " (:tt "MIZAR") " verifier that is being
    used here and the verifier that one would obtain by downloading
    binaries from the " (:tt "MIZAR") " homepage.  For those who
    know " (:tt "MIZAR") ", the differences are:")
    (:ul
     (:li "The official verifier permits only 50 reservation
     statements to be present in an article.  Because of some
     transformations taht I carry out on the texts, this turns out to
     be too low; I permit 100 reservations.")
     (:li "The official verifier permits at most 90 variables to be
     reserved.  Again, for my purposes, this is too low, and I changed
     it to 150."))
    (:p "These differences have no logical significance; the verifier
    with these changes validates a slightly larger set of texts, but
    the difference has no bearing on the mathematical soundness of
    the " (:tt "MIAR") " verifier.  Informally, the difference is
    this: the verifier would reject locutions such as")
    ((:blockquote :class "font-style:oblique;")
     "Let X" (:sub "1") ", X" (:sub "2") ", " (str "&hellip;") ", X" (:sub "90") ", X" (:sub "91") " be sets such that " (str "&hellip;"))
    (:p "simply because there are too many variables: here, there are 91, but the hard-coded limit in the official " (:tt "MIZAR") " verifier is 90.")
    (:h1 "getting the data")
    (:p "If you like, you can download the data that this site uses and remix it or verify it your own way (e.g., by making your own visualizations of the data or by checking that the dependency graph is acyclic).")
    (:ul
     (:li ((:a :href "/ckb-ckb-depgraph" :title "Fragment dependency graph") "The fragment dependency graph")
	  (:p "Dependency data among article fragments.  This is the heart of the whole site. Although this site emphasizes dependencies among items, what is in fact computed are dependencies among fragments.  Fragments are the human-understanable &lsquo;witnesses&rsquo; to dependencies between items.")
	  (:p "The file is a list of space-separated lines.  A line")
	  (:blockquote
	   (:tt "node-A node-B"))
	  (:p "is an edge in the graph and means: " (:em "node-A immediately depends on node-B") ".  The syntax of nodes in this graph is ")
	  (:blockquote
	   (:tt (str "&lang;article-name&rang;:&lang;fragment-number&rang;"))))
     (:li ((:a :href "/mizar-item-ckb-table" :title "Mapping from framents to items") "The fragment-to-item table")
	  (:p "Mapping between fragments and items.  For some kinds of items, this is a one-to-one mapping.  For definition fragements, though, this is in general a one-to-many relation.")
	  (:p "The dependency information that is presented on this site has a simple mathematical characterization: viewing this table as a relation " (:em "R") " who domain contains fraements and whose range contains items, and the fragment dependency table above as the relation " (:em "S") " whose domain and range are fragments, the full dependency table is simply the composition of " (:em "R") " convervse with the composition of " (:em "S") " and " (:em "R") ".")
	  (:p "The file is a list of space-separated lines.  A line")
	  (:blockquote
	   (:tt "node-A node-B"))
	  (:p "is an edge in the graph and means: " (:em "node-A generates node-B") ". The syntax of nodes in this graph is ")
	  (:blockquote
	   (:tt (str "&lang;article-name&rang;:&lang;fragment-number&rang; &lang;article-name&rang;:&lang;item-kind&rang;:&lang;item-number&rang;"))))
     (:li ((:a :href "/full-item-depgraph" :title "The complete dependency graph for items"))
	  (:p "The dependency graph for items.  It is composed from the previous two graphs.")
	  (:p "The file is a list of space-separated lines.  A line")
	  (:blockquote
	   (:tt (str "&lang;article-name&rang;:&lang;item-kind&rang;:&lang;number&rang; &lang;article-name&rang;:&lang;item-kind&rang;:&lang;number&rang;")))
	  (:p "is an edge in the graph and means: " (:em "the item on the left-hand side immediately depends on the item on the right-hand side") ".")))
    (:h1 "behind the scenes")
    (:p "This site was implemented in " ((:a :href "http://en.wikipedia.org/wiki/Common_Lisp" :title "Common Lisp (wikipedia)") "Common Lisp") " and runs on the " ((:a :href "http://weitz.de/hunchentoot/") "hunchentoot") " web server.  If you're curious, you're welcome to " ((:a :href "https://github.com/jessealama/mizar-items/blob/hunchentoot-site/site.lisp") "browse the source code") " of the site.")))

(defun link-for-item (item)
  (destructuring-bind (article kind num)
      (split ":" item)
    (format nil "/item/~a/~a/~a" article kind num)))


(defun dependence-uri-for-items (item-1 item-2)
  (format nil "/dependence/~a/~a" item-1 item-2))

(defun dependence-link-title (dependent-item supporting-item)
  (destructuring-bind (dep-article dep-kind dep-num)
      (split-item-identifier dependent-item)
    (destructuring-bind (sup-article sup-kind sup-num)
	(split-item-identifier supporting-item)
      (format nil 
	      "~:@(~a~):~a:~a depends on ~:@(~a~):~a:~a"
	      dep-article dep-kind dep-num
	      sup-article sup-kind sup-num))))

(defun emit-direct-dependence-page (item-1 item-2)
  (declare (ignore item-1 item-2))
  (miz-item-html ("verify a dependence")
      nil
    (:p (:em "(Not implemented yet.)"))))

(defgeneric explain-search-solution (source destination solution))

(defmethod explain-search-solution (source destination (steps list))
  (destructuring-bind (source-article source-item-kind source-item-number-str)
      (split ":" source)
    (destructuring-bind (dest-article dest-item-kind dest-item-number-str)
	(split ":" destination)
      (let ((source-uri (uri-for-item-as-string source))
	    (dest-uri (uri-for-item-as-string destination)))
	(with-html-output-to-string (s nil :indent nil)
	  ((:table :class "dependence-path")
	   (:caption
	    "A path of dependence from "
	    ((:a :href source-uri :title source) ((:span :class "article-name") (str source-article)) ":" (str source-item-kind) ":" (str source-item-number-str))
	    " to "
	    ((:a :href dest-uri :title destination) ((:span :class "article-name") (str dest-article)) ":" (str dest-item-kind) ":" (str dest-item-number-str)))
	   (:thead (:tr (:th "Item")))
	   (:tbody
	    (if (length= 1 steps)
		(let* ((item (car steps))
		       (item-html (html-for-item item)))
		  (htm ((:tr :class "dependence-path-node")
			(:td (str item-html)))))
		(loop
		   initially 
		     (let* ((first-step (first steps))
			    (first-step-html (html-for-item first-step))
			    (first-step-uri (uri-for-item-as-string first-step)))
		       (htm
			((:tr :class "dependence-path-node")
			 (:td ((:a :href first-step-uri :title first-step) (str first-step-html))))))
		   for step-from in steps
		   for step-to in (cdr steps)
		   for step-from-html = (html-for-item step-from)
		   for step-from-uri = (uri-for-item-as-string step-from)
		   for step-to-html = (html-for-item step-to)
		   for step-to-uri = (uri-for-item-as-string step-to)
		   for dependence-uri = (dependence-uri-for-items step-from step-to)
		   for dependence-link-title = (dependence-link-title step-from step-to)
		   do
		     (htm
		      ((:tr :class "dependence-path-edge")
		       ((:td :class "arrow") ((:a :href dependence-uri :title dependence-link-title) (str +downward-arrow-entity+))))
		      ((:tr :class "dependence-path-node")
		       (:td ((:a :href step-to-uri :title step-to) (str step-to-html))))))))))))))

(defmethod explain-search-solution (source destination (solution node))
  (explain-search-solution source destination (explain-solution solution)))

(defun path-form-as-string (source destination)
  (with-html-output-to-string (dummy)
    (:fieldset
     (:legend "Specify a path of dependence from a source to a destination")
     ((:form :action "/path"
	     :method "get"
	     :enctype "text/plain")
      (:table
       (:tr
	(:td ((:label :for "source-field") "Source"))
	(:td (if source
		 (htm ((:input :type "textarea"
			       :name "from"
			       :id "from-field"
			       :value source)))
		 (htm ((:input :type "textarea"
			       :id "from-field"
			       :name "from"))))))
       
       (:tr
	(:td ((:label :for "destination-field") "Destination"))
	(:td (if destination
		 (htm ((:input :type "textarea"
			       :name "to"
			       :id "to-field"
			       :value destination)))
		 (htm ((:input :type "textarea"
			       :name "to"
			       :id "to-field"))))))
       (:tr
	((:td :colspan "2")
	 ((:input :type "submit"
		  :value "Search")))))))))

(defvar *path-table* (make-hash-table :test #'equal)
  "A tale mapping pairs (cons cells) (SOURCE . DESTINATION) to pairs (PATHS . MORE-NODES).  The interpretation is that PATHS is a list of all paths computed so far from SOURCE to DESTINATION, and MORE-NODES is a queue representing a 'frozen' search state: the search for all paths from SOURCE to DESTINATION was most recently stopped, but there were MORE-NODES to consider that could lead to more paths being found.")

(defun paths-from-to (source destination)
  (multiple-value-bind (paths-and-more-paths we-done-been-here-before?)
      (gethash (cons source destination) *path-table*)
    (cond (we-done-been-here-before? 
	   (destructuring-bind (path-list . more-nodes)
	       paths-and-more-paths
	     (values path-list more-nodes))) 
	  (t ;; no one has asked for a path between these two
	   (values nil (make-initial-queue source))))))

(defun register-path-with-nodes (source destination path nodes)
  (multiple-value-bind (path-list more-nodes)
      (paths-from-to source destination)
    (setf (gethash (cons source destination) *path-table*)
	  (cons (if (member path path-list :test #'equalp)
		    path-list
		    (cons path path-list))
		nodes))
    more-nodes))

(defun update-paths-with-nodes (source destination new-nodes)
  (multiple-value-bind (paths-and-nodes)
      (gethash (cons source destination) *path-table*)
    (destructuring-bind (paths . nodes)
	paths-and-nodes
      (setf (gethash (cons source destination) *path-table*)
	    (cons paths new-nodes))
      nodes)))

(defgeneric emit-path-between-items-form ())

(defmethod emit-path-between-items-form ()
  (let ((source (get-parameter "from"))
	(destination (get-parameter "to")))
    (if (and source (string/= source ""))
	(if (and destination (string/= destination ""))
	    (if (known-item? source)
		(if (known-item? destination)
		    (redirect (path-between-items-uri source destination 1)
			      :code +http-see-other+)
		    (miz-item-html ("invalid item")
			(:return-code +http-bad-request+)
		      ((:p :class "error-message")
		       "The given destination, '" (str destination) "', is not the name of a known item.")
		      (str (path-form-as-string source nil))))
		(miz-item-html ("invalid item")
		    (:return-code +http-bad-request+)
		  ((:p :class "error-message") 
		   "The given source, '" (str source) "', is not the name of a known item.")
		  (str (path-form-as-string nil destination))))
	    (miz-item-html ("specify a destination")
		nil
	      ((:p :class "error-message")
	       "You must specify a destination item.")
	      (str (path-form-as-string source nil))))
	(if destination
	    (miz-item-html ("specify a source")
		nil
	      ((:p :class "error-message")
	       "You must specify a source item.")
	      (str (path-form-as-string nil destination)))
	    (miz-item-html ("specify a source and destination")
		nil
	      (str (path-form-as-string nil nil)))))))

(defgeneric emit-path-between-items ()
  (:documentation "Display a path that shows a path between two items, possibly with some intermediate items in between."))

(defmethod emit-path-between-items :around ()
  (destructuring-bind (empty path-part source destination path-number-str)
      (split "/" (request-uri*))
    (declare (ignore empty path-part))
    (if (and source (string/= source ""))
	(if (and destination (string/= destination ""))
	  (if (known-item? source)
	      (if (known-item? destination)
		  ;; now check paths.  Generate an error page only
		  ;; when we have computed all paths from the source
		  ;; to the destination, but the given path number
		  ;; is out-of-bounds.
		  (let ((path-number (parse-integer path-number-str)))
		    (multiple-value-bind (paths more-nodes)
			(paths-from-to source destination)
		      (let ((num-paths (length paths)))
			(if (<= path-number num-paths)
			    (call-next-method)
			    (if (empty-queue? more-nodes)
				(miz-item-html ("invalid path request")
				    (:return-code +http-not-found+)
				  ((:p :class "error-message")
				   "There aren't that many paths between"
				   (str (pretty-print-item source))
				   " and "
				   (str (pretty-print-item destination))
				   "; there are (only) "
				   (fmt "~d" num-paths)
				   " such paths.  Please supply a different path number."))
				(if (> path-number (1+ num-paths))
				    (let ((next-path-uri (path-between-items-uri source destination (1+ num-paths)))
					  (next-path-link-title (format nil "Path number #~d from ~a to ~a" (1+ num-paths) source destination)))
				      (miz-item-html ("search for a path")
					  (:return-code +http-see-other+)
					(:p (if (zerop num-paths)
						(htm "We haven't computed any paths from "
						     (str (pretty-print-item source))
						     " to "
						     (str (pretty-print-item destination)) ".")
						(htm "We have already computed "
						     (fmt "~d" num-paths)
						     " path(s) from "
						     (str (pretty-print-item source))
						     " to "
						     (str (pretty-print-item destination)) ". There may be more paths, but we don't know yet.")) 
					    " You have requested path number " (str path-number-str) ".  Since paths are computed one at a time, we cannot process your request for path number " (str path-number-str) " before computing path number " (fmt "~d" (1+ num-paths)) " (which might not even exist).  To proceed, specify a smaller path number for a path that is known to exist, or "
					    ((:a :href next-path-uri
						 :title next-path-link-title) "search for path " (fmt "~d" (1+ num-paths)) "."))))
				    (call-next-method)))))))
		  (miz-item-html ("invalid item")
		      (:return-code +http-bad-request+)
		    ((:p :class "error-message")
		     "The given destination, '" (str destination) "', is not the name of a known item.")
		    (str (path-form-as-string source nil))))
	      (miz-item-html ("invalid item")
		  (:return-code +http-bad-request+)
		((:p :class "error-message") 
		 "The given source, '" (str source) "', is not the name of a known item.")
		(str (path-form-as-string nil destination))))
	  (miz-item-html ("specify a destination")
	      nil
	    ((:p :class "error-message")
	     "You must specify a destination item.")
	    (str (path-form-as-string source nil))))
      (if destination
	  (miz-item-html ("specify a source")
	      nil
	    ((:p :class "error-message")
	     "You must specify a source item.")
	    (str (path-form-as-string nil destination)))
	  (miz-item-html ("specify a source and destination")
	      nil
	    (str (path-form-as-string nil nil)))))))

(defun render-path-search-solution (source destination path path-number link-to-previous? link-to-next?)
  (let* ((path-len (length path))
	 (explanation (explain-search-solution source destination path))
	 (source-uri (uri-for-article source))
	 (source-uri-link-title (item-link-title-from-string source))
	 (dest-uri (uri-for-article destination))
	 (dest-uri-link-title (item-link-title-from-string destination))
	 (next-path-uri (when link-to-next? (path-between-items-uri source destination (1+ path-number))))
	 (next-path-link-title (format nil "Next path from ~a to ~a" source-uri-link-title dest-uri-link-title))
	 (prev-path-uri (when link-to-previous? (path-between-items-uri source destination (1- path-number))))
	 (prev-path-link-title (format nil "Previous path from ~a to ~a" source-uri-link-title dest-uri-link-title)))
    (with-html-output-to-string (dummy)
      ((:table :class "item-table")
       (:tr
	((:td :width "25%" :align "left")
	 ((:table :class "item-info")
	  ((:tr :class "item-info-row")
	   ((:td :colspan "2" :class "item-info-heading") "Path Info"))
	  ((:tr :class "item-info-row")
	   ((:td :class "item-info-key") "Source")
	   ((:td :class "item-info-value")
	    ((:a :href source-uri :class "item-name" :title source-uri-link-title) (str source-uri-link-title))))
	  ((:tr :class "item-info-row")
	   ((:td :class "item-info-key") "Destination")
	   ((:td :class "item-info-value")
	    ((:a :href dest-uri :class "item-name" :title dest-uri-link-title) (str dest-uri-link-title))))
	  ((:tr :class "item-info-row")
	 ((:td :class "item-info-key") "Length")
	 ((:td :class "item-info-value") (str path-len)))
	((:tr :class "item-info-row")
	 ((:td :class "item-info-key") "Path Number")
	 ((:td :class "item-info-value")
	  "[" (if prev-path-uri
		  (htm ((:a :href prev-path-uri :title prev-path-link-title) "&lt;"))
		  (htm "&lt;"))
	  "]"
	  " "
	  (fmt "~d" path-number)
	  " "
	  "[" (if next-path-uri
		  (htm ((:a :href next-path-uri :title next-path-link-title) "&gt;"))
		  (htm "&gt;"))
	  "]"))
	  ((:tr :class "item-info-row")
	   ((:td :colspan "2" :class "item-info-heading") "Items"))
	  ((:tr :class "item-info-row")
	   ((:td :colspan "2" :class "item-info-value")
	    (:ol
	     (loop
		for step in path
		for step-link = (link-to-item step)
		do
		  (htm (:li (str step-link)))))))))
      ((:td :width "75%" :valign "top")
       (str explanation)))))))

(defmethod emit-path-between-items ()
  (destructuring-bind (empty path-part source destination path-number-str)
      (split "/" (request-uri*))
    (declare (ignore empty path-part))
    (multiple-value-bind (paths more-nodes)
	(paths-from-to source destination)
      (let ((path-number (parse-integer path-number-str))
	    (num-paths (length paths)))
      (if (<= path-number num-paths)
	  (let* ((path (nth (1- path-number) paths))
		 (html (render-path-search-solution source
						destination
						path
						path-number
						(> path-number 1)
						(or (not (empty-queue? more-nodes))
						    (<= path-number num-paths)))))
	    (miz-item-html ("path found")
		nil
	      (str html)))
	  (multiple-value-bind (solution-found? solution more-nodes)
	      (one-path source destination +search-depth+ more-nodes)
	    (cond (solution-found?
		   (register-path-with-nodes source destination solution more-nodes)
		   (miz-item-html ("path found")
		       nil
		     (str (render-path-search-solution source
						       destination
						       solution
						       path-number
						       (> path-number 1)
						       (or (not (empty-queue? more-nodes))
							   (<= path-number num-paths))))))
		  ((eq solution :cut-off)
		   ;; update the path table
		   (update-paths-with-nodes source destination more-nodes)
		   (miz-item-html ("search cut off")
		       nil
		     (:p "There may be a path from " (str source) " to " (str destination) ", but we were unable to find one given the current search limits.  (Searches are currenly restricted to not go deeper than depth " (:b (str +search-depth+)))))
		  (t
		   ;; update the path table -- we're done with source and destination
		   (update-paths-with-nodes source destination (make-empty-queue))
		   (miz-item-html ("there is no path")
		       nil
		     (:p "There is no path of dependence from " (str (link-to-item source)) " to " (str (link-to-item destination)) "."))))))))))

(defun article-listing ()
  (with-html-output-to-string (foo)
    ((:table :class "article-listing" :rules "rows")
     (:thead
      (:tr
       (:th "MML Name")
       (:th "Title")))
     (:tbody
      (loop
	 for (article-name title author) in *articles*
	 for article-uri = (uri-for-article article-name)
	 for title-escaped = (escape-string title)
	 do
	   (htm
	    (:tr
	     ((:td :class "article-name")
	      ((:a :href article-uri :title title-escaped)
	       (str article-name)))
	     ((:td :class "article-title") (str title)))))))))

(defgeneric emit-article-xml ()
  (:documentation "Emit an XML representation of an article."))

(defmethod emit-article-xml :around ()
  (register-groups-bind (article)
      (+article-xml-uri-regexp+ (request-uri*))
    (if (member article *mml-lar* :test #'string=)
	(if (handled-article? article)
	    (call-next-method)
	    (miz-item-html ("article cannot be displayed")
		(:return-code +http-internal-server-error+)
	      (:p ((:span :class "article-name") (str article)) " is a valid article in the MML, but it has not yet been processed by this site.  Please try again later.")))
	(miz-item-html ("article not found")
	    (:return-code +http-not-found+)
	  (:p ((:span :class "article-name") (str article)) " is not known.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-article-xml ()
  (register-groups-bind (article)
      (+article-xml-uri-regexp+ (request-uri*))
    (let ((xml-path (xml-path-for-article article)))
      (handle-static-file xml-path "application/xml"))))

(defgeneric emit-unitemized-article-page ()
  (:documentation "An HTML representation of an itemized article."))

(defmethod emit-unitemized-article-page :around ()
  (register-groups-bind (article)
      (+article-html-uri-regexp+ (request-uri*))
    (if (member article *mml-lar* :test #'string=)
	(if (handled-article? article)
	    (call-next-method)
	    (miz-item-html ("article cannot be displayed")
		(:return-code +http-internal-server-error+)
	      (:p ((:span :class "article-name") (str article)) " is a valid article in the MML, but it has not yet been processed by this site.  Please try again later.")))
	(miz-item-html ("article not found")
	    (:return-code +http-not-found+)
	  (:p ((:span :class "article-name") (str article)) " is not known.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-unitemized-article-page ()
  (register-groups-bind (article)
      (+article-html-uri-regexp+ (request-uri*))
    (let* ((html (html-for-article article))
	   (bib-title (article-title article))
	   (title (if bib-title
		      (format nil "~a: ~a" article bib-title)
		      (format nil "~a" article))))
      (miz-item-html (title)
	  nil
	(str html)))))

(defgeneric emit-itemized-article-page ()
  (:documentation "An HTML representation of an itemized article."))

(defmethod emit-itemized-article-page :around ()
  (register-groups-bind (article)
      (+itemized-article-uri-regexp+ (request-uri*))
    (if (member article *mml-lar* :test #'string=)
	(if (handled-article? article)
	    (let ((num-items (gethash article *article-num-items*)))
	      (if (zerop num-items)
		  (miz-item-html ("error")
		      (:return-code +http-internal-server-error+)
		    (:p "The article "
			((:span :class "article-name") (str article))
			" somehow has zero items in it.  Please notify the site maintainers."))
		  (call-next-method)))
	    (miz-item-html ("article cannot be displayed")
		(:return-code +http-internal-server-error+)
	      (:p ((:span :class "article-name") (str article)) " is a valid article in the MML, but it has not yet been processed by this site.  Please try again later.")))
	(miz-item-html ("article not found")
	    (:return-code +http-not-found+)
	  (:p ((:span :class "article-name") (str article)) " is not known.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-itemized-article-page ()
  (register-groups-bind (article)
      (+itemized-article-uri-regexp+ (request-uri*))
    (let* ((num-items (gethash article *article-num-items*))
	   (source-uri (format nil "/~a.miz" article))
	   (mizar-uri (format nil "/~a.html" article))
	   (bib-title (article-title article))
	   (title (if bib-title
		      (format nil "~a: ~a" article bib-title)
		      (format nil "~a" article))))
      (miz-item-html (title)
	  nil
	(:p ((:span :class "article-name") (str article)) " ["  ((:a :href mizar-uri) "non-itemized") ", " ((:a :href source-uri) "source") "] has " (:b (str num-items)) " items ")
	(htm
	 ((:ol :class "fragment-listing")
	  (loop
	     with article-dir = (format nil "~a/~a" (mizar-items-config 'html-source) article)
	     with article-text-dir = (format nil "~a/text" article-dir)
	     for i from 1 upto num-items
	     for i-str = (format nil "fragment-~d" i)
	     for fragment-path = (format nil "~a/ckb~d.html" article-text-dir i)
	     for item-html = (file-as-string fragment-path)
	     for item-uri = (format nil "/fragment/~a/~d" article i)
	     for item-link-title = (format nil "Fragment ~d of ~:@(~a~)" i article)
	     do
	       (htm
		((:li :class "fragment-listing" :id i-str)
		 
		 ((:a :href item-uri :title item-link-title))
		  (str item-html))))))))))

(defun http-sensitive-redirect (new-uri)
  (let ((client-server-protocol (server-protocol*)))	
    (redirect new-uri
	      :code (if (string= client-server-protocol "HTTP/1.1")
			+http-temporary-redirect+
			+http-moved-temporarily+))))

(defun emit-random-item ()
  (let ((random-vertex (random-elt (hash-table-keys *all-items*))))
    (destructuring-bind (article kind number)
	(split ":" random-vertex)
      (http-sensitive-redirect (uri-for-item article kind number)))))

(defmacro register-static-file-dispatcher (uri path &optional mime-type)
  `(progn
     (unless (file-exists-p ,path)
       (warn "Can't register URI '~a' to point to '~a', because there's no file at that path" ,uri ,path))
     (push (create-static-file-dispatcher-and-handler ,uri 
						      ,path
						      ,mime-type)
	   items-dispatch-table)
     t))

(defmacro register-directory-dispatcher (uri path &optional mime-type)
  `(progn
     (unless (file-exists-p ,path)
       (error "Can't register URI '~a' to point to '~a', because there's no file at that path" ,uri ,path))
     (unless (directory-p ,path)
       (error "Can't register URI '~a' to point to directory '~a', because '~a' isn't a directory" ,uri ,path ,path))
     (unless (scan "/$" ,uri)
       (error "Can't register URI '~a' to point to directory '~a', because '~a'  doesn't end with a slash '/'" ,uri ,path ,uri))
     (push (create-folder-dispatcher-and-handler ,uri ,path ,mime-type)
	   items-dispatch-table)
     t))

(defmacro register-regexp-dispatcher (uri-regexp dispatcher)
  `(progn
     (push
      (create-regex-dispatcher ,uri-regexp ,dispatcher)
      items-dispatch-table)
     t))

(defmacro register-exact-uri-dispatcher (uri dispatcher)
  (let ((exact-uri (exact-regexp uri)))
    `(register-regexp-dispatcher ,exact-uri ,dispatcher)))

(defun split-item-identifier (item-string)
  (split ":" item-string))

(defun xml-path-for-item (item-string)
  (destructuring-bind (article-name item-kind item-number)
      (split-item-identifier item-string)
    (let* ((item-key (format nil "~a:~a:~a" article-name item-kind item-number))
	   (ckb-for-item (gethash item-key *item-to-fragment-table*))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir)))
      (when ckb-for-item
	(destructuring-bind (ckb-article-name ckb-number)
	    (split-item-identifier ckb-for-item)
	  (declare (ignore ckb-article-name)) ;; same as ARTICLE
	  (format nil "~a/ckb~d.xml" article-text-dir ckb-number))))))

(defun xml-path-for-article (article-name)
  (let ((article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name)))
    (format nil "~a/~a.xml" article-dir article-name)))

(defun html-path-for-article (article-name)
  (let ((article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name)))
    (format nil "~a/~a.html" article-dir article-name)))

(defun html-path-for-fragment (fragment-string)
  (destructuring-bind (article-name fragment-number)
      (split-item-identifier fragment-string)
    (let* ((article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir)))
      (format nil "~a/ckb~d.html" article-text-dir fragment-number))))

(defun html-path-for-item (item-string)
  (destructuring-bind (article-name item-kind item-number)
      (split-item-identifier item-string)
    (let* ((item-key (format nil "~a:~a:~a" article-name item-kind item-number))
	   (ckb-for-item (gethash item-key *item-to-fragment-table*))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir)))
      (if ckb-for-item
	  (destructuring-bind (ckb-article-name . ckb-number)
	      ckb-for-item
	    (declare (ignore ckb-article-name)) ;; same as ARTICLE
	    (format nil "~a/ckb~d.html" article-text-dir ckb-number))
	  (error "There is no known fragment for the item '~a'" item-string)))))

(defun html-for-item (item-string)
  (file-as-string (html-path-for-item item-string)))

;; (defun html-for-item (item-string)
;;   (mhtml (xml-path-for-item item-string)))

(defun html-for-article (article-name)
  (file-as-string (html-path-for-article article-name)))

;; (defun html-for-article (article-name)
;;   (mhtml (xml-path-for-article article-name)))

(defun pretty-item-kind (item-kind)
  (switch (item-kind :test #'string=)
    ("definiens" "definiens")
    ("deftheorem" "definitional theorem")
    ("kconstructor" "function")
    ("mconstructor" "type")
    ("rconstructor" "relation")
    ("vconstructor" "adjective")
    ("ccluster" "registration")
    ("fcluster" "registration")
    ("rcluster" "registration")
    ("kpattern" "notation (function)")
    ("mpattern" "notation (type)")
    ("rpattern" "notation (relation)")
    ("vpattern" "notation (adjective)")
    ("theorem" "theorem")
    ("lemma" "lemma")
    ("scheme" "scheme")
    ("identification" "identification")
    (t "(unknown)")))

(defun requires-uri-for-item (article-name item-kind item-number)
  (format nil "/item/~a/~a/~a/requires" article-name item-kind item-number))

(defun supports-uri-for-item (article-name item-kind item-number)
  (format nil "/item/~a/~a/~a/supports" article-name item-kind item-number))

(defun search-from-uri (item)
  (format nil "/path?from=~a" item))

(defun search-to-uri (item)
  (format nil "/path?to=~a" item))

(defun search-via-uri (item)
  (format nil "/path?via=~a" item))

(defgeneric emit-mizar-item-page ()
  (:documentation "Emit an HTML representation of a single Mizar item."))

(defmethod emit-mizar-item-page :around ()
  (register-groups-bind (article-name item-kind item-number-str)
      (+item-uri-regexp+ (request-uri*))
    (if (handled-article? article-name)
	(let ((item (item-from-components article-name item-kind item-number-str)))
	  (if (known-item? item)
	      (let ((html-path (html-path-for-item item))
		    (item-name-pretty (item-inline-name article-name item-kind item-number-str)))
		(if (file-exists-p html-path)
		    (if (empty-file-p html-path)
			(miz-item-html ("error")
			    (:return-code +http-internal-server-error+)
			  (:p "The HTML file for the item " (str item-name-pretty) " exists, but has zero size.")
			  (:p "Please inform the site maintainers about this."))
			(call-next-method))
		    (miz-item-html ("error")
			(:return-code +http-internal-server-error+)
		      (:p "The HTML file for the item " (str item-name-pretty) " does not exist.")
		      (:p "Please inform the site maintainers about this."))))
	      (miz-item-html ("error")
		  (:return-code +http-bad-request+)
		(:p "The identifier")
		(:blockquote
		 (str item))
		(:p "is not the name of a known item"))))
	(miz-item-html ("error")
	    (:return-code +http-bad-request+)
	  (:p ((:span :class "article-name") (str article-name)) " is not known, or not yet suitably processed for this site.  Please try again later.")))))

(defmethod emit-mizar-item-page ()
  (register-groups-bind (article-name item-kind item-number-str)
      (+item-uri-regexp+ (request-uri*))
      (let* ((item-number (parse-integer item-number-str))
	     (item-kind-pretty (pretty-item-kind item-kind))
	     (article-uri (uri-for-article article-name))
	     (num-items-for-article (gethash article-name *article-num-items*))
	     (item-key (format nil "~a:~a:~d" article-name item-kind item-number))
	     (ckb-for-item (gethash item-key *item-to-fragment-table*))
	     (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	     (article-text-dir (format nil "~a/text" article-dir)))
	(destructuring-bind (ckb-article-name . ckb-number)
	    ckb-for-item
	  (declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	  (let* ((article-name-uc (format nil "~:@(~a~)" article-name))
		 (fragment-path (format nil "~a/ckb~d.html"
					article-text-dir
					ckb-number))
		 (item-html (file-as-string fragment-path))
		 (prev-ckb-uri (unless (= ckb-number 1)
				 (uri-for-fragment article-name (1- ckb-number))))
		 (prev-ckb-link-title (format nil "Previous fragment in ~:@(~a~)" article-name))
		 (next-ckb-uri (unless (= ckb-number num-items-for-article)
				 (uri-for-fragment article-name (1+ ckb-number))))
		 (next-ckb-link-title (format nil "Next fragment in ~:@(~a~)" article-name))
		 (prev-item-kind-uri (unless (= item-number 1)
				       (uri-for-item article-name item-kind (1- item-number))))
		 (prev-item-kind-link-title (format nil "Previous item of this kind in ~:@(~a~)" article-name))
		 (next-item-kind-uri (unless (= item-number (count-items-of-kind-for-article article-name item-kind))
				       (uri-for-item article-name item-kind (1+ item-number))))
		 (next-item-kind-link-title (format nil "Next item of this kind in ~:@(~a~)" article-name))
		 (search-from-uri (search-from-uri item-key))
		 (search-from-link-title (format nil "Search for paths of dependence starting from ~:@(~a~):~a:~d" article-name item-kind item-number))
		 (search-to-uri (search-to-uri item-key))
		 (search-to-link-title (format nil "Search for paths of dependence ending at ~:@(~a~):~a:~d" article-name item-kind item-number))
		 (search-via-uri (search-via-uri item-key))
		 (search-via-link-title (format nil "Search for paths of dependence passing through ~:@(~a~):~a:~d" article-name item-kind item-number))
		 (requires-uri (requires-uri-for-item article-name item-kind item-number))
		 (supports-uri (supports-uri-for-item article-name item-kind item-number)))
	    (miz-item-html (item-key)
			   nil
			   ((:table :class "item-table")
			    (:tr
			     ((:td :width "25%" :align "left")
			      ((:table :class "item-info")
			       ((:tr :class "item-info-row")
				((:td :colspan "2" :class "item-info-heading") "Item Info"))
			       ((:tr :class "item-info-row")
				((:td :class "item-info-key") "Article")
				((:td :class "item-info-value")
				 ((:a :href article-uri :class "article-name" :title article-name-uc)
				  ((:span :class "article-name") (str article-name)))))
			       ((:tr :class "item-info-row")
				((:td :class "item-info-key") "Fragment")
				((:td :class "item-info-value")
				 "[" (if prev-ckb-uri
					 (htm ((:a :href prev-ckb-uri :title prev-ckb-link-title) "&lt;"))
					 (htm "&lt;"))
				 "]"
				 " "
				 (str ckb-number)
				 " "
				 "[" (if next-ckb-uri
					 (htm ((:a :href next-ckb-uri :title next-ckb-link-title) "&gt;"))
					 (htm "&gt;"))
				 "]"))
			       ((:tr :class "item-info-row")
				((:td :class "item-info-key") "Item Kind")
				((:td :class "item-info-value") (str item-kind-pretty)))
			       ((:tr :class "item-info-row")
				((:td :class "item-info-key") "Number")
				((:td :class "item-info-value")
				 "[" (if prev-item-kind-uri
					 (htm ((:a :href prev-item-kind-uri :title prev-item-kind-link-title) "&lt;"))
					 (htm "&lt"))
				 "]"
				 " "
				 (str item-number-str)
				 " "
				 "[" (if next-item-kind-uri
					 (htm ((:a :href next-item-kind-uri :title next-item-kind-link-title) "&gt;"))
					 (htm "&gt;"))
				 "]"))
			       ((:tr :class "item-info-row")
				((:td :colspan "2" :class "item-info-heading") "Actions"))
			       ((:tr :class "item-info-row")
				((:td :colspan "2") "Search for a path of dependence:"
				 ((:ul :class "path-options")
				  (:li ((:a :href search-from-uri :title search-from-link-title) (:b "starting at")))
				  (:li ((:a :href search-via-uri :title search-via-link-title) (:b "passing through")))
				  (:li ((:a :href search-to-uri :title search-to-link-title) (:b "ending at"))))
				 "this item."))
			       ((:tr :class "item-info-row")
				((:td :colspan "2") "See what this item"
				 ((:ul :class "item-dep-options")
				  (:li ((:a :href requires-uri :title "Items on which this item depends") (:b "requires")))
				  (:li ((:a :href supports-uri :title "Items that this item supports") (:b "supports"))))))))
			     ((:td :width "75%" :valign "top")
			      (str item-html))))))))))

(defun fragment->items (fragment)
  (loop
     for v being the hash-values in *item-to-fragment-table*
     for k being the hash-keys in *item-to-fragment-table*
     when (string= v fragment) collect k into keys
     finally (return keys)))

(defgeneric emit-fragment-page ()
  (:documentation "Emit an HTML representation of a fragment of a Mizar article."))

(defmethod emit-fragment-page :around ()
  (register-groups-bind (article-name fragment-number-str)
      (+fragment-uri-regexp+ (request-uri*))
    (let ((fragment (fragment-from-components article-name fragment-number-str)))
      (if (handled-article? article-name)
	  (if (fragment->items fragment)
	      (let ((html-path (html-path-for-fragment fragment))
		    (fragment-name-pretty (fragment-inline-name article-name fragment-number-str)))
		(if (file-exists-p html-path)
		    (if (empty-file-p html-path)
			(miz-item-html ("error")
			    (:return-code +http-internal-server-error+)
			  (:p "The HTML file for the fragment " (str fragment-name-pretty) " exists, but has zero size.")
			  (:p "Please inform the site maintainers about this."))
			(call-next-method))
		    (miz-item-html ("error")
			(:return-code +http-internal-server-error+)
		      (:p "The HTML file for the fragment " (str fragment-name-pretty) " does not exist.")
		      (:p "Please inform the site maintainers about this."))))
	      (miz-item-html ("error")
		  (:return-code +http-bad-request+)
		(:p "The identifier")
		(:blockquote
		 (str fragment))
		(:p "is not the name of a known fragment")))
	  (miz-item-html ("error")
	      (:return-code +http-bad-request+)
	    (:p ((:span :class "article-name") (str article-name)) " is not known, or not yet suitably processed for this site.  Please try again later."))))))

(defmethod emit-fragment-page ()
  (register-groups-bind (article-name fragment-number-str)
      (+fragment-uri-regexp+ (request-uri*))
    (let* ((fragment-number (parse-integer fragment-number-str))
	   (article-uri (uri-for-article article-name))
	   (num-items-for-article (gethash article-name *article-num-items*))
	   (fragment (format nil "~a:~d" article-name fragment-number))
	   (fragment-items (fragment->items fragment))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir))
	   (article-name-uc (format nil "~:@(~a~)" article-name))
	   (fragment-path (format nil "~a/ckb~d.html" article-text-dir fragment-number-str))
	   (fragment-html (file-as-string fragment-path))
	   (prev-ckb-uri (unless (= fragment-number 1)
			   (uri-for-fragment article-name (1- fragment-number))))
	   (prev-ckb-link-title (format nil "Previous fragment in ~:@(~a~)" article-name))
	   (next-ckb-uri (unless (= fragment-number num-items-for-article)
			   (uri-for-fragment article-name (1+ fragment-number))))
	   (next-ckb-link-title (format nil "Next fragment in ~:@(~a~)" article-name)))
	(miz-item-html (fragment)
	    nil
	  ((:table :class "item-table")
	   (:tr
	    ((:td :width "25%" :align "left")
	     ((:table :class "item-info")
	      ((:tr :class "item-info-row")
	       ((:td :colspan "2" :class "item-info-heading") "Fragment Info"))
	      ((:tr :class "item-info-row")
	       ((:td :class "item-info-key") "Article")
	       ((:td :class "item-info-value")
		((:a :href article-uri :class "article-name" :title article-name-uc)
		 ((:span :class "article-name") (str article-name)))))
	      ((:tr :class "item-info-row")
	       ((:td :class "item-info-key") "Number")
	       ((:td :class "item-info-value")
		"[" (if prev-ckb-uri
			(htm ((:a :href prev-ckb-uri :title prev-ckb-link-title) "&lt;"))
			(htm "&lt;"))
		"]"
		" "
		(str fragment-number-str)
		" "
		"[" (if next-ckb-uri
			(htm ((:a :href next-ckb-uri :title next-ckb-link-title) "&gt;"))
			(htm "&gt;"))
		"]"))
	      ((:tr :class "item-info-row")
	       ((:td :colspan "2" :class "item-info-heading") "Generated Items"))
	      ((:tr :class "item-info-row")
	       ((:td :colspan "2" :class "item-info-value")
		(:ul
		 (dolist (gen-item fragment-items)
		   (let ((gen-item-link (link-to-item gen-item)))
		     (htm (:li (str gen-item-link))))))))))
	    ((:td :width "75%" :valign "top")
	     (str fragment-html))))))))

(defun emit-articles-page ()
  (miz-item-html ("articles from the mml")
      nil
    (:p "The following articles from the MML are handled:")
    ((:table :class "article-listing" :rules "rows")
     (:thead
      (:tr
       (:th "MML Name")
       (:th "Title")))
     (:tbody
      (loop
	 for article-name in *handled-articles*
	 for article-uri = (uri-for-article article-name)
	 do
	   (let ((bib-entry (member article-name *articles*
				    :key #'first
				    :test #'string=)))
	     (if bib-entry
		 (destructuring-bind (identifier title author)
		     (car bib-entry)
		   (declare (ignore identifier author))
		   (let ((title-escaped (escape-string title)))
		     (htm
		      (:tr
		       ((:td :class "article-name")
			((:a :href article-uri :title title-escaped)
			 (str article-name)))
		       ((:td :class "article-title") (str title))))))
		 (htm
		  (:tr
		   ((:td :class "article-name")
		    ((:a :href article-uri :title article-name)
		     (str article-name)))
		   ((:td :class "article-title") "(no title was supplied)"))))))))))

(defun emit-main-page ()
  (miz-item-html ("fine-grained dependencies in mizar")
      nil
    (:h1 "welcome")
    (:p "Interested in learning more about the " ((:a :href "http://www.mizar.org/") (:tt "MIZAR") " Mathematical Library") " (MML), the largest corpus of formalized mathematics in the world?  This site provides a way to
    get a handle on the large contents of the MML. This site presents the MML by showing its " (:b "items") " and showing, for each item, what it " (:b "depends")  " upon and conversely (what items depend on the item).  This website presents " (:tt "MIZAR") " items, their dependency information, and provides a way of exploring these dependencies by finding " (:b "paths") " among dependencies.")
    (:p "The dependency graph that this site lets you explore has "  (:b (str (hash-table-count *all-items*))) " nodes (items) and " (:b (str (count-dependency-graph-edges))) " edges.")
    (:h1 "getting started")
    (:p "One can visit " ((:a :href "/articles") "the complete list of handled articles") ".  Alternatively, one can visit " ((:a :href "/random-item") " a random item") " or " ((:a :href "/random-path") "search for a path between two random items") ".")
    (:p "You might want to visit the " ((:a :href "/landmarks") "landmarks") " page to get acquainted with how this site provides fine-grained dependency information for some notable results of mathematics.") 
    (:h1 "learning more about " (:tt "MIZAR"))
    (:p "The " (:tt "MIZAR") " system and its library, the MML, are rather complex.  To learn more about the system, see the excellent overview article")
    (:blockquote
     (:p
      "&ldquo;"
      ((:a :href "http://jfr.cib.unibo.it/article/view/1980") "Mizar in a nutshell")
      "&rdquo;, by Adam Grabowski, Artur Kornilowicz, and Adam Naumowicz, " (:em "Journal of Formalized Reasoning") " " (:b "3") "(2), (2010), pp. 153&ndash;245"))
    (:p "For a historical overview, see:")
    (:blockquote
     (:p
      "&ldquo;"
      ((:a :href "http://markun.cs.shinshu-u.ac.jp/mizar/mma.dir/2005/mma2005(2).pdf") "MIZAR: The first 30 years")
      "&rdquo;, by Roman Mutuszewski and Piotr Rudnicki, " (:em "Mechanized Mathematics and its Applications") (:b "4") "(1), (2005), pp. 3&ndash;24"))
    (:p "At the moment, this site is not really interactive: you can't work with " (:tt "MIZAR") " texts here.  If you'd like to get your hands dirty, you might want to visit " ((:a :href "http://mws.cs.ru.nl/mwiki/") "the " (:tt "MIZAR") " wiki") " project at Radboud University Nijmegen.")))

(defun item-uri (item-identifier)
  (format nil "/item/~a" (substitute #\/ #\: item-identifier)))
     
(defun emit-landmarks-page ()
  (miz-item-html ("landmarks")
      nil
    (:p "One might also be interested in entering the vast space of " (:tt "MIZAR") " items by inspecting some landmarks.")
    (:p "This page is divided into the following sections:")
    (:ul
     (:li ((:a :href "#mmlquery") "A selected list taken from the work of the MML Query project"))
     (:li ((:a :href "#100theorems") "100 Theorems")))
    ((:h1 :id "mmlquery")
     "A selected list taken from the work of the MML Query project")
    (:p "The following is a list of theorems selected by the maintainers of the " ((:a :href "http://mmlquery.mizar.org/" :title "MML Query") "MML Query") " project, " ((:a :href "http://webdocs.cs.ualberta.ca/~piotr/Mizar/Important_thms.html" :title "Name-carrying facts/theorems/definitions in MML") "extended by Piotr Rudnicki")  ".")
    (:ul
     (loop
	for (entry items) in +piotr-theorems+
	do
	  (if (stringp items) ; unique reference ("/item/article/theorem/5")
	      (htm
	       (:li ((:a :href items :title entry) (str entry))))
	      (if (stringp (car items))
		  (let ((num-alternatives (length items))) ; list of anonymous alternatives ("/item/article/deftheorem/1" "/item/article/deftheorem/5")
		    (if (> num-alternatives 2) ; count alternatives: "Alternative 1" "Alternative 2", ...
			(let ((primary-alternative (first items))
			      (remaining-alternatives (rest items)))
			  (htm
			   (:li 
			    ((:a :href primary-alternative :title entry) (str entry))
			    "["
			    (let ((second-alternative (car remaining-alternatives))
				  (second-alternative-text "alternative 1")
				  (second-alternative-title (format nil "~a (alternative 1)" entry)))
			      (htm
			       ((:a :href second-alternative :title second-alternative-title)
				(str second-alternative-text))))
			    (loop
			       for alternative in (cdr remaining-alternatives)
			       for i from 2
			       for alternative-text = (format nil "alternative ~d" i)
			       for alternative-title = (format nil "~a (alternative ~d" entry i)
			       do
				 (htm
				  ", "
				  ((:a :href alternative :title alternative-title)
				   (str alternative-text))))
			    "]")))
			(let* ((primary (first items))
			       (alternative (second items)) ; only one alternative
			       (alternative-title (format nil "~a (alternative)" entry)))
			  (htm
			   (:li
			    (str entry)
			    (:ul
			     (:li
			      ((:a :href primary :title entry) "Primary version"))
			     (:li
			      ((:a :href alternative :title alternative-title) "Another version"))))))))
		  (htm  ; list of named alternatives (("First" "/item/article/theorem/25") ("Second" "/item/article/theorem/50") ...)
		   (:li (str entry)
			(:ul
			 (dolist (item items)
			   (destructuring-bind (name uri)
			       item
			     (let ((alternative-name (format nil "~a (~a)"
							     entry 
							     name)))
			       (htm
				(:li ((:a :href uri 
					  :title alternative-name)
				      (str name))))))))))))))
    ((:h1 :id "100theorems")
     "100 Theorems")
    (:p "The following is a list of 'notable' mathematical results formalized in " (:tt "MIZAR") ".  What does 'notable' mean here?  Certainly, it's a value term.  This list comes from Freek Wiedijk's "((:a :href "http://www.cs.ru.nl/~freek/100/" :title "Formalizing 100 Theorems") "100 theorems") " and its associated list of " ((:a :href "http://www.cs.ru.nl/~freek/100/mizar.html" :title "Formalizing 100 Theorems in Mizar") "theorems formalized in " (:tt "MIZAR")) ".  Here is the list, with links to the corresponding entries in this site's database.")
    (:dl
     (loop
	for i from 1 upto 100
	for theorem-name across +100-theorems+
	for theorem-name-escaped = (escape-string theorem-name)
	do
	  (htm
	   ((:dt :class "theorem-name") (fmt "~d. ~a" i theorem-name))
	   (:dd
	    (:p
	     (let ((formalizations (gethash i +mizar-formalized+)))
	       (if formalizations
		   (if (cdr formalizations) ; more than one formalization
		       (htm
			(:ul
			 (dolist (formalization formalizations)
			   (htm
			    (:li
			     (let* ((formalization-uri (item-uri formalization))
				    (formalization-html-path (html-path-for-item formalization)))
			       (if formalization-html-path
				   (if (file-exists-p formalization-html-path)
				       (if (empty-file-p formalization-html-path)
					   (htm "(The HTML representation exists but is empty; please notify the site maintainer.)")
					   (let ((formalization-html (file-as-string formalization-html-path)))
					     (htm ((:a :href formalization-uri
						       :class "mhtml-text"
						       :title theorem-name-escaped))
						  (str formalization-html))))
				       (htm "(HTML representation not present)")))))))))
		       (let* ((formalization (car formalizations))
			      (formalization-uri (item-uri formalization))
			      (formalization-html-path (html-path-for-item formalization)))
			 (if formalization-html-path
			     (if (file-exists-p formalization-html-path)
				 (if (empty-file-p formalization-html-path)
				     (htm "(The HTML representation exists but is empty; please notify the site maintainer.)")
				     (let ((formalization-html (file-as-string formalization-html-path)))
				       (htm
					((:a :href formalization-uri
					     :class "mhtml-text"
					     :title theorem-name-escaped)
					 (str formalization-html)))))
				 (htm "(HTML representation not present)"))
			     (htm "(HTML representation not present)"))))
		   (htm
		    (:em "(not yet formalized in " (:tt "MIZAR") ")")))))
	    (let ((100theorems-uri (format nil "http://www.cs.ru.nl/~~freek/100/#~d" i))
		  (100theorems-title (format nil "Known formalizations of: ~a" theorem-name-escaped)))
	      (htm
	       (:p "[" ((:a :href 100theorems-uri
			    :title 100theorems-title)
			"other formalizations") "]")))))))))

(defun emit-feedback-page ()
  (miz-item-html ("feedback")
      nil
    (:p
     "Thanks for using this site.  The maintainer is " ((:a :href "http://centria.di.fct.unl.pt/~alama/") "Jesse Alama") ".  If your have questions, comments, bug reports (e.g., broken links), or feature requests, please do " ((:a :href "mailto:jesse.alama@gmail.com") "send an email") "; your feedback is appreciated.")))

(defun items-by-mml-order (item-list)
  (sort (copy-list item-list) #'item-<))

(defun fragment-from-components (article-name item-number)
  (format nil "~a:~a" article-name item-number))

(defun item-from-components (article-name item-kind item-number)
  (format nil "~a:~a:~a" article-name item-kind item-number))

(defun item-link-title (article kind number)
  (format nil "~:@(~a~):~a:~a" article kind number))

(defun item-link-title-from-string (item-string)
  (destructuring-bind (article kind number)
      (split-item-identifier item-string)
    (item-link-title article kind number)))

(defun item-inline-name (article kind number)
  (with-html-output-to-string (foo)
    (:span
     ((:span :class "article-name") (str article))
     ":"
     (str kind)
     ":"
     (str number))))

(defun fragment-inline-name (article number)
  (with-html-output-to-string (foo)
    (:span
     ((:span :class "article-name") (str article))
     ":"
     (str number))))

(defun pretty-print-item (item)
  (destructuring-bind (article kind number)
      (split-item-identifier item)
    (item-inline-name article kind number)))

(defun earlier-in-tsorted-list (item-1 item-2)
  (< (position item-1 *items-tsorted* :test #'string=)
     (position item-2 *items-tsorted* :test #'string=)))

(defun item-requires-tsorted (item)
  (let* ((requires (gethash item *item-dependency-graph-forward*))
	 (sorted (sort (copy-list requires) #'earlier-in-tsorted-list)))
    sorted))

(defun item-supports-tsorted (item)
  (flet ((item-< (item-1 item-2)
	   (< (position item-1 *items-tsorted*)
	      (position item-2 *items-tsorted*))))
    (let* ((requires (gethash item *item-dependency-graph-backward*))
	   (sorted (sort (copy-list requires) #'item-<)))
      sorted)))

(defun item-article (item-identifier)
  (destructuring-bind (article kind number)
      (split-item-identifier item-identifier)
    (declare (ignore kind number))
    article))

(defun items-by-article (item-list)
  (let ((by-article (make-hash-table :test #'equal)))
    (dolist (item item-list)
      (let ((article (item-article item)))
	(push item (gethash article by-article))))
    (loop
       with by-article-as-list = nil
       for k being the hash-keys in by-article
       do
	 (push (cons k (gethash k by-article))
	       by-article-as-list)
       finally
	 (return by-article-as-list))))

(defun items-by-article-in-mml-lar-order (item-list)
  (let* ((by-article (items-by-article item-list))
	 (sorted (sort by-article #'mml-< :key #'first)))
    sorted))

(defgeneric emit-requires-page ()
  (:documentation "An HTML presentation of a page showing what a given item requires"))

(defmethod emit-requires-page :around ()
  (register-groups-bind (article kind number)
      (+requires-uri-regexp+ (request-uri*))
    (if (belongs-to-mml article)
	(let ((item (item-from-components article kind number)))
	  (if (known-item? item)
	      (call-next-method)
	      (let ((article-uri (uri-for-article article)))
		(miz-item-html ("unknown item")
		    (:return-code +http-not-found+)
		  (:p "The item "
		      ((:a :href article-uri :class "article-name")
		       (str article))
		      ":"
		      (str kind)
		      ":"
		      (str number)
		      " is not known.")))))
	(miz-item-html ("unknown article")
	    (:return-code +http-not-found+)
	  (:p (fmt "'~a'" article) " is not the name of a known article.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-requires-page ()
  (register-groups-bind (article kind number)
      (+requires-uri-regexp+ (request-uri*))
    (let* ((item (item-from-components article kind number))
	   (item-uri (item-uri item))
	   (item-link-title (item-link-title article kind number))
	   (item-inline-name (item-inline-name article kind number))
	   (deps (item-requires-tsorted item)))
      (let* ((requires-page-title (format nil "requirements of ~a" item-link-title))
	     (html (html-for-item item)))
	(miz-item-html (requires-page-title)
	    nil
	  (:p "The item "
	      ((:a :href item-uri :title item-link-title)
	       (str item-inline-name)))
	  (str html)
	  (if (null deps)
	      (htm (:p "requires nothing."))
	      (htm (:p "requires:")
		   (:ul
		    (loop
		       for (article . deps-from-article) in (items-by-article-in-mml-lar-order deps)
		       do
			 (htm
			  (:li ((:span :class "article-name") (str article))
			       (:ul
				(dolist (dep deps-from-article)
				  (destructuring-bind (dep-article dep-kind dep-num)
				      (split-item-identifier dep)
				    (let ((dep-uri (dependence-uri-for-items item dep))
					  (dep-link-title (dependence-link-title item dep))
					  (dep-inline-name (item-inline-name dep-article dep-kind dep-num)))
				      (htm
				       (:li ((:a :href dep-uri :title dep-link-title)
					     (str dep-inline-name)))))))))))))))))))

(defgeneric emit-supports-page ()
  (:documentation "An HTML presentation of the items that a given item supports."))

(defmethod emit-supports-page :around ()
  (register-groups-bind (article kind number)
      (+supports-uri-regexp+ (request-uri*))
    (if (belongs-to-mml article)
	(let ((item (item-from-components article kind number)))
	  (if (known-item? item)
	      (call-next-method)
	      (let ((article-uri (uri-for-article article)))
		(miz-item-html ("unknown item")
		    (:return-code +http-not-found+)
		  (:p "The item "
		      ((:a :href article-uri :class "article-name")
		       (str article))
		      ":"
		      (str kind)
		      ":"
		      (str number)
		      " is not known.")))))
	(miz-item-html ("unknown article")
	    (:return-code +http-not-found+)
	  (:p (fmt "'~a'" article) " is not the name of a known article.  Here is a list of all known articles:")
	  (str (article-listing))))))

(defmethod emit-supports-page ()
  (register-groups-bind (article kind number)
      (+supports-uri-regexp+ (request-uri*))
    (let* ((item (item-from-components article kind number))
	   (item-uri (item-uri item))
	   (item-link-title (item-link-title article kind number))
	   (item-inline-name (item-inline-name article kind number))
	   (deps (item-supports-tsorted item)))
      (let* ((supports-page-title (format nil "what ~a supports" item-link-title))
	     (html (html-for-item item)))
	(miz-item-html (supports-page-title)
	    nil
	  (:p "The item "
	      ((:a :href item-uri :title item-link-title)
	       (str item-inline-name)))
	  (str html)
	  (if (null deps)
	      (htm (:p "supports nothing."))
	      (htm (:p "supports:")
		   (:ul
		    (loop
		       for (article . deps-from-article) in (items-by-article-in-mml-lar-order deps)
		       do
			 (htm
			  (:li ((:span :class "article-name") (str article))
			       (:ul
				(dolist (dep deps-from-article)
				  (destructuring-bind (dep-article dep-kind dep-num)
				      (split-item-identifier dep)
				    (let ((dep-uri (dependence-uri-for-items dep item))
					  (dep-link-title (dependence-link-title item dep))
					  (dep-inline-name (item-inline-name dep-article dep-kind dep-num)))
				      (htm
				       (:li ((:a :href dep-uri :title dep-link-title)
					     (str dep-inline-name)))))))))))))))))))

(defgeneric emit-dependence-page ()
  (:documentation "Emit an HTML description of the dependence between two items."))

(defmethod emit-dependence-page :around ()
  ;; sanity check
  (register-groups-bind (supporting-item whatever dependent-item)
      (+dependence-uri-regexp+ (request-uri*))
    (declare (ignore whatever)) ;; WHATEVER matches the item kind inside SUPPORTING-ITEM
    (if supporting-item
	(if dependent-item
	    (if (known-item? supporting-item)
		(if (known-item? dependent-item)
		    (let ((supp-html-path (html-path-for-item supporting-item))
			  (dep-html-path (html-path-for-item dependent-item)))
		      (if (file-exists-p supp-html-path)
			  (if (file-exists-p dep-html-path)
			      (call-next-method)
			      (miz-item-html ("error")
				  (:return-code +http-internal-server-error+)
				(:p "The HTML file for the item")
				(:blockquote
				 (fmt "~a" dependent-item))
				(:p "doesn't exist at the expected location")
				(:blockquote
				 (fmt "~a" dep-html-path))
				(:p "Please inform the site administrator about this.")))
			  (miz-item-html ("error")
			      (:return-code +http-internal-server-error+)
			    (:p "The HTML file for the item")
			    (:blockquote
			     (fmt "~a" supporting-item))
			    (:p "doesn't exist at the expected location")
			    (:blockquote
			     (fmt "~a" supp-html-path))
			    (:p "Please inform the site administrator about this."))))
		    (miz-item-html ("error")
			(:return-code +http-bad-request+)
		      (:p (fmt "'~a' is not the name of a known item." dependent-item))))
		(miz-item-html ("error")
		    (:return-code +http-bad-request+)
		  (:p (fmt "'~a' is not the name of a known item." supporting-item))))
	    (miz-item-html ("error")
		(:return-code +http-bad-request+)
	      (:p (fmt "Something went wrong matching the requested URI, '~a', against the pattern for this page" (request-uri*)))))
	(miz-item-html ("error")
	    (:return-code +http-bad-request+)
	  (:p (fmt "Something went wrong matching the requested URI, '~a', against the pattern for this page" (request-uri*)))))))

(defmethod emit-dependence-page ()
  (register-groups-bind (supporting-item whatever dependent-item)
      (+dependence-uri-regexp+ (request-uri*))
    (declare (ignore whatever)) ;; WHATEVER matches the item kind inside SUPPORTING-ITEM
    (destructuring-bind (supp-article supp-kind supp-num)
	(split-item-identifier supporting-item)
      (destructuring-bind (dep-article dep-kind dep-num)
	  (split-item-identifier dependent-item)
	(let* ((supp-html (html-for-item supporting-item))
	       (dep-html (html-for-item dependent-item))
	       (supp-title (item-link-title supp-article supp-kind supp-num))
	       (dep-title (item-link-title dep-article dep-kind dep-num))
	       (title (format nil "~a depends on ~a" supp-title dep-title))
	       (supp-name-inline (item-inline-name supp-article supp-kind supp-num))
	       (dep-name-inline (item-inline-name dep-article dep-kind dep-num))
	       (supp-uri (item-uri supporting-item))
	       (dep-uri (item-uri supporting-item)))
      (miz-item-html (title)
	  nil
	(:p "The item " ((:a :href supp-uri :title supp-title) (str supp-name-inline)))
	(str supp-html)
	(:p "depends on " ((:a :href dep-uri :title dep-title) (str dep-name-inline)))
	(str dep-html)
	(:p "because the attempt to verify "
	    ((:a :href supp-uri :title supp-title) (str supp-name-inline))
	    " in the absence of "
	    ((:a :href dep-uri :title dep-title) (str dep-name-inline))
	    " would generate the following " (:tt "MIZAR") " errors:")
	(:blockquote
	 (:em "(not implemented yet; check back soon)"))))))))


(defun register-proofs-for-article (article)
  (let ((num-items (gethash article *article-num-items*)))
    (if (integerp num-items)
	(loop
	   for i from 1 upto num-items
	   for fragment-proof-dir = (format nil "~a/~a/proofs/ckb~d"
					    (mizar-items-config 'html-source)
					    article
					    i)
	   for proofs = (list-directory fragment-proof-dir)
	   do
	     (loop
		for proof-path in proofs
		for proof-namestring = (namestring proof-path)
		for proof-name = (car (last (split "/" proof-namestring)))
		for proof-uri = (format nil "/proofs/~a/ckb~d/~a" article i proof-name)
		do
		  ;; (warn "Registering URI '~a' to point to path '~a'" proof-uri proof-path)
		  (register-static-file-dispatcher proof-uri proof-path "text/xml")))
	(error "The article '~a' does not have a known number of items!" article))))

(defun initialize-uris (&optional (articles :all))
  ;; ecmascript, css
  (register-static-file-dispatcher "/mhtml.css"
				   (mizar-items-config 'mhtml-css-path)
				   "text/css")
  (register-static-file-dispatcher "/mhtml.js"
				   (mizar-items-config 'mhtml-js-path)
				   "text/ecmascript")
  (register-static-file-dispatcher "/ckb-ckb-depgraph"
				   (mizar-items-config 'fragment-depdenency-graph)
				   "text/plain")
  (register-static-file-dispatcher "/full-item-depgraph"
				   (mizar-items-config 'full-item-dependency-graph)
				   "text/plain")
  (register-static-file-dispatcher "/mizar-item-ckb-table"
				   (mizar-items-config 'item-to-fragment-path)
				   "text/plain")
  (register-static-file-dispatcher "/full-vertex-neighbors-depgraph-backward"
				   (mizar-items-config 'vertex-neighbors-backward-graph-path)
				   "text/plain")
  (register-static-file-dispatcher "/full-vertex-neighbors-depgraph-forward"
				   (mizar-items-config 'vertex-neighbors-forward-graph-path)
				   "text/plain")
  ;; intro
  (register-exact-uri-dispatcher "/" #'emit-main-page)
  ;; about page
  (register-exact-uri-dispatcher "/about" #'emit-about-page)
  ;; feedback page
  (register-exact-uri-dispatcher "/feedback" #'emit-feedback-page)
  ;; articles page
  (register-exact-uri-dispatcher "/articles" #'emit-articles-page)
  (register-exact-uri-dispatcher "/landmarks" #'emit-landmarks-page)
  (register-exact-uri-dispatcher "/random-item" #'emit-random-item)
  (register-static-file-dispatcher "/favicon.ico" (mizar-items-config 'favicon-path))
  ;; directory setup
  (push 'hunchentoot-dir-lister:dispatch-dir-listers items-dispatch-table)
  (when articles
    (loop
       for article in (if (eq articles :all)
			      *mml-lar*
			      (first-n *mml-lar* articles))
       do
	 (pushnew article *handled-articles* :test #'string=)
	 (let* ((article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article))
		(miz-uri (format nil "/~a.miz" article))
		(miz-path (format nil "~a/~a.miz" article-dir article))
		(prel-dir-uri (format nil "/~a/prel/" article))
		(prel-dir-path (format nil "~a/prel/" article-dir))
		(text-dir-uri (format nil "/~a/text" article))
		(text-dir-path (format nil "~a/text/" article-dir)))
	   ;; static files for the whole article
	   (register-static-file-dispatcher miz-uri miz-path "text/plain")
	   (hunchentoot-dir-lister:add-simple-lister prel-dir-uri prel-dir-path)
	   (hunchentoot-dir-lister:add-simple-lister text-dir-uri text-dir-path))
       finally
	 (when (integerp articles)
	   (setf *unhandled-articles* (last-n *mml-lar*
					      (- (length *mml-lar*) articles))))))

  ;; three presentations of an article: itemized HTML, unitemized HTML, raw XML
  (register-regexp-dispatcher +itemized-article-uri-regexp+ #'emit-itemized-article-page)
  (register-regexp-dispatcher +article-html-uri-regexp+ #'emit-unitemized-article-page)
  (register-regexp-dispatcher +article-xml-uri-regexp+ #'emit-article-xml)

  ;; fragments
  (register-regexp-dispatcher +fragment-uri-regexp+ #'emit-fragment-page)

  ;; items, what they require, and what they support
  (register-regexp-dispatcher +item-uri-regexp+ #'emit-mizar-item-page)
  (register-regexp-dispatcher +requires-uri-regexp+ #'emit-requires-page)
  (register-regexp-dispatcher +supports-uri-regexp+ #'emit-supports-page)

  ;; paths
  (register-regexp-dispatcher +path-between-items-form-regexp+
			      #'emit-path-between-items-form)
  (register-regexp-dispatcher +path-between-items-uri-regexp+
			      #'emit-path-between-items)

  ;; dependence between items
  (register-regexp-dispatcher +dependence-uri-regexp+ #'emit-dependence-page)

  ;; proofs
  (loop
     for article in *handled-articles*
     do
       (register-proofs-for-article article))
  t)
