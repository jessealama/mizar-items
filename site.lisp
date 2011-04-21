
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
(define-constant +number-regexp+ "[0-9]+"
  :test #'string=)
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

(define-constant +fragment-uri-regexp+
    (exact-regexp (concat "/" "fragment"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/?"))
  :test #'string=)

(define-constant +article-uri-regexp+
    (exact-regexp (concat "/" "article"
			  "/" "(" +article-name-regexp+ ")" "/?"))
  :test #'string=)

(defun uri-for-article (article)
  (format nil "/article/~a" article))

(define-constant +path-between-items-uri-regexp+
    (format nil "^/path[?]?")
  :test #'string=
  :documentation "A regular expression that matches the URI (sans query parameters) for searching for paths in the items dependency graph.")

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
;;; Main page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *handled-articles* nil
  "Articles that we can handle (i.e., articles for which we have
accurate dependency information and which are stored properly.")

(defun handled-article (article)
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

(defun explain-search-solution (source destination solution)
  (destructuring-bind (source-article source-item-kind source-item-number-str)
      (split ":" source)
    (destructuring-bind (dest-article dest-item-kind dest-item-number-str)
	(split ":" destination)
      (let ((steps (explain-solution solution))
	    (source-uri (uri-for-item-as-string source))
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
		       (item-html (file-as-string (html-path-for-item item))))
		  (htm ((:tr :class "dependence-path-node")
			(:td (str item-html)))))
		(loop
		   for step-from in steps
		   for step-to in (cdr steps)
		   for step-from-html = (file-as-string (html-path-for-item step-from))
		   for step-from-uri = (uri-for-item-as-string step-from)
		   for step-to-html = (file-as-string (html-path-for-item step-to))
		   for step-to-uri = (uri-for-item-as-string step-to)
		   for dependence-uri = (dependence-uri-for-items step-from step-to)
		   for dependence-link-title = (dependence-link-title step-from step-to)
		   do
		     (htm
		      ((:tr :class "dependence-path-node")
		       (:td ((:a :href step-from-uri :title step-from) (str step-from-html))))
		      ((:tr :class "dependence-path-edge")
		       ((:td :class "arrow") ((:a :href dependence-uri :title dependence-link-title) (str +downward-arrow-entity+))))
		      ((:tr :class "dependence-path-node")
		       (:td ((:a :href step-to-uri :title step-to) (str step-to-html))))))))))))))

(defgeneric emit-path-between-items ()
  (:documentation "Display a path that shows a path between two items, possibly with some intermediate items in between."))

(defmethod emit-path-between-items :around ()
  "Validate the query string.  It must contain:

* source ('from')
* destination ('to')

It may also contain:

* intermediate steps ('via'), semicolon delimited"
  (let ((source (get-parameter "from"))
	(destination (get-parameter "to"))
	(via (get-parameter "via")))
    (if source
	(if destination
	    (let ((intermediates (split #\; via)))
	      (multiple-value-bind (all-ok bad-guy)
		  (every-with-falsifying-witness intermediates #'known-item?)
		(if all-ok
		    (if (known-item? source)
			(if (known-item? destination)
			    (call-next-method)
			    (miz-item-html ("invalid item")
				(:return-code +http-bad-request+)
			      (:p "The parameter '" (str destination) "' is not the name of a known item.")))
			(miz-item-html ("invalid item")
			    (:return-code +http-bad-request+)
			  (:p "The parameter '" (str source) "' is not the name of a known item.")))
		    (miz-item-html ("invalid item")
			(:return-code +http-bad-request+)
		      (:p "The parameter '" (str bad-guy) "' is not the name of a known item.")))))
	    (miz-item-html ("invalid item")
		(:return-code +http-bad-request+)
	      (:p "You must specify a destination item.")))
	(miz-item-html ("invalid item")
	    (:return-code +http-bad-request+)
	  (:p "You must specify a source item.")))))

(defmethod emit-path-between-items ()
  (let* ((source (get-parameter "from"))
	 (destination (get-parameter "to"))
	 (source-mml-pos (mml-lar-index source)))
    (let ((search-problem (make-item-search-problem :initial-state source
						    :goal destination)))
      ;; custom successors method: if the current node occurs earler
      ;; in the MML than the destination node, don't expand
      (defmethod successors :around ((problem (eql search-problem)) node)
	(let* ((ns (node-state node))
	       (mml-pos (mml-lar-index ns)))
	  (when (<= source-mml-pos mml-pos)
	    (call-next-method))))
      (multiple-value-bind (solution-found? solution)
	  (bounded-depth-first-search search-problem +search-depth+)
	(cond (solution-found?
	       (let ((explanation (explain-search-solution source destination solution)))
		 (miz-item-html ("seach for paths")
		     nil
		   (:p "Here's a solution:")
		   (str explanation))))
	      ((eq solution :cut-off)
	       (miz-item-html ("search cut off")
		   nil
		 (:p "There may be a path from " (str source) " to " (str destination) ", but we were unable to find one given the current search limits.")))
	      (t
	       (miz-item-html ("there is no path")
		   nil
		 (:p "There is no path from " (str source) " to " (str destination) ".."))))))))


(defun emit-article-page ()
  (register-groups-bind (article)
      (+article-uri-regexp+ (request-uri*))
    (if (member article *mml-lar* :test #'string=)
	(if (member article *handled-articles* :test #'string=)
	    (let* ((num-items (gethash article *article-num-items*))
		   (source-uri (format nil "/~a.miz" article))
		   (mizar-uri (format nil "http://mizar.org/version/current/html/~a.html" article))
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
			 
			 ((:a :href item-uri :title item-link-title)
			  (str item-html)))))))))
	    (miz-item-html ("article cannot be displayed")
		(:return-code +http-not-found+)
	      (:p ((:span :class "article-name") (str article)) " is a valid article in the MML, but unfortunately it has not yet been processed by this site.  Please try again later.")))
	(miz-item-html ("article not found")
	    (:return-code +http-not-found+)
	  (:p ((:span :class "article-name") (str article)) " is not known.  Here is a list of all known articles:")
	  (:p "The result of testing presence in the MML: " (let ((present (member article *mml-lar* :test #'string=)))
							      (htm
							       (str present))))
	  ((:table :class "article-listing" :rules "rows")
	   (:thead
	    (:tr
	     (:th "MML Name")
	     (:th "Title")))
	   (:tbody
	    (loop
	       for (article-name title author) in *articles*
	       for article-uri = (format nil "/article/~a" article-name)
	       for title-escaped = (escape-string title)
	       do
		 (htm
		  (:tr
		   ((:td :class "article-name")
		    ((:a :href article-uri :title title-escaped)
		     (str article-name)))
		   ((:td :class "article-title") (str title)))))))))))

(defun emit-random-item ()
  (let ((random-vertex (random-elt (hash-table-keys *all-items*))))
    (destructuring-bind (article kind number)
	(split ":" random-vertex)
      (let ((client-server-protocol (server-protocol*)))	
	    (redirect (uri-for-item article kind number)
		      :code (if (string= client-server-protocol "HTTP/1.1")
				+http-temporary-redirect+
				+http-moved-temporarily+))))))

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

(defun html-path-for-item (item-string)
  (destructuring-bind (article-name item-kind item-number)
      (split-item-identifier item-string)
    (let* ((item-key (format nil "~a:~a:~a" article-name item-kind item-number))
	   (ckb-for-item (gethash item-key *item-to-ckb-table*))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir)))
      (when ckb-for-item
	(destructuring-bind (ckb-article-name ckb-number)
	    (split-item-identifier ckb-for-item)
	  (declare (ignore ckb-article-name)) ;; same as ARTICLE
	  (format nil "~a/ckb~d.html" article-text-dir ckb-number))))))

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

(defun emit-mizar-item-page ()
  (register-groups-bind (article-name item-kind item-number-str)
      (+item-uri-regexp+ (request-uri*))
    (if (member article-name *handled-articles* :test #'string=)
	(let* ((item-number (parse-integer item-number-str))
	       (item-kind-pretty (pretty-item-kind item-kind))
	       (article-uri (uri-for-article article-name))
	       (num-items-for-article (gethash article-name *article-num-items*))
	       (item-key (format nil "~a:~a:~d" article-name item-kind item-number))
	       (ckb-for-item (gethash item-key *item-to-ckb-table*))
	       (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	       (article-text-dir (format nil "~a/text" article-dir)))
	  (destructuring-bind (ckb-article-name ckb-number-str)
	      (split ":" ckb-for-item)
	    (declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	    (let* ((article-name-uc (format nil "~:@(~a~)" article-name))
		   (fragment-path (format nil "~a/ckb~d.html"
					  article-text-dir
					  ckb-number-str))
		   (item-html (file-as-string fragment-path))
		   (ckb-number (parse-integer ckb-number-str))
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
		     ((:td :colspan "2" :class "item-info-heading") "Info"))
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
		      (str ckb-number-str)
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
		   (str item-html))))))))
	(miz-item-html ("unhandled article")
	    nil
	  (:p ((:span :class "article-name") (str article-name)) " is not known, or not yet suitably processed for this site.  Please try again later.")))))

(defun emit-fragment-page ()
  (register-groups-bind (article-name item-number)
      (+fragment-uri-regexp+ (request-uri*))
    (let* ((item-key (format nil "~a:~a" article-name item-number))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'html-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir))
	   (items-for-ckb (gethash item-key *ckb-to-items-table*)))
      (destructuring-bind (ckb-article-name ckb-number)
	  (split ":" item-key)
	(declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	(let* ((fragment-path (format nil "~a/ckb~a.html"
				      article-text-dir
				      ckb-number))
	       (item-html (file-as-string fragment-path)))
	  (miz-item-html (item-key)
	      nil
	    (let ((fragment-uri (format nil "/article/~a/#fragment~d" article-name ckb-number))
		  (article-uri (format nil "/article/~a" article-name)))
	      (htm
	       (:p (str item-key) " is " ((:a :href fragment-uri) "fragment #" (str ckb-number)) " of article " ((:a :href article-uri :class "article-name") (str article-name)) ".")
	       (if (null (cdr items-for-ckb))
		   (let ((item (car items-for-ckb)))
		     (destructuring-bind (item-article item-kind item-number)
			 (split ":" item)
		       (let ((item-uri (format nil "/item/~a/~a/~a" item-article item-kind item-number)))
			 (htm 
			  (:p "This fragment generates only one item: " ((:a :href item-uri) (str item)) ".")))))
		   (htm
		    (:p "This fragment generates multiple items:")
		    ((:ul :class "dep-list")
		     (dolist (other-item items-for-ckb)
		       (destructuring-bind (other-item-article other-item-kind other-item-number)
			   (split ":" other-item)
			 (let ((other-item-uri (format nil "/item/~a/~a/~a" other-item-article other-item-kind other-item-number)))
			   (htm
			    (:li ((:a :href other-item-uri) (str other-item))))))))))
	       (str item-html)))))))))

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
	 for article-uri = (format nil "/article/~a" article-name)
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

(defun item-from-components (article-name item-kind item-number)
  (format nil "~a:~a:~a" article-name item-kind item-number))

(defun item-link-title (article kind number)
  (format nil "~:@(~a~):~a:~a" article kind number))

(defun item-inline-name (article kind number)
  (with-html-output-to-string (foo)
    (:span
     ((:span :class "article-name") (str article))
     ":"
     (str kind)
     ":"
     (str number))))

(defun item-requires-tsorted (item)
  (flet ((item-< (item-1 item-2)
	   (< (position item-1 *items-tsorted*)
	      (position item-2 *items-tsorted*))))
    (let* ((requires (gethash item *item-dependency-graph-forward*))
	   (sorted (sort (copy-list requires) #'item-<)))
      sorted)))

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

(defun emit-requires-page ()
  (register-groups-bind (article kind number)
      (+requires-uri-regexp+ (request-uri*))
    (let* ((item (item-from-components article kind number))
	   (item-uri (item-uri item))
	   (item-link-title (item-link-title article kind number))
	   (item-inline-name (item-inline-name article kind number))
	   (deps (item-requires-tsorted item)))
      (let* ((requires-page-title (format nil "requirements of ~a" item-link-title))
	     (html-path (html-path-for-item item))
	     (html (file-as-string html-path)))
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

(defun emit-supports-page ()
  (register-groups-bind (article kind number)
      (+supports-uri-regexp+ (request-uri*))
    (let* ((item (item-from-components article kind number))
	   (item-uri (item-uri item))
	   (item-link-title (item-link-title article kind number))
	   (item-inline-name (item-inline-name article kind number))
	   (deps (item-supports-tsorted item)))
      (let* ((supports-page-title (format nil "what ~a supports" item-link-title))
	     (html-path (html-path-for-item item))
	     (html (file-as-string html-path)))
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
	(let* ((supp-html-path (html-path-for-item supporting-item))
	       (dep-html-path (html-path-for-item dependent-item))
	       (supp-html (file-as-string supp-html-path))
	       (dep-html (file-as-string dep-html-path))
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
  (register-regexp-dispatcher +article-uri-regexp+ #'emit-article-page)
  (register-regexp-dispatcher +fragment-uri-regexp+ #'emit-fragment-page)
  (register-regexp-dispatcher +item-uri-regexp+ #'emit-mizar-item-page)
  (register-regexp-dispatcher +path-between-items-uri-regexp+
			      #'emit-path-between-items)
  ;; requires and supports
  (register-regexp-dispatcher +requires-uri-regexp+ #'emit-requires-page)
  (register-regexp-dispatcher +supports-uri-regexp+ #'emit-supports-page)
  ;; dependence
  (register-regexp-dispatcher +dependence-uri-regexp+ #'emit-dependence-page)
  ;; proofs
  (loop
     for article in *handled-articles*
     do
       (register-proofs-for-article article))
  t)
