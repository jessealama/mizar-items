
(in-package :mizar)

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

(defun uri-for-item (article kind number)
  (format nil "/item/~a/~a/~a" article kind number))

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
    (exact-regexp (concat "/" "path"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/?" ; maybe end with a '/'
			  ))
  :test #'string=)

(define-constant +path-between-items-via-item-uri-regexp+
    (exact-regexp (concat "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"
			  "/?" ; maybe end with a '/'
			  ))
  :test #'string=)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *handled-articles* nil
  "Articles that we can handle (i.e., articles for which we have
accurate dependency information and which are stored properly.")

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
  (miz-item-html "fine-grained dependencies in the mizar mathematical library"
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

(defun link-for-two-items (item-1 item-2)
  (destructuring-bind (article-1 kind-1 num-1)
      (split ":" item-1)
    (destructuring-bind (article-2 kind-2 num-2)
	(split ":" item-2)
      (format nil "/path/~a/~a/~a/~a/~a/~a" 
	          article-1 kind-1 num-1
		  article-2 kind-2 num-2))))

(defun emit-path-between-items ()
  (register-groups-bind (article-1 kind-1 num-1 article-2 kind-2 num-2)
      (+path-between-items-uri-regexp+ (request-uri*))
    ;; check that these items exist
    (let ((source-item (format nil "~a:~a:~a" article-1 kind-1 num-1))
	  (destination-item (format nil "~a:~a:~a" article-2 kind-2 num-2)))
      (if (gethash source-item *all-items*)
	  (if (gethash destination-item *all-items*)
	      (let ((problem (make-item-search-problem 
			       :initial-state source-item
			       :goal destination-item))
		    (source-item-uri (link-for-item source-item))
		    (dest-item-uri (link-for-item destination-item))
		    (opposite-path-uri (link-for-two-items destination-item
							   source-item)))
		(multiple-value-bind (solution-exists? solution)
		    (bounded-depth-first-search problem +search-depth+)
		  (miz-item-html (fmt "from ~a to ~a" source-item destination-item)
		  (if solution-exists?
		      (htm
		       (:p (fmt "Here is a path from ~a to ~a" source-item destination-item))
		       (:ol
			(let ((source-uri (format nil "/~a/~a/~a" article-1 kind-1 num-1)))
			  (htm
			   (:li ((:a :href source-uri)
				 (str source-item)))))
			(dolist (step (explain-solution solution))
			  (destructuring-bind (step-article step-kind step-num)
			      (split ":" step)
			    (let ((step-uri (format nil "/~a/~a/~a" step-article step-kind step-num)))
			      (htm
			       (:li ((:a :href step-uri)
				     (str step)))))))))
		      (if (null solution)
			  (htm
			   (:p "There is no path from " ((:a :href source-item-uri) (str source-item)) " to " ((:a :href dest-item-uri) (str destination-item)) ".  Care to " 
			       ((:a :href opposite-path-uri)
				"search for a path going the other way")
			       "?"))
			  (htm
			   (:p "There may be a path from "  ((:a :href source-item-uri) (str source-item)) "  to "  ((:a :href dest-item-uri) (str destination-item)) ", but I'm afraid we were unable to find one given the current depth limit in effect on searches.")))))))
	      (miz-item-html "Invalid URI"
		(:p "The requested destination item, '" (str destination-item) "', is not the name of any known item.")))
	  (miz-item-html "Invalid URI"
	    (:p "The requested source item, '" (str source-item) "', is not the name of any known item."))))))

(defun emit-path-between-items-via-item ()
  (register-groups-bind (source-article source-kind source-num 
					via-article via-kind via-num
					destination-article destination-kind destination-num)
      (+path-between-items-via-item-uri-regexp+ (request-uri*))
    (let ((source (format nil "~a:~a:~a" source-article source-kind source-num))
	  (via (format nil "~a:~a:~a" via-article via-kind via-num))
	  (destination (format nil "~a:~a:~a" destination-article destination-kind destination-num)))	
      (if (gethash source *all-items*)
	  (if (gethash via *all-items*)
	      (if (gethash destination *all-items*)
		  (let ((get-params (get-parameters*)))
		    (if (null get-params) ; first time here, eh?
			(miz-item-html (fmt "from ~a to ~a via ~a" source destination via)
			  (:dl
			   (:dt "Source")
			   (:dd (str source))
			   (:dt "Destination")
			   (:dd (str destination))
			   (:dt "Via")
			   (:dd (str via)))
			  (:p "What kind of search would you like to do?")
			  (:ul
			   (:li "Find one path from source to destination;)")
			   (:li "Find " (:em "all") " paths;")
			   (:li "Find a path from the source to the destination that " (:em "avoids") " the the intermediate verte;x")
			   (:li "Find " (:em "all") " paths from the source to the destination that avood the intermediate vertex.")))
			(miz-item-html "You're asking too much"
			  (:p "I can't handle this: " (fmt "~A" get-params)))))
		  (miz-item-html "Invalid URI"
		    (:p "There given destination item, '" (str destination) "', is not the name of any known item.")))
	      (miz-item-html "Invalid URI"
		(:p "There given intermediate item, '" (str via) "', is not the name of any known item.")))
	  (miz-item-html "Invalid URI"
	    (:p "There given source item, '" (str source) "', is not the name of any known item."))))))

(defun emit-article-page ()
  (register-groups-bind (article)
      (+article-uri-regexp+ (request-uri*))
    (if (member article *mml-lar* :test #'string=)
	(if (member article *handled-articles* :test #'string=)
	    (let ((num-items (gethash article *article-num-items*)))
	      (miz-item-html (fmt "~a" article)
		(:p "The article " (str article) " has " (:b (str num-items)) " items ")
		(:p "See " (:a :href (format nil "http://mizar.org/version/current/html/~a.html" article) "an HTMLized presentation of the whole article") ", or " (:a :href (format nil "/~a.miz" article) "its raw source") ".")
		(htm
		 ((:ol :class "fragment-listing")
		  (loop
		     with article-dir = (format nil "~a/~a" (mizar-items-config 'itemization-source) article)
		     with article-text-dir = (format nil "~a/text" article-dir)
		     for i from 1 upto num-items
		     for i-str = (format nil "~d" i)
		     for fragment-path = (format nil "~a/ckb~d.html" article-text-dir i)
		     for item-html = (file-as-string fragment-path)
		     for item-uri = (format nil "/fragment/~a/~d" article i)
		     do
		       (htm
			((:li :class "fragment-listing" :id i-str)
			 ((:a :href item-uri :class "fragment-listing")
			  (str item-html)))))))))
	    (progn
	      ; (setf (return-code *reply*) +http-not-found+)
	      (miz-item-html "article cannot be displayed"
		(:p "The article '" (fmt "~a" article) "' is a valid article in the MML, but unfortunately it has not yet been processed by this site.  Please try again later."))))
	(progn
	  ; (setf (return-code *reply*) +http-not-found+)
	  (miz-item-html "article not found"
	    (:p "The article '" (fmt "~a" article) "' is not known.  Here is a list of all known articles:")
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
		     ((:td :class "article-title") (str title))))))))))))

(defun emit-random-item ()
  (let ((random-vertex (random-elt (hash-table-keys *all-items*))))
    (destructuring-bind (article kind number)
	(split ":" random-vertex)
      (redirect (uri-for-item article kind number)))))

(defun emit-random-path ()
  (let* ((keys (hash-table-keys *all-items*))
	 (random-vertex-1 (random-elt keys))
	 (random-vertex-2 (random-elt keys)))
    (redirect (link-for-two-items random-vertex-1 random-vertex-2))))

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
	   (article-dir (format nil "~a/~a" (mizar-items-config 'itemization-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir)))
      (when ckb-for-item
	(destructuring-bind (ckb-article-name ckb-number)
	    (split-item-identifier ckb-for-item)
	  (declare (ignore ckb-article-name)) ;; same as ARTICLE
	  (format nil "~a/ckb~d.html" article-text-dir ckb-number))))))

(defun emit-mizar-item-page ()
  (register-groups-bind (article-name item-kind item-number)
      (+item-uri-regexp+ (request-uri*))
    (if (member article-name *handled-articles* :test #'string=)
	(let* ((item-key (format nil "~a:~a:~a" article-name item-kind item-number))
	       (ckb-for-item (gethash item-key *item-to-ckb-table*))
	       (article-dir (format nil "~a/~a" (mizar-items-config 'itemization-source) article-name))
	       (article-text-dir (format nil "~a/text" article-dir))
	       (forward-deps (gethash item-key *item-dependency-graph-forward*))
	       (backward-deps (gethash item-key *item-dependency-graph-backward*))
	       (forward-deps-sorted (sort (copy-list forward-deps) 
					  #'item-<))
	       (backward-deps-sorted (sort (copy-list backward-deps)
					   #'item-<)))
	  (destructuring-bind (ckb-article-name ckb-number)
	      (split ":" ckb-for-item)
	    (declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	    (let* ((fragment-path (format nil "~a/ckb~d.html"
					  article-text-dir
					  ckb-number))
		   (item-html (file-as-string fragment-path)))
	      (miz-item-html (str item-key)
		((:table :width "100%")
		 ((:tr :valign "top")
		  (:td (str item-html)))
		 ((:tr :valign "middle")
		  ((:td :class "fullwidth" :align "center")
		   ((:table :rules "cols")
		    (:tr
		     ((:td :align "center" :class "arrow")
		      (str "&#8593;"))
		     ((:td :align "center" :class "arrow")
		      (str "&#8595;")))
		    ((:tr :valign "top")
		     ((:td :class "halfwidth" :align "center")
		      (if forward-deps-sorted
			  (htm
			   (:table
			    (:caption "Depends On")
			    (dolist (forward-dep forward-deps-sorted)
			      (let ((dep-uri (link-for-item forward-dep)))
				(htm
				 (:tr (:td ((:a :href dep-uri) (str forward-dep)))))))))
			  (htm (:p (:em "(This item immediately depends on nothing.)")))))
		     ((:td :class "halfwidth" :align "center")
		      (if backward-deps-sorted
			  (htm
			   (:table
			    (:caption "Supports")
			    (dolist (backward-dep backward-deps-sorted)
			      (let ((dep-uri (link-for-item backward-dep)))
				(htm
				 (:tr (:td ((:a :href dep-uri) (str backward-dep)))))))))
			  (htm (:p (:em "(No item immediately depends on this one.)"))))))))))))))
	(progn
	  ; (setf (return-code *reply*) +http-not-found+)
	  (miz-item-html "unhandled article"
	    (:p "The article '" (str article-name) "' is not known, or not yet suitably processed for this site.  Please try again later."))))))

(defun emit-fragment-page ()
  (register-groups-bind (article-name item-number)
      (+fragment-uri-regexp+ (request-uri*))
    (let* ((item-key (format nil "~a:~a" article-name item-number))
	   (article-dir (format nil "~a/~a" (mizar-items-config 'itemization-source) article-name))
	   (article-text-dir (format nil "~a/text" article-dir))
	   (items-for-ckb (gethash item-key *ckb-to-items-table*)))
      (destructuring-bind (ckb-article-name ckb-number)
	  (split ":" item-key)
	(declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	(let* ((fragment-path (format nil "~a/ckb~a.html"
				      article-text-dir
				      ckb-number))
	       (item-html (file-as-string fragment-path)))
	  (miz-item-html (str item-key)
	    (let ((fragment-uri (format nil "/article/~a/#~d" article-name ckb-number))
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
  (miz-item-html "articles from the mml"
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
  (miz-item-html (str "fine-grained dependencies in mizar")
    (:h1 "welcome")
    (:p "Interested in learning more about the " ((:a :href "http://www.mizar.org/") (:tt "MIZAR") " Mathematical Library") " (MML), the largest corpus of formalized mathematics in the world?  This site provides a way to
    get a handle on the large contents of the MML.")
    (:p "The " (:tt "MIZAR") " community has " ((:a :href "http://mizar.org/version/current/html/") "an attractive presentation of the contents of the MML") ".  (It is simply a directory listing at the moment, listing every article of the MML in alphabetical order.)  This site presents the MML by showing its " (:b "items") " and showing, for each item, what it " (:b "depends")  "upon and conversely (what items depend on the item).  This website presents " (:tt "MIZAR") " items, their dependency information, and provides a way of exploring these dependencies by finding " (:b "paths") " among dependencies.")
    (:p "The dependency graph that this site lets you explore has "  (:b (str (hash-table-count *all-items*))) " nodes (items) and " (:b (str (count-dependency-graph-edges))) " edges.")
    (:h1 "getting started")
    (:p "One can visit " ((:a :href "/articles") "the complete list of handled articles") ".  Alternatively, one can visit " ((:a :href "/random-item") " a random item") " or " ((:a :href "/random-path") "search for a path between two random items") ".")
    (:p "You might want to visit the " ((:a :href "/landmarks") "landmarks") " page to get acquainted with how this site provides fine-grained dependency information for some notable results of mathematics.") 
    (:h1 "learning more about" (:tt "MIZAR"))
    (:p "The " (:tt "MIZAR") " system and its library, the MML, are rather complex.  To learn more about the system, see the excellent overview article")
    (:blockquote
     (:p
      "&ldquo;"
      ((:a :href "http://jfr.cib.unibo.it/article/view/1980") "Mizar in a nutshell")
      "&rdquo; , by Adam Grabowski, Artur Kornilowicz, and Adam Naumowicz, " (:em "Journal of Formalized Reasoning") (:b "3") "(2), (2010), pp. 153&ndash;245"))
    (:p "For a historical overview, see:")
    (:blockquote
     (:p
      "&ldquo;"
      ((:a :href "http://markun.cs.shinshu-u.ac.jp/mizar/mma.dir/2005/mma2005(2).pdf") "MIZAR: The first 30 years")
      "&rdquo; , by Roman Mutuszewski and Piotr Rudnicki, " (:em "Mechanized Mathematics and its Applications") (:b "4") "(1), (2005), pp. 3&ndash;24"))
    (:p "At the moment, this site is not really interactive: you can't work with " (:tt "MIZAR") " texts here.  If you'd like to get your hands dirty, you might want to visit " ((:a :href "http://mws.cs.ru.nl/mwiki/") "the " (:tt "MIZAR") " wiki") " project at Radboud University Nijmegen.")))

(defun item-uri (item-identifier)
  (format nil "/item/~a" (substitute #\/ #\: item-identifier)))
     
(defun emit-landmarks-page ()
  (miz-item-html "landmarks"
    (:p "One might also be interested in entering the vast space of " (:tt "MIZAR") " items by inspecting some landmarks.")
    (:p "This page is divided into the following sections:")
    (:ul
     (:li ((:a :href "#selected-list") "The site designer's biased list of notable theorems"))
     (:li ((:a :href "#100theorems") "100 Theorems")))
    ((:h1 :id "selected-list") "A selected list of landmarks")
    (:p "The following is a selected list of some notable mathematical results that can be found in the " (:tt "MIZAR") " Mathematical Library.  What does 'notable' mean?  Obviously, it's a value term.  The following list has a bias towards results of metamathematical or foundational significance, which is what the site designer is especially interested in.  If you're not especially interested in mathematical logic, see the other section, " ((:a :href "#100theorems" :title "100 Theorems Formalized in Mizar") "100 Theorems Formalized in Mizar") ", for more mathematical contentful examples.")
    (:ul
     (:li ((:a :href "/item/tarski/theorem/9" :title "Tarski universe axiom") "Tarski universe axiom"))
     (:li "Axiom of infinity")
     (:li "Power set")
     (:li "Axiom of choice")
     (:li "Zorn's lemma")
     (:li "Zermelo's well-ordering theorem")
     (:li "All vector spaces have a basis"))
    ((:h1 :id "100theorems") "100 Theorems")
    (:p "The following is a list of 'notable' mathematical results formalized in " (:tt "MIZAR") ".  What does 'notable' mean here?  Certainly, it's a value term.  This list comes from Freek Wiedijk's "((:a :href "http://www.cs.ru.nl/~freek/100/" :title "Formalizing 100 Theorems") "100 theorems") " and its associated list of " ((:a :href "http://www.cs.ru.nl/~freek/100/mizar.html" :title "Formalizing 100 Theorems in Mizar") "theorems formalized in " (:tt "MIZAR")) ".  Here is the list, with links to the corresponding entries in this site's database.")
    (:dl
     (loop
	for i from 1 upto 100
	for theorem-name across +100-theorems+
	for theorem-name-escaped = (escape-string theorem-name)
	do
	  (htm
	   (:dt (fmt "~d. ~a" i theorem-name))
	   (:dd
	    (:p
	     (let ((formalizations (gethash i +mizar-formalized+)))
	       (if formalizations
		   (if (cdr formalizations) ; more than one formalization
		       (htm
			(:ul
			 (dolist (formalization formalizations)
			   (let* ((formalization-uri (item-uri formalization))
				  (formalization-html-path (html-path-for-item formalization))
				  (formalization-html (if formalization-html-path
							  (if (file-exists-p formalization-html-path)
							      (file-as-string formalization-html-path)
							      "(HTML representation not present)")
							  "(HTML representation not present)")))
			     (htm
			      (:li ((:a :href formalization-uri
					:title theorem-name-escaped)
				    (str formalization-html))))))))
		       (let* ((formalization (car formalizations))
			      (formalization-uri (item-uri formalization))
			      (formalization-html-path (html-path-for-item formalization))
			      (formalization-html (if formalization-html-path
						      (if (file-exists-p formalization-html-path)
							  (file-as-string formalization-html-path)
							  "(HTML representation not present)")
						      "(HTML representation not present)")))
			 (htm
			  ((:a :href formalization-uri
			       :title theorem-name-escaped)
			   (str formalization-html)))))
		   (htm
		    (:em "(not yet formalized in " (:tt "MIZAR") ")")))))
	    (let ((100theorems-uri (format nil "http://www.cs.ru.nl/~~freek/100/#~d" i))
		  (100theorems-title (format nil "Known formalizations of: ~a" theorem-name-escaped)))
	      (htm
	       (:p "[" ((:a :href 100theorems-uri
			    :title 100theorems-title)
			"other formalizations") "]")))))))))

(defun emit-feedback-page ()
  (miz-item-html "feedback"
    (:p
     "Thanks for using this site.  The maintainer is " ((:a :href "http://centria.di.fct.unl.pt/~alama/") "Jesse Alama") ".  If your have questions, comments, bug reports (e.g., broken links), or feature requests, please do " ((:a :href "mailto:jesse.alama@gmail.com") "send an email") "; your feedback is appreciated.")))

(defun initialize-uris (&optional (articles :all))
  ;; ecmascript, css
  (register-static-file-dispatcher "/mhtml.css"
				   (mizar-items-config 'mhtml-css-path)
				   "text/css")
  (register-static-file-dispatcher "/screen.css"
				   (mizar-items-config 'screen-css-path)
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
  (register-exact-uri-dispatcher "/random-path" #'emit-random-path)
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
	 (let* ((article-dir (format nil "~a/~a" (mizar-items-config 'itemization-source) article))
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
  (register-regexp-dispatcher +path-between-items-via-item-uri-regexp+
			      #'emit-path-between-items-via-item)
  t)
