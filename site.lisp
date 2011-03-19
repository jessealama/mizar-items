
(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server and application setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +search-depth+ 15
  :test #'=
  :documentation "The depth limit for doing searches.")

(defmacro with-mizar-favicon-and-title (title &body body)
  `(with-favicon-and-title "/favicon.ico" ,title ,@body))

(defmacro miz-item-html (title &body body)
  `(with-html
     (:head 
      ((:link :rel "icon" :href "/favicon.ico" :type "image/x-icon"))
      ((:link :href "/mhtml.css" :rel "stylesheet" :type "text/css"))
      ((:link :href "/screen.css" :rel "stylesheet" :type "text/css"))
      ((:script :src "/mhtml.js" :type "text/ecmascript"))
      (:title ,title))
     (:body
      ((:table :border "1"
	       :summary "navigation"
	       :class "header"
	       :width "100%")
       (:tr
	(:td
	 ((:span :class "menu")
	  ((:a :href "/") "main")
	  " "
	  ((:a :href "/about") "about")
	  " "
	  ((:a :href "/random-item") "random-item")
	  " "
	  ((:a :href "/random-path") "random-path")))))
      ,@body
      (:hr)
      ((:div :class "footer")
       ((:span :class "fleft") "See the " ((:a :href "/feedback") "feedback page") " for information about contacting us.")
       ((:span :class "menu")
	"Validate: " ((:a :href "http://jigsaw.w3.org/css-validator/check/referer") "CSS") ((:a :href "http://validator.w3.org/check/referer") "XHTML"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *itemization-source*
  "/Users/alama/sources/mizar/mizar-items/itemization")

(defparameter *articles*
  '(
    ("hidden" . "Built-in Concepts")
    ("tarski" . "Tarski-Grothendieck Set Theory")
    ("xboole_0" . "Boolean Properties of Sets - Definitions")
    ("boole" . "Boolean Properties of Sets - Requirements")
    ("xboole_1" . "Boolean Properties of Sets - Theorems")
    ("enumset1" . "Enumerated Sets")
    ("zfmisc_1" . "Some Basic Properties of Sets")
    ("subset_1" . "Properties of Subsets")
    ("subset" . "Basic Properties of Subsets - Requirements")
    ("setfam_1" . "Families of Sets")
    ("relat_1" . "Relations and Their Basic Properties")
    ("funct_1" . "Functions and Their Basic Properties")
    ("grfunc_1" . "Graphs of Functions")
    ("relat_2" . "Properties of Binary Relations")
    ("ordinal1" . "The Ordinal Numbers. Transfinite Induction and Defining by Transfinite Induction")
    ("wellord1" . "The Well Ordering Relations")
    ("relset_1" . "Relations Defined on Sets")
    ("partfun1" . "Partial Functions")
    ("mcart_1" . "Tuples, Projections and Cartesian Products")
    ("wellord2" . "Zermelo Theorem and Axiom of Choice. The correspondence of well ordering relations and ordinal numbers")
    ("funct_2" . "Functions from a Set to a Set")
    ("binop_1" . "Binary Operations")
    ("domain_1" . "Domains and Their Cartesian Products")
    ("funct_3" . "Basic Functions and Operations on Functions")
    ("funcop_1" . "Binary Operations Applied to Functions")
    ("funct_4" . "The Modification of a Function by a Function and the Iteration of the Composition of a Function")
    ("numerals" . "Numerals - Requirements")
    ("ordinal2" . "Sequences of Ordinal Numbers. Beginnings of Ordinal Arithmetics")
    ("ordinal3" . "Ordinal Arithmetics")
    ("wellset1" . "Zermelo's Theorem")
    ("multop_1" . "Three-Argument Operations and Four-Argument Operations")
    ("mcart_2" . "N-Tuples and Cartesian Products for n=5")
    ("schems_1" . "Schemes")
    ("sysrel" . "Some Properties of Binary Relations")
    ("gate_1" . "Logic Gates and Logical Equivalence of Adders")
    ("gate_2" . "Correctness of Binary Counter Circuits")
    ("gate_3" . "Correctness of Johnson Counter Circuits")
    ("gate_4" . "Correctness of a Cyclic Redundancy Check Code Generator")
    ("gate_5" . "Correctness of the High Speed Array Multiplier Circuits")
    ("finset_1" . "Finite Sets")
    ("finsub_1" . "Boolean Domains")
    ("orders_1" . "Partially Ordered Sets")
    ("setwiseo" . "Semilattice Operations on Finite Subsets")
    ("fraenkel" . "Function Domains and Fr&aelig;nkel Operator")
    ("card_1" . "Cardinal Numbers")
    ("funct_5" . "Curried and Uncurried Functions")
    ("partfun2" . "Partial Functions from a Domain to a Domain")
    ("classes1" . "Tarski's Classes and Ranks")
    ("arytm_3" . "Arithmetic of Non Negative Rational Numbers")
    ("arytm_2" . "Non negative real numbers. Part I")
    ("arytm_1" . "Non negative real numbers. Part II")
    ("numbers" . "Subsets of Complex Numbers")
    ("arytm_0" . "Introduction to Arithmetics")
    ("xcmplx_0" . "Complex Numbers - Basic Definitions")
    ("arithm" . "Field Properties of Complex Numbers - Requirements")
    ("xxreal_0" . "Introduction to Arithmetic of Extended Real Numbers")
    ("xreal_0" . "Introduction to Arithmetic of Real Numbers")
    ("real" . "Basic Properties of Real Numbers - Requirements")
    ("xcmplx_1" . "Complex Numbers - Basic Theorems")
    ("xreal_1" . "Real Numbers - Basic Theorems")
    ("axioms" . "Strong arithmetic of real numbers")
    ("real_1" . "Basic Properties of Real Numbers")
    ("square_1" . "Some Properties of Real Numbers. Operations: min, max, square, and square root")
    ("nat_1" . "The Fundamental Properties of Natural Numbers")
    ("int_1" . "Integers")
    ("rat_1" . "Basic Properties of Rational Numbers")
    ("membered" . "On the Sets Inhabited by Numbers")
    ("valued_0" . "Number-Valued Functions")
    ("complex1" . "The Complex Numbers")
    ("absvalue" . "Some Properties of Functions Modul and Signum")
    ("int_2" . "The Divisibility of Integers and Integer Relatively Primes")
    ("nat_d" . "Divisibility of Natural Numbers")
    ("binop_2" . "Binary Operations on Numbers")
    ("xxreal_1" . "Basic Properties of Extended Real Numbers")
    ("card_2" . "Cardinal Arithmetics")
    ("xxreal_2" . "Suprema and Infima of Intervals of Extended Real Numbers")
    ("xxreal_3" . "Basic Operations on Extended Real Numbers")
    ("member_1" . "Collective Operations on Number-Membered Sets")
    ("supinf_1" . "Infimum and Supremum of the Set of Real Numbers. Measure Theory")
    ("quin_1" . "Quadratic Inequalities")
    ("card_3" . "K&ouml;nig's Theorem")
    ("realset1" . "Group and Field Definitions")
    ("classes2" . "Universal Classes")
    ("ordinal4" . "Increasing and Continuous Ordinal Sequences")
    ("finseq_1" . "Segments of Natural Numbers and Finite Sequences")
    ("recdef_1" . "Recursive Definitions")
    ("valued_1" . "Properties of Number-Valued Functions")
    ("finseq_2" . "Finite Sequences and Tuples of Elements of a Non-empty Sets")
    ("seq_1" . "Real Sequences and Basic Operations on Them")
    ("xboolean" . "On the Arithmetic of Boolean Values")
    ("eqrel_1" . "Equivalence Relations and Classes of Abstraction")
    ("seq_2" . "Convergent Sequences and the Limit of Sequences")
    ("finseqop" . "Binary Operations Applied to Finite Sequences")
    ("finseq_3" . "Non-contiguous Substrings and One-to-one Finite Sequences")
    ("margrel1" . "Many-Argument Relations")
    ("toler_1" . "Relations of Tolerance")
    ("trees_1" . "Introduction to Trees")
    ("finseq_4" . "Pigeon Hole Principle")
    ("finsop_1" . "Binary Operations on Finite Sequences")
    ("setwop_2" . "Semigroup operations on finite subsets")
    ("rfunct_1" . "Partial Functions from a Domain to the Set of Real Numbers")
    ("rvsum_1" . "The Sum and Product of Finite Sequences of Real Numbers")
    ("pboole" . "Manysorted Sets")
    ("newton" . "Factorial and Newton coefficients")
    ("card_4" . "Countable Sets and Hessenberg's Theorem")
    ("card_5" . "On Powers of Cardinals")
    ("trees_2" . "K&ouml;nig's Lemma")
    ("valued_2" . "Arithmetic Operations on Functions from Sets into Functional Sets")
    ("seqm_3" . "Monotone Real Sequences. Subsequences")
    ("rfinseq" . "Functions and Finite Sequences of Real Numbers")
    ("seq_4" . "Convergent Real Sequences. Upper and Lower Bound of Sets of Real Numbers")
    ("rcomp_1" . "Topological Properties of Subsets in Real Numbers")
    ("comseq_1" . "Complex Sequences")
    ("comseq_2" . "Conjugate Sequences, Bounded Complex Sequences and Convergent Complex Sequences")
    ("rfunct_2" . "Properties of Real Functions")
    ("cfunct_1" . "Property of Complex Functions")
    ("fcont_1" . "Real Function Continuity")
    ("fcont_2" . "Real Function Uniform Continuity")
    ("fdiff_1" . "Real Function Differentiability")
    ("rolle" . "Average Value Theorems for Real Functions of One Variable")
    ("prepower" . "Integer and Rational Exponents")
    ("finseq_5" . "Some Properties of Restrictions of Finite Sequences")
    ("rewrite1" . "Reduction Relations")
    ("funct_7" . "Miscellaneous Facts about Functions")
    ("scheme1" . "Schemes of Existence of some Types of Functions")
    ("abian" . "Abian's Fixed Point Theorem")
    ("power" . "Real Exponents and Logarithms")
    ("polyeq_1" . "Solving Roots of Polynomial Equations of Degree 2 and 3 with Real Coefficients")
    ("series_1" . "Series")
    ("comseq_3" . "Convergence and the Limit of Complex Sequences. Series")
    ("cfcont_1" . "Property of Complex Sequence and Continuity of Complex Function")
    ("cfdiff_1" . "Complex Function Differentiability")
    ("rpr_1" . "Introduction to Probability")
    ("funct_6" . "Cartesian Product of Functions")
    ("supinf_2" . "Series of Positive Real Numbers. Measure Theory")
    ("trees_a" . "Replacement of Subtrees in a Tree")
    ("pre_ff" . "Two Programs for <b>SCM</b>. Part I - Preliminaries")
    ("trees_3" . "Sets and Functions of Trees and Joining Operations of Trees")
    ("partit1" . "A theory of partitions, I")
    ("trees_4" . "Joining of Decorated Trees")
    ("card_fil" . "Basic facts about inaccessible and measurable cardinals")
    ("binarith" . "Binary Arithmetics. Addition")
    ("pre_circ" . "Preliminaries to Circuits, I")
    ("finseq_6" . "On the Decomposition of Finite Sequences")
    ("mboolean" . "Definitions and Basic Properties of Boolean &amp; Union of Many Sorted Sets")
    ("wsierp_1" . "The Chinese Remainder Theorem")
    ("glib_000" . "Alternative Graph Structures")
    ("pzfmisc1" . "Some Basic Properties of Many Sorted Sets")
    ("genealg1" . "Basic Properties of Genetic Algorithm")
    ("binari_2" . "Binary Arithmetics. Addition and Subtraction of Integers")
    ("trees_9" . "Subtrees")
    ("mssubfam" . "Certain Facts about Families of Subsets of Many Sorted Sets")
    ("relset_2" . "Properties of First and Second Order Cutting of Binary Relations")
    ("recdef_2" . "Recursive Definitions. Part II")
    ("prob_1" . "&sigma;-Fields and Probability")
    ("prob_2" . "Probability. Independence of Events and Conditional Probability")
    ("limfunc1" . "The Limit of a Real Function at Infinity. Halflines. Real Sequence Divergent to Infinity")
    ("limfunc2" . "One-Side Limits of a Real Function at a Point")
    ("seqfunc" . "Functional Sequence from a Domain to a Domain")
    ("limfunc3" . "The Limit of a Real Function at a Point")
    ("fcont_3" . "Monotonic and Continuous Real Function")
    ("limfunc4" . "The Limit of a Composition of Real Functions")
    ("l_hospit" . "The de l'Hospital Theorem")
    ("fdiff_2" . "Real Function Differentiability - Part II")
    ("fdiff_3" . "Real Function One-Side Differantiability")
    ("measure1" . "The &sigma;-additive Measure Theory")
    ("measure2" . "Several Properties of the &sigma;-additive Measure. Discrete categories")
    ("measure3" . "Completeness of the &sigma;-Additive Measure. Measure Theory")
    ("measure4" . "Properties of Caratheodor's Measure")
    ("rfunct_3" . "Properties of Partial Functions from a Domain to the Set of Real Numbers")
    ("measure5" . "Properties of the Intervals of Real Numbers")
    ("rearran1" . "Introduction to Theory of Rearrangment")
    ("measure6" . "Some Properties of the Intervals")
    ("extreal1" . "Basic Properties of Extended Real Numbers")
    ("measure7" . "The One-Dimensional Lebesgue Measure As an Example of a Formalization in the Mizar Language of the Classical Definition of a Mathematical Object")
    ("rfunct_4" . "Introduction to Several Concepts of Convexity and Semicontinuity for Function from REAL to REAL")
    ("mesfunc1" . "Definitions and Basic Properties of Measurable Functions")
    ("extreal2" . "Some Properties of Extended Real Numbers Operations: absolute value, min and max")
    ("sin_cos" . "Trigonometric Functions and Existence of Circle Ratio")
    ("mesfunc2" . "Measurability of Extended Real Valued Functions")
    ("sin_cos2" . "Properties of Trigonometric Function")
    ("sin_cos3" . "Trigonometric Functions on Complex Space")
    ("sin_cos4" . "Formulas And Identities of Trigonometric Functions")
    ("sin_cos5" . "Formulas And Identities of Trigonometric Functions")
    ("asympt_0" . "Asymptotic notation. Part I: Theory")
    ("comptrig" . "Trigonometric Form of Complex Numbers")
    ("asympt_0" . "Asymptotic notation. Part I: Theory")
    ("comptrig" . "Trigonometric Form of Complex Numbers")
    ("complex2" . "Inner Products and Angles of Complex Numbers")
    ("polyeq_2" . "Solving Roots of Polynomial Equation of Degree 4 with Real Coefficients")
    ("polyeq_3" . "Solving Complex Roots of Polynomial Equation of Degree 2 and 3 with Complex Coefficients")
    ("polyeq_4" . "Solving the Roots of the Special Polynomial Equation with Real Coefficients")
    ("polyeq_5" . "Solution of Cubic and Quartic Equations")
    ("sin_cos6" . "Inverse Trigonometric Functions Arcsin and Arccos")
    ("euler_1" . "Euler's Function")
    ("euler_2" . "Euler's Theorem and Small Fermat's Theorem")
    ("asympt_1" . "Asymptotic notation. Part II: Examples and Problems")
    ("series_3" . "On the Partial Product of Series and Related Basic Inequalities")
    ("series_4" . "Partial Sum and Partial Product of Some Series")
    ("series_5" . "On the Partial Product and Partial Sum of Series and Related Basic Inequalities")
    ("quaterni" . "The Quaternion Numbers")
    ("afinsq_1" . "Zero Based Finite Sequences")
    ("nat_2" . "Natural Numbers")
    ("pepin" . "Public-Key Cryptography and Pepin's Test for the Primality of Fermat Numbers")
    ("irrat_1" . "Irrationality of e")
    ("taylor_1" . "The Taylor Expansions")
    ("holder_1" . "H&ouml;lder's Inequality and Minkowski's Inequality")
    ("fdiff_4" . "Several Differentiable Formulas of Special Functions")
    ("fdiff_5" . "Some Differentiable Formulas of Special Functions")
    ("fdiff_6" . "Several Differentiable Formulas of Special Functions &ndash; Part II")
    ("fdiff_7" . "Several Differentiation Formulas of Special Functions &ndash; Part III")
    ("fdiff_8" . "Several Differentiation Formulas of Special Functions &ndash; Part IV")
    ("sin_cos7" . "Formulas And Identities of Inverse Hyperbolic Functions")
    ("sin_cos8" . "Formulas and Identities of Hyperbolic Functions")
    ("bvfunc_1" . "A Theory of Boolean Valued Functions and Partitions")
    ("bvfunc_2" . "A Theory of Boolean Valued Functions and Quantifiers with Respect to Partitions")
    ("taylor_2" . "The Maclaurin Expansions")
    ("catalan1" . "Catalan Numbers")
    ("pythtrip" . "Pythagorean triples")
    ("series_2" . "Partial Sum of Some Series")
    ("fib_num" . "Fibonacci Numbers")
    ("partit_2" . "Classes of Independent Partitions")
    ("bvfunc_3" . "Predicate Calculus for Boolean Valued Functions, I")
    ("bvfunc_4" . "Predicate Calculus for Boolean Valued Functions, II")
    ("bvfunc_5" . "Propositional Calculus for Boolean Valued Functions, I")
    ("bvfunc_6" . "Propositional Calculus for Boolean Valued Functions, II")
    ("bvfunc_7" . "Propositional Calculus For Boolean Valued Functions, III")
    ("bvfunc_8" . "Propositional Calculus For Boolean Valued Functions, IV")
    ("bvfunc_9" . "Propositional Calculus for Boolean Valued Functions, V")
    ("bvfunc10" . "Propositional Calculus for Boolean Valued Functions, VI")
    ("bvfunc11" . "Predicate Calculus for Boolean Valued Functions, III")
    ("bvfunc14" . "Predicate Calculus for Boolean Valued Functions, VI")
    ("bvfunc25" . "Propositional Calculus for Boolean Valued Functions, VII")
    ("bvfunc26" . "Propositional Calculus for Boolean Valued Functions, VIII")
    ("finseq_7" . "On Replace Function and Swap Function for Finite Sequences")
    ("prgcor_1" . "Correctness of Non Overwriting Programs. Part I")
    ("fdiff_9" . "Several Differentiation Formulas of Special Functions &ndash; Part V")
    ("arrow" . "Arrow's Impossibility Theorem")
    ("real_3" . "Simple Continued Fractions and Their Convergents")
    ("fdiff_10" . "Several Differentiation Formulas of Special Functions &ndash; Part V")
    ("hfdiff_1" . "Several Higher Differentiation Formulas of Special Functions")
    ("pre_poly" . "Preliminaries to Polynomials")
    ("prgcor_2" . "Logical Correctness of Vector Calculation Programs")
    ("sin_cos9" . "Inverse Trigonometric Functions Arctan and Arccot")
    ("sincos10" . "Inverse Trigonometric Functions Arcsec1, Arcsec2, Arccosec1 and Arccosec2")
    ("mesfunc3" . "Lebesgue Integral of Simple Valued Function")
    ("mesfunc4" . "Linearity of Lebesgue Integral of Simple Valued Function")
    ("rvsum_2" . "The Sum and Product of Finite Sequences of Complex Numbers")
    ("finseq_8" . "Concatenation of Finite Sequences Reducing Overlapping Part and an Argument of Separators of Sequential Files")
    ("integra1" . "The Definition of Riemann Definite Integral and some Related Lemmas")
    ("integra2" . "Scalar Multiple of Riemann Definite Integral")
    ("rfinseq2" . "Sorting Operators for Finite Sequences")
    ("integra3" . "Darboux's Theorem")
    ("integra4" . "Integrability of Bounded Total Functions")
    ("integra5" . "Definition of Integrability for Partial Functions from REAL to REAL and Integrability for Continuous Functions")
    ("integr12" . "Integrability Formulas &ndash; Part I")
    ("integra8" . "Several Integrability Formulas of Special Functions")
    ("card_lar" . "Mahlo and inaccessible cardinals")
    ("zf_lang" . "A Model of ZF Set Theory Language")
    ("zf_model" . "Models and Satisfiability. Defining by Structural Induction and Free Variables in ZF-formulae")
    ("zf_colla" . "The Contraction Lemma")
    ("zfmodel1" . "Properties of ZF Models")
    ("zf_lang1" . "Replacing of Variables in Formulas of ZF Theory")
    ("zf_refle" . "The Reflection Theorem")
    ("zfrefle1" . "Consequences of the Reflection Theorem")
    ("qc_lang1" . "A First Order Language")
    ("qc_lang2" . "Connectives and Subformulae of the First Order Language")
    ("qc_lang3" . "Variables in Formulae of the First Order Language")
    ("cqc_lang" . "A Classical First Order Language")
    ("cqc_the1" . "A First-Order Predicate Calculus. Axiomatics, the Consequence Operation and a Concept of Proof")
    ("valuat_1" . "Interpretation and Satisfiability in the First Order Logic")
    ("zfmodel2" . "Definable Functions")
    ("lukasi_1" . "Propositional Calculus")
    ("procal_1" . "Calculus of Propositions")
    ("zf_fund1" . "Mostowski's Fundamental Operations - Part I")
    ("intpro_1" . "Intuitionistic Propositional Calculus in the Extended Framework with Modal Operator, Part I")
    ("cqc_the2" . "Calculus of Quantifiers. Deduction Theorem")
    ("zf_fund2" . "Mostowski's Fundamental Operations - Part II")
    ("hilbert1" . "Hilbert Positive Propositional Calculus")
    ("cqc_sim1" . "Similarity of Formulae")
    ("modal_1" . "Introduction to Modal Propositional Logic")
    ("cqc_the3" . "Logical Equivalence of Formulae")
    ("qc_lang4" . "The Subformula Tree of a Formula of the First Order Language")
    ("substut1" . "Substitution in First-Order Formulas: Elementary Properties")
    ("sublemma" . "Coincidence Lemma and Substitution Lemma")
    ("substut2" . "Substitution in First-Order Formulas &ndash; Part II. The Construction of First-Order Formulas")
    ("calcul_1" . "A Sequent Calculus for First-Order Logic")
    ("calcul_2" . "Consequences of the Sequent Calculus")
    ("henmodel" . "Equivalences of Inconsistency and Henkin Models")
    ("goedelcp" . "G&ouml;del's Completeness Theorem")
    ("struct_0" . "Preliminaries to Structures")
    ("algstr_0" . "Basic Algebraic Structures")
    ("incsp_1" . "Axioms of Incidence")
    ("pre_topc" . "Topological Spaces and Continuous Functions")
    ("orders_2" . "Kuratowski - Zorn Lemma")
    ("graph_1" . "Graphs")
    ("cat_1" . "Introduction to Categories and Functors")
    ("petri" . "Basic Petri Net Concepts. Place/Transition Net Structure, Deadlocks, Traps, Dual Nets")
    ("net_1" . "Some Elementary Notions of the Theory of Petri Nets")
    ("lattices" . "Introduction to Lattice Theory")
    ("tops_1" . "Subsets of Topological Spaces")
    ("connsp_1" . "Connected Spaces")
    ("tops_2" . "Families of Subsets, Subspaces and Mappings in Topological Spaces")
    ("rlvect_1" . "Vectors in Real Linear Space")))

(defparameter *dependency-graph-file*
  (mizar-items-config 'fragment-depdenency-graph))
(defparameter *item-to-ckb-file*
  (mizar-items-config 'item-to-fragment-path))
(defparameter *full-item-dependency-graph*
  (mizar-items-config 'full-item-dependency-graph))

(defvar *dependency-graph* nil)
(defvar *num-dependency-graph-edges* nil)
(defvar *ckb-dependency-graph-forward* nil)
(defvar *ckb-dependency-graph-backward* nil)
(defvar *true-item-dependency-graph-forward* nil)
(defvar *true-item-dependency-graph-backward* nil)
(defvar *item-to-ckb-table* nil)
(defvar *ckb-to-items-table* nil)
(defvar *all-ckb-items* nil)
(defvar *all-true-items* nil)
(defvar *graphs-loaded* nil)

(defun write-full-item-dependency-graph ()
  (loop
     with true-item-forward-table = (make-hash-table :test #'equal)
     for item being the hash-keys of *item-to-ckb-table*
     for ckb = (gethash item *item-to-ckb-table*)
     for forward-ckb-deps = (gethash ckb *ckb-dependency-graph-forward*)
     for forward-item-deps = (reduce #'append (mapcar #'(lambda (ckb-dep)
							  (gethash ckb-dep *ckb-to-items-table*))
						      forward-ckb-deps))
     do
       (setf (gethash item true-item-forward-table)
	     forward-item-deps)
     finally
       (with-open-file (item-depgraph *full-item-dependency-graph*
				      :direction :output
				      :if-exists :error
				      :if-does-not-exist :create)
	 (loop
	    for item being the hash-keys of true-item-forward-table
	    for deps = (gethash item true-item-forward-table)
	    do
	      (dolist (dep deps)
		(format item-depgraph "~a ~a~%" item dep))))))


(defun load-dependency-graph ()
  ;; all possible items 
  (let ((all-ckb-items (make-hash-table :test #'equal))
	(all-true-items (make-hash-table :test #'equal)))
    ;; ckb graph
    (let ((lines (lines-of-file *dependency-graph-file*))
	  (edges nil)
	  (ckb-forward-table (make-hash-table :test #'equal))
	  (ckb-backward-table (make-hash-table :test #'equal)))
      (dolist (line lines)
	(destructuring-bind (lhs rhs)
	    (split " " line)
	  (push (cons lhs rhs) edges)
	  (setf (gethash lhs all-ckb-items) t
		(gethash rhs all-ckb-items) t)
	  (pushnew rhs (gethash lhs ckb-forward-table) :test #'string=)
	  (pushnew lhs (gethash rhs ckb-backward-table) :test #'string=)))
      (setf *dependency-graph* edges
	    *num-dependency-graph-edges* (length edges)
	    *ckb-dependency-graph-forward* ckb-forward-table
	    *ckb-dependency-graph-backward* ckb-backward-table))
    ;; items-to-ckbs
    (let ((lines (lines-of-file *item-to-ckb-file*))
	  (item-to-ckb-table (make-hash-table :test #'equal))
	  (ckb-to-items-table (make-hash-table :test #'equal)))
      (dolist (line lines)
	(destructuring-bind (item ckb)
	    (split " " line)
	  (setf (gethash item all-true-items) t)
	  (setf (gethash item item-to-ckb-table) ckb)
	  (pushnew item (gethash ckb ckb-to-items-table) :test #'string=)))
      (setf *item-to-ckb-table* item-to-ckb-table
	    *ckb-to-items-table* ckb-to-items-table))
    ;; if the full item dependency graph doesn't exist, make it
    (unless (file-exists-p *full-item-dependency-graph*)
      (write-full-item-dependency-graph))
    ;; now it exists; load it
    (let ((lines (lines-of-file *full-item-dependency-graph*))
	  (edges nil)
	  (forward-table (make-hash-table :test #'equal))
	  (backward-table (make-hash-table :test #'equal)))
      (dolist (line lines)
	(destructuring-bind (lhs rhs)
	    (split " " line)
	  (push (cons lhs rhs) edges)
	  (pushnew rhs (gethash lhs forward-table) :test #'string=)
	  (pushnew lhs (gethash rhs backward-table) :test #'string=)))
      (setf *true-item-dependency-graph-forward* forward-table
	    *true-item-dependency-graph-backward* backward-table))
    (setf *all-ckb-items* all-ckb-items
	  *all-true-items* all-true-items))
  (setf *graphs-loaded* t)
  t)

(defun count-miz-in-directory (dir)
  (let ((counter 0))
    (walk-directory dir #'(lambda (foo)
			    (declare (ignore foo))
			    (incf counter))
		    :test #'(lambda (path)
			      (scan "ckb[0-9]+\.miz$" (namestring path))))
    counter))

(defvar *article-num-items* nil)

(defun load-article-num-items (&optional force)
  (if (or force (null *article-num-items*))
    (loop
       with num-items-table = (make-hash-table :test #'equal)
       for (article-name . title) in *articles*
       do
	 (let ((article-dir (concat *itemization-source* "/" article-name "/" "text")))
	   (setf (gethash article-name num-items-table)
		 (count-miz-in-directory article-dir)))
       finally
	 (setf *article-num-items* num-items-table)
	 (return *article-num-items*))
    *article-num-items*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching for paths between items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (item-search-problem (:include problem)))

(defmethod successors ((isp item-search-problem) node)
  (mapcar #'(lambda (item)
	      (cons item item))
	  (gethash (node-state node) *true-item-dependency-graph-forward*)))

(defun all-paths (source destination)
  (if (string= source destination)
      (list (list source))
      (mapcar #'(lambda (path)
		  (cons source path))
	      (reduce #'append 
		      (mapcar #'(lambda (successor)
				  (all-paths successor destination))
			      (gethash source *true-item-dependency-graph-forward*))))))

(defun all-paths-from-via (source destination via)
  (let ((paths-from-source-to-via (all-paths source via)))
    (when paths-from-source-to-via
      (let ((paths-from-via-to-destination (all-paths via destination)))
	(when paths-from-via-to-destination
	  (map-product #'(lambda (path-1 path-2)
			   (append path-1 (cdr path-2)))
		       paths-from-source-to-via
		       paths-from-via-to-destination))))))

(defun one-path-from-via (source destination via)
  "Find one path from SOURCE to DESTINATION that passes through VIA.
If there is no such path, return nil."
  (let ((source-to-via-problem (make-item-search-problem 
				:initial-state source
				:goal via))
	(via-to-destination-problem (make-item-search-problem
				     :initial-state via
				     :goal destination)))
    (multiple-value-bind (solution-to-via-found? solution-to-via)
	(bounded-depth-first-search source-to-via-problem +search-depth+)
      (if solution-to-via-found?
	  (multiple-value-bind (solution-to-dest-found? solution-to-dest)
	      (bounded-depth-first-search via-to-destination-problem
					  +search-depth+)
	    (if solution-to-dest-found?
		(append (explain-solution solution-to-via)
			(cdr (explain-solution solution-to-dest)))
		(if (null solution-to-dest)
		    (values nil nil)
		    (values nil :cut-off))))
	  (if (null solution-to-via)
	      (values nil nil)
	      (values nil :cut-off))))))

(defun all-paths-pass-through (source destination via)
  "Determine whether all paths from SOURCE to DESTINATION pass through
VIA.  Return two values: if all paths from SOURCE to DESTINATION do
pass through VIA, return T and NIL; otherwise, return NIL and a path
from SOURCE to DESTINATION that does not pass through VIA.

Note that STRING= is used as the hard-coded test for vertex equality."
  (every-with-falsifying-witness (all-paths source destination)
				 #'(lambda (path)
				     (member via path :test #'string=))))

(defun all-paths-avoid (source destination bad-guy)
  "Detemine whether all paths from node SOURCE to node DESTINATION
avoid (that is, do not pass through) node BAD-GUY.  Returns two
values: if there is a path from SOURCE to DESTINATION that passes
through BAD-GUY, return NIL as the first value and that withnessing
path as the second value; otherwise, return T as the first value and
NIL as the second value."
  (let* ((to-bad-guy (make-item-search-problem :goal bad-guy
					       :initial-state source))
	 (to-bad-guy-solution (bounded-depth-first-search to-bad-guy
							  +search-depth+)))
    (if to-bad-guy-solution
	(let* ((from-bad-guy (make-item-search-problem :goal destination
						       :initial-state bad-guy))
	       (from-bad-guy-solution (bounded-depth-first-search from-bad-guy
								  +search-depth+)))
	  (if from-bad-guy-solution
	      (let ((path-to-bad-guy (explain-solution to-bad-guy-solution))
		    (path-from-bad-guy (explain-solution from-bad-guy-solution)))
		(values nil (append path-to-bad-guy (cdr path-from-bad-guy))))
	      (values t nil)))
	(values t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar items-dispatch-table nil)

(defun items-request-dispatcher (request)
  "Selects a request handler based on a list of individual request
dispatchers all of which can either return a handler or neglect by
returning NIL."
  (loop for dispatcher in items-dispatch-table
        for action = (funcall dispatcher request)
        when action return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

(defvar *acceptor* (make-instance 'hunchentoot:acceptor 
				  :port 4242
				  :request-dispatcher #'items-request-dispatcher))

(defun start-server ()
  (hunchentoot:start *acceptor*)
  t)

(defun setup-server (&optional reload-graphs)
  (format t "Loading article item counts...")
  (load-article-num-items reload-graphs)
  (format t "done.~%")
  (when (or reload-graphs (null *graphs-loaded*))
    (format t "Loading dependency graph data...")
    (load-dependency-graph)
    (format t "done.~%"))
  (format t "Initializing URIs...")
  (initialize-uris)
  (format t "done~%")
  (setf *message-log-pathname* "/tmp/hunchentoot-messages"
	*access-log-pathname* "/tmp/hunchentoot-access"
	*handle-http-errors-p* t
	*http-error-handler* #'handle-http-error
	*log-lisp-errors-p* t
	*log-lisp-warnings-p* t
	*log-lisp-backtraces-p* t
	*show-lisp-errors-p* t)
  t)

(defun handle-http-error (error-code)
  (when (= error-code +http-not-found+)
    (miz-item-html "No"
      (:p "I still haven't found what you're looking for."))))

(setq *http-error-handler* #'handle-http-error)

;; set up articles

(defun ckb-item-< (item-name-1 item-name-2)
  (destructuring-bind (item-article-name-1 item-num-as-str-1)
      (split ":" item-name-1)
    (destructuring-bind (item-article-name-2 item-num-as-str-2)
	(split ":" item-name-2)
      (or (string< item-article-name-1 item-article-name-2)
	  (let ((item-num-1 (parse-integer item-num-as-str-1))
		(item-num-2 (parse-integer item-num-as-str-2)))
	    (and (string= item-article-name-1 item-article-name-2)
		 (< item-num-1 item-num-2)))))))

(defun true-item-< (item-name-1 item-name-2)
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

(define-constant +true-item-uri-regexp+
    (exact-regexp (concat "/" "item"
			  "/" "(" +article-name-regexp+ ")"
			  "/" "(" +item-kind-regexp+ ")"
			  "/" "(" +number-regexp+ ")"))
  :test #'string=)

(defun uri-for-item (article kind number)
  (format nil "/item/~a/~a/~a" article kind number))

(define-constant +ckb-item-uri-regexp+
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

(define-constant +path-between-true-items-uri-regexp+
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

(define-constant +path-between-true-items-via-item-uri-regexp+
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
	  (:p "The file is a list of space-seaprated lines.  A line")
	  (:blockquote
	   (:tt "node-A node-B"))
	  (:p "is an edge in the graph and means: " (:em "node-A generates node-B") ". The syntax of nodes in this graph is ")
	  (:blockquote
	   (:tt (str "&lang;article-name&rang;:&lang;fragment-number&rang; &lang;article-name&rang;:&lang;item-kind&rang;:&lang;item-number&rang;")))))
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
      (+path-between-true-items-uri-regexp+ (request-uri*))
    ;; check that these items exist
    (let ((source-item (format nil "~a:~a:~a" article-1 kind-1 num-1))
	  (destination-item (format nil "~a:~a:~a" article-2 kind-2 num-2)))
      (if (gethash source-item *all-true-items*)
	  (if (gethash destination-item *all-true-items*)
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
      (+path-between-true-items-via-item-uri-regexp+ (request-uri*))
    (let ((source (format nil "~a:~a:~a" source-article source-kind source-num))
	  (via (format nil "~a:~a:~a" via-article via-kind via-num))
	  (destination (format nil "~a:~a:~a" destination-article destination-kind destination-num)))	
      (if (gethash source *all-true-items*)
	  (if (gethash via *all-true-items*)
	      (if (gethash destination *all-true-items*)
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
    (let ((num-items (gethash article *article-num-items*)))
      (miz-item-html (fmt "~a" article)
	(:p "The article " (str article) " has " (:b (str num-items)) " items ")
	(:p "See " (:a :href (format nil "http://mizar.org/version/current/html/~a.html" article) "an HTMLized presentation of the whole article") ", or " (:a :href (format nil "/~a.miz" article) "its raw source") ".")
	(htm
	 ((:ol :class "fragment-listing")
	  (loop
	     with article-dir = (format nil "~a/~a" *itemization-source* article)
	     with article-text-dir = (format nil "~a/text" article-dir)
	     for i from 1 upto num-items
	     for fragment-path = (format nil "~a/ckb~d.html" article-text-dir i)
	     for item-html = (file-as-string fragment-path)
	     for item-uri = (format nil "/fragment/~a/~d" article i)
	     do
	       (htm
		((:li :class "fragment-listing")
		 ((:a :href item-uri :class "fragment-listing")
		  (str item-html)))))))))))

(defun emit-random-item ()
  (let ((random-vertex (random-elt (hash-table-keys *all-true-items*))))
    (destructuring-bind (article kind number)
	(split ":" random-vertex)
      (redirect (uri-for-item article kind number)))))

(defun emit-random-path ()
  (let* ((keys (hash-table-keys *all-true-items*))
	 (random-vertex-1 (random-elt keys))
	 (random-vertex-2 (random-elt keys)))
    (redirect (link-for-two-items random-vertex-1 random-vertex-2))))

(defmacro register-static-file-dispatcher (uri path &optional mime-type)
  `(progn
     (unless (file-exists-p ,path)
       (error "Can't register URI '~a' to point to '~a', because there's no file at that path" ,uri ,path))
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

(defun emit-mizar-item-page ()
  (register-groups-bind (article-name item-kind item-number)
      (+true-item-uri-regexp+ (request-uri*))
    (let* ((item-key (format nil "~a:~a:~a" article-name item-kind item-number))
	   (ckb-for-item (gethash item-key *item-to-ckb-table*))
	   (article-dir (format nil "~a/~a" *itemization-source* article-name))
	   (article-text-dir (format nil "~a/text" article-dir))
	   (forward-deps (gethash item-key *true-item-dependency-graph-forward*))
	   (backward-deps (gethash item-key *true-item-dependency-graph-backward*))
	   (forward-deps-sorted (sort (copy-list forward-deps) 
				      #'true-item-<))
	   (backward-deps-sorted (sort (copy-list backward-deps)
				       #'true-item-<)))
      (destructuring-bind (ckb-article-name ckb-number)
	  (split ":" ckb-for-item)
	(declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	(let* ((ckb-item-path (format nil "~a/ckb~d.html"
				      article-text-dir
				      ckb-number))
	       (item-html (file-as-string ckb-item-path)))
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
		  (htm (:p (:em "(No item immediately depends on this one.)"))))))))))))))))

(defun emit-ckb-item-page ()
  (register-groups-bind (article-name item-number)
      (+ckb-item-uri-regexp+ (request-uri*))
    (let* ((item-key (format nil "~a:~a" article-name item-number))
	   (article-dir (format nil "~a/~a" *itemization-source* article-name))
	   (article-text-dir (format nil "~a/text" article-dir))
	   (items-for-ckb (gethash item-key *ckb-to-items-table*))
	   (forward-deps (gethash item-key *ckb-dependency-graph-forward*))
	   (backward-deps (gethash item-key *ckb-dependency-graph-backward*))
	   (forward-deps-sorted (sort (copy-list forward-deps) #'ckb-item-<))
	   (backward-deps-sorted (sort (copy-list backward-deps) #'ckb-item-<)))
      (destructuring-bind (ckb-article-name ckb-number)
	  (split ":" item-key)
	(declare (ignore ckb-article-name)) ;; same as ARTICLE-NAME
	(let* ((ckb-item-path (format nil "~a/ckb~a.html"
				      article-text-dir
				      ckb-number))
	       (item-html (file-as-string ckb-item-path)))
	  (miz-item-html (str item-key)
	    (:p (str item-key) " is toplevel fragment #" (str ckb-number) " of article " (str article-name) ".")
	    (if (null (cdr items-for-ckb)) ; the CKB for this item generates only this item
		(htm 
		 (:p "This is the only item generated by that toplevel item."))
		
		(htm
		 (:p "This toplevel item generates this item, as well as these other items:")
		 ((:ul :class "dep-list")
		  (dolist (other-item items-for-ckb)
		    (destructuring-bind (other-item-article other-item-kind other-item-number)
			(split ":" other-item)
		      (let ((other-item-uri (format nil "/~a/~a/~a" other-item-article other-item-kind other-item-number)))
			(htm
			 (:li ((:a :href other-item-uri) (str other-item))))))))))
	    (:table
	     ((:tr :valign "top")
	      (:td 
	       (if forward-deps-sorted
		   (htm
		    (:table
		     (:caption "This item immediately depends on")
		     (dolist (forward-dep forward-deps-sorted)
		       (destructuring-bind (dep-name dep-num)
			   (split ":" forward-dep)
			 (let ((dep-uri (format nil "/~a/~a" dep-name dep-num)))
			   (htm
			    (:tr (:td ((:a :href dep-uri) (str forward-dep))))))))))
		   (htm (:p (:em "(This item immediately depends on nothing.)")))))
	      (:td (str "&lArr;"))
	      (:td :rowspan 2 (str item-html))
	      (:td (str "&rArr;"))
	      (:td 
	       (if backward-deps-sorted
		   (htm
		    (:table
		     (:caption "These items immediately depend on this one:")
		     (dolist (backward-dep backward-deps-sorted)
		       (destructuring-bind (dep-name dep-num)
			   (split ":" backward-dep)
			 (let ((dep-uri (format nil "/~a/~a" dep-name dep-num)))
			   (htm
			    (:tr (:td ((:a :href dep-uri) (str backward-dep))))))))))
		   (htm (:p (:em "(No item immediately depends on this one.)")))))))))))))

(defun emit-main-page ()
  (miz-item-html (str "fine-grained dependencies in mizar")
    (:h1 "welcome")
    (:p "Interested in learning more about the " ((:a :href "http://www.mizar.org/") (:tt "MIZAR") "Mathematical Library") " (MML), the largest corpus of formalized mathematics in the world?  This site provides a way to
    get a handle on the large contents of the MML.")
    (:p "The " (:tt "MIZAR") " community has " ((:a :href "http://mizar.org/version/current/html/") "an attractive presentation of the contents of the MML") ".  (It is simply a directory listing at the moment, listing every article of the MML in alphabetical order.)  This site presents the MML by showing its " (:b "items") " and showing, for each item, what it " (:b "depends") "upon and conversely (what items depend on the item).  This website presents " (:tt "MIZAR") " items, their dependency information, and provides a way of exploring these dependencies by finding " (:b "paths") " among dependencies.")
    (:p "The dependency graph that this site lets you explore has "  (:b (str (hash-table-count *all-true-items*))) " nodes (items) and " (:b (str (length *dependency-graph*))) " edges.")
    (:p "The following articles from the MML are handled:")
    ((:table :class "article-listing" :rules "rows")
     (:thead
      (:tr
       (:th "MML Name")
       (:th "Title")))
     (:tbody
      (loop
	 for (article-name . title) in *articles*
	 for article-uri = (format nil "/article/~a" article-name)
	 for title-escaped = (escape-string title)
	 do
	   (htm
	    (:tr
	     ((:td :class "article-name")
	      ((:a :href article-uri :title title-escaped)
	       (str article-name)))
	     ((:td :class "article-title") (str title)))))))
    (:h1 "getting started")
    (:p "One can inspect " ((:a :href "/random-item") "a random item") " or " ((:a :href "/random-path") "search for a path between two random items") ".")
    (:p "One might also be interested in entering the vast space of " (:tt "MIZAR") " items by inspecting some landmarks.")
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
     


(defun emit-feedback-page ()
  (miz-item-html "feedback"
    (:p
     "Thanks for using this site.  The maintainer is " ((:a :href "http://centria.di.fct.unl.pt/~alama/") "Jesse Alama") ".  If your have questions, comments, bug reports (e.g., broken links), or feature requests, please do " ((:a :href "mailto:jesse.alama@gmail.com") "send an email") "; your feedback is appreciated.")))

(defun initialize-uris ()
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
  (register-static-file-dispatcher "/mizar-item-ckb-table"
				   (mizar-items-config 'item-to-fragment-path)
				   "text/plain")
  ;; intro
  (register-exact-uri-dispatcher "/" #'emit-main-page)
  ;; about page
  (register-exact-uri-dispatcher "/about" #'emit-about-page)
  ;; feedback page
  (register-exact-uri-dispatcher "/feedback" #'emit-feedback-page)
  (register-exact-uri-dispatcher "/random-item" #'emit-random-item)
  (register-exact-uri-dispatcher "/random-path" #'emit-random-path)
  (register-static-file-dispatcher "/favicon.ico" "/Users/alama/sources/mizar/mizar-items/mizar.ico")
  ;; directory setup
  (push 'hunchentoot-dir-lister:dispatch-dir-listers items-dispatch-table)
  (loop
     for (article . title) in *articles*
     do
       (let* ((article-dir (format nil "~a/~a" *itemization-source* article))
	      (miz-uri (format nil "/~a.miz" article))
	      (miz-path (format nil "~a/~a.miz" article-dir article))
	      (prel-dir-uri (format nil "/~a/prel/" article))
	      (prel-dir-path (format nil "~a/prel/" article-dir))
	      (text-dir-uri (format nil "/~a/text" article))
	      (text-dir-path (format nil "~a/text/" article-dir)))
	 ;; static files for the whole article
	 (register-static-file-dispatcher miz-uri miz-path "text/plain")
	 (hunchentoot-dir-lister:add-simple-lister prel-dir-uri prel-dir-path)
	 (hunchentoot-dir-lister:add-simple-lister text-dir-uri text-dir-path)))
  (register-regexp-dispatcher +article-uri-regexp+ #'emit-article-page)
  (register-regexp-dispatcher +ckb-item-uri-regexp+ #'emit-ckb-item-page)
  (register-regexp-dispatcher +true-item-uri-regexp+ #'emit-mizar-item-page)
  (register-regexp-dispatcher +path-between-true-items-uri-regexp+
			      #'emit-path-between-items)
  (register-regexp-dispatcher +path-between-true-items-via-item-uri-regexp+
			      #'emit-path-between-items-via-item)
  t)
