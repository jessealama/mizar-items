
(in-package :mizar)

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
    ("rlvect_1" . "Vectors in Real Linear Space")
    ("rlsub_1" . "Subspaces and Cosets of Subspaces in Real Linear Space")
    ("group_1" . "Groups")
    ("vectsp_1" . "Abelian Groups, Fields and Vector Spaces")
    ("algstr_1" . "From Loops to Abelian Multiplicative Groups with Zero")
    ("complfld" . "The Field of Complex Numbers")
    ("parsp_1" . "Parallelity Spaces")
    ("symsp_1" . "Construction of a bilinear antisymmetric form in symplectic vector space")
    ("ortsp_1" . "Construction of a bilinear symmetric form in orthogonal vector space")
    ("compts_1" . "Compact Spaces")
    ("rlsub_2" . "Operations on Subspaces in Real Linear Space")
    ("midsp_1" . "Midpoint algebras")
    ("funcsdom" . "Real Functions Spaces")
    ("vectsp_2" . "Construction of Rings and Left-, Right-, and Bi-Modules over a Ring")
    ("filter_0" . "Filters - Part I. Implicative Lattices")
    ("lattice2" . "Finite Join and Finite Meet, and Dual Lattices")
    ("realset2" . "Properties of Fields")
    ("robbins1" . "Robbins Algebras vs. Boolean Algebras")
    ("qmax_1" . "The Fundamental Logic Structure in Quantum Mechanics")
    ("parsp_2" . "Fano-Desargues Parallelity Spaces")
    ("rlvect_2" . "Linear Combinations in Real Linear Space")
    ("analoaf" . "Analytical Ordered Affine Spaces")
    ("metric_1" . "Metric Spaces")
    ("diraf" . "Ordered Affine Spaces Defined in Terms of Directed Parallelity - part I")
    ("aff_1" . "Parallelity and Lines in Affine Spaces")
    ("aff_2" . "Classical Configurations in Affine Planes")
    ("aff_3" . "Affine Localizations of Desargues Axiom")
    ("collsp" . "The Collinearity Structure")
    ("pasch" . "Classical and Non--classical Pasch Configurations in Ordered Affine Planes")
    ("real_lat" . "The Lattice of Real Numbers. The Lattice of Real Functions")
    ("tdgroup" . "A Construction of an Abstract Space of Congruence of Vectors")
    ("transgeo" . "Transformations in Affine Spaces")
    ("cat_2" . "Subcategories and Products of Categories")
    ("translac" . "Translations in Affine Planes")
    ("anproj_1" . "A Construction of Analytical Projective Space")
    ("anproj_2" . "Projective Spaces")
    ("rlvect_3" . "Basis of Real Linear Space")
    ("group_2" . "Subgroup and Cosets of Subgroups. Lagrange theorem")
    ("vectsp_4" . "Subspaces and Cosets of Subspaces in Vector Space")
    ("vectsp_5" . "Operations on Subspaces in Vector Space")
    ("normsp_0" . "Preliminaries to Normed Spaces")
    ("normsp_1" . "Real Normed Space")
    ("vfunct_1" . "Algebra of Vector Functions")
    ("vectsp_6" . "Linear Combinations in Vector Space")
    ("vectsp_7" . "Basis of Vector Space")
    ("analmetr" . "Analytical Metric Affine Spaces and Planes")
    ("group_3" . "Classes of Conjugation. Normal Subgroups")
    ("projdes1" . "Desargues Theorem In Projective 3-Space")
    ("group_4" . "Lattice of Subgroups of a Group. Frattini Subgroup")
    ("connsp_2" . "Locally Connected Spaces")
    ("algseq_1" . "Construction of Finite Sequences over Ring and Left-, Right-, and Bi-Modules over a Ring")))

    ;; ("homothet" . "Homotheties and Shears in Affine Planes")
    ;; ("afvect0" . "Directed Geometrical Bundles and Their Analytical Representation")
    ;; ("complsp1" . "Complex Spaces")
    ;; ("realset3" . "Several Properties of Fields. Field Theory")
    ;; ("algstr_2" . "From Double Loops to Fields")
    ;; ("metric_2" . "On Pseudometric Spaces")
    ;; ("metric_3" . "Metrics in Cartesian Product")
    ;; ("hessenbe" . "Hessenberg Theorem")
    ;; ("incproj" . "Incidence Projective Spaces")
    ;; ("afvect01" . "One-Dimensional Congruence of Segments, Basic Facts and Midpoint Relation")
    ;; ("normform" . "Algebra of Normal Forms")
    ;; ("o_ring_1" . "Ordered Rings - Part I")
    ;; ("algstr_3" . "Ternary Fields")
    ;; ("projred1" . "Incidence Projective Space (a reduction theorem in a plane)")
    ;; ("lmod_5" . "Linear Independence in Left Module over Domain")
    ;; ("rmod_2" . "Submodules and Cosets of Submodules in Right Module over Associative Ring")
    ;; ("rmod_3" . "Operations on Submodules in Right Module over Associative Ring")
    ;; ("rmod_4" . "Linear Combinations in Right Module over Associative Ring")
    ;; ("geomtrap" . "A Construction of Analytical Ordered Trapezium Spaces")
    ;; ("projred2" . "On Projections in Projective Planes. Part II")
    ;; ("conaffm" . "Metric-Affine Configurations in Metric Affine Planes -  Part I")
    ;; ("conmetr" . "Metric-Affine Configurations in Metric Affine Planes - Part II")
    ;; ("papdesaf" . "Fanoian, Pappian and Desarguesian Affine Spaces")
    ;; ("pardepap" . "Elementary Variants of Affine Configurational Theorems")
    ;; ("semi_af1" . "Semi-Affine Space")
    ;; ("aff_4" . "Planes in Affine Spaces")
    ;; ("afproj" . "A Projective Closure and Projective Horizon of an Affine Space")
    ;; ("heyting1" . "Algebra of Normal Forms Is a Heyting Algebra")
    ;; ("prelamb" . "Preliminaries to the Lambek Calculus")
    ;; ("oppcat_1" . "Opposite Categories and Contravariant Functors")
    ;; ("euclmetr" . "Fundamental Types of Metric Affine Spaces")
    ;; ("filter_1" . "Filters - Part II. Quotient Lattices Modulo Filters and Direct Product of Two Lattices")
    ;; ("conmetr1" . "Shear Theorems and Their Role in Affine Geometry")
    ;; ("nat_lat" . "The Lattice of Natural Numbers and The Sublattice of it. The Set of Prime Numbers")
    ;; ("group_5" . "Commutator and Center of a Group")
    ;; ("nattra_1" . "Natural Transformations. Discrete Categories")
    ;; ("matrix_1" . "Matrices. Abelian Group of Matrices")
    ;; ("pcomps_1" . "Paracompact and Metrizable Spaces")
    ;; ("midsp_2" . "Atlas of Midpoint Algebra")
    ;; ("ali2" . "Fix Point Theorem for Compact Spaces")
    ;; ("bhsp_1" . "Introduction to Banach and Hilbert spaces - Part I")
    ;; ("bhsp_2" . "Introduction to Banach and Hilbert spaces - Part II")
    ;; ("bhsp_3" . "Introduction to Banach and Hilbert spaces - Part III")
    ;; ("ens_1" . "Category Ens")
    ;; ("borsuk_1" . "A Borsuk Theorem on Homotopy Types")
    ;; ("tbsp_1" . "Totally Bounded Metric Spaces")
    ;; ("grcat_1" . "Categories of Groups")
    ;; ("group_6" . "Homomorphisms and Isomorphisms of Groups. Quotient Group")
    ;; ("matrix_2" . "Transpose Matrices and Groups of Permutations")
    ;; ("fvsum_1" . "Sum and Product of Finite Sequences of Elements of a Field")))